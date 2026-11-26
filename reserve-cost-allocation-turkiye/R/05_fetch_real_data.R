# =============================================================================
# 05_fetch_real_data.R  -  Download the REAL inputs that replace the model's
#                          simulated/assumed placeholders (see Table 1).
#
#   Rscript R/05_fetch_real_data.R [path/to/pje_paper] [start_date] [end_date]
#   e.g. Rscript R/05_fetch_real_data.R . 2025-01-01 2025-12-31
#
# What it downloads (CSV files under data/raw/):
#   EPIAS Transparency Platform (needs a free account):
#     1. realtime_generation_by_source.csv  hourly generation by fuel (system)
#     2. kgup_by_source.csv                 day-ahead schedule (KGUP/DPP) by fuel
#     3. realtime_consumption.csv           hourly consumption
#     4. pfc_amount.csv, pfc_price.csv      primary frequency reserve MW, TL/MW
#        sfc_amount.csv, sfc_price.csv      secondary frequency reserve MW, TL/MW
#     5. powerplant_list.csv                plants with ids (for step 6)
#     6. realtime_generation_by_plant.csv   hourly output per plant (largest
#                                           online plant -> contingency size)
#     7. wind_generation_forecast.csv       YEKDEM wind forecast vs realised
#   Exchange rate (USD/TRY):
#     8. usdtry_evds.csv  TCMB EVDS (needs a free API key), or
#        usdtry_ecb.csv   ECB reference rates (no key; used if EVDS fails)
#   Annual generation (already used by the model):
#     9. owid-energy-data.csv  Our World in Data (no key)
#
# CREDENTIALS - never write them in this file. Put them in ~/.Renviron:
#     EPIAS_USERNAME=your_epias_login_email
#     EPIAS_PASSWORD=your_epias_password
#     EVDS_API_KEY=your_evds_key          (optional; ECB fallback otherwise)
# then restart R. In R you can open the file with usethis::edit_r_environ().
#
# Endpoint paths follow the EPIAS Transparency Platform v1 API
# (https://seffaflik.epias.com.tr/electricity-service/technical/en/index.html).
# If EPIAS renames an endpoint, change it only in the `EP` list below.
# =============================================================================

suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(data.table)
})

args  <- commandArgs(trailingOnly = TRUE)
ROOT  <- if (length(args) >= 1) args[1] else "."
START <- as.Date(if (length(args) >= 2) args[2] else "2025-01-01")
END   <- as.Date(if (length(args) >= 3) args[3] else "2025-12-31")
OUT   <- file.path(ROOT, "data", "raw"); dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
FETCH_PLANT_LEVEL <- TRUE    # step 6 is the slowest (one request per day)

BASE <- "https://seffaflik.epias.com.tr/electricity-service/v1"
EP <- list(
  rt_gen      = "generation/data/realtime-generation",
  kgup        = "generation/data/dpp",
  rt_cons     = "consumption/data/realtime-consumption",
  pfc_amount  = "markets/ancillary-services/data/primary-frequency-capacity-amount",
  pfc_price   = "markets/ancillary-services/data/primary-frequency-capacity-price",
  sfc_amount  = "markets/ancillary-services/data/secondary-frequency-capacity-amount",
  sfc_price   = "markets/ancillary-services/data/secondary-frequency-capacity-price",
  pp_list     = "generation/data/powerplant-list",            # GET
  rt_gen_bulk = "generation/data/realtime-generation-bulk",
  wind_fc     = "renewables/data/res-generation-and-forecast"
)

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
need_env <- function(v) {
  x <- Sys.getenv(v)
  if (!nzchar(x)) stop("Environment variable ", v, " is not set. Add it to ~/.Renviron and restart R.", call. = FALSE)
  x
}

# ---------------------------------------------------------------- EPIAS auth
.tgt <- new.env()
epias_tgt <- function(force = FALSE) {
  if (!force && !is.null(.tgt$value) && Sys.time() < .tgt$expires) return(.tgt$value)
  resp <- request("https://giris.epias.com.tr/cas/v1/tickets") |>
    req_headers(Accept = "text/plain") |>
    req_body_form(username = need_env("EPIAS_USERNAME"), password = need_env("EPIAS_PASSWORD")) |>
    req_error(is_error = function(r) FALSE) |>
    req_perform()
  tgt <- trimws(resp_body_string(resp))
  if (!resp_status(resp) %in% c(200, 201) || !startsWith(tgt, "TGT-"))
    stop("EPIAS login failed (HTTP ", resp_status(resp), "). Check EPIAS_USERNAME / EPIAS_PASSWORD.", call. = FALSE)
  .tgt$value <- tgt; .tgt$expires <- Sys.time() + 90 * 60     # tickets last ~2 h
  tgt
}

# Throttle: EPIAS allows 80 requests/min per gateway -> keep >= 0.9 s between calls
.last_call <- new.env(); .last_call$t <- Sys.time() - 10
throttle <- function(min_gap = 0.9) {
  wait <- min_gap - as.numeric(difftime(Sys.time(), .last_call$t, units = "secs"))
  if (wait > 0) Sys.sleep(wait)
  .last_call$t <- Sys.time()
}

epias_call <- function(ep, body = NULL, method = "POST", tries = 6, timeout = 600) {
  for (k in seq_len(tries)) {
    throttle()
    req <- request(file.path(BASE, ep)) |>
      req_headers(TGT = epias_tgt(force = k > 2), Accept = "application/json") |>
      req_timeout(timeout) |>
      req_error(is_error = function(r) FALSE)
    if (method == "POST") req <- req_body_json(req, body, auto_unbox = TRUE)
    resp <- tryCatch(req_perform(req), error = function(e) e)   # timeouts, resets
    if (inherits(resp, "error")) {
      msg("  network error on", ep, "- retry", k, ":", conditionMessage(resp))
      Sys.sleep(min(60, 5 * k)); next
    }
    st <- resp_status(resp)
    if (st %in% c(200, 201)) {
      js <- fromJSON(resp_body_string(resp), flatten = TRUE)
      it <- if (!is.null(js$items)) js$items else js
      return(as.data.table(it))
    }
    msg("  HTTP", st, "on", ep, "- retry", k, ":", substr(resp_body_string(resp), 1, 120))
    Sys.sleep(if (st == 429) 61 else min(60, 5 * k))   # 429 = rate limit: wait a full minute
  }
  stop("EPIAS request failed after ", tries, " tries: ", ep, call. = FALSE)
}

iso <- function(d) format(as.Date(d), "%Y-%m-%dT00:00:00+03:00")
month_chunks <- function(a, b) {
  s <- seq(as.Date(format(a, "%Y-%m-01")), b, by = "month")
  data.table(from = pmax(s, a), to = pmin(c(s[-1] - 1, b), b))
}

# Fetch an endpoint month by month (keeps each request small) and save it
fetch_range <- function(name, ep, extra = list()) {
  f <- file.path(OUT, paste0(name, ".csv"))
  if (file.exists(f)) { msg("skip (exists):", f); return(invisible(fread(f))) }
  ch <- month_chunks(START, END)
  res <- rbindlist(lapply(seq_len(nrow(ch)), function(i) {
    msg(name, as.character(ch$from[i]), "->", as.character(ch$to[i]))
    epias_call(ep, c(list(startDate = iso(ch$from[i]), endDate = iso(ch$to[i])), extra))
  }), fill = TRUE)
  fwrite(res, f); msg("saved", nrow(res), "rows ->", f)
  invisible(res)
}

# ---------------------------------------------------------------- 1-4, 7
run_epias <- nzchar(Sys.getenv("EPIAS_USERNAME"))
if (!run_epias) {
  msg("EPIAS_USERNAME not set: skipping EPIAS downloads (steps 1-7).")
} else {
  fetch_range("realtime_generation_by_source", EP$rt_gen)
  fetch_range("kgup_by_source",               EP$kgup, list(region = "TR1"))
  fetch_range("realtime_consumption",         EP$rt_cons)
  fetch_range("pfc_amount", EP$pfc_amount)
  fetch_range("pfc_price",  EP$pfc_price)
  fetch_range("sfc_amount", EP$sfc_amount)
  fetch_range("sfc_price",  EP$sfc_price)
  try(fetch_range("wind_generation_forecast", EP$wind_fc))

  # -------------------------------------------------------------- 5-6 plants
  pp_f <- file.path(OUT, "powerplant_list.csv")
  pp <- if (file.exists(pp_f)) fread(pp_f) else { x <- epias_call(EP$pp_list, method = "GET"); fwrite(x, pp_f); x }
  msg("power plants listed:", nrow(pp))

  if (FETCH_PLANT_LEVEL) {
    f6 <- file.path(OUT, "realtime_generation_by_plant.csv")
    days <- seq(START, END, by = "day")
    log6 <- file.path(OUT, "realtime_generation_by_plant_days_done.txt")   # lets you resume
    done <- if (file.exists(log6)) readLines(log6) else character()
    ids <- pp$id
    id_chunks <- split(ids, ceiling(seq_along(ids) / 150))   # smaller payloads, fewer timeouts
    for (d in as.character(days)) {
      if (d %in% done) next
      day_dt <- rbindlist(lapply(id_chunks, function(ii)
        epias_call(EP$rt_gen_bulk, list(date = iso(d), powerPlantIds = as.list(ii)))), fill = TRUE)
      if (nrow(day_dt)) fwrite(day_dt, f6, append = file.exists(f6))
      cat(d, "\n", file = log6, append = TRUE, sep = "")
      msg("plant-level", d, nrow(day_dt), "rows")
    }
  }
}

# ---------------------------------------------------------------- 8 USD/TRY
fx_ok <- FALSE
if (nzchar(Sys.getenv("EVDS_API_KEY"))) {
  evds_base <- Sys.getenv("EVDS_BASE_URL", "https://evds2.tcmb.gov.tr/service/evds")
  url <- sprintf("%s/series=TP.DK.USD.A.YTL&startDate=%s&endDate=%s&type=json",
                 evds_base, format(START, "%d-%m-%Y"), format(END, "%d-%m-%Y"))
  r <- try(request(url) |> req_headers(key = Sys.getenv("EVDS_API_KEY")) |>
             req_timeout(60) |> req_perform(), silent = TRUE)
  if (!inherits(r, "try-error")) {
    js <- fromJSON(resp_body_string(r))
    fx <- as.data.table(js$items)[, .(date = as.Date(Tarih, "%d-%m-%Y"),
                                       usdtry = as.numeric(TP_DK_USD_A_YTL))]
    fwrite(fx, file.path(OUT, "usdtry_evds.csv")); fx_ok <- TRUE
    msg("saved USD/TRY from TCMB EVDS:", nrow(fx), "days")
  } else msg("EVDS request failed:", conditionMessage(attr(r, "condition")))
}
if (!fx_ok) {
  ecb <- function(cur) {
    u <- sprintf("https://data-api.ecb.europa.eu/service/data/EXR/D.%s.EUR.SP00.A?startPeriod=%s&endPeriod=%s&format=csvdata",
                 cur, START, END)
    fread(text = resp_body_string(req_perform(request(u))))[, .(date = as.Date(TIME_PERIOD), v = OBS_VALUE)]
  }
  r <- try({
    fx <- merge(ecb("TRY")[, .(date, try_eur = v)], ecb("USD")[, .(date, usd_eur = v)], by = "date")
    fx[, usdtry := try_eur / usd_eur]
    fwrite(fx, file.path(OUT, "usdtry_ecb.csv"))
    msg("saved USD/TRY from ECB reference rates:", nrow(fx), "days")
  }, silent = TRUE)
  if (inherits(r, "try-error")) msg("ECB fallback failed too; add EVDS_API_KEY.")
}

# ---------------------------------------------------------------- 9 OWID
owid_f <- file.path(ROOT, "data", "owid-energy-data.csv")
if (!file.exists(owid_f)) {
  download.file("https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv", owid_f, mode = "wb")
  msg("saved OWID energy data")
}

msg("Done. Files in", normalizePath(OUT))
msg("Not downloadable by API (download by hand): EPDK licence list / TEIAS unit list (unit block sizes),",
    "EPDK tariff tables (system operation charge), grid-code frequency limits.")
