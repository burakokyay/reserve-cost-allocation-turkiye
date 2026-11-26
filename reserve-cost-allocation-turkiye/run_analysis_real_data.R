# =============================================================================
# run_analysis_real_data.R  -  ONE self-contained script for the PJE paper
# "Who should pay for system security? Causation-based allocation of reserve
#  costs in Turkiye's low-inertia electricity transition"
#
# HOW TO RUN (RStudio): setwd() to the folder that contains data/raw/
# (e.g. setwd("~/Desktop")), open this file and press "Source".
# Outputs:
#   results/PJE_results_tables_figures.docx   all tables + figures in Word
#   tables/  (T0, T2-T8 CSV)   figures/ (Fig1-Fig6 PNG)
#   data/results.rds, data/real_data_diagnostics.txt
# Runtime: ~10-15 min in RStudio (single core). Progress is printed with times.
# =============================================================================

## ---- 0. setup --------------------------------------------------------------
pkgs <- c("data.table", "ggplot2", "scales", "officer", "flextable")
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss)
ROOT <- getwd()
for (d in c("tables", "figures", "data", "results")) dir.create(file.path(ROOT, d), showWarnings = FALSE)
STEP <- "setup"
options(error = function() cat("\n!! SCRIPT STOPPED during step:", STEP,
                                "\n   Please send the error message above and data/real_data_diagnostics.txt\n"))
cat("Working directory:", ROOT, "\n")
if (!dir.exists(file.path(ROOT, "data", "raw")))
  cat("NOTE: data/raw/ not found here - the model will use SYNTHETIC profiles.\n",
      "     setwd() to the folder that contains data/raw/ and run again.\n")






## ---- 1. scenarios and assumptions ------------------------------------------
STEP <- "1. scenarios and assumptions"
# =============================================================================
# 00_config.R  -  Scenario definitions and model parameters
# Project: Who should pay for system security? Causation-based allocation of
#          frequency-containment reserve costs in Turkiye (PJE manuscript)
#
# DATA PROVENANCE FLAGS (also reported in the manuscript, Table 1):
#   [REAL]   taken from public data (OWID energy dataset, built on Ember / EI)
#   [PUBLIC] public figure quoted in the research proposal (ETKB, Akkuyu NPP)
#   [ASSUMED] modelling assumption / literature-typical value -> PLACEHOLDER,
#            to be replaced by EPIAS Transparency Platform & TEIAS data
# =============================================================================

HOURS <- 8760L

# ---- Annual energy targets (TWh) --------------------------------------------
# 2025 values are overwritten in 01_data.R by the REAL OWID figures.
scenarios <- list(
  S2025 = list(
    label = "2025 (calibration)",
    demand_twh = NA,            # [REAL] set from OWID (generation, 2025)
    wind_gw = 14.3, solar_gw = 25.6,   # [PUBLIC] 39.9 GW W+S end-2025 (ETKB)
    wind_twh = NA, solar_twh = NA,     # [REAL] OWID 2025
    hydro_gw = 32.3, hydro_twh = NA,   # cap [ASSUMED]; energy [REAL]
    other_twh = NA,                    # [REAL] geothermal+biomass (OWID)
    coal_twh = NA,                     # [REAL] coal target (OWID)
    nuclear_units = 0L,
    coal_scale = 1.00
  ),
  S2030 = list(
    label = "2030 (2 Akkuyu units)",
    demand_growth = 1.03^5,            # [ASSUMED] 3 %/yr demand growth
    wind_gw = 24, solar_gw = 40,       # [ASSUMED] linear path to 120 GW in 2035
    hydro_gw = 33.5,
    nuclear_units = 2L,                # [PUBLIC] Akkuyu units 1-2 online
    coal_scale = 0.85                  # [ASSUMED] coal energy -15 % vs 2025
  ),
  S2035 = list(
    label = "2035 (4 Akkuyu units, 120 GW W+S)",
    demand_growth = 1.03^10,
    wind_gw = 45, solar_gw = 75,       # [PUBLIC] 120 GW wind+solar target
    hydro_gw = 35,
    nuclear_units = 4L,                # [PUBLIC] all four 1,200 MW units
    coal_scale = 0.65
  )
)

# ---- Stylised unit fleet (MW) [ASSUMED placeholder for TEIAS unit list] -------
# Unit sizes follow typical Turkish block sizes (imported-coal 600-660 MW,
# lignite 150-360 MW, CCGT blocks 150-450 MW, large reservoir hydro 200-300 MW).
fleet_spec <- function(scen_name) {
  coal_ret <- switch(scen_name, S2025 = 0, S2030 = 1500, S2035 = 3500)
  list(
    coal = c(rep(660, 4), rep(600, 8), rep(350, 7),
             rep(200, max(0, 60 - coal_ret %/% 200))),
    gas  = c(rep(450, 10), rep(300, 30), rep(150, 50), rep(50, 70)),
    hydro_res = c(rep(300, 14), rep(200, 6), rep(100, 60)),
    nuclear = rep(1200, scenarios[[scen_name]]$nuclear_units)
  )
}

# ---- Technology parameters [ASSUMED, literature-typical] ---------------------
tech_par <- data.frame(
  tech   = c("nuclear", "coal", "gas", "hydro_res", "hydro_ror", "other", "wind", "solar"),
  H_s    = c(6.0, 4.5, 5.0, 3.0, 3.0, 2.0, 0.0, 0.0),   # inertia constant (s)
  FOR    = c(0.03, 0.08, 0.05, 0.02, 0.02, 0.05, 0.02, 0.02),  # forced outage rate
  stringsAsFactors = FALSE
)

# ---- Security / reserve parameters [ASSUMED] --------------------------------
sec_par <- list(
  f0 = 50,            # nominal frequency (Hz)
  df_max = 0.8,       # admissible frequency deviation at nadir (Hz)
  Td = 10,            # effective FCR delivery time (s); Turkish Grid Code requires full activation within 30 s (see sensitivity)
  z = 2.33,           # 99 % quantile for forecast-error reserve
  e_wind = 0.08,      # hour-ahead forecast error sd, share of wind output
  e_solar = 0.06,     # share of solar output
  e_load = 0.010,     # share of load
  E_import = 0,       # inertia support from interconnection (MWs); sensitivity
  E_floor = 60000,    # minimum system inertia kept by the TSO (MWs)
  kappa_v = 1,        # scale of the variability component; with real data it is
                      # calibrated so the 2025 requirement matches actual PFC+SFC volume
  use_measured_errors = FALSE  # TRUE = use measured day-ahead errors as hourly reserve errors
)

# ---- Reserve price [ASSUMED] ------------------------------------------------
price_par <- list(
  p0 = 15,            # mean reserve capacity price, USD/MW-h (illustrative)
  beta = 1.5,         # elasticity to net load
  sd = 0.30, phi = 0.9
)

N_MC <- 30L           # Monte Carlo weather/outage years per scenario


## ---- 2a. real-data loader --------------------------------------------------
STEP <- "2a. real-data loader"
# =============================================================================
# 01b_real_data.R  -  Load the REAL EPIAS / ECB files from data/raw/ (made by
# 05_fetch_real_data.R) and turn them into hourly model inputs for 2025.
#
# Column names of EPIAS responses are matched by keyword (English or Turkish),
# so small naming differences do not break the loader. Everything it detects
# is written to data/real_data_diagnostics.txt - paste that file back if
# something looks wrong.
#
# Returns (in the global environment):
#   REAL     data.table, 8760 rows: dt, load, wind, solar, hydro, coal, gas,
#            other, nuclear, res_amt (MW), res_price_usd (USD/MW-h), ...
#   USE_REAL TRUE if the hourly generation and consumption files were found
# =============================================================================
library(data.table)

RAW <- file.path(ROOT, "data", "raw")
DIAG <- file.path(ROOT, "data", "real_data_diagnostics.txt")
diag_lines <- character()
dlog <- function(...) {
  x <- paste0(...); diag_lines <<- c(diag_lines, x); cat(x, "\n")
}

norm_names <- function(x) {
  x <- tolower(x)
  for (p in list(c("\u00e7", "c"), c("\u011f", "g"), c("\u0131", "i"), c("\u00f6", "o"),
                 c("\u015f", "s"), c("\u00fc", "u"), c("\u00e2", "a"), c("\u00ee", "i")))
    x <- gsub(p[1], p[2], x, fixed = TRUE)
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "")
  gsub("[^a-z0-9]", "", x)
}

read_raw <- function(name) {
  f <- file.path(RAW, paste0(name, ".csv"))
  if (!file.exists(f)) { dlog("[missing] ", name, ".csv"); return(NULL) }
  d <- fread(f, encoding = "UTF-8")
  dlog("[found]   ", name, ".csv  rows=", nrow(d), "  cols=", paste(names(d), collapse = ", "))
  d
}

# --- timestamps: EPIAS gives ISO strings such as 2025-01-01T00:00:00+03:00 ---
parse_dt <- function(d) {
  nn <- norm_names(names(d))
  dcol <- names(d)[match(TRUE, nn %in% c("date", "tarih", "datetime", "dt"))]
  if (is.na(dcol)) dcol <- names(d)[grep("date|tarih", nn)[1]]
  if (is.na(dcol)) stop("no date column found in: ", paste(names(d), collapse = ", "))
  x <- d[[dcol]]
  if (inherits(x, "POSIXct")) {            # fread already parsed ISO-8601 (stored in UTC)
    dt <- x; attr(dt, "tzone") <- "Etc/GMT-3"
    return(dt)
  }
  s <- as.character(x)
  dt <- as.POSIXct(substr(gsub("T", " ", s), 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-3")
  if (all(is.na(dt))) dt <- as.POSIXct(substr(s, 1, 10), format = "%Y-%m-%d", tz = "Etc/GMT-3")
  # add an hour/time column if the date column carries only the day
  hcol <- names(d)[match(TRUE, nn %in% c("hour", "saat", "time"))]
  if (!is.na(hcol) && length(unique(format(dt, "%H"))) == 1) {
    hh <- as.integer(substr(as.character(d[[hcol]]), 1, 2))
    dt <- dt + 3600 * hh
  }
  dt
}

# --- fuel-type columns (English API names and Turkish names) ------------------
fuel_patterns <- list(
  coal    = "lignite|linyit|importcoal|ithalkomur|ithalkomur|hardcoal|blackcoal|taskomur|asphaltite|asfaltit|coal|komur",
  gas     = "naturalgas|dogalgaz|lng|fueloil|fuelolu|naphta|nafta|^oil",
  hydro   = "dammed|baraj|river|akarsu|hydro|hidro",
  wind    = "wind|ruzgar",
  solar   = "^sun|solar|gunes",
  nuclear = "nuclear|nucklear|nukleer",
  other   = "geothermal|jeotermal|biomass|biokutle|biyokutle|wasteheat|atikisi|other|diger"
)
skip_pattern <- "total|toplam|importexport|ithalatihracat|date|tarih|hour|saat|time|^id$|powerplant|santral|uevcb|organization|^version"

by_fuel <- function(d) {
  nn <- norm_names(names(d)); num <- vapply(d, is.numeric, TRUE)
  out <- list(); used <- character()
  for (f in names(fuel_patterns)) {
    cols <- names(d)[num & grepl(fuel_patterns[[f]], nn) & !grepl(skip_pattern, nn) & !(names(d) %in% used)]
    used <- c(used, cols)
    out[[f]] <- if (length(cols)) rowSums(d[, ..cols], na.rm = TRUE) else rep(0, nrow(d))
    dlog("    ", sprintf("%-8s", f), "<- ", if (length(cols)) paste(cols, collapse = " + ") else "(none)")
  }
  left <- setdiff(names(d)[num & !grepl(skip_pattern, nn)], used)
  if (length(left)) dlog("    unassigned numeric columns (ignored): ", paste(left, collapse = ", "))
  tot <- names(d)[num & grepl("^total$|^toplam$", nn)]
  if (length(tot)) {
    s <- Reduce(`+`, out)
    dlog("    check: sum of fuels / reported total = ",
         round(sum(s, na.rm = TRUE) / sum(d[[tot[1]]], na.rm = TRUE), 4))
  }
  as.data.table(out)
}

pick_num <- function(d, pattern, label) {
  nn <- norm_names(names(d)); num <- vapply(d, is.numeric, TRUE)
  col <- names(d)[num & grepl(pattern, nn)][1]
  if (is.na(col)) col <- names(d)[num & !grepl(skip_pattern, nn)][1]
  dlog("    ", label, " <- column '", col, "'")
  d[[col]]
}

to_hourly <- function(dt, dtab) {
  # average sub-hourly rows (e.g. 10-minute data) to hourly means
  h <- as.POSIXct(format(dt, "%Y-%m-%d %H:00:00"), tz = "Etc/GMT-3")
  dtab[, dt := h][, lapply(.SD, mean, na.rm = TRUE), by = dt]
}

# =============================================================================
load_real_inputs <- function(year = 2025) {
  dlog("==== Real-data loader  (", format(Sys.time()), ")  folder: ", normalizePath(RAW, mustWork = FALSE))
  grid <- data.table(dt = seq(as.POSIXct(sprintf("%d-01-01 00:00:00", year), tz = "Etc/GMT-3"),
                              by = "hour", length.out = HOURS))

  # 1. hourly generation by source ------------------------------------------
  g <- read_raw("realtime_generation_by_source")
  c_ <- read_raw("realtime_consumption")
  if (is.null(g) || is.null(c_)) {
    dlog("!! generation or consumption file missing -> model keeps SYNTHETIC profiles")
    writeLines(diag_lines, DIAG); return(NULL)
  }
  gd <- by_fuel(g); gd <- to_hourly(parse_dt(g), gd)
  cd <- data.table(load = pick_num(c_, "consumption|tuketim", "load")); cd <- to_hourly(parse_dt(c_), cd)
  R <- merge(grid, gd, by = "dt", all.x = TRUE)
  R <- merge(R, cd, by = "dt", all.x = TRUE)

  # 2. KGUP (day-ahead schedule) by source -> forecast errors -----------------
  k <- read_raw("kgup_by_source")
  if (!is.null(k)) {
    kd <- by_fuel(k); setnames(kd, paste0("kgup_", names(kd)))
    kd <- to_hourly(parse_dt(k), kd); R <- merge(R, kd, by = "dt", all.x = TRUE)
  }
  # 3. YEKDEM wind forecast vs realised (10-min) -------------------------------
  w <- read_raw("wind_generation_forecast")
  if (!is.null(w)) {
    wd <- data.table(wf_gen = pick_num(w, "^generation$|^uretim$|generation|uretim", "wind realised"),
                     wf_fc  = pick_num(w, "^forecast$|^tahmin$|forecast|tahmin", "wind forecast"))
    wd <- to_hourly(parse_dt(w), wd); R <- merge(R, wd, by = "dt", all.x = TRUE)
  }
  # 4. ancillary services: amounts (MW) and prices (TL/MW) -------------------
  for (nm in c("pfc_amount", "pfc_price", "sfc_amount", "sfc_price")) {
    a <- read_raw(nm)
    if (is.null(a)) next
    v <- pick_num(a, if (grepl("price", nm)) "price|fiyat|bedel" else "amount|miktar|quantity", nm)
    ad <- to_hourly(parse_dt(a), data.table(v = v)); setnames(ad, "v", nm)
    R <- merge(R, ad, by = "dt", all.x = TRUE)
  }
  # 5. exchange rate -----------------------------------------------------------
  fxf <- c(file.path(RAW, "usdtry_evds.csv"), file.path(RAW, "usdtry_ecb.csv"))
  fxf <- fxf[file.exists(fxf)][1]
  if (!is.na(fxf)) {
    fx <- fread(fxf); dlog("[found]   ", basename(fxf), " rows=", nrow(fx))
    fx <- fx[, .(day = as.Date(date), usdtry = as.numeric(usdtry))][order(day)]
    alld <- data.table(day = seq(as.Date(sprintf("%d-01-01", year)) - 7, as.Date(sprintf("%d-12-31", year)), by = "day"))
    fx <- fx[alld, on = "day", roll = TRUE, rollends = c(TRUE, TRUE)]                     # carry rate over weekends/holidays
    R[, day := as.Date(format(dt, "%Y-%m-%d"))]
    R <- merge(R, fx, by = "day", all.x = TRUE); R[, day := NULL]
    dlog("    USD/TRY mean ", round(mean(R$usdtry, na.rm = TRUE), 2))
  } else dlog("[missing] usdtry file -> reserve price stays ASSUMED")

  setorder(R, dt)
  # fill isolated gaps (DST-free calendar, but API gaps happen)
  numc <- setdiff(names(R), "dt")
  for (cc in numc) {
    x <- R[[cc]]; nna <- sum(is.na(x))
    if (nna > 0 && nna < HOURS) {
      dlog("    gap-filled ", nna, " NA hours in ", cc)
      x <- approx(seq_along(x), x, xout = seq_along(x), rule = 2)$y; set(R, j = cc, value = x)
    }
  }

  # 6. derived reserve price (capacity-weighted PFC+SFC), USD/MW-h -------------
  if (all(c("pfc_amount", "pfc_price", "sfc_amount", "sfc_price", "usdtry") %in% names(R))) {
    R[, res_amt := pfc_amount + sfc_amount]
    R[, res_cost_tl := pfc_amount * pfc_price + sfc_amount * sfc_price]
    R[, res_price_usd := ifelse(res_amt > 0, res_cost_tl / res_amt, NA_real_) / usdtry]
    R[is.na(res_price_usd), res_price_usd := median(R$res_price_usd, na.rm = TRUE)]
    dlog("    actual 2025 PFC+SFC volume: mean ", round(mean(R$res_amt)), " MW (PFC ",
         round(mean(R$pfc_amount)), ", SFC ", round(mean(R$sfc_amount)), ")")
    dlog("    actual 2025 PFC+SFC capacity cost: ",
         round(sum(R$res_cost_tl / R$usdtry) / 1e6, 1), " million USD; mean price ",
         round(mean(R$res_price_usd), 2), " USD/MW-h")
  } else dlog("!! ancillary amounts/prices or FX missing -> reserve price stays ASSUMED")

  dlog("    annual energy (TWh): load ", round(sum(R$load) / 1e6, 1),
       " | coal ", round(sum(R$coal) / 1e6, 1), " gas ", round(sum(R$gas) / 1e6, 1),
       " hydro ", round(sum(R$hydro) / 1e6, 1), " wind ", round(sum(R$wind) / 1e6, 1),
       " solar ", round(sum(R$solar) / 1e6, 1), " other ", round(sum(R$other) / 1e6, 1),
       " nuclear ", round(sum(R$nuclear) / 1e6, 2))
  writeLines(diag_lines, DIAG)
  R
}

# --- forecast-error parameters from real data --------------------------------
estimate_errors <- function(R) {
  # Forecast-error sd as a share of mean output. A series is only used when the
  # forecast and the realised series cover the same plants (energy ratio 0.7-1.3)
  # and the result is plausible (1-60 %); otherwise the assumption is kept.
  out <- list()
  try_err <- function(real, fc, label, mask = rep(TRUE, length(real))) {
    real <- real[mask]; fc <- fc[mask]
    ratio <- sum(fc, na.rm = TRUE) / max(sum(real, na.rm = TRUE), 1e-9)
    e <- sd(real - fc, na.rm = TRUE) / mean(real[real > 0], na.rm = TRUE)
    ok <- is.finite(e) && ratio > 0.7 && ratio < 1.3 && e > 0.01 && e < 1
    dlog("    ", label, ": forecast/realised energy ratio = ", round(ratio, 2),
         ", error sd = ", round(100 * e, 1), "% of mean output -> ", if (ok) "USED" else "REJECTED (assumption kept)")
    if (ok) e else NA_real_
  }
  ew <- NA_real_
  if ("wf_fc" %in% names(R)) ew <- try_err(R$wf_gen, R$wf_fc, "wind (YEKDEM forecast vs realised)")
  if (!is.finite(ew) && "kgup_wind" %in% names(R)) ew <- try_err(R$wind, R$kgup_wind, "wind (KGUP vs realised)")
  if (is.finite(ew)) out$e_wind <- ew
  if ("kgup_solar" %in% names(R)) {
    es <- try_err(R$solar, R$kgup_solar, "solar (KGUP vs realised, daylight hours)", R$solar > 0)
    if (is.finite(es)) out$e_solar <- es
  }
  dlog("    NOTE: EPIAS real-time 'sun' covers licensed plants only (", round(sum(R$solar) / 1e6, 1),
       " TWh). Its hourly SHAPE is used and scaled to total solar incl. unlicensed (OWID).")
  writeLines(diag_lines, DIAG)
  out
}


## ---- 2b. data: OWID annual + EPIAS hourly ----------------------------------
STEP <- "2b. data: OWID annual + EPIAS hourly"
# =============================================================================
# 01_data.R  -  REAL public data: Turkiye electricity generation 2010-2025
# Source: Our World in Data energy dataset (owid/energy-data, GitHub), which
#         compiles Ember and Energy Institute statistics. Downloaded Nov 2026.
# =============================================================================
library(data.table)

owid_path <- file.path(ROOT, "data", "owid-energy-data.csv")
if (!file.exists(owid_path)) {
  download.file("https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv",
                owid_path, mode = "wb")
}
owid <- fread(owid_path)
tr <- owid[country == "Turkey" & year >= 2010,
           .(year, total = electricity_generation, demand = electricity_demand,
             coal = coal_electricity, gas = gas_electricity, oil = oil_electricity,
             hydro = hydro_electricity, wind = wind_electricity,
             solar = solar_electricity, other = other_renewable_electricity,
             nuclear = nuclear_electricity)]
tr[, vre_share := (wind + solar) / total]
fwrite(tr, file.path(ROOT, "tables", "T0_turkiye_generation_2010_2025_OWID.csv"))

act25 <- tr[year == 2025]
# Calibrate the 2025 scenario to REAL data
scenarios$S2025$demand_twh <- act25$total
scenarios$S2025$wind_twh   <- act25$wind
scenarios$S2025$solar_twh  <- act25$solar
scenarios$S2025$hydro_twh  <- act25$hydro
scenarios$S2025$other_twh  <- act25$other
scenarios$S2025$coal_twh   <- act25$coal

# Derive 2030 / 2035 energy targets from 2025 capacity factors
cf_w <- act25$wind  / (scenarios$S2025$wind_gw  * 8.76)
cf_s <- act25$solar / (scenarios$S2025$solar_gw * 8.76)
cf_h <- act25$hydro / (scenarios$S2025$hydro_gw * 8.76)
for (s in c("S2030", "S2035")) {
  sc <- scenarios[[s]]
  sc$demand_twh <- act25$total * sc$demand_growth
  sc$wind_twh   <- sc$wind_gw  * 8.76 * cf_w
  sc$solar_twh  <- sc$solar_gw * 8.76 * cf_s
  sc$hydro_twh  <- sc$hydro_gw * 8.76 * cf_h
  sc$other_twh  <- act25$other * (1 + 0.04)^(ifelse(s == "S2030", 5, 10))
  sc$coal_twh   <- act25$coal * sc$coal_scale
  scenarios[[s]] <- sc
}

# ---- REAL hourly inputs from EPIAS (if data/raw/ exists) ---------------------
REAL <- load_real_inputs(2025)
USE_REAL <- !is.null(REAL)
PRICE_SOURCE <- "assumed"
if (USE_REAL) {
  errs <- estimate_errors(REAL)
  # Measured errors are DAY-AHEAD errors (YEKDEM forecast, KGUP). Most of them are
  # handled by intraday trading and the balancing market, not by PFC/SFC capacity,
  # so by default they are reported only; the reserve level is calibrated instead.
  if (isTRUE(sec_par$use_measured_errors))
    for (nm in names(errs)) if (is.finite(errs[[nm]])) sec_par[[nm]] <- errs[[nm]]
  MEASURED_ERRORS <- errs
  if ("res_price_usd" %in% names(REAL)) PRICE_SOURCE <- "EPIAS PFC+SFC 2025"
  cat("Using REAL EPIAS hourly data. e_wind =", round(sec_par$e_wind, 3),
      " e_solar =", round(sec_par$e_solar, 3), " price:", PRICE_SOURCE, "\n")
} else cat("data/raw not found: using SYNTHETIC hourly profiles\n")

# ---- OBSERVED unit fleet from EPIAS plant-level data (07_fleet_from_epias.R) --
if (!exists("USE_OBSERVED_FLEET")) USE_OBSERVED_FLEET <- TRUE
FLEET_SOURCE <- "assumed"
f_fleet <- file.path(ROOT, "data", "fleet_observed.rds")
if (USE_OBSERVED_FLEET && file.exists(f_fleet)) {
  FLEET_OBS <- readRDS(f_fleet)
  fleet_spec <- function(scen_name) {
    coal_ret <- switch(scen_name, S2025 = 0, S2030 = 1500, S2035 = 3500)
    coal <- sort(FLEET_OBS$coal, decreasing = TRUE)
    while (coal_ret > 0 && length(coal) > 1) {         # retire the smallest units first
      coal_ret <- coal_ret - coal[length(coal)]; coal <- coal[-length(coal)]
    }
    list(coal = coal, gas = FLEET_OBS$gas, hydro_res = FLEET_OBS$hydro_res,
         nuclear = rep(1200, scenarios[[scen_name]]$nuclear_units))
  }
  for (tt in names(FLEET_OBS$FOR)) if (is.finite(FLEET_OBS$FOR[[tt]]) && FLEET_OBS$FOR[[tt]] > 0)
    tech_par$FOR[tech_par$tech == tt] <- FLEET_OBS$FOR[[tt]]
  FLEET_SOURCE <- FLEET_OBS$source
  cat("Using OBSERVED fleet from EPIAS plant data: coal", length(FLEET_OBS$coal), "units,",
      "gas", length(FLEET_OBS$gas), "units, hydro", length(FLEET_OBS$hydro_res), "units; FOR:",
      paste(names(FLEET_OBS$FOR), round(FLEET_OBS$FOR, 3), collapse = ", "), "\n")
}


## ---- 3. hourly system model ------------------------------------------------
STEP <- "3. hourly system model"
# =============================================================================
# 02_model.R  -  Hourly system simulation (stylised, calibrated)
#   * synthetic hourly load / wind / solar profiles scaled to annual energy
#     targets  [SIMULATED: placeholder for EPIAS hourly series]
#   * merit-order dispatch: nuclear -> VRE/other -> hydro -> coal -> gas
#   * unit commitment by priority list; unit outputs; random forced outages
#   * contingency reserve requirement with a frequency-nadir (low-inertia)
#     term and a forecast-error (variability) term
# =============================================================================

ar1 <- function(n, phi, sd) {
  e <- rnorm(n, 0, sd * sqrt(1 - phi^2))
  as.numeric(stats::filter(e, phi, method = "recursive"))
}

calib_scale <- function(shape, target_mean, cap = 1) {
  # find a such that mean(pmin(a*shape, cap)) == target_mean
  f <- function(a) mean(pmin(a * shape, cap)) - target_mean
  uniroot(f, c(1e-6, 100 / max(mean(shape), 1e-9)), tol = 1e-10)$root
}

day_resample_index <- function(seed) {
  # seed 1 = actual 2025 calendar; other seeds resample whole days within the
  # same month (keeps seasonality, joint load/wind/solar/price patterns)
  d <- rep(1:365, each = 24)
  if (seed == 1) return(seq_len(HOURS))
  set.seed(seed + 7)
  mon <- as.integer(format(as.Date("2025-01-01") + 0:364, "%m"))
  newday <- vapply(1:365, function(k) { c <- which(mon == mon[k]); c[sample.int(length(c), 1)] }, 1L)
  as.vector(rbind(sapply(newday, function(nd) (nd - 1) * 24 + 1:24)))
}

make_profiles_real <- function(sc, seed) {
  idx <- day_resample_index(seed)
  t <- 0:(HOURS - 1); h <- t %% 24; d <- t %/% 24 + 1
  lshape <- REAL$load[idx]
  load <- lshape / mean(lshape) * sc$demand_twh * 1e6 / HOURS
  cap25 <- scenarios$S2025
  wshape <- pmax(REAL$wind[idx], 0) / (cap25$wind_gw * 1e3)
  aw <- calib_scale(wshape, sc$wind_twh * 1e6 / HOURS / (sc$wind_gw * 1e3), 0.92)
  wind <- pmin(aw * wshape, 0.92) * sc$wind_gw * 1e3
  sshape <- pmax(REAL$solar[idx], 0) / (cap25$solar_gw * 1e3)
  as_ <- calib_scale(sshape, sc$solar_twh * 1e6 / HOURS / (sc$solar_gw * 1e3), 1)
  solar <- pmin(as_ * sshape, 1) * sc$solar_gw * 1e3
  hshape <- pmax(REAL$hydro[idx], 0)
  hydro <- pmin(hshape / mean(hshape) * sc$hydro_twh * 1e6 / HOURS, sc$hydro_gw * 1e3 * 0.95)
  set.seed(seed)
  peps <- if ("res_price_usd" %in% names(REAL)) log(REAL$res_price_usd[idx]) else ar1(HOURS, price_par$phi, price_par$sd)
  data.table(t, h, d, load, wind, solar, peps, hydro_real = hydro)
}

make_profiles <- function(sc, seed) {
  if (exists("USE_REAL") && USE_REAL) return(make_profiles_real(sc, seed))
  set.seed(seed)
  t   <- 0:(HOURS - 1)
  h   <- t %% 24
  d   <- t %/% 24 + 1
  dow <- (d + 2) %% 7           # 1 Jan 2025 = Wednesday; 5=Sat, 6=Sun
  # ---- load ------------------------------------------------------------
  seas  <- 1 + 0.10 * cos(2 * pi * (d - 205) / 365) + 0.04 * cos(4 * pi * (d - 20) / 365)
  diur  <- 0.80 + 0.22 * exp(-((h - 13.5)^2) / 18) + 0.26 * exp(-((h - 20.5)^2) / 6) +
           0.05 * exp(-((h - 9)^2) / 4)
  wk    <- ifelse(dow == 5, 0.93, ifelse(dow == 6, 0.88, 1))
  lshape <- seas * diur * wk * exp(ar1(HOURS, 0.95, 0.025))
  load <- lshape / mean(lshape) * sc$demand_twh * 1e6 / HOURS          # MW
  # ---- wind ------------------------------------------------------------
  wseas <- 1 + 0.35 * cos(2 * pi * (d - 210) / 365) + 0.10 * cos(4 * pi * (d - 15) / 365)
  wdiur <- 1 + 0.15 * sin(2 * pi * (h - 9) / 24)
  wshape <- wseas * wdiur * exp(ar1(HOURS, 0.985, 0.60))
  aw <- calib_scale(wshape, sc$wind_twh * 1e6 / HOURS / (sc$wind_gw * 1e3), 0.92)
  wind <- pmin(aw * wshape, 0.92) * sc$wind_gw * 1e3
  # ---- solar -----------------------------------------------------------
  lat <- 39 * pi / 180
  dec <- 23.45 * pi / 180 * sin(2 * pi * (284 + d) / 365)
  cosz <- sin(lat) * sin(dec) + cos(lat) * cos(dec) * cos(pi / 12 * (h + 0.5 - 12.5))
  clear <- pmax(cosz, 0)^1.15
  kt_day <- pmin(pmax(0.62 + 0.18 * cos(2 * pi * (d - 190) / 365) +
                        ar1(365, 0.6, 0.15)[pmin(d, 365)], 0.1), 1)
  sshape <- clear * kt_day
  as_ <- calib_scale(sshape, sc$solar_twh * 1e6 / HOURS / (sc$solar_gw * 1e3), 1)
  solar <- pmin(as_ * sshape, 1) * sc$solar_gw * 1e3
  # ---- reserve price shock ---------------------------------------------
  peps <- ar1(HOURS, price_par$phi, price_par$sd)
  data.table(t, h, d, load, wind, solar, peps)
}

simulate_year <- function(scen_name, seed, sp = sec_par, pp = price_par,
                          fleet = NULL) {
  sc <- scenarios[[scen_name]]
  pr <- make_profiles(sc, seed)
  if (is.null(fleet)) fleet <- fleet_spec(scen_name)
  set.seed(seed + 1e5)
  ndays <- 365L

  # ---- availability of units (daily forced outages; nuclear refuelling) ----
  avail <- function(sizes, FOR) {
    if (length(sizes) == 0) return(matrix(0, 0, ndays))
    matrix(rbinom(length(sizes) * ndays, 1, 1 - FOR), nrow = length(sizes))
  }
  forv <- setNames(tech_par$FOR, tech_par$tech)
  A_coal <- avail(fleet$coal, forv["coal"])
  A_gas  <- avail(fleet$gas,  forv["gas"])
  A_hyd  <- avail(fleet$hydro_res, forv["hydro_res"])
  A_nuc  <- avail(fleet$nuclear, forv["nuclear"])
  if (length(fleet$nuclear) > 0) {
    for (i in seq_along(fleet$nuclear)) {      # 30-day refuelling outage
      s0 <- sample(1:335, 1); A_nuc[i, s0:(s0 + 29)] <- 0
      # forced outages last ~5 days rather than 1: smooth
      fo <- which(A_nuc[i, ] == 0 & !(seq_len(ndays) %in% s0:(s0 + 29)))
      for (f in fo) A_nuc[i, f:min(ndays, f + 4)] <- 0
    }
  }
  dd <- pmin(pr$d, ndays)

  # ---- dispatch -----------------------------------------------------------
  nuc_out_unit <- if (length(fleet$nuclear)) t(A_nuc[, dd, drop = FALSE] * fleet$nuclear) else matrix(0, HOURS, 0)
  nuc <- rowSums(nuc_out_unit)
  other <- rep(sc$other_twh * 1e6 / HOURS, HOURS)
  hyd_E <- sc$hydro_twh * 1e6
  hseas <- 1 + 0.45 * cos(2 * pi * (pr$d - 125) / 365)
  ror   <- 0.35 * hyd_E * hseas / sum(hseas)                 # run-of-river
  if ("hydro_real" %in% names(pr)) ror <- 0.35 * pr$hydro_real
  resid0 <- pr$load - nuc - other - ror - pr$wind - pr$solar
  # reservoir: daily energy budget shaped by residual load within the day
  res_day <- 0.65 * hyd_E * tapply(hseas, pr$d, sum) / sum(hseas)
  res_cap <- sum(fleet$hydro_res)
  hres <- numeric(HOURS)
  if ("hydro_real" %in% names(pr)) {
    hres <- pmin(0.65 * pr$hydro_real, res_cap)
  } else for (k in 1:365) {
    idx <- which(pr$d == k)
    w <- pmax(resid0[idx] - min(resid0[idx]), 0) + 1
    x <- res_day[k] * w / sum(w)
    hres[idx] <- pmin(x, res_cap)
  }
  resid1 <- resid0 - hres
  curtail <- pmax(-resid1, 0)
  resid1 <- pmax(resid1, 0)
  coal_cap_t <- colSums(A_coal * fleet$coal)[dd]
  theta <- uniroot(function(th) sum(pmin(resid1, th * coal_cap_t)) - sc$coal_twh * 1e6,
                   c(0, 1.5), extendInt = "yes")$root
  theta <- min(theta, 0.95)
  coal <- pmin(resid1, theta * coal_cap_t)
  gas_cap_t <- colSums(A_gas * fleet$gas)[dd]
  gas <- pmin(resid1 - coal, gas_cap_t)
  ens <- resid1 - coal - gas            # unserved -> assumed met by imports

  # ---- unit commitment by priority list (largest first) -------------------
  commit <- function(P, sizes, Amat, load_max = 0.85) {
    n <- length(sizes); out <- matrix(0, HOURS, n)
    if (n == 0) return(out)
    ord <- order(-sizes)
    for (tt in which(P > 0)) {
      cap <- sizes[ord] * Amat[ord, dd[tt]]
      cc <- cumsum(cap)
      k <- which(cc >= P[tt] / load_max)[1]
      if (is.na(k)) k <- n
      on <- ord[seq_len(k)][cap[seq_len(k)] > 0]
      out[tt, on] <- sizes[on] * P[tt] / sum(sizes[on])
    }
    out
  }
  U_coal <- commit(coal, fleet$coal, A_coal)
  U_gas  <- commit(gas,  fleet$gas,  A_gas)
  U_hyd  <- commit(hres, fleet$hydro_res, A_hyd)
  U_nuc  <- nuc_out_unit

  units <- rbindlist(lapply(c("nuclear", "coal", "gas", "hydro_res"), function(tc) {
    sz <- fleet[[tc]]
    if (length(sz)) data.table(tech = tc, size = sz) else NULL
  }))
  units[, uid := .I]
  P <- cbind(U_nuc, U_coal, U_gas, U_hyd)        # HOURS x units (MW output)

  # ---- inertia (MWs) ------------------------------------------------------
  Hv <- setNames(tech_par$H_s, tech_par$tech)
  online_mva <- (P > 0) %*% diag(units$size, nrow(units))  # committed rating
  E <- as.numeric(online_mva %*% Hv[units$tech]) +
       Hv["hydro_ror"] * ror / 0.7 + Hv["other"] * other / 0.7 + sp$E_import
  E <- pmax(E, sp$E_floor)

  # ---- reserve requirement -------------------------------------------------
  dPmax <- apply(P, 1, max)
  nadir_req <- function(dp) dp^2 * sp$Td * sp$f0 / (4 * sp$df_max * E)
  Rc  <- pmax(dPmax, nadir_req(dPmax))                 # contingency component
  sig_w <- sp$e_wind * pr$wind; sig_s <- sp$e_solar * pr$solar
  sig_l <- sp$e_load * pr$load
  sig <- sqrt(sig_w^2 + sig_s^2 + sig_l^2)
  Rv  <- sp$kappa_v * sp$z * sig                                     # variability component
  netload <- pr$load - pr$wind - pr$solar
  price <- if (exists("PRICE_SOURCE") && PRICE_SOURCE != "assumed") exp(pr$peps) else
           pp$p0 * pmax(netload / mean(netload), 0.05)^pp$beta * exp(pr$peps - pp$sd^2 / 2)
  list(scen = scen_name, seed = seed, units = units,
       wind_cap = sc$wind_gw * 1e3, solar_cap = sc$solar_gw * 1e3, P = P, E = E,
       Rc = Rc, Rv = Rv, dPmax = dPmax, price = price,
       sig_w = sig_w, sig_s = sig_s, sig_l = sig_l, sig = sig,
       hourly = data.table(t = pr$t, h = pr$h, d = pr$d, load = pr$load,
                           wind = pr$wind - curtail * pr$wind / pmax(pr$wind + pr$solar, 1),
                           solar = pr$solar - curtail * pr$solar / pmax(pr$wind + pr$solar, 1),
                           nuclear = nuc, coal = coal, gas = gas, hydro = ror + hres,
                           other = other, ror = ror, curtail = curtail, imports = ens,
                           E = E, dPmax = dPmax, Rc = Rc, Rv = Rv, price = price),
       nadir_fun = nadir_req)
}


## ---- 4. cost-allocation rules ----------------------------------------------
STEP <- "4. cost-allocation rules"
# =============================================================================
# 03_allocate.R  -  Cost-allocation rules
#   D1 Socialised: all reserve cost to consumers per MWh consumed (status quo
#      in Turkiye: recovered through the TEIAS system-operation charge)
#   D2 Pro-rata generator: capacity x forced-outage-rate of online units
#      [Strbac & Kirschen 2000]
#   D3 Causation-based, Shapley / "airport" rule [Littlechild & Owen 1973;
#      Badesa et al. 2025]: contingency cost shared along the sorted vector
#      of unit outputs; variability cost by Euler (marginal) shares of the
#      forecast-error variance (wind, solar, load)
#   D4 Causation-based, marginal "security charge": the whole contingency cost
#      is charged to the unit(s) that set the binding contingency - a stylised
#      analogue of the Lagrangian security charge of Ribeiro et al. [2026];
#      variability cost as in D3
# Returns allocations (USD) by technology and per nuclear unit.
# =============================================================================

allocate <- function(sim, sp = sec_par) {
  P <- sim$P; units <- sim$units; nU <- nrow(units)
  price <- sim$price; E <- sim$E
  Cc <- price * sim$Rc; Cv <- price * sim$Rv; C <- Cc + Cv
  kt <- sp$Td * sp$f0 / (4 * sp$df_max * E)
  cost_fun <- function(x, k) pmax(x, k * x^2)

  # ---------------- D3 airport / Shapley ----------------------------------
  shap <- numeric(nU)                       # contingency cost by unit (annual)
  marg <- numeric(nU)                       # D4
  for (tt in seq_len(HOURS)) {
    x <- P[tt, ]; on <- which(x > 0)
    if (!length(on)) next
    xo <- x[on]; o <- order(xo); xs <- xo[o]; n <- length(xs)
    cvals <- price[tt] * cost_fun(xs, kt[tt])
    inc <- diff(c(0, cvals))                # increments of the cost function
    share <- cumsum(inc / (n - seq_len(n) + 1))   # airport (Littlechild-Owen)
    shap[on[o]] <- shap[on[o]] + share
    mx <- which(xo >= max(xo) - 1e-6)        # D4: binding contingency
    marg[on[mx]] <- marg[on[mx]] + Cc[tt] / length(mx)
  }
  # variability (Euler shares of variance)
  s2 <- pmax(sim$sig^2, 1e-9)
  var_w <- sum(Cv * sim$sig_w^2 / s2)
  var_s <- sum(Cv * sim$sig_s^2 / s2)
  var_l <- sum(Cv * sim$sig_l^2 / s2)

  # energy by technology
  hr <- sim$hourly
  energy <- c(nuclear = sum(hr$nuclear), coal = sum(hr$coal), gas = sum(hr$gas),
              hydro = sum(hr$hydro), other = sum(hr$other),
              wind = sum(hr$wind), solar = sum(hr$solar))
  consumption <- sum(hr$load)

  # ---------------- D2 pro-rata capacity x FOR ----------------------------
  forv <- setNames(tech_par$FOR, tech_par$tech)
  online <- (P > 0)
  w_units <- sweep(online, 2, units$size * forv[units$tech], `*`)   # HOURS x nU
  w_other <- cbind(hydro = forv["hydro_ror"] * hr$ror / 0.7,
                   other = forv["other"] * hr$other / 0.7,
                   wind  = forv["wind"]  * sim$wind_cap,
                   solar = forv["solar"] * sim$solar_cap)
  wtot <- rowSums(w_units) + rowSums(w_other)
  d2_units <- colSums(w_units * (C / wtot))
  d2_other <- colSums(w_other * (C / wtot))

  agg <- function(v) tapply(v, units$tech, sum)
  techs <- c("nuclear", "coal", "gas", "hydro", "wind", "solar", "other", "consumers")
  mk <- function() setNames(numeric(length(techs)), techs)

  D1 <- mk(); D1["consumers"] <- sum(C)
  D2 <- mk(); a <- agg(d2_units)
  for (n in names(a)) D2[ifelse(n == "hydro_res", "hydro", n)] <- D2[ifelse(n == "hydro_res", "hydro", n)] + a[n]
  D2["hydro"] <- D2["hydro"] + d2_other[1]; D2["other"] <- d2_other[2]
  D2["wind"] <- d2_other[3]; D2["solar"] <- d2_other[4]
  D3 <- mk(); a <- agg(shap)
  for (n in names(a)) D3[ifelse(n == "hydro_res", "hydro", n)] <- D3[ifelse(n == "hydro_res", "hydro", n)] + a[n]
  D3["wind"] <- var_w; D3["solar"] <- var_s; D3["consumers"] <- var_l
  D4 <- mk(); a <- agg(marg)
  for (n in names(a)) D4[ifelse(n == "hydro_res", "hydro", n)] <- D4[ifelse(n == "hydro_res", "hydro", n)] + a[n]
  D4["wind"] <- var_w; D4["solar"] <- var_s; D4["consumers"] <- var_l

  alloc <- rbindlist(lapply(c("D1", "D2", "D3", "D4"), function(r) {
    v <- get(r); data.table(rule = r, tech = names(v), cost = as.numeric(v))
  }))
  alloc[, energy_mwh := c(energy, consumers = consumption)[tech]]
  stopifnot(all(abs(alloc[, sum(cost), by = rule]$V1 - sum(C)) < 1e-6 * sum(C)))

  nuc_ids <- units[tech == "nuclear", uid]
  nuc_unit <- if (length(nuc_ids)) data.table(
    uid = nuc_ids, energy_mwh = colSums(P[, nuc_ids, drop = FALSE]),
    D2 = d2_units[nuc_ids], D3 = shap[nuc_ids], D4 = marg[nuc_ids]) else NULL

  list(alloc = alloc, total = sum(C), total_c = sum(Cc), total_v = sum(Cv),
       nuc_unit = nuc_unit,
       summary = data.table(scen = sim$scen, seed = sim$seed,
                            cost_total = sum(C), cost_cont = sum(Cc), cost_var = sum(Cv),
                            mean_R = mean(sim$Rc + sim$Rv), mean_Rc = mean(sim$Rc),
                            mean_Rv = mean(sim$Rv), p95_R = quantile(sim$Rc + sim$Rv, 0.95),
                            nadir_bind = mean(sim$Rc > sim$dPmax + 1e-6),
                            mean_dP = mean(sim$dPmax), mean_E = mean(E),
                            consumption = consumption))
}


## ---- 5. Monte Carlo, tables and figures ------------------------------------
STEP <- "5. Monte Carlo, tables and figures"
# =============================================================================
# 04_run.R  -  Monte Carlo runs, experiments, tables and figures
# Run from anywhere:  Rscript R/main.R   (main.R sets ROOT and sources all)
# =============================================================================
library(data.table); library(ggplot2); library(scales); library(parallel)

ncores <- if (interactive() || .Platform$OS.type == "windows") 1L else max(1L, detectCores())  # forking is unsafe inside RStudio
run_mc <- function(scen, seeds, sp = sec_par, pp = price_par, fleet = NULL, tag = "base") {
  res <- mclapply(seeds, function(s) {
    sim <- simulate_year(scen, s, sp, pp, fleet)
    a <- allocate(sim, sp)
    a$alloc[, `:=`(scen = scen, seed = s, tag = tag)]
    a$summary[, tag := tag]
    nu <- a$nuc_unit; if (!is.null(nu)) nu[, `:=`(scen = scen, seed = s, tag = tag)]
    list(alloc = a$alloc, summary = a$summary, nuc = nu,
         hourly = if (s == seeds[1]) sim$hourly[, scen := scen] else NULL)
  }, mc.cores = ncores)
  list(alloc = rbindlist(lapply(res, `[[`, "alloc")),
       summary = rbindlist(lapply(res, `[[`, "summary")),
       nuc = rbindlist(lapply(res, `[[`, "nuc")),
       hourly = rbindlist(lapply(res, `[[`, "hourly")))
}
q05 <- function(x) quantile(x, 0.05, na.rm = TRUE); q95 <- function(x) quantile(x, 0.95, na.rm = TRUE)

# ---------------------------------------------------------------- calibration to REAL reserve volume
KAPPA_INFO <- "not calibrated (synthetic data)"
if (exists("USE_REAL") && USE_REAL && "res_amt" %in% names(REAL)) {
  s0 <- simulate_year("S2025", 1, modifyList(sec_par, list(kappa_v = 1)))
  target <- mean(REAL$res_amt)
  k <- (target - mean(s0$Rc)) / mean(s0$Rv)
  if (!is.finite(k) || k <= 0.05) k <- 0.05
  sec_par$kappa_v <- k
  KAPPA_INFO <- sprintf("kappa_v = %.3f: 2025 mean requirement calibrated to actual PFC+SFC %.0f MW (contingency %.0f MW + variability %.0f MW)",
                        k, target, mean(s0$Rc), k * mean(s0$Rv))
  cat(KAPPA_INFO, "\n"); dlog("    ", KAPPA_INFO); writeLines(diag_lines, DIAG)
}

# ---------------------------------------------------------------- base runs
seeds <- seq_len(N_MC)
cat("Tables will be written to:", normalizePath(file.path(ROOT, "tables")), "\n")
base <- lapply(names(scenarios), function(s) {
  cat(format(Sys.time(), "%H:%M:%S"), "Monte Carlo:", s, "(", length(seeds), "years ) ...\n")
  run_mc(s, seeds)
})
A  <- rbindlist(lapply(base, `[[`, "alloc"))
S  <- rbindlist(lapply(base, `[[`, "summary"))
NU <- rbindlist(lapply(base, `[[`, "nuc"))
HR <- rbindlist(lapply(base, `[[`, "hourly"))
scen_lab <- c(S2025 = "2025", S2030 = "2030", S2035 = "2035")

# ---------------------------------------------------------------- Table 2: calibration
act <- fread(file.path(ROOT, "tables", "T0_turkiye_generation_2010_2025_OWID.csv"))[year == 2025]
h25 <- HR[scen == "S2025"]
cal <- data.table(
  item = c("Total generation", "Coal", "Gas and oil", "Hydro", "Wind", "Solar", "Other renewables"),
  actual_TWh = c(act$total, act$coal, act$gas + act$oil, act$hydro, act$wind, act$solar, act$other),
  simulated_TWh = c(sum(h25$load), sum(h25$coal), sum(h25$gas), sum(h25$hydro),
                    sum(h25$wind), sum(h25$solar), sum(h25$other)) / 1e6)
cal[, dev_pct := 100 * (simulated_TWh / actual_TWh - 1)]
fwrite(cal[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)],
       file.path(ROOT, "tables", "T2_calibration_2025.csv"))

# ---------------------------------------------------------------- Table 8: validation vs REAL 2025 (only with EPIAS data)
if (exists("USE_REAL") && USE_REAL) {
  s25 <- S[scen == "S2025"]
  v <- data.table(
    indicator = c("EPIAS hourly consumption (TWh)", "EPIAS hourly generation: coal (TWh)",
                  "EPIAS hourly generation: gas+oil (TWh)", "EPIAS hourly generation: hydro (TWh)",
                  "EPIAS hourly generation: wind (TWh)", "EPIAS hourly generation: solar (TWh)",
                  "Model hourly wind error, sd (% of output, assumed)", "Model hourly solar error, sd (% of output, assumed)",
                  "Correlation of hourly coal output, model vs EPIAS", "Correlation of hourly gas output, model vs EPIAS"),
    value = c(sum(REAL$load) / 1e6, sum(REAL$coal) / 1e6, sum(REAL$gas) / 1e6, sum(REAL$hydro) / 1e6,
              sum(REAL$wind) / 1e6, sum(REAL$solar) / 1e6, 100 * sec_par$e_wind, 100 * sec_par$e_solar,
              suppressWarnings(cor(h25$coal, REAL$coal)), suppressWarnings(cor(h25$gas, REAL$gas))))
  me <- if (exists("MEASURED_ERRORS")) MEASURED_ERRORS else list()
  v <- rbind(v, data.table(indicator = c("Measured day-ahead wind error, sd (% of mean output; diagnostic)",
                                         "Measured day-ahead solar error, sd (% of mean output; diagnostic)"),
                           value = c(100 * (if (is.null(me$e_wind)) NA else me$e_wind),
                                     100 * (if (is.null(me$e_solar)) NA else me$e_solar))))
  if ("res_amt" %in% names(REAL)) v <- rbind(v, data.table(
    indicator = c("Actual PFC+SFC volume, mean (MW)", "Modelled upward requirement, mean (MW)",
                  "Actual PFC+SFC capacity cost (USD m)", "Modelled reserve cost (USD m)",
                  "Actual mean reserve price (USD/MW-h)"),
    value = c(mean(REAL$res_amt), mean(s25$mean_R), sum(REAL$res_cost_tl / REAL$usdtry) / 1e6,
              mean(s25$cost_total) / 1e6, mean(REAL$res_price_usd))))
  fwrite(v[, .(indicator, value = round(value, 2))], file.path(ROOT, "tables", "T8_validation_real_2025.csv"))
  print(v)
}

# ---------------------------------------------------------------- Table 3: requirement & cost
T3 <- S[, .(total_cost_musd = mean(cost_total) / 1e6,
            total_cost_lo = q05(cost_total) / 1e6, total_cost_hi = q95(cost_total) / 1e6,
            contingency_share_pct = 100 * mean(cost_cont / cost_total),
            mean_req_MW = mean(mean_R), mean_Rc_MW = mean(mean_Rc), mean_Rv_MW = mean(mean_Rv),
            p95_req_MW = mean(p95_R),
            nadir_binding_pct = 100 * mean(nadir_bind),
            mean_largest_cont_MW = mean(mean_dP), mean_inertia_GWs = mean(mean_E) / 1e3,
            D1_usd_per_mwh = mean(cost_total / consumption)), by = scen]
fwrite(T3[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)],
       file.path(ROOT, "tables", "T3_requirement_and_cost.csv"))

# ---------------------------------------------------------------- Table 4: allocation
A[, share := cost / sum(cost), by = .(scen, seed, rule)]
A[, eshare := ifelse(tech == "consumers", NA_real_,
                     energy_mwh / sum(energy_mwh[tech != "consumers"])), by = .(scen, seed, rule)]
T4 <- A[, .(cost_musd = mean(cost) / 1e6,
            usd_per_mwh = mean(ifelse(energy_mwh > 0, cost / energy_mwh, NA_real_)), lo = q05(cost / energy_mwh), hi = q95(cost / energy_mwh),
            cost_share_pct = 100 * mean(share), energy_share_pct = 100 * mean(eshare)),
        by = .(scen, rule, tech)]
T4[, disproportionality := cost_share_pct / energy_share_pct]
fwrite(T4[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)],
       file.path(ROOT, "tables", "T4_allocation_by_rule_tech.csv"))
T4w <- dcast(T4[tech != "other" | rule == "D2"], scen + tech ~ rule, value.var = "usd_per_mwh")
fwrite(T4w[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)],
       file.path(ROOT, "tables", "T4b_usd_per_mwh_wide.csv"))

# ---------------------------------------------------------------- Table 5: nuclear units
T5 <- NU[, .(D2_musd = mean(D2) / 1e6, D3_musd = mean(D3) / 1e6, D4_musd = mean(D4) / 1e6,
             D2_usd_mwh = mean(D2 / energy_mwh), D3_usd_mwh = mean(D3 / energy_mwh),
             D4_usd_mwh = mean(D4 / energy_mwh),
             D3_lo = q05(D3 / energy_mwh), D3_hi = q95(D3 / energy_mwh)), by = scen]
fwrite(T5[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)],
       file.path(ROOT, "tables", "T5_nuclear_unit_charges.csv"))

# ---------------------------------------------------------------- Experiment: unit size (2030)
cat(format(Sys.time(), "%H:%M:%S"), "Unit-size experiment (2030) ...\n")
size_runs <- rbindlist(lapply(c(300, 600, 1200), function(sz) {
  fl <- fleet_spec("S2030"); fl$nuclear <- rep(sz, 2400 / sz)
  r <- run_mc("S2030", seq_len(min(N_MC, 20)), fleet = fl, tag = paste0("size", sz))
  nu <- r$nuc[, .(D3 = sum(D3), D4 = sum(D4), E = sum(energy_mwh)), by = seed]
  data.table(unit_MW = sz, n_units = 2400 / sz,
             total_cost_musd = mean(r$summary$cost_total) / 1e6,
             cont_cost_musd = mean(r$summary$cost_cont) / 1e6,
             mean_Rc_MW = mean(r$summary$mean_Rc),
             nadir_binding_pct = 100 * mean(r$summary$nadir_bind),
             nuclear_D3_usd_mwh = mean(nu$D3 / nu$E), nuclear_D4_usd_mwh = mean(nu$D4 / nu$E))
}))
fwrite(size_runs[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)],
       file.path(ROOT, "tables", "T6_unit_size_experiment_2030.csv"))

# ---------------------------------------------------------------- Sensitivity (2030)
sens_def <- list(
  "Baseline" = list(),
  "Nadir limit 0.5 Hz" = list(df_max = 0.5),
  "Nadir limit 1.0 Hz" = list(df_max = 1.0),
  "FCR delivery time 30 s" = list(Td = 30),
  "FCR delivery time 30 s + interconnection inertia +100 GWs" = list(Td = 30, E_import = 1e5),
  "FCR delivery time 30 s + interconnection inertia +200 GWs" = list(Td = 30, E_import = 2e5),
  "Interconnection inertia +100 GWs" = list(E_import = 1e5),
  "Inertia floor 90 GWs" = list(E_floor = 9e4),
  "Forecast errors x0.5" = list(e_wind = 0.04, e_solar = 0.03, e_load = 0.005),
  "Forecast errors x1.5" = list(e_wind = 0.12, e_solar = 0.09, e_load = 0.015),
  "z = 1.645 (95 %)" = list(z = 1.645))
cat(format(Sys.time(), "%H:%M:%S"), "Sensitivity analysis (2030) ...\n")
sens <- rbindlist(lapply(names(sens_def), function(nm) {
  cat("   ", nm, "\n")
  sp <- modifyList(sec_par, sens_def[[nm]])
  r <- run_mc("S2030", seq_len(min(N_MC, 15)), sp = sp, tag = nm)
  al <- r$alloc[, share := cost / sum(cost), by = .(seed, rule)]
  data.table(case = nm, total_cost_musd = mean(r$summary$cost_total) / 1e6,
             contingency_share_pct = 100 * mean(r$summary$cost_cont / r$summary$cost_total),
             nadir_binding_pct = 100 * mean(r$summary$nadir_bind),
             nuclear_share_D3_pct = 100 * al[rule == "D3" & tech == "nuclear", mean(share)],
             nuclear_D3_usd_mwh = al[rule == "D3" & tech == "nuclear", mean(cost / energy_mwh)],
             consumer_D1_usd_mwh = al[rule == "D1" & tech == "consumers", mean(cost / energy_mwh)],
             consumer_D3_usd_mwh = al[rule == "D3" & tech == "consumers", mean(cost / energy_mwh)])
}))
fwrite(sens[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)],
       file.path(ROOT, "tables", "T7_sensitivity_2030.csv"))

cat(format(Sys.time(), "%H:%M:%S"), "Tables written:", paste(list.files(file.path(ROOT, "tables")), collapse = ", "), "\n")
fig_ok <- tryCatch({
# ================================================================ FIGURES
pal <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
theme_pje <- theme_minimal(base_size = 10, base_family = "serif") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
        legend.position = "bottom", strip.text = element_text(face = "bold"),
        plot.title = element_blank())
save_fig <- function(p, name, w = 16, h = 9) {
  ggsave(file.path(ROOT, "figures", paste0(name, ".png")), p, width = w, height = h,
         units = "cm", dpi = 300, bg = "white")
}

# Fig 1: REAL generation mix
f1d <- melt(act <- fread(file.path(ROOT, "tables", "T0_turkiye_generation_2010_2025_OWID.csv"))[
  , .(year, Coal = coal, `Gas & oil` = gas + oil, Hydro = hydro, Wind = wind, Solar = solar,
      `Other RES` = other)], id.vars = "year", variable.name = "src", value.name = "twh")
p1 <- ggplot(f1d, aes(year, twh, fill = src)) +
  geom_area(colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = pal[1:6], name = NULL) +
  scale_x_continuous(breaks = seq(2010, 2025, 3)) +
  labs(x = NULL, y = "Electricity generation (TWh)") + theme_pje
save_fig(p1, "Fig1_generation_mix_Turkiye_2010_2025")

# Fig 2: duration curves of total reserve requirement (seed 1)
f2d <- HR[, .(R = sort(Rc + Rv, decreasing = TRUE), hour = seq_len(.N)), by = scen]
f2d[, Scenario := scen_lab[scen]]
p2 <- ggplot(f2d, aes(hour, R / 1e3, colour = Scenario)) + geom_line(linewidth = 0.7) +
  scale_colour_manual(values = pal[1:3]) +
  labs(x = "Hours of the year (sorted)", y = "Upward reserve requirement (GW)", colour = NULL) +
  theme_pje
save_fig(p2, "Fig2_reserve_duration_curves")

# Fig 3: cost shares by payer, rule and scenario
payer_lab <- c(consumers = "Consumers", coal = "Coal", gas = "Gas", hydro = "Hydro",
               nuclear = "Nuclear", wind = "Wind", solar = "Solar", other = "Other RES")
f3d <- T4[, .(scen, rule, tech, share = cost_share_pct)]
f3d[, Payer := factor(payer_lab[tech], levels = payer_lab)]
f3d[, Scenario := scen_lab[scen]]
rule_lab <- c(D1 = "D1 Socialised", D2 = "D2 Pro-rata", D3 = "D3 Shapley", D4 = "D4 Marginal")
f3d[, Rule := factor(rule_lab[rule], levels = rev(rule_lab))]
p3 <- ggplot(f3d, aes(share, Rule, fill = Payer)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.4) +
  facet_wrap(~Scenario, ncol = 1) +
  scale_fill_manual(values = setNames(pal, payer_lab), name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = "Share of annual reserve cost", y = NULL) + theme_pje +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3)) +
  theme(plot.margin = margin(5, 12, 5, 5)) + guides(fill = guide_legend(nrow = 2))
save_fig(p3, "Fig4_cost_shares_by_rule", h = 13)

# Fig 4: USD/MWh by technology, D2-D4, 2030 & 2035 with 90% MC intervals
f4d <- T4[rule %in% c("D2", "D3", "D4") & tech %in% c("nuclear", "coal", "gas", "hydro", "wind", "solar") &
          scen != "S2025"]
f4d[, `:=`(Tech = factor(payer_lab[tech], levels = payer_lab), Rule = factor(rule_lab[rule], levels = rule_lab),
           Scenario = scen_lab[scen])]
p4 <- ggplot(f4d, aes(Tech, usd_per_mwh, colour = Rule)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.55), size = 0.3) +
  facet_wrap(~Scenario) + scale_colour_manual(values = pal[2:4], name = NULL) +
  labs(x = NULL, y = "Reserve charge (USD per MWh generated)") + theme_pje
save_fig(p4, "Fig5_charge_per_mwh_by_technology")

# Fig 5: unit-size experiment
f5d <- melt(size_runs[, .(unit_MW, `Total reserve cost (USD m)` = total_cost_musd,
                          `Nuclear D3 charge (USD/MWh)` = nuclear_D3_usd_mwh)], id.vars = "unit_MW")
p5 <- ggplot(f5d, aes(factor(unit_MW), value)) +
  geom_col(width = 0.55, fill = pal[1]) +
  geom_text(aes(label = round(value, 1)), vjust = -0.4, size = 3, family = "serif") +
  facet_wrap(~variable, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Nuclear unit size in 2030 (MW; 2,400 MW installed in all cases)", y = NULL) + theme_pje
save_fig(p5, "Fig6_unit_size_experiment")

# Fig 6: diurnal inertia & contingency requirement
f6d <- HR[, .(`System inertia (GWs)` = mean(E) / 1e3, `Contingency reserve (GW)` = mean(Rc) / 1e3), by = .(scen, h)]
f6d <- melt(f6d, id.vars = c("scen", "h")); f6d[, Scenario := scen_lab[scen]]
p6 <- ggplot(f6d, aes(h, value, colour = Scenario)) + geom_line(linewidth = 0.7) +
  facet_wrap(~variable, scales = "free_y") + scale_colour_manual(values = pal[1:3], name = NULL) +
  scale_x_continuous(breaks = seq(0, 23, 6)) +
  labs(x = "Hour of day", y = NULL) + theme_pje
save_fig(p6, "Fig3_diurnal_inertia_and_contingency_reserve")

TRUE }, error = function(e) { cat("!! FIGURE STEP FAILED:", conditionMessage(e), "\n   (tables are already saved)\n"); FALSE })
if (fig_ok) cat(format(Sys.time(), "%H:%M:%S"), "Figures written to:", normalizePath(file.path(ROOT, "figures")), "\n")
saveRDS(list(A = A, S = S, NU = NU, T3 = T3, T4 = T4, T5 = T5, size = size_runs, sens = sens,
             cal = cal, scenarios = scenarios, sec_par = sec_par, KAPPA_INFO = KAPPA_INFO,
             REAL = if (exists("REAL")) REAL else NULL,
             MEASURED_ERRORS = if (exists("MEASURED_ERRORS")) MEASURED_ERRORS else NULL,
             T8 = if (file.exists(file.path(ROOT, "tables", "T8_validation_real_2025.csv")))
                    fread(file.path(ROOT, "tables", "T8_validation_real_2025.csv")) else NULL),
        file.path(ROOT, "data", "results.rds"))
cat("Done.\n")


## ---- 6. Word file with tables and figures ----------------------------------
STEP <- "6. Word file with tables and figures"
tryCatch({
# =============================================================================
# 06_word_report.R  -  Put all result tables and figures into one Word file:
#                      results/PJE_results_tables_figures.docx
# Uses the CSVs in tables/ and the PNGs in figures/ written by the analysis.
# =============================================================================
for (p in c("officer", "flextable"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(officer); library(flextable); library(data.table)

dir.create(file.path(ROOT, "results"), showWarnings = FALSE)
docx_path <- file.path(ROOT, "results", "PJE_results_tables_figures.docx")

tab_spec <- list(
  T2 = list(f = "T2_calibration_2025.csv",
            cap = "Table 2. Calibration: observed versus simulated generation in 2025 (TWh)"),
  T3 = list(f = "T3_requirement_and_cost.csv",
            cap = "Table 3. Reserve requirement and cost by scenario (Monte Carlo means)"),
  T4 = list(f = "T4b_usd_per_mwh_wide.csv",
            cap = "Table 4. Reserve charge by payer and allocation rule (USD/MWh of own generation or consumption)"),
  T5 = list(f = "T5_nuclear_unit_charges.csv",
            cap = "Table 5. Annual reserve charges per Akkuyu unit"),
  T6 = list(f = "T6_unit_size_experiment_2030.csv",
            cap = "Table 6. Unit-size experiment, 2030 (2,400 MW of nuclear capacity in all cases)"),
  T7 = list(f = "T7_sensitivity_2030.csv",
            cap = "Table 7. Sensitivity analysis, 2030 scenario"),
  T8 = list(f = "T8_validation_real_2025.csv",
            cap = "Table 8. Validation against real EPIAS data, 2025"),
  T4full = list(f = "T4_allocation_by_rule_tech.csv",
            cap = "Table A1. Full allocation results by scenario, rule and payer")
)
fig_spec <- list(
  c("Fig1_generation_mix_Turkiye_2010_2025.png", "Figure 1. Electricity generation in Turkiye by source, 2010-2025 (TWh)"),
  c("Fig2_reserve_duration_curves.png", "Figure 2. Duration curves of the hourly upward reserve requirement"),
  c("Fig3_diurnal_inertia_and_contingency_reserve.png", "Figure 3. Average system inertia and contingency reserve by hour of day"),
  c("Fig4_cost_shares_by_rule.png", "Figure 4. Distribution of annual reserve cost among payers under the four rules", "1.23"),
  c("Fig5_charge_per_mwh_by_technology.png", "Figure 5. Reserve charge per MWh by technology, rules D2-D4"),
  c("Fig6_unit_size_experiment.png", "Figure 6. Total reserve cost and nuclear charge by unit size, 2030")
)

fmt_tab <- function(d) {
  d <- as.data.frame(d)
  for (j in seq_along(d)) if (is.numeric(d[[j]])) d[[j]] <- round(d[[j]], 2)
  ft <- flextable(d)
  ft <- font(ft, fontname = "Times New Roman", part = "all")
  ft <- fontsize(ft, size = 9, part = "all")
  ft <- bold(ft, part = "header")
  ft <- theme_booktabs(ft)
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  ft
}

doc <- read_docx()
doc <- body_add_par(doc, "Results: tables and figures", style = "heading 1")
doc <- body_add_par(doc, paste("Generated", format(Sys.time(), "%Y-%m-%d %H:%M"),
                               "| data mode:", if (exists("USE_REAL") && USE_REAL) "REAL EPIAS 2025" else "SYNTHETIC",
                               if (exists("KAPPA_INFO")) paste("|", KAPPA_INFO) else ""), style = "Normal")
for (t in tab_spec) {
  f <- file.path(ROOT, "tables", t$f)
  if (!file.exists(f)) next
  doc <- body_add_par(doc, t$cap, style = "Normal")
  doc <- body_add_flextable(doc, fmt_tab(fread(f)))
  doc <- body_add_par(doc, "Source: own calculations in R.", style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")
}
for (g in fig_spec) {
  f <- file.path(ROOT, "figures", g[1])
  if (!file.exists(f)) next
  asp <- if (length(g) >= 3) as.numeric(g[3]) else 9 / 16
  doc <- body_add_img(doc, src = f, width = if (asp > 1) 5.2 else 6.3, height = (if (asp > 1) 5.2 else 6.3) * asp)
  doc <- body_add_par(doc, paste(g[2], "Source: own calculations in R."), style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")
}
print(doc, target = docx_path)
cat("Word file with all tables and figures:", normalizePath(docx_path), "\n")

}, error = function(e) cat("!! Word export failed:", conditionMessage(e), "\n   (CSV tables and PNG figures are saved)\n"))


STEP <- "finish"
writeLines(capture.output(sessionInfo()), file.path(ROOT, "data", "sessionInfo.txt"))
options(error = NULL)
cat("\nFinished. Data mode:", if (USE_REAL) "REAL EPIAS 2025" else "SYNTHETIC", "| price:", PRICE_SOURCE, "\n")
cat("Send back: data/results.rds, data/real_data_diagnostics.txt and results/PJE_results_tables_figures.docx\n")
