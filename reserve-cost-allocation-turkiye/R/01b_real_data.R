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
