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
source(file.path(ROOT, "R", "01b_real_data.R"))
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
