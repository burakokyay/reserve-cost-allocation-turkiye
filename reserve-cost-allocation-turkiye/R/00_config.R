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
