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
