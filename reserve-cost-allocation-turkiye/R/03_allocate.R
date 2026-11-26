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
