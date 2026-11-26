# =============================================================================
# 04_run.R  -  Monte Carlo runs, experiments, tables and figures
# Run from anywhere:  Rscript R/main.R   (main.R sets ROOT and sources all)
# =============================================================================
library(data.table); library(ggplot2); library(scales); library(parallel)

ncores <- max(1L, detectCores() - 0L)
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
