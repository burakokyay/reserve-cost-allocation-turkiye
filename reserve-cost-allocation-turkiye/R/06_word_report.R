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
