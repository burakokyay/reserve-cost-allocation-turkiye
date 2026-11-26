# Reserve cost allocation in Türkiye – replication code

Replication code by Burak Okyay (Mudanya University) for "Who pays for the largest contingency? Reserve cost allocation and nuclear unit size in Türkiye's low-inertia transition".

## Requirements
R ≥ 4.3 with data.table, ggplot2, scales, httr2, jsonlite, officer and flextable.

## Steps
1. **Data.** Set `EPIAS_USERNAME` and `EPIAS_PASSWORD` (a free EPİAŞ Transparency Platform account) in `~/.Renviron`, then run `Rscript R/05_fetch_real_data.R . 2025-01-01 2025-12-31`. This downloads the hourly EPİAŞ series, ECB exchange rates and the Our World in Data energy dataset into `data/raw/` and `data/`. Credentials are read from environment variables only.
2. **Fleet.** Run `Rscript fleet_from_epias_plants.R .` to derive unit sizes and coal trip rates from plant-level output (`data/fleet_observed.rds`, also included here).
3. **Model.** Run `Rscript run_analysis_real_data.R` from this folder. It reproduces all tables (`tables/`), figures (`figures/`) and a Word file with both, in about 3 minutes on 4 cores.

`results/` contains the tables and figures reported in the paper. `R/` holds the same code split into modules: `00_config.R` (scenarios and parameters), `01_data.R`/`01b_real_data.R` (data), `02_model.R` (hourly model and reserve requirement), `03_allocate.R` (allocation rules), `04_run.R` (Monte Carlo, experiments, figures) and `06_word_report.R`.

Raw EPİAŞ data are not redistributed here; they are public and are downloaded by step 1.

## Licence
Code: MIT License (see `LICENSE`). Derived data, tables and figures in `data/` and `results/`: Creative Commons Attribution 4.0 International (CC BY 4.0). Raw EPİAŞ, ECB and Our World in Data inputs remain under their providers' terms.
