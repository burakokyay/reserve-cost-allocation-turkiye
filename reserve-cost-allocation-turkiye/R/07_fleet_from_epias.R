# =============================================================================
# 07_fleet_from_epias.R  -  Replace the ASSUMED unit fleet and outage rates
#                           with values derived from EPIAS plant-level data.
#
# Run from your working directory (the folder that contains data/raw/):
#     source("07_fleet_from_epias.R")        or   Rscript 07_fleet_from_epias.R .
#
# Inputs (written earlier by the data-fetch script):
#     data/raw/realtime_generation_by_plant.csv   hourly MWh per power plant
#     data/raw/powerplant_list.csv                plant ids and names
# Optional input you fill in by hand (see step 4):
#     data/fleet_units_override.csv               number of units per plant
#
# Outputs:
#     data/fleet_plants_2025.csv     one row per plant: technology, observed
#                                    maximum output, units, unit size, trips
#     data/fleet_summary_2025.csv    per technology: capacity, units, FOR
#     data/largest_online_2025.csv   hourly output of the largest online plant
#     data/fleet_observed.rds        read automatically by the main analysis
#                                    script, which then uses these unit sizes
#                                    and outage rates instead of the assumed ones
#
# Limits of plant-level data (state them in the paper):
#   * EPIAS publishes output per PLANT, not per UNIT. A plant's maximum hourly
#     output is its online capacity, but a plant with several blocks is several
#     contingencies. Step 4 splits plants into units; check the largest plants
#     against the EPDK licence register (unit list on each licence page).
#   * The forced-outage rate is estimated from sudden trips (output falls from
#     >= 50% to <= 10% of the plant maximum within one hour), so economic
#     shut-downs, which ramp down over several hours, are not counted.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
ROOT <- if (length(args) >= 1) args[1] else if (exists("ROOT")) ROOT else getwd()
RAW  <- file.path(ROOT, "data", "raw")
OUTD <- file.path(ROOT, "data")
say  <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

f_gen <- file.path(RAW, "realtime_generation_by_plant.csv")
f_pp  <- file.path(RAW, "powerplant_list.csv")
if (!file.exists(f_gen)) stop("Not found: ", f_gen, "\nRun the data-fetch script with FETCH_PLANT_LEVEL <- TRUE first.")

# ---------------------------------------------------------------- 1. read
g <- fread(f_gen)
say("plant-level file:", nrow(g), "rows,", ncol(g), "columns")
cat("Columns:", paste(names(g), collapse = ", "), "\n")
nm <- tolower(gsub("[^A-Za-z]", "", names(g)))

pick <- function(pattern, required = TRUE) {
  i <- grep(pattern, nm)
  if (!length(i) && required) stop("Could not find a column matching '", pattern,
                                   "'. Paste the 'Columns:' line above into the chat.")
  if (length(i)) names(g)[i[1]] else NA_character_
}
c_id   <- pick("^powerplantid$|^ppid$|^plantid$|^santralid$|^id$", required = FALSE)
if (is.na(c_id)) c_id <- pick("^powerplantname$|^plantname$|^santraladi$|^name$")   # file keyed by plant name
KEY_IS_NAME <- grepl("name|adi", tolower(c_id))
c_date <- pick("^date$|^tarih$")
c_hour <- pick("^hour$|^saat$", required = FALSE)
c_tot  <- pick("^total$|^toplam$", required = FALSE)

# fuel columns -> model technologies
fuel_map <- c(
  naturalgas = "gas", lng = "gas", dogalgaz = "gas",
  lignite = "coal", linyit = "coal", importcoal = "coal", importedcoal = "coal",
  ithalkomur = "coal", blackcoal = "coal", hardcoal = "coal", taskomur = "coal",
  asphaltitecoal = "coal", asfaltitkomur = "coal",
  dammedhydro = "hydro_res", barajli = "hydro_res", river = "hydro_ror", akarsu = "hydro_ror",
  nuclear = "nuclear", nucklear = "nuclear", nukleer = "nuclear",
  wind = "wind", ruzgar = "wind", sun = "solar", gunes = "solar",
  geothermal = "other", jeotermal = "other", biomass = "other", biyokutle = "other",
  fueloil = "other", fuelOil = "other", naphta = "other", wasteheat = "other",
  atikisi = "other")
fuel_cols <- names(g)[nm %in% names(fuel_map)]
fuel_tech <- fuel_map[nm[nm %in% names(fuel_map)]]
if (!length(fuel_cols) && is.na(c_tot)) stop("No fuel or total columns found; paste the 'Columns:' line into the chat.")

# ---------------------------------------------------------------- 2. hourly series
dt_parse <- function(x) {
  if (inherits(x, "POSIXct")) return(as.POSIXct(format(x, tz = "Etc/GMT-3"), tz = "Etc/GMT-3"))
  as.POSIXct(substr(as.character(x), 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-3")
}
g[, ts := dt_parse(get(c_date))]
if (all(is.na(g$ts))) g[, ts := as.POSIXct(as.Date(get(c_date)), tz = "Etc/GMT-3")]
# add the hour column only if the date column holds the day (all times at midnight)
if (!is.na(c_hour) && all(format(g$ts[seq(1, nrow(g), length.out = min(nrow(g), 5000))], "%H") == "00", na.rm = TRUE))
  g[, ts := ts + 3600 * as.integer(substr(as.character(get(c_hour)), 1, 2))]
g[, id := trimws(as.character(get(c_id)))]
g <- g[!is.na(id) & id != ""]
for (cc in fuel_cols) set(g, j = cc, value = as.numeric(g[[cc]]))
g[, mw := if (!is.na(c_tot)) as.numeric(get(c_tot)) else rowSums(.SD, na.rm = TRUE), .SDcols = fuel_cols]
g[is.na(mw) | mw < 0, mw := 0]

# technology = fuel with the largest annual output in the plant
if (length(fuel_cols)) {
  fe <- g[, lapply(.SD, function(v) sum(as.numeric(v), na.rm = TRUE)), by = id, .SDcols = fuel_cols]
  fe[, tech := unname(fuel_tech[apply(as.matrix(.SD), 1, which.max)]), .SDcols = fuel_cols]
  tech_of <- fe[, .(id, tech)]
} else tech_of <- g[, .(tech = NA_character_), by = id]

h <- g[, .(mw = sum(mw)), by = .(id, ts)][order(id, ts)]
rm(g); invisible(gc())
say("plants:", uniqueN(h$id), " hours:", uniqueN(h$ts))

# ---------------------------------------------------------------- 3. plant table
pl <- h[, .(p_max = round(as.numeric(quantile(mw, 0.999))),      # ignore single spikes
            energy_gwh = sum(mw) / 1000,
            hours_on = sum(mw > 0)), by = id]
pl <- merge(pl, tech_of, by = "id", all.x = TRUE)
if (KEY_IS_NAME) {
  pl[, name := id]
} else if (file.exists(f_pp)) {
  pp <- fread(f_pp)
  pid <- names(pp)[grep("^id$", tolower(names(pp)))[1]]
  pnm <- names(pp)[grep("^name$|shortname|^ad", tolower(names(pp)))[1]]
  if (!is.na(pid) && !is.na(pnm)) pl <- merge(pl, pp[, .(id = get(pid), name = get(pnm))], by = "id", all.x = TRUE)
}
if (!"name" %in% names(pl)) pl[, name := as.character(id)]

# trips: output falls from >= 50% to <= 10% of p_max in one hour
h <- merge(h, pl[, .(id, p_max)], by = "id")
h[, prev := shift(mw), by = id]
h[, trip := !is.na(prev) & p_max > 0 & prev >= 0.5 * p_max & mw <= 0.1 * p_max]
# duration of each trip: hours until output is back above 50% (capped at 30 days;
# longer stops are treated as planned outages or mothballing)
trip_stats <- h[, {
  tr <- which(trip); dur <- integer(0)
  for (k in tr) {
    back <- if (k < .N) which(mw[(k + 1):.N] >= 0.5 * p_max[1]) else integer(0)
    d <- if (length(back)) back[1] else .N - k
    dur <- c(dur, min(d, 720L))
  }
  .(trips = length(tr), trip_hours = sum(dur), mean_trip_h = if (length(dur)) mean(dur) else NA_real_)
}, by = id]
pl <- merge(pl, trip_stats, by = "id", all.x = TRUE)
pl[, hours_obs := uniqueN(h$ts)]
pl[, FOR_trip := round(trip_hours / hours_obs, 4)]

# ---------------------------------------------------------------- 4. units per plant
# Default split: the typical largest block per technology in Turkiye. Replace
# with the true number of units from EPDK licence pages via the override file.
max_block <- c(coal = 700, gas = 470, hydro_res = 350, nuclear = 1250)
pl[, n_units := ifelse(tech %in% names(max_block),
                       pmax(1L, as.integer(ceiling(p_max / max_block[tech]))), 1L)]
f_ovr <- file.path(OUTD, "fleet_units_override.csv")
if (file.exists(f_ovr)) {
  ov <- fread(f_ovr, colClasses = list(character = "id"))[!is.na(n_units_true)]
  pl[ov, on = "id", n_units := i.n_units_true]
  say("applied", nrow(ov), "unit-count corrections from fleet_units_override.csv")
} else {
  tmpl <- pl[tech %in% c("coal", "gas", "hydro_res") & p_max >= 300][order(-p_max)]
  fwrite(tmpl[, .(id, name, tech, p_max, n_units_default = n_units, n_units_true = NA_integer_)], f_ovr)
  say("wrote", f_ovr, "- fill 'n_units_true' for the largest plants from the EPDK licence register, then re-run")
}
pl[, unit_mw := round(p_max / n_units)]
setcolorder(pl, c("id", "name", "tech", "p_max", "n_units", "unit_mw", "energy_gwh",
                  "hours_on", "trips", "mean_trip_h", "FOR_trip"))
fwrite(pl[order(tech, -p_max)], file.path(OUTD, "fleet_plants_2025.csv"))

# ---------------------------------------------------------------- 5. summary
sm <- pl[!is.na(tech), .(plants = .N, units = sum(n_units), capacity_mw = sum(p_max),
                         largest_unit_mw = max(unit_mw), mean_unit_mw = round(mean(unit_mw)),
                         trips_per_unit_year = round(sum(trips, na.rm = TRUE) / sum(n_units) *
                                                       8760 / hours_obs[1], 2),
                         FOR_capweighted = round(weighted.mean(FOR_trip, p_max, na.rm = TRUE), 4)),
         by = tech][order(-capacity_mw)]
fwrite(sm, file.path(OUTD, "fleet_summary_2025.csv"))
cat("\n==== Fleet derived from EPIAS plant-level data, 2025 ====\n"); print(sm)

# largest online plant each hour (upper bound of the largest thermal contingency)
lo <- merge(h[, .(id, ts, mw)], pl[, .(id, tech, n_units)], by = "id")[tech %in% c("coal", "gas", "hydro_res", "nuclear")]
lo[, unit_out := mw / n_units]
lo <- lo[, .SD[which.max(unit_out)], by = ts][, .(ts, largest_unit_out_mw = round(unit_out), plant_id = id, tech)]
fwrite(lo, file.path(OUTD, "largest_online_2025.csv"))
cat("\nLargest online unit output (MW), 2025: ")
print(round(quantile(lo$largest_unit_out_mw, c(0, .1, .5, .9, 1))))

# ---------------------------------------------------------------- 6. for the model
sizes <- function(tt) { x <- pl[tech == tt & unit_mw > 0]; rep(x$unit_mw, x$n_units) }
fo <- setNames(sm$FOR_capweighted, sm$tech)
saveRDS(list(coal = sort(sizes("coal"), decreasing = TRUE),
             gas = sort(sizes("gas"), decreasing = TRUE),
             hydro_res = sort(sizes("hydro_res"), decreasing = TRUE),
             # only coal: for gas and hydro, sudden drops are mostly cycling and flexible
             # dispatch, not forced outages, so the assumed rates are kept for them
             FOR = fo[intersect(names(fo), "coal")],
             source = "EPIAS realtime generation by plant, 2025"),
        file.path(OUTD, "fleet_observed.rds"))
say("saved data/fleet_observed.rds - the main analysis script will now use it.",
    "Set USE_OBSERVED_FLEET <- FALSE at the top of that script to go back to the assumed fleet.")
