# ====================================================================
# Preprocessing code for paper_plots_v2.R
# ====================================================================
# This file contains ONLY the preprocessing code. All configuration
# (library loading, colors/variable sourcing, budgets, file names,
# input glob, rerun/caching toggles, and model renaming mapping) is set
# in the calling script (paper_plots_v2.R) before this file is sourced.

# mifpath <- mifpaths[1]
read_filtmif <- function(mifpath) {
  inmag <- read.report(mifpath)
  scen <- names(inmag)
  frem <- inmag[[1]]$REMIND[, , extractvarsrem[extractvarsrem %in% getNames(inmag[[1]]$REMIND)]]
  fmag <- inmag[[1]]$MAgPIE[, , extractvarsmag[extractvarsmag %in% getNames(inmag[[1]]$MAgPIE)]]
  fmif <- rbind(as.quitte(frem), as.quitte(fmag)) %>%
    mutate(scenario = scen, model = "REMIND-MAgPIE")
  return(fmif)
}

# ====================================================================
# Reading all MIFs with only selected REMIND-MAgPIE data =============
if (rerun_bigmif || !file.exists("inbigmif.rds")) {
  cat("!!!!!!!! Re-running and caching bigmif !!!!!!!!!!\n")
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Starting bigmif preprocessing\n")
  miflist <- lapply(mifpaths, read_filtmif)
  inbigmif <- bind_rows(miflist)
  saveRDS(inbigmif, "inbigmif.rds")
  # inbigmif <- readRDS("inbigmif.rds")

  # Calculating new LUC emissions split ===============================
  inbigmif <- inbigmif %>%
  filter(variable %in% c(
    "Emissions|CO2|Land RAW|+|Land-use Change",
    "Emissions|CO2|Land RAW|Land-use Change|+|Deforestation",
    "Emissions|CO2|Land RAW|Land-use Change|+|Other land conversion",
    "Emissions|CO2|Land RAW|Land-use Change|+|Peatland",
    "Emissions|CO2|Land RAW|Land-use Change|+|Regrowth",
    "Emissions|CO2|Land RAW|Land-use Change|+|Residual",
    "Emissions|CO2|Land RAW|Land-use Change|+|Soil",
    "Emissions|CO2|Land RAW|Land-use Change|+|Timber",
    "Emissions|CO2|Land RAW|Land-use Change|+|Wood Harvest"
  )) %>%
  calc_addVariable(
    `Emissions|CO2|Land RAW|Land-use Change|+++|Deforestation and Wood Harvest` = "`Emissions|CO2|Land RAW|Land-use Change|+|Deforestation` + `Emissions|CO2|Land RAW|Land-use Change|+|Wood Harvest`",
    `Emissions|CO2|Land RAW|Land-use Change|+++|Regrowth` = "`Emissions|CO2|Land RAW|Land-use Change|+|Regrowth`",
    `Emissions|CO2|Land RAW|Land-use Change|+++|Other LUC` =
    "`Emissions|CO2|Land RAW|Land-use Change|+|Other land conversion` +
    `Emissions|CO2|Land RAW|Land-use Change|+|Peatland` +
    `Emissions|CO2|Land RAW|Land-use Change|+|Residual` +
    `Emissions|CO2|Land RAW|Land-use Change|+|Soil` +
    `Emissions|CO2|Land RAW|Land-use Change|+|Timber`",
    unit = "Mt CO2/yr",
    only.new = T
  # ) %>% tail
  ) %>%
  bind_rows(inbigmif, .)

  # Adding extended scenario info columns
  allscens <- inbigmif %>%
    select(scenario) %>%
    unique()
  sceninfo <- allscens %>%
    separate(scenario, c("vtag", "lsm", "ssp", "policy"), sep = "-", remove = F) %>%
    select(-vtag) %>%
    mutate(cbudget = ifelse(str_detect(policy, "Budg"), str_extract(policy, "[0-9][0-9][0-9]"), "NA")) %>%
    mutate(cbudget = as.numeric(cbudget))

  bigmif <- inbigmif %>%
    left_join(sceninfo)

  # Adding cumulative variables
  cumvars <- c(
    "Emi|CO2",
    "Emi|CO2|+|Land-Use Change",
    "Emi|CO2|+|Energy",
    "Emi|CO2|+|Industrial Processes",
    "Emi|CO2|+|Land-Use Change",
    "Emi|CO2|+|Waste",
    "Emi|CO2|+|non-ES CDR",
    "Emi|CO2|CDR",
    "Emi|CO2|CDR|+|BECCS",
    "Emi|CO2|CDR|+|Land-Use Change",
    "Emissions|CO2|Land RAW|+|Land-use Change",
    "Emissions|CO2|Land RAW|Land-use Change|+|Deforestation",
    "Emissions|CO2|Land RAW|Land-use Change|+|Other land conversion",
    "Emissions|CO2|Land RAW|Land-use Change|+|Peatland",
    "Emissions|CO2|Land RAW|Land-use Change|+|Regrowth",
    "Emissions|CO2|Land RAW|Land-use Change|+|Residual",
    "Emissions|CO2|Land RAW|Land-use Change|+|Soil",
    "Emissions|CO2|Land RAW|Land-use Change|+|Timber",
    "Emissions|CO2|Land RAW|Land-use Change|+|Wood Harvest",
    "Emissions|CO2|Land RAW|Land-use Change|+++|Deforestation and Wood Harvest",
    "Emissions|CO2|Land RAW|Land-use Change|+++|Regrowth",
    "Emissions|CO2|Land RAW|Land-use Change|+++|Other LUC"
  )
  cummif <- bigmif %>%
    filter(variable %in% cumvars) %>%
    group_by_at(vars(-period, -value)) %>%
    arrange(period, .by_group = T) %>%
    mutate(dt = period - lag(period)) %>%
    filter(period >= 2020) %>%
    mutate(value = cumsum(value * dt)) %>%
    mutate(variable = paste0(variable, "|Cum")) %>%
    select(-dt) %>%
    ungroup()
  bigmif <- bind_rows(bigmif, cummif)

  # bigmif %>% select(variable) %>% unique %>% filter(str_detect("Harmonized"))

  bigmif %>%
    select(variable) %>%
    unique() %>%
    write.csv("varlist_bigmif.csv", row.names = F)

  saveRDS(sceninfo, "sceninfo.rds")
  saveRDS(bigmif, "bigmif.rds")
} else {
  cat("!!!!!!!! Reading cached bigmif !!!!!!!!!!\n")
  # inbigmif <- readRDS("inbigmif.rds")
  sceninfo <- readRDS("sceninfo.rds")
  bigmif <- readRDS("bigmif.rds")
}

object.size(bigmif) %>% print(unit = "Mb")

# ====================================================================
# Define helper functions for model renaming and filtering ===========
# ====================================================================
if (!exists("rename_models")) {
  rename_models <- list(
    rename = character(),
    include = numeric()
  )
}

# Apply model renaming to data frames (both 'model' and 'lsm' columns if they exist)
apply_model_rename <- function(df, rename_vec) {
  if (length(rename_vec) == 0) return(df)

  for (col in c("model", "lsm")) {
    if (col %in% names(df)) {
      df <- df %>%
        mutate(across(all_of(col), ~recode(., !!!rename_vec)))
    }
  }
  return(df)
}

# Apply filtering to data frames
apply_model_filter <- function(df, include_vec) {
  if (length(include_vec) == 0) return(df)

  # Get all models to filter
  models_to_exclude <- names(include_vec)[include_vec == 0]

  for (col in c("model", "lsm")) {
    if (col %in% names(df)) {
      df <- df %>%
        filter(!.data[[col]] %in% models_to_exclude)
    }
  }
  return(df)
}

# ==================================================================================
# Append climate MIF and harmonized emissions ======================================
# in a separate mixbigmif variable
if (rerun_mixbigmif || !file.exists("cache_mixbigmif.rds")) {
  cat("!!!!!!!! Re-running and caching mixbigmif !!!!!!!!!!\n")
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Starting mixbigmif preprocessing\n")
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Loading climate data...\n")
  inclimmif <- readRDS(climfname)
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Climate data loaded\n")

  # inclimmif %>%
  #   left_join(select(sceninfo, scenario, lsm)) %>%
  #   filter(
  #     variable == "Surface Air Temperature Change",
  #     period == 2050,
  #     quant == "q50"
  #     ) %>%
  #   select(model,lsm) %>% distinct %>% View
  #

  # Keep only scenario(lsm)-model combinations that are either
  # compatible or AR6.
  inclimmif <- inclimmif %>%
    left_join(select(sceninfo, scenario, lsm)) %>%
    filter((model == lsm | model == "AR6"))

  # Adjusting GSAT so that the median matches 0.85K in 1995-2014
  # As in Kikstra et al.
  # TODO: Do this step at the individual run level. Doing it after
  # evaluating the quantiles is very close but not necessarily the same
  adjfacs <- inclimmif %>%
    filter(
      variable %in% c("Surface Air Temperature Change"),
      quant == "q50"
      ) %>%
      filter(between(period, 1995, 2014)) %>%
      group_by(across(c(-period,-value))) %>%
      summarise(value = mean(value, na.rm = T)) %>%
      ungroup() %>%
      mutate(adjfac = 0.85 - value) %>%
      select(model, scenario, adjfac)

  tempclimmif <- inclimmif %>%
    filter(variable %in% c("Surface Air Temperature Change")) %>%
    left_join(adjfacs) %>%
    mutate(value = value + adjfac) %>%
    select(-adjfac)

  inclimmif <- inclimmif %>%
      filter(!(variable %in% c("Surface Air Temperature Change"))) %>%
      bind_rows(tempclimmif)

  mixbigmif <- inclimmif %>%
    mutate(variable = paste0(variable, "|", quant)) %>%
    mutate(region = "World") %>%
    select(-quant) %>%
    left_join(sceninfo) %>%
    bind_rows(bigmif, .)

  # Harmonized emissions
  inharmmif <- read.delim(harmfname, sep = ",") %>%
    select(-X, -exclude) %>%
    pivot_longer(
      cols = starts_with("X"),
      names_pattern = "X(.*)",
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(period = as.integer(str_sub(period, 1L, 4L))) %>%
    mutate(variable = paste0("Harmonized|", variable)) %>%
    calc_addVariable(
      `Harmonized|Emissions|CO2` = "`Harmonized|Emissions|CO2|MAGICC AFOLU` + `Harmonized|Emissions|CO2|MAGICC Fossil and Industrial`",
      unit = "Mt CO2/yr"
    ) %>%
    left_join(sceninfo)

  mixbigmif <- mixbigmif %>%
    bind_rows(., inharmmif)

  cat("[", format(Sys.time(), "%H:%M:%S"), "] Starting cumulative calculations...\n")

  # ====================================================================
  # Calculating indicators that need variables from different sources ==
  cumvars <- c(
    "Harmonized|Emissions|CO2",
    "Harmonized|Emissions|CO2|MAGICC AFOLU",
    "Harmonized|Emissions|CO2|MAGICC Fossil and Industrial"
  )
  cummif <- mixbigmif %>%
    filter(variable %in% cumvars) %>%
    group_by_at(vars(-period, -value)) %>%
    arrange(period, .by_group = T) %>%
    mutate(dt = period - lag(period)) %>%
    filter(period >= 2020) %>%
    mutate(value = cumsum(value * dt)) %>%
    mutate(variable = paste0(variable, "|Cum")) %>%
    select(-dt) %>%
    ungroup()

  mixbigmif <- mixbigmif %>%
    bind_rows(., cummif)

  # Equivalent CO2 emissions from atmospheric concentrations ================

  # Filter and save in a variable, as it takes some time to filter
  atmmif <- mixbigmif %>%
    filter(str_detect(variable, "Atmospheric Concentrations\\|CO2"))
  refatm <- atmmif %>%
    filter(period == 2020) %>%
    select(-period) %>%
    rename(valref = value)
  # Conversion factor 1 ppm = 2.124GtC taken from
  # Friedlingstein et al., 2024, Table 1, also 1 GtC = 3.664 GtCO2
  eqemif <- left_join(atmmif, refatm) %>%
    filter(period >= 2020) %>%
    mutate(value = value - valref) %>%
    mutate(variable = str_replace(
      variable,
      "Atmospheric Concentrations\\|CO2\\|",
      "Atmospheric Concentrations|CO2|EqEm2020|"
    )) %>%
    mutate(unit = "GtCO2") %>%
    select(-valref) %>%
    mutate(value = value * 2.124 * 3.664 * 1e3) # ppm to GtC, GtC to GtCO2, GtCO2 to MtCO2

  # Airborne fraction ===============================
  abfmif <- cummif %>%
    filter(variable == "Harmonized|Emissions|CO2|Cum") %>%
    mutate(period = period + 1) %>% # Shift by one year to approximate emissions causing concentration changes
    select(-model, -unit, -variable) %>%
    rename(valuemi = value) %>%
    left_join(eqemif, .) %>%
    mutate(value = value / valuemi) %>%
    mutate(variable = str_replace(
      variable,
      "Atmospheric Concentrations\\|CO2\\|EqEm2020\\|",
      "Airborne Fraction|Since2020|"
    )) %>%
    select(-valuemi)

  mixbigmif <- mixbigmif %>%
    bind_rows(., eqemif, abfmif)

  mixbigmif %>%
    select(variable) %>%
    unique() %>%
    write.csv("varlist_mixbigmif.csv", row.names = F)

  cat("[", format(Sys.time(), "%H:%M:%S"), "] Applying model renaming to fresh mixbigmif...\n")
  # Apply renaming and filtering BEFORE saving cache
  mixbigmif <- apply_model_rename(mixbigmif, rename_models$rename)
  mixbigmif <- apply_model_filter(mixbigmif, rename_models$include)
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Model renaming/filtering complete before save\n")

  cat("[", format(Sys.time(), "%H:%M:%S"), "] Saving mixbigmif cache...\n")
  saveRDS(mixbigmif, "cache_mixbigmif.rds")
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Cache saved\n")
} else {
  cat("!!!!!!!! Reading cached mixbigmif from file !!!!!!!!!!\n")
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Loading cached mixbigmif...\n")
  mixbigmif <- readRDS("cache_mixbigmif.rds")
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Cache loaded\n")
}

cat("[", format(Sys.time(), "%H:%M:%S"), "] mixbigmif size: ")
object.size(mixbigmif) %>% print(unit = "Mb")

# ====================================================================
# Apply model renaming and filtering ================================
# The rename_models mapping is expected to be defined in the calling
# script before this file is sourced. It should be a list with:
#   $rename: named vector of old_name = new_name (defaults to no renaming)
#   $include: named vector with 1 = include, 0 = exclude model (defaults: include all)
# ====================================================================

# Reading global mean stocks, join as needed
# Always filer by landtype!
allglostocks <-
  read.csv(allglostocksfname, sep = ";") %>%
  as_tibble() %>%
  rename(lsm = model)
stockmodels <- unique(allglostocks$lsm)

# Reading historical fluxes, join as needed
# Filtering for TRENDY scenario
histfluxmif <-
  read.csv(histfluxfname, sep = ";") %>%
  as_tibble() %>%
  filter(scenario == "S2") %>%
  rename(lsm = model) %>%
  filter(lsm %in% stockmodels) %>%
  filter(between(period, 1960, 2020)) %>%
  group_by(across(-c("period","value"))) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(value = ifelse(
    str_detect(unit, "GtC"),
    value*3.66,
    value
    )) %>%
  mutate(unit = str_replace(unit, "GtC", "GtCO2"))

# ====================================================================
# Apply renaming to modelcolors (final step)
# ====================================================================
cat("[", format(Sys.time(), "%H:%M:%S"), "] Updating modelcolors...\n")

# Apply renaming to modelcolors
if (exists("modelcolors") && length(rename_models$rename) > 0) {
  for (i in seq_along(rename_models$rename)) {
    old_name <- names(rename_models$rename)[i]
    new_name <- rename_models$rename[i]

    if (old_name %in% names(modelcolors)) {
      names(modelcolors)[names(modelcolors) == old_name] <- new_name
    }
  }
}

# Remove filtered models from modelcolors
if (exists("modelcolors") && length(rename_models$include) > 0) {
  models_to_exclude <- names(rename_models$include)[rename_models$include == 0]
  modelcolors <- modelcolors[!names(modelcolors) %in% models_to_exclude]
}

cat("[", format(Sys.time(), "%H:%M:%S"), "] All preprocessing complete!\n")