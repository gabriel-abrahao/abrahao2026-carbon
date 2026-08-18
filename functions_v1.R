require(dplyr)

# Recalculates `value` in a quitte-like data frame as the difference to a
# reference period, for every combination of the remaining columns
# (typically model, scenario, region, variable, unit, ...).
#
# df: a quitte-like data frame/tibble with (at least) `period`, `value`,
#     `variable` and `unit` columns.
# ref_period: the period used as the baseline for the difference. Must be
#     present for every group that should be recalculated.
# variables: optional character vector of `variable` values to recalculate.
#     If NULL (default), all variables are recalculated.
# only.new: if TRUE, return only the recalculated variables. Defaults to
#     FALSE, in which case variables not selected are returned unchanged.
rebase_to_period <- function(df, ref_period, variables = NULL, only.new = FALSE) {
  id_cols <- setdiff(names(df), c("period", "value"))

  if (is.null(variables)) {
    dfsel <- df
  } else {
    issel <- df$variable %in% variables
    dfsel <- df[issel, ]
  }

  dfsel <- dfsel %>%
    group_by(across(all_of(id_cols))) %>%
    mutate(value = value - value[match(ref_period, period)]) %>%
    ungroup() %>%
    mutate(unit = factor(paste(as.character(unit), "wrt", ref_period)))

  if (isTRUE(only.new) || is.null(variables)) {
    return(dfsel)
  }

  bind_rows(dfsel, df[!issel, ])
}
