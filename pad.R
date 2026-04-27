
inclimmif <- readRDS(climfname)

usescen <- "C_ESM2025v05-LPJml-SSP2-PkBudg840-rem-5"

dold <- bigmif %>% 
  filter(
    region == "GLO",
    scenario == usescen,
    variable == "MAGICC7 AR6|Surface Temperature (GSAT)|67p0th Percentile"
    )


dnew <- inclimmif %>%
 filter(
    scenario == usescen,
    model == "AR6",
    variable == "Surface Air Temperature Change",
    quant == "q67"
  ) %>%
  mutate(variable = paste0(variable, "|", quant)) %>%
  select(-quant)

dadj <- tempclimmif %>%
 filter(
    scenario == usescen,
    model == "AR6",
    variable == "Surface Air Temperature Change",
    quant == "q67"
  ) %>%
  mutate(variable = paste0("ADJ|",variable, "|", quant)) %>%
  select(-quant) 

bind_rows(dold, dnew, dadj) %>%
  filter(between(period, 1995,2100)) %>% #View
  ggplot(aes(x = period, y = value, color = variable, lty = variable)) +
  geom_line() +
  theme(legend.position = "bottom", legend.direction = "vertical")

"MAGICC7 AR6|Surface Temperature (GSAT)|67p0th Percentile",
"Surface Air Temperature Change|q67"