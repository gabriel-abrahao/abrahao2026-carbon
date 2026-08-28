require(tidyverse)
require(quitte)

ar6datafname <- "/mnt/c/pik/nena-klims-analysis/data/data.Rdata"
ar6data <- load(ar6datafname)
ar6data[3]

modelfamily <- read.csv2("/mnt/c/pik/nena-klims-analysis/model_family.csv")

ar6_data_world %>% saveRDS("ar6_data_world.rds")

ar6_data_world %>% select(variable) %>% unique %>% write.csv("varlist_ar6_data_world.csv", row.names = FALSE)

ar6emissions <- ar6_data_world %>% 
  filter(variable %in% c(
    "Emissions|CO2",
    "Emissions|CO2|AFOLU",
    "Emissions|CO2|Energy and Industrial Processes"
  )) %>%
    group_by_at(vars(-period, -value)) %>%
  arrange(period, .by_group = T) %>%
  mutate(dt = period - lag(period)) %>%
  filter(period >= 2020) %>%
  mutate(value = cumsum(value * dt)) %>%
  mutate(variable = paste0(variable, "|Cum")) %>%
  select(-dt) %>%
  ungroup() %>%
  left_join(modelfamily, by = "model") 

ar6peakbudgets <- ar6emissions %>%
  filter(variable == "Emissions|CO2|Cum") %>%
  group_by_at(vars(-period,-value)) %>%
  filter(value == max(value)) %>%
  mutate(peakbudget = value*1e-3) %>%
  ungroup() %>%
  select(model, scenario, peakbudget) 

options(width = 200)
ar6emissions <- ar6emissions %>%
  left_join(ar6peakbudgets) 

ar6emissions %>%
  filter(period == 2050) %>%
  filter(variable == "Emissions|CO2|AFOLU|Cum") %>%
  filter(modfamily == "REMIND-MAgPIE") %>%
  mutate(value = value*1e-3) %>%
  ggplot(aes(x = peakbudget, y = value, color = modfamily)) +
  geom_point() +
  xlim(0,1000)

ar6emissions %>%
  filter(variable == "Emissions|CO2|Cum") %>%
  mutate(value = value*1e-3) %>%
  filter(model %in% c("REMIND-MAgPIE 2.1-4.3")) %>%
  ggplot(aes(x = period, y = value, color = modfamily, group = scenario)) +
  geom_line() 

saveRDS(ar6emissions, "ar6emissions.rds")

inclimmif <- readRDS(climfname)

usescen <- "C_ESM2025v05-LPJml-SSP2-PkBudg840-rem-5"

inclimmif %>% select(model) %>% unique

dum <- inclimmif %>% 
  filter(
    scenario == usescen,
    # model == "AR6",
    variable == "Surface Air Temperature Change",
    quant == "q67"
  )

dum %>%
  ggplot(aes(x = period, y = value, color = model)) +
  geom_line()

dum %>%
  filter(model == "OCN") %>%
  print(n=100)


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