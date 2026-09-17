require(tidyverse)
require(quitte)
require(magclass)
require(lucode2)
options(width = 140)
# Model colors
source("colors_models_v1.R")


# inbigfname <- "ESM2025v05p1_mag-4_cell.land_split_0.5.mz"
inbigfname <- "ESM2025v05p1_mag-4_cell.land_split_0.5_2050.mz"

# useyear <- 2050
inbigmag <- read.magpie(inbigfname)#[,useyear,]
# inbigmag <- readRDS(inbigfname)

object.size(inbigmag) %>% print(unit = "Mb")


str(inbigmag)
allscens <- getItems(inbigmag, "scenario")
alltypes <- getItems(inbigmag, "d3")

keepscens <- allscens[(str_detect(allscens, "LPJml") | str_detect(allscens, "JSBACH")) & str_detect(allscens, "PkBudg840")]


keepmag <- inbigmag[,,keepscens]
# saveRDS(keepmag,"keepmag.rds")


str(keepmag)
keepmag["BRA",,"PlantedForest_Afforestation"] %>%
  as.tibble()

keepmag["-35p75.-8p75.BRA",,keepscens[1]] %>%
  as.tibble()


keeptib <- as.tibble(keepmag[,,"other"])
keeptib

keeptib

keeptib %>%
  filter(scenario == keepscens[1]) %>%
  ggplot(aes(x = x, y = y, fill = value)) +
  geom_raster() +
  facet_wrap(~scenario) +
  # scale_fill_binned() +
  theme(aspect.ratio = 1)
