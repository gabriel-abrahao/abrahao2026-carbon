library(magclass)
library(quitte)
library(tidyverse)
require(madrat)
require(mrmagpie)
require(luplot)
require(rworldmap)
require(RColorBrewer)
require(classInt)

source("colors_models_v1.R")

basefolder <- "frankentrendy_v4/"


allmodels <- c(
    "CABLEPOP",
    "DLEM",
    "IBIS",
    "JSBACH",
    "LPJGUESS",
    "LPJml",
    "LPJwsl",
    "lpxqs",
    "OCN",
    "ORCHIDEE",
    "SDGVM"
)

plot_countries <- function(mag, addLegend = TRUE, tit = "", brks = NULL) {
    countries <- getRegions(mag)
    values <- as.vector(mag)
    DF <- data.frame(country = countries, namedim = values, hatching = values)
    mapobject <- rworldmap::joinCountryData2Map(DF, joinCode = "ISO3", nameJoinColumn = "country")

    if (is.null(brks)) {
        brks <- classInt::classIntervals(values, n = 10, style = "jenks")[["brks"]]
    }
    # brks <- seq(0, 160, 20)
    colourPalette <- RColorBrewer::brewer.pal(length(brks), "YlGn")
    mapParams <- rworldmap::mapCountryData(
        mapobject,
        nameColumnToPlot = dimnames(DF)[[2]][[2]],
        # nameColumnToPlot = "dimnames(DF)[[2]][[2]]",
        catMethod = brks, colourPalette = colourPalette,
        mapTitle = tit,
        addLegend = addLegend
    )
    return(mapParams)
}


useyear <- 2015

allplts <- vector("list", length = length(allmodels))
names(allplts) <- allmodels
allglostocks <- tibble()
usemodel <- allmodels[1]
par(fin = c(20,20), mfrow = c(4, 3), mai=c(0,0,0.2,0.2))
for (usemodel in allmodels) {
    print(usemodel)
    # instocks <- read.magpie(paste0(basefolder, usemodel, "/lpj_carbon_stocks_0.5.mz"))
    instocks <- read.magpie(paste0(basefolder, usemodel, "/lpj_carbon_stocks_c200.mz"))[, useyear, ]

    landinit <- read.magpie(paste0(basefolder, usemodel, "/avl_land_full_t_c200.mz"))[, useyear, ]
    tmpother <- setItems(landinit[, , "primother"], 3, "other") + setItems(landinit[, , "secdother"], 3, "other")
    landinit <- mbind(landinit, tmpother)[, , c("primother", "secdother"), invert = T]
    tmpother <- setItems(landinit[, , "past"], 3, "past") + setItems(landinit[, , "range"], 3, "past")
    landinit <- mbind(landinit[, , c("past", "range"), invert = T], tmpother)


    cmap <- readRDS(paste0(basefolder, usemodel, "/clustermap_rev4.116_c200_67420_h12.rds"))



    glostocks <- toolAggregate(instocks, cmap, weight = landinit, to = "global", verbosity = 2)
    glostocks <- as.quitte(glostocks) %>% mutate(model = as.character(usemodel))


    countrystocks <- toolAggregate(instocks, cmap, weight = landinit, to = "country", verbosity = 2)

    plt <- plot_countries(
        countrystocks[, , "primforest.vegc"],
        tit = usemodel,
        brks = seq(0, 250, 20),
        addLegend = FALSE
    )


    allglostocks <- rbind(allglostocks, glostocks)
    # allplts <- c(allplts, plt)
    allplts[[usemodel]] <- plt
}

allglostocks <- allglostocks %>% mutate(model = gsub("LPJml", "LPJmL", model))
displaymodels <- setdiff(unique(allglostocks$model), "lpxqs")

# print(allplts)

do.call(
    addMapLegend,
    c(allplts[[length(allplts)]],
        legendLabels = "all",
        # legendWidth = 0.5,
        legendWidth = 5,
        legendIntervals = "data",
        legendMar = 5,
        labelFontSize = 1
    )
)


allglostocks %>%
    filter(model != "lpxqs") %>%
    ggplot(aes(x = landtype, y = value, color = model)) +
    geom_point() +
    facet_wrap(~c_pools, scales = "free", ncol = 1)


allglostocks %>%
    filter(model != "lpxqs") %>%
    filter(c_pools %in% c("vegc","soilc")) %>%
    filter(landtype %in% c("primforest","secdforest", "forestry", "other","crop","past")) %>%
    ggplot(aes(x = landtype, y = value, color = model)) +
    geom_point(size = 1) +
    geom_text(aes(label = model), size = 2.1, vjust = "bottom", position = "jitter", show.legend = FALSE) +
   facet_wrap(~c_pools, scales = "free_y", ncol = 1, labeller = labeller(c_pools = c(soilc = "Soil carbon pool", vegc = "Vegetation carbon pool"))) +
    scale_color_manual(values = modelcolors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Land type", y = "Area-weighted mean carbon stock (tC/ha)", color = "DVGM")
ggsave("figA2_globalmean_stocks.png", width = 6, height = 7)



allglostocks %>%
    filter(model != "lpxqs") %>%
    filter(c_pools %in% c("vegc","litc","soilc")) %>%
    filter(landtype %in% c("primforest","secdforest", "forestry", "other","crop","past")) %>%
    ggplot(aes(x = landtype, y = value, color = model)) +
    geom_point(size = 1) +
    geom_text(aes(label = model), size = 2.1, vjust = "bottom", position = "jitter") +
    facet_wrap(~c_pools, scales = "free", ncol = 1) +
    scale_color_manual(values = modelcolors) +
    labs(x = "Land type", y = "Area-weighted mean carbon stock (tC/ha)", color = "DVGM")
ggsave("figure_globalmean_stocks_wlit.png", width = 6, height = 9)

allglostocks %>%
    filter(model != "lpxqs") %>%
    filter(c_pools %in% c("vegc")) %>%
    # filter(landtype %in% c("primforest","secdforest", "forestry", "other","crop","past")) %>%
    filter(landtype %in% c("primforest")) %>%
    ggplot(aes(x = landtype, y = value, color = model)) +
    geom_point(size = 1) +
    # geom_text(aes(label = model), size = 2.1, vjust = "bottom", position = "jitter") +
    # facet_wrap(~c_pools, scales = "free_y", ncol = 1) +
    scale_color_manual(values = modelcolors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank()
  )
    labs(x = "Land type", y = "Area-weighted mean carbon stock (tC/ha)", color = "DVGM")
ggsave("figure_globalmean_stocks.png", width = 6, height = 7)



options(width=150)
allglostocks %>%
    filter(model != "lpxqs") %>%
    select(-region,-scenario,-unit,-variable,-period) %>%
    filter(c_pools %in% c("vegc","litc","soilc")) %>%
    filter(landtype %in% c("primforest","secdforest", "forestry", "other","crop","past")) %>%
    pivot_wider(names_from = c_pools, values_from = value) %>%
    mutate(totc = vegc + litc + soilc) %>%
    mutate(flitveg = litc/vegc) %>%
    mutate(flitsoi = litc/soilc) %>%
    mutate(flittot = litc/totc) %>%
    arrange(landtype) %>%
    write_delim("table_globalmean_stocks.csv", delim = ";")
