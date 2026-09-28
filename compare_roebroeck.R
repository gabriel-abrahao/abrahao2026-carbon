library(magclass)
library(quitte)
library(tidyverse)
require(madrat)
require(mrmagpie)
require(luplot)
require(rworldmap)
require(RColorBrewer)
require(classInt)
require(terra)

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

useyear <- 2015


inrbravg <- read.magpie("roebroeck/mean_forest_biomass_0.5.nc")
inrbrmax <- read.magpie("roebroeck/max_forest_biomass_0.5.nc")
rbrsravg <- as.SpatRaster(inrbravg)
rbrsrmax <- as.SpatRaster(inrbrmax)


allglostocks <- tibble()
usemodel <- allmodels[6]
# par(fin = c(20,20), mfrow = c(4, 3), mai=c(0,0,0.2,0.2))
for (usemodel in allmodels) {
    print(usemodel)
    instocksclust <- read.magpie(paste0(basefolder, usemodel, "/lpj_carbon_stocks_c200.mz"))

    cmap <- readRDS(paste0(basefolder, usemodel, "/clustermap_rev4.116_c200_67420_h12.rds"))

    instocks <- toolAggregate(instocksclust, cmap, to = "cell")[, useyear, ]

    # instocks <- read.magpie(paste0(basefolder, usemodel, "/lpj_carbon_stocks_c200.mz"))[, useyear, ]
    allcforest <- dimSums(instocks[, , c("primforest", "secdforest", "forestry")], dim = 3.1)
    vegcforest <- as.SpatRaster(setItems(allcforest[, , "vegc"], 3, NULL))

    landinit <- read.magpie(paste0(basefolder, usemodel, "/avl_land_full_t_0.5.mz"))[, useyear, ]
    tmpother <- setItems(landinit[, , "primother"], 3, "other") + setItems(landinit[, , "secdother"], 3, "other")
    landinit <- mbind(landinit, tmpother)[, , c("primother", "secdother"), invert = T]
    tmpother <- setItems(landinit[, , "past"], 3, "past") + setItems(landinit[, , "range"], 3, "past")
    landinit <- mbind(landinit[, , c("past", "range"), invert = T], tmpother)
    landinit <- setYears(landinit)
    forestinit <- dimSums(landinit[, , c("primforest", "secdforest", "forestry")], 3)

    forestinit <- as.SpatRaster(forestinit)

    erbrsravg <- extend(rbrsravg, forestinit)
    erbrsrmax <- extend(rbrsrmax, forestinit)

    forestmask <- mask(forestinit, erbrsravg)

    mdf <- tibble(
        model = usemodel, source = "TRENDY",
        value = global(vegcforest, fun = mean, weights = forestmask, na.rm = TRUE)[[1]]
    )

    allglostocks <- bind_rows(allglostocks, mdf)
}

allglostocks <- allglostocks %>% mutate(model = gsub("LPJml", "LPJmL", model))

fullstocks <- rbind(
    allglostocks,
    tibble(
        model = "RBR23-Current", source = "Roebroeck et al. 2023 (RBR23)",
        value = global(erbrsravg, fun = mean, weights = forestmask, na.rm = TRUE)[[1]]
    ),
    tibble(
        model = "RBR23-Potential", source = "Roebroeck et al. 2023 (RBR23)",
        value = global(erbrsrmax, fun = mean, weights = forestmask, na.rm = TRUE)[[1]]
    )
)

displaymodels <- setdiff(unique(allglostocks$model), "lpxqs")
fullstocks %>%
    filter(model != "lpxqs") %>%
    mutate(model = factor(model, levels = c(displaymodels, "RBR23-Current", "RBR23-Potential"))) %>%
    mutate(source = factor(source, levels = c("TRENDY", "Roebroeck2023")))

fullstocks %>%
    filter(model != "lpxqs") %>%
    mutate(model = factor(model, levels = c(displaymodels, "RBR23-Current", "RBR23-Potential"))) %>%
    mutate(source = factor(source, levels = c("TRENDY", "Roebroeck et al. 2023 (RBR23)"))) %>%
    ggplot(aes(x = model, y = value, color = model, shape = source)) +
    geom_point(size = 3) +
    # facet_wrap(~source, nrow = 1) +
    geom_vline(xintercept = 10.5, color = "grey40", linetype = "dashed") +
    scale_color_manual(values = c(modelcolors, "RBR23-Current" = "magenta", "RBR23-Potential" = "magenta")) +
    labs(x = "Model", color = "Model", shape = "Source",
    y = "Area-weighted mean\nforest vegegatation carbon stock (tC/ha)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figA3_globalmean_stocks_roebroeck.png", width = 7, height = 6)

fullstocks %>%
    filter(model != "lpxqs") %>%
    mutate(model = factor(model, levels = c(displaymodels, "RBR23-Current", "RBR23-Potential"))) %>%
    mutate(source = factor(source, levels = c("TRENDY", "Roebroeck et al. 2023 (RBR23)"))) %>%
    write.csv2("fullstocks_mean_forest_withroebroeck.csv")

fullstocks %>%
    filter(model != "lpxqs") %>%
    mutate(model = factor(model, levels = c(displaymodels, "RBR23-Current", "RBR23-Potential"))) %>%
    mutate(source = factor(source, levels = c("TRENDY", "Roebroeck et al. 2023 (RBR23)"))) %>%
    ggplot(aes(x = model, y = value, color = model, shape = source)) +
    geom_point(size = 3) +
    # facet_wrap(~source, nrow = 1) +
    geom_vline(xintercept = 10.5, color = "grey40", linetype = "dashed") +
    scale_color_manual(values = c(modelcolors, "RBR23-Current" = "magenta", "RBR23-Potential" = "magenta")) +
    labs(x = "Model", color = "Model", shape = "Source",
    y = "Area-weighted mean\nforest vegegatation carbon stock (tC/ha)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figure_globalmean_stocks_roebroeck_small.png", width = 7, height = 6)



# tibble(
#     model = usemodel, source = "Roebroeck2024-MEAN",
#     value = global(erbrsravg, fun = mean, weights = forestmask, na.rm = TRUE)
# )


# global(erbrsravg, fun = mean, weights = forestmask, na.rm = TRUE)
# global(erbrsrmax, fun = mean, weights = forestmask, na.rm = TRUE)




allglostocks %>%
    filter(model != "lpxqs") %>%
    ggplot(aes(x = landtype, y = value, color = model)) +
    geom_point() +
    facet_wrap(~c_pools, scales = "free", ncol = 1)


allglostocks %>%
    filter(model != "lpxqs") %>%
    filter(c_pools %in% c("vegc", "soilc")) %>%
    filter(landtype %in% c("primforest", "secdforest", "forestry", "other", "crop", "past")) %>%
    ggplot(aes(x = landtype, y = value, color = model)) +
    geom_point(size = 1) +
    geom_text(aes(label = model), size = 2.1, vjust = "bottom", position = "jitter") +
    facet_wrap(~c_pools, scales = "free", ncol = 1) +
    labs(x = "Land type", y = "Area-weighted mean carbon stock (tC/ha)")
ggsave("figure_globalmean_stocks.png", width = 6, height = 6)


bigrbravg <- rast("roebroeck/mean_forest_biomass_0.5.nc")
bigrbrmax <- rast("roebroeck/max_forest_biomass_0.5.nc")
plot(bigrbravg)

names(bigrbravg) <- "mean_forest_biomass"
names(bigrbrmax) <- "max_forest_biomass"
c(bigrbravg, bigrbrmax) %>% 
    plot(range = c(0, 500)
    )


# str((landinit[, useyear, ] * instocks[, useyear, ]) / landinit[, useyear, ])


# ./lpj_carbon_stocks_0.5.mz
