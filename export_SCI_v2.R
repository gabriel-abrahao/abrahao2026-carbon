require(tidyverse)
require(quitte)
e()


rawmiffolder <- "/mnt/c/pik/abrahao2026-carbon/clusterdown_v05p1/"
# rawmiffolder <- "/p/projects/dipol/paperLandMatters/v6/cpl/remind/output/"
renamedfolder <- "abrahao_renamed_mifs/"
conffname <- "scenario_names_SCI_v2.csv"

dorename <- TRUE

conf <- read.csv2(conffname) %>% as_tibble

rawmifnames <- list.files(rawmiffolder, pattern = ".mif$")

# Match each scenario to its corresponding file(s)
conf$rawmifpath <- map_chr(conf$scenario, function(scenario) {
  matches <- rawmifnames[grepl(scenario, rawmifnames, fixed = FALSE)]
  if (length(matches) == 0) {
    warning(paste("No match found for scenario:", scenario))
    return(NA_character_)
  } else if (length(matches) > 1) {
    # If multiple matches, take the first; adjust this logic as needed
    message(paste("Multiple matches for scenario:", scenario, "→ using", matches[1]))
    return(matches[1])
  }
  return(matches)
})

conf$rawmifpath <- paste0(rawmiffolder, conf$rawmifpath)

rawmifprefs <- str_remove(rawmifnames, ".mif$")
# setdiff(conf$mif, rawmifprefs)

conf$renamedpath <- paste0(renamedfolder, conf$newscenario, ".mif")

if (dorename) {
    dir.create(renamedfolder, showWarnings = FALSE)
    # Loop through mifs instead of applying, so we can do some pedestrian logic here
    for (i in seq_along(conf$rawmifpath)) {   
        trow <- conf[i,]
        print(paste0("Renaming ", trow$rawmifpath))
        print(trow$newscenario)
        inmif <- read.quitte(trow$rawmifpath)
        inmif$scenario <- trow$newscenario
        inmif$model <- "REMIND-MAgPIE 3.5-4.10"
        # conf$renamedpath[i] <- paste0(renamedfolder, trow$newscenario, ".mif")
        print(paste0("Writing renamed mif to ", conf$renamedpath[i]))
        write.mif(inmif, conf$renamedpath[i])
    }
}
