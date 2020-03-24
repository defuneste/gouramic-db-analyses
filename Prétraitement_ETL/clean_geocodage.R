## script du 24/03/2020
# mise en forme nettoyage du jeux de données des sujets
# et geocodage
# convention de code :
# .dat <- tableau
# .shp <- sf 

##.###################################################################################33
## I. Chargement des données et Mise en forme ====
##.#################################################################################33

# 1- chargement des packages et données ========================

pkgs <-  c("dplyr","stringr", "lubridate", "ggplot2", "sf")
inst <- lapply(pkgs, library, character.only = TRUE)


allsujet_SansNA.dat <- readRDS("data/allsujet_cleanNA.rds")
geocodage_evs.shp <- sf::st_read("data/geocodev2.geojson", stringsAsFactors = FALSE)
geocodage_clb.shp <- sf::st_read("data/Geocoding_Result.shp", stringsAsFactors = FALSE)

# 2- Mise en forme  ========================

summary(geocodage_clb.shp)
summary(geocodage_evs.shp)

# Passage des deux infos de types de localisation en facteur
geocodage_clb.shp$Loc_name <- as.factor(geocodage_clb.shp$Loc_name)

geocodage_evs.shp$result_type <- as.factor(geocodage_evs.shp$result_type)
geocodage_evs.shp$source_loc <- as.factor(geocodage_evs.shp$source_loc)

# les dates 
geocodage_evs.shp$Date_birth <- as.Date(geocodage_evs.shp$Date_birth, origin = "1899-12-30")
geocodage_evs.shp$Date_start <- as.Date(geocodage_evs.shp$Date_start, origin = "1899-12-30")
geocodage_evs.shp$Date_end <- as.Date(geocodage_evs.shp$Date_start, origin = "1899-12-30")


hist(geocodage_evs.shp$Date_birth, breaks = "year")

View(allsujet_SansNA.dat)
