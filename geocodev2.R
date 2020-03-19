## script du 03/03/2020
# geocodage version après avoir recu la version du clb
# convention de code :
# .dat <- tableau
# .shp <- sf 

# 1- chargement des packages et données ========================


pkgs <-  c("dplyr","stringr", "lubridate", "ggplot2", "banR", "sf")
inst <- lapply(pkgs, library, character.only = TRUE)

# version CLB / ESRI

geocodage_clb.shp <- sf::st_read("data/Geocoding_Result.shp", stringsAsFactors = FALSE) # repasser en factor dans certains cas

str(geocodage_clb.shp)

# Version EVS / Bano cf : sujet_gouranic_eda. R
# une partie a été aussi faite à la main cf rapport à faire

geocodage_evs.shp <- sf::st_read("data/geocodev2.geojson", stringsAsFactors = FALSE)

str(geocodage_evs.shp)

