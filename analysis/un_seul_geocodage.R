# exploration des cas avec un seul géocodage

library(sf)

# les cas avec les deux 
adresse <- sf::st_read("data/verif/distance.geojson")

# le géocodage esri

geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

de_cote.shp <- geocodage_clb.shp[!geocodage_clb.shp$ID_CARTO %in% adresse$ID_CARTO,]
