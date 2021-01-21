# exploration des cas avec un seul géocodage

library(sf)
library(lubridate)
library(dplyr)

# les cas avec les deux 
adresse <- sf::st_read("data/verif/distance.geojson")

# le géocodage esri

geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

#correction formatage
geocodage_clb.shp$date_start <- parse_date_time(geocodage_clb.shp$date_start, orders = c("my", "dmy"))
geocodage_clb.shp$date_end_a <- parse_date_time(geocodage_clb.shp$date_end_a, orders = c("my", "dmy"))

geocodage_clb.shp <- geocodage_clb.shp %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, sep = " ", na.rm = TRUE) %>% 
    dplyr::select(ID_CARTO, date_start, date_end_a, Commune, Adresse, CP, Info_sup, Match_addr, Loc_name)

de_cote.shp <- geocodage_clb.shp[!geocodage_clb.shp$ID_CARTO %in% adresse$ID_CARTO,]

de_cote.shp
