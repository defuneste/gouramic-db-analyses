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

table(de_cote.shp$Loc_name, useNA = "ifany")

# on vire les Na et on regarde la répartition rural urbain

de_cote.shp <- de_cote.shp[!is.na(de_cote.shp$Loc_name),]

# on reprend le shape des communes avec l'info rural-urbain-peri
communes.shp <- st_read("data/commune.shp")

# rajout du type de commune par adresse !!! attention c'est le type de commune en 2019
adresse_commune.shp <- st_join(de_cote.shp,
                               st_transform(communes.shp[,c("TYPE_CO", "insee")], 2154))

table(adresse_commune.shp$Loc_name, adresse_commune.shp$TYPE_CO)
# st_write(de_cote.shp, "data/verif/de_cote.geojson")