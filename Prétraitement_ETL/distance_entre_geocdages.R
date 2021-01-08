# verif des distances entre les deux geocodages 

library(sf)

# rapport/prepa_publi.Rmd

adresse <- sf::st_read("data/verif/distance.geojson")
adresse_filtre <- sf::st_drop_geometry( adresse) 

adresse_filtre$distance <-as.numeric(adresse_filtre$distance)

# données deja geocodées

geocoder_main <- sf::st_read("data/geocodage_main_total.geojson")
#st_write(geocoder_main, "data/verif/deja_geocoder.geojson")

# on va chercher l'adresse 
geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

adresse_complement <- geocodage_clb.shp %>% 
    dplyr::select(ID_CARTO, Commune, CP, nb_rue_p, rue_p, compl_add_, pt_remarq_, lieudit_p ) %>% 
    tidyr::unite("Adresse", nb_rue_p, rue_p, sep = " ",  na.rm = TRUE) %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, na.rm = TRUE)


adresse_complement_distance <- dplyr::inner_join(adresse_complement, adresse_filtre, by = "ID_CARTO")

adresse_complement_distance <- adresse_complement_distance[adresse_complement_distance$distance >= 5,]
adresse_complement_distance$geocodage_main <- NA

# finalement on vire les  deja geocodés

adresse_complement_distance <- adresse_complement_distance[!adresse_complement_distance$ID_CARTO %in% geocoder_main$adresse_id,]

adresse_matthieu <- adresse_complement_distance[1:(length(adresse_complement_distance$ID_CARTO)/2),]
st_write(adresse_matthieu, "data/verif/adresse_matthieu.geojson" )

adresse_olivier <- adresse_complement_distance[(length(adresse_complement_distance$ID_CARTO)/2):length(adresse_complement_distance$ID_CARTO),]
st_write(adresse_olivier, "data/verif/adresse_olivier.geojson")
