# verif des distances entre les deux geocodages 

library(sf)

# rapport/prepa_publi.Rmd

adresse <- sf::st_read("data/verif/distance.geojson")
adresse_filtre <- adresse %>% 
                sf::st_drop_geometry() %>% 
                dplyr::select(ID_CARTO, preci_clb, preci_evs, distance)
            
# données deja geocodées

geocoder_main <- sf::st_read("data/geocodage_main_total.geojson")

# on va chercher l'adresse 
geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

adresse_complement <- geocodage_clb.shp %>% 
    dplyr::select(ID_CARTO, Commune, CP, nb_rue_p, rue_p, compl_add_, pt_remarq_, lieudit_p ) %>% 
    tidyr::unite("Adresse", nb_rue_p, rue_p, sep = " ",  na.rm = TRUE) %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, na.rm = TRUE)


adresse_complement_distance <- dplyr::left_join(adresse_complement, )