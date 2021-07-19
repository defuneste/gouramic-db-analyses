## cas identique entre les geocodeurs

source("reassemblage_geocodage_mains.R")


##### rural / urbain 

communes.shp <- sf::st_read("data/commune.shp")

# on va chercher l'adresse 
geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

adresse_complement <- geocodage_clb.shp %>% 
    dplyr::select(ID_CARTO, Commune, CP, nb_rue_p, rue_p, compl_add_, pt_remarq_, lieudit_p ) %>% 
    tidyr::unite("Adresse", nb_rue_p, rue_p, sep = " ",  na.rm = TRUE) %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, na.rm = TRUE)


adresse_complement_distance <- dplyr::inner_join(adresse_complement, adresse_filtre, by = "ID_CARTO")

pas_verif_a_la_main <- adresse_complement_distance[!adresse_complement_distance$ID_CARTO %in% verif_ecartv3.shp$ID_CARTO,] 

adresse_commune.shp <- st_join(
    pas_verif_a_la_main, 
    st_transform(communes.shp[,c("TYPE_CO", "insee")], 2154))


table(adresse_commune.shp$TYPE_CO)