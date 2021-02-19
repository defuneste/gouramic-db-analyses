# assemblage d'un fichier après les différents géocodage

# on a besoin des verif à la main 
# des pas verif à là main 

source("Prétraitement_ETL/reassemblage_geocodage_mains.R") # les verif à la main 

geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

adresse_complement <- geocodage_clb.shp %>% 
    dplyr::select(ID_CARTO, Commune, CP, nb_rue_p, rue_p, compl_add_, pt_remarq_, lieudit_p ) %>% 
    tidyr::unite("Adresse", nb_rue_p, rue_p, sep = " ",  na.rm = TRUE) %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, na.rm = TRUE)


adresse_complement_distance <- dplyr::inner_join(adresse_complement, adresse_filtre, by = "ID_CARTO")

pas_verif_a_la_main <- adresse_complement_distance[!adresse_complement_distance$ID_CARTO %in% verif_ecartv3.shp$ID_CARTO,] 
pas_verif_a_la_main[,"preci_evs"] <- NULL
pas_verif_a_la_main$preci_clb <- substr(pas_verif_a_la_main$preci_clb, 1, 1)

de_cotecorrigé.shp <- st_read("data/verif/de_coteMatthieu.geojson")
de_cotecorrigé.shp$preci_clb <- substr(de_cotecorrigé.shp$Loc_name, 1, 1)
de_cotecorrigé.shp$distance  <-  "NA"

names(de_cotecorrigé.shp)

a_garder <- names(pas_verif_a_la_main)

merge <- rbind(verif_ecartv3.shp[,a_garder], pas_verif_a_la_main, de_cotecorrigé.shp[,a_garder])
