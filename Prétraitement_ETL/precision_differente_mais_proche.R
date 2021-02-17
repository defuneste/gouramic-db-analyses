#### ici je vais sortir les cas de type de precisions mais proche pour les regrouper avec ceux loin mais identique

source("reassemblage_geocodage_mains.R")

## modifier verif_ecartv3 par v2 si on veut exclure les proches mais avec une categorie de precision différente
pas_verif_a_la_main <- adresse_filtre[!adresse_filtre$ID_CARTO %in% verif_ecartv3.shp$ID_CARTO,] 

library(dplyr)
library(lubridate)

pas_verif_a_la_main$preci_clb

pas_verif_a_la_main <- pas_verif_a_la_main %>% 
    mutate(preci_clb = case_when(
                            preci_clb == "1_PointAdresse"  ~ 1,
                            preci_clb == "2_AdresseInter" ~ 2,
                            preci_clb == "3_Voie" ~ 3,
                            preci_clb ==  "4_LieuDitHabit" ~ 4),
           preci_evs = case_when(
                            preci_evs == "housenumber" ~ 1,
                            preci_evs == "street" ~ 3,
                            preci_evs == "locality" ~ 4
           )
    )

table(pas_verif_a_la_main$preci_clb)

# c'est vide si on met la v3
a_verif <- pas_verif_a_la_main[pas_verif_a_la_main$preci_clb != pas_verif_a_la_main$preci_evs,]


#CLB
geocodage_clbv2.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)
#correction formatage
geocodage_clbv2.shp$date_start <- parse_date_time(geocodage_clbv2.shp$date_start, orders = c("my", "dmy"))
geocodage_clbv2.shp$date_end_a <- parse_date_time(geocodage_clbv2.shp$date_end_a, orders = c("my", "dmy"))

geocodage_clbv2.shp <- geocodage_clbv2.shp %>% 
    dplyr::select(ID_CARTO, Loc_name, Commune, CP, nb_rue_p, rue_p, compl_add_, pt_remarq_, lieudit_p ) %>% 
    tidyr::unite("Adresse", nb_rue_p, rue_p, sep = " ",  na.rm = TRUE) %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, na.rm = TRUE)

# fichier de geocodage EVS
geocodage_evs.shp <- sf::st_read("data/geocodev2.geojson", stringsAsFactors = FALSE)
geocodage_evs.shp$Date_birth <- as.Date(geocodage_evs.shp$Date_birth, origin = "1899-12-30")
geocodage_evs.shp$Date_start <- as.Date(geocodage_evs.shp$Date_start, origin = "1899-12-30")
geocodage_evs.shp$Date_end <- as.Date(geocodage_evs.shp$Date_end, origin = "1899-12-30")

geocodage_clbv2.shp <- geocodage_clbv2.shp[geocodage_clbv2.shp$ID_CARTO %in% a_verif$ID_CARTO,]

geocodage_evs.shp <- geocodage_evs.shp %>% 
    dplyr::select(ID_CARTO = Id_cart, precision = result_type ) %>% 
    st_drop_geometry()

geocodage_evs.shp <- geocodage_evs.shp[geocodage_evs.shp$ID_CARTO %in% a_verif$ID_CARTO,]

join_adresse <- left_join(geocodage_clbv2.shp, geocodage_evs.shp, by = c("ID_CARTO" = "ID_CARTO"))

st_write(join_adresse, "data/verif/preci_diff.geojson")