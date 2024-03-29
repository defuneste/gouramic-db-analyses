# Date: Juillet 2020 / Janvier 2021
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: Recompose tout ce qui a été geocodé à la main 
# Description du problème: 
# Recompose tout ce qui a été geocodé à la main
#
# Libraries utilisées:
# "dplyr", "lubridate",  "sf"

pkgs <-  c("dplyr", "lubridate",  "sf")
inst <- lapply(pkgs, library, character.only = TRUE)

# 1. lecture des deux fichiers geocodées à la main + celui que j'avais fait avant ===============
# j'avais fait une erreur dans les date_End et il me faut la coriger dans ces imports 
# correction de l'erreur de date_end

geocodage_evs.shp <- sf::read_sf("data/geocodev2.geojson", stringsAsFactors = FALSE) %>% 
    sf::st_transform(2154)
geocodage_evs.shp$Date_end <- as.Date(geocodage_evs.shp$Date_end, origin = "1899-12-30")

dist.dat <- read.csv("data/DistClean.csv", stringsAsFactors = FALSE) 
NA_dist.dat <- geocodage_evs.shp[!geocodage_evs.shp$Id_cart %in%  dist.dat$ID_CARTO ,]

### 1.1 lecture geocodage mains fait en premier cf. precision_geocodage.R ========================

geocodage_evs_oli.shp <- sf::read_sf("data/geocode_mains_Na.geojson") %>% 
    dplyr::select(adresse_id = Id_cart,
                  date_start = Date_start,
                  commune = Commune,
                  adresse = Adresse,
                  cp = Code_postal,
                  info_sup = Info_sup,
                  precision = result_type) %>% 
    dplyr::mutate(sujet_id = substr(adresse_id, 1,7),
                  source_codage = "Main", 
                  precision = as.numeric(precision)) %>% 
    sf::st_transform(2154)

correction <- NA_dist.dat[NA_dist.dat$Id_cart %in% geocodage_evs_oli.shp$adresse_id, c("Id_cart", "Date_end")]
correction <- st_drop_geometry(correction)

geocodage_evs_oli.shp <- left_join(geocodage_evs_oli.shp,
                                   correction,
                                   by = c("adresse_id" = "Id_cart")) %>% 
                        select(adresse_id, 
                               date_start, 
                               date_end = Date_end, 
                               commune, 
                               adresse, 
                               cp,
                               info_sup,
                               precision,
                               sujet_id, 
                               source_codage,
                               geometry)

rm(dist.dat, geocodage_evs.shp, correction)

### 1.2 lecture geocodage Olivier + Matthieu ==========================
# ici il y avait une erreur sur date fin à corriger via Na_dist.dat de precision_geocodage.R
# ces deux geocodages correspondent au données manquantes entre les 

# la partie de Matthieu
RM2.shp <- sf::read_sf("data/REgeocodage/RM2_OL.shp") %>% 
    dplyr::select(adresse_id = Id_cart,
                  date_start = Date_start,
                  commune = Commune,
                  adresse = Adresse,
                  cp = Postal,
                  info_sup = Info_sup,
                  precision = New_Loc) %>% 
    dplyr:: mutate(sujet_id = substr(adresse_id, 
                                     1,
                                     7),
                   source_codage = "Main")

# passer trois plombes à trouver cette erreur
RM2.shp$adresse_id[RM2.shp$adresse_id == "08_006_2"] <- "08_0006_2"

correction <- NA_dist.dat[NA_dist.dat$Id_cart %in% RM2.shp$adresse_id, c("Id_cart", "Date_end")]
correction <- st_drop_geometry(correction)

RM2.shp <- left_join(RM2.shp, 
                     correction, 
                     by = c("adresse_id" = "Id_cart")) %>% 
            select(adresse_id,
                   date_start,
                   date_end = Date_end,
                   commune, 
                   adresse,
                   cp,
                   info_sup,
                   precision,
                   sujet_id,
                   source_codage,
                   geometry) %>% 
            mutate(precision = as.numeric(precision))

rm(correction)

# la partie d'Olivier
geocodage_clb_oli.shp <- sf::read_sf("data/REgeocodage/geocodage_clb_oli.shp" ) %>% 
    dplyr::select(adresse_id = ID_CARTO,
                  date_start = date_start,
                  date_end = date_end_a,
                  commune = Commune,
                  adresse = Adresse,
                  cp = CP,
                  info_sup = Info_sup,
                  New_LocNam) %>% 
    dplyr::mutate(sujet_id = substr(adresse_id, 1,7),
                  precision = substr(New_LocNam, 1, 1), 
                  source_codage = "Main") %>% 
    dplyr::select(-New_LocNam) %>% 
    dplyr::mutate(precision = as.numeric(precision))

# un pb avec une adresse qui doit rester inconnue
geocodage_clb_oli.shp$precision[geocodage_clb_oli.shp$adresse_id == "20_0755_2"] <- NA

### 1.3 on rajoute on_ne_fera_pas mieux. ===========================
# c'est les cas d'adresses où aucune magie ne nous les fera deviner
geocodage_clb.shp <- sf::read_sf("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

# correction formatage
geocodage_clb.shp$date_start <- lubridate::parse_date_time(geocodage_clb.shp$date_start, orders = c("my", "dmy"))
geocodage_clb.shp$date_end_a <- lubridate::parse_date_time(geocodage_clb.shp$date_end_a, orders = c("my", "dmy"))

on_ne_fera_pas_mieux.shp <- sf::read_sf("data/on_fera_pas_mieux.csv")

on_ne_fera_pas_mieux_add.shp <- geocodage_clb.shp[geocodage_clb.shp$ID_CARTO %in% on_ne_fera_pas_mieux.shp$Id_cart,] %>% 
    tidyr::unite(info_sup, 
                 lieudit_p,
                 compl_add_,
                 pt_remarq_,
                 na.rm = TRUE) %>% 
    dplyr::select(adresse_id = ID_CARTO,
                  date_start = date_start,
                  date_end = date_end_a,
                  commune = Commune,
                  adresse = Adresse,
                  cp = CP,
                  info_sup,
                  Loc_name) %>% 
    dplyr::mutate(sujet_id = substr(adresse_id, 1,7),
                  precision = as.numeric(substr(Loc_name, 1, 1)), 
                  source_codage = "ESRI") %>% 
    dplyr::select(-Loc_name)

rm(on_ne_fera_pas_mieux.shp)

### 1.4 on regroupe le tout 

## un vecteur avec le bon ordre de columns 
vec_ordre <- c("adresse_id", "date_start", "date_end", "commune", "cp", "info_sup","sujet_id", "precision", "source_codage", "geometry")

# on ordonne le tout
geocodage_evs_oli.shp <- geocodage_evs_oli.shp[vec_ordre]
on_ne_fera_pas_mieux_add.shp <- on_ne_fera_pas_mieux_add.shp[vec_ordre]
RM2.shp <- RM2.shp[vec_ordre]
geocodage_clb_oli.shp <- geocodage_clb_oli.shp[vec_ordre]

# on ajoute 
geocode_main_totale.shp  <- rbind(geocodage_evs_oli.shp, 
                                  on_ne_fera_pas_mieux_add.shp,
                                  RM2.shp, 
                                  geocodage_clb_oli.shp)

rm(geocodage_evs_oli.shp, on_ne_fera_pas_mieux_add.shp, RM2.shp, geocodage_clb_oli.shp, vec_ordre, NA_dist.dat)

#st_write(geocode_main_totale.shp, "data/geocodage_main_total2.geojson")
