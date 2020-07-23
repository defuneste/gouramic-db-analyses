## test pour normaliser les adresses

##.###################################################################################33
## I. Chargement des données et Mise en forme ====
##.#################################################################################33

# 1- chargement des packages et données ========================

pkgs <-  c("dplyr","stringr", "lubridate", "ggplot2", "sf", "microbenchmark")
inst <- lapply(pkgs, library, character.only = TRUE)


# toutes les adresses sans les 48 vides
allsujet_SansNA.dat <- readRDS("data/allsujet_cleanNA.rds")
geocodage_evs.shp <- sf::st_read("data/geocodev2.geojson", stringsAsFactors = FALSE) %>% 
                        sf::st_transform(2154)

geocodage_clb.shp <- sf::st_read("data/Geocoding_Result.shp", stringsAsFactors = FALSE)


# 2- un seul fichier avec un point = une ligne

# pas mal d'info et un benchmark ici : https://github.com/r-spatial/sf/issues/669

# test_compar <- microbenchmark(times = 10,  
#                          r1 <- st_difference(geocodage_evs.shp),
#                         r2 <- distinct(geocodage_evs.shp, .keep_all = TRUE),
#                         r3 <- st_intersection(st_transform(geocodage_evs.shp, 2154)),
#                         r4 <- geocodage_evs.shp[!duplicated(geocodage_evs.shp[,"geometry"]),]
#                             )   

# j'ai pris distinct pour piper un peu meme si il est un tout petit plus lents
# je trouve pas que intersection et difference font sens sur des points

# il faut aussi filtrer les cas où il n'y qu'une adresse et manquante
sujet.dat <- allsujet_SansNA.dat %>% 
    dplyr::mutate(sujet = substr(allsujet_SansNA.dat$Id_cart, 1,7)) %>% 
    dplyr::select(sujet, Date_birth) %>% 
    dplyr::group_by(sujet) %>% 
    dplyr::summarize(Date_birth = first(Date_birth))

# un exemple de preparation de données à partir du geocoadage EVS
table_adresse_test <- geocodage_evs.shp %>% 
                           dplyr::mutate(sujet_id = substr(geocodage_evs.shp$Id_cart, 1,7),
                                    adresse_clb = as.numeric(str_extract(geocodage_evs.shp$Id_cart, pattern = "[0-9]{1,2}?$"))) %>% 
                            dplyr::select(sujet_id, adresse_clb, result_type, source_loc, geometry) %>% 
                            dplyr::distinct(.keep_all = TRUE) %>% 
                            dplyr::mutate(adresse_id = row_number()) %>% 
                            # me faut reorder et mettre dans le bon CRS 
                            dplyr::select(adresse_id, sujet_id, adresse_clb, result_type,source_loc, geometry) %>% 
                            sf::st_transform(2154) %>% 
                            # ici on filtre les sujet sans residences
                            dplyr::filter(sujet_id %in% sujet.dat$sujet)

length(unique(table_adresse_test$sujet_id))

# option 1 dans un csv
# write.table(table_adresse_test, 
#             "data/adresse.csv", 
#             sep = ";",
#             quote = FALSE,
#             row.names = FALSE,
#             col.names=FALSE) 

# option 2 via un shapefile  

# st_write(table_adresse_test, dsn = "data/adresse.shp")


##.###################################################################################33
## I. Chargement des données corrigées ====
##.#################################################################################33

## une partie de données a été corrigées à la main et une autre est issue du géocodage
# nous avons decidé de prendre celui d'ESRI comme base
# il faut regrouper les données et normaliser les adresses
# via leur localisation ex :
# On peut avoir une localisation sur un lieu dit imprecis mais il nous faut cependant la 
# meme adresse

## 1. hors du geocodage main



## 2. lecture des deux fichiers geocoder à la main + celui que j'avais fait avant ===============

### 2.1 lecture geocodage mains fait en premier cf. precision_geocodage.R
# j'ai touts reverifier en juillet

geocodage_evs_oli.shp <- sf::st_read("data/geocode_mains_Na.geojson") %>% 
    dplyr::select(adresse_id = Id_cart,
                  Date_start,
                  Date_end,
                  Commune,
                  Adresse,
                  Postal = Code_postal,
                  precision = result_type) %>% 
    sf::st_transform(2154)

geocodage_evs_oli.shp$precision <- as.numeric(geocodage_evs_oli.shp$precision) 

summary(geocodage_evs_oli.shp)

# on exporte et on va verifier à la main

### 2.2 lecture geocodge olivier + matthieu 

RM2.shp <- sf::st_read("data/REgeocodage/RM2_OL.shp") %>% 
                dplyr::select(adresse_id = Id_cart,
                              Date_start,
                              Date_end,
                              Commune,
                              Adresse,
                              Postal,
                              precision = New_Loc) %>% 
                dplyr:: mutate(source_codage = "main")

summary(RM2.shp)

geocodage_clb_oli.shp <- sf::st_read("data/REgeocodage/geocodage_clb_oli.shp" ) %>% 
                              dplyr::select(adresse_id = ID_CARTO,
                                            Date_start = date_start,
                                            Date_end = date_end_a,
                                            Commune,
                                            Adresse,
                                            CP,
                                            New_LocNam) %>% 
                            dplyr::mutate(sujet_id = substr(adresse_id, 1,7),
                                          precision = substr(New_LocNam, 1, 1))

summary(geocodage_clb_oli.shp ) 

### 2.3 on rajoute on_ne_fera_pas mieux.

on_ne_fera_pas_mieux.shp <- sf::st_read("data/on_fera_pas_mieux.csv")

on_ne_fera_pas_mieux_add.shp <- geocodage_clbv2.shp[geocodage_clbv2.shp$ID_CARTO %in% on_ne_fera_pas_mieux.shp$Id_cart,] %>% 
                        dplyr::select(adresse_id = ID_CARTO,
                                      Date_start = date_start,
                                      Date_end = date_end_a,Commune,
                                      Adresse,
                                      CP,
                                      Loc_name) %>% 
                        dplyr::mutate(sujet_id = substr(adresse_id, 1,7),
                                      precision = substr(Loc_name, 1, 1))
                        

rm(on_ne_fera_pas_mieux.shp)

##.###################################################################################33
## II. Normalisation des adresses ====
##.#################################################################################33
