## test pour nomaliser les adresses

##.###################################################################################33
## I. Chargement des données et Mise en forme ====
##.#################################################################################33

# 1- chargement des packages et données ========================

pkgs <-  c("dplyr","stringr", "lubridate", "ggplot2", "sf", "microbenchmark")
inst <- lapply(pkgs, library, character.only = TRUE)


allsujet_SansNA.dat <- readRDS("data/allsujet_cleanNA.rds")
geocodage_evs.shp <- sf::st_read("data/geocodev2.geojson", stringsAsFactors = FALSE)
geocodage_clb.shp <- sf::st_read("data/Geocoding_Result.shp", stringsAsFactors = FALSE)


# 2- un seul fichier avec un point = une ligne

# pas mal d'info et un benchmark ici : https://github.com/r-spatial/sf/issues/669

test_compar <- microbenchmark(times = 10,  
                         r1 <- st_difference(geocodage_evs.shp),
                        r2 <- distinct(geocodage_evs.shp, .keep_all = TRUE),
                        r3 <- st_intersection(st_transform(geocodage_evs.shp, 2154)),
                        r4 <- geocodage_evs.shp[!duplicated(geocodage_evs.shp[,"geometry"]),]
                            )   

# j'ai pris distcint pour piper un peu meme si il est un tout petit plus lents
# je trouve pas que intersection et difference font sens sur des points

table_adresse_test <- geocodage_evs.shp %>% 
                            dplyr::mutate(sujet_id = substr(geocodage_evs.shp$Id_cart, 1,7),
                                    adresse_clb = as.numeric(str_extract(geocodage_evs.shp$Id_cart, pattern = "[0-9]{1,2}?$"))) %>% 
                            dplyr::select(sujet_id, adresse_clb, result_type, source_loc, geometry) %>% 
                            dplyr::distinct(.keep_all = TRUE)

write.table(table_adresse_test, 
            "data/adresse.csv", 
            sep = ",",
            quote = FALSE,
            row.names = FALSE,
            col.names=FALSE) 
    