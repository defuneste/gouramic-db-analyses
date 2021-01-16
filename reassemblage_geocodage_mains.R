### reassemblage des fichier géocoder verifier après la mesure de distance entre les desux geocodages.
# objectif recupérer l'ensemble des adresses qui avant le geocodages était a plus de 5 m

library(sf)
library(dplyr)

part_oli <- sf::st_read("data/verif/adresse_olivier.geojson")
part_oli$geocodeur <- "olivier"
# 1299

part_matt_a <- sf::st_read("data/verif/adresse_matthieuok.geojson") 
part_matt_a$geocodeur <- "matthieu"
# 2090

part_matt_b <- sf::st_read("data/verif/adresse_matthieu_bisbok.geojson")
part_matt_b$geocodeur <- "matthieu"

# # on en a 7 en commun 
# verif_ecart.shp %>% 
#     dplyr::group_by(ID_CARTO) %>% 
#     mutate(count = n()) %>% 
#     ungroup() %>% 
#     filter(count > 1)
# il y avait 7 identiques j'ai pris ceux de matthieu
part_oli <- part_oli[!part_oli$ID_CARTO %in% part_matt_a$ID_CARTO,]

verif_ecart.shp <- rbind(part_oli, part_matt_a, part_matt_b)
rm(part_matt_a, part_matt_b, part_oli)

dim(verif_ecart.shp)

table(verif_ecart.shp$geocodage_main)

## ce qui avait deja éte goecodé à la main 
adresse <- sf::st_read("data/verif/distance.geojson")
adresse_filtre <- sf::st_drop_geometry(adresse) 
adresse_filtre$distance <-as.numeric(adresse_filtre$distance)
rm(adresse)

geocoder_main <- sf::st_read("data/geocodage_main_total.geojson")

# on en a 93 qui avaient déja été fait ...
geocoder_main[geocoder_main$adresse_id %in% verif_ecart.shp$ID_CARTO, ]

deja_fait <- geocoder_main[geocoder_main$adresse_id %in% adresse_filtre$ID_CARTO ,]

ce_qui_manque <- deja_fait[!deja_fait$adresse_id %in% verif_ecart.shp$ID_CARTO, ]

ce_qui_manque.shp <- left_join(ce_qui_manque, adresse_filtre, by = c("adresse_id" = "ID_CARTO")) %>% 
    dplyr::mutate(geocodage_main = "12",
                  Adresse = NA) %>% 
    dplyr::select(ID_CARTO = adresse_id,
                  Commune = commune,
                  CP = cp, 
                  Adresse,
                  Info_sup = info_sup,
                  preci_clb = precision,
                  preci_evs,
                  distance, 
                  geocodage_main,
                  geocodeur = source_codage)

verif_ecartv2.shp <- rbind(verif_ecart.shp, ce_qui_manque.shp)


