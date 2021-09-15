# Date: Fevrier 2021
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: Recupérer l'ensemble des adresses qui avant le geocodage manuelle étaient a plus de 5 m
# Description du problème:
# réassemblage des fichier géocodées, verifees après la mesure de distance entre les deux geocodages.
#
# Libraries utilisées:
# "dplyr",  "sf" 

library(dplyr)
library(sf)

# I 4075 addresses avec un ecart de plus de 5 m vérifiées =============================== 

# on va charger les différentes addresses verifiées 
part_oli <- sf::read_sf("data/verif/adresse_olivier.geojson")
part_oli$geocodeur <- "olivier"
# 1299

part_matt_a <- sf::read_sf("data/verif/adresse_matthieuok.geojson") 
part_matt_a$geocodeur <- "matthieu"
# 2090

part_matt_b <- sf::read_sf("data/verif/adresse_matthieu_bisbok.geojson")
part_matt_b$geocodeur <- "matthieu"

# il y avait des doublons ... j'ai gardé ceux de Matthieu
part_oli <- part_oli[!part_oli$ID_CARTO %in% part_matt_a$ID_CARTO,]

# on regroupe puis on supprime
verif_ecart.shp <- rbind(part_oli, 
                         part_matt_a, 
                         part_matt_b)

rm(part_matt_a, 
   part_matt_b, 
   part_oli)


# II on rajoute ce qui avait dejà éte fait à la main =================================

## ce qui avait deja éte geocodé à la main 
# cf rapport/prepa_publi.Rmd pour la prod du geojson
adresse <- sf::read_sf("data/verif/distance.geojson") %>% 
                 sf::st_drop_geometry() %>% 
                 dplyr::mutate(distance = as.numeric(distance)) 

# cf geocoderalmain.R
geocoder_main <- sf::read_sf("data/geocodage_main_total2.geojson")

# on en a 93 qui avaient déja été faites ...
# geocoder_main[geocoder_main$adresse_id %in% verif_ecart.shp$ID_CARTO, ]

deja_fait <- geocoder_main[geocoder_main$adresse_id %in% adresse$ID_CARTO ,]

ce_qui_manque <- deja_fait[!deja_fait$adresse_id %in% verif_ecart.shp$ID_CARTO, ]

ce_qui_manque.shp <- dplyr::left_join(ce_qui_manque, 
                               adresse, 
                               by = c("adresse_id" = "ID_CARTO")) %>% 
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

verif_ecartv2.shp$preci_clb <-  substr(verif_ecartv2.shp$preci_clb, 1, 1)
# correction d'un street qui devrait être 3
verif_ecartv2.shp$preci_clb[verif_ecartv2.shp$preci_clb  =="s"] <- "3" 

rm(ce_qui_manque, ce_qui_manque.shp, deja_fait, geocoder_main, verif_ecart.shp)

### rajout des corrections de precisions differentes 
# le script precision_differente_mais_proche.R doit etre revu

preci_diff <- sf::read_sf("data/verif/preci_diff_oli.geojson") %>% 
                dplyr::arrange(ID_CARTO) %>% 
                dplyr::rename(preci_clb = Loc_name)

preci_diff$geocodeur <- "olivier"
preci_diff$preci_evs <- NA
preci_diff$geocodage_main <- NA
preci_diff$distance <- adresse$distance[adresse$ID_CARTO %in% preci_diff$ID_CARTO]

# verif si les noms des colonnes coorespondent
# names(verif_ecartv2.shp)[!names(verif_ecartv2.shp) %in% names(preci_diff)]

# on ne garde que ce que l'on a besoin
preci_diff <- preci_diff[,names(verif_ecartv2.shp)]

verif_ecartv3.shp <- rbind(verif_ecartv2.shp, preci_diff)
#   rm(verif_ecartv2.shp)