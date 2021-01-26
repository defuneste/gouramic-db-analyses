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

rm(ce_qui_manque, ce_qui_manque.shp, deja_fait, geocoder_main, verif_ecart.shp)


#### mesure de l'efficacité du geocodage à la main 
# distance 
# changement de classe
# pe ensuite en faire un script propre


########## Changement de classe ======================== 
# il faut corriger le preci_clb pour ne prendre que le premier char et pe passer en facteur 

dim(adresse_filtre[adresse_filtre$distance > 5,])
from <- adresse_filtre[adresse_filtre$distance > 5,]

verif_ecartv2.shp$preci_clb <-  substr(verif_ecartv2.shp$preci_clb, 1, 1)
# correction d'un street qui devrait être 3
verif_ecartv2.shp$preci_clb[verif_ecartv2.shp$preci_clb  =="s"] <- "3" 

to <- verif_ecartv2.shp

table(verif_ecartv2.shp$preci_clb)

# on va dropper la geometrie et faire une jointure entre les deux jeu de données puis faire une matrice 

from_to <- to %>% 
                st_drop_geometry() %>% 
                left_join(from, by = c("ID_CARTO" = "ID_CARTO"),
                           suffix = c(".to", ".from"))

# from_to$preci_clb.from <- substr(from_to$preci_clb.from, 1, 1)

table(from_to$preci_clb.from, from_to$preci_clb.to)


####### Distance / precision gagnée spatialement 
# pour la distance il y a plusieurs options 
# le géocodage ESRI, celui banR
# un point au milieu 
# j'ai pris l'option 1
# on peut aussi prendre le type de lieu d'arriver ou de départ, j'ai pris arrivé

geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

geocodage_clb_simp.shp <- geocodage_clb.shp[,c("ID_CARTO", "Loc_name")]
names(geocodage_clb_simp.shp) <- c("ID_CARTO", "preci_clb", "geometry")

geocodage_clb_simp.shp <- geocodage_clb_simp.shp[geocodage_clb_simp.shp$ID_CARTO %in% verif_ecartv2.shp$ID_CARTO,]

verif_simp.shp <- verif_ecartv2.shp[, c("ID_CARTO", "preci_clb")]

band_of_bistance  <- rbind(verif_simp.shp, geocodage_clb_simp.shp)

compare_distance <- aggregate(band_of_bistance, by = list(band_of_bistance$ID_CARTO), first)

sans_distance <- compare_distance[st_is(compare_distance, "POINT"),]

sans_distance$distance <- 0 

avec_distance <- compare_distance[!st_is(compare_distance, "POINT"),]
avec_distance <- st_cast(avec_distance, "LINESTRING")
avec_distance$distance <- st_length(avec_distance)

summary(avec_distance$distance)

aggregate(avec_distance$distance, by = list(avec_distance$preci_clb), sum)
aggregate(avec_distance$distance, by = list(avec_distance$preci_clb), median)
aggregate(avec_distance$distance, by = list(avec_distance$preci_clb), mean)
table(avec_distance$preci_clb)


sum(avec_distance$distance)

### rural urbain 
# le chargement de communes c'est fait ailleurs 

# on reprend le shape des communes avec l'info rural-urbain-peri
communes.shp <- st_read("data/commune.shp")

# il faut reprendre coords verif et pas les lignes

avec_distance.dat <- st_drop_geometry(avec_distance) 

avec_distance.dat <- verif_simp.shp %>% left_join(avec_distance.dat[,c("ID_CARTO", "distance")], by = c("ID_CARTO" = "ID_CARTO"))
avec_distance.dat$distance <- as.numeric(avec_distance.dat$distance)
avec_distance.dat$distance[is.na(avec_distance.dat$distance)] <- 0

# rajout du type de commune par adresse !!! attention c'est le type de commune en 2019
adresse_commune.shp <- st_join(avec_distance.dat,
                               st_transform(communes.shp[,c("TYPE_CO", "insee")], 2154))

adresse_commune <- st_drop_geometry(adresse_commune.shp)
adresse_commune$distance <- as.numeric(adresse_commune$distance)

adresse_commune <- adresse_commune[adresse_commune$distance != 0,]

distance_tab <- aggregate(adresse_commune$distance, by = list(adresse_commune$TYPE_CO, adresse_commune$preci_clb), length)



names(distance_tab) <- c("Type_co", "Preci_clb", "nombre")

distance_tab$total <- aggregate(adresse_commune$distance, by = list(adresse_commune$TYPE_CO, adresse_commune$preci_clb), sum)$x
                              
distance_tab$median <- aggregate(adresse_commune$distance, by = list(adresse_commune$TYPE_CO, adresse_commune$preci_clb), median)$x

distance_tab$mean <- aggregate(adresse_commune$distance, by = list(adresse_commune$TYPE_CO, adresse_commune$preci_clb), mean)$x

distance_tab$IQR <- aggregate(adresse_commune$distance, by = list(adresse_commune$TYPE_CO, adresse_commune$preci_clb), IQR)$x

sum(distance_tab$total)  

openxlsx::write.xlsx(distance_tab, "data/distance_rurbain_erreur.xls")
