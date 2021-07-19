
#### mesure de l'efficacité du geocodage à la main 
# distance 
# changement de classe
# pe ensuite en faire un script propre

# en attendant de faire un truc plus propre
source("reassemblage_geocodage_mains.R")


########## Changement de classe ======================== 
# il faut corriger le preci_clb pour ne prendre que le premier char et pe passer en facteur 

dim(adresse_filtre[adresse_filtre$distance > 5,])

#from <- adresse_filtre[adresse_filtre$distance > 5,]
from <- adresse_filtre[adresse_filtre$ID_CARTO %in% verif_ecartv3.shp$ID_CARTO,]

to <- verif_ecartv3.shp

table(verif_ecartv3.shp$preci_clb)

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


### on regarde avec le temps 
# il faut ajouter les precisions 

geocode_annee.shp <- st_drop_geometry(geocode_annee.shp)

# on est sur 4180, le fait plutot sur l'ensemble ?
distance_année.shp <- left_join(avec_distance.dat, geocode_annee.shp, by = c("ID_CARTO" = "ID_CARTO")) 
# rajoute on rural urbain
distance_année.shp <- st_join(distance_année.shp,
                              st_transform(communes.shp[,c("TYPE_CO", "insee")], 2154))


# après du base pkoi pas du tidyverse
distance_année <- st_drop_geometry(distance_année.shp) %>% 
    group_by(preci_clb) %>% 
    summarize(moy_annee = mean(annee , na.rm = T),
              med_annee = median(annee, na.rm = T),
              IQR_annee = IQR(annee, na.rm = T),
              sd_annee = sd(annee, na.rm = T)
    )

library(ggplot2)
# sur les 4180 de matrice de distance là encore pe pas l'ensemble le plus pertinent
distance_année.shp %>% 
    st_drop_geometry() %>%
    ggplot(aes(x = annee, y = distance, col = TYPE_CO)) +
    geom_point() + 
    ylim(c(0,10000))

# sur les 1177 avec erreurs
distance_année.shp %>% 
    filter(distance != 0) %>% 
    st_drop_geometry() %>%
    ggplot(aes(x = annee, y = distance, col = TYPE_CO)) +
    geom_point() + 
    ylim(c(0,10000))
