# exploration des cas avec un seul géocodage
# dit 887

library(sf)
library(lubridate)
library(dplyr)

# les cas avec les deux 
adresse <- sf::st_read("data/verif/distance.geojson")

# le géocodage esri

geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

#correction formatage
geocodage_clb.shp$date_start <- parse_date_time(geocodage_clb.shp$date_start, orders = c("my", "dmy"))
geocodage_clb.shp$date_end_a <- parse_date_time(geocodage_clb.shp$date_end_a, orders = c("my", "dmy"))

geocodage_clb.shp <- geocodage_clb.shp %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, sep = " ", na.rm = TRUE) %>% 
    dplyr::select(ID_CARTO, date_start, date_end_a, Commune, Adresse, CP, Info_sup, Match_addr, Loc_name)

de_cote.shp <- geocodage_clb.shp[!geocodage_clb.shp$ID_CARTO %in% adresse$ID_CARTO,]

# table(de_cote.shp$Loc_name, useNA = "ifany")

# on vire les Na et on regarde la répartition rural urbain

de_cote.shp <- de_cote.shp[!is.na(de_cote.shp$Loc_name),]

# on reprend le shape des communes avec l'info rural-urbain-peri
communes.shp <- st_read("data/commune.shp")

# rajout du type de commune par adresse !!! attention c'est le type de commune en 2019
adresse_commune.shp <- st_join(de_cote.shp,
                               st_transform(communes.shp[,c("TYPE_CO", "insee")], 2154))

table(adresse_commune.shp$Loc_name, adresse_commune.shp$TYPE_CO)
# st_write(de_cote.shp, "data/verif/de_cote.geojson")

### lecture du fichier mis de coté ================================

de_cotecorrigé.shp <- st_read("data/verif/de_coteMatthieu.geojson")

str(de_cotecorrigé.shp)
table(de_cotecorrigé.shp$Loc_name)

de_cotecorrigé.shp$Loc_name <- substr(de_cotecorrigé.shp$Loc_name, 1, 1)

# rajout du type de commune par adresse !!! attention c'est le type de commune en 2019
adresse_commune.shp <- st_join(de_cotecorrigé.shp,
                               st_transform(communes.shp[,c("TYPE_CO", "insee")], 2154))

table(adresse_commune.shp$Loc_name, adresse_commune.shp$TYPE_CO)

## on a 99 adresses qui
#ont eu une precision de changées
# si on veut regarder les distances 

# on doit prendre de cote et de coté corriger
# les simplifier/homogeneiser 

nom_a_garder <- c("ID_CARTO", "Loc_name")

de_cote.shp <- de_cote.shp[, nom_a_garder] 
de_cotecorrigé.shp <- de_cotecorrigé.shp[, nom_a_garder]

# matrice de diff
de_cote.dat <- st_drop_geometry(de_cote.shp)
de_cotecorrigé.dat <- st_drop_geometry(de_cotecorrigé.shp)
join_decote <- left_join(de_cote.dat, de_cotecorrigé.dat, by = c("ID_CARTO" = "ID_CARTO"), suffix = c("_avant", "_après"))

table(join_decote$Loc_nameavant, join_decote$Loc_nameaprès)

de_cote_distance <- rbind(de_cotecorrigé.shp, de_cote.shp)

compare_distance <- aggregate(de_cote_distance, by = list(de_cote_distance$ID_CARTO), first)

sans_distance <- compare_distance[st_is(compare_distance, "POINT"),]

sans_distance$distance <- 0 

avec_distance <- compare_distance[!st_is(compare_distance, "POINT"),]
avec_distance <- st_cast(avec_distance, "LINESTRING")
avec_distance$distance <- st_length(avec_distance)

sum(avec_distance$distance)
mean(avec_distance$distance)
median(avec_distance$distance)
IQR(avec_distance$distance)

aggregate(avec_distance$distance, list(avec_distance$Loc_name), sum)
aggregate(avec_distance$distance, list(avec_distance$Loc_name), median)
