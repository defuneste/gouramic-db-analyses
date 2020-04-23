### geocodage precision
# 20/04 

pkgs <-  c("dplyr", "tidyr", "ggplot2", "sf", "lubridate")
inst <- lapply(pkgs, library, character.only = TRUE)

## lecture des fichiers

dist.dat <- read.csv("data/DistClean.csv", stringsAsFactors = FALSE) 
geocodage_clbv2.shp <- sf::st_read("data/sortie_15_04.shp")

geocodage_clbv2.shp$date_start <- parse_date_time(geocodage_clbv2.shp$date_start, orders = c("my", "dmy"))
geocodage_clbv2.shp$date_end_a <- parse_date_time(geocodage_clbv2.shp$date_end_a, orders = c("my", "dmy"))


geocodage_evs.shp <- sf::st_read("data/geocodev2.geojson", stringsAsFactors = FALSE)

## 1- Stats descriptives rapides =========================
# un rapide boxplot
# completement écraser par 4 valeurs 
boxplot(dist.dat$Dist_m)
    
names(dist.dat)

sum(table(dist.dat$Preci_CLB[dist.dat$Dist_m <= 5], dist.dat$PreciBan[dist.dat$Dist_m <= 5]))

table(dist.dat$Preci_CLB[dist.dat$Dist_m > 5], dist.dat$PreciBan[dist.dat$Dist_m > 5])

sum(table(dist.dat$Preci_CLB[dist.dat$Dist_m > 5], dist.dat$PreciBan[dist.dat$Dist_m > 5]))

## 2 Export pour Remi ================================
# l'idée est de prendre ceux dont la precision entre les deux geocoder est proche 
# avec une precision de localisation et d'en garder l'identifiant
# on utilisera cette identifiant filtrer un tableau contenant lat/long en wgs84 + date en année

filtre_geocode <- dist.dat[dist.dat$PreciBan == dist.dat$Preci_CLB,]
# je ne suis pour prendre que voie et point_adresse pour le moment 
filtre_geocode <- filtre_geocode[!filtre_geocode$PreciBan == "4_LieuDitHabit",]

filtre_geocode_proche <- filtre_geocode[filtre_geocode$Dist_m <= 5,]

# il y a st coordinates mais j'aime bien sfc_as_cols
# pris ici : https://github.com/r-spatial/sf/issues/231

sfc_as_cols <- function(x, names = c("x","y")) {
    stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT")) ## un stop si ps un objet sf avec des points
    ret <- sf::st_coordinates(x)  # coordinates retourne une marice X / Y
    ret <- tibble::as_tibble(ret) # passage en tibble
    stopifnot(length(names) == ncol(ret)) # stop si on essaie de mettre autre chose que deux noms
    x <- x[ , !names(x) %in% names]  # ici c'est un peu brute car on va supprimer des colonnes qui aurait les noms donnés
    ret <- setNames(ret,names) # on renome
    dplyr::bind_cols(x,ret) # on bind sur les cols
}


geocodage_clbv2_clean.shp  <- geocodage_clbv2.shp   %>% 
    select(ID_CARTO,
           date_debut = date_start,
           date_fin = date_end_a) %>% 
    filter(ID_CARTO %in% filtre_geocode_proche$ID_CARTO) %>% 
    mutate(interval = interval(date_debut,date_fin)) %>% 
    st_transform(4326) %>% 
    sfc_as_cols() %>% 
    st_transform(2154) %>% 
    st_buffer(2000) %>% 
    st_transform(4326) %>% 
    select(ID_CARTO, date_debut, date_fin, interval,  x , y, geometry) %>% 
    arrange(ID_CARTO, date_debut)


write.table(geocodage_clbv2_clean.shp , 
            "data/clean_adresse.csv", 
            sep = ";",
            quote = FALSE,
            row.names = FALSE,
            col.names=TRUE) 


write.table(geocodage_clbv2_clean.shp , 
            "data/clean_adresse.csv", 
            sep = ";",
            quote = FALSE,
            row.names = FALSE,
            col.names=TRUE) 

## export pour Matthieu et Olivier avec les adresses à identifier 
# objectif ici est d'avoir les adresses à verifier 


