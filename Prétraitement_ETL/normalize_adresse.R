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

geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

#correction formatage
geocodage_clb.shp$date_start <- parse_date_time(geocodage_clb.shp$date_start, orders = c("my", "dmy"))
geocodage_clb.shp$date_end_a <- parse_date_time(geocodage_clb.shp$date_end_a, orders = c("my", "dmy"))

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
# table_adresse_test <- geocodage_evs.shp %>% 
#                            dplyr::mutate(sujet_id = substr(geocodage_evs.shp$Id_cart, 1,7),
#                                     adresse_clb = as.numeric(str_extract(geocodage_evs.shp$Id_cart, pattern = "[0-9]{1,2}?$"))) %>% 
#                             dplyr::select(sujet_id, adresse_clb, result_type, source_loc, geometry) %>% 
#                             dplyr::distinct(.keep_all = TRUE) %>% 
#                             dplyr::mutate(adresse_id = row_number()) %>% 
#                             # me faut reorder et mettre dans le bon CRS 
#                             dplyr::select(adresse_id, sujet_id, adresse_clb, result_type,source_loc, geometry) %>% 
#                             sf::st_transform(2154) %>% 
#                             # ici on filtre les sujet sans residences
#                             dplyr::filter(sujet_id %in% sujet.dat$sujet)

# length(unique(table_adresse_test$sujet_id))

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


## 1. lecture des deux fichiers geocoder à la main + celui que j'avais fait avant ===============

### 1.1 lecture geocodage mains fait en premier cf. precision_geocodage.R ========================
# j'ai touts reverifier en juillet

geocodage_evs_oli.shp <- sf::st_read("data/geocode_mains_Na.geojson") %>% 
    dplyr::select(adresse_id = Id_cart,
                  date_start = Date_start,
                  date_end = Date_end,
                  commune = Commune,
                  adresse = Adresse,
                  cp = Code_postal,
                  info_sup = Info_sup,
                  precision = result_type) %>% 
    dplyr::mutate(sujet_id = substr(adresse_id, 1,7),
                  source_codage = "Main") %>% 
    sf::st_transform(2154)

geocodage_evs_oli.shp$precision <- as.numeric(geocodage_evs_oli.shp$precision) 

summary(geocodage_evs_oli.shp)

# on exporte et on va verifier à la main

### 1.2 lecture geocodge olivier + matthieu ==========================

RM2.shp <- sf::st_read("data/REgeocodage/RM2_OL.shp") %>% 
                dplyr::select(adresse_id = Id_cart,
                              date_start = Date_start,
                              date_end = Date_end,
                              commune = Commune,
                              adresse = Adresse,
                              cp = Postal,
                              info_sup = Info_sup,
                              precision = New_Loc) %>% 
                dplyr:: mutate(sujet_id = substr(adresse_id, 1,7),
                    source_codage = "Main")

RM2.shp$precision <- as.numeric(RM2.shp$precision)

summary(RM2.shp)

geocodage_clb_oli.shp <- sf::st_read("data/REgeocodage/geocodage_clb_oli.shp" ) %>% 
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
                            dplyr::select(-New_LocNam)

geocodage_clb_oli.shp$precision <- as.numeric(geocodage_clb_oli.shp$precision)

# un pb avec une adresse qui doit rester inconnue
geocodage_clb_oli.shp$geometry[geocodage_clb_oli.shp$adresse_id == "20_0755_2"] <- c(NaN, NaN)
geocodage_clb_oli.shp$precision[geocodage_clb_oli.shp$adresse_id == "20_0755_2"] <- NA

summary(geocodage_clb_oli.shp) 

### 1.3 on rajoute on_ne_fera_pas mieux. ===========================

on_ne_fera_pas_mieux.shp <- sf::st_read("data/on_fera_pas_mieux.csv")

on_ne_fera_pas_mieux_add.shp <- geocodage_clb.shp[geocodage_clb.shp$ID_CARTO %in% on_ne_fera_pas_mieux.shp$Id_cart,] %>% 
                        tidyr::unite("info_sup", lieudit_p, compl_add_, pt_remarq_, na.rm = TRUE) %>% 
                        dplyr::select(adresse_id = ID_CARTO,
                                      date_start = date_start,
                                      date_end = date_end_a,
                                      commune = Commune,
                                      adresse = Adresse,
                                      cp = CP,
                                      info_sup,
                                      Loc_name) %>% 
                        dplyr::mutate(sujet_id = substr(adresse_id, 1,7),
                                      precision = substr(Loc_name, 1, 1), 
                                      source_codage = "ESRI") %>% 
                        dplyr::select(-Loc_name)


on_ne_fera_pas_mieux_add.shp$precision <- as.numeric(on_ne_fera_pas_mieux_add.shp$precision)

summary(on_ne_fera_pas_mieux_add.shp) 
                     

rm(on_ne_fera_pas_mieux.shp)

### 1.4 on regroupe le tout 

## un vecteur avec le bon ordre de columns 

vec_ordre <- c("adresse_id", "date_start", "date_end", "commune", "cp", "info_sup","sujet_id", "precision", "source_codage", "geometry")

geocodage_evs_oli.shp <- geocodage_evs_oli.shp[vec_ordre]
on_ne_fera_pas_mieux_add.shp <- on_ne_fera_pas_mieux_add.shp[vec_ordre]
RM2.shp <- RM2.shp[vec_ordre]
geocodage_clb_oli.shp <- geocodage_clb_oli.shp[vec_ordre]

geocode_main_totale.shp <- bind_rows(list(geocodage_evs_oli.shp, on_ne_fera_pas_mieux_add.shp, RM2.shp, geocodage_clb_oli.shp))

rm(geocodage_evs_oli.shp, on_ne_fera_pas_mieux_add.shp, RM2.shp, geocodage_clb_oli.shp, vec_ordre)

## 2. hors du geocodage main ===================

summary(geocodage_clb.shp)

geocodage_clb_tot.shp <- geocodage_clb.shp %>% 
                            tidyr::unite("info_sup", lieudit_p, compl_add_, pt_remarq_, na.rm = TRUE) %>%
                            dplyr::select(adresse_id = ID_CARTO,
                                date_start = date_start,
                                date_end = date_end_a,
                                commune = Commune,
                                adresse = Adresse,
                                cp = CP,
                                info_sup,
                                Loc_name) %>% 
                            dplyr::mutate(sujet_id = substr(adresse_id, 1,7),
                                        precision = substr(Loc_name, 1, 1), 
                                        source_codage = "ESRI") %>% 
                            dplyr::select(-Loc_name)

geocodage_clb_tot.shp$precision <- as.numeric(geocodage_clb_tot.shp$precision)

rm(geocodage_clb.shp)

## moins ce qui a été fait à la main 

geocodage_clb_auto.shp <- geocodage_clb_tot.shp[!geocodage_clb_tot.shp$adresse_id %in% geocode_main_totale.shp$adresse_id,] 

rm(geocodage_clb_tot.shp)

# si on enleve les cas avec une seule adresse manquante et ceux qui n'ont pas de date départ
# il y a des #value etrange avec des mauvais geocodages
# sf::st_write(test[test$commune == "#VALUE!",], dsn = "data/value.geojson")

arrondissement.shp <-  sf::st_read("data/value.geojson")

arrondissement.shp$precision <- as.numeric(arrondissement.shp$precision)

geocodage_clb_auto.shp <- geocodage_clb_auto.shp[geocodage_clb_auto.shp$sujet_id %in% sujet.dat$sujet,] %>% 
        dplyr::filter(!is.na(date_start)) 

summary(geocodage_clb_auto.shp)

# on doit donc rajouter ces 23 à la mains
geocodage_clb_auto.shp <- geocodage_clb_auto.shp[!geocodage_clb_auto.shp $adresse_id %in% arrondissement.shp$adresse_id,] %>% 
                        bind_rows(arrondissement.shp)

## 3. Tous ensemble ========

geocodage_adresse.shp <- bind_rows(geocodage_clb_auto.shp, geocode_main_totale.shp)

rm(geocodage_clb_auto.shp, geocode_main_totale.shp, arrondissement.shp, geocodage_evs.shp)

##.###################################################################################33
## II. Normalisation des adresses ====
##.#################################################################################33

## 1. Correction de certains points ========
## pour le calcul du distance on va enlever les territoires d'outre mer et des NA etranges
# il y a des NA à corriger dans les dates .....
geocodage_adresse.shp <- subset(geocodage_adresse.shp, !is.na(geocodage_adresse.shp$precision) & precision < 100)
summary(geocodage_adresse.shp)

## 2. Cluster des adresses  =========================================
# on est en lambert 93 donc la distance est en m 

## 2.1 Avec une matrice de distance/cluster =======
# avantage permet de savoir qu'elles sont les adresses dans le meme cluster
mat_dist <- st_distance(geocodage_adresse.shp)
hc <- hclust(as.dist(mat_dist), method="complete")

# sur d m
d=1

geocodage_adresse.shp$cluster <- cutree(hc, h=d)
# sur une plus grande distance : 50 m cf plot plus bas
geocodage_adresse.shp$bigcluster <- cutree(hc, h=100)
## 2.2 Avec un buffer de d distance et un intersects
# peut être utile de faire un filtre 

buffer_10 <-st_buffer(geocodage_adresse.shp, d)
buffer_50 <- st_buffer(geocodage_adresse.shp, 100) # buffer de 100 m et pas bufer_50 mauvais nom

geocodage_adresse.shp$nb_cluster <-lengths(st_intersects(geocodage_adresse.shp, buffer_10))
geocodage_adresse.shp$nb_bigcluster <-lengths(st_intersects(geocodage_adresse.shp, buffer_50))

rm(hc, buffer_10, buffer_50, mat_dist)

##  2.2 Plot pour regarder l'evolution du clustering en fonction de la distance  ========================

# une fonction de ce qui est fait avec le buffer
number_cluster <- function(data = geocodage_adresse.shp, d) {
                            buffer_XX <- st_buffer(geocodage_adresse.shp, d)
                            dt <- data.frame(d,
                                       sum(lengths(st_intersects(geocodage_adresse.shp, buffer_XX)) != 1))
                            colnames(dt) <- c("d", "nb")
                            return(dt)
}

cluster_dist <- rbind(
    number_cluster(d = 1),
    number_cluster(d = 5),
    number_cluster(d = 10),
    number_cluster(d = 25),
    number_cluster(d = 50),
    number_cluster(d = 100),
    number_cluster(d = 150),
    number_cluster(d = 200)
#    number_cluster(d = 500),
#    number_cluster(d = 1000),
#    number_cluster(d = 10000)
    
)

# graphique du nombre de cluster  

plot(cluster_dist ,
    type = "b",
    ylab = "Nb de clusters avec plus d'une adresse",
    xlab = "distance (m)")

rm(d)

# il y a clusters dont les adresses sont proches au m près on peut les considérer comme des doublons quasi sûr 
# cela correspond soit à des adresses identiques précises soit à des adresses peu precise, ie meme ville
# on peut donc regarder celle qui se regroupe à 50 m près moins celle au m près pour avoir une liste de "probable"

# View(geocodage_adresse.shp[geocodage_adresse.shp$nb_bigcluster > 1 & geocodage_adresse.shp$nb_cluster > 1,] %>% 
#                        dplyr::arrange(bigcluster))

# on a un peu de tout : des adresses différentes mais proches, comme des numeros de rues, 
# des adresses proches avec des niveaux de précision différents exemple,  un numero de rue (1) + la rue en question
# des rues proches mais pas identiques 
# on arrive à regrouper en partie le cas des écoles, lycées, casernes en partie geocodés à la main mais c'est pas tip top
# je pense rajouter "info_sup" dans geocodage adresse au moins pour verifier, puis pe le retirer
# j'ai exporté sur Qgis avec 1 m et 100 m puis j'ai selectionner les clusters bon et pas bon (à la main) 
# le résultats est cluster16_08.geojson

cluster.shp <- st_read("data/cluster16_08.geojson")
# str(cluster.shp)

# un peu de nettoyage on garde le cluster le plus large 
# on eneleve le cluster si idem == 0 ie n'est pas un cluster et du coup prend la valeur de 0
cluster.shp <- cluster.shp  %>% 
    mutate(bigcluster = ifelse(idem == 1, bigcluster, 0) ) %>% 
    select(-c("cluster", "nb_cluster", "idem")) %>% 
    filter(bigcluster != 0) # on retire les non cluster

# hist(cluster.shp$nb_bigcluster)

# il faut changer la loc des points formant le clusters par le point au milieu 
# comment definit-on le milieu ? 
# cas avec deux points et cas avec plus de deux points 

centre_cluster <- cluster.shp %>% 
    filter(nb_bigcluster >= 2) %>% 
    group_by(bigcluster) %>% 
    distinct(count = n_distinct(geometry) ) %>% # on produit un comptage de geometry distinct 
    st_drop_geometry() %>% 
    right_join(cluster.shp, by = "bigcluster") %>% 
    ungroup() %>% 
    st_as_sf(sf_column_name = "geometry")

 
# attention ici meme si on a plusieurs points ils peuvent :
# - se superposer donc on ne peut calculer le polygones
# - n'avoir que deux points differents : lignes

# attention ne marche que pour un cas
centre_cluster$geometry[centre_cluster$count == 3] <- st_centroid(st_combine(st_as_sf(centre_cluster[centre_cluster$count == 3,])) , "POLYGON")
 
# centroid marche pour une ligne, vu notre cas il sera sur la ligne mais utiliser une variante si ligne courbe
centre_cluster_ligne <- aggregate(
    centre_cluster$geometry[centre_cluster$count == 2],
        list(centre_cluster$bigcluster[centre_cluster$count == 2]),
        function(x){st_centroid(st_cast(st_combine(x),"LINESTRING"))} 
        )

# match est utilise pour produire un vectuer d'indexation attribuant on va attribuer le poin
centre_cluster$geometry[centre_cluster$count == 2] <- st_sfc(centre_cluster_ligne$geometry)[match(centre_cluster$bigcluster[centre_cluster$count == 2],  centre_cluster_ligne$Group.1)]

# on prepare pour un rajout
centre_cluster_clean <- centre_cluster %>% 
        group_by(bigcluster) %>% 
        summarize(adresse_id = first(adresse_id),
                  sujet_id = first(sujet_id),
                  precision = first(precision),
                  source_codage = first(source_codage)) %>% 
        select(-bigcluster)

# il faut retirer les clusters et preparer le jeux de données
table_adresse.shp <- geocodage_adresse.shp[!geocodage_adresse.shp$adresse_id %in% centre_cluster$adresse_id,] %>% 
    select(-c(date_start, date_end, commune, adresse, cp, info_sup, cluster, bigcluster, nb_cluster, nb_bigcluster)) %>% 
    bind_rows(centre_cluster_clean) %>% 
    group_by(adresse_id) %>% # c'est pas ultra propre
    summarize(sujet_id = first(sujet_id),
              precision = first(precision),
              source_codage = first(precision))
    
    
