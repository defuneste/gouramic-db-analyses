# Date: Fevrier 2021 / Avril 2020 pour v1
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif:assemblage d'un fichier après les différents géocodages
# Description du problème:
# 
# Libraries utilisées:
#  "dplyr",  "sf", "tidyr", "" 

# on a besoin des verif à la main 
# des pas verif à là main 
source("Prétraitement_ETL/reassemblage_geocodage_mains.R") # les verif à la main 

# on recharge le geocodage clb pour prendre ce qui na pas été codé à la main 
geocodage_clb.shp <- sf::read_sf("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

adresse_complement <- geocodage_clb.shp %>% 
    dplyr::select(ID_CARTO, Commune, CP, nb_rue_p, rue_p, compl_add_, pt_remarq_, lieudit_p ) %>% 
    tidyr::unite("Adresse", nb_rue_p, rue_p, sep = " ",  na.rm = TRUE) %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, na.rm = TRUE)

# plus besoin de geocodage_clb.shp
rm(geocodage_clb.shp)

# adresse vient de reassemblage_geocodage_main
adresse_complement_distance <- dplyr::inner_join(adresse_complement, 
                                                 adresse,
                                                 by = "ID_CARTO")

# on retire celle que l'on a verifier à la main du total avec distance 
pas_verif_a_la_main <- adresse_complement_distance[!adresse_complement_distance$ID_CARTO %in% verif_ecartv3.shp$ID_CARTO,] 
pas_verif_a_la_main[,"preci_evs"] <- NULL                                    # on vire la precision evs
pas_verif_a_la_main$preci_clb <- substr(pas_verif_a_la_main$preci_clb,
                                        1,
                                        1) # pour homogeneiser 



# Ici ce sont ceux qui n'ont pas de comparaison avec evs
de_cotecorrigé.shp <- sf::read_sf("data/verif/de_coteMatthieu.geojson")
de_cotecorrigé.shp$preci_clb <- substr(de_cotecorrigé.shp$Loc_name, 
                                       1,
                                       1)

# sans comparaison il ne peuvent pas avoir de distance
de_cotecorrigé.shp$distance  <-  "NA"

names(de_cotecorrigé.shp)

# un vecteur pour ne garder que les bon champs
a_garder <- names(pas_verif_a_la_main)

# ici on arrive à 7611 mais avec des cas proches
adresse_pre_cluster <- rbind(verif_ecartv3.shp[,a_garder], pas_verif_a_la_main, de_cotecorrigé.shp[,a_garder])

rm(a_garder, adresse_complement, adresse_complement_distance, adresse, de_cotecorrigé.shp, pas_verif_a_la_main)

###### un cluster pour les gouverner tous =========================================================
# suite à la prod de cluster on les à  verifier à la main chacun une partie cf clusterv2

aggregat_filter_matthieu <- sf::read_sf("data/verif/aggregat_filter_matthieu(1).geojson")
aggregat_filter_matthieu$source_codage <- "Main"
aggregat_filter_olivier <- sf::read_sf("data/verif/aggregat_filter_olivier.geojson")
aggregat_filter_olivier$source_codage <- "Main"

aggregat_filter <- rbind(aggregat_filter_matthieu, aggregat_filter_olivier)

rm(aggregat_filter_matthieu, aggregat_filter_olivier)

# un peu de nettoyage on garde le cluster le plus large
# on enleve le cluster si idem == 0 ie n'est pas un cluster et du coup prend la valeur de 0
cluster.shp <- aggregat_filter %>%
    dplyr::mutate(cluster = ifelse(verif == 1, 
                                   clust_100,
                                   0) ) %>%
    dplyr::filter(cluster != 0) # on retire les non clusters

#pas_cluster.shp <-  aggregat_filter %>%
#    dplyr::mutate(cluster = ifelse(verif == 1, 
#                                   clust_100, 
#                                   0) ) %>%
#    dplyr::filter(cluster == 0) # on garde les clusters

rm(aggregat_filter)

# il faut changer la loc des points formant le clusters par le point du milieu
# comment definit-on le milieu ?
# cas avec deux points et cas avec plus de deux points

centre_cluster <- cluster.shp %>%
    dplyr::filter(comptage_100 >= 2) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(count = dplyr::n_distinct(geometry) ) %>% # on produit un comptage de geometry distinct
    sf::st_drop_geometry() %>%
    dplyr::right_join(cluster.shp, by = ("cluster" = "cluster")) %>%
    ungroup() %>%
    sf::st_as_sf(sf_column_name = "geometry")


#table(centre_cluster$count)

# attention ici meme si on a plusieurs points ils peuvent :
# - se superposer donc on ne peut calculer le polygones
# - n'avoir que deux points differents : lignes
# 
# attention ne marche que pour un cas

centre_cluster$geometry[centre_cluster$count == 3] <- st_centroid(
                                st_combine(
                                    st_as_sf(
                                        centre_cluster[centre_cluster$count == 3,])) , "POLYGON")


# centroid marche pour une ligne, vu notre cas il sera sur la ligne mais utiliser une variante si ligne courbe
centre_cluster_ligne <- aggregate(
    centre_cluster$geometry[centre_cluster$count == 2],
    list(centre_cluster$clust_100[centre_cluster$count == 2]),
    function(x){sf::st_centroid(sf::st_cast(sf::st_combine(x),
                                    "LINESTRING"))}
)

# match est utilisé pour produire un vecteur d'indexation attribuant on va attribuer le point
centre_cluster$geometry[centre_cluster$count == 2] <- st_sfc(centre_cluster_ligne$geometry)[match(centre_cluster$clust_100[centre_cluster$count == 2],
                                                                                                  centre_cluster_ligne$Group.1)]
    
# st_write(centre_cluster, "data/verif/verif_cluster2.geojson")

# on prepare pour un rajout
transit <- data.frame(
                     sort(unique(centre_cluster$clust_100)),
                     1:length(unique(centre_cluster$clust_100))
            )

names(transit) <- c("clust_100", "addresse_passage")

centre_cluster <- centre_cluster %>% left_join(transit,  
                                               by = c("clust_100" = "clust_100"))

rm(transit)

# un bout de la futur table de passage
transit_passage <- centre_cluster %>%
    sf::st_drop_geometry() %>%
    dplyr::select(addresse_passage, ID_CARTO)  

# centre_cluster_clean <- centre_cluster %>%
#     group_by(adresse_passage) %>%
#     summarize(adresse_clb = first(adresse_id),
#               sujet_id = first(sujet_id),
#               precision = first(preci_clb),
#               source_codage = first(source_codage))
# 
# # il faut retirer les clusters et preparer le jeux de données
# puis les rajouter et ceux non definit comme cluster
# # c'est un peu lourd en computation pour ce que cela fait ...
# # il y a l'ajout puis la mise en forme

table_adresse.shp <- adresse_pre_cluster[!adresse_pre_cluster$ID_CARTO %in% centre_cluster$ID_CARTO,] 

a_garder <- names(table_adresse.shp)

table_adresse.shp <- rbind(table_adresse.shp, 
                           centre_cluster[,a_garder])

geocodage_clb.shp <- sf::read_sf("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

geocodage_clb.shp$date_start <- lubridate::parse_date_time(geocodage_clb.shp$date_start, orders = c("my", "dmy"))
geocodage_clb.shp$date_end_a <- lubridate::parse_date_time(geocodage_clb.shp$date_end_a, orders = c("my", "dmy"))

geocodage_clb.shp <- geocodage_clb.shp %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(ID_CARTO, 
                  date_start, 
                  date_end_a )

table_adresse.shp <- dplyr::left_join(table_adresse.shp, 
                                      geocodage_clb.shp, 
                                      by = c("ID_CARTO" = "ID_CARTO")) 

rm(a_garder, centre_cluster, centre_cluster_ligne, cluster.shp, geocodage_clb.shp, preci_diff, transit_passage)
#sf::st_write(table_adresse.shp, "data/envoi/clean_adressev1.geojson")


