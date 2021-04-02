# assemblage d'un fichier après les différents géocodage

# on a besoin des verif à la main 
# des pas verif à là main 
source("Prétraitement_ETL/reassemblage_geocodage_mains.R") # les verif à la main 

# on recharge le geocodage clb pour prendre ce qui na pas été codé à la main 
geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

adresse_complement <- geocodage_clb.shp %>% 
    dplyr::select(ID_CARTO, Commune, CP, nb_rue_p, rue_p, compl_add_, pt_remarq_, lieudit_p ) %>% 
    tidyr::unite("Adresse", nb_rue_p, rue_p, sep = " ",  na.rm = TRUE) %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, na.rm = TRUE)

# plus besoin de geocodage_clb.shp
rm(geocodage_clb.shp)

# adresse filtre vient de reassemblage_geocodage_main
adresse_complement_distance <- dplyr::inner_join(adresse_complement, adresse_filtre, by = "ID_CARTO")

# on retire celle que l'on a verifier à la main du total avec distance 
pas_verif_a_la_main <- adresse_complement_distance[!adresse_complement_distance$ID_CARTO %in% verif_ecartv3.shp$ID_CARTO,] 
pas_verif_a_la_main[,"preci_evs"] <- NULL                                    # on vire la precision evs
pas_verif_a_la_main$preci_clb <- substr(pas_verif_a_la_main$preci_clb, 1, 1) # pour homogeneiser 

# Ici ce sont ceux qui n'ont pas de comparaison avec evs
de_cotecorrigé.shp <- sf::st_read("data/verif/de_coteMatthieu.geojson")
de_cotecorrigé.shp$preci_clb <- substr(de_cotecorrigé.shp$Loc_name, 1, 1)
de_cotecorrigé.shp$distance  <-  "NA"

names(de_cotecorrigé.shp)

# un vecteur pour ne garder que les bon champs
a_garder <- names(pas_verif_a_la_main)

adresse_pre_cluster <- rbind(verif_ecartv3.shp[,a_garder], pas_verif_a_la_main, de_cotecorrigé.shp[,a_garder])

rm(a_garder, adresse_complement, adresse_complement_distance, adresse_filtre, de_cotecorrigé.shp, pas_verif_a_la_main)

###### un cluster pour les gouverner tous =========================================================

aggregat_filter_matthieu <- sf::st_read("data/verif/aggregat_filter_matthieu(1).geojson")
aggregat_filter_matthieu$source_codage <- "Main"
aggregat_filter_olivier <- sf::st_read("data/verif/aggregat_filter_olivier.geojson")
aggregat_filter_olivier$source_codage <- "Main"

aggregat_filter <- rbind(aggregat_filter_matthieu, aggregat_filter_olivier)

rm(aggregat_filter_matthieu, aggregat_filter_olivier)

# un peu de nettoyage on garde le cluster le plus large
# on eneleve le cluster si idem == 0 ie n'est pas un cluster et du coup prend la valeur de 0
cluster.shp <- aggregat_filter %>%
    dplyr::mutate(cluster = ifelse(verif == 1, clust_100, 0) ) %>%
    dplyr::filter(cluster != 0) # on retire les non clusters

pas_cluster.shp <-  aggregat_filter %>%
    dplyr::mutate(cluster = ifelse(verif == 1, clust_100, 0) ) %>%
    dplyr::filter(cluster == 0) # on garde les clusters

rm(aggregat_filter)

# il faut changer la loc des points formant le clusters par le point au milieu
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
    function(x){st_centroid(st_cast(st_combine(x),"LINESTRING"))}
)

# match est utilise pour produire un vecteur d'indexation attribuant on va attribuer le point
centre_cluster$geometry[centre_cluster$count == 2] <- st_sfc(centre_cluster_ligne$geometry)[match(centre_cluster$clust_100[centre_cluster$count == 2],  centre_cluster_ligne$Group.1)]
    
# st_write(centre_cluster, "data/verif/verif_cluster2.geojson")

# on prepare pour un rajout
transit <- data.frame(
    sort(unique(centre_cluster$clust_100)),
    1:length(unique(centre_cluster$clust_100))
)
names(transit) <- c("clust_100", "addresse_passage")

centre_cluster <- centre_cluster %>% left_join(transit,  by = c("clust_100" = "clust_100"))

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

bob <- centre_cluster[,a_garder]
table_adresse.shp <- rbind(table_adresse.shp, bob)

geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

geocodage_clbv2.shp$date_start <- parse_date_time(geocodage_clbv2.shp$date_start, orders = c("my", "dmy"))
geocodage_clbv2.shp$date_end_a <- parse_date_time(geocodage_clbv2.shp$date_end_a, orders = c("my", "dmy"))

geocodage_clbv2.shp <- geocodage_clbv2.shp %>% 
    dplyr::select(ID_CARTO, Loc_name, Commune, CP, nb_rue_p, rue_p, compl_add_, pt_remarq_, lieudit_p ) %>% 
    tidyr::unite("Adresse", nb_rue_p, rue_p, sep = " ",  na.rm = TRUE) %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, na.rm = TRUE)

# %>%
#     select(-c(date_start, date_end, commune, adresse, cp, info_sup,  nb_cluster, nb_bigcluster)) %>%
#     bind_rows(centre_cluster_clean) %>%
#     group_by(adresse_id) %>% # c'est pas ultra propre
#     summarize(sujet_id = first(sujet_id),
#               precision = first(precision),
#               source_codage = first(source_codage)) %>%
#     dplyr::mutate(adresse_clb = adresse_id) %>%
#     dplyr::mutate(adresse_id = 1:length(adresse_id))  %>%
#     dplyr::select(adresse_id, sujet_id, adresse_clb, precision, source_codage)
# 
# # il y a des id de sujet avec  des fautes de frappes à corriger
# # oui j'ai verifier 08_006X
# table_adresse.shp$sujet_id[table_adresse.shp$sujet_id == "08_006_"] <- "08_0006"
# geocodage_adresse.shp$sujet_id[geocodage_adresse.shp$sujet_id == "08_006_"] <- "08_0006"

