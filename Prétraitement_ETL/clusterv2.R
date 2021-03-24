### on recupère les cluster et on les fusionne


### clustering 

mat_dist <- st_distance(merge)
hc <- hclust(as.dist(mat_dist), method="complete")

#merge$id_cluster_100 <- cutree(hc, h=100)

# sur d m
d=1

## 2.2 Avec un buffer de d distance et un intersects
# peut être utile de faire un filtre 

buffer_1 <-st_buffer(merge, d)
parts_1 <- st_cast(st_union(buffer_1),"POLYGON")
clust_1 <- unlist(st_intersects(buffer_1, parts_1))
diss <- cbind(buffer_1, clust_1) %>%
    group_by(clust_1) %>%
    summarize(#id_cluster_100 = paste(id_cluster_100, collapse = ", "),
        comptage_1 = n())

buffer_100_plus <- st_join(merge, diss)


buffer_10 <- st_buffer(merge, 10) # buffer de 100 m et pas bufer_50 mauvais nom
parts_10 <- st_cast(st_union(buffer_10),"POLYGON")
clust_10 <- unlist(st_intersects(buffer_10, parts_10))
diss <- cbind(buffer_10, clust_10) %>%
    group_by(clust_10) %>%
    summarize(#id_cluster_100 = paste(id_cluster_100, collapse = ", "),
        comptage_10 = n())

buffer_100 <- st_buffer(merge, 100)
parts_100 <- st_cast(st_union(buffer_100),"POLYGON")
clust_100 <- unlist(st_intersects(buffer_100, parts_100))
diss <- cbind(buffer_100, clust_100) %>%
    group_by(clust_100) %>%
    summarize(#id_cluster_100 = paste(id_cluster_100, collapse = ", "),
        comptage_100 = n())

buffer_100_plus <- st_join(buffer_100_plus, diss)

#st_write(buffer_100_plus, "data/verif/adresse_buffer.geojson")
#st_write(buffer_100, "data/verif/buffer_100.geojson")

#aggregat <- sf::st_read("data/verif/adresse_buffer.geojson")
#aggregat_filter <- aggregat[aggregat$comptage_10 > 1,] 


##  2.2 Plot pour regarder l'evolution du clustering en fonction de la distance  ========================

# une fonction de ce qui est fait avec le buffer
number_cluster <- function(data = geocodage_adresse.shp, d) {
    buffer_XX <- st_buffer(data, d)
    dt <- data.frame(d,
                     sum(lengths(st_intersects(data, buffer_XX)) != 1))
    colnames(dt) <- c("d", "nb")
    return(dt)
}

cluster_dist <- rbind(
    number_cluster(merge, d = 1),
    number_cluster(merge, d = 5),
    number_cluster(merge, d = 10),
    number_cluster(merge, d = 25),
    number_cluster(merge, d = 50),
    number_cluster(merge, d = 100),
    number_cluster(merge, d = 150),
    number_cluster(merge, d = 200)
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




#### export et verif des cluster ==========================================================================

library(sf)
library(dplyr)

aggregat_filter <- aggregat_filter %>% 
    dplyr::arrange(aggregat_filter$clust_100)

aggregat_filter_matthieu <- aggregat_filter[1:round(length(aggregat_filter$ID_CARTO)/2),]
aggregat_filter_olivier <- aggregat_filter[533:length(aggregat_filter$ID_CARTO),]

sf::st_write(aggregat_filter_matthieu, "data/verif/aggregat_filter_matthieu.geojson")
sf::st_write(aggregat_filter_olivier, "data/verif/aggregat_filter_olivier.geojson")

table(aggregat$comptage_100, aggregat$comptage_10)

###### un cluster pour les gouverner tous =========================================================

aggregat_filter_matthieu <- st_read("data/verif/aggregat_filter_matthieu.geojson")
aggregat_filter_olivier <- st_read("data/verif/aggregat_filter_olivier.geojson")

aggregat_filter <- rbind(aggregat_filter_matthieu, aggregat_filter_olivier)

# un peu de nettoyage on garde le cluster le plus large
# on eneleve le cluster si idem == 0 ie n'est pas un cluster et du coup prend la valeur de 0
cluster.shp <- aggregat_filter %>%
    mutate(cluster = ifelse(verif == 1, clust_100, 0) ) %>%
    filter(cluster != 0) # on retire les non cluster

# il faut changer la loc des points formant le clusters par le point au milieu
# comment definit-on le milieu ?
# cas avec deux points et cas avec plus de deux points

# centre_cluster <- cluster.shp %>%
#     filter(comptage_100 >= 2) %>%
#     group_by(cluster) %>%
#     distinct(count = n_distinct(geometry) ) %>% # on produit un comptage de geometry distinct
#     
#     
#     st_drop_geometry() %>%
#     right_join(cluster.shp, by = "cluster") %>%
#     ungroup() %>%
#     st_as_sf(sf_column_name = "geometry")

# # attention ici meme si on a plusieurs points ils peuvent :
# # - se superposer donc on ne peut calculer le polygones
# # - n'avoir que deux points differents : lignes
# 
# # attention ne marche que pour un cas
# centre_cluster$geometry[centre_cluster$count == 3] <- st_centroid(st_combine(st_as_sf(centre_cluster[centre_cluster$count == 3,])) , "POLYGON")
# # 
# # centroid marche pour une ligne, vu notre cas il sera sur la ligne mais utiliser une variante si ligne courbe
# centre_cluster_ligne <- aggregate(
#     centre_cluster$geometry[centre_cluster$count == 2],
#     list(centre_cluster$bigcluster[centre_cluster$count == 2]),
#     function(x){st_centroid(st_cast(st_combine(x),"LINESTRING"))} 
# )
# 
# # match est utilise pour produire un vecteur d'indexation attribuant on va attribuer le point
# centre_cluster$geometry[centre_cluster$count == 2] <- st_sfc(centre_cluster_ligne$geometry)[match(centre_cluster$bigcluster[centre_cluster$count == 2],  centre_cluster_ligne$Group.1)]
# 
# # on prepare pour un rajout
# transit <- data.frame(
#     sort(unique(centre_cluster$bigcluster)),
#     1:length(unique(centre_cluster$bigcluster))
# )
# names(transit) <- c("bigcluster", "addresse_passage")
# 
# centre_cluster <- centre_cluster %>% left_join(transit,  by = c("bigcluster" = "bigcluster"))
# centre_cluster <-rename(centre_cluster, adresse_clb = adresse_id)
# 
# # un bout de la futur table de passage
# transit_passage <- centre_cluster %>% 
#     st_drop_geometry() %>% 
#     dplyr::select(addresse_passage, adresse_clb)  #%>%
# #dplyr::mutate(adresse_id = 1:length(addresse_passage))
# 
# names(transit_passage) <- c("adresse_id", "adresse_passage")
# 
# length(unique(geocodage_adresse.shp$adresse_id))
# 
# centre_cluster_clean <- centre_cluster %>% 
#     group_by(addresse_id) %>% 
#     summarize(adresse_clb = first(adresse_id),
#               sujet_id = first(sujet_id),
#               precision = first(precision),
#               source_codage = first(source_codage)) 
# 
# # il faut retirer les clusters et preparer le jeux de données
# # c'est un peu lourd en computation pour ce que cela fait ...
# # il y a l'ajout puis la mise en forme
# table_adresse.shp <- geocodage_adresse.shp[!geocodage_adresse.shp$adresse_id %in% centre_cluster$adresse_id,] %>% 
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

