    ### on recupère les cluster et on les fusionne
    
    source("Prétraitement_ETL/normalize_adressev2.R")
    
    ### clustering 
    
    # mat_dist <- st_distance(adresse_pre_cluster)
    # hc <- hclust(as.dist(mat_dist), method="complete")
    # 
    # # sur d m
    d = 1
    
    ## 2.2 Avec un buffer de d distance et un intersects
    # peut être utile de faire un filtre 
    # devrait être fonctionnalisée
    
    buffer_adresse <- function(data, d){
        buffer <-sf::st_buffer(data, d)
        parts <- sf::st_cast(sf::st_union(buffer),"POLYGON")
        clust <- unlist(sf::st_intersects(buffer, parts))
        diss <- cbind(buffer, clust) %>%
            dplyr::group_by(clust) %>%
            dplyr::summarize(comptage = dplyr::n())
    }

buffer.shp <- buffer_adresse(adresse_pre_cluster, d )
adresse_buffer.shp <- sf::st_join(adresse_pre_cluster, buffer.shp , suffix = c("", paste0("_", d)))

d = 10
buffer.shp <-  buffer_adresse(adresse_pre_cluster, d)
adresse_buffer.shp <- sf::st_join(adresse_buffer.shp, buffer.shp, suffix = c("", paste0("_", d)))

d = 100

buffer.shp <-  buffer_adresse(adresse_pre_cluster, d )
adresse_buffer.shp <- sf::st_join(adresse_buffer.shp, buffer.shp, suffix = c("", paste0("_", d)))

#st_write(adresse_buffer.shp, "data/verif/adresse_buffer.geojson")

# ici j'ai pas verifier avoir les memes resultats 
aggregat <- sf::st_read("data/verif/adresse_buffer.geojson")
aggregat_filter <- aggregat[aggregat$comptage_10 > 1,] 


# ##  2.2 Plot pour regarder l'evolution du clustering en fonction de la distance  ========================
#  Commenté car devrait aller dans analyse

# # une fonction de ce qui est fait avec le buffer
# number_cluster <- function(data = geocodage_adresse.shp, d) {
#     buffer_XX <- st_buffer(data, d)
#     dt <- data.frame(d,
#                      sum(lengths(st_intersects(data, buffer_XX)) != 1))
#     colnames(dt) <- c("d", "nb")
#     return(dt)
# }
# 
# cluster_dist <- rbind(
#     number_cluster(merge, d = 1),
#     number_cluster(merge, d = 5),
#     number_cluster(merge, d = 10),
#     number_cluster(merge, d = 25),
#     number_cluster(merge, d = 50),
#     number_cluster(merge, d = 100),
#     number_cluster(merge, d = 150),
#     number_cluster(merge, d = 200)
#     #    number_cluster(d = 500),
#     xlab = "distance (m)")
# 
# rm(d)
# #    number_cluster(d = 1000),
#     #    number_cluster(d = 10000)
#     
# )
# 
# # graphique du nombre de cluster  
# 
# plot(cluster_dist ,
#      type = "b",
#      ylab = "Nb de clusters avec plus d'une adresse",
#      xlab = "distance (m)")
# 
# rm(d)
# 

#### export et verif des cluster ==========================================================================

aggregat_filter <- aggregat_filter %>% 
    dplyr::arrange(aggregat_filter$clust_100)

aggregat_filter_matthieu <- aggregat_filter[1:round(length(aggregat_filter$ID_CARTO)/2),]
aggregat_filter_olivier <- aggregat_filter[533:length(aggregat_filter$ID_CARTO),]

sf::st_write(aggregat_filter_matthieu, "data/verif/aggregat_filter_matthieu.geojson")
sf::st_write(aggregat_filter_olivier, "data/verif/aggregat_filter_olivier.geojson")



