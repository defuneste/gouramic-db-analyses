# assemblage d'un fichier après les différents géocodage

# on a besoin des verif à la main 
# des pas verif à là main 

source("Prétraitement_ETL/reassemblage_geocodage_mains.R") # les verif à la main 

geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

adresse_complement <- geocodage_clb.shp %>% 
    dplyr::select(ID_CARTO, Commune, CP, nb_rue_p, rue_p, compl_add_, pt_remarq_, lieudit_p ) %>% 
    tidyr::unite("Adresse", nb_rue_p, rue_p, sep = " ",  na.rm = TRUE) %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, na.rm = TRUE)


adresse_complement_distance <- dplyr::inner_join(adresse_complement, adresse_filtre, by = "ID_CARTO")

pas_verif_a_la_main <- adresse_complement_distance[!adresse_complement_distance$ID_CARTO %in% verif_ecartv3.shp$ID_CARTO,] 
pas_verif_a_la_main[,"preci_evs"] <- NULL
pas_verif_a_la_main$preci_clb <- substr(pas_verif_a_la_main$preci_clb, 1, 1)

de_cotecorrigé.shp <- st_read("data/verif/de_coteMatthieu.geojson")
de_cotecorrigé.shp$preci_clb <- substr(de_cotecorrigé.shp$Loc_name, 1, 1)
de_cotecorrigé.shp$distance  <-  "NA"

names(de_cotecorrigé.shp)

a_garder <- names(pas_verif_a_la_main)

merge <- rbind(verif_ecartv3.shp[,a_garder], pas_verif_a_la_main, de_cotecorrigé.shp[,a_garder])

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

st_write(buffer_100_plus, "data/verif/adresse_buffer.geojson")
st_write(buffer_100, "data/verif/buffer_100.geojson")



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


