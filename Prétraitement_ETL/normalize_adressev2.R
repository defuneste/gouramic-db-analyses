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


# sur d m
d=1

merge$id_cluster_1 <- cutree(hc, h=d)
merge$id_cluster_10 <- cutree(hc, h=10)
merge$id_cluster_100 <- cutree(hc, h=100)
merge$id_cluster_150 <- cutree(hc, h=150)

# sur une plus grande distance : 50 m cf plot plus bas

## 2.2 Avec un buffer de d distance et un intersects
# peut être utile de faire un filtre 

buffer_1 <-st_buffer(merge, d)
buffer_10 <- st_buffer(merge, 10) # buffer de 100 m et pas bufer_50 mauvais nom
buffer_100 <- st_buffer(merge, 100)
buffer_150 <- st_buffer(merge, 150)

merge$cluster_1 <-lengths(st_intersects(merge, buffer_1))
merge$cluster_10 <-lengths(st_intersects(merge, buffer_10))
merge$cluster_100 <-lengths(st_intersects(merge, buffer_100))
merge$cluster_150 <-lengths(st_intersects(merge, buffer_150))


st_write(merge[merge$cluster_1 > 1,], "data/clusteringv2.geojson")

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


