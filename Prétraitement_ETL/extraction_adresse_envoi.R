# novenbre 2020
# extraction des adresses d'un envoi

ou_sont_les_images <- "data"

list_png <- list.files("ou_sont_les_images" , recursive = TRUE, pattern = "res.png$")

sujet <- unique(substr(list_png, 1,7))

source("Prétraitement_ETL/exploration_db.R")


envoi_adresse.shp <-  adresse_sujet_temporal.shp[adresse_sujet_temporal.shp$sujet_id %in% sujet, ]