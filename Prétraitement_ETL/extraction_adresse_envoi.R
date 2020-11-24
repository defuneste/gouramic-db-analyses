# novenbre 2020
# extraction des adresses d'un envoi

# le repertoire ou l'aro de fichier est 
ou_sont_les_images <- "data"

list_png <- list.files("ou_sont_les_images" , recursive = TRUE, pattern = "res.png$")

# un veteur de liste de sujet
sujet <- unique(substr(list_png, 1,7))

# on charge la BD
source("Prétraitement_ETL/exploration_db.R")

# filtre
envoi_adresse.shp <-  adresse_sujet_temporal.shp[adresse_sujet_temporal.shp$sujet_id %in% sujet, ]

# export en xls, pe changer si besoin des coords
# openxlsx::write.xlsx(st_drop_geometry(envoi_adresse.shp), "data/envoi.xls")