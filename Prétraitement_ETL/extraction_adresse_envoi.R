# Date: Novembre 2020
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: Permet de sortir un xlsx ou autre des adresses en fonction des images 
# dans un dossier 
# Description du problème:
# Dans certains cas j'avais un envoi avec des photos suivant cette structure :
# Num_sujet
#     |__ Num_Adresse 
#         |__ Année
#             |__ photo aerienne(s)
# Et je voulais savoir ce qu'on avait dans les données d'enquetes dessus
# Il y a un export à en xlsx à la fin 
# 
# libraries utilisées:
# "DBI","RPostgreSQL", "sf",  "dplyr" et openxlsx

# extraction des adresses d'un envoi

# indiquer le repertoire de l'envoi
ou_sont_les_images <- "data"

list_png <- list.files(ou_sont_les_images 
                       , recursive = TRUE
                       , pattern = "res.png$")

# un vecteur des sujets
sujet <- unique(substr(list_png, 1,7))

# on charge la BD
source("Prétraitement_ETL/exploration_db.R")

# filtre
envoi_adresse.shp <-  adresse_sujet_temporal.shp[adresse_sujet_temporal.shp$sujet_id %in% sujet, ]

# export en xls, pe changer si besoin des coords
openxlsx::write.xlsx(sf::st_drop_geometry(envoi_adresse.shp), 
                     "data/envoi.xls") # où on mets !