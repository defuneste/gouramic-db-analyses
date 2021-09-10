# Date:  novembre 2020 - remanié septembre 2021
# Auteur Olivier Leroy https://github.com/defuneste
# Objectif:  Script pour ajouter les fichiers word aux classifs associées
# Description du problème: Le Logiciel Gouramic porduit des classifications 
# en image et si l'on veut utiliser pour de l'analyse spatiale il faut leur redonner 
# un systeme de coordonnées
# liste des libraries utilisées 
# library(magrittr)
# library(stringr)
# library(dplyr)
# library(lubridate)


##.###################################################################################33
## I. Vérification de l'envoi ====
##.#################################################################################33

# I.1 Produit une liste des pngs et des fichiers world ########
# on part du principe que les fichies sont dans un repertoire "data"

list_png <- list.files("data", 
                       recursive = TRUE, 
                       pattern = "res.png$")

# /!\ dans ces deux cas je parts du principe d'avoir du png et du fichier world avec un x 
# si pour une raison particulière on a un jpeg il faut modifier l'argument pattern dans les deux 
# fonctions list.files()
# le world file est dans le repertoire parent
fichier_world <- list.files("data",  
                            recursive = TRUE, 
                            pattern = "wx$")

# I. 2 Verifie si on a la classif sans fichier words ########
# classifs sans fichier world 
setdiff( substr(list.files("data", recursive = TRUE, pattern = "0.res.png$"), 1,15) ,
         substr(list.files("data",  recursive = TRUE, pattern = "wx$"), 1,15))


# I. 3 Vérifie si on a pas la classif sur une image  
# /!\ attention se script par du principe qu'il y a pas plus de 9 adresses
# ce qui ne sera pas bon pour les cas plus generaux
# TODO amelioration est de passer par strsplit() comme pour le tableau
# Num_sujet
#     |__ Num_Adresse 
#         |__ Année
#             |__ photo aerienne(s)


# ici donne l'image 
classif_manquante <- setdiff(substr(fichier_world, 
                                    nchar("XX_XXXX/X/XXXX/") + 1, 
                                    nchar(fichier_world) -5),
                             substr(list_png, 
                                    nchar("XX_XXXX/X/XXXX/gouResult/") + 1 , 
                                    nchar(list_png) -14))

# ici à partir des images manquantes retourne leurs localisations
# sur le disque après data/

emplacement_manquant <- function(x_classif_manquante) {
                        substr(list.files("data",  
                                          recursive = TRUE, 
                                          pattern = x_classif_manquante),
                               1, 15)[1]} # je n'ai besoin de l'emplacement que pour 1 
 
emplacement_manquant(classif_manquante)

lapply(classif_manquante, emplacement_manquant)

##.###################################################################################33
## II. un tableau de synthèse des photos ====
##.#################################################################################33

# II. 1-  un tableau de synthése des photos =======================
# le but est de produire un tableau listant les classifs en prenant la dernière version à partir un envoi

# on extrait les sujet
sujet <- substr(list_png, 1,7)

# on va exraire les adresses, deux temps, (i) separation par "/" puis extraction du second
on_separe <- sapply(list_png, function(x){ strsplit(x, "/")})

# l'adresse est le second élement
adresse <- sapply(on_separe, function(x) {as.numeric(x[2])})

# un df
exemple.dat <- data.frame(sujet, adresse)

exemple.dat$path <-  paste0("data/", row.names(exemple.dat))

row.names(exemple.dat) <- c()

rm(on_separe, adresse, sujet)

# on extrait les dates des photos aeriennes
# pour le moment deux cas soit l'année unique soit ymd /!\ si on a d'autres cas
exemple.dat$date <- lubridate::parse_date_time(stringr::str_extract(list_png, 
                                                                    pattern = "(?<=__).*(?=__)"), 
                                                orders = c("ymd", "y"))

# la version est juste avant .res.png
exemple.dat$version <- as.numeric(stringr::str_extract(list_png, 
                                                       pattern = "[:digit:](?=.res.png)"))

# cas où on a plusieurs photos /!\ ici on part du principe que c'est du jp
exemple.dat$id_photo <- stringr::str_extract(list_png, pattern = "(?<=C).*(?=.jp)")

# on a pas le cas là on ne garde que la dernière version de la classif
library(magrittr)
exemple.dat <- exemple.dat %>% 
    dplyr::group_by(.dots = c("id_photo","sujet","adresse")) %>%
    dplyr::filter(version == max(version))

# II. 2- Les photos dans un seul répertoire  =======================

# où veut-on mettre les photos
ou_mettre_les_classifs <- "data/envoi"

ifelse(!dir.exists(ou_mettre_les_classifs), 
       dir.create(ou_mettre_les_classifs),
       "c'est bon!")

# un filtre pour en prendre que là où on a tout
# et plus avoir de données manquantes
exemple.dat$filtre <- grepl(paste(exemple.dat$id_photo, 
                                  collapse = "|"), 
                            fichier_world)

# le nom de la photo
# ici on enchaine deux apply 
# le premier permet dútiliser le path pour recupérer toutes les infos
# le second extrait juste le 6 élements pour avoir le nom de la classif
classifs <- sapply(sapply(exemple.dat$path[exemple.dat$filtre == TRUE], 
                          function(x){ strsplit(x, "/")})
                  , function(x) {x[6]})

# on deplace les fichiers des images classifiées
# une de la bonne longueur 

sequence <- 1:length(classifs)
file.copy(from = exemple.dat$path[exemple.dat$filtre == TRUE][sequence],
          to = paste0(ou_mettre_les_classifs, "/", classifs)[sequence])

### ici juste une verification visuelle 
list.files("data/envoi/")

# Puis on va deplacer les fichier world avec un bon nom associé
# on les copie par le nom remplacé pris dans le tableau après avoir filtrer les versions
# /!\  ici je suppose un meme nombre/ordre de fichier wolrd que de classifs
# d'ou l'importance de verif les diff dans la partie I
# le grepl corrige un peu en ne prenant que 

file.copy(from = paste0("data/",
                        fichier_world[grepl(
                                            paste(exemple.dat$id_photo, 
                                                  collapse = "|"),
                                            fichier_world)]
                        )[sequence],  
          to = paste0(ou_mettre_les_classifs, 
                      "/", 
                      stringr::str_replace(classifs, 
                                           pattern = "png$", "pgw")
                      )[sequence]
              )

      