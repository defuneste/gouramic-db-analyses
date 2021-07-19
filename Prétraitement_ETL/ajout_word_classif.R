# novenbre 2020
# script pour ajouter les fichiers word aux classifs associées 
# library(stringr)
# library(dplyr)

##.###################################################################################33
## I. Vérification de l'envoi ====
##.#################################################################################33

# liste des pngs et des fichiers world
list_png <- list.files("data", recursive = TRUE, pattern = "res.png$")
# /!\ dans ces deux cas je parts du principe d'avoir du png et du fichier world avec un x 
fichier_world <- list.files("data",  recursive = TRUE, pattern = "wx$")

# classifs sans fichier world 
setdiff( substr(list.files("data", recursive = TRUE, pattern = "0.res.png$"), 1,15) ,
         substr(list.files("data",  recursive = TRUE, pattern = "wx$"), 1,15))

# /!\ attention se script par du principe que il y a pas plus de 9 adresses
# ce qui ne sera pas bon pour les cas plus generaux
# amelioration est de passé par strsplit() comme pour le tableau
classif_manquante <- setdiff(substr(fichier_world, nchar("XX_XXXX/X/XXXX/") + 1, nchar(fichier_world) -5),
                             substr(list_png, nchar("XX_XXXX/X/XXXX/gouResult/") + 1 , nchar(list_png) -14))

emplacement_manquant <- function(x_classif_manquante) {substr(list.files("data",  recursive = TRUE, pattern = x_classif_manquante), 1, 15)}
lapply(classif_manquante, emplacement_manquant)

##.###################################################################################33
## II. un tableau de synthése des photos ====
##.#################################################################################33

# 1-  un tableau de synthése des photos =======================
# le but est de produire un tableau listant les classifs en prenant la dernière version à partir un envoi

# on extrait les sujet
sujet <- substr(list_png, 1,7)

# on va exraire les adresses, deux temps, (i) separation par "/" puis extraction du second
on_separe <- sapply(list_png, function(x){ strsplit(x, "/")})
adresse <- sapply(on_separe, function(x) {as.numeric(x[2])})

# un df
exemple.dat <- data.frame(sujet, adresse)
exemple.dat$path <-  paste0("data/", row.names(exemple.dat))
row.names(exemple.dat) <- c()

rm(on_separe, adresse, sujet)

# on extrait les dates des photos aeriennes
# pour le moment deux cas soit l'année unique soit ymd /!\ si on a d'autres cas
exemple.dat$date <- parse_date_time(stringr::str_extract(list_png, pattern = "(?<=__).*(?=__)"), orders = c("ymd", "y"))

# la version est juste avant .res.png
exemple.dat$version <- as.numeric(stringr::str_extract(list_png, pattern = "[:digit:](?=.res.png)"))

# cas ou on a plusieurs photos /!\ ici on part du principe que c'est du jp
exemple.dat$id_photo <- stringr::str_extract(list_png, pattern = "(?<=C).*(?=.jp)")

# on a pas le cas là on ne garde que la dernière version de la classif
exemple.dat <- exemple.dat %>%
    dplyr::group_by(.dots = c("id_photo","sujet","adresse")) %>%
    dplyr::filter(version == max(version))

# 2-  les photos dans un seul répertoire  =======================

ou_mettre_les_classifs <- "data/envoi"

dir.create(ou_mettre_les_classifs)

# un filtre pour en prendre que là ou on a tous
exemple.dat$filtre <- grepl(paste(exemple.dat$id_photo, collapse = "|"), fichier_world)

# le nom de la photo
classifs <- sapply(sapply(exemple.dat$path[exemple.dat$filtre == TRUE], function(x){ strsplit(x, "/")})
                  , function(x) {x[6]})
# on deplace 
# une sequence 
sequence <- 1:162
file.copy(from = exemple.dat$path[exemple.dat$filtre == TRUE][sequence],
          to = paste0(ou_mettre_les_classifs, "/", classifs)[sequence])

list.files("data/envoi/")

# on les copie par le nom remplace pris dans le tableau après avoir filtrer les versions
# /!\  ici je suppose un meme nombre/ordre de fichier wolrd que de classifs
# d'ou l'importance de verif les diff dans la partie I
# le grepl corrige un peu en ne prenant que 

    file.copy(from = paste0("data/",fichier_world[grepl(paste(exemple.dat$id_photo, collapse = "|"), fichier_world)])[sequence],  
              to = paste0(ou_mettre_les_classifs, "/", stringr::str_replace(classifs, pattern = "png$", "pgw"))[sequence]
              )

      