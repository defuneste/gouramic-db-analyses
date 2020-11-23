# novenbre 2020
# script pour ajouter les fichiers word aux classifs associées 

##.###################################################################################33
## I. Vérification de l'envoi ====
##.#################################################################################33

# liste des pngs et des fichiers world
list_png <- list.files("data", recursive = TRUE, pattern = "res.png$")
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


# ##.###################################################################################33
# ## II. un tableau de synthése des photos ====
# ##.#################################################################################33
# 
# # 1-  un tableau de synthése des photos =======================
# 
# # on extrait les sujet
# sujet <- substr(list_png, 1,7)
# fichier_world <- list.files("data",  recursive = TRUE, pattern = "wx$")
# list_png <- list.files("data", recursive = TRUE, pattern = "res.png$")
# 
# # on va exraire les adresses, deux temps, (i) separation par "/" puis extraction du second
# on_separe <- sapply(list_png, function(x){ strsplit(x, "/")})
# adresse <- sapply(on_separe, function(x) {as.numeric(x[2])})
# 
# # un df 
# exemple.dat <- data.frame(sujet, adresse)
# exemple.dat$path <-  paste0("data/", row.names(exemple.dat))
# row.names(exemple.dat) <- c()
# 
# rm(on_separe, adresse, sujet)
# 
# # on extrait les dates des photos aeriennes 
# # pour le moment deux cas soit l'année unique soit ymd
# # attention si on a d'autres cas
# exemple.dat$date <- parse_date_time(str_extract(list_png, pattern = "(?<=__).*(?=__)"), orders = c("ymd", "y"))
# 
# # la version est juste avant .res.png
# exemple.dat$version <- as.numeric(str_extract(list_png, pattern = "[:digit:](?=.res.png)"))
# 
# # cas ou on a plusieurs photos
# exemple.dat$id_photo <- str_extract(list_png, pattern = "(?<=C).*(?=.jp)")
# 
# # on a pas le cas là on ne garde que la dernière version de la classif
# exemple.dat <- exemple.dat %>% 
#     group_by(.dots = c("id_photo","sujet","adresse")) %>% 
#     filter(version == max(version))
# 
# exemple.dat$path
# 
# # on les copie par le nom remplace pris dans le tableau après avoir filtrer les version
# file.copy(from = paste0("data/",fichier_world),  to = str_replace(exemple.dat$path, pattern = "png$", "j2wx"))
# 
# # file.remove(list_a_copier)
# file.remove(list.files("data",  recursive = TRUE, pattern = "zzzzzzzzz$", full.names = TRUE))
# 
# # ici on ne va garder que la dernière bonne version
# exemple.dat <- exemple.dat %>% 
#     dplyr::group_by(sujet, adresse, date, id_photo) %>% 
#     dplyr::summarize(version = max(version)) %>% 
#     ungroup()
# 
# str(exemple.dat)