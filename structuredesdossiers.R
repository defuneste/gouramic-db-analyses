### premier script explo gouramic

pkgs <-  c("dplyr","stringr", "rgdal", "raster", "lubridate", "tidyr", "ggplot2")
inst <- lapply(pkgs, library, character.only = TRUE)
# attention on a pas mal de nom de fonction pbatique penser à les appeler directement

# les données
# il peut y avoir plusieur plusieurs photos aerienne pour un cas
# le second il y a eu plusieurs interpretation et donc c'est le dernier resultat qui est bon 
# dans le second cas on doit avoir des noms de photos differents

list.files("data", recursive = TRUE, pattern = "res.png$")

list.files("data", recursive = TRUE, pattern = "0.res.png$")

list.files("data", recursive = TRUE, pattern = "1.res.png$")

list.files("data", recursive = TRUE, pattern = "2.res.png$")

list.files("data", recursive = TRUE, pattern = "3.res.png$")

list.files("data", recursive = TRUE, pattern = "4.res.png$")

head(list.files("data", recursive = TRUE, pattern = "res.png$"), 10)


# liste des png
# on fait une liste des fichiers qui ont besoin d'un world file,
# le world file doit avoir le meme nom mais terminer par w
list_png <- list.files("data", recursive = TRUE, pattern = "res.png$")


# une liste/vector des fichiers à copier
fichier_world <- list.files("data",  recursive = TRUE, pattern = "wx$")

#list.dirs("data", recursive = T)[!str_detect(list.dirs("data", recursive = T), pattern = "gouResult")]

# classifs sans fichier world 
setdiff( substr(list.files("data", recursive = TRUE, pattern = "0.res.png$"), 1,15) ,
         substr(list.files("data",  recursive = TRUE, pattern = "wx$"), 1,15))

# image aerienne sans classifs 
classif_manquante <- setdiff(substr(fichier_world, nchar("XX_XXXX/X/XXXX/") + 1, nchar(fichier_world) -5),
substr(list_png, nchar("XX_XXXX/X/XXXX/gouResult/") + 1 , nchar(list_png) -14))


emplacement_manquant <- function(x_classif_manquante) {substr(list.files("data",  recursive = TRUE, pattern = x_classif_manquante), 1, 15)}
lapply(classif_manquante, emplacement_manquant)

# 1-  un tableau de synthése des photos =======================

# on extrait les sujet
sujet <- substr(list_png, 1,7)
fichier_world <- list.files("data",  recursive = TRUE, pattern = "wx$")
list_png <- list.files("data", recursive = TRUE, pattern = "res.png$")

# on va exraire les adresses, deux temps, (i) separation par "/" puis extraction du second
on_separe <- sapply(list_png, function(x){ strsplit(x, "/")})
adresse <- sapply(on_separe, function(x) {as.numeric(x[2])})

# un df 
exemple.dat <- data.frame(sujet, adresse)
exemple.dat$path <-  paste0("data/", row.names(exemple.dat))
row.names(exemple.dat) <- c()

rm(on_separe, adresse, sujet)

# on extrait les dates des photos aeriennes 
# pour le moment deux cas soit l'année unique soit ymd
# attention si on a d'autres cas
exemple.dat$date <- parse_date_time(str_extract(list_png, pattern = "(?<=__).*(?=__)"), orders = c("ymd", "y"))

# la version est juste avant .res.png
exemple.dat$version <- as.numeric(str_extract(list_png, pattern = "[:digit:](?=.res.png)"))

# cas ou on a plusieurs photos
exemple.dat$id_photo <- str_extract(list_png, pattern = "(?<=C).*(?=.jp)")

# on a pas le cas là on ne garde que la dernière version de la classif
exemple.dat <- exemple.dat %>% 
        group_by(.dots = c("id_photo","sujet","adresse")) %>% 
        filter(version == max(version))

exemple.dat$path

# on les copie par le nom remplacer pris dans le tableau après avoir filtrer les version
file.copy(from = paste0("data/",fichier_world),  to = str_replace(exemple.dat$path, pattern = "png$", "j2wx"))

# file.remove(list_a_copier)
file.remove(list.files("data",  recursive = TRUE, pattern = "zzzzzzzzz$", full.names = TRUE))

# ici on ne va garder que la dernière bonne version
exemple.dat <- exemple.dat %>% 
    dplyr::group_by(sujet, adresse, date, id_photo) %>% 
    dplyr::summarize(version = max(version)) %>% 
    ungroup()

str(exemple.dat)

# 2-  lecture des dates debut et fin par sujet ==================

sujet.dat <- read.csv("data/Liste.csv", sep = "\t")

# si on veut tidy le jeux de données il faut recoder Date_Debut et Date_Fin 

# reformatage du temps
sujet.dat$Date_Debut <- parse_date_time(sujet.dat$Date_Debut, orders = c("my", "dmy"))
sujet.dat$Date_Fin <- parse_date_time(sujet.dat$Date_Fin, orders = c("my", "dmy"))
names(sujet.dat)[1] <- "sujet"
names(sujet.dat)[2] <- "adresse"
sujet.dat$adresse <- as.factor(sujet.dat$adresse) # pe plus caractere

str(sujet.dat)

# si on veut passer en tidyr et on change date. Pas necessaire pour le moment
sujet.dat <- tidyr::gather(sujet.dat, "Date_Debut", "Date_Fin", key = "type_date", value = "date") %>%
                    dplyr::select(sujet, adresse, type_date, date)

    
# 3 - un graphique

ggplot(sujet.dat, aes(y = sujet, x = date, color = adresse))+
    geom_line(lwd = 1.5) +
    scale_color_brewer(palette = "Set1") +
    geom_point(data = exemple.dat, aes(y = sujet, x = date), inherit.aes = FALSE,  alpha = 0.9, pch = 3) +
    theme_bw()



# pb dans ces raster
test <- raster(list_png[1])

res(test)

plot(test)