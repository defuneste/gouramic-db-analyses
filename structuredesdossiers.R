### premier script explo gouramic

pkgs <-  c("dplyr","stringr", "rgdal", "raster", "lubridate", "tidyr", "ggplot2")
inst <- lapply(pkgs, library, character.only = TRUE)

# les données

list.files("data", recursive = TRUE, pattern = "res.png$")

list.files("data", recursive = TRUE, pattern = "0.res.png$")

list.files("data", recursive = TRUE, pattern = "1.res.png$")

list.files("data", recursive = TRUE, pattern = "2.res.png$")

list.files("data", recursive = TRUE, pattern = "3.res.png$")

head(list.files("data", recursive = TRUE, pattern = "res.png$"), 10)


# liste des png
# on fait une liste des fichiers qui ont besoin d'un world file,
# le world file doit avoir le meme nom mais terminer par w
list_png <- list.files("data", recursive = TRUE, pattern = "res.png$", full.names = TRUE)
str_replace(list_png, pattern = "png$", "pgw")

# une liste/vector des fichiers à copier
list_a_copier <- list.files("data", pattern = "w$", full.names = TRUE)

# on les copies par le nom remplacer 
file.copy(list_a_copier,  str_replace(list_png, pattern = "png$", "pgw"))


# 1-  un tableau de synthése des photos =======================

# liste des png
list_png <- list.files("data", recursive = TRUE, pattern = "res.png$")

# on extrait les sujet
sujet <- substr(list_png, 1,7)

# on va exraire les adresses, deux temps, (i) separation par "/" puis extraction du second
on_separe <- sapply(list_png, function(x){ strsplit(x, "/")})
adresse <- sapply(on_separe, function(x) {as.numeric(x[2])})

# un df 
exemple.dat <- data.frame(sujet, adresse)
rm(on_separe, adresse, sujet)

# on extrait les dates des photos aeriennes 
# pour le moment deux cas soit l'année unique soit ymd
# attention si on a d'autres cas
exemple.dat$date <- parse_date_time(str_extract(list_png, pattern = "(?<=__).*(?=__)"), orders = c("ymd", "y"))

# la version est juste avant .res.png
exemple.dat$version <- as.numeric(str_extract(list_png, pattern = "[:digit:](?=.res.png)"))

# cas ou on a plusieurs photos
exemple.dat$id_photo <- str_extract(list_png, pattern = "(?<=C).*(?=.jp)")

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

# si on veut passer en tidyr et on vire le pas necessaire pour le moment
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