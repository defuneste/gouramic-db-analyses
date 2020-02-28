### premier script explo gouramic

pkgs <-  c("stringr", "rgdal", "raster", "lubridate")
inst <- lapply(pkgs, library, character.only = TRUE)

# les données

list.files("data", recursive = TRUE, pattern = "res.png$")

list.files("data", recursive = TRUE, pattern = "0.res.png$")

list.files("data", recursive = TRUE, pattern = "1.res.png$")

list.files("data", recursive = TRUE, pattern = "2.res.png$")

list.files("data", recursive = TRUE, pattern = "3.res.png$")

head(list.files("data", recursive = TRUE, pattern = "res.png$"), 10)

# un tableau de synthése

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

# pb dans ces raster
test <- raster(paste0("data/","03_0031/1/1976/gouResult/IGNF_PVA_1-0__1976-06-08__C3620-0051_1976_FR2796_0061.jp2.0.res.png"))

res(test)

plot(test)