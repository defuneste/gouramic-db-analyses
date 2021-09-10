# Date: Juillet 2020
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: se connecter à la DB et reformer un jeux de données adresses
# 
# libraries utilisées:
# "DBI","RPostgreSQL", "sf",  "dplyr"

source("code.R") # ici j'ai mis codes/pwd/port/adresse c'est en .gitignore

pkgs <-  c("DBI","RPostgreSQL", "sf",  "dplyr")
inst <- lapply(pkgs, library, character.only = TRUE)


drv <- dbDriver("PostgreSQL")
usr <- "postgres"

con <- dbConnect(
    drv,
    user = usr,
    password = pwd,
    dbname = db,
    host = adresse,
    port = port
)

dbListTables(con)

rm(pwd, db, adresse, port, usr)

## 1- série de jointures ================================== 

sujet.dat <- sf::read_sf(con, query="select * from gou.t_sujet;")

adresse.shp <- sf::read_sf(con, query="select * from gou.t_adresse;")

p_t_adresse_interval.dat <- sf::read_sf(con, query="select * from gou.p_t_adresse_interval;")

interval_date <- sf::read_sf(con, query="select * from gou.t_interval_date;")

adresse_sujet.shp <- dplyr::left_join(adresse.shp, sujet.dat, by = c("sujet_id"))

temporal.dat <- dplyr::left_join(p_t_adresse_interval.dat, interval_date, by = c("interval_id"))
    
adresse_sujet_temporal.shp <- dplyr::left_join(adresse_sujet.shp, temporal.dat, by = c("adresse_id")) %>% 
    dplyr::select(-interval_id)

# en fonction du besoin pe garder des table intermediaires
rm(sujet.dat, adresse.shp, p_t_adresse_interval.dat, adresse_sujet.shp, temporal.dat, interval_date)
  
# on peut se deconnecter
dbDisconnect(con)

