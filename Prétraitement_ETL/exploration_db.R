#### test de connection de le base via R + test dm


source("code.R") # ici j'ai mis codes/pwd/port/adresse c'est en .gitignore

pkgs <-  c("DBI","RPostgreSQL", "stringr",  "microbenchmark", "sf", "purrr", "lubridate", "dplyr", "tidyr")
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

## 1- serie de jointures ================================== 

sujet.dat <- st_read(con, query="select * from gou.t_sujet;")

adresse.shp <- st_read(con, query="select * from gou.t_adresse;")

p_t_adresse_interval.dat <- st_read(con, query="select * from gou.p_t_adresse_interval;")

interval_date <- st_read(con, query="select * from gou.t_interval_date;")

adresse_sujet.shp <- dplyr::left_join(adresse.shp, sujet.dat, by = c("sujet_id"))

temporal.dat <- dplyr::left_join(p_t_adresse_interval.dat, interval_date, by = c("interval_id"))
    
adresse_sujet_temporal.shp <- dplyr::left_join(adresse_sujet.shp, temporal.dat, by = c("adresse_id")) %>% 
    dplyr::select(-interval_id)

rm(sujet.dat, adresse.shp, p_t_adresse_interval.dat, adresse_sujet.shp, temporal.dat, interval_date)
  
# on peut se deconnecter
dbDisconnect(con)

