#### test de connection de le base via R + test dm


source("code.R") # ici j'ai mis codes/pwd/port/adresse c'est en .gitignore

pkgs <-  c("DBI","RPostgreSQL", "dm",  "microbenchmark", "sf")
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

rm(pwd, db, adresse, port)

sujet.dat <- st_read(con, query="select * from gou.t_sujet;")

adresse.shp <- st_read(con, query="select * from gou.t_adresse;")

p_t_adresse_interval.dat <- st_read(con, query="select * from gou.p_t_adresse_interval;")

interval_date <- st_read(con, query="select * from gou.t_interval_date;")

adresse_sujet.shp <- dplyr::left_join(adresse.shp, sujet.dat, by = c("sujet_id"))

temporal.dat <- dplyr::left_join(p_t_adresse_interval.dat, interval_date, by = c("interval_id"))
    
adresse_sujet_temporal.shp <- dplyr::left_join(adresse_sujet.shp, temporal.dat, by = c("adresse_id")) %>% 
                                dplyr::select(-interval_id)
# on peut se deconnecter
dbDisconnect(con)




#################### test 
list_table <- c("t_sujet", "t_adresse", "p_t_adresse_interval", "t_interval_date")

my_dm <- dm_from_src(con, list_table = c("gou.t_sujet"), schema = "gou", learn_keys = F)
my_dm
