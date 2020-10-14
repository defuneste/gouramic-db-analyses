#### test de connection de le base via R + test dm


source("code.R") # ici j'ai mis codes/pwd/port/adresse c'est en .gitignore

pkgs <-  c("DBI","RPostgreSQL", "dm",  "microbenchmark", "sf", "purrr", "lubridate", "dplyr", "tidyr")
inst <- lapply(pkgs, library, character.only = TRUE)

sfc_as_cols <- function(x, names = c("x","y")) {
    stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT")) ## un stop si ps un objet sf avec des points
    ret <- sf::st_coordinates(x)  # coordinates retourne une marice X / Y
    ret <- tibble::as_tibble(ret) # passage en tibble
    stopifnot(length(names) == ncol(ret)) # stop si on essaie de mettre autre chose que deux noms
    x <- x[ , !names(x) %in% names]  # ici c'est un peu brute car on va supprimer des colonnes qui aurait les noms donnés
    ret <- setNames(ret,names) # on renome
    dplyr::bind_cols(x,ret) # on bind sur les cols
}


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

## 1- serie de jointures ================================== 

sujet.dat <- st_read(con, query="select * from gou.t_sujet;")

adresse.shp <- st_read(con, query="select * from gou.t_adresse;")

p_t_adresse_interval.dat <- st_read(con, query="select * from gou.p_t_adresse_interval;")

interval_date <- st_read(con, query="select * from gou.t_interval_date;")

adresse_sujet.shp <- dplyr::left_join(adresse.shp, sujet.dat, by = c("sujet_id"))

temporal.dat <- dplyr::left_join(p_t_adresse_interval.dat, interval_date, by = c("interval_id"))
    
adresse_sujet_temporal.shp <- dplyr::left_join(adresse_sujet.shp, temporal.dat, by = c("adresse_id")) %>% 
                                dplyr::select(-interval_id) %>% 
                                dplyr::filter(precision <= 4)

rm(sujet.dat, adresse.shp, p_t_adresse_interval.dat, adresse_sujet.shp, temporal.dat)

## 2- on rajoute une ligne par année ===========================

# On filtre les NA 

temp <- adresse_sujet_temporal.shp[!is.na(adresse_sujet_temporal.shp$date_start),]

temp  <- temp   %>% 
    select(adresse_id,adresse_clb, sujet_id,
           date_debut = date_start,
           date_fin = date_end) %>% 
    mutate(interval = interval(date_debut,date_fin)) %>% 
    st_transform(4326) %>% 
    sfc_as_cols() %>% 
    st_transform(2154) %>% 
    st_buffer(2000) %>% 
    st_transform(4326) %>% 
    dplyr::select(adresse_id,adresse_clb, sujet_id, date_debut, date_fin, interval,  x , y, geometry) %>% 
    arrange(adresse_id, date_debut)

temp$date_debut <- year(temp$date_debut)
temp$date_fin <- year(temp$date_fin)

temp_dupli <-  temp %>% 
    st_drop_geometry() %>% 
    nest(date_debut, date_fin) %>% 
    mutate(data = map(data, ~seq(unique(.x$date_debut), unique(.x$date_fin), 1))) %>% 
    unnest(data) %>% 
    st_as_sf(coords = c("x", "y")) %>% 
    select(-interval) %>%  
    st_set_crs(st_crs("EPSG:4326")) %>% 
    sfc_as_cols() %>% 
    st_transform(2154) %>% 
    st_buffer(1500) %>% 
    st_transform(4326) %>% 
    dplyr::select(adresse_id, adresse_clb, sujet_id, date_y = data, x, y, geometry)


dir.create("data/csv")

outlist <- list() # initialisation d'un liste
longueur <- length(unique(temp_dupli$sujet_id)) # le nombre de fichier souhaité

for(i in 1:longueur) { 
    # on passe par une liste, c'est pas indispensable mais je voulais verifier un peu avant d'ecrire des fichier
    outlist[[i]] <- temp_dupli %>%
        filter(sujet_id == unique(temp_dupli$sujet_id)[i]) 
    # on écrit des tas de fichiers 
    st_write(outlist[[i]], dsn = paste0("data/csv/", unique(temp_dupli$sujet_id)[i], ".csv"),
                sep = ";",
                quote = FALSE,
                row.names = FALSE,
                col.names=TRUE)
}

# on peut se deconnecter
dbDisconnect(con)

#################### test 
list_table <- c("t_sujet", "t_adresse", "p_t_adresse_interval", "t_interval_date")

my_dm <- dm_from_src(con, list_table = c("gou.t_sujet"), schema = "gou", learn_keys = F)
my_dm
