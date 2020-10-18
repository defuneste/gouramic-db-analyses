### octobre 2020
# extraction pour le script de REmi
# ici l'objectif c'est de produire une ligne par année

source("Prétraitement_ETL/exploration_db.R")

# une fonction qui rajoute les coord x et y 
sfc_as_cols <- function(x, names = c("x","y")) {
    stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT")) ## un stop si ps un objet sf avec des points
    ret <- sf::st_coordinates(x)  # coordinates retourne une marice X / Y
    ret <- tibble::as_tibble(ret) # passage en tibble
    stopifnot(length(names) == ncol(ret)) # stop si on essaie de mettre autre chose que deux noms
    x <- x[ , !names(x) %in% names]  # ici c'est un peu brute car on va supprimer des colonnes qui aurait les noms donnés
    ret <- setNames(ret,names) # on renome
    dplyr::bind_cols(x,ret) # on bind sur les cols
}


# on ne garde que ceux avec la precision egale ou inf au lieu dit 
adresse_sujet_temporal.shp <- adresse_sujet_temporal.shp[adresse_sujet_temporal.shp$precision <= 4,]

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


dir.create("data/csv2")

outlist <- list() # initialisation d'un liste
longueur <- length(unique(temp_dupli$sujet_id)) # le nombre de fichier souhaité

for(i in 1:longueur) { 
    # on passe par une liste, c'est pas indispensable mais je voulais verifier un peu avant d'ecrire des fichier
    outlist[[i]] <- temp_dupli %>%
        filter(sujet_id == unique(temp_dupli$sujet_id)[i]) 
    # on écrit des tas de fichiers 
    st_write(outlist[[i]], dsn = paste0("data/csv2/", unique(temp_dupli$sujet_id)[i], ".csv"),
             sep = ";",
             quote = FALSE,
             row.names = FALSE,
             col.names=TRUE,
             layer_options = "GEOMETRY=AS_WKT")
}
