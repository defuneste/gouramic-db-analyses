# Date: Octobre 2020
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: Produire un csv une ligne par année pour le script de Remi qui DL les photos 
# Description du problème: 
# Le script de Remi prend un csv, il faut une ligne par année 
# ie pas une date fin debut mais ex: 1981 autre ligne 1982 autre ligne 1983 etc
# libraries utilisées:
# DBI","RPostgreSQL", "sf",  "dplyr", "lubridate", "purrr"


source("Prétraitement_ETL/exploration_db.R")

# une fonction qui rajoute les coord x et y 
# source : https://github.com/r-spatial/sf/issues/231#issuecomment-290817623

sfc_as_cols <- function(x, names = c("x","y")) {
    
    stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT")) ## un stop si pas un objet sf avec des points
    
    ret <- sf::st_coordinates(x)  # coordinates retourne une matrice X / Y
    ret <- tibble::as_tibble(ret) # passage en tibble
    
    stopifnot(length(names) == ncol(ret)) # stop si on essaie de mettre autre chose que deux noms
    
    x <- x[ , !names(x) %in% names]  # ici c'est un peu brute car on va supprimer des colonnes qui aurait les noms donnés
    
    ret <- setNames(ret,names) # on renome
    dplyr::bind_cols(x,ret) # on bind sur les cols
    
}

# Choix de la taille du Buffer en m (va être passer dans du lambert93 )

BUFFER <- 2000


# on ne garde que ceux avec la precision égale ou inf au lieu dit et on filtre les NA
adresse_sujet_temporal.shp <- adresse_sujet_temporal.shp[adresse_sujet_temporal.shp$precision <= 4,]
temp <- adresse_sujet_temporal.shp[!is.na(adresse_sujet_temporal.shp$date_start),]

# Etape 1: un tableau avec un buffer et avec les info necessaires ===================== 

temp  <- temp   %>% 
    dplyr::select(adresse_id,adresse_clb, sujet_id,
           date_debut = date_start,
           date_fin = date_end) %>% 
    dplyr::mutate(interval = lubridate::interval(date_debut,date_fin)) %>% 
    sf::st_transform(4326) %>% 
    sfc_as_cols() %>% 
    sf::st_transform(2154) %>% # oui c'est un peu lourd
    sf::st_buffer(BUFFER) %>%   
    sf::st_transform(4326) %>%  # je crois que le script de Remi prenait du WGS84
    dplyr::select(adresse_id, 
                  adresse_clb, 
                  sujet_id,
                  date_debut,
                  date_fin,
                  interval,
                  x,
                  y,
                  geometry) %>% 
    dplyr::arrange(adresse_id, date_debut)

temp$date_debut <- lubridate::year(temp$date_debut)
temp$date_fin <- lubridate::year(temp$date_fin)

# Etape 2 on duplicate pour X nombre d'années ===================================
# c'est un peu lourd

temp_dupli <-  temp %>% 
    sf::st_drop_geometry() %>% 
    tidyr::nest(date_debut, date_fin) %>% 
    mutate(date = purrr::map(data, 
                             ~seq(unique(.x$date_debut), 
                                  unique(.x$date_fin), 
                                  1)
                             )
           ) %>% 
    tidyr::unnest(date) %>% 
    sf::st_as_sf(coords = c("x", "y")) %>% 
    dplyr::select(-interval) %>%  
    sf::st_set_crs(st_crs("EPSG:4326")) %>% 
    sfc_as_cols() %>% 
    sf::st_transform(2154) %>% # comprends pas pkoi je refais un buffer 
    sf::st_buffer(BUFFER) %>% 
    sf::st_transform(4326) %>% 
    dplyr::select(adresse_id, 
                  adresse_clb, 
                  sujet_id, 
                  date_y = date,
                  x, 
                  y, 
                  geometry)


out_dir <- ("data/csv2")
# si le dossier n'existe pas il est crée
ifelse(!dir.exists(out_dir), 
       dir.create(out_dir, 
                  recursive = TRUE),
       "c'est bon!")

## Etape 3 Un CSV par sujet ===================================================================
# Comme c'etait lourd on avait decider de faire un csv par sujet 

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
