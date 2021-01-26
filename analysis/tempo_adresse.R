### un truc rapide pour extraire et refusionner la tempo

library(sf)

# le géocodage esri

geocodage_clb.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

#correction formatage
geocodage_clb.shp$date_start <- lubridate::parse_date_time(geocodage_clb.shp$date_start, orders = c("my", "dmy"))
geocodage_clb.shp$date_end_a <- lubridate::parse_date_time(geocodage_clb.shp$date_end_a, orders = c("my", "dmy"))

geocodage_clb.shp <- geocodage_clb.shp %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, sep = " ", na.rm = TRUE) %>% 
    dplyr::select(ID_CARTO, date_start, date_end_a, preci_clb = Loc_name)

### attention on peut avoir des Id_carto differents
# je vais passer en année cr je pebse que c'est un grain suffisant 

geocodage_clb.shp$date_start <- lubridate::year(geocodage_clb.shp$date_start)
geocodage_clb.shp$date_end_a <- lubridate::year(geocodage_clb.shp$date_end_a)

# un arrondi pour ne recup que des année : arrondi en dessous 1984.5 => 1984
geocodage_clb.shp$annee <- round((geocodage_clb.shp$date_start + geocodage_clb.shp$date_end_a)/2, 0)

# choix de na pas droppre la geometrie 
geocode_annee.shp <-  geocodage_clb.shp[,c("ID_CARTO", "annee")]

rm(geocodage_clb.shp)

hist(geocodage_clb.shp$annee)