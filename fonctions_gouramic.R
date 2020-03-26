## script du 26/03/2020
# fonctions pratiques pour l'analyses du geocodage et d'allsujet
#  convention de code :
# .dat <- tableau
# .shp <- sf 


# fonction rapide d'explo/verif des données
# a besoin de allsujet.dat et prend un "un_Id_carto"
affiche_un_sujet <- function(un_Id_carto) {
    allsujet.dat %>% 
        dplyr::filter(ID_SECONDAIRE == un_Id_carto) %>% 
        View()
}



# pour les resultats du geocodage
affiche_une_loc <- function(un_sujet) {
    produit_geocode %>% 
        dplyr::filter(sujet == un_sujet) %>% 
        View()
    produit_geocode %>%  # c'est un peu laid 
        dplyr::filter(sujet == un_sujet) %>% 
        dplyr::select(Id_cart, Info_sup)
}


