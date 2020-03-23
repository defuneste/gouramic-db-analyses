## script du 03/03/2020
# Mise en forme et EDA "contexte" des sujets Gouramic
# convention de code .dat <- tableau

##.###################################################################################33
## I. Chargement des données et Mise en forme ====
##.#################################################################################33

# 1- Chargement des packages et données ========================

# ici j'ai pris la derniere version
# devtools::install_github("joelgombin/banR", build_vignettes = TRUE)

pkgs <-  c("dplyr","stringr", "lubridate", "ggplot2", "banR", "sf")
inst <- lapply(pkgs, library, character.only = TRUE)

# 2 - Mise en forme : ici  Prétraitement_ETL/clean_order_all_sujet.R ================

allsujet.dat <- openxlsx::read.xlsx("data/all_Sujets.xlsx")
allsujet_clean.dat <- readRDS("data/allsujet_clean.rds")
produit_geocode <- readRDS("data/produit_geocode.rds")

produit_geocode %>% 
    ggplot(aes(result_score, fill = result_type)) +
    geom_histogram(color = "white") 

summary(as.factor(produit_geocode$result_type))

##.###################################################################################33
## II. Quelques stats à garder en tête / EDA ====
##.#################################################################################33

# 1a- Quelques stats à garder en tête / EDA =============
# sans doute séparer après avoir nettoyer 

# 1155 sujet / id unique
length(unique(allsujet.dat$ID_SECONDAIRE))

# adresse 
summary(allsujet.dat$ID_VISITE)

# ici je fais un histo mais un wafle sur des categories serait plus correcte 
# pour le nombre d'adresse / sujet
allsujet.dat %>% 
    dplyr::group_by(ID_SECONDAIRE) %>% 
    dplyr::summarize(nb_adresse = max(ID_VISITE)) %>% 
    ggplot(aes(nb_adresse)) +
    geom_histogram(binwidth = 1, color = "white" )

# stats de base
allsujet.dat %>% 
    dplyr::group_by(ID_SECONDAIRE) %>% 
    dplyr::summarize(nb_adresse = max(ID_VISITE)) %>% 
    dplyr::summarize(mean_adresse = mean(nb_adresse),
                     sd_adresse = sd(nb_adresse))

# pour les dates de naissance
allsujet_clean.dat %>% 
    ggplot(aes(x = year(Date_birth))) +
    geom_histogram(binwidth = 1, color = "white" ) + 
    labs(x = "Années de naissance", y = "Nombre de sujets")


#1b- Adresses manquantes ===================

allsujet_clean.dat %>% 
    mutate(Nun_adresse = as.numeric(str_extract(allsujet_clean.dat$Id_cart, pattern = "[0-9]{1,2}?$"))) %>% 
    filter(is.na(allsujet_clean.dat$Commune)) %>% 
    ggplot(aes(x = Nun_adresse)) +
    geom_histogram(binwidth = 1, color = "white" ) +
    labs(y = "Nombre")

#verif si ce sont pas mes transformations qui ont introduit ces NA, reponse : non 
allsujet_clean.dat[is.na(allsujet_clean.dat$Commune),]
View(allsujet.dat[is.na(allsujet.dat$CP_commune_p),])

adresse_NA <-  allsujet_clean.dat %>% 
    mutate(Num_adresse = as.numeric(str_extract(allsujet_clean.dat$Id_cart, pattern = "[0-9]{1,2}?$")),
           Sujet = substr(allsujet_clean.dat$Id_cart, 1,7)) %>% 
    filter(Sujet %in% allsujet.dat$ID_SECONDAIRE[is.na(allsujet.dat$CP_commune_p)]) %>% 
    group_by(Sujet) %>% 
    summarise(Nb_na = sum(is.na(Commune)),
              Max_adresse = max(Num_adresse) )

#tableau pour avoir la place des valeurs manquantes dans le num adresse

allsujet_clean.dat %>% 
    mutate(Num_adresse = as.numeric(str_extract(allsujet_clean.dat$Id_cart, pattern = "[0-9]{1,2}?$")),
           Sujet = substr(allsujet_clean.dat$Id_cart, 1,7)) %>% 
    filter(Sujet %in% allsujet.dat$ID_SECONDAIRE[is.na(allsujet.dat$CP_commune_p)]) %>% 
    full_join(adresse_NA, by = c("Sujet", "Sujet")) %>% 
    filter(is.na(Commune)) %>% 
    select(Sujet, Num_adresse, Nb_na, Max_adresse)


allsujet.dat$ID_SECONDAIRE[is.na(allsujet.dat$CP_commune_p)]

#ici charcher affiche_un_sujet

affiche_un_sujet("01_0095")

# 2- Determiner des adresses correspondantes à des stades de vie d’intérêt ======
# la naissance doit correspondre à la premiere adresse
# pour la periode 8-9 doit on y mettre un buffer ? +/-1 une année -> 7-8-9-10
# ici je propose Enfance, c'est un interval
# idem pour celle de 12-14 idem +/- 1 une annee 11-12-13-14-15 
# ici Adolescence, c'est un interval
# au niveau adresse une adresse peut correspondre plusieurs stade de vie d'interet 
# du coup je part en format long avec un champ /stade de vie il faudra en tenir compte dans le schema 


# 2-a Naissance =========================
# extraction du dernier num d'ID carto
# attention les nb adresses peut depasser 9
produit_geocode$Nun_adresse <- as.numeric(str_extract(produit_geocode$Id_cart, pattern = "[0-9]{1,2}?$"))
# si 1 -> Naissance
produit_geocode$Naissance <- ifelse(produit_geocode$Nun_adresse == 1,  1, 0)

# 2-b Enfance =====================
produit_geocode$interval_adresse <-  interval(produit_geocode$Date_start, produit_geocode$Date_end)

produit_geocode$Enfance <- ifelse(
                                int_overlaps(produit_geocode$interval_adresse, 
                                             interval(produit_geocode$Date_birth + years(7), produit_geocode$Date_birth + years(10))) == TRUE
                                , 1, 0)

# 2-c Adolescence =====================

produit_geocode$Adolescence <- ifelse(
    int_overlaps(produit_geocode$interval_adresse, 
                 interval(produit_geocode$Date_birth + years(11), produit_geocode$Date_birth + years(15))) == TRUE
    , 1, 0)

# 2-d Analyse des adresses par stades de vie ======

produit_geocode$sujet <- substr(produit_geocode$Id_cart, 1,7)

produit_geocode %>% 
    summarize(Nb_adresse_naissance = sum(Naissance),
              Nb_adresse_Enfance = sum(Enfance, na.rm = T),
              Nb_adresse_Adolescence = sum(Adolescence, na.rm = T))

nb_stade_vie_sujet <- produit_geocode %>% 
    group_by(sujet, result_type) %>% 
    summarize(Sum_naissance = sum(Naissance), 
              Sum_enfance = sum(Enfance, na.rm = TRUE),
              Sum_adolescence = sum(Adolescence,  na.rm = TRUE))

table(nb_stade_vie_sujet$Sum_enfance, nb_stade_vie_sujet$result_type, useNA = "ifany")
table(nb_stade_vie_sujet$Sum_adolescence, nb_stade_vie_sujet$result_type, useNA = "ifany")


produit_geocode$Importance_adresse <- produit_geocode$Naissance + produit_geocode$Enfance + produit_geocode$Adolescence

# ici rajout d'un champs pour specifier le moyen du geocodage
# a ce stade c'est via banR donc qu'une seule valeur 
produit_geocode$source_loc <- "geocodage"

table(produit_geocode$Importance_adresse, produit_geocode$result_type)

# 3- Exploration de valeurs manquantes =====================================

# 3-a via les valeurs manquantes dans les date ==============
produit_geocode %>% 
    filter_at(vars(Date_start, Date_end), any_vars(is.na(.))) %>% 
    View()

# fonction rapide d'explo/verif des données
affiche_un_sujet <- function(un_Id_carto) {
    allsujet.dat %>% 
        dplyr::filter(ID_SECONDAIRE == un_Id_carto)
                    }

affiche_un_sujet("15_0269")

# pour les resultats du geocodage
affiche_une_loc <- function(un_sujet) {
                        produit_geocode %>% 
                            dplyr::filter(sujet == un_sujet) %>% 
                            View()
                            produit_geocode %>%  # c'est un peu laid 
                                dplyr::filter(sujet == un_sujet) %>% 
                                dplyr::select(Id_cart, Info_sup)
}

affiche_une_loc("02_0712")

# 3-a via les valeurs manquantes dans les loc ==============

produit_geocode %>% 
    filter(Importance_adresse >= 1) %>% 
    filter(is.na(latitude)) %>% 
    arrange(desc(Importance_adresse)) %>% 
    View()


# 4- un export pour passer en SIG et faire du geocodage à la main =======================

# #une version xls au besoin
openxlsx::write.xlsx(produit_geocode, "data/produit_geocode.xls")


produit_geocode.shp <- sf::st_as_sf(produit_geocode, coords = c("longitude", "latitude"), crs = 4326
                                    , na.fail = FALSE)

st_write(produit_geocode.shp, "data/geocode.geojson")
