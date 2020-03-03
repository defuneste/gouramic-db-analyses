## script du 03/03/2020
# Mie en forne et EDA "contexte" des sujets Gouramic
# convention de code .dat <- tableau

# 1- chargement des packages et données ========================

pkgs <-  c("dplyr","stringr", "lubridate", "ggplot2")
inst <- lapply(pkgs, library, character.only = TRUE)

allsujet.dat <- openxlsx::read.xlsx("data/all_Sujets.xlsx")


# 2- Mise en forme des données ================

str(allsujet.dat)

# a priori on a plusieurs formes d'encoding
# testé latin1 sans succés 
# virer les "�" semble marcher 
allsujet.dat$compl_add_p <- str_remove_all(allsujet.dat$compl_add_p, pattern = "�")
allsujet.dat$pt_remarq_p <- str_remove_all(allsujet.dat$pt_remarq_p, pattern = "�")
allsujet.dat$CP_commune_p <- str_remove_all(allsujet.dat$CP_commune_p, pattern = "�")

# a priori on a encore deux format de date "my" et "dmy"
allsujet.dat$date_start_add_p <- parse_date_time(allsujet.dat$date_start_add_p, orders = c("my", "dmy"))
allsujet.dat$date_end_add_p <- parse_date_time(allsujet.dat$date_end_add_p, orders = c("my", "dmy"))
# les dates de naissances sont toujours renseignées et au meme format 
allsujet.dat$date_birth_p <- dmy(allsujet.dat$date_birth_p)

# verif des valeurs manquantes 
summary(allsujet.dat) 
# on a 44 valeurs manquantes en date_start et 53 date_end pourquoi ? 

# 3- quelques stats à garder en tête / EDA =============

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

# pour les dates de naissance
allsujet.dat %>% 
    ggplot(aes(x = year(date_birth_p))) +
    geom_histogram(binwidth = 1, color = "white" ) + 
    labs(x = "Années de naissance", y = "Nombre de sujets")


