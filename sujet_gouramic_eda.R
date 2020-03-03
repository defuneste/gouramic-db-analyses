## script du 03/03/2020
# Mie en forne et EDA "contexte" des sujets Gouramic
# convention de code .dat <- tableau

# 1- chargement des packages et données ========================

pkgs <-  c("dplyr","stringr")
inst <- lapply(pkgs, library, character.only = TRUE)

allsujet.dat <- openxlsx::read.xlsx("data/all_Sujets.xlsx")


str(allsujet.dat)
summary(allsujet.dat)

# a priori on a plusieurs formes d'encoding 
# testé latin1 sans succés 

allsujet.dat$compl_add_p <- str_remove_all(allsujet.dat$compl_add_p, pattern = "�")
allsujet.dat$pt_remarq_p <- str_remove_all(allsujet.dat$pt_remarq_p, pattern = "�")

