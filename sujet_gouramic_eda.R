## script du 03/03/2020
# Mie en forne et EDA "contexte" des sujets Gouramic
# convention de code .dat <- tableau

##.###################################################################################33
## I. Chargement des données et Mise en forme ====
##.#################################################################################33


# 1- chargement des packages et données ========================

# ici j'ai pris la derniere version
# devtools::install_github("joelgombin/banR", build_vignettes = TRUE)

pkgs <-  c("dplyr","stringr", "lubridate", "ggplot2", "banR")
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

# on peut droper study_population qui prend tjrs la valeur de 1
allsujet.dat <- subset(allsujet.dat, select = -study_population)

##.###################################################################################33
## II. Geocodage ====
##.#################################################################################33

# il y a 7661 adresses 
# combien de Na

dim(allsujet.dat[is.na(allsujet.dat$CP_commune_p),])
# il y a 48 adresses sans renseignement : NA
# il faut divisier le probleme
# je pense d'abord regarder les cas ou j'ai un numero de rue 

# un exemple
# adresse_test <- paste("LA TUILERIE D EN HAUT", "28400 NOGENT LE ROTROU")
# geocode(adresse_test)

# 1- Préparation des données ========================

# on fait tout ceux avec un numero de rue
allsujet_num_rue.dat <- allsujet.dat[!is.na(allsujet.dat$nb_rue_p),]

# on va extraire le code postale
allsujet_num_rue_clean.dat <- data.frame(
                                     code_post = str_extract_all(allsujet_num_rue.dat$CP_commune_p, pattern = "\\b\\d{5}\\b", simplify = TRUE),
                                     commune = str_to_upper(                                                               # on passe en UPPER
                                               str_trim(                                                                     # sans whitespace
                                               str_remove_all(allsujet_num_rue.dat$CP_commune_p, pattern = "\\b\\d{5}\\b"))) # on retire les codes postaux
                                           )


# pas des facteurs
allsujet_num_rue_clean.dat$commune <- as.character(allsujet_num_rue_clean.dat$commune)
allsujet_num_rue_clean.dat$code_post <- as.character(allsujet_num_rue_clean.dat$code_post)
# id 
allsujet_num_rue_clean.dat$ID_CARTO <- allsujet.dat$ID_CARTO[!is.na(allsujet.dat$nb_rue_p)]

# il y a des trous, principalement des villes sans code postal
# il va falloir aller le chercher dans la liste des codes postaux


allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$code_post == ""]
# trois corretions à la main 
allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$commune == "MEUDON (92)"] <- "MEUDON"
allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$commune == "76 SOTTEVILLE LES ROUENS"] <- "SOTTEVILLE LES ROUENS"
allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$commune == "VIROFALY"] <- "VIROFLAY"

# https://www.data.gouv.fr/fr/datasets/base-officielle-des-codes-postaux/

la_poste.dat <- read.csv("data/laposte_hexasmal.csv", sep = ";")
summary(la_poste.dat)

# remplacement des manquants 
# un part doit être corrige à la main 
# ancienne commune en esperant que banR marche bien avec les anciennes communes 
# sinon on devra convertir les anciennes communes en nouvelles
allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$code_post == ""][!
                                                allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$code_post == ""] %in% la_poste.dat$Nom_commune]

allsujet_num_rue_clean.dat$code_post[allsujet_num_rue_clean.dat$commune == "MONTIGNY LES BRETONNEUX"] <- 78180
allsujet_num_rue_clean.dat$code_post[allsujet_num_rue_clean.dat$commune == "SEYNOD"] <- 74600
allsujet_num_rue_clean.dat$code_post[allsujet_num_rue_clean.dat$commune == "SAINT JORIOZ"] <- 74410
allsujet_num_rue_clean.dat$code_post[allsujet_num_rue_clean.dat$commune == "LYON"] <- 69001 # faux mais on va tester si cela passe
allsujet_num_rue_clean.dat$code_post[allsujet_num_rue_clean.dat$commune == "VELIZY"] <- 78140
allsujet_num_rue_clean.dat$code_post[allsujet_num_rue_clean.dat$commune == "MORET SUR LOING"] <- 77250
allsujet_num_rue_clean.dat$code_post[allsujet_num_rue_clean.dat$commune == "SOTTEVILLE LES ROUENS"] <- 76300
allsujet_num_rue_clean.dat$code_post[allsujet_num_rue_clean.dat$commune == "SAINT GERMAIN EN LAYE"] <- 78100

# "CASSETETE" celui la doit etre du troll a verif

# la part via la bd de la poste
# je mets de cote car une commune peut avoir plusieurs code postaux et qu'il va falloir rentrer à la main
# on va voir si il s'en sort sans
# sort(allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$code_post == ""]) 
#     
# la_poste.dat[la_poste.dat$Nom_commune %in% allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$code_post == ""],] %>% 
#     arrange(Nom_commune)

allsujet_num_rue_clean.dat$adresse_num_rue <- paste(allsujet_num_rue.dat$nb_rue_p, allsujet_num_rue.dat$rue_p)

# 2- Geocodage de cette partie ========================
produit_geocode_p1 <- geocode_tbl(tbl = allsujet_num_rue_clean.dat, adresse = adresse_num_rue, code_postal = code_post)
openxlsx::write.xlsx(produit_geocode_p1, "data/part1_geocodage.xls")

##.###################################################################################33
## III. quelques stats à garder en tête / EDA ====
##.#################################################################################33

# 1- quelques stats à garder en tête / EDA =============
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
allsujet.dat %>% 
    ggplot(aes(x = year(date_birth_p))) +
    geom_histogram(binwidth = 1, color = "white" ) + 
    labs(x = "Années de naissance", y = "Nombre de sujets")


