## script du 03/03/2020
# Mie en forne et EDA "contexte" des sujets Gouramic
# convention de code .dat <- tableau

##.###################################################################################33
## I. Chargement des données et Mise en forme ====
##.#################################################################################33


# 1- chargement des packages et données ========================

# ici j'ai pris la derniere version
# devtools::install_github("joelgombin/banR", build_vignettes = TRUE)

pkgs <-  c("dplyr","stringr", "lubridate", "ggplot2", "banR", "sf")
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
# il faudra cibler les adresses d'importances et passer par un sig ?

# adresse_test <- paste("LA TUILERIE D EN HAUT", "28400 NOGENT LE ROTROU")
# geocode(adresse_test)

# 1- Préparation des données ========================

# 1-a Cas avec ceux avec un numero de rue =====
allsujet_num_rue.dat <- allsujet.dat

# extraction du code postal pour geocode_tbl 
allsujet_num_rue_clean.dat <- data.frame(
                                     Id_cart = allsujet_num_rue.dat$ID_CARTO, # id unique
                                     # date de naissance 
                                     Date_birth = allsujet_num_rue.dat$date_birth_p,
                                     # date start et end d'adresse
                                     Date_start = allsujet_num_rue.dat$date_start_add_p,
                                     Date_end = allsujet_num_rue.dat$date_end_add_p,
                                     Code_postal = str_extract_all(allsujet_num_rue.dat$CP_commune_p, pattern = "\\b\\d{5}\\b", simplify = TRUE),
                                     Commune = str_to_upper(                                                               # on passe en UPPER
                                               str_trim(                                                                     # sans whitespace
                                               str_remove_all(allsujet_num_rue.dat$CP_commune_p, pattern = "\\b\\d{5}\\b"))), # on retire les codes postaux
                                     Nb_rue = allsujet_num_rue.dat$nb_rue_p,
                                     Rue = allsujet_num_rue.dat$rue_p,
                                     Lieu_dit = allsujet_num_rue.dat$lieudit_p,
                                     Compl_add_p = allsujet_num_rue.dat$compl_add_p,
                                     stringsAsFactors = FALSE)

# ici je passe par tidyr::unite() car il ya un filtre des Na

allsujet_num_rue_clean.dat <- allsujet_num_rue_clean.dat %>% 
    tidyr::unite("Adresse", Nb_rue, Rue, sep = " ",  na.rm = TRUE) %>% 
    tidyr::unite("Info_sup", Lieu_dit, Compl_add_p, pt_remarq_p, na.rm = TRUE)


# il y a des trous, principalement des villes sans code postal

sort(allsujet_num_rue_clean.dat$Commune[allsujet_num_rue_clean.dat$Code_postal == ""])

# Des corretions à la main  ======
allsujet_num_rue_clean.dat$Commune[allsujet_num_rue_clean.dat$Commune == "MEUDON (92)"] <- "MEUDON"
allsujet_num_rue_clean.dat$Commune[allsujet_num_rue_clean.dat$Commune == "76 SOTTEVILLE LES ROUENS"] <- "SOTTEVILLE LES ROUENS"
allsujet_num_rue_clean.dat$Commune[allsujet_num_rue_clean.dat$Commune == "VIROFALY"] <- "VIROFLAY"

# On complete, apres verif de pas eccraser dans le cas de plusieurs codes postaux pour une commune 
# cas pb des villes a arrondissement Lyon, bordeau
# attention aussi au homonymes
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "AGEN"] <- "47000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "AIGLUN"] <- "04510"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "ANNECY"] <- "74000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "ANNECY LE VIEUX"] <- "74940"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "AUXERRE"] <- "89000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "AZAY LE RIDEAU"] <- "37190"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "BANYULS SUR MER"] <- "66650"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "BOISBERGUES"] <- "80600"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "BOURG SAINT MAURICE"] <- "73700"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "BRESLES"] <- "60510"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "CARCASSONE"] <- "11000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "CHAMBERY"] <- "73000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "CHAMPTERCIER"] <- "04660" 
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "CHARLEVILLE MEZIERES"] <- "08000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "CHATOU"] <- "78400"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "CHAUMONT"] <- "52000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "CHEILLE LA CHAPELLE"] <- "37190"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "DAOURS"] <- "80800"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "DIGNE LES BAINS"] <- "04000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "DORMELLES"] <- "77130"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "DOUCHY LES MINES"] <- "59282"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "DUGNY"] <- "93440"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "ESCAUDAIN"] <- "59124"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "FLESSELLES"] <- "80260"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "FRAISSE CABARDES"] <- "11600"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "GESPUNSART"] <- "08700"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "LAMBESC"] <-"13410"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "LE CHAFFAUT SAINT JURSON"] <- "04510" 
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "LES CLAYES SOUS BOIS"] <- "78340"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "MALLEMORT"] <- "13370"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "MELUN"] <- "77000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "MEUDON"] <- "92190"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "MONTIGNY LES BRETONNEUX"] <- "78180"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "MORET SUR LOING"] <- "77250"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "NEUFMANIL"] <- "08700" 
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "NEVERS"] <- "58000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "ORLEANS LA SOURCE"] <- "45100"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "OUTREBOIS"] <- "80600"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "POITIERS"] <- "86000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "ROUBAIX"]  <- "59100"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "SAINT GERMAIN EN LAYE"] <- "78100"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "SAINT JORIOZ"] <- "74410"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "SAULZOIR"] <- "59227"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "SENNECEY LE GRAND"] <- "71240"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "SEYNOD"] <- "74600"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "SOTTEVILLE LES ROUENS"] <- "76300"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "TASSIN LA DEMI LUNE"] <- "69160"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "TOUL"] <- "54200"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "VALENCIENNES"] <- "59300"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "VELIZY"] <- "78140"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "VERSAILLES"] <- "78000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune ==  "VILLECERF"] <- "77250" 
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune ==  "VILLEFRANCHE SUR MER"] <- "06230"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune ==  "VILLEMER"] <- "77250"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune ==  "VILLERS_SEMEUSE"] <- "08000"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune ==  "VILLEURBANNE"] <- "69100"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune ==  "VIROFLAY"] <- "78220"
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune ==  "YVERNAUMONT"] <- "08430"

# il va falloir aller le chercher dans la liste des codes postaux
# # https://www.data.gouv.fr/fr/datasets/base-officielle-des-codes-postaux/
# 
# la_poste.dat <- read.csv("data/laposte_hexasmal.csv", sep = ";")
# summary(la_poste.dat)
#
# allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$code_post == ""][!
#                                                                                    allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$code_post == ""] %in% la_poste.dat$Nom_commune]
# remplacement des manquants 
# un part doit être corrige à la main 
# ancienne commune en esperant que banR marche bien avec les anciennes communes 
# sinon on devra convertir les anciennes communes en nouvelles
# "CASSETETE" celui la doit etre du troll a verif
# la part via la bd de la poste
# je mets de cote car une commune peut avoir plusieurs code postaux et qu'il va falloir rentrer à la main
# on va voir si il s'en sort sans
# sort(allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$code_post == ""]) 
#     
# la_poste.dat[la_poste.dat$Nom_commune %in% allsujet_num_rue_clean.dat$commune[allsujet_num_rue_clean.dat$code_post == ""],] %>% 
#     arrange(Nom_commune)

# 2 Geocodage de cette partie ========================
# un hist de verif
produit_geocode <- geocode_tbl(tbl = allsujet_num_rue_clean.dat, adresse = Adresse, code_postal = Code_postal)

# une sauvegarde pour eviter un appel das l'API
# saveRDS(produit_geocode, "data/produit_geocode.rds")

rm(allsujet_num_rue_clean.dat, allsujet_num_rue.dat)

produit_geocode %>% 
    ggplot(aes(result_score, fill = result_type)) +
    geom_histogram(color = "white") 

summary(as.factor(produit_geocode$result_type))

# cas generique 

openxlsx::write.xlsx(produit_geocode, "data/produit_geocode.xls")

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


table(produit_geocode$Importance_adresse, produit_geocode$result_type)


# 3- un export pour passer en SIG et faire du geocodage à la main =======================

produit_geocode.shp <- sf::st_as_sf(produit_geocode, coords = c("longitude", "latitude"), crs = 4326
                                    , na.fail = FALSE)

produit_geocode.shp$source_loc <- "geocodage"
names(produit_geocode.shp)

st_write(produit_geocode.shp, "data/geocode.geojson")
