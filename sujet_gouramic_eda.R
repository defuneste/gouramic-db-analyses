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
    tidyr::unite("Info_sup", Lieu_dit, Compl_add_p, na.rm = TRUE)


# il y a des trous, principalement des villes sans code postal

sort(allsujet_num_rue_clean.dat$Commune[allsujet_num_rue_clean.dat$Code_postal == ""])

# Des corretions à la main  ======
allsujet_num_rue_clean.dat$Commune[allsujet_num_rue_clean.dat$Commune == "MEUDON (92)"] <- "MEUDON"
allsujet_num_rue_clean.dat$Commune[allsujet_num_rue_clean.dat$Commune == "76 SOTTEVILLE LES ROUENS"] <- "SOTTEVILLE LES ROUENS"
allsujet_num_rue_clean.dat$Commune[allsujet_num_rue_clean.dat$Commune == "VIROFALY"] <- "VIROFLAY"

# On complete, apres verif de pas eccraser dans le cas de plusieurs codes postaux pour une commune 
# cas pb des villes a arrondissement Lyon, bordeau
# attention aussi au homonymes
allsujet_num_rue_clean.dat$Code_postal[allsujet_num_rue_clean.dat$Commune == "AGEN",] <- "47000"
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

rm(allsujet_num_rue_clean.dat, allsujet_num_rue.dat)

produit_geocode %>% 
    ggplot(aes(result_score, fill = result_type)) +
    geom_histogram(color = "white") 

summary(as.factor(produit_geocode$result_type))

# cas generique 

openxlsx::write.xlsx(produit_geocode_p1, "data/produit_geocode.xls")

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


