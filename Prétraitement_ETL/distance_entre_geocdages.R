# Date: Janvier 2021 
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: Verifier les deux geocodages
# Description du problème:
# Les données sont déjà geocodées verif des distances entre les deux geocodages 
# C'est en preparant la publi que je me suis renduy compte que le premier calcul de 
# distances était pas bon (cf rapport/prepa_publi.Rmd)
#
# libraries utilisées:
# "sf", "dplyr", "tidyr"

library(sf)

# I Chargement des produits des deux géocodages ============================
# cf. rapport/prepa_publi.Rmd
# distance.geojson correspond au 6724 adresses ou on a deux géocodage
# Les couples d'adresses sont repreśentés par des lignes 

adresse <- sf::read_sf("data/verif/distance.geojson")
# Ici on veut juste la distance et plus la geometrie
adresse_filtre <- sf::st_drop_geometry(adresse) 
adresse_filtre$distance <-as.numeric(adresse_filtre$distance)

# sum(adresse_filtre$distance <= 5)

# cf. geocoderalmain.R
geocoder_main <- sf::read_sf("data/geocodage_main_total.geojson")
#st_write(geocoder_main, "data/verif/deja_geocoder.geojson")

# On va chercher l'adresse dans le premier geocodage clb
geocodage_clb.shp <- sf::read_sf("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

# La mise en forme du geocodage_clb
adresse_complement <- geocodage_clb.shp %>% 
    dplyr::select(ID_CARTO, 
                  Commune,
                  CP, 
                  nb_rue_p,
                  rue_p,
                  compl_add_, 
                  pt_remarq_, 
                  lieudit_p ) %>% 
    tidyr::unite(col = Adresse,
                 nb_rue_p,
                 rue_p, 
                 sep = " ",
                 na.rm = TRUE) %>% 
    tidyr::unite(col = Info_sup,
                 lieudit_p, 
                 compl_add_, 
                 pt_remarq_, 
                 na.rm = TRUE)

# En y rajoutant la distance de distance.geojson
adresse_complement_distance <- dplyr::inner_join(adresse_complement, 
                                                 adresse_filtre, 
                                                 by = "ID_CARTO")

adresse_complement_distance <- adresse_complement_distance[adresse_complement_distance$distance >= 5,]
adresse_complement_distance$geocodage_main <- NA

# finalement on vire les  deja geocodés
adresse_complement_distance <- adresse_complement_distance[!adresse_complement_distance$ID_CARTO %in% geocoder_main$adresse_id,]


# II Un split Matthieu et Olivier pour les vérifier ============================

# le split c'est plutot fait sur rapport/prepa_publi.Rmd car ce dernier contenais des lignes ce qui fait que cétait plus facile de localisé 
# les deux adresses en meme temps
# adresse_matthieu <- adresse_complement_distance[1:(length(adresse_complement_distance$ID_CARTO)/2),]
# st_write(adresse_matthieu, "data/verif/adresse_matthieu.geojson" )
# adresse_olivier <- adresse_complement_distance[(length(adresse_complement_distance$ID_CARTO)/2):length(adresse_complement_distance$ID_CARTO),]
# st_write(adresse_olivier, "data/verif/adresse_olivier.geojson")

# III Ici on a de l'analyse je sais pas bien ce qu'elle fait là =======================
# Ici on a la répartition entre les adresses proches de moins de 5  m Rural/urbain

adresse_filtre$preci_clb <-  substr(adresse_filtre$preci_clb , 1, 1)
# table(substr(adresse_filtre$preci_clb , 1, 1))

table(adresse_filtre$preci_evs, useNA = "ifany")

adresse_filtre$preci_evs[adresse_filtre$preci_evs =="housenumber"] <- "1"
adresse_filtre$preci_evs[adresse_filtre$preci_evs =="street"] <- "3"
adresse_filtre$preci_evs[adresse_filtre$preci_evs =="locality"] <- "4"
adresse_filtre$preci_evs[adresse_filtre$preci_evs =="municipality"] <- "6"

adresse_filtre_proche <- adresse_filtre[adresse_filtre$distance <= 5 , ]

table(adresse_filtre_proche$preci_clb, adresse_filtre_proche$preci_evs)


##### rural / urbain 

communes.shp <- sf::st_read("data/commune.shp")

adresse$distance <- as.numeric(adresse$distance)

adresse_commune.shp <- st_join(
                        adresse, 
                        st_transform(communes.shp[,c("TYPE_CO", "insee")], 2154))

adresse_commune_proche.shp <- adresse_commune.shp[adresse_commune.shp$distance <= 5,]

## Ici on a la répartition entre les adresses proches de moins de 5  m Rural/urbain
table(adresse_commune_proche.shp$TYPE_CO)