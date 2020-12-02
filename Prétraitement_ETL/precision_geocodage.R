### geocodage precision
# 20/04 

pkgs <-  c("dplyr", "tidyr", "ggplot2", "sf", "lubridate", "purrr")
inst <- lapply(pkgs, library, character.only = TRUE)

## lecture des fichiers

# fichier de distance de Matthieu
dist.dat <- read.csv("data/DistClean.csv", stringsAsFactors = FALSE) 
geocodage_clbv2.shp <- sf::st_read("data/sortie_15_04.shp" , stringsAsFactors = FALSE)

#correction formatage
geocodage_clbv2.shp$date_start <- parse_date_time(geocodage_clbv2.shp$date_start, orders = c("my", "dmy"))
geocodage_clbv2.shp$date_end_a <- parse_date_time(geocodage_clbv2.shp$date_end_a, orders = c("my", "dmy"))

# fichier de geocodage EVS
geocodage_evs.shp <- sf::st_read("data/geocodev2.geojson", stringsAsFactors = FALSE)

#correction formatage
geocodage_evs.shp$Date_birth <- as.Date(geocodage_evs.shp$Date_birth, origin = "1899-12-30")
geocodage_evs.shp$Date_start <- as.Date(geocodage_evs.shp$Date_start, origin = "1899-12-30")
geocodage_evs.shp$Date_end <- as.Date(geocodage_evs.shp$Date_end, origin = "1899-12-30")

## 1- Stats descriptives rapides =========================
# un rapide boxplot
# completement écraser par 4 valeurs 
boxplot(dist.dat$Dist_m)
        
names(dist.dat)
    
sum(table(dist.dat$Preci_CLB[dist.dat$Dist_m <= 5], dist.dat$PreciBan[dist.dat$Dist_m <= 5]))
    
table(dist.dat$Preci_CLB[dist.dat$Dist_m > 5], dist.dat$PreciBan[dist.dat$Dist_m > 5])
    
sum(table(dist.dat$Preci_CLB[dist.dat$Dist_m > 5], dist.dat$PreciBan[dist.dat$Dist_m > 5]))

## 2 - Exports pour Matthieu et Olivier avec les adresses à identifier ================
# objectif ici est d'avoir les adresses à verifier 

# 2.1 on va commencer par les NA ==========

# tout ce qui est dans geocodage EVS mais n'a pas de distance.
NA_dist.dat <- geocodage_evs.shp[!geocodage_evs.shp$Id_cart %in%  dist.dat$ID_CARTO ,]

# On mets de coté ceux remplie à la mains 

geocode_mains_Na <- NA_dist.dat[NA_dist.dat$source_loc == "main",] %>% 
        select(Id_cart, Date_start, Date_end, Commune, Adresse, Code_postal, Info_sup, result_type) #%>% 
        # st_transform(2154) # à changer si on veut passer en lambert 93

geocodage_clb_mains <- geocodage_clbv2.shp[geocodage_clbv2.shp$ID_CARTO %in% geocode_mains_Na$Id_cart, ] %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, sep = " ", na.rm = TRUE) %>% 
    select(ID_CARTO, date_start, date_end_a, Commune, Adresse, CP, Info_sup, Match_addr, Loc_name) %>% 
    st_transform(4326)

# attention il y une adresse non présente dans geocodage_clbv2.shp 
geocode_mains_Na[!geocode_mains_Na$Id_cart %in% geocodage_clbv2.shp$ID_CARTO, ]

st_write(geocode_mains_Na, dsn = "data/geocode_mains_Na.geojson", append=FALSE)
st_write(geocodage_clb_mains , dsn = "data/geocodage_clb_mains.geojson", append=FALSE)

# on exclue ce qui a été codé à la main
# il faut verifier si cela correspond à quelque chose dans le geocodage ESRI

# dim(geocodage_evs.shp[geocodage_evs.shp$source_loc == "main",])

NA_ageocoder.shp <- geocodage_evs.shp[!geocodage_evs.shp$Id_cart %in%  dist.dat$ID_CARTO, ] %>% 
                        # c'est ce que j'ai codé à la main au tout début
                        filter(source_loc != "main") %>% 
                        select(Id_cart, Date_start, Date_end, Commune, Adresse, Code_postal, Info_sup, result_type) %>% 
                        st_transform(2154)

# on sort ce qui n'a que la commune/code postal comme info

on_fera_pas_mieux <- NA_ageocoder.shp %>% 
                        filter(is.na(Adresse)) %>% 
                        filter(is.na(Info_sup)) %>% 
                        filter(!is.na(Commune)) %>% 
                        st_drop_geometry()

# comment sont-il codé par le geocodage ESRI
table(geocodage_clbv2.shp$Loc_name[geocodage_clbv2.shp$ID_CARTO %in% on_fera_pas_mieux$Id_cart])


write.table(on_fera_pas_mieux, 
            "data/on_fera_pas_mieux.csv", 
            sep = ";",
            quote = FALSE,
            row.names = FALSE,
            col.names=TRUE) 

# on retire ceux dont on ne fera pas mieux
ageocoder.shp <- NA_ageocoder.shp[!NA_ageocoder.shp$Id_cart %in% on_fera_pas_mieux$Id_cart,] %>% 
                    # ce filtre retire les cas (47) ou a très très peu d'info
                    filter(!is.na(Commune)) %>% 
                    arrange(Id_cart)

write.table(ageocoder.shp[385:769,], 
            "data/ageocoder_oli.csv", 
            sep = ";",
            quote = FALSE,
            row.names = FALSE,
            col.names=TRUE) 

geocodage_clb_oli <- geocodage_clbv2.shp[geocodage_clbv2.shp$ID_CARTO %in% ageocoder.shp$Id_cart[385:769], ] %>% 
    tidyr::unite("Info_sup", lieudit_p, compl_add_, pt_remarq_, sep = " ", na.rm = TRUE) %>% 
    select(ID_CARTO, date_start, date_end_a, Commune, Adresse, CP, Info_sup, Match_addr, Loc_name)

st_write(geocodage_clb_oli, dsn = "data/geocodage_clb_oli.shp")

ageocoder.shp[385:769,]

# on peut regarder à quoi cela correspond sur le géocodage ESRI 

table(geocodage_clbv2.shp$Loc_name[geocodage_clbv2.shp$ID_CARTO %in% ageocoder.shp$Id_cart])

nrow(ageocoder.shp)/2

    # ici c'est les differences dans les precisions

filtre_geocode <- dist.dat[dist.dat$PreciBan != dist.dat$Preci_CLB,]
