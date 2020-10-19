### petit test des respartitions des adresses en fonction de rural/urbain et peri
# je souhaite verifier la precision

##.###################################################################################33
## I. Chargement des données et Mise en forme ====
##.#################################################################################33

source("Prétraitement_ETL/exploration_db.R")

library(ggplot2)


##.###################################################################################33
## II. répartition temporelle ====
##.#################################################################################33

adresse_commune.shp$intervalle_tps <- lubridate::interval(adresse_commune.shp$date_start, adresse_commune.shp$date_end) %>% 
                                        as.duration() %>%  
                                        as.numeric("days")
# dureee moyenne par adresse
mean(adresse_commune.shp$intervalle_tps, na.rm = T) 
median(adresse_commune.shp$intervalle_tps, na.rm = T)

 # 602 interval de temps nul 
length(adresse_commune.shp$intervalle_tps[adresse_commune.shp$intervalle_tps == 0])
 
adresse_commune.shp %>% 
    ggplot(aes(intervalle_tps)) +
    geom_histogram(binwidth = 360, col = "white") +
    geom_vline(xintercept =  mean(adresse_commune.shp$intervalle_tps, na.rm = T), col = "red", lwd = 1.25) +
    geom_vline(xintercept =  median(adresse_commune.shp$intervalle_tps, na.rm = T), col = "red", lwd = 1.25, lty = 2) +
    xlab("Durée de residence (jours)") + 
    ylab("nbr") +
    theme_bw()

sujet.dat <- adresse_commune.shp %>% 
                st_drop_geometry() %>% 
                group_by(sujet_id) %>% 
                summarize(date_naissance = first(date_naissance),
                          date_min = min(date_start), 
                          date_max = max(date_end),
                          nbr_adresse = n())

affiche_un_sujet("09_0431")

##.###################################################################################33
## III. répartition spatiale / type d'espace ====
##.#################################################################################33

# on reprend le shape des communes avec l'info rural-urbain-peri
communes.shp <- st_read("data/commune.shp")

str(communes.shp)

# rajout du type de commune par adresse !!! attention c'est le type de commune en 2019
adresse_commune.shp <- st_join(st_transform(adresse_sujet_temporal.shp, 2154), st_transform(communes.shp[,c("TYPE_CO", "insee")], 2154))

rm(communes.shp)

tbl_cont <- table(adresse_commune.shp$TYPE_CO, adresse_commune.shp$precision)

# repartition de la precision par rapport au type de commune 
adresse_commune.shp %>% 
    ggplot2::ggplot(aes(precision,fill = TYPE_CO)) +
    geom_bar(position="fill") +
    geom_hline(yintercept = as.numeric(apply(prop.table(tbl_cont, 2) , 1, mean)["URBAIN"])) +
    theme_bw()


dev.off()