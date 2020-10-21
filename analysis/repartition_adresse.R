### petit test des respartitions des adresses en fonction de rural/urbain et peri
# je souhaite verifier la precision

##.###################################################################################33
## I. Chargement des données et Mise en forme ====
##.#################################################################################33

source("Prétraitement_ETL/exploration_db.R")

library(ggplot2)


# on reprend le shape des communes avec l'info rural-urbain-peri
communes.shp <- st_read("data/commune.shp")

str(communes.shp)

# rajout du type de commune par adresse !!! attention c'est le type de commune en 2019
adresse_commune.shp <- st_join(st_transform(adresse_sujet_temporal.shp, 2154), st_transform(communes.shp[,c("TYPE_CO", "insee")], 2154))


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

bill <- affiche_un_sujet("15_0738")


## fonction pour ploter les sujets avec leurs adresses groupées (ie jointive)
# penser à renomer les variables intermediaires 

plot_group_adresse <- function(liste_adresse){
jim <- liste_adresse 
jim$adresse_clb <- with(jim, reorder(adresse_clb, date_start)) 
jim <- st_drop_geometry(jim)
jim$adresse_jointives <- 1:nrow(jim)

merge.indices <- lapply(2:nrow(jim), function(x) {
    indices <- which(findInterval(jim$date_end[1:(x-1)], jim$date_start[x]) == 1 )
    if (length(indices) > 0) indices <- c(indices, x) 
    indices})

for (i in 1:length(merge.indices)) {
    if (length(merge.indices[[i]]) > 0) {
        jim$adresse_jointives[merge.indices[[i]]] <- min(jim$adresse_jointives[merge.indices[[i]]])
    }
}

ggplot(jim, aes(y = adresse_clb, x = date_start)) +
    geom_segment(aes(x = date_start, y = adresse_clb, xend = date_end, yend = adresse_clb, col = as.factor(adresse_jointives), lwd = 1), data = jim) +
    theme_bw()

}

plot_group_adresse(bill)



as.duration(interval(first(jim$date_naissance), max(bob$end))) -
as.duration(interval(first(test$date_naissance), max(test$date_end)))

interval_total <- lubridate::interval(min(test$date_start), max(test$date_end))


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