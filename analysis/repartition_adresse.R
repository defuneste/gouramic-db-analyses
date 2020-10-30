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

 # 23 intervals de temps nul 
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
                          nbr_adresse = n()) %>% 
                mutate(intervalle_tps = as.numeric(as.duration(interval(date_min, date_max)) , "days"))


adresse_commune.dat <- st_drop_geometry(adresse_commune.shp) %>% 
    arrange(sujet_id, date_start) %>% 
    group_by(sujet_id) %>% 
    mutate(adresse_jointives = 1:n())

for(i in 1:length(sujet.dat$sujet_id)) {
            jim <- adresse_commune.dat[adresse_commune.dat$sujet_id ==sujet.dat$sujet_id[i],]
            merge.indices <- lapply(2:nrow(jim), function(x) {
                indices <- which(findInterval(jim$date_end[1:(x-1)], jim$date_start[x]) == 1 )
                if (length(indices) > 0) indices <- c(indices, x) 
                indices})
    
            for (j in 1:length(merge.indices)) {
                if (length(merge.indices[[j]]) > 0) {
adresse_commune.dat[adresse_commune.dat$sujet_id ==sujet.dat$sujet_id[i],]$adresse_jointives[merge.indices[[j]]] <- min(adresse_commune.dat[adresse_commune.dat$sujet_id ==sujet.dat$sujet_id[i],]$adresse_jointives[merge.indices[[j]]])

        }
    }}

# il faut calculer la durée de chaque groupe d'adresses jointives (les adresses pouvant se superposer)

temps_habite.dat <- adresse_commune.dat %>% 
    group_by(sujet_id, adresse_jointives) %>% 
    summarise(date_min = min(date_start),
              date_max = max(date_end)) %>% 
    mutate(inter = as.numeric(as.duration(interval(date_min, date_max)) , "days")) %>% 
    select(sujet_id, inter) %>% 
    group_by(sujet_id) %>% 
    summarise(temps_habite = sum(inter))


sujet.dat <- sujet.dat %>% 
             left_join(temps_habite.dat, by = "sujet_id")

# a noter que je ne garde pas la date de la naisance sinon je peux faire des trou negatif dans le cas ou la date
# de naissance est inferieur à 
sujet.dat$dif <- sujet.dat$intervalle_tps - sujet.dat$temps_habite

sujet.dat %>% 
    filter(dif == 0) 

# nombre de jour sans adresses
sujet.dat %>% 
    filter(dif != 0) %>% 
ggplot( aes(x = dif)) +
    geom_histogram(binwidth = 180, col = "white") + 
    xlab("Nbr de jours, Pas de 180 jours") + 
    ylab("nbr") +
    labs(title = "Répartition des écarts entre les jours avec une résidence et ceux sans",
        subtitle = "682 sujets sans écart") +
    theme_bw() 

# ici vu un petit pb sur le cas de meme adresse

adresse_commune.dat[adresse_commune.dat$sujet_id == "20_0493",] %>% 
    ggplot(aes(y = adresse_clb, x = date_start)) +
    geom_segment(aes(x = date_start, y = adresse_clb, xend = date_end, yend = adresse_clb, col = as.factor(adresse_jointives), lwd = 1)) +
    theme_bw()

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

