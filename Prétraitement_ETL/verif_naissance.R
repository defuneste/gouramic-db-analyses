## decembre 2020
# extraction naissance et split entre Matthieu et olivier


source("Prétraitement_ETL/exploration_db.R")

# une solution à la va vite

ordered_adresse <- adresse_sujet_temporal.shp[order(adresse_sujet_temporal.shp$sujet_id, adresse_sujet_temporal.shp$date_start),]

naissance <- ordered_adresse[!duplicated(ordered_adresse$sujet_id),]


