### petit test des respartitions des adresses en fonction de rural/urbain et peri
# je souhaite verifier la precision

source("Prétraitement_ETL/exploration_db.R")

# on reprend le shape des communes avec l'info rural-urbain-peri

communes.shp <- st_read("data/commune.shp")

str(communes.shp)

adresse_commune.shp <- st_join(st_transform(adresse_sujet_temporal.shp, 2154), st_transform(communes.shp[,c("TYPE_CO", "insee")], 2154))

table(adresse_commune.shp$TYPE_CO, adresse_commune.shp$precision)

