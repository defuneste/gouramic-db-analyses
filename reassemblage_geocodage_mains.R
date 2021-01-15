### reassemblage des fichier géocoder verifier après la mesure de distance entre les desux geocodages.


library(sf)

part_oli <- sf::st_read("data/verif/adresse_olivier.geojson")
part_oli$geocodeur <- "olivier"
# 1299

part_matt_a <- sf::st_read("data/verif/adresse_matthieuok.geojson") 
part_matt_a$geocodeur <- "matthieu"
# 2090

part_matt_b <- sf::st_read("data/verif/adresse_matthieu_bisbok.geojson")
part_matt_b$geocodeur <- "matthieu"

verif_ecart.shp <- rbind(part_oli, part_matt_a, part_matt_b)

table(verif_ecart.shp$geocodage_main)