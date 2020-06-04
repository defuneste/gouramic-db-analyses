## un test pour voir si un reseau de sankey presenterait bien les données

# 1- chargement des packages et données ========================

pkgs <-  c("dplyr","stringr", "lubridate", "ggplot2", "sf", "networkD3")
inst <- lapply(pkgs, library, character.only = TRUE)


## 2 l exemple pour comprendre le format d entré ===============

# URL <- paste0(
#     "https://cdn.rawgit.com/christophergandrud/networkD3/",
#     "master/JSONdata/energy.json")
# Energy <- jsonlite::fromJSON(URL)
# # Plot
# sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
#               Target = "target", Value = "value", NodeID = "name",
#               units = "TWh", fontSize = 12, nodeWidth = 30)


# la fonction prend une liste de deux df ou probablement deux df avec des clefs similaires
# $nodes prend les nom des noeuds
# $links corresponds a un reseau de type "from - to " avec des poids 

etape <- data.frame(c(0 ,1 ,2 ,3 ,4 ,5, 6 ,7), c("Départ", "NA", "Sans-NA", "- de 5m", "+ de 5m","Adresses non trouvées", "Loc. idem",  "Loc. diff."), fix.empty.names = F)
names(etape) <- c("num", "nom")

liens <- data.frame(c(0 ,0 ,2 ,2, 2, 3, 3), c(1 ,2, 3, 4, 5, 6 , 7), fix.empty.names = F)
names(liens) <- c("from", "to")
liens$value <- c(48, 7613, 6395, 131, 1135, 5428, 967)

sankeyNetwork(Links = liens, Nodes = etape, Source = "from",
                            Target = "to", Value = "value", NodeID = "nom",
                            units = "", fontSize = 12, nodeWidth = 30)


sankeyGouramic <- list()

