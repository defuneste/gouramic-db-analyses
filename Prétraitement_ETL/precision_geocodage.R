### geocodage precision
# 20/04 

pkgs <-  c("dplyr", "tidyr", "ggplot2")
inst <- lapply(pkgs, library, character.only = TRUE)

## lecture du fichier

dist.dat <- read.csv("data/DistClean.csv") 

# un rapide boxplot
# completement écraser par 4 valeurs 
boxplot(dist.dat$Dist_m)
    
names(dist.dat)

sum(table(dist.dat$Preci_CLB[dist.dat$Dist_m <= 5], dist.dat$PreciBan[dist.dat$Dist_m <= 5]))

table(dist.dat$Preci_CLB[dist.dat$Dist_m > 5], dist.dat$PreciBan[dist.dat$Dist_m > 5])

sum(table(dist.dat$Preci_CLB[dist.dat$Dist_m > 5], dist.dat$PreciBan[dist.dat$Dist_m > 5]))