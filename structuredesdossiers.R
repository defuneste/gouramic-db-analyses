### premier script explo gouramic

library(stringr)
library(rgdal)
library(raster)

list.files("data", recursive = TRUE, pattern = "res.png$")

list.files("data", recursive = TRUE, pattern = "0.res.png$")

list.files("data", recursive = TRUE, pattern = "1.res.png$")

list.files("data", recursive = TRUE, pattern = "2.res.png$")

list.files("data", recursive = TRUE, pattern = "3.res.png$")


head(list.files("data", recursive = TRUE, pattern = "res.png$"), 10)

cas <- substr(list.files("data", recursive = TRUE, pattern = "res.png$"), 1,7)

on_separe <- sapply(list.files("data", recursive = TRUE, pattern = "res.png$"), function(x){ strsplit(x, "/")})

resid <- sapply(on_separe, function(x) {as.numeric(x[2])})

exemple.dat <- data.frame(cas, resid)

# pb dans ces raster
test <- raster(paste0("data/","03_0031/1/1976/gouResult/IGNF_PVA_1-0__1976-06-08__C3620-0051_1976_FR2796_0061.jp2.0.res.png"))

res(test)

plot(test)