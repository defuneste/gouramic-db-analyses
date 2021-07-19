### octobre 2020 
## pour le georef on a besoin de toutes les images dans le repertoire image <- modifiable aussi au besoin dans le .py 
# on va chercher les path des images qui nous interesse pour les mettres dans le bon repertoire

# attention j'attrape par l'extension 
# il y a des jpg et jp2
# si il y a d'autres formats il faut les rajouter

ou_sont_les_images <- "data"
# note il faut créer le repertoire avant
#dir.create() si on veut rester dans R
ou_vont_les_images <- "path"

liste_abouger <- c(
                    list.files(ou_sont_les_images, recursive = TRUE, pattern = "jp2$"),
                    list.files(ou_sont_les_images, recursive = TRUE, pattern = "jpg$"))

# le script de remi prend le nom de l'image commencant par IGNF etc 
# ici on prend le nom de l'image : principe c'est tout ce qui est après le C
on_separe <- sapply(liste_abouger, function(x){ strsplit(x, "/")})

# /!\ c'est une indexation relative, c'est un peu laid d'indexer ici 
nom_image <- sapply(on_separe, function(x) {x[4]})

# chemin avec nom de fichier
un_chemin <- paste0(ou_vont_les_images,  nom_image)
# juste penser à rajouter le dir ou_sont_les_images

file.copy(paste0(ou_sont_les_images, "/", liste_abouger), un_chemin)

rm(nom_image, un_chemin, on_separe, ou_sont_les_images, ou_vont_les_images)


