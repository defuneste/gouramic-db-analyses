# Date: Octobre 2020, modification Septembre 2021 
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: Script pour deplacer avant le pregeoref auto re Remi
# Description du problème:
# pour le georef on a besoin de toutes les images dans le repertoire "image" <- modifiable aussi au besoin dans le .py
# on va chercher les path des images qui nous interessent pour les mettres dans le bon repertoire.
# Comme la tache est assez simple, elle est aussi concue pour passer par Rscript avec des arguments dans un ordre precis
# Ex: Rscript deplacement_images.R ou_sont_les_images ou_vont_les_images 
# /!\ c'est du brut et il y a 0 test 
# libraries utilisées:
# R base si je ne me plante pas 
# /!\ attention j'attrape par l'extension 
# il y a des jpg et jp2
# si il y a d'autres formats il faudra les rajouter

main <- function() {
    
    argv <- commandArgs(trailingOnly = TRUE)
    
    ou_sont_les_images <- argv[1]
    # note il faut créer le repertoire avant
    #dir.create() si on veut rester dans R
    ou_vont_les_images <- argv[2]
    
    liste_abouger <- c(
                        list.files(ou_sont_les_images, 
                                   recursive = TRUE, 
                                   pattern = "jp2$"),
                        list.files(ou_sont_les_images, 
                                   recursive = TRUE, 
                                   pattern = "jpg$")
                        )
    
    # le script de Remi prend le nom de l'image commencant par IGNF etc 
    # ici on prend le nom de l'image : principe c'est tout ce qui est après le C
    on_separe <- sapply(liste_abouger, 
                        function(x){ strsplit(x, "/")})
    
    # /!\ c'est une indexation relative, c'est un peu laid d'indexer ici 
    nom_image <- sapply(on_separe, 
                        function(x) {x[4]})
    
    # chemin avec nom de fichier
    un_chemin <- paste0(ou_vont_les_images,  nom_image)
    # juste penser à rajouter le dir où_sont_les_images
    
    file.copy(paste0(ou_sont_les_images, "/", 
                     liste_abouger),
              un_chemin)

}

# On peut nettoyer l'envt avec la suite si on est plus dans une fonction
# rm(nom_image, un_chemin, on_separe, ou_sont_les_images, ou_vont_les_images)


