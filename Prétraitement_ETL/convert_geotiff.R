# Date: octobre 2020
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: Convertir en geotiff les images + world file
# Description du problème:
# Fais la meme chose que convert_classif_gtiff.sh
# Il faut au besoin specifier l'argument pattern ici 
# il fait jp2 et jpg
# /!\ Attention l'argument pattern ne prend qu'une chaine de caractère pas un vecteur
# libraries utilisées:
# "gdalUtils" 

library("gdalUtils")

# on demande où sont les images 
ou_sont_les_images <- "/home/lo82302h/ortho/images/"

# et où elles vont
out_dir <- "/home/lo82302h/ortho/images/img_gtiff/"

# si le dossier n'existe pas il est crée
ifelse(!dir.exists(out_dir), 
       dir.create(out_dir, 
                  recursive = TRUE),
       "c'est bon!")


gdalUtils::batch_gdal_translate(
    infiles = ou_sont_les_images,
    outdir = out_dir,
    outsuffix = ".tif",
    of = "GTiff",
    pattern = "jp2$"
)

gdalUtils::batch_gdal_translate(
    infiles = ou_sont_les_images,
    outdir = out_dir,
    outsuffix = ".tif",
    of = "GTiff",
    pattern = "jpg$"
)