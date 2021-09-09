#! /bin/bash 
# Date: novembre 2020
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: Transformation de png + world file en gtiff
# Description du problème:
# C'est parfois plus simple de travailler avec un gtiff que plethore
# de fichiers + sidecars files 
# 
# c'est plus simple de se placer dans le depot ou il y a les images
# TODO tester si jpeg marche aussi et adapter 

    for filename in *.png
         do
        	echo "Prends un café ${filename}."
        	ancien_nom=${filename}
        	nouveau_nom=${ancien_nom%.*}
        	
        	gdalwarp -t_srs epsg:2154 $ancien_nom $nouveau_nom.tiff 
        	
        	echo "Reviens du café $nouveau_nom."
             
    done 