#! /bin/bash 

# novembre 2020
# transformation de png + world file en gtiff
# besoin de gdal  

    for filename in *.png
         do
        	echo "Prends un café ${filename}."
        	ancien_nom=${filename}
        	nouveau_nom=${ancien_nom%.*}
        	
        	gdalwarp -t_srs epsg:2154 $ancien_nom $nouveau_nom.tiff 
        	
        	echo "Reviens du café $nouveau_nom."
             
                   
    done 