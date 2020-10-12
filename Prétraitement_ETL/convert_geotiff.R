### un scripts pour convertire en geotif 
# octobre 2020

library("gdalUtils")

ou_sont_les_images <- "/home/lo82302h/ortho/images/"

out_dir <- "/home/lo82302h/ortho/images/img_gtiff/"
dir.create("/home/lo82302h/ortho/images/img_gtiff/")

batch_gdal_translate(
    infiles = ou_sont_les_images,
    outdir = out_dir,
    outsuffix = ".tif",
    of = "GTiff",
    pattern = "jp2$"
)

batch_gdal_translate(
    infiles = ou_sont_les_images,
    outdir = out_dir,
    outsuffix = ".tif",
    of = "GTiff",
    pattern = "jpg$"
)