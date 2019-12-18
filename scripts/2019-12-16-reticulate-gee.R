
# reticulate
library(reticulate)
use_condaenv(condaenv = "ee", required = TRUE)
#reticulate::conda_version()

# get earth engine options:
ee <- import("ee")

ee$Initialize()

np=import("numpy")
pd=import("pandas")

# get geometry
geometry = ee$Geometry$Point(13.4807, 52.4872)
S2 = ee$ImageCollection("COPERNICUS/S2_SR")
S2 = S2$filterDate(ee$Date('2019-05-01'), ee$Date('2019-08-01'))

# filter
S2 = S2$filterBounds(geometry)

# number of images
nbrIMages = S2$size()$getInfo()

# stuff
S2 = S2$sort("ClOUD_COVERAGE_ASSESSMENT")$first()
ndvi = S2$normalizedDifference(c('B8', 'B4'))$rename("ndvi")


ndvi$getInfo()
#ndvi$


# https://developers.google.com/earth-engine/image_edges