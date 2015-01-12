# Team: ZeaPol   
# Team Members: Roeland de Koning / Barbara Sienkiewicz    
# Date: 12/01/2015       
# Exercise 6


# Load libraries:

library(raster)
library(rgeos)
library(rgdal)
library(maptools)
library(gstat)


# Source:

source('R/ZipFileDownloader.R')


# Use ZipFileDownloader:

ZipFileDownloader("https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip",
                  'MODIS','MODIS.zip')


# Locate MODIS and Municipality data, from file and online respectively:

MODIS <- list.files('data/ MODIS/', pattern = glob2rx("*.grd"), full.names = TRUE)
NLCity <- getData('GADM', country = 'NLD', level = 3, path = 'data/')


# Create MODIS raster and adjsut to more legible scale:

MODISRaster <- brick(MODIS)
MODISRaster2 <- MODISRaster/10000


# Transform municipality data to raster projection:

NLCityUTM <- spTransform(NLCity, CRS(proj4string(MODISRaster2)))
MODISNL <- mask(MODISRaster2, NLCityUTM)


# Calculate average NDVI of all months:

MODISNLYearavg <- mean(MODISNL)


# Check and view basic raster and municipality plots:

plot(MODISNL)
plot(NLCityUTM)


# Extract raster averages based on the municipality boundaries:

NDVIJanuaryCityMean <- extract(MODISNL$January, NLCityUTM, fun = mean, df = TRUE, factors = FALSE, 
                                along = FALSE, sp = TRUE)
NDVIAugustCityMean <- extract(MODISNL$August, NLCityUTM, fun = mean, df = TRUE, factors = FALSE, 
                                along = FALSE, sp = TRUE)
NDVIYearCityMean <- extract(MODISNLYearavg, NLCityUTM, fun = mean, df = TRUE, factors = FALSE, 
                                along = FALSE, sp = TRUE)


# Create sp plots and store them for later use:

p1 <- spplot(NDVIJanuaryCityMean, zcol= "January", main = 'NDVI average January Municipalities NL', 
             col.regions = colorRampPalette(c("#FF7F00","#7FFF00"))(20))
p2 <- spplot(NDVIAugustCityMean, zcol= "August", main = 'NDVI average August Municipalities NL', 
             col.regions = colorRampPalette(c("#FF7F00","#7FFF00"))(20))
p3 <- spplot(NDVIYearCityMean, zcol = 'layer', main = 'NDVI average Year Municipalities NL', 
             col.regions = colorRampPalette(c("#FF7F00","#7FFF00"))(20))


# Plot the sp plots as a single view:

print(p1, position = c(0,.5,.5,1),more=T)
print(p2, position = c(.5,.5,1,1),more = T)
print(p3, position = c(0,0,1,.5))


# Calculate the greenest city (NDVI) in January, August and Year round:

i <- max(NDVIJanuaryCityMean$January, na.rm = T)
subset(NDVIJanuaryCityMean, January == i, select = c(NAME_1, NAME_2, January))
# Greenest City in January: Friesland, Littenseradiel, 0.8379682 

j <- max(NDVIAugustCityMean$August, na.rm = T)
subset(NDVIAugustCityMean, August == j, select = c(NAME_1, NAME_2, August))
# Greenest City in August: Gelderland, Vorden, 0.835038 

k <- max(NDVIYearCityMean$layer, na.rm = T)
subset(NDVIYearCityMean, layer == k, select = c(NAME_1, NAME_2, layer))
# Greenest Year round city: Zuid-Holland, Graafstroom, 0.8024537

