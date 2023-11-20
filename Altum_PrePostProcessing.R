# This script is used for processing remote sensing UAV imagery for the multispectral and thermal Micasense Altum (not Altum-PT)

### PREPROCESSING (lines 3-45)

#load libraries
{library(raster)
library(rgdal)
library(viridis)}

# load tiffs
{B1 <- raster("B1.tiff") #blue
B2 <- raster("B2.tiff") #green
B3 <- raster("B3.tiff") #red
B4 <- raster("B4.tiff") #red edge
B5 <- raster("B5.tiff") #near-IR
B6C <- raster("B6C.tiff") #LWIR/thermal Celsius
B6K <- raster("B6K.tiff")} #LWIR/thermal Kelvin (for TSEB)

# load and crop tiffs to field extent using a .kml file created in google earth
{kml25 <- readOGR("KG25.kml") # read kml for block 25
kml26 <- readOGR("KG26.kml")} # read kml for block 26

{B1 <- crop(B1, kml25)
B2 <- crop(B2, kml25)
B3 <- crop(B3, kml25)
B4 <- crop(B4, kml25)
B5 <- crop(B5, kml25)
B6C <- crop(B6C, kml25)
B6K <- crop(B6K, kml25)}

# Aggregate so that each pixel includes both veg and soil components for TSEB model
  # Formula: 
    # Ground sample distance = 0.0388m at 90m, row width = 6.4m, so 6.4/0.0388 = 164.94 (factor = 164.94)

B6K_agg <- aggregate(B6K, fact=164.94, fun=mean)

# Quality check
plot(B6K_agg, axes = FALSE, col=viridis(256), main = "B6K")

# Save aggregated rasters as a .tiff
writeRaster(B6K_agg, filename="B6K_agg.tif", overwrite=TRUE)

#Calculate NDVI
NDVI <- ((B5-B3)/(B5+B3))
writeRaster(NDVI, filename="NDVI.tiff", overwrite=TRUE)






#POSTPROCESSING (lines 52-72)

LE <- raster("test_LE1.tif") # load latent heat flux raster from TSEB outputs

  # load constants
  mW <- 0.018                  # [kg mol-1] - molecular mass of water, from C&N table A1
  rhoW <- 1000                 # [kg m-3] - density of water, from C&N table A2
  T_air <- 27.04               # [deg. C] - air temperature during time of flight, need to change
  
  # Convert LE (W/m2) to ETa (mm/hr)
  lamda <- (2.495-(2.36e-3)*T_air)*mW*1000000    # [J mol-1] - latent heat of vaporization of water, dependent on temp [degC]. Formula B-21 from Dingman 'Physical Hydrology'
  ET <- (mW/rhoW)*(60*60)*1000*LE/lamda         # [mm hr-1] - evaporation rate
  ET <- calc(ET, fun=function(x){ x[x < 0.0001] <- 0; return(x)} ) # sets an ET pixel equal to 0 if it calculates something below 0 (seems to happen occasionally in areas with extremely low veg cover)  
  
  # Plot ET raster  
  plot(ET, axes = FALSE, col=viridis(256), main = "ET Flight ___")
  
  # export large rasterlayer as png
  {png('ET.png', height=nrow(ET), width=ncol(ET)) 
  plot(ET, maxpixels=ncell(ET), axes = FALSE)
  dev.off()}
