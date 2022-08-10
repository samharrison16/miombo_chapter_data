
library(raster)
library(sp)
library(rgdal)


L <- list()
L$agb <-raster('agb_2017.tif')
L$ESA <-raster('ESA_landcover.tif')
L$ecoregion <-raster('ecoregions.tif')
L$spam <-raster('MAPspam.tif')

L$lat <- Lresamp[[1]] # create temp raster 
L$long <- Lresamp[[1]]
L$area <- raster::area(Lresamp$biomass)

veg_class = c(10, 11, 12, 20, 30, 40, 50, 60, 61, 62, 70, 71, 72, 80, 
              81, 82, 90, 100, 110, 120, 121, 122, 130, 140, 150, 151, 
              152, 153, 160, 170, 180) 
crop_class = c(10, 11, 12, 20, 30, 40)
noncrop_class = c(50, 60, 61, 62, 70, 71, 72, 80, 
                  81, 82, 90, 100, 110, 120, 121, 122, 130, 140, 150, 151, 
                  152, 153, 160, 170, 180)
#ecoregion IDs for SA woodlands
objIDs = c(30704,30701,30706,30719,30725,30702,30726,30709,31006,31001,
           30101,30108,31015,31014,30902,30907,30906,30908,30125,30128,
           30203,31002,30708)

lcskey = read.csv('lcskey.csv')
table = subset(lcskey, ECO_ID %in% objIDs)


miombo = lapply(L, mask, mask = (L$ecoregion %in% objIDs), maskvalue = 1, inverse = TRUE)
noncrop =  lapply(miombo, mask, mask = (miombo$spam > 0.1 & miombo$spam < 2), maskvalue = 1, inverse = FALSE)

GADM = readOGR('gadm36_0_miombo.shp')
moz = subset(GADM, GID_0 == 'MOZ')
plot(moz)
moz_noncrop = lapply(noncrop, mask, mask = moz)
moz_miombo = lapply(miombo, mask, mask = moz)


#area of ecoregion (any vegetated land cover)
areafunc = function(olson){
  sum(moz_miombo$area[moz_miombo$ecoregion %in% olson & 
                        moz_miombo$ESA %in% veg_class]/1000000)
}

#wooded area, biomass >10, land cover vegetated not cropland
woodedarea = function(olson){
  sum(moz_noncrop$area[moz_noncrop$ecoregion %in% olson & 
                         moz_noncrop$biomass > 10 &
                         moz_noncrop$ESA %in% noncrop_class
  ]/1000000)
}


table$olsen <- apply(table[,c('ECO_ID'), drop = F], 1,areafunc)
table$wooded2 <- apply(table[,c('ECO_ID'), drop = F], 1,woodedarea)

