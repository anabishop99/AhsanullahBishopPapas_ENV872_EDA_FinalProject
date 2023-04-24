library(ncdf4) 
library(raster)
library(rgdal) 
library(ggplot2) 

temp <- nc_open('AQUA_MODIS.20230101_20230131.L3m.MO.SST.sst.9km.nc')
temps <- ncvar_get(temp, "sst")
