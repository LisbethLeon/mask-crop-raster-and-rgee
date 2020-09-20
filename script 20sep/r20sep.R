setwd("C:/Users/Usuario/Desktop/20sep")
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(rgee)
library(mapview)
library(mapedit)
ee_Initialize("fer")
#cargar shape provincias
prov <- st_read("Provincias.shp")
mapview(canta_dist)
##dem
dem1 <- raster("ASTGTM_S12W077_dem.tif")
dem2 <- raster("ASTGTM_S12W078_dem.tif")
#Unir 2 raster con merge
dem <- merge(dem1, dem2)
#filtrar canta
canta <- prov %>% 
  filter(PROCOD98 == 1504)
#mapview(list(dem_t, canta))
#mask raster -> as(simple feature, "Spatial)
sp_canta <- as(canta, "Spatial")
canta_mask<- mask(dem, sp_canta)        
plot(canta_mask)                
##crop (corte)
canta_crop <- crop(dem, sp_canta)
plot(canta_crop)
canta_dem <- mask(canta_crop, sp_canta)
plot(canta_dem)
##altitud media canta
altitud_media <- raster::extract(canta_dem, 
                                 sp_canta, f
                                 un = mean)
##area relativa a canta
area <- mapview(canta_dem) %>% 
  editMap()
area_sf <- area$all
plot(area_sf)
mapview(list(canta_dem, area_sf))

##sf -> ee
area_ee <- sf_as_ee(area_sf)

##extrayendo datos con rgee
##pp
ee_pp <- ee$ImageCollection("ECMWF/ERA5/MONTHLY")$
  filterDate("2018-01-01", "2019-01-01")$
  first()
pp_stack <- ee_as_raster(imag  = ee_pp,
                         region = area_ee$geometry())
pp_area <- pp_stack[[5]]
plot(pp_area)
mapview(list(pp_area, canta_dem))

##surface pressure (Pa)
presion_area <- pp_stack[[6]]
plot(presion_area)

##puntos
puntos <- mapview(canta_dem) %>% 
  editMap()
puntos_sf <- puntos$all
plot(canta_dem,
     main = "Canta")
plot(puntos_sf, add = T)
#agregando columna de pp 
puntos_sf$pp <- raster::extract(pp_area, puntos_sf)
puntos_sf$Altitud <- raster::extract(canta_dem, puntos_sf)
puntos_sf$Presión <- raster::extract(presion_area, puntos_sf)

##Relacionando elementos climáticos 
comparar <- puntos_sf %>%
  as_tibble() %>% 
  dplyr::select(pp, Altitud, Presión)
cor(comparar)
#plot altitud vs surface pressure
plot(comparar$Altitud, comparar$Presión,
     main = "Altitude vs Surface Pressure",
     ylab = "Surface Pressure(Pa)",
     xlab = " Altitude (m)")
## pp instantánea
pp_hour <- ee$ImageCollection("JAXA/GPM_L3/GSMaP/v6/operational")$
  filterDate("2018-08-06", "2018-08-07")$
  first()
pp2_stack <- ee_as_raster(imag  = ee_pp,
                         region = area_ee$geometry())
pp_area_hour <- pp2_stack[[1]]
plot(pp_area_hour)
mapview(list(pp_area_hour, canta_dem))

##Albedo instantáneo
albedo <- ee$ImageCollection("NASA/GLDAS/V021/NOAH/G025/T3H")$
  filterDate("2018-08-06", "2018-08-07")$
  first()
albedo_stack <- ee_as_raster(imag  = albedo,
                          region = area_ee$geometry())
albedo_area <- albedo_stack[[1]]
plot(albedo_area)
mapview(list(albedo_area, canta_dem))

puntos_sf$Albedo <- raster::extract(albedo_area, puntos_sf)

comparar <- puntos_sf %>%
  as_tibble() %>% 
  dplyr::select(pp, Altitud, Presión, Albedo)
