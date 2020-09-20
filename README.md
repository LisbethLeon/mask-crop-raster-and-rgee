# Usando librerías raster y rgee para relacionar elementos climáticos
Elaborado por Fernando Herrera, 20 de septiembre del 2020.

## Introducción

En este ejercicio, extraeremos el dem para una provincia en específico para luego con ayuda de la librería "rgee" extraer imágenes y relacionar sus elementos climáticos

## Librerías necesarias en R

    library(raster)
    library(rgee)
	library(sf)
	library(mapview)
	library(mapedit)

#### Antes de empezar ⏸
>**¡No olvidarte setear tu directorio de trabajo!**

### Descarga de datos 

>Descargar los datos raster de algún geoservidor oficial. Ejemplo:
 http://geoservidorperu.minam.gob.pe/geoservidor/download_raster.aspx
 

### Empecemos 🔧
##### 1.Cargar los shapes de provincia
>prov <- st_read("Provincias.shp")

##### 2. Cargar los raster previamente descargados
>dem1 <- raster("ASTGTM_S12W077_dem.tif")
dem2 <- raster("ASTGTM_S12W078_dem.tif")

##### 3. Unir 2 raster con merge
>dem <- merge(dem1, dem2)

##### 4. Filtrar solo la provincia deseada
>canta <- prov %>% 
  filter(PROCOD98 == 1504)
  
##### 5. Comprobar si es el area deseada
>mapview(list(dem, canta))

![Mapview_dem_canta](https://user-images.githubusercontent.com/70491176/93718911-ca622900-fb44-11ea-8138-dd6d83ba92e9.PNG)

##### 6. Importante: Objeto Simple Feature a Spatial: as(tu simple feature, "spatial")
> sp_canta <- as(canta, "Spatial")

##### 7. Hacer una máscara para nuestra región
>canta_mask<- mask(dem, sp_canta)        
plot(canta_mask) 
![dem_canta_plo](https://user-images.githubusercontent.com/70491176/93719025-8cb1d000-fb45-11ea-9e8e-e46cfcf013c1.PNG)

##### 8. Hora de hacer un corte y solo tener los datos de nuestra región deseada 
>canta_crop <- crop(dem, sp_canta)
plot(canta_crop)
![crop_dem_canta](https://user-images.githubusercontent.com/70491176/93719060-dbf80080-fb45-11ea-8cb6-435d562f95d9.png)
Como se puede apreciar en el ploteo, aún no tenemos el area que necesitamos. Para esto haremos una máscara encima de este crop.

##### 9. Hacer mask del crop
>canta_dem <- mask(canta_crop, sp_canta)
plot(canta_dem)
![dem_solo_canta](https://user-images.githubusercontent.com/70491176/93719117-342f0280-fb46-11ea-8da9-552502ebc3dc.png)
¡Con este raster obtenido ya podríamos empezar a trabajar!

##### Hallar la altitud media para canta (w/e)
>altitud_media <- raster::extract(canta_dem, 
                                 sp_canta, 
				fun = mean)
								 
2992.368
##### 10. Ahora con el uso de mapview y editMap seleccionaremos una area relativa a Canta para analizar algunos elementos climáticos.
>area <- mapview(canta_dem) %>% 
  editMap()

>area_sf <- area$all

>plot(area_sf)

>mapview(list(canta_dem, area_sf))

![mapview_area_demcanta](https://user-images.githubusercontent.com/70491176/93719829-bde0cf00-fb4a-11ea-9607-99bdb00eb2ae.png)


##### 11. Conviertiendo un objeto sf a ee con la librería rgee para luego extrar datos.
>area_ee <- sf_as_ee(area_sf)

##### 12. Extrayendo datos con rgee
>ee_pp <- ee$ImageCollection("ECMWF/ERA5/MONTHLY")$
  filterDate("2018-01-01", "2019-01-01")$
  first()

>pp_stack <- ee_as_raster(imag  = ee_pp,
                         region = area_ee$geometry())

>pp_area <- pp_stack[[5]]

>plot(pp_area)

>mapview(list(pp_area, canta_dem))
![mapview_pparea_cantadem](https://user-images.githubusercontent.com/70491176/93719868-04cec480-fb4b-11ea-9cf0-0f7b03e09afd.png)


##### 13. Surface Pressure (Pa)
>presion_area <- pp_stack[[6]]
plot(presion_area)

##### 14. Seleccionando puntos el análisis posterior
>puntos <- mapview(canta_dem) %>% 
  editMap()
puntos_sf <- puntos$all
plot(canta_dem,
     main = "Canta")
plot(puntos_sf, add = T)
 ![puntos_cantadem](https://user-images.githubusercontent.com/70491176/93719915-4d867d80-fb4b-11ea-8fc3-4b438611c0c8.png)

##### 15. Agregando las columnas de los datos que acabamos de extraer
>puntos_sf$pp <- raster::extract(pp_area, puntos_sf)

>puntos_sf$Altitud <- raster::extract(canta_dem, puntos_sf)

>puntos_sf$Presión <- raster::extract(presion_area, puntos_sf)

##### 16. Relacionando elementos climáticos
>comparar <- puntos_sf %>%
  as_tibble() %>% 
  dplyr::select(pp, Altitud, Presión)
>comparar 
![tibble_comparar](https://user-images.githubusercontent.com/70491176/93719999-efa66580-fb4b-11ea-89cd-3b9325093082.PNG)


##### Algunos ploteos
>plot(comparar$Altitud, comparar$Presión,
     main = "Altitude vs Surface Pressure",
     ylab = "Surface Pressure(Pa)",
     xlab = " Altitude (m)")
	 
##### pp instantánea
>pp_hour <- ee$ImageCollection("JAXA/GPM_L3/GSMaP/v6/operational")$
  filterDate("2018-08-06", "2018-08-07")$
  first()

>pp2_stack <- ee_as_raster(imag  = ee_pp,
                         region = area_ee$geometry())

>pp_area_hour <- pp2_stack[[1]]

>plot(pp_area_hour)

>mapview(list(pp_area_hour, canta_dem))

##### Albedo instantáneo
>albedo <- ee$ImageCollection("NASA/GLDAS/V021/NOAH/G025/T3H")$
  filterDate("2018-08-06", "2018-08-07")$
  first()

>albedo_stack <- ee_as_raster(imag  = albedo,
                          region = area_ee$geometry())
>albedo_area <- albedo_stack[[1]]

>plot(albedo_area)

>mapview(list(albedo_area, canta_dem))

>puntos_sf$Albedo <- raster::extract(albedo_area, puntos_sf)

>comparar <- puntos_sf %>%
  as_tibble() %>% 
  dplyr::select(pp, Altitud, Presión, Albedo)
  
## Extraer data de Google Earth Engine

>terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2010-01-01","2017-12-31")$
  map(function(x) x$reproject("EPSG:4326")$select("pr"))


### Extraer datos de pp 
>ppt <- ee_extract(terraclimate, canta,
                  fun = ee$Reducer$mean())

### Otro formato 
>ppt_2 <- pivot_longer(ppt, everything(), 
                      names_to = "month",
                      values_to = "pr")

### Crear una columna de fechas
>Fecha <- seq(as.Date("2010-01-01"),
              by = "month",
              length.out = 96)
ppt_2$Fecha <- Fecha

## Gráfica 
>ggplot(ppt_2, aes(fechas, pr))+
  geom_line(col = "blue")+
  ggtitle("Precipitación de Canta")+
  xlab("Fecha")+
  ylab("Precipitación (mm)")
  ![pp_mensual_canta](https://user-images.githubusercontent.com/70491176/93722854-600ab200-fb5f-11ea-9516-ef017e59fc8b.png)

## De mensual a anual
>library(lubridate)  #Esta librería es necesaria para luego hacer el mutate

>ppt_anual <- ppt_2 %>% 
  mutate(year = year(Fecha), month = month(Fecha)) %>% 
  group_by(year) %>% 
  summarize(pr = sum(pr))

## Gráfica de la precipitación anual de Canta

>ggplot(ppt_anual, aes(year, pr))+
  geom_line(col = "blue")+
  ggtitle("Precipitación Anual en Canta (mm)")+
  xlab("Fecha")+
  ylab("Precipitación (mm)")+
  theme_dark()
![pp_anual_canta](https://user-images.githubusercontent.com/70491176/93722856-60a34880-fb5f-11ea-891c-8085b769ab0e.png)
