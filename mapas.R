### Directorio de trabajo

#setwd("~/Documents/tesis/modelo_tesis/")

######## Plot  ########

### Invoco los archivos de https://gadm.org/download_country_v3.html

argentina  <- readRDS("gadm36_ARG_0_sf")
argentina2 <- readRDS("gadm36_ARG_1_sf")
argentina3 <- readRDS("gadm36_ARG_2_sf")

### Plot introduciendo la geolocalizacion de las universidades

### Sin delimitar provincias

ggplot(data = argentina) +
  geom_sf(fill = "white", color = "black") +
  xlab("Longitud") + ylab("Latitud")  +
  geom_point(aes(universidades$lon, universidades$lat), 
  alpha=0.8, size=2, color="blue", shape=16, data=universidades) 

### Delimitando provincias

ggplot(data = argentina2) +
  geom_sf(fill = "white", color = "black") +
  xlab("Longitud") + ylab("Latitud")  +
  geom_point(aes(universidades$lon, universidades$lat), 
  alpha=0.8, color="blue", size=2, shape=16, data=universidades)

### Con delimitacion de departamentos

ggplot(data = argentina3) +
  geom_sf(fill = "white", color = "black") +
  xlab("Longitud") + ylab("Latitud")  +
  geom_point(aes(universidades$lon, universidades$lat), 
  alpha=0.8, color="blue", size=2, shape=16, data=universidades) 

### Baires se puede ver que hay más concentracion, plot aparte
### filtramos por departamento en la base argentina3

baires <- argentina3[argentina3$NAME_1=="Buenos Aires",]

### universidades de baires

uni.ba <- c("Universidad Nacional de Mar del Plata", "Universidad Nacional de Quilmes",
            "Universidad Nacional del Centro de la Provincia de Buenos Aires",
            "Universidad Nacional de La Matanza", "Universidad Nacional de Tres de Febrero",
            "Universidad Nacional de San Martín - Campus Miguelete", "Universidad Nacional Arturo Jauretche",
            "Universidad Nacional de Luján", "Universidad Nacional de General Sarmiento (UNGS)",
            "Universidad Nacional del Sur", "Universidad Nacional de Lanús",
            "UNNOBA - UNIVERSIDAD NACIONAL DEL NOROESTE DE LA PCIA DE BUENOS AIRES",
            "Universidad de Buenos Aires", "UNDEF - Universidad de la Defensa Nacional",
            "Universidad Nacional de Lomas de Zamora", "Universidad Nacional de Moreno",
            "Universidad Nacional de José C. Paz")

uni.ba <- data.frame(uni.ba, geocode(uni.ba))

### Plot de baires por departamento
### Plot 

ggplot(data = baires) +
  geom_sf(fill = "white", color = "black") +
  xlab("Longitud") + ylab("Latitud")  +
  geom_point(aes(lon,lat),color="blue", alpha=0.8, size=4, shape=18, data=uni.ba)

