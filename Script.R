######### Packages  #########
packages <- c("readxl", "tseries","dynlm","mFilter","quantmod","forecast",
              "lmtest","ggplot2","ggfortify","vars","lmtest","dplyr",
              "leaflet","Rcpp","geosphere","ggmap","tidyverse",
              "spData","rgeos", "ggmap", "rgdal","maptools", "tidyr","sf",
              "raster", "maps", "sp","spdep","splm","spData","spatialreg",
              "stargazer")
sapply(packages, require, character.only = TRUE) 

### Ubuntu Bionic correr en terminal 
###  sudo apt-get install libgdal-dev gdal-bin libproj-dev proj-data proj-bin libgeos-dev

######### Base con API #########

### Clave de la API

register_google (key = "AIzaSyAq6y5sBAYBjWDwJTa22RUXBrdw0GYWyDk")

### Armamos vectores, id es el identificador para cada provincia

institucion <- c("Universidad Nacional de Córdoba",
                   "Universidad de Buenos Aires",
                   "Universidad Nacional del Litoral",
                   "Universidad Nacional de La Plata",
                   "Universidad Nacional de Tucumán ",
                   "Universidad Nacional de Cuyo",
                   "Universidad Tecnológica Nacional Facultad Regional Córdoba",
                   "Rectorado - Universidad Nacional del Nordeste",
                   "Universidad Nacional del Sur palihue",
                   "Universidad Nacional de Rosario",
                   "Universidad Nacional de La Rioja",
                   "Universidad Nacional de Río Cuarto",
                   "Servicio Social - UNCo - Sede Central",
                   "Universidad Nacional de Catamarca",
                   "Universidad Nacional de Lomas de Zamora",
                   "Universidad Nacional de Luján",
                   "Universidad Nacional de Salta",
                   "Rectorado Universidad Nacional de Entre Ríos",
                   "Rectorado Universidad Nacional de Jujuy",
                   "Universidad Nacional de Misiones",
                   "Rectorado de la Universidad Nacional de San Juan",
                   "Universidad Nacional de San Luis",
                   "Universidad Nacional de Santiago del Estero",
                   "UNLPam - Facultad de Ciencias Exactas y Naturales",
                   "Universidad Nacional de la Patagonia San Juan Bosco - Sede Comodoro Rivadavia",
                   "Universidad Nacional del Centro de la Provincia de Buenos Aires",
                   "Universidad Nacional de Mar del Plata",
                   "Universidad Nacional de Formosa",
                   "Universidad Nacional de La Matanza",
                   "Universidad Nacional de Quilmes",
                   "Universidad Nacional de San Martín - Campus Miguelete",
                   "Universidad Nacional de General Sarmiento (UNGS)",
                   "Universidad Nacional de la Patagonia Austral UNPA - Rectorado",
                   "Universidad Nacional de Lanús",
                   "Universidad Nacional de Tres de Febrero",
                   "Universidad Nacional de Villa María - Rectorado",
                   "UNNOBA - UNIVERSIDAD NACIONAL DEL NOROESTE DE LA PCIA DE BUENOS AIRES",
                   "Universidad Nacional de Chilecito (UNdeC)",
                   "Universidad Nacional del Chaco Austral",
                   "Universidad Nacional de Río Negro",
                   "Universidad Nacional Arturo Jauretche",
                   "Universidad nacional De Avellaneda",
                   "Universidad Nacional de José C. Paz",
                   "Universidad Nacional de Moreno",
                   "Universidad Nacional de Villa Mercedes",
                   "Universidad Nacional del Oeste",
                   "Universidad Nacional de Tierra del Fuego, Antártida e Islas del Atlántico Sur",
                   "UNDEF - Universidad de la Defensa Nacional",
                   "Universidad Nacional de Hurlingham",
                   "UNC - Facultad de Artes - Pabellón México",
                   "Universidad Nacional de los Comechingones",
                   "Universidad Nacional de Rafaela",
                   "Universidad Nacional de San Antonio de Areco",
                   "Universidad Nacional del Alto Uruguay",
                   "Universidad nacional Raúl Scalabrini Ortiz",
                   "Universidad Nacional Guillermo Brown - UNaB",
                   "Universidad Nacional del Sur"
)

### geocode() busca las coordenadas

universidades <- data.frame(institucion, geocode(institucion)); base_popup = popup = institucion

######## Modelo ########

### Mapa para chequear las coordenadas

prov_popup = popup = provincias
provincias %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(popup = prov_popup, clusterOptions = markerClusterOptions())

##panel armado como en el ejemplo
panel <- read_excel("Base.xlsx")
View(panel)

##Matriz de contactos
arg <- raster::getData("GADM", country = "Argentina", level = 1)
summary(arg)
arg_nb <- poly2nb(arg, queen=TRUE)
coords <- coordinates(arg)
plot(arg,border="grey",lwd=1.5)
plot(arg_nb, coordinates(arg), add=TRUE, col="darkred")

#no funciona xq tierra del fuego queda sin vecionos 
#dlist <- nbdists(nbarg, coords)
#idw <- lapply(dlist, function(x) 1/(x/1000))
#nb2listw(nbarg,glist=idw,style="B",zero.policy = TRUE) #nofunciona

##los k=4 vecinos mas cercanos
arg_k <- knn2nb((knearneigh(coords,k=4)))
arg_k
warg <- nb2listw(arg_k)#funciona
plot(arg_k, coordinates(arg), add=TRUE, col="darkred")
summary(arg_k)

#esp_pro_U <- nb2listw(esp_nb,zero.policy=TRUE, style="U")
#esp_pro_S <- nb2listw(esp_nb,zero.policy=TRUE, style="S")
#dsts <-  nbdists(esp_nb, coordinates(esp_pro))
#idw  <-  lapply (dsts, function(x) 1/(x/1000))
#esp_pro_dist <- nb2listw(esp_nb, glist=idw, style="B",zero.policy=TRUE)

##regresion lineal de prueba
names(panel)
reg_multi <- lm(Pbg ~Pob + num_uni, data = panel)
summary(reg_multi)


resid <- reg_multi$residuals[1:24]
length(resid)
####

a <- data(oldcol)
coord.OLD <- cbind(COL.OLD$X, COL.OLD$Y)
moran.test(COL.OLD$CRIME, nb2listw(COL.nb, style = "W"))

#Modelo con datos del panel
# data(boston, package="spData")
# gp2mM <- lagsarlm(log(CMEDV) ~ CRIM + ZN + INDUS + CHAS + I(NOX^2) + 
#                     I(RM^2) +  AGE + log(DIS) + log(RAD) + TAX + PTRATIO + B + log(LSTAT), 
#                   data=boston.c, spdep::nb2listw(boston.soi), type="mixed", method="Matrix")
# summary(gp2mM)
# moran.mc(boston.c$TRACT, spdep::nb2listw(boston.soi))
# 
# 
# moran.mc(resid, warg,999)
# 
# length(resid)
# length(warg)
# 
# 
# m1s <- lagsarlm(eq, data=panel, warg, tol.solve=1.0e-30)]

# # Una alternativa, pero no da el test de moran
# GM1 <- spgm(eq, data=panel, listw = warg,
#             moments="fullweights", spatial.error = TRUE)
# summary(GM1)

eq <- log(Pbg) ~ log(Pob) + num_uni

## Da

test1 <- bsktest(x = eq, data = panel, listw = warg,test = "LM1")
test1

test2 <- sphtest(x = eq, data = panel, listw = warg, spatial.model = "error", method = "GM")
test2

p2 <- spml(eq, data = panel, index = , listw = warg, 
           model = "pooling", lag = TRUE, spatial.error = "b")
p3 <- spml(eq, data = panel, index = , listw = warg, 
           model = "random", lag = FALSE, spatial.error = "kkp")
summary(p2) 
summary(p3)

phtest(eq, data=panel)



