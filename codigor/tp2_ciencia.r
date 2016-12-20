setwd("ProjTP2Ciencia")
getwd()


install.packages("sp")
install.packages("rworldmap")
install.packages("rworldxtra")
library(sp)
library(rworldmap)
library(rworldxtra)

# install.packages("dplyr") 
library(dplyr)

# install.packages("igraph")
library(igraph)


coords_pais = function(points)
{
  # Una función para recuperar el código ISO 3 de pais de una locación
  countriesSP <- getMap(resolution='high')  # convertir las coordenadas en puntos espaciales
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  # recuperar el poligono-pais de cada punto espacial
  indices = over(pointsSP, countriesSP)# recuperar el nombre de cada pais por código ISO de tres letras
  indices$ISO3
}


#download.file("http://snap.stanford.edu/data/loc-brightkite_edges.txt.gz", destfile = "brkred.gz")
red <- read.table(gzfile("brkred.gz"), header=F,stringsAsFactors = F)

#download.file("http://snap.stanford.edu/data/loc-brightkite_totalCheckins.txt.gz", destfile = "brkchk.gz")
checkins <- read.table(file = gzfile("brkchk.gz"), header=F, fill=T, stringsAsFactors = F)
# el argumento “fill” en el read.table() es para que se puedan leer correctamente los registros con datos faltantes

head(red)
head(checkins)

names(checkins) <- c("id", "time", "lat", "lon", "loc_id")
# igraph no acepta identificadores numéricos para los vértices que tomen valor 0, le sumamos un 1 a todos los IDs
checkins$id <- checkins$id + 1
red$V1 <- red$V1 + 1
red$V2 <- red$V2 + 1

head(red)
head(checkins)
# Sebas que onda un Hash puede tener mas bytes? fijate
# id                 time      lat       lon                                   loc_id
# 1  1 2010-10-17T01:48:53Z 39.74765 -104.9925         88c46bf20db295831bd2d1718ad7e6f5
# 4  1 2010-10-14T18:25:51Z 39.75047 -104.9991 9848afcc62e500a01cf6fbf24b797732f8963683

# Los check-ins con datos faltantes se eliminan

length(which(complete.cases(checkins) == TRUE))
checkins_completos <- checkins[complete.cases(checkins),]

table(checkins_completos$loc_id)
head(table(checkins_completos$loc_id))

head(sort(table(checkins_completos$loc_id), decreasing=T), n=14)

head(checkins_completos[,3:5])
head(checkins_completos[,3:5][checkins_completos$loc_id =="00000000000000000000000000000000",])
head(checkins_completos[,3:5][checkins_completos$loc_id =="00000000000000000000000000000000",],15)

dim(checkins_completos)

# El dataframe de check-ins tiene muchos registros, muchos de ellos repetidos, para
# reducir los tiempos de búsqueda, es conveniente construir un dataframe con
# geolocalizaciones únicas.

dim(unique(checkins_completos[,3:5]))
uniq_checks <- unique(checkins_completos[,3:5])
dim(uniq_checks)

head(uniq_checks)
head(uniq_checks[, 2:1])
dim(uniq_checks)
pais_iso <- coords_pais(uniq_checks[, 2:1])# notar que pasa primero Lon luego Lat
head(pais_iso)
length(pais_iso)
table(pais_iso)

# data.frame(table(pais_iso))
head(sort(table(pais_iso), decreasing=T), 20)
table(pais_iso)["DNK"]
# cristian hizo la tabla sin los checkins unicos y nosotros nos quedamos con Dinamarca

# Ejemplo: construir un subgrafo inducido de los usuarios que hicieron check-ins en la
# Federación Rusa. Primero hay que extraer los usuarios con al menos un check-in en
# Rusia, luego hay que preparar una lista única de usuarios:

head(uniq_checks)
uniq_checks <- data.frame(uniq_checks, ISO3 = as.character(pais_iso)) # le agrego una columna mas al dataframe
head(uniq_checks)


uniq_checks_dinamarca <- filter(uniq_checks, ISO3 == "DNK")
dim(uniq_checks_dinamarca)
head (uniq_checks_dinamarca)
checkins_dinamarca <- inner_join(checkins_completos, uniq_checks_dinamarca[,3:4], by = "loc_id")
head(checkins_dinamarca)
dim(checkins_dinamarca)
# todos los checkins de dinamarca son 7624
# lo checkins de dinamarca unicos es decir que los lugares son 1644
# > dim(uniq_checks_dinamarca)
# [1] 1644    4
# > dim(checkins_dinamarca)
# [1] 7624    6

head(checkins_dinamarca,10)
# fijate que el id 62 se loguea siempre en el mismo loc_id
# y fijate que el id 70 y el 103 se loguearon en distintos lugares 
# y tambien se encontraron en loc_id a0dfd31ac109d0bc0889dd595c81358d0ebc373a
# id                 time      lat       lon                                   loc_id ISO3
# 1   62 2008-07-12T20:31:41Z 56.15674 10.210762         d107b1dea22411dd8530dbbd2e76d061  DNK
# 2   62 2008-06-24T06:27:40Z 56.15674 10.210762         d107b1dea22411dd8530dbbd2e76d061  DNK
# 3   62 2008-06-24T06:25:00Z 56.15674 10.210762         d107b1dea22411dd8530dbbd2e76d061  DNK
# 5   70 2008-06-24T07:21:19Z 55.62053 12.649457 a0dfd31ac109d0bc0889dd595c81358d0ebc373a  DNK
# 6  103 2008-05-31T12:37:15Z 55.62053 12.649457 a0dfd31ac109d0bc0889dd595c81358d0ebc373a  DNK
# 

head(sort(table(checkins_dinamarca$id), decreasing=T), n=15)
# > head(sort(table(checkins_dinamarca$id), decreasing=T), n=15)
# 
# 13380 45217 17059 17054 17077 20033 11552 13587 13239 35486 17088 13375 13592 47759 25689 
# 1162   731   672   283   237   215   202   187   177   133   122   117   105   102    92 

head(sort(table(checkins_dinamarca$id), decreasing=F), n=15)
plot(density(table(checkins_dinamarca$id),bw = 100))
boxplot(table(checkins_dinamarca$id))

# > head(sort(table(checkins_dinamarca$id), decreasing=F), n=15)
# 
# 385  1452  1462  1988  3886  5840  6858  8739  9112 11866 13458 13538 15300 15726 15729 
# 1     1     1     1     1     1     1     1     1     1     1     1     1     1     1 

dim(checkins_dinamarca)
# cuantos seran los de frecuencia 1 o 2
length(which(table(checkins_dinamarca$id) < 4))
# son 201 casos son pocos... vamos bien

# lo unico es el  sitio !!!
uniq_usuarios_dinamarca <- unique(checkins_dinamarca$id)
dim(checkins_dinamarca)
length(uniq_usuarios_dinamarca)
head(uniq_usuarios_dinamarca)
dim(table(checkins_dinamarca$id))



red.amistad <- graph.edgelist(as.matrix(red), directed = F) 
# red es el archivo original que levantamos y tiene las aristas
summary(red.amistad)
red_dinamarca <- induced_subgraph(red.amistad, vids = uniq_usuarios_dinamarca)
# vids	
# Numeric vector, the vertices of the original graph which will form the subgraph.
summary(red_dinamarca)
#Analisis topologico:
ecount(red_dinamarca)
vcount(red_dinamarca)
str(red_dinamarca)
is.simple(red_dinamarca)
is.connected(red_dinamarca)
is.connected(red_dinamarca,mode = "weak")
diameter(red_dinamarca)
get.diameter(red_dinamarca)
graph.density(red_dinamarca)
head(transitivity(red_dinamarca, type = "local"),20)
transitivity(red_dinamarca, type = "global")

# Algunos tips de código
#
walktrap_dinamarca <- walktrap.community(red_dinamarca, steps = 6)
walktrap_dinamarca
names(walktrap_dinamarca)
walktrap_dinamarca$modularity
walktrap_dinamarca$membership

# La mayoría de los clusters tienen un solo miembro porque el grafo no es conectado y
# hay muchos nodos de grado cero:
sort(table(walktrap_dinamarca$membership), decreasing=T)


# Lo podemos ver también con un gráfico:
color_grado <- ifelse(degree(red_dinamarca) > 0, "red", "blue")
plot(red_dinamarca)
red_dinamarca
plot(red_dinamarca, vertex.label = NA, vertex.size = 8, vertex.color = color_grado)
plot(walktrap_dinamarca,red_dinamarca, vertex.label = NA, vertex.size = 8)
