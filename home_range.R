############################################################################
# Environnement de travail
############################################################################
setwd("D:\projets\MoveIt")
library(adehabitatLT)
library(adehabitatHR)
library(Hmisc)
library(rgdal)
library(RPostgreSQL)
library(stringr)
library(shapefiles)
library(sf)

############################################################################
# Import data
############################################################################
#femelle=read.csv("femelles.csv",sep=";",header=T)

### connect to server
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="db_cefs", host="pggeodb.nancy.inra.fr", user="xxxx", password="xxxx")

### example from db_cefs
animals<- c("504","F572","542")
animals<-paste0("('",paste0(animals,collapse ="','"),"')")

spatial <- dbGetQuery(con, paste0("SELECT ani_etiq, cap_bague, cap_annee_suivi, cap_date,
                      ani_sexe, cap_age_classe, cap_poids, cap_lpa, sit_nom_court,
                      pos_x, pos_y, pos_systeme, pos_x_corrige, pos_y_corrige,
                      cpos_date, cpos_heure, cpos_prog6heure,cpos_prog1heure
                      FROM public.v_aniposi_gpsgsm
                      where ani_etiq in ",animals,";"))

spatial$ani_etiq=as.factor(spatial$ani_etiq)
spatial$cap_bague=as.factor(spatial$cap_bague)
spatial$ani_sexe=as.factor(spatial$ani_sexe)
spatial$cap_age_classe=as.factor(spatial$cap_age_classe)
spatial$sit_nom_court=as.factor(spatial$sit_nom_court)
spatial$pos_systeme=as.factor(spatial$pos_systeme)

spatial$dater <- as.POSIXlt(strptime(paste(spatial$cpos_date, spatial$cpos_heure), "%Y-%m-%d %H:%M:%S"), tz = "UTC")
spatial$posx=ifelse(is.na(spatial$pos_x_corrige)=="FALSE",spatial$pos_x_corrige,spatial$pos_x)
spatial$posy=ifelse(is.na(spatial$pos_y_corrige)=="FALSE",spatial$pos_y_corrige,spatial$pos_y)
spat=subset(spatial, is.na(posx)=="FALSE" & cpos_prog6heure=="TRUE")

coordinates(spat) <- c("posx","posy")
#crs.LIII<-CRS("+init=epsg:27583")
wkt <- sf::st_crs(27583)[[2]]
crs.LIII <-sp::CRS(wkt)
#crs.L93<-CRS("+init=epsg:2154")
wkt <- sf::st_crs(2154)[[2]]
crs.L93 <-sp::CRS(wkt)

proj4string(spat) <- crs.LIII
spat<-spTransform(spat, crs.L93)

#####HOME RANGE
#####enter the value of percent level parameter per
per<- 90 
ker<-kernelUD(spat["cap_bague"], h = "href", kern = "bivnorm", grid= 500, same4all = FALSE)

kernel.area(ker, percent = per, unin = c("m"), unout = c("ha"))
vert=getverticeshr(ker, percent = per, unin = c("m"),unout = c("ha"))

writeOGR(vert,"Outputs",layer="hr", overwrite_layer=T, driver="ESRI Shapefile")

####à lancer en ligne de commande windows, il faut avoir postgresql intallé sur son ordinateur avec l'extension postgis
"C:\Program Files\PostgreSQL\12\bin\shp2pgsql.exe" -s 2154 -I D:\projets\MoveIt\Outputs\nm_dv.shp temporaire.dvnm | "C:\Program Files\PostgreSQL\12\bin\psql.exe" -p 5432 -h pggeodb.nancy.inra.fr -d db_cefs -U xxxx



