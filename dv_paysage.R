#-------------------------------Calculer les domaines vitaux et leur composition paysagère----------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  30/10/2019
#  modification: 02/02/2021 pour rendre le code plus générique
#  Description: ce script permet, à partir de la base de données de suivi de mouvement de chevreuil db_cefs:
#  1- de sélectionner les localisations GPS de chevreuils:
#                - à partir d'un intervalle d'année de suivi,
#                - d'enlever un certain nombre de jours de suivi après la capture (période pendant laquelle le mouvement des animaux est aberrant généralement 11 jours) 
#                - d'enlever les individus dont le nombre de jour de suivi est jugé insuffisant
#                - de choisir un schedule (i.e. une programmation homogène de l'écart entre deux localisations gps)
#                - d'enlever des disperseurs à partir du fichier de référence de Delphine Ducros Classification_Delphine_12_04_2018.csv
#                  ATTENTION CE FICHIER NE CLASSE LES ANIMAUX QUE JUSQU4EN 2017
#                - De la probabilité de présence pour l'établissement des kernel density.
#                - de retirer les outlayer (methode de Bjøerneraas).
#                - de choisir entre domaine annuel et dommaine mensuel.
#  2- d'installer la couche des domaines vitaux sur la base de données
#  3- de décrire la composition des domaines vitaux en regroupant les informations du parcellaire annuel de la zone d'étude sous trois catégories (humain, fermé, ouvert)
#  n.b: le domaine vital d'un animal peu déborder la zone couverte par le parcellaire annuel. En plus du pourcentage de chacune des trois classes décrites ci-dessus dans le domaine vital de l'individu, le script calcule également le pourcentage de recouvrement du domaine vital avec le relevé parcellaire.
#  Documentation:
#
#
#
#
#
#------------------------------------------------------------------------------
#-------------------------- environnement de travail --------------------------
mypackages<-c("RPostgreSQL", "rgdal","adehabitatHR","lubridate","xts","raster","maptools","foreign","shapefiles","dplyr","sp",'sf')
for (p in mypackages){
if(!require(p, character.only = TRUE)){
install.packages(p)
library(p, character.only = TRUE)
}
}
#-----------------------------------------------------------------------------
#-------------------------- connection aux bases de donnees ------------------
#source("C:/Users/ychaval/Documents/BD_CEFS/con_raspi_dbchevreuils.R")
#source("C:/Users/ychaval/Documents/BD_CEFS/con_raspi_dbchevreuils.R"))
#source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
#source("C:/Users/ychaval/Documents/BD_Gardouch/Programmes/R/con_serveur_dbgardouch.R")
#-------------------------- chargement de mes fonctions ----------------------
devtools::source_url("https://github.com/yannickkk/mes_fonctions/blob/main/fonctions_sans_connect.R?raw=TRUE")

#####mettre son utilisateur name
con<- dbConnect(PostgreSQL(), host="pggeodb.nancy.inra.fr", dbname="db_cefs", utilisateur="xxxx", password="xxxx")

###chemin d'accès jusqu'au fichier de Delphine qui référence les animaux disperseur jusqu'en 2017
setwd("C:/chemin/vers/le/ficher/disperseur")
dat_disp<- read.csv2("Classification_Delphine_12_04_2018.csv", header = TRUE)

###############################
###############################
utilisateur<-"yc"
######choix de l'annee de debut
an_deb<- 2009
an_fin<- 2018
######nombre à enlever après capture
nbre<- 11
######disp est un vecteur contenant ani_etiq+cap_annee_suivi des individus disperseurs census Delphine Ducros
sel<-c("DN") #####sélectionner les disp pour les exclure de l'analyse. Ici disp = DN dans le fichier de Delphine. le vecteur serait c("DN","P") si on veut exclure aussi les philopatriques
#SI ON VEUT TOUT LE MONDE sel<-NA
######nombre de jours de suivi minimum (y compris jours enlevés après le laché)
jours_suivi<- 50 
### pour sélectionner un suivi entre 9 jours et 15 jours choisir nbre<-9 et jours_suivi<- 24 et remplacer ligne 83 par 86
######choix programmation
prog_sel<- 6 ###nombre d'heures ou de minutes si moins d'une heure
######valeur du kernel
kerval<- 90
######"annuel" ou "mensuel"
dv_period<-"annuel"
######chemin pour enregistrer le shape file de domaines vitaux
path<-"C:/Users/ychaval/Desktop"
##############si on ne veut DANS l'analyse que les ani_etiq contenus dans ce vecteur. Si on veut tous les individus vecteur_cap_date<-NA
#vecteur_cap_date<-v2db(c("1000"))
vecteur_cap_date<-NA
############################

##################### recup prog et disperseurs
prog<- grep(prog_sel , t(dbGetQuery(con, "SELECT column_name
                  FROM information_schema.columns
                  WHERE table_schema = 'public'
                  AND table_name   = 't_campagne_pos_cpos'
                  AND column_name ~* 'cpos_prog'")),  value = T)

disp<-paste0("'",as.vector(dat_disp[which(dat_disp[,"C_Delphine"] %in% sel),"cap_bague"]),"'",collapse = ",")
######################NB: les disperseurs seront enlevés du jeux de données
####NOT RUN: 
######create table temporaire.",utilisateur,"_t_select_loctot on localhost which gather the data set###
######locs from public.v_aniposi_gpsgsm having more than 80 locs from 11 days after animal (from 2008 to 2017) release to the end and prog 6 hours) #####
dbSendQuery(con, paste0("DROP TABLE IF EXISTS temporaire.",utilisateur,"_t_select_loc,  temporaire.",utilisateur,"_t_select_loctot;"))
dbSendQuery(con, paste0("create table temporaire.",utilisateur,"_t_select_loc as
SELECT cpos_id, ani_id, ani_etiq, cap_bague, cap_annee_suivi, cap_date,
ani_sexe, cap_age_classe, cap_poids, cap_lpa, sit_nom_court,
teq_nom_court, eqt_id_usuel, mar_libelle, mod_libelle, eqa_date_debut,
eqa_date_fin, ani_mortalite, ani_date_mort, ani_cause_mort, eqa_activite,
eqa_probleme, eqa_date_fin_text, eqa_date_fin_arrondi, ani_date_mort_arrondi,
ani_date_mort_text, ani_poids_mort, ani_poids_mort_na, ani_remarque,
ani_mort_x, ani_mort_y, ani_inconnu, cap_faon, cap_age, cap_age_corrige,
cap_circou, cap_etat_sante, cap_heure_lacher, sit_id, the_geom,
pos_x, pos_y, pos_systeme, pos_z, pos_x_corrige, pos_y_corrige,
pos_z_corrige, pos_nb_sat, pos_dop, pos_dop_corrige, pos_fixstatus,
pos_validated, cpos_date, cpos_heure, cpos_delta,cpos_prog6heure,
cpos_prog4heure, cpos_prog3heure, cpos_prog1heure, cpos_prog10minutes,
date_capture, pos_distance_route, pos_distance_bois, pos_distance_bati,
pos_distance_haie, pos_localisation_par_id, par_os, par_grd_cat,
par_annee
FROM public.v_aniposi_gpsgsm WHERE cap_annee_suivi between ",an_deb," and ",an_fin," and ",prog," is TRUE and cpos_date >= cap_date + ",nbre,";"))
dbSendQuery(con, paste0("DROP TABLE IF EXISTS temporaire.",utilisateur,"_t_select_loctot;"))
if (is.na(vecteur_cap_date)){
dbSendQuery(con, paste0("create table temporaire.",utilisateur,"_t_select_loctot as SELECT    cpos_id, ani_id, ani_etiq, cap_bague, cap_annee_suivi, cap_date,
ani_sexe, cap_age_classe, cap_poids, cap_lpa, sit_nom_court,
teq_nom_court, eqt_id_usuel, mar_libelle, mod_libelle, eqa_date_debut,
eqa_date_fin, ani_mortalite, ani_date_mort, ani_cause_mort, eqa_activite,
eqa_probleme, eqa_date_fin_text, eqa_date_fin_arrondi, ani_date_mort_arrondi,
ani_date_mort_text, ani_poids_mort, ani_poids_mort_na, ani_remarque,
ani_mort_x, ani_mort_y, ani_inconnu, cap_faon, cap_age, cap_age_corrige,
cap_circou, cap_etat_sante, cap_heure_lacher, sit_id, the_geom,
pos_x, pos_y, pos_systeme, pos_z, pos_x_corrige, pos_y_corrige,
pos_z_corrige, pos_nb_sat, pos_dop, pos_dop_corrige, pos_fixstatus,
pos_validated, cpos_date, cpos_heure, cpos_delta,cpos_prog6heure,
cpos_prog4heure, cpos_prog3heure, cpos_prog1heure, cpos_prog10minutes,
date_capture, pos_distance_route, pos_distance_bois, pos_distance_bati,
pos_distance_haie, pos_localisation_par_id, par_os, par_grd_cat, par_annee 
FROM temporaire.",utilisateur,"_t_select_loc 
WHERE concat(ani_etiq,'_', cap_annee_suivi) IN (SELECT concat(ani_etiq,'_', cap_annee_suivi) as etiq_an FROM temporaire.",utilisateur,"_t_select_loc group by etiq_an having count(extract(day from cpos_date)) > ",jours_suivi,") AND cap_bague not in (",disp,");"))
}else{
  dbSendQuery(con, paste0("create table temporaire.",utilisateur,"_t_select_loctot as SELECT    cpos_id, ani_id, ani_etiq, cap_bague, cap_annee_suivi, cap_date,
ani_sexe, cap_age_classe, cap_poids, cap_lpa, sit_nom_court,
teq_nom_court, eqt_id_usuel, mar_libelle, mod_libelle, eqa_date_debut,
eqa_date_fin, ani_mortalite, ani_date_mort, ani_cause_mort, eqa_activite,
eqa_probleme, eqa_date_fin_text, eqa_date_fin_arrondi, ani_date_mort_arrondi,
ani_date_mort_text, ani_poids_mort, ani_poids_mort_na, ani_remarque,
ani_mort_x, ani_mort_y, ani_inconnu, cap_faon, cap_age, cap_age_corrige,
cap_circou, cap_etat_sante, cap_heure_lacher, sit_id, the_geom,
pos_x, pos_y, pos_systeme, pos_z, pos_x_corrige, pos_y_corrige,
pos_z_corrige, pos_nb_sat, pos_dop, pos_dop_corrige, pos_fixstatus,
pos_validated, cpos_date, cpos_heure, cpos_delta,cpos_prog6heure,
cpos_prog4heure, cpos_prog3heure, cpos_prog1heure, cpos_prog10minutes,
date_capture, pos_distance_route, pos_distance_bois, pos_distance_bati,
pos_distance_haie, pos_localisation_par_id, par_os, par_grd_cat, par_annee 
FROM temporaire.",utilisateur,"_t_select_loc 
WHERE concat(ani_etiq,'_', cap_annee_suivi) IN (SELECT concat(ani_etiq,'_', cap_annee_suivi) as etiq_an FROM temporaire.",utilisateur,"_t_select_loc group by etiq_an having count(extract(day from cpos_date)) > ",jours_suivi,") AND ani_etiq in ",v2db(vecteur_cap_date),";"))
}
  
  dbSendQuery(con, paste0("DROP TABLE IF EXISTS temporaire.",utilisateur,"_t_select_loc;"))

#FROM temporaire.",utilisateur,"_t_select_loc WHERE cpos_prog6heure = TRUE AND date_capture between date_capture + ",nbre," AND date_capture +",jours_suivi," AND cap_bague not in (",disp,");"))
#FROM temporaire.",utilisateur,"_t_select_loc WHERE cpos_prog6heure = TRUE AND concat(ani_etiq,'_', cap_annee_suivi) IN (SELECT concat(ani_etiq,'_', cap_annee_suivi) as etiq_an FROM temporaire.",utilisateur,"_t_select_loc group by etiq_an having count(extract(day from cpos_date)) > ",jours_suivi,") AND cap_bague not in (",disp,");"))

####LOAD DATASET #####
#####################################selection des points pour lesquels existent des données de paysage (decoupe sur 2016 car c'est le parcellarie le plus large)
dbSendQuery(con,paste0("Drop table if exists temporaire.",utilisateur,"_result_dv; Create table temporaire.",utilisateur,"_result_dv as SELECT cpos_id, ani_id, ani_etiq, cap_bague, cap_annee_suivi, cap_date, 
                ani_sexe, cap_age_classe, cap_poids, cap_lpa, sit_nom_court, 
                teq_nom_court, eqt_id_usuel, mar_libelle, mod_libelle, eqa_date_debut, 
                eqa_date_fin, ani_mortalite, ani_date_mort, ani_cause_mort, eqa_activite, 
                eqa_probleme, eqa_date_fin_text, eqa_date_fin_arrondi, ani_date_mort_arrondi, 
                ani_date_mort_text, ani_poids_mort, ani_poids_mort_na, ani_remarque, 
                ani_mort_x, ani_mort_y, ani_inconnu, cap_faon, cap_age, cap_age_corrige, 
                cap_circou, cap_etat_sante, cap_heure_lacher, sit_id, ST_transform(the_geom,2154)
                pos_x, pos_y, pos_systeme, pos_z, pos_x_corrige, pos_y_corrige, 
                pos_z_corrige, pos_nb_sat, pos_dop, pos_dop_corrige, pos_fixstatus, 
                pos_validated, cpos_date, cpos_heure, cpos_delta, 
                cpos_prog4heure, cpos_prog3heure, cpos_prog1heure, cpos_prog10minutes, 
                date_capture, pos_distance_route, pos_distance_bois, pos_distance_bati, 
                pos_distance_haie, pos_localisation_par_id, temporaire.",utilisateur,"_t_select_loctot.par_os, temporaire.",utilisateur,"_t_select_loctot.par_grd_cat, 
                temporaire.",utilisateur,"_t_select_loctot.par_annee FROM temporaire.",utilisateur,"_t_select_loctot, tr_parcellaire_par WHERE tr_parcellaire_par.par_annee = '2016' AND the_geom IS NOT NULL AND tr_parcellaire_par.par_grd_cat not in ('non renseigne','hors zone') AND ST_Intersects(ST_transform(the_geom,2154),geom)"))

if (dv_period == "mensuel") { ##chargement donnees dv mensuel
dat<-dbGetQuery(con, paste0("SELECT cpos_id, ani_id, ani_etiq, cap_bague, cap_annee_suivi, cap_date, concat(cap_bague,'_',extract(month from cpos_date)) as cap_bague_mois,
                ani_sexe, cap_age_classe, cap_poids, cap_lpa, sit_nom_court, 
                teq_nom_court, eqt_id_usuel, mar_libelle, mod_libelle, eqa_date_debut, 
                eqa_date_fin, ani_mortalite, ani_date_mort, ani_cause_mort, eqa_activite, 
                eqa_probleme, eqa_date_fin_text, eqa_date_fin_arrondi, ani_date_mort_arrondi, 
                ani_date_mort_text, ani_poids_mort, ani_poids_mort_na, ani_remarque, 
                ani_mort_x, ani_mort_y, ani_inconnu, cap_faon, cap_age, cap_age_corrige, 
                cap_circou, cap_etat_sante, cap_heure_lacher, sit_id,
                pos_x, pos_y, pos_systeme, pos_z, pos_x_corrige, pos_y_corrige, 
                pos_z_corrige, pos_nb_sat, pos_dop, pos_dop_corrige, pos_fixstatus, 
                pos_validated, cpos_date, cpos_heure, cpos_delta, 
                cpos_prog4heure, cpos_prog3heure, cpos_prog1heure, cpos_prog10minutes, 
                date_capture, pos_distance_route, pos_distance_bois, pos_distance_bati, 
                pos_distance_haie, pos_localisation_par_id, temporaire.",utilisateur,"_t_select_loctot.par_os, temporaire.",utilisateur,"_t_select_loctot.par_grd_cat, 
                temporaire.",utilisateur,"_t_select_loctot.par_annee 
                FROM 
                temporaire.",utilisateur,"_t_select_loctot, 
                tr_parcellaire_par 
                WHERE tr_parcellaire_par.par_annee = '2016' AND the_geom IS NOT NULL AND tr_parcellaire_par.par_grd_cat not in ('non renseigne','hors zone') AND ST_Intersects(ST_transform(the_geom,2154),geom)"))
}else{
dat<-dbGetQuery(con, paste0("SELECT cpos_id, ani_id, ani_etiq, cap_bague, cap_annee_suivi, cap_date, 
                ani_sexe, cap_age_classe, cap_poids, cap_lpa, sit_nom_court, 
                teq_nom_court, eqt_id_usuel, mar_libelle, mod_libelle, eqa_date_debut, 
                eqa_date_fin, ani_mortalite, ani_date_mort, ani_cause_mort, eqa_activite, 
                eqa_probleme, eqa_date_fin_text, eqa_date_fin_arrondi, ani_date_mort_arrondi, 
                ani_date_mort_text, ani_poids_mort, ani_poids_mort_na, ani_remarque, 
                ani_mort_x, ani_mort_y, ani_inconnu, cap_faon, cap_age, cap_age_corrige, 
                cap_circou, cap_etat_sante, cap_heure_lacher, sit_id,
                pos_x, pos_y, pos_systeme, pos_z, ST_x(the_geom) as pos_x_corrige, ST_Y(the_geom) as pos_y_corrige, 
                pos_z_corrige, pos_nb_sat, pos_dop, pos_dop_corrige, pos_fixstatus, 
                pos_validated, cpos_date, cpos_heure, cpos_delta, 
                cpos_prog4heure, cpos_prog3heure, cpos_prog1heure, cpos_prog10minutes, 
                date_capture, pos_distance_route, pos_distance_bois, pos_distance_bati, 
                pos_distance_haie, pos_localisation_par_id, temporaire.",utilisateur,"_t_select_loctot.par_os, temporaire.",utilisateur,"_t_select_loctot.par_grd_cat, 
                temporaire.",utilisateur,"_t_select_loctot.par_annee
                FROM temporaire.",utilisateur,"_t_select_loctot, tr_parcellaire_par 
                WHERE tr_parcellaire_par.par_annee = '2016' AND the_geom IS NOT NULL AND tr_parcellaire_par.par_grd_cat not in ('non renseigne','hors zone') AND ST_Intersects(ST_transform(the_geom,2154),geom)"))
}####chargement donnees dv annuel

#dbDisconnect(con)

if (dv_period == "mensuel") {cap<-"cap_bague_mois"} else {cap<-"cap_bague"}
###########################################################################
########################### Outlayers #####################################
###########################################################################
datt<-dat
dat$posx=dat$pos_x_corrige
#dat$posx=ifelse(is.na(dat$posx) | dat$posx==0,dat$pos_x,dat$posx)
dat$posy=dat$pos_y_corrige
#dat$posy=ifelse(is.na(dat$posy) | dat$posy==0,dat$pos_y,dat$posy)
dat$dater=as.POSIXct(strptime(paste(dat$cpos_date,dat$cpos_heure),format="%Y-%m-%d %H:%M:%S",tz="GMT"))

############################################################################
# Retrait des données aberrantes
############################################################################

dataF=dat [is.na(dat$posx)=="FALSE" & is.na(dat$posy)=="FALSE",]#enlever les na
dataF=droplevels(dataF)
dataF$dater=as.POSIXct(strptime(paste(dataF$cpos_date,dataF$cpos_heure),format="%Y-%m-%d %H:%M:%S",tz="GMT"))
traject<- as.ltraj(dataF[,c("posx","posy")],id=dataF$cap_bague, date = dataF$dater)
dataF1=dataF[seq(1,dim(dataF)[1],2),] # selection des lignes impaires uniquement
dataF2=dataF[seq(2,dim(dataF)[1],2),] #selection des lignes paires uniquement
traject1<- as.ltraj(dataF1[,c("posx","posy")],id=dataF1$cap_bague, date = dataF1$dater)
traject2<- as.ltraj(dataF2[,c("posx","posy")],id=dataF2$cap_bague, date = dataF2$dater)

#summary(traject)

toto<-function(traject,traject1,traject2){
  for (i in 1:length(traject)){
    names(traject1[[i]])=c("x","y","date","dx","dy", "dist1","dt","R2n","abs.angle","rel.angle")
    traject11=traject1[[i]]
    names(traject2[[i]])=c("x","y","date","dx","dy", "dist2","dt","R2n","abs.angle","rel.angle")
    traject21=traject2[[i]]
    
    traject[[i]]=merge(traject[[i]],traject11[,c("date","dist1")],by="date",all.x=T,all.y=T)
    traject[[i]]=merge(traject[[i]],traject21[,c("date","dist2")],by="date",all.x=T,all.y=T)
    traject[[i]]$dist13[is.na(traject[[i]]$dist1)=="FALSE"]=traject[[i]]$dist1[is.na(traject[[i]]$dist1)=="FALSE"]
    traject[[i]]$dist13[is.na(traject[[i]]$dist2)=="FALSE"]=traject[[i]]$dist2[is.na(traject[[i]]$dist2)=="FALSE"]
    traject[[i]]$dist23=NA
    traject[[i]]$dist23[1:(nrow(traject[[i]])-1)]=traject[[i]]$dist[2:nrow(traject[[i]])]
    traject[[i]]$relangle1=NA
    traject[[i]]$relangle1[1:(nrow(traject[[i]])-1)]=traject[[i]]$rel.angle[2:(nrow(traject[[i]]))]
    
    
    traject[[i]]$pb=0
    traject[[i]]$pb[(((traject[[i]]$dist>2*traject[[i]]$dist13 & traject[[i]]$dt > 3800) |
                        (traject[[i]]$dist>6*traject[[i]]$dist13 & traject[[i]]$dt <= 900) |
                        (traject[[i]]$dist>3*traject[[i]]$dist13 & traject[[i]]$dt > 900 & traject[[i]]$dt <= 3800)) &
                       ((traject[[i]]$dist/traject[[i]]$dt > (1000/3600) & traject[[i]]$dt > 3800) |
                          (traject[[i]]$dist/traject[[i]]$dt > 2 & traject[[i]]$dt <= 900) | 
                          (traject[[i]]$dist/traject[[i]]$dt > 2 & traject[[i]]$dt > 900 & traject[[i]]$dt <= 3800)) &
                       abs(traject[[i]]$relangle1)>2.5) | (traject[[i]]$dist>4000 & traject[[i]]$dist23>4000) |
                      (traject[[i]]$dist>2000 & traject[[i]]$dist23>2000 & abs(traject[[i]]$relangle1)>2.5)]=1
    
    traject[[i]]$pb2=0
    traject[[i]]$pb2[2:nrow(traject[[i]])]=traject[[i]]$pb[1:(nrow(traject[[i]])-1)]
    traject[[i]]$pb2[traject[[i]]$x==0]=1
  }    
  return(traject)
}
if (length(which(summary(traject)[,"nb.reloc"] <= 10)) > 0) {enl<-summary(traject[which(summary(traject)[,"nb.reloc"] <= 10)])$id
traject<-traject[-which(summary(traject)[,"nb.reloc"] <= 10)]} else {enl<-NULL}
if (length(which(summary(traject1)[,"nb.reloc"] <= 5))) {
traject1<-traject1[-which(summary(traject1)[,"nb.reloc"] <= 5)]}
if (length(which(summary(traject2)[,"nb.reloc"] <= 5))) {
traject2<-traject2[-which(summary(traject2)[,"nb.reloc"] <= 5)]}
cap_bague = unique(unlist(lapply(traject, attr, which = "id")))
voir=toto(traject,traject1,traject2) #Met un 1 pour les valeurs qui posent pb selon les conditions énoncées dans traject[[i]]$pb
# rajout des identifiants des anx

new=function (traject,id) {
  for (i in 1:length(traject)){
    traject[[i]]$cap_bague=id[i]
    if (i==1) essai=traject[[i]]
    if (i>1) essai=rbind(essai,traject[[i]])
  }
  return(essai)
}


traj=new(voir,cap_bague)


print(paste("nombre d'outlayers: ",length(which(traj$pb != 0)) + length(which(traj$pb2 != 0))))
# dat$pos_x_corrige<-ifelse(is.na(dat$pos_x_corrige) | dat$pos_x_corrige==0,dat$pos_x,dat$pos_x_corrige)
# dat$pos_y_corrige<-ifelse(is.na(dat$pos_y_corrige) | dat$pos_y_corrige==0,dat$pos_y,dat$pos_y_corrige)
outlayers<-dat[append(which(traj$pb1 != 0),which(traj$pb2 != 0)),]
if(dim(outlayers)[1]>0) {dat<-dat[-append(which(traj$pb1 != 0),which(traj$pb2 != 0)),]}
#####on rend le dataframe spatial et on le reprojette de lambert III en Lambert 93 #####

if (length(enl) > 0) {dats<-dat[-which(dat[,cap] %in% enl),]} else {dats<-dat}
coordinates(dats) <- c("pos_x_corrige","pos_y_corrige")
#crs.LIII<-CRS(SRS_string='EPSG:4326')#27583 "+proj=lcc +lat_1=44.10000000000001 +lat_0=44.10000000000001 +lon_0=0 +k_0=0.999877499 +x_0=600000 +y_0=3200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs ") #EPSG:27583") 
# wkt <- sf::st_crs(27573)[[2]]
# crs.LIII<-sp::CRS(wkt)
#crs.L93<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") #"+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
wkt <- sf::st_crs(2154)[[2]]
crs.L93<-sp::CRS(wkt)
proj4string(dats) <- crs.L93
#dats<-spTransform(dats, crs.L93)
if (length(enl) > 0) {dat[-which(dat[,cap] %in% enl),c("pos_x_corrige","pos_y_corrige")] <- dats@coords} else {dat[,c("pos_x_corrige","pos_y_corrige")] <- dats@coords }###replace localisations by reprojected ones 


   #domaines vitaux par mois
    if (dv_period == "mensuel") {sell<- c("cap_bague_mois","pos_x_corrige","pos_y_corrige");cap<-"cap_bague_mois"} else {sell<- c("cap_bague","pos_x_corrige","pos_y_corrige");cap<-"cap_bague"}
    datker=droplevels(subset(dat,select= sell))
    #tt=as.data.frame(table(datker$cap_bague_mois)[table(datker$cap_bague_mois)<10]);tt ## les individus dans tt on moins de 10 locs, a supprimer
    #datker=droplevels(subset(datker,!cap_bague_mois%in% levels(as.factor(tt$Var1))))
    datkers<-split(datker,as.factor(datker[,cap]))###partition du jeu de données par domaine vital à calculer
    verti<-NA
    for (i in 1:length(unique(datker$cap))){    
    datkerb<-datkers[[i]]
    coordinates(datkerb)=~pos_x_corrige+pos_y_corrige
    wkt <- sf::st_crs(2154)[[2]]
    crs.L93<-sp::CRS(wkt)
    proj4string(datkerb) <- crs.L93
    ker<-kernelUD(datkerb, h = "href", kern = "bivnorm", extent = 5, grid= 500)
    vert<-getverticeshr(ker, kerval)
    verti<-append(verti,vert)
    }
 verti<-verti[2:length(verti)]
 if (length(verti)>1) {verti<- do.call(rbind, verti)} else {verti<-verti[[1]]}
 is.projected(verti)

setwd(path)
writeOGR(verti,"Domaines_vitaux",layer=paste0(date(Sys.time()),"_dv"), overwrite_layer=T, driver="ESRI Shapefile")

setwd(paste0(path,"/Domaines_vitaux"))
 dbf<-read.dbf(paste0(date(Sys.time()),"_dv.dbf"))
 dbf$dbf[,"hdv_year"]<- paste0("20",sapply(strsplit(as.character(dbf$dbf[,"id"]),"_"),"[[",2))
 names(dbf$dbf)[1]<- "names"
 shell(paste0("DEL ",paste0(date(Sys.time()),"_dv.dbf"),""))
 write.dbf(dbf,paste0(date(Sys.time()),"_dv.dbf"))

 ###########attention ligne 269 il faut renommer le 2018-04-25_dv.shp avec le nom donné par la commande de la ligne 260, changer le chemin d'accès au fichier et remplacer les XXXX par votre utilisateur name
 ############################################
 dbSendQuery(con, paste0("drop table if exists temporaire.",utilisateur,"_dv;"))
 setwd("C:/Program Files/PostgreSQL/12/bin/")
 ##############Installer postgresl (avec l'extension postgis) et exécuter: 
paste0("\"C:/Program Files/PostgreSQL/12/bin/shp2pgsql.exe\" -s 2154 -I \"",paste0(path,"/Domaines_vitaux"),"/",grep("shp",list.files(paste0(path,"/Domaines_vitaux")), value =TRUE),"\" temporaire.",utilisateur,"_dv | \"C:/Program Files/PostgreSQL/12/bin/psql.exe\" -p 5432 -h pggeodb.nancy.inra.fr -d db_cefs -U xxxxx")
#######une fois exécuter copier le résultat en enlevant les antislash (\) ainsi que les " en début et en fin de ligne pour avoir quelque chose comme ça:
#"C:/Program Files/PostgreSQL/12/bin/shp2pgsql.exe" -s 2154 -I "C:/Users/ychaval/Desktop/Domaines_vitaux/2021-04-02_dv.shp" temporaire.yc_dv | "C:/Program Files/PostgreSQL/12/bin/psql.exe" -p 5432 -h pggeodb.nancy.inra.fr -d db_cefs -U xxxx
#copier la ligne et coller là dans l'invite de commande windows, lancer, votre password sera demandé

dbSendQuery(con, paste0("drop table if exists temporaire.",utilisateur,"_dv_u;"))
dbSendQuery(con, paste0("create table temporaire.",utilisateur,"_dv_u as select names,hdv_year, ST_UNION(geom) as geom_u from temporaire.",utilisateur,"_dv group by names, hdv_year;"))
dbSendQuery(con, paste0("drop table if exists temporaire.",utilisateur,"_t_hab_dv_hdv;"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_dv_u ADD COLUMN hdv_id serial"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_dv_u ADD COLUMN id serial"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_dv_u ADD COLUMN hdv_area serial"))
dbSendQuery(con, paste0("update temporaire.",utilisateur,"_dv_u set hdv_area = round(ST_AREA (geom_u))"))
dbSendQuery(con, paste0("drop table if exists temporaire.",utilisateur,"_t_hab_dv_hdv;"))
dbSendQuery(con, paste0("create table temporaire.",utilisateur,"_t_hab_dv_hdv AS SELECT hdv_id, names AS hdv_cap_bague, hdv_year, hdv_area, geom_u as hdv_geom from temporaire.",utilisateur,"_dv_u"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv ADD CONSTRAINT t_hab_dv_hdv_graphpkey PRIMARY KEY (hdv_id);"))
dbSendQuery(con, paste0("drop INDEX if exists temporaire.",utilisateur,"_t_hab_dv_hdv_graphgeom_gist cascade;"))
dbSendQuery(con, paste0("CREATE INDEX if not exists t_hab_dv_hdv_graphgeom_gist ON temporaire.",utilisateur,"_t_hab_dv_hdv USING gist (hdv_geom);"))


dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv DROP COLUMN IF EXISTS hdv_surf_humain"))   
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv DROP COLUMN IF EXISTS hdv_surf_cultures"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv DROP COLUMN IF EXISTS hdv_surf_bois"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv ADD COLUMN hdv_surf_humain text"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv ADD COLUMN hdv_surf_cultures text"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv ADD COLUMN hdv_surf_bois text"))

dbSendQuery(con, paste0("COMMENT ON COLUMN temporaire.",utilisateur,"_t_hab_dv_hdv.hdv_surf_humain IS 'bati,chemin,chemin prive,dependance ferme,depot chantier,enclos,jardin,parking,route\'"))
dbSendQuery(con, paste0("COMMENT ON COLUMN temporaire.",utilisateur,"_t_hab_dv_hdv.hdv_surf_cultures IS 'jachere,legumineuse,lin,luzerne,maraichage,moutarde,autre culture,oleoproteagineux,parc arbore,pelouse,polygonacee,prairie,prairie artificielle,prairie naturelle,soja,sorgho,stade foot,terre,tournesol,trefle,trefle+luzerne,verger,vigne,cereale,cereale+colza,cereale+prairie,chanvre,bande enherbe,colza,culture,eau,feverole\'"))
dbSendQuery(con, paste0("COMMENT ON COLUMN temporaire.",utilisateur,"_t_hab_dv_hdv.hdv_surf_bois IS 'bois,haie,friche\'"))
#dbSendQuery(con, "COMMENT ON COLUMN temporaire.",utilisateur,"_t_hab_dv_hdv.hdv_surf_buffer IS 'surface du buffer\'")

#####CORRECTION DU PARCELLAIRE
#dbSendQuery(con, "UPDATE tr_parcellaire_par SET geom = ST_Buffer(ST_makevalid(tr_parcellaire_par.geom),0)")
#######ici on calcule la surface de chaque classe consideree (humain, culture, bois) pour la taille de buffer donnee que l'on ajoute ? la table t_hab_dv_hdv
dbSendQuery(con, paste0("UPDATE temporaire.",utilisateur,"_t_hab_dv_hdv SET  hdv_surf_humain = surf_hab_humain FROM
                       (
                       SELECT hdv_cap_bague, hdv_year, round(SUM(ST_Area(ST_Intersection(hdv_geom, tr_parcellaire_par.geom)))) as surf_hab_humain
                       from
                       tr_parcellaire_par,
                       temporaire.",utilisateur,"_t_hab_dv_hdv
                       WHERE par_grd_cat in ('bati','chemin','chemin prive','dependance ferme','depot chantier','enclos','jardin','parking','route')
                       and
                       tr_parcellaire_par.par_annee = hdv_year::integer AND
                       ST_intersects(hdv_geom, tr_parcellaire_par.geom)
                       group by hdv_cap_bague, hdv_year --, par_id
                       ORDER BY hdv_cap_bague) as foo
                       where concat(foo.hdv_cap_bague,foo.hdv_year) = concat(temporaire.",utilisateur,"_t_hab_dv_hdv.hdv_cap_bague,temporaire.",utilisateur,"_t_hab_dv_hdv.hdv_year);"))

dbSendQuery(con, paste0("UPDATE temporaire.",utilisateur,"_t_hab_dv_hdv SET   hdv_surf_cultures = surf_cultures FROM
                       (
                       SELECT hdv_cap_bague, hdv_year, round(SUM(ST_Area(ST_Intersection(hdv_geom, tr_parcellaire_par.geom)))) as surf_cultures
                       from
                       tr_parcellaire_par,
                       temporaire.",utilisateur,"_t_hab_dv_hdv
                       WHERE par_grd_cat in ('jachere','legumineuse','lin','luzerne','maraichage','moutarde','oleoproteagineux','parc arbore','pelouse','polygonacee','prairie','prairie artificielle','prairie naturelle','soja','sorgho','stade foot','terre','tournesol','trefle','trefle+luzerne','verger','vigne','cereale','cereale+colza','cereale+prairie','chanvre','bande enherbe','colza','culture','feverole')
                       and
                       tr_parcellaire_par.par_annee = hdv_year::integer AND
                       ST_intersects(hdv_geom, tr_parcellaire_par.geom)
                       group by hdv_cap_bague, hdv_year --, par_id
                       ORDER BY hdv_cap_bague) as foo
                       where concat(foo.hdv_cap_bague,foo.hdv_year) = concat(temporaire.",utilisateur,"_t_hab_dv_hdv.hdv_cap_bague,temporaire.",utilisateur,"_t_hab_dv_hdv.hdv_year);"))

dbSendQuery(con, paste0("UPDATE temporaire.",utilisateur,"_t_hab_dv_hdv SET  hdv_surf_bois = surf_hab_bois FROM
                       (
                       SELECT hdv_cap_bague, hdv_year, round(SUM(ST_Area(ST_Intersection(hdv_geom, tr_parcellaire_par.geom)))) as surf_hab_bois
                       from
                       tr_parcellaire_par,
                       temporaire.",utilisateur,"_t_hab_dv_hdv
                       WHERE par_grd_cat in ('bois','haie','friche')
                       and
                       tr_parcellaire_par.par_annee = hdv_year::integer AND
                       ST_intersects(hdv_geom, tr_parcellaire_par.geom)
                       group by hdv_cap_bague, hdv_year --, par_id
                       ORDER BY hdv_cap_bague) as foo
                       where concat(foo.hdv_cap_bague,foo.hdv_year) = concat(temporaire.",utilisateur,"_t_hab_dv_hdv.hdv_cap_bague,temporaire.",utilisateur,"_t_hab_dv_hdv.hdv_year);"))



######on cree une couche graphique de l'intersect pour le rayon donn?e avec l'ensemble des grd_cat
dbSendQuery(con, paste0("DROP TABLE IF EXISTS temporaire.",utilisateur,"_t_hab_dv_hdv_graph;"))
dbSendQuery(con, paste0("CREATE TABLE temporaire.",utilisateur,"_t_hab_dv_hdv_graph as select hdv_id, hdv_cap_bague, hdv_year, par_grd_cat, ST_Intersection(hdv_geom, tr_parcellaire_par.geom) as geom from temporaire.",utilisateur,"_t_hab_dv_hdv, tr_parcellaire_par where tr_parcellaire_par.par_annee = hdv_year::integer and ST_Intersects(hdv_geom, tr_parcellaire_par.geom) --AND par_id not in ('254687','169902','259170','170673','134160','188227','254756','170479','170436','170045','169977')"))

######on cree une couche graphique qui regroupe les geom des grd_cat en humain, culture, bois 
dbSendQuery(con, paste0("DROP TABLE IF EXISTS temporaire.",utilisateur,"_t_hab_dv_hdv_graph_grps ;"))
dbSendQuery(con, paste0("CREATE TABLE temporaire.",utilisateur,"_t_hab_dv_hdv_graph_grps AS (
                       SELECT hdv_id, hdv_cap_bague, hdv_year, MAX(par_grd_cat) AS grd_cat, ST_Union(ST_MakeValid(ST_SnapToGrid(geom, 0.0001))) as geom --ST_Union(geom) AS geom --ST_Collect(geom) AS geom--
                       FROM temporaire.",utilisateur,"_t_hab_dv_hdv_graph
                       WHERE par_grd_cat in ('bati','chemin','chemin prive','dependance ferme','depot chantier','enclos','jardin','parking','route')
                       GROUP BY hdv_id, hdv_cap_bague, hdv_year
                       UNION
                       SELECT hdv_id, hdv_cap_bague, hdv_year, MAX(par_grd_cat) AS grd_cat, ST_Union(ST_MakeValid(ST_SnapToGrid(geom, 0.0001))) as geom  --ST_Union(geom) AS geom --ST_Collect(geom) AS geom --
                       FROM temporaire.",utilisateur,"_t_hab_dv_hdv_graph
                       WHERE par_grd_cat in ('jachere','legumineuse','lin','luzerne','maraichage','moutarde','oleoproteagineux','parc arbore','pelouse','polygonacee','prairie','prairie artificielle','prairie naturelle','soja','sorgho','stade foot','terre','tournesol','trefle','trefle+luzerne','verger','vigne','cereale','cereale+colza','cereale+prairie','chanvre','bande enherbe','colza','culture','feverole')
                       GROUP BY hdv_id, hdv_cap_bague, hdv_year
                       UNION
                       SELECT hdv_id, hdv_cap_bague, hdv_year, MAX(par_grd_cat) AS grd_cat,   ST_Union(ST_MakeValid(ST_SnapToGrid(geom, 0.0001))) as geom   --ST_Union(geom) AS geom --ST_Collect(geom) AS geom --
                       FROM temporaire.",utilisateur,"_t_hab_dv_hdv_graph
                       WHERE par_grd_cat in ('bois','haie','friche')
                       GROUP BY hdv_id, hdv_cap_bague, hdv_year
                       order by hdv_cap_bague, hdv_year ASC);"))

dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv_graph_grps ADD COLUMN hdv_id_grps serial;"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv_graph_grps ADD CONSTRAINT t_hab_dv_hdv_graph_grps_pkey PRIMARY KEY (hdv_id_grps)"))

dbSendQuery(con, paste0("UPDATE temporaire.",utilisateur,"_t_hab_dv_hdv_graph_grps SET grd_cat = 'humain' where grd_cat in ('bati','chemin','chemin prive','dependance ferme','depot chantier','enclos','jardin','parking','route');"))
dbSendQuery(con, paste0("UPDATE temporaire.",utilisateur,"_t_hab_dv_hdv_graph_grps SET grd_cat = 'culture' where grd_cat in ('jachere','legumineuse','lin','luzerne','maraichage','moutarde','oleoproteagineux','parc arbore','pelouse','polygonacee','prairie','prairie artificielle','prairie naturelle','soja','sorgho','stade foot','terre','tournesol','trefle','trefle+luzerne','verger','vigne','cereale','cereale+colza','cereale+prairie','chanvre','bande enherbe','colza','culture','feverole');"))
dbSendQuery(con, paste0("UPDATE temporaire.",utilisateur,"_t_hab_dv_hdv_graph_grps SET grd_cat = 'bois' where grd_cat in ('bois','haie','friche');"))
dbSendQuery(con, paste0("DROP INDEX IF EXISTS t_hab_dv_hdv_graph_grps_index1; DROP INDEX IF EXISTS t_hab_dv_hdv_graph_grps_index2;"))
dbSendQuery(con, paste0("CREATE INDEX  if not exists t_hab_dv_hdv_graph_grps_index1 ON temporaire.",utilisateur,"_t_hab_dv_hdv_graph_grps USING gist (geom); CREATE INDEX  if not exists t_hab_dv_hdv_graph_grps_index2 ON temporaire.",utilisateur,"_t_hab_dv_hdv_graph_grps (grd_cat);"))
dbSendQuery(con, paste0("DROP INDEX IF EXISTS t_hab_dv_hdv_graph_index1;DROP INDEX IF EXISTS t_hab_dv_hdv_graph_grps_index2;"))
dbSendQuery(con, paste0("CREATE INDEX  if not exists t_hab_dv_hdv_graph_index1 ON temporaire.",utilisateur,"_t_hab_dv_hdv_graph USING gist (geom);CREATE INDEX  if not exists t_hab_dv_hdv_graph_grps_index2 ON temporaire.",utilisateur,"_t_hab_dv_hdv_graph (par_grd_cat);"))

#####"pourcentage de couverture de l'assolement
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv add column hdv_hab_dv_percentcov text"))
dbSendQuery(con, paste0("UPDATE temporaire.",utilisateur,"_t_hab_dv_hdv set hdv_surf_humain = 0 where hdv_surf_humain is null"))
dbSendQuery(con, paste0("UPDATE temporaire.",utilisateur,"_t_hab_dv_hdv set hdv_surf_cultures = 0 where hdv_surf_cultures is null"))
dbSendQuery(con, paste0("UPDATE temporaire.",utilisateur,"_t_hab_dv_hdv set hdv_surf_bois = 0 where hdv_surf_bois is null"))
dbSendQuery(con, paste0("UPDATE temporaire.",utilisateur,"_t_hab_dv_hdv set hdv_hab_dv_percentcov = round(100*(hdv_surf_humain::real + hdv_surf_cultures::real + hdv_surf_bois::real)/(hdv_area))"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv add column hdv_cap_id integer"))
dbSendQuery(con, paste0("UPDATE temporaire.",utilisateur,"_t_hab_dv_hdv set hdv_cap_id = toto.cap_id from (SELECT cap_id,cap_bague, ani_etiq, cap_annee_suivi FROM public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id) as toto where toto.cap_bague = hdv_cap_bague and hdv_year::integer = toto.cap_annee_suivi ;"))
# dbSendQuery(con, "ALTER TABLE temporaire.",utilisateur,"_t_hab_dv_hdv add CONSTRAINT hdv_fk FOREIGN KEY (hdv_cap_bague)
#       REFERENCES public.t_capture_cap (cap_bague) MATCH SIMPLE
#             ON UPDATE NO ACTION ON DELETE NO ACTION")
dbSendQuery(con, paste0("Comment on table temporaire.",utilisateur,"_t_hab_dv_hdv IS 'Pour ",utilisateur," table d''intersection entre domaine vitaux des adultes et yearling (kernel90, href par d?faut) et l''assolement correspondant a l''annee de suivi'"))
dbSendQuery(con, paste0("Comment on table temporaire.",utilisateur,"_t_hab_dv_hdv IS 'Pour ",utilisateur," table de visualisation de l''intersect entre les domaines vitaux annuels (kernel90, href par defaut) des adultes et yearling et les assolements correspondant a l''annee de suivi simplifie en milieu boises, urbain et cultures'"))

############on renomme les tables pour qu'elles contiennent la valeur du kernel
if (dv_period == "mensuel") {
dbSendQuery(con, paste0("alter table temporaire.",utilisateur,"_t_hab_dv_hdv rename to ",utilisateur,"_t_hab_dv_ker",kerval,"_hdv_month"))
dbSendQuery(con, paste0("alter table temporaire.",utilisateur,"_t_hab_dv_hdv_graph rename to ",utilisateur,"_t_hab_dv_ker",kerval,"_hdvv_graph_month"))
dbSendQuery(con, paste0("alter table temporaire.",utilisateur,"_t_hab_dv_hdv_graph_grps rename to ",utilisateur,"_t_hab_dv_ker",kerval,"_hdvv_graph_grps_month"))
dbSendQuery(con, paste0("alter table temporaire.",utilisateur,"_dv rename to ",utilisateur,"_dv_ker",kerval,"_month"))
dbSendQuery(con, paste0("alter table temporaire.",utilisateur,"_dv_u rename to ",utilisateur,"_dv_u_ker",kerval,"_month"))
}else{
  dbSendQuery(con, paste0("alter table temporaire.",utilisateur,"_t_hab_dv_hdv rename to ",utilisateur,"_t_hab_dv_ker",kerval,"_hdv_year"))
  dbSendQuery(con, paste0("alter table temporaire.",utilisateur,"_t_hab_dv_hdv_graph rename to ",utilisateur,"_t_hab_dv_ker",kerval,"_hdvv_graph_year"))
  dbSendQuery(con, paste0("alter table temporaire.",utilisateur,"_t_hab_dv_hdv_graph_grps rename to ",utilisateur,"_t_hab_dv_ker",kerval,"_hdvv_graph_grps_year"))
  dbSendQuery(con, paste0("alter table temporaire.",utilisateur,"_dv rename to ",utilisateur,"_dv_ker",kerval,"_year"))
  dbSendQuery(con, paste0("alter table temporaire.",utilisateur,"_dv_u rename to ",utilisateur,"_dv_u_ker",kerval,"_year"))
}  
  

####Grant privilege to utilisateurs######
schem<- c("analyse")
for (j in 1:length(schem)){
  tab<-dbGetQuery(con,paste0("SELECT table_name
                            FROM information_schema.tables
                            WHERE table_schema='",schem[j],"'
                            AND table_type='BASE TABLE';"))
  tab<-tab[c(1:2),1]
  test<-dbGetQuery(con,paste0("SELECT table_name
                             FROM information_schema.views
                             WHERE table_schema='",schem[j],"'
                             "))
  
  seq<-dbGetQuery(con,paste0("SELECT sequence_name
                            FROM information_schema.sequences
                            WHERE sequence_schema='",schem[j],"'
                            ;"))
  
  
  if (dim(test)[1]!= 0)  {view<-dbGetQuery(con,paste0("SELECT table_name
                                                     FROM information_schema.views
                                                     WHERE table_schema='",schem[j],"'
                                                     ;"))}
  
  
  schem<- c("temporaire")
  
  if (length(tab) > 0) {for (i in 1:length(t(tab))){
    dbSendQuery(con, paste0("ALTER TABLE ",schem[j],".",t(tab)[i],"
                           OWNER TO cefs_admin;
                           GRANT ALL ON TABLE ",schem[j],".",t(tab)[i]," TO ychaval;
                           GRANT SELECT ON TABLE ",schem[j],".",t(tab)[i]," TO cefs_lecture;
                           GRANT ALL ON TABLE ",schem[j],".",t(tab)[i]," TO cefs_ecriture;"))}}
  
  if (length(seq) > 0) {for (i in 1:length(t(seq))){
    dbSendQuery(con, paste0("ALTER SEQUENCE ",schem[j],".",t(seq)[i],"
                           OWNER TO cefs_admin;
                           GRANT ALL ON SEQUENCE ",schem[j],".",t(seq)[i]," TO ychaval;
                           GRANT SELECT ON SEQUENCE ",schem[j],".",t(seq)[i]," TO mov_it_lecture;
                           GRANT ALL ON SEQUENCE ",schem[j],".",t(seq)[i]," TO mov_it_ecriture;"))}}
  
  
  if (dim(test)[1]!= 0) {if (length(view) > 0) {for (i in 1:length(t(view))){
    dbSendQuery(con, paste0("ALTER TABLE ",schem[j],".",t(view)[i],"
                           OWNER TO cefs_admin;
                           GRANT ALL ON TABLE ",schem[j],".",t(view)[i]," TO ychaval;
                           GRANT SELECT ON TABLE ",schem[j],".",t(view)[i]," cefs_lecture;
                           GRANT ALL ON TABLE ",schem[j],".",t(view)[i]," TO cefs_ecriture;"))}}}
  
  dbSendQuery(con, paste0("GRANT ALL ON SCHEMA ",schem[j]," TO cefs_admin;
                         GRANT ALL ON SCHEMA ",schem[j]," TO public;"))}
dbDisconnect(con)
