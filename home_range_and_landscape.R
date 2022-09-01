############################Intersect home range and landscape###########################################
#### author: Yannick Chaval, INRA (French National Institute for Agricultural Research), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#### date: 17-10-2016
#### target db: db_cefs, use custom shape file to create the home ranges table (dv) and tr_parcellaire_par from db_cefs as landscape information and combine them toward news tables
#### objet: transform a multipolygon shape file to polygon geometry and intersect with a landscape table on db_cefs data base
#### source file: home range shape file with a geom defined as a polygon  geometry or multi-polygon geometry
#### output tables: create tables hr (homes ranges potentially multipolygons), hr_u (home ranges group by individuals), t_land_hr_lhr (table containing numeric values of tranghe intersect of each home range with the lanscape), t_land_hr_graph_lhrg (graphical intersection of each home range and sub-classes of landscape), t_land_hr_graph_grps (graphical intersection of each home range and classes, defined as grouped sub-classes, of landscape)
#### delete tables: script will create and delete hr, hr_u when they are not usefull anymore 
#### Please don't forget to drop your tables when your work is over
#### repository source: https://github.com/yannickkk/movit_tools/landscape_inside_home_ranges.R
#########################################################################################################

###first you have to define 3 R vectors encompassing sub-classes (grd_cat) to gather inside classes of habitat
humain<-c("bati","chemin","chemin prive","dependance ferme","depot chantier","enclos","jardin","parking","route")
cultures<-c("eau","jachere","legumineuse","lin","luzerne","maraichage","moutarde","oleoproteagineux","parc arbore","pelouse","polygonacee","prairie","prairie artificielle","prairie naturelle","soja","sorgho","stade foot","terre","tournesol","trefle","trefle+luzerne","verger","vigne","cereale","cereale+colza","cereale+prairie","chanvre","bande enherbe","colza","culture","feverole")
bois<-c("bois","haie","friche")

library(RPostgreSQL)
###connect to db_cefs on serveur
#source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
#source("C:/Users/ychaval/Documents/BD_MOVIT/Programmes/postgresql/movit_install_shapefile_analyse.R")
### fill the following fields: "serveur_adress", "db_name", "username" for both of them, "password"
con<- dbConnect(PostgreSQL(), host="serveur_adress", dbname="db_name", user="username", password="password")
user<-"username" ##don't forget this "user_name"
###R call shp2pgsql software using cmd command line (valid for windows os), replace the path way by your own for the shape file and the psql software and fill server_adress, db_name and username
shell("shp2pgsql.exe -s 2154 -I \"pathway_to_shapefile_including_.shp\" temporaire.hr | \"pathway_to_psql_including_.exe\" -p 5432 -h server_adress -d db_name -U username")
###rename columns
dbSendQuery(con, "ALTER TABLE temporaire.hr RENAME COLUMN gid TO id")
dbSendQuery(con, "ALTER TABLE temporaire.hr RENAME COLUMN names TO hr_names")
dbSendQuery(con, "ALTER TABLE temporaire.hr RENAME COLUMN area TO hr_area")
dbSendQuery(con, "ALTER TABLE temporaire.hr RENAME COLUMN hdv_year TO hr_year")
###when dv table had been created inside temporaire schema create a table dv_u and unified multi-polygon geom to polygon
dbSendQuery(con, "drop table if exists temporaire.hr_u;")
dbSendQuery(con, "create table temporaire.hr_u as select hr_names,hr_year, ST_UNION(geom) as geom_u from temporaire.hr group by hr_names, hr_year;")
dbSendQuery(con, "drop table if exists temporaire.hr;")
###Add columns to ease the creation of the home range table t_land_hr_lhr
dbSendQuery(con, "ALTER TABLE temporaire.hr_u ADD COLUMN hr_id serial")
dbSendQuery(con, "ALTER TABLE temporaire.hr_u ADD COLUMN id serial")
dbSendQuery(con, "ALTER TABLE temporaire.hr_u ADD COLUMN hr_area integer")
dbSendQuery(con, "update temporaire.hr_u set hr_area = round(ST_AREA (geom_u))")
###create a table to host home range data t_land_hr_lhr
dbSendQuery(con, "drop table if exists temporaire.t_land_hr_lhr;")
dbSendQuery(con, "create table temporaire.t_land_hr_lhr AS SELECT hr_id as lhr_id, hr_names AS lhr_cap_bague, hr_year as lhr_year, hr_area as lhr_area, geom_u as lhr_geom from temporaire.hr_u")
dbSendQuery(con, "ALTER TABLE temporaire.t_land_hr_lhr drop CONSTRAINT if exists t_hab_dv_hr_graphpkey;")
dbSendQuery(con, "ALTER TABLE temporaire.t_land_hr_lhr ADD CONSTRAINT t_hab_dv_hr_graphpkey PRIMARY KEY (lhr_id);")
dbSendQuery(con, "drop INDEX if exists temporaire.t_hab_dv_hr_graphgeom_gist cascade;")
dbSendQuery(con, "CREATE INDEX t_hab_dv_hr_graphgeom_gist ON temporaire.t_land_hr_lhr USING gist (lhr_geom);")
dbSendQuery(con, "drop table if exists temporaire.hr_u;")
###add columns to t_land_hr_lhr to store the square of each landscape group (ie human as umain, crops as cultures, wood as bois)
dbSendQuery(con, "ALTER TABLE temporaire.t_land_hr_lhr DROP COLUMN IF EXISTS lhr_surf_humain")   
dbSendQuery(con, "ALTER TABLE temporaire.t_land_hr_lhr DROP COLUMN IF EXISTS lhr_surf_cultures") 
dbSendQuery(con, "ALTER TABLE temporaire.t_land_hr_lhr DROP COLUMN IF EXISTS lhr_surf_bois") 
dbSendQuery(con, "ALTER TABLE temporaire.t_land_hr_lhr ADD COLUMN lhr_surf_humain text")
dbSendQuery(con, "ALTER TABLE temporaire.t_land_hr_lhr ADD COLUMN lhr_surf_cultures text")
dbSendQuery(con, "ALTER TABLE temporaire.t_land_hr_lhr ADD COLUMN lhr_surf_bois text")

###format for sql code
humain<-paste0("('",paste0(humain,collapse ="','"),"')")
cultures<-paste0("('",paste0(cultures,collapse ="','"),"')")
bois<-paste0("('",paste0(bois,collapse ="','"),"')")

###calculate the square of each class of landscape
dbSendQuery(con, paste0("UPDATE temporaire.t_land_hr_lhr SET  lhr_surf_humain = surf_hab_humain FROM
                       (
                       SELECT lhr_cap_bague, lhr_year, round(SUM(ST_Area(ST_Intersection(lhr_geom, tr_parcellaire_par.geom)))) as surf_hab_humain
                       from
                       tr_parcellaire_par,
                       temporaire.t_land_hr_lhr
                       WHERE par_grd_cat in ",humain,"
                       and
                       tr_parcellaire_par.par_annee = lhr_year::integer AND
                       ST_intersects(lhr_geom, tr_parcellaire_par.geom)
                       group by lhr_cap_bague, lhr_year --, par_id
                       ORDER BY lhr_cap_bague) as foo
                       where concat(foo.lhr_cap_bague,foo.lhr_year) = concat(temporaire.t_land_hr_lhr.lhr_cap_bague,temporaire.t_land_hr_lhr.lhr_year);"))

dbSendQuery(con, paste0("UPDATE temporaire.t_land_hr_lhr SET   lhr_surf_cultures = surf_cultures FROM
                       (
                       SELECT lhr_cap_bague, lhr_year, round(SUM(ST_Area(ST_Intersection(lhr_geom, tr_parcellaire_par.geom)))) as surf_cultures
                       from
                       tr_parcellaire_par,
                       temporaire.t_land_hr_lhr
                       WHERE par_grd_cat in ",cultures,"
                       and
                       tr_parcellaire_par.par_annee = lhr_year::integer AND
                       ST_intersects(lhr_geom, tr_parcellaire_par.geom)
                       group by lhr_cap_bague, lhr_year
                       ORDER BY lhr_cap_bague) as foo
                       where concat(foo.lhr_cap_bague,foo.lhr_year) = concat(temporaire.t_land_hr_lhr.lhr_cap_bague,temporaire.t_land_hr_lhr.lhr_year);"))

dbSendQuery(con, paste0("UPDATE temporaire.t_land_hr_lhr SET  lhr_surf_bois = surf_hab_bois FROM
                       (
                       SELECT lhr_cap_bague, lhr_year, round(SUM(ST_Area(ST_Intersection(lhr_geom, tr_parcellaire_par.geom)))) as surf_hab_bois
                       from
                       tr_parcellaire_par,
                       temporaire.t_land_hr_lhr
                       WHERE par_grd_cat in ",bois,"
                       and
                       tr_parcellaire_par.par_annee = lhr_year::integer AND
                       ST_intersects(lhr_geom, tr_parcellaire_par.geom)
                       group by lhr_cap_bague, lhr_year --, par_id
                       ORDER BY lhr_cap_bague) as foo
                       where concat(foo.lhr_cap_bague,foo.lhr_year) = concat(temporaire.t_land_hr_lhr.lhr_cap_bague,temporaire.t_land_hr_lhr.lhr_year);"))

dbSendQuery(con, paste0("UPDATE temporaire.t_land_hr_lhr SET lhr_surf_humain = 0 where lhr_surf_humain is null;"))
dbSendQuery(con, paste0("UPDATE temporaire.t_land_hr_lhr SET lhr_surf_cultures = 0 where lhr_surf_cultures is null;"))
dbSendQuery(con, paste0("UPDATE temporaire.t_land_hr_lhr SET lhr_surf_bois = 0 where lhr_surf_bois is null;"))

dbSendQuery(con, paste0("ALTER TABLE temporaire.t_land_hr_lhr  ALTER COLUMN lhr_surf_humain TYPE INTEGER USING lhr_surf_humain::integer"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.t_land_hr_lhr  ALTER COLUMN lhr_surf_cultures TYPE INTEGER USING lhr_surf_cultures::integer"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.t_land_hr_lhr  ALTER COLUMN lhr_surf_bois TYPE INTEGER USING lhr_surf_bois::integer"))


######Create a graphic table of the intersect to check visualy the former results and calculate further landscpae metrics
dbSendQuery(con, paste0("DROP TABLE IF EXISTS temporaire.t_land_hr_graph_lhrg;"))
dbSendQuery(con, paste0("CREATE TABLE temporaire.t_land_hr_graph_lhrg as select lhr_id as lhrg_id, lhr_cap_bague as lhrg_cap_bague, lhr_year as lhrg_year, par_grd_cat, ST_Intersection(lhr_geom, tr_parcellaire_par.geom) as geom from temporaire.t_land_hr_lhr, tr_parcellaire_par where tr_parcellaire_par.par_annee = lhr_year::integer and ST_Intersects(lhr_geom, tr_parcellaire_par.geom)"))

#compare home ranges contain in to_check data.frame to see if the differences correspond to missing grd_cat (ie 'hors zone','non renseigne'))
#to do that you have to display in Qgis t_hab_dv_hr_graph and filter "lhr_id" = to_check[1,"lhr_id"] and t_parcellaire_par ("par_annee" = to_check[1,"lhr_year"] and "par_grd_cat" in ('hors zone','non renseigne'))

###Drop if exists and then Create a graphical layer that group landscape in defined classes
dbSendQuery(con, paste0("DROP TABLE IF EXISTS temporaire.t_land_hr_graph_grps ;"))
dbSendQuery(con, paste0("CREATE TABLE temporaire.t_land_hr_graph_grps AS (
                       SELECT lhrg_id, lhrg_cap_bague, lhrg_year, MAX(par_grd_cat) AS grd_cat,  ST_Union(ST_MakeValid(ST_SnapToGrid(geom, 0.0001))) AS geom -- found non-noded intersection between LINESTRING see https://gis.stackexchange.com/questions/50399/how-best-to-fix-a-non-noded-intersection-problem-in-postgis
                       FROM temporaire.t_land_hr_graph_lhrg
                       WHERE par_grd_cat in ",humain,"
                       GROUP BY lhrg_id, lhrg_cap_bague, lhrg_year
                       UNION
                       SELECT lhrg_id, lhrg_cap_bague, lhrg_year, MAX(par_grd_cat) AS grd_cat, ST_Union(ST_MakeValid(ST_SnapToGrid(geom, 0.0001))) as geom
                       FROM temporaire.t_land_hr_graph_lhrg
                       WHERE par_grd_cat in ",cultures,"
                       GROUP BY lhrg_id, lhrg_cap_bague, lhrg_year
                       UNION
                       SELECT lhrg_id, lhrg_cap_bague, lhrg_year, MAX(par_grd_cat) AS grd_cat,  ST_Union(ST_MakeValid(ST_SnapToGrid(geom, 0.0001))) as geom
                       FROM temporaire.t_land_hr_graph_lhrg
                       WHERE par_grd_cat in ",bois,"
                       GROUP BY lhrg_id, lhrg_cap_bague, lhrg_year
                       order by lhrg_cap_bague, lhrg_year ASC);"))

dbSendQuery(con, paste0("ALTER TABLE temporaire.t_land_hr_graph_grps ADD COLUMN hr_id_grps serial;"))
dbSendQuery(con, paste0("ALTER TABLE temporaire.t_land_hr_graph_grps ADD CONSTRAINT t_land_hr_graph_grps_pkey PRIMARY KEY (hr_id_grps)"))
dbSendQuery(con, paste0("UPDATE temporaire.t_land_hr_graph_grps SET grd_cat = 'humain' where grd_cat in ",humain,";"))
dbSendQuery(con, paste0("UPDATE temporaire.t_land_hr_graph_grps SET grd_cat = 'culture' where grd_cat in ",cultures,";"))
dbSendQuery(con, paste0("UPDATE temporaire.t_land_hr_graph_grps SET grd_cat = 'bois' where grd_cat in ",bois,";"))
#### Now you can open Qgis and look at the resulting table t_land_hr_graph_grps
dbSendQuery(con, paste0("DROP INDEX IF EXISTS t_land_hr_graph_grps_index1; DROP INDEX IF EXISTS t_land_hr_graph_grps_index2;"))
dbSendQuery(con, paste0("CREATE INDEX t_land_hr_graph_grps_index1 ON \"analyse\".t_land_hr_graph_grps USING gist (geom); CREATE INDEX t_land_hr_graph_grps_index2 ON temporaire.t_land_hr_graph_grps (grd_cat);"))
dbSendQuery(con, paste0("DROP INDEX IF EXISTS temporaire.t_hab_dv_hr_graph_index1;DROP INDEX IF EXISTS temporaire.t_land_hr_graph_grps_index2;"))
dbSendQuery(con, paste0("CREATE INDEX t_hab_dv_hr_graph_index1 ON \"analyse\".t_land_hr_graph_lhrg USING gist (geom);CREATE INDEX t_land_hr_graph_grps_index2 ON temporaire.t_land_hr_graph_lhrg (par_grd_cat);"))
###add comments on columns of t_land_hr_lhr describing the classes
dbSendQuery(con, paste0("COMMENT ON COLUMN temporaire.t_land_hr_lhr.lhr_surf_humain IS 'grouping classes: ",humain,"\'"))
dbSendQuery(con, paste0("COMMENT ON COLUMN temporaire.t_land_hr_lhr.lhr_surf_cultures IS 'grouping classes: ",cultures,"\'"))
dbSendQuery(con, paste0("COMMENT ON COLUMN temporaire.t_land_hr_lhr.lhr_surf_bois IS 'grouping classes: ",bois,"\'"))
dbSendQuery(con, paste0("COMMENT ON COLUMN temporaire.t_land_hr_lhr.lhr_geom IS 'home range\'"))

###some individuals have a part of their home range not fully descripbed by the landscape table
to_check<-dbGetQuery(con, paste0("select lhr_id, lhr_year, lhr_area-(lhr_surf_humain+lhr_surf_cultures+lhr_surf_bois) as test_surfaces from temporaire.t_land_hr_lhr where lhr_area-(lhr_surf_humain+lhr_surf_cultures+lhr_surf_bois) not in (0,-1,1)"))

#####"pourcentage de couverture de l'assolement
dbSendQuery(con, "ALTER TABLE temporaire.t_land_hr_lhr add column lhr_land_hr_percentcov text")
dbSendQuery(con, "UPDATE temporaire.t_land_hr_lhr set lhr_land_hr_percentcov = round(100*(lhr_surf_humain::real + lhr_surf_cultures::real + lhr_surf_bois::real)/(lhr_area))")
#####create and update cap_id to be able to cross data with t_capture_cap
dbSendQuery(con, "ALTER TABLE temporaire.t_land_hr_lhr add column lhr_cap_id integer")
dbSendQuery(con, "UPDATE temporaire.t_land_hr_lhr set lhr_cap_id = toto.cap_id from (SELECT cap_id,cap_bague, ani_etiq, cap_annee_suivi FROM public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id) as toto where toto.cap_bague = lhr_cap_bague and lhr_year::integer = toto.cap_annee_suivi ;")
##create a foreign key on cap_id to control referential integrity 
# dbSendQuery(con, "ALTER TABLE \"analyse\".t_land_hr_lhr add CONSTRAINT hr_fk FOREIGN KEY (hr_cap_bague)
#       REFERENCES public.t_capture_cap (cap_bague) MATCH SIMPLE
#             ON UPDATE NO ACTION ON DELETE NO ACTION")
dbSendQuery(con, "Comment on table temporaire.t_land_hr_lhr IS 'table de visualisation de l''intersect entre les domaines vitaux les assolements correspondant a l''annee de suivi simplifie en milieu boises, urbain et cultures'")

#######to go further with postgis and calculate numerous 
#https://github.com/siose-innova/pg_landmetrics (FRAGSTATS (McGarigal, 2015))


####Grant privilege to users
schem<- c("temporaire") ##select schemas you want to change
owner<-c("cefs_admin")
for (j in 1:length(schem)){

  tab<-dbGetQuery(con,paste0("SELECT tablename
                              FROM pg_tables
                              WHERE schemaname = '",schem[j],"' 
                              and tableowner = '",owner,"';"))
  
  
  tab<-tab[,1]
  test<-dbGetQuery(con,paste("SELECT table_name
                             FROM information_schema.views
                             WHERE table_schema='",schem[j],"'
                             ", sep=""))
  
  seq<-dbGetQuery(con,paste("SELECT sequence_name
                            FROM information_schema.sequences
                            WHERE sequence_schema='",schem[j],"'
                            ;", sep=""))
  
  
  if (dim(test)[1]!= 0)  {view<-dbGetQuery(con,paste("SELECT table_name
                                                     FROM information_schema.views
                                                     WHERE table_schema='",schem[j],"'
                                                     ;", sep=""))}

  if (schem[j] == "analyse") {schem[j]<-paste0("\"",schem[j],"\"")}
    
  if (length(tab) > 0) {for (i in 1:length(t(tab))){
    dbSendQuery(con, paste("ALTER TABLE ",schem[j],".",t(tab)[i],"
                           OWNER TO ",owner,";
                           GRANT ALL ON TABLE ",schem[j],".",t(tab)[i]," TO ychaval;
                           GRANT SELECT ON TABLE ",schem[j],".",t(tab)[i]," TO cefs_lecture;
                           GRANT ALL ON TABLE ",schem[j],".",t(tab)[i]," TO cefs_ecriture;", sep=""))}}
  
  if (length(seq) > 0) {for (i in 1:length(t(seq))){
    dbSendQuery(con, paste("ALTER SEQUENCE ",schem[j],".",t(seq)[i],"
                           OWNER TO ",owner,";
                           GRANT ALL ON SEQUENCE ",schem[j],".",t(seq)[i]," TO ychaval;
                           GRANT SELECT ON SEQUENCE ",schem[j],".",t(seq)[i]," TO mov_it_lecture;
                           GRANT ALL ON SEQUENCE ",schem[j],".",t(seq)[i]," TO mov_it_ecriture;", sep=""))}}
  
  
  if (dim(test)[1]!= 0) {if (length(view) > 0) {for (i in 1:length(t(view))){
    dbSendQuery(con, paste("ALTER TABLE ",schem[j],".",t(view)[i],"
                           OWNER TO ",owner,";
                           GRANT ALL ON TABLE ",schem[j],".",t(view)[i]," TO ychaval;
                           GRANT SELECT ON TABLE ",schem[j],".",t(view)[i]," cefs_lecture;
                           GRANT ALL ON TABLE ",schem[j],".",t(view)[i]," TO cefs_ecriture;", sep=""))}}}
  
    dbSendQuery(con, paste("GRANT ALL ON SCHEMA ",schem[j]," TO ychaval;
                           GRANT ALL ON SCHEMA ",schem[j]," TO public;",sep=""))}

dbDisconnect(con)
