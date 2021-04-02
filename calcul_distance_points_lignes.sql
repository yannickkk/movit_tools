-------------------Exemple de calcul de distance minimale d'un point gps à une route -------------------------------
--base de donnees: bd_cefs
--tables/views utilisées : v_aniposi_gpsgsmv_aniposi_gpsgsm
--						   env_data.topo_17_route_emprise_disp
-- le script lie les deux tables et calcule la distance la plus courte entre chaque point GPS et le réseau routier (chaîne de caractère qui contient 'Route' dans le champ nature de la couche IGN) dans un rayon de 5 km autour -- de celui-ci

select cpos_id, ani_id, ani_etiq, cap_bague, cap_annee_suivi, pos_distance_route,
 closest_road.dist_route, 
 closest_road.nature, 
 cap_date, ani_sexe, cap_age_classe, cap_poids, cap_lpa, sit_nom_court, teq_nom_court, eqt_id_usuel, mar_libelle, mod_libelle, eqa_date_debut, eqa_date_fin, ani_mortalite, ani_date_mort, ani_cause_mort, eqa_activite, eqa_probleme, eqa_date_fin_text, eqa_date_fin_arrondi, ani_date_mort_arrondi, ani_date_mort_text, ani_poids_mort, ani_poids_mort_na, ani_remarque, ani_mort_x, ani_mort_y, ani_inconnu, cap_faon, cap_age, cap_age_corrige, cap_circou, cap_etat_sante, cap_heure_lacher, sit_id, the_geom, pos_x, pos_y, pos_systeme, pos_z, pos_x_corrige, pos_y_corrige, pos_z_corrige, pos_nb_sat, pos_dop, pos_dop_corrige, pos_fixstatus, pos_validated, cpos_date, cpos_heure, cpos_delta, cpos_prog6heure, cpos_prog4heure, cpos_prog3heure, cpos_prog1heure, cpos_prog10minutes, date_capture, pos_distance_bois, pos_distance_bati, pos_distance_haie, pos_localisation_par_id, par_os, par_grd_cat, par_annee
 FROM public.v_aniposi_gpsgsm as gps
CROSS JOIN LATERAL 
  (SELECT
      road.nature,
      ST_Distance(road.geom, gps.the_geom) as dist_route
      FROM env_data.topo_17_route_emprise_disp as road
      WHERE ST_DWithin(road.geom, gps.the_geom, 5000)
      ORDER BY ST_Distance(road.geom, gps.the_geom)
     LIMIT 1
   ) AS closest_road where nature ~* 'Route'
