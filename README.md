# movit_tools
tools for the members of the movit project
calcul_distance_points_lignes.sql
permet de calculer la distance la plus courte entre un point GPS et un élément du paysage dans un rayon (par défaut de 5km) autour du point
dv_paysage.R
est une version plus abutie des deux scripts suivant qui permet:
  1- de sélectionner les localisations GPS de chevreuils:
                - à partir d'un intervalle d'année de suivi,
                - d'enlever un certain nombre de jours de suivi après la capture (période pendant laquelle le mouvement des animaux est aberrant généralement 11 jours) 
                - d'enlever les individus dont le nombre de jour de suivi est jugé insuffisant
                - de choisir un schedule (i.e. une programmation homogène de l'écart entre deux localisations gps)
                - d'enlever des disperseurs à partir du fichier de référence de Delphine Ducros Classification_Delphine_12_04_2018.csv
                  ATTENTION CE FICHIER NE CLASSE LES ANIMAUX QUE JUSQU4EN 2017
                - De la probabilité de présence pour l'établissement des kernel density.
                - de retirer les outlayer (methode de Bjøerneraas).
                - de choisir entre domaine annuel et dommaine mensuel.
  2- d'installer la couche des domaines vitaux sur la base de données
  3- de décrire la composition des domaines vitaux en regroupant les informations du parcellaire annuel de la zone d'étude sous trois catégories (humain, fermé, ouvert)
  n.b: le domaine vital d'un animal peu déborder la zone couverte par le parcellaire annuel. En plus du pourcentage de chacune des trois classes décrites ci-dessus dans le domaine vital de l'individu, le script calcule également le pourcentage de recouvrement du domaine vital avec le relevé parcellaire.
home_range.R
permet à partir des ani_etiq des individus de calculer leur domaine vitaux annuels
home_range_and_landscape.R
permet de calculer la compositions paysagères des domaines vitaux calculés par le script précédent.
