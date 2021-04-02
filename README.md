# movit_tools
tools for the members of the movit project

A -**Classification_Delphine_12_04_2018.csv**

Classification des suivis d'animaux suivant la tactique de mouvement des animaux subadultes (yearling). Ce fichier peut être utilisé par le script dv_paysage.R pour filtrer les disperseurs.
**DN**: disperseurs pour lesquels on n'a pas de doutes sur la dispersion
**DF**: disperseurs flous, qui sont soit des disperseurs soit des individus observant une tactique multi-range. 
**P**: philopatriques.
**PD**: pseudo-disperseurs.

A -**calcul_distance_points_lignes.sql**

permet de calculer la distance la plus courte entre un point GPS et un élément du paysage dans un rayon (par défaut de 5km) autour du point.

B-**dv_paysage.R**

Est une version plus aboutie des deux scripts suivant. Il permet:

  1- de sélectionner les localisations GPS de chevreuils:
  
                - à partir d'un intervalle d'années de suivi,
                - d'enlever un certain nombre de jours de suivi après la capture (période pendant laquelle le mouvement des animaux est aberrant généralement on      enlève 11 jours) 
                - d'enlever les individus dont le nombre de jour de suivi est jugé insuffisant
                - de choisir un schedule (i.e. une programmation homogène de l'écart entre deux localisations gps)
                - d'enlever des disperseurs à partir du fichier de référence de Delphine Ducros Classification_Delphine_12_04_2018.csv
                  ATTENTION CE FICHIER NE CLASSE LES ANIMAUX QUE JUSQU'EN 2017
                - De définir une valeur de probabilité de présence pour l'établissement des kernel density.
                - de retirer les points GPS outlayers (methode de Bjøerneraas).
                - de choisir entre domaine annuel et dommaine mensuel.
                - de choisir des individus particulier lorqu'un vecteur des ani_etiq est fourni.
  
  2- d'installer la couche des domaines vitaux sur la base de données
  
  3- de décrire la composition des domaines vitaux en regroupant les informations du parcellaire annuel de la zone d'étude sous trois catégories (humain, fermé, ouvert)
  
               - le script permet d'identifier les tables crées avec les initials de l'utilisateur (cela évite d'écraser les tables des autres utilisateurs et permet de simplifier le nettoyage par l'administrateur)
  
  n.b: le domaine vital d'un animal peu déborder la zone couverte par le parcellaire annuel. En plus du pourcentage de chacune des trois classes décrites ci-dessus dans le domaine vital de l'individu, le script calcule également le pourcentage de recouvrement du domaine vital avec le relevé parcellaire.
  
C- **Script home_range.R**

permet à partir des ani_etiq des individus de calculer leur domaine vitaux annuels

D- **home_range_and_landscape.R**

permet de calculer la compositions paysagères des domaines vitaux calculés par le script précédent.
