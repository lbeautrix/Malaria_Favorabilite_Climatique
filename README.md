# Favorabilité climatique de la transmission du paludisme pour le Rwanda
Données et codes R pour le mémoire intitulé "Modélisation de la favorabilité climatique de la transmission du paludisme au Rwanda".

# Auteur : 
Louis Beautrix (louis.beautrix@gmail.com)

# Avant-propos
Ce code est directement issu du travail réalisé par Oswaldo C. Villena, Sadie J. Ryan, Courtney C. Murdock et Leah R. Johnson dans le cadre de leur recherche sur l'impact de la température sur la favorabilité environnementale du paludisme la transmission de Plasmodium falciparum et Plasmodium vivax par Anopheles gambiae et Anopheles stephensi. 
Cet article s'intitule : "Temperature impacts the environmental suitability for malaria transmission by Anopheles gambiae and Anopheles stephensi". 
L'article peut être lu à l'adresse suivante : https://doi.org/10.1002/ecy.3685

# Description des données
Le fichier data.zip consiste en l'ensemble des données brutes nécessaires pour faire tourner le code "Data_Preparation.R" et "Suitability_Rwanda.R".
Il contient 3 dossiers :
- Le premier, "incidence_data" correspond aux données d'incidence annuelle du paludisme au Rwanda produite par l'OMS.
- Le deuxième, "raw_data" correspond aux données météorologiques pour les 28 stations au Rwanda produite par la Rwanda Meteorology Agency.
- Le troisième, "villena_data" correspond aux 100 000 échantillons associés aux différents traits pour la transmission du Plasmodium falciparum par Anopheles gambiae (Villena et al., 2022).

# Description des codes
Le premier, "Data_Preparation.R" permet de modifier et corriger les données brutes contenues dans le dossier "raw_data".
Il est indispensable de le faire tourner avant de faire fonctionner le code "Suitability_Rwanda.R".
Il nécessite la création d'un fichier "input", localisé dans le fichier "data".

Le second, "Suitability_Rwanda.R" permet de réaliser la méthode utilisée dans le mémoire et détaillée à la section 3.4.
Il nécessite la création d'un fichier "output", localisé dans le fichier "data".

# Remarques
Les codes présents ici concernent uniquement la partie de la méthode réalisée sur R. 
Pour les analyses réalisées sur ArcGIS Pro, se référer à la section 3.4 du mémoire.
