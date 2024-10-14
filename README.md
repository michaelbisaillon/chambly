# Application Chambly

Ce dépôt contient le code de l'application Shiny **Chambly**. L'application est accessible à l'adresse suivante : [luxuriant.shinyapps.io/chambly](https://luxuriant.shinyapps.io/chambly/).

## Description de l'application

L'application **Chambly** est une plateforme interactive développée en R Shiny qui permet aux utilisateurs d'explorer les données sur la ville de Chambly. Elle propose plusieurs fonctionnalités, notamment :
- **Visualisation des données** : Cartes interactives et graphiques pour visualiser les informations clés.
- **Analyse des données démographiques et commerciales** : Données démographiques de la ville et répartition des commerces.
- **Filtrage dynamique** : Permet de filtrer les données par secteur, type de commerce, etc.
- **Interface utilisateur intuitive** : Utilisation de Leaflet pour la carte interactive et de divers modules Shiny pour gérer les entrées utilisateurs.

## Structure du code

Le projet est structuré de la manière suivante :


### Fichiers principaux :

- **ui.R** : Ce fichier contient la définition de l'interface utilisateur, y compris la mise en page, les boutons, et les cartes interactives.
- **server.R** : Ce fichier gère la logique serveur, incluant le traitement des données, les calculs et la gestion des entrées/sorties utilisateur.
- **global.R** : Ce fichier est utilisé pour charger les packages R nécessaires et les données utilisées globalement dans l'application.
- **data/** : Répertoire contenant les fichiers de données utilisés dans l'application. Ce répertoire est crucial pour le bon fonctionnement de l'application.
- **helpers/** : Ce répertoire contient des fonctions supplémentaires qui aident à organiser et structurer le code.
- **modules/** : Ce répertoire contient des modules Shiny, qui permettent de structurer le code en composants réutilisables.
- **www/** : Répertoire pour les ressources statiques comme les images, fichiers CSS, etc.

## Installation et exécution

Pour exécuter cette application en local :

1. Clonez le dépôt :
   ```bash
   git clone https://github.com/michaelbisaillon/chambly.git

Développé par Michaël Bisaillon et Dominque Roy.
