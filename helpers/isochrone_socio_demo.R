library(tidyverse)
library(sf)
library(openrouteservice)
library(RPostgres)
library(jsonlite)
library(lwgeom)
library(feather)
library(arrow)


calcul_socio_demo <- function(iso, onProgress = function(progress) {}) {
  
  #étapes pour barre de calcul
  total_steps <- 3 
  
  for (i in 1:total_steps) {
  
  isochrone <- iso 

  #charger la table isocrone
  ad <- st_read("data/ad.gpkg") %>% rename(geometry = geom)

  #intersection entre isochrone et les ad et ajouter s2 false
  onProgress(1 / total_steps)
  sf_use_s2(FALSE)
  ad_isochrone <- st_intersection(ad, isochrone)
  
  #calculer la superficie de l'intersection
  ad_isochrone <- ad_isochrone %>% mutate(iso_superficie = as.numeric(st_area(ad_isochrone)))
  
  #calculer un ratio entre la superficie de l'intersection et la superficie de l'ad
  ad_isochrone <- ad_isochrone %>% 
    mutate(ratio = iso_superficie / ad_superficie)
  
  # Construction de la requête SQL
  onProgress(1 / total_steps)

  # Chemin pour le fichier Parquet de sortie
  parquet_path <- "data/recensement_2021_iso.parquet"
  
  #ouvrir le fichier 
  recensement <- open_dataset(parquet_path) 
  
  #rec <- recensement |> 
    #mutate(nom_caracteristique = case_when(
      #id_caracteristique == 2016 ~ "Secondaire",
      #id_caracteristique == 2024 ~ "Universitaire",
     # id_caracteristique == 2018 ~ "Collégial/DEP",
    #  TRUE ~ nom_caracteristique
   #))
  
  #parquet_path <- "data/recensement_2021_iso.parquet"
  
  #write_parquet(rec, parquet_path)

  #ouvrir le fichier 
  recensement <- open_dataset(parquet_path)
  
  #valeur adidu à filtrer
  valeurs_adidu <- ad$adidu
  
  # Filtrer le dataset pour ne garder que les lignes avec les valeurs adidu spécifiées
  filtered_recensement  <- recensement %>%
    filter(adidu %in% valeurs_adidu)
  
  #collecter les données et jointure
  rec <- filtered_recensement %>% 
    collect() %>% 
    inner_join(ad_isochrone, by = "adidu") %>%
    st_drop_geometry() %>% select(-ratio, -iso_superficie, -ad_superficie, -geometry) 
  

    

  
  #query <- sprintf(
   # "SELECT * FROM recensement_2021
   #WHERE id_caracteristique IN (1, 6, 
    #           3000, 3001, 3002, 3003, 3004, 3005,
     #          39,
      #         50, 51, 52, 52, 53, 54, 55,
       ##        57,
         #      243,
          #     1467, 1486, 1488,
           #    1529,
            #   2014, 2015, 2016, 2017, 2018, 2024,
             #  2224, 2226, 2227, 2228, 2229, 2230,
        #       2259, 2260, 2261, 2262, 2263, 2264, 2265, 2266, 2267, 2268, 2270, 2271, 2272, 2273, 2274, 2275, 2276, 2277, 2278, 2279, 2280, 2281,
         #      2282, 2283, 2284, 2285,
          #     2603, 2604, 2607, 2608, 2609, 2610,
           #    2611, 2612, 2613, 2614, 2615, 2616, 
            #   2617, 2618, 2619, 2620, 2621, 2622, 2623)  
   #AND adidu IN ('%s')",
    #paste(ad_isochrone$adidu, collapse = "', '")
  #)
  
  
  #filtrer les id_caracteristique 1 et 50 et les pivoter sur les colonnes
  rec2 <- rec %>% 
    filter(id_caracteristique %in% c(1, 50)) %>% 
    select(adidu, nom_caracteristique, chiffre_total) %>% 
    pivot_wider(names_from = nom_caracteristique, values_from = chiffre_total) %>% 
    rename(ad_population_2021 = `Population, 2021`, ad_menage_2021 = `Total - Ménages privés selon la taille du ménage - Données intégrales (100 %)`)
  
  #joindre données du rencensement aux colonnes créees
  rec <- left_join(rec, rec2)
  
  
  #joindre rec avec ad_isochrone
  rec_ad <- left_join(rec, ad_isochrone, by = "adidu")
  
  #calculs des colonnes population et ménages
  rec_ad_calcul_pop_men <- rec_ad %>% 
    mutate(iso_population_2021 = round(ad_population_2021 * ratio, 0),
           iso_menage_2021 = round(ad_menage_2021 * ratio, 0))
  
  #calculer la population en multipliant la population totale par le ratio
  rec_ad_calcul <- rec_ad_calcul_pop_men  %>% 
    group_by(nom_caracteristique) %>% 
    mutate(iso_chiffre_total = case_when(
      id_caracteristique %in% c(1, 50:55, 1467,1529, 2014:2018, 2024, 2224, 2226, 2259:2623, 3000:3005) ~ sum(as.integer(chiffre_total * ratio), na.rm = TRUE),
      id_caracteristique %in% c(6) ~ as.integer(weighted.mean(chiffre_total, ratio, na.rm = TRUE)),
      id_caracteristique %in% c(39, 2227, 2228, 2229, 2230) ~ round(weighted.mean(chiffre_total, iso_population_2021, na.rm = TRUE), 1),
      id_caracteristique %in% c(57, 243, 1486, 1488) ~ round(weighted.mean(chiffre_total, iso_menage_2021, na.rm = TRUE), 1),
    )) %>% 
    mutate(iso_homme_total = case_when(
      id_caracteristique %in% c(3000:3005) ~ sum(as.integer(chiffre_hommes * ratio), na.rm = TRUE)
    )) %>% 
    mutate(iso_femme_total = case_when(
      id_caracteristique %in% c(3000:3005) ~ sum(as.integer(chiffre_femmes * ratio), na.rm = TRUE)
    )) %>% 
    ungroup()
  
  #mettre le jeu de donnée en format large et retirer adidu, chiffre_total, chiffre_hommes, chiffre_femmes, taux_total, taux_hommes, taux_femmes, ad_superficie,iso_superficie, ratio, nom_geo, indicateur_qualite_donnees, niveau_geo, id.x, note_caracteristique
  onProgress(1 / total_steps)
  rec_ad_calcul2 <- rec_ad_calcul %>% 
    arrange(id_caracteristique) %>%
    select(-adidu, -chiffre_total, -chiffre_hommes, -chiffre_femmes, -taux_total, -taux_hommes, -taux_femmes, -ad_superficie, -ratio, -nom_geo, -indicateur_qualite_donnees, 
           -niveau_geo, -note_caracteristique, -geometry, -id_caracteristique,
           -iso_population_2021, -iso_menage_2021, -ad_population_2021, -ad_menage_2021) %>%
    # Distinct avant de résumer pour éviter les répétitions dans les listes
    distinct(nom_caracteristique, .keep_all = TRUE) %>%   
    # Résumer en listes
    summarise(nom_caracteristique = list(nom_caracteristique),
              iso_chiffre_total = list(iso_chiffre_total),
              iso_homme_total = list(iso_homme_total),
              iso_femme_total = list(iso_femme_total)) 
  
  rec_ad_isochrone <- bind_cols(isochrone, rec_ad_calcul2) 

  }
  
  return(rec_ad_isochrone)
  
}

#test

