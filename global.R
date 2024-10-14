library(shiny)
library(shinyWidgets)
library(shinyBS)
library(tidyverse)
library(janitor)
library(leaflet)
library(leaflet.extras)
library(openrouteservice)
library(sf)
library(here)
library(bslib)
library(bsicons)
library(htmlwidgets)
library(plotly)
library(webshot)
library(dotenv)
library(fontawesome)
library(RPostgres)
#library(duckdb)
library(DT)
library(shinyjs)
library(lwgeom)
library(scales)
library(arrow)
library(sfarrow)
library(gt)
#library(profvis)

#renv::init()
#renv::snapshot()

#tools::package_dependencies("gt", recursive = TRUE)



#chargement de l'environnement global
dotenv::load_dot_env("env.env")

#api de Openrouteservice
ors_api_key <- Sys.getenv("OPENROUTESERVICE_KEY")

#pour intersection
sf_use_s2(FALSE)

#palette
#"#111c26", "#203245", "#6FC9FD", "#0065FF", "#FF5302", "#FFA400


#-----------------------
# Chargement des données
#-----------------------


#SDR
sdr <- st_read("data/SDR_chambly.gpkg")

#Occupant
#---------------------------
# Icones et polices
#--------------------------

#custom leaflet marker
icon.glyphicon <- makeAwesomeIcon(icon= 'glyphicon-search', markerColor = '#0065FF', iconColor = 'white')


#police pour plotly
f <- list(
  family = "onest",
  size = 13, 
  color = "black"
)



#-------------------------
#sources des modules
#------------------------
source(here("modules", "accueil.R"))
source(here("modules", "isochrone.R"))
source(here("modules", "requetes.R"))
source(here("modules", "tableau_de_bord.R"))
source(here("modules", "carte_interactive.R"))


#-------------------------
# Helpers
#-------------------------
source(here("helpers", "popup.R")) #popup information
source(here("helpers", "isochrone_socio_demo.R")) #pour le calcul des données du recensement dans les courbes isochrones


#----------------------------
# parquet files
#----------------------------


#occupant <- st_read("data/occupant_chambly_final.gpkg") |> 
# unite(rue, voie, rue, sep = " ") |> 
#rename(fonction = type_etablissement) |> 
# rename(entreprise = occupant) |> 
#relocate(fonction, .before = usage_1) |> 
# mutate(no_civique = ifelse(id == "c92c530e-aade-43e6-aeff-1265a9970b18", 1748, no_civique)) |> 
#mutate(no_civique = as.numeric(no_civique)) |>
#mutate odd 
#mutate(odd = ifelse(no_civique %% 2 == 0, "pair", "impair")) |> 
#filter(!statut_1 == "Démoli") |> 
#remove empty geometry
#filter(!is_empty(geom)) |> 
#mutate(vacance = ifelse(entreprise == "VACANT", "Vacant", "Occupé")) 

# Chemin pour le fichier Parquet de sortie
#parquet_path <- "data/occupant_final.parquet"

# Utiliser st_write_parquet pour sauvegarder `occupant` au format Parquet
#st_write_parquet(occupant, dsn = parquet_path)

#ouvrir le fichier 
# Ouvrir le dataset Parquet
#occupant_dataset <- arrow::open_dataset("data/occupant.parquet", format = "parquet") 

occupant_dataset <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") 

#x <- occupant_dataset |> 
#  read_sf_dataset()  
#  group_by(vacance) |> 
# summarise(vacance = n()) %>% 
#  ungroup() 

#how to change font with popify
#https://stackoverflow.com/questions/63368078/how-to-change-font-with-popify






