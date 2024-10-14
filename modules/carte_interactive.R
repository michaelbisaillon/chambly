carte_interactive_UI <- function(id, session){
  
  ns <- NS(id)
  
  ui <- page_sidebar(
    sidebar = sidebar(position = "left", width = 500,
                      accordion(id = ns("type_requete"), 
                                multiple = FALSE,
                                #accordion_panel(title = "Structure commerciale", icon = bsicons::bs_icon("pin-map-fill"), open = TRUE,
                                #               pickerInput(ns("geo_zone"), label = "", choices = c("Pôle local", "Pôle de proxmité"), multiple = FALSE, options = list(placeholder = "Sélectionner un pôle"))),
                                accordion_panel(title = "Portrait sociodémographique", icon = bsicons::bs_icon("person-fill"), open = TRUE,
                                                pickerInput(ns("socio_sujet"), label = "", choices = c("Population", "Densité de population", "Revenu médian des ménages", "Diplômés postsecondaires", "Immigration"), selected = "Population", multiple = FALSE, options = list(placeholder = "Sélectionner un thème")),
                                                pickerInput(ns("socio_zone"), label = "", choices = c("Chambly"), selected = "Chambly", multiple = FALSE, options = list(placeholder = "Sélectionner une zone"))
                                ),
                                accordion_panel(title = "Portrait commercial et industriel", icon = bsicons::bs_icon("building-fill"),
                                                pickerInput(ns("commercial_sujet"), label = "", choices = c("Mix commercial", "Vacance commerciale", "Taux de roulement"), multiple = FALSE, selected = "Mix commercial", options = list(placeholder = "Sélectionner un thème"))
                                                #pickerInput(ns("commercial_zone"), label = "", choices = c("Chambly", "Bourgogne - de Périgny", "Zone industrielle"), multiple = FALSE, options = list(placeholder = "Sélectionner une zone"))
                      )
    )
    ),
    navset_card_pill(
      nav_panel("Carte",  icon = icon("map"), 
                card_body(class = "p-0", 
                          height = "78vh",
                          leafletOutput(ns("carte_recherche"), width = "100%")
                )
      ),
      nav_panel("Informations", icon = icon("question-circle"),
                tags$h5("Comment effectuer une recherche?"),
                tags$p("La carte interactive permet de trouver des informations sur la structure commerciale et le recensement le plus récent de la population."),
                tags$p("Pour effectuer une recherche, vous pouvez utiliser les différents filtres disponibles dans le menu de recherche situé à gauche."),
                tags$p("Les résultats s'affichent sur la carte. Vous pouvez cliquer sur un polygone pour obtenir plus d'informations sur une zone ou une aire de diffusion.")
      )
    )
  )
}
  

carte_interactive_server <- function(id, data) {
  moduleServer(id, function(input, output, session){
    
    
    #---------------------------
    #carte leaflet
    #---------------------------
    output$carte_recherche <- renderLeaflet({
      
      req(input$type_requete %in% c('Portrait sociodémographique', 'Portrait commercial et industriel'))   
      
      if(input$type_requete == 'Portrait sociodémographique' & input$socio_sujet == 'Population'){
        
        base <-  arrow::open_dataset("data/recensement_2021_ad_chambly.parquet", format = "parquet") |> 
          filter(id_caracteristique == 1) |>
          read_sf_dataset()
        
        #palette de courleur avec les valeurs chiffre_total
        pal <- colorNumeric(palette = "YlOrRd", domain = base$chiffre_total)
        
        popup <- paste(
          '<div style="font-family: Onest, sans-serif; color: #203245;">', #appliquer la police au popup
          "<b>", "Aire de diffusion: ", "</b>", base$adidu, "<br>",
          "<b>", "Population: ", "</b>", format(base$chiffre_total, big.mark = ' ')
          
        )
        
        leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) |> 
          addPolygons(data = base, 
                      fillColor = ~pal(chiffre_total), 
                      fillOpacity = 0.75, 
                      color = "white",
                      opacity = 1,
                      weight = 1.5,
                      popup = popup) %>% 
          addLegend(
            "bottomright",
            pal = pal,
            values = base$chiffre_total,
            labFormat = labelFormat(suffix = " hab.", big.mark = " "),
            title = "Population",
            opacity = 1
          )
        
      }else if(input$type_requete == 'Portrait sociodémographique' & input$socio_sujet == 'Densité de population'){
        
        base <-  arrow::open_dataset("data/recensement_2021_ad_chambly.parquet", format = "parquet") |> 
          filter(id_caracteristique == 6) |>
          read_sf_dataset()
        
        #palette de courleur avec les valeurs chiffre_total
        pal <- colorNumeric(palette = "YlOrRd", domain = base$chiffre_total)
        
        popup <- paste(
          '<div style="font-family: Onest, sans-serif; color: #203245;">', #appliquer la police au popup
          "<b>", "Aire de diffusion: ", "</b>", base$adidu, "<br>",
          "<b>", "Densité de population: ", "</b>", format(as.integer(base$chiffre_total), big.mark = ' '), "km²"
          
        )
        
        leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) |> 
          addPolygons(data = base, 
                      fillColor = ~pal(chiffre_total), 
                      fillOpacity = 0.75, 
                      color = "white",
                      opacity = 1,
                      weight = 1.5,
                      popup = popup) %>% 
          addLegend(
            "bottomright",
            pal = pal,
            values = base$chiffre_total,
            labFormat = labelFormat(suffix = " hab./km2", big.mark = " "),
            title = "Densité de population",
            opacity = 1
          )
        
        
      }else if(input$type_requete == 'Portrait sociodémographique' & input$socio_sujet == 'Revenu médian des ménages'){
        
        base <-  arrow::open_dataset("data/recensement_2021_ad_chambly.parquet", format = "parquet") |>  
          filter(id_caracteristique == 243) |>
          read_sf_dataset()
        
        #palette de courleur avec les valeurs chiffre_total
        pal <- colorNumeric(palette = "YlOrRd", domain = base$chiffre_total)
        
        popup <- paste(
          '<div style="font-family: Onest, sans-serif; color: #203245;">', #appliquer la police au popup
          "<b>", "Aire de diffusion: ", "</b>", base$adidu, "<br>",
          "<b>", "Revenu des ménages: ", "</b>", format(as.integer(base$chiffre_total), big.mark = ' '), "$"
          
        )
        
        leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) |> 
          addPolygons(data = base, 
                      fillColor = ~pal(chiffre_total), 
                      fillOpacity = 0.75, 
                      color = "white",
                      opacity = 1,
                      weight = 1.5,
                      popup = popup) %>% 
          addLegend(
            "bottomright",
            pal = pal,
            values = base$chiffre_total,
            labFormat = labelFormat(suffix = "$", big.mark = " "),
            title = "Revenu des ménages",
            opacity = 1
          )
        
        
        
      }else if(input$type_requete == 'Portrait sociodémographique' & input$socio_sujet == 'Diplômés postsecondaires'){
        
        base <-  arrow::open_dataset("data/recensement_2021_ad_chambly.parquet", format = "parquet") |>  
          filter(id_caracteristique == 2017) |>
          read_sf_dataset()
        
        #palette de courleur avec les valeurs chiffre_total
        pal <- colorNumeric(palette = "YlOrRd", domain = base$taux_total)
        
        popup <- paste(
          '<div style="font-family: Onest, sans-serif; color: #203245;">', #appliquer la police au popup
          "<b>", "Aire de diffusion: ", "</b>", base$adidu, "<br>",
          "<b>", "Diplômés postsecondaires: ", "</b>", format(as.integer(base$taux_total), big.mark = ' '), "%"
          
        )
        
        leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) |> 
          addPolygons(data = base, 
                      fillColor = ~pal(taux_total), 
                      fillOpacity = 0.75, 
                      color = "white",
                      opacity = 1,
                      weight = 1.5,
                      popup = popup) %>% 
          addLegend(
            "bottomright",
            pal = pal,
            values = base$taux_total,
            labFormat = labelFormat(suffix = "%", big.mark = " "),
            title = "Diplômés postsecondaires",
            opacity = 1
          )
        
      }else if(input$type_requete == 'Portrait sociodémographique' & input$socio_sujet == 'Immigration'){
        
        base <-  arrow::open_dataset("data/recensement_2021_ad_chambly.parquet", format = "parquet") |>  
          filter(id_caracteristique %in% c(1529)) |>  
          read_sf_dataset()
        
        #palette de courleur avec les valeurs chiffre_total
        pal <- colorNumeric(palette = "YlOrRd", domain = base$taux_total)
        
        popup <- paste(
          '<div style="font-family: Onest, sans-serif; color: #203245;">', #appliquer la police au popup
          "<b>", "Aire de diffusion: ", "</b>", base$adidu, "<br>",
          "<b>", "% immigrants: ", "</b>", format(as.integer(base$taux_total), big.mark = ' '), "%"
        )
        
        leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) |> 
          addPolygons(data = base, 
                      fillColor = ~pal(taux_total), 
                      fillOpacity = 0.75, 
                      color = "white",
                      opacity = 1,
                      weight = 1.5,
                      popup = popup) %>% 
          addLegend(
            "bottomright",
            pal = pal,
            values = base$taux_total,
            labFormat = labelFormat(suffix = "%", big.mark = " "),
            title = "Immigration",
            opacity = 1
          )
        
      }else if(input$type_requete == 'Portrait commercial et industriel' & input$commercial_sujet == 'Mix commercial'){
        
        base <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
          filter(!usage_1 == "VACANT") |>
          read_sf_dataset() %>% 
          mutate(usage_1 = factor(usage_1, levels = c("Biens courants", "Biens semi-courants", "Biens réfléchis", 
                                                      "Restauration", "Divertissement, loisirs et hébergement", 
                                                      "Fabrication", "Transport", "Construction", "Commerce de gros", 
                                                      "Organismes publics et privés", "Services d'enseignement", "Services aux particuliers", "Services aux entreprises", "Services professionnels", 
                                                      "Autres", "Utilités publiques"
                                                      ))) %>% 
          arrange(usage_1)
 
        
        #palette de courleur avec les valeurs chiffre_total
        #pal <- colorFactor(palette = "Spectral", domain = base$usage_1)
        
        pal <- colorFactor(palette = c(
          "#6FC9FD", "#0065FF", "#003c99",  #bleu,
          "#8000ff", "#c000ff", "#de003f", #green
          "#531B02", "#A33502", "#FF5302", "#FFA77D",             
          "#E0E0E0", "#cfd1d3", "#9fa4a8", "#70767c", "#404951", "#111c26", #gris
          "red", "#FAA0A0" #autres
        ), domain = base$usage_1)
        
        popup <- paste(
          '<div style="font-family: Onest, sans-serif; color: #203245;">', #appliquer la police au popup
          "<h5>", base$entreprise, "<br></h5>",
          "<b>", base$no_civique, base$rue, "<br>", base$ville, "Québec", base$code_postal, "</b><br>",
          "<b>","Usage: ", "</b>", base$usage_3, "<br><br>",
          "<b>","Téléphone: ", "</b>", base$telephone, "<br>",
          "<b>","Courriel: ", "</b>", base$courriel , "<br>"
        )
        
        leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) |> 
          addPolygons(data = sdr, fillOpacity = 0, color = "#203245", weight = 1, opacity = 1, dashArray = 5) %>% 
          addCircleMarkers(data = base, 
                           fillColor = ~pal(usage_1), 
                           fillOpacity = 1, 
                           color = "white",
                           opacity = 1,
                           weight = 1,
                           radius = 5,
                           popup = popup,
                           group = base$usage_1) %>%
          addLegend(
            "bottomright",
            pal = pal,
            values = base$usage_1,
            title = "Usage",
            opacity = 1
          ) #%>% 
          #addLayersControl(
          #  overlayGroups =base$usage_1,
          #  options = layersControlOptions(collapsed = FALSE))
      }else if(input$type_requete == 'Portrait commercial et industriel' & input$commercial_sujet == 'Vacance commerciale'){
        
        base <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
          filter(usage_1 == "VACANT") |>
          read_sf_dataset() 
        
        
        leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) |> 
          addPolygons(data = sdr, fillOpacity = 0, color = "#203245", weight = 1, opacity = 1, dashArray = 5) %>% 
          addHeatmap(data = base, minOpacity = 0.90, blur =40) %>% 
          addLegend(
            "bottomright",
            colors = c("#0E21A0", "aquamarine", "yellow", "orange", "red"),
            labels = c("Très faible", "Faible", "Moyen", "Élevé", "Très élevé"),
            title = "Intensité de la vacance",
            opacity = 0.75
          )
      }else if(input$type_requete == 'Portrait commercial et industriel' & input$commercial_sujet == 'Taux de roulement'){
        
        base <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
          filter(!statut_1 == "À jour") |>
          read_sf_dataset() 
        
        
        leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) |> 
          addPolygons(data = sdr, fillOpacity = 0, color = "#203245", weight = 1, opacity = 1, dashArray = 5) %>% 
          addHeatmap(data = base, minOpacity = 0.90, blur =40) %>% 
          addLegend(
            "bottomright",
            colors = c("#0E21A0", "aquamarine", "yellow", "orange", "red"),
            labels = c("Très faible", "Faible", "Moyen", "Élevé", "Très élevé"),
            title = "Intensité du roulement",
            opacity = 0.75
          )
      }
      
    })
    
    
  })
}