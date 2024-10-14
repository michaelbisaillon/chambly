

isochrone_UI <- function(id, session){
  
  ns <- NS(id)
  
  ui <- page_sidebar(
    sidebar = sidebar(title = tags$h5("Paramètres courbe isochrone"), position = "right", width = 400, id = ns("sidebarID"), 
                      br(),
                      pickerInput(ns("type_transport"),label = h5("Mode de transport"), width = "100%",
                                  choices = c("Voiture" = "driving-car", 
                                              "Vélo" = "cycling-regular", 
                                              "À pied" = "foot-walking"), 
                                  selected = "foot-walking"),
                      sliderInput(ns("temps_transport"), width = "100%",
                                  label = h5("Durée du transport (minutes)"),
                                  ticks = FALSE,
                                  step = 5,
                                  min = 0,
                                  max = 30,
                                  value = 5),
                      actionButton(ns("action_isochrone"), label = tags$h5( "Afficher les résultats")),
    ),
    navset_card_pill(
      nav_panel("Carte", icon = icon("map"),
                card_body(class = "p-0", 
                          height = "78vh",
                          leafletOutput(ns("carte_isochrone"), width = "100%")
                )
      ),
      nav_panel("Statistiques", icon = icon("bar-chart"),
                #),
                #tabPanel(tags$h5("Statistiques"),
                br(),
                layout_columns(
                  col_widths = c(2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 8 ,4),
                  value_box(title = 'Population', 
                            theme = "primary",
                            textOutput(ns("population_2021"))),
                  value_box(title = 'Densité de population',
                            theme = "primary",
                            textOutput(ns("densite_pop"))),     
                  value_box(title = 'Âge moyen', 
                            theme = "primary",
                            textOutput(ns("age_moyen"))),
                  value_box(title = 'Revenu médian des ménages', 
                            theme = "primary",
                            textOutput(ns("revenu"))),
                  value_box(title = "Nombre d'entreprises", 
                            theme = "primary",
                            textOutput(ns("nombre_entreprise"))),
                  value_box(title = "Densité d'entreprises", 
                            theme = "primary",
                            textOutput(ns("densite_entreprise"))),
                  card(
                    fill = TRUE, 
                    fillable_mobile = TRUE,
                    min_height = 350,
                    card_header(
                      class = "custom-header-color", 
                      tags$h5("Vacance commerciale", class = "text-white")),
                    full_screen = TRUE,
                    plotlyOutput(ns("pie_vacance"), height = "18vh")),
                  
                  card(
                    fill = TRUE,
                    fillable_mobile = TRUE,
                    min_height = 350,
                    card_header(
                      class = "custom-header-color", 
                      tags$h5("Type de locaux")),
                    full_screen = TRUE,
                    plotlyOutput(ns("pie_fonction"), height = "18vh")),
                  
                  card(
                    fill = TRUE,
                    fillable_mobile = TRUE,
                    min_height = 350,
                    card_header(
                      class = "custom-header-color", 
                      tags$h5("Taille des ménages")),
                    full_screen = TRUE,
                    plotlyOutput(ns("test_pie_2"), height = "18vh")),
                  
                  card(
                    fill = TRUE,
                    fillable_mobile = TRUE,
                    min_height = 350,
                    card_header(
                      class = "custom-header-color",
                      tags$h5("Scolarité")),
                    full_screen = TRUE,
                    plotlyOutput(ns("test_pie_3"), height = "18vh")),
                  
                  card(
                    fill = TRUE,
                    fillable_mobile = TRUE,
                    min_height = 500,
                    card_header(
                      class = "custom-header-color",
                      tags$h5("Typologie des entreprises présentes")),
                    full_screen = TRUE,
                    plotlyOutput(ns("typologie_entreprise_presente"))),
                  
                  card(
                    fill = TRUE,
                    fillable_mobile = TRUE,
                    min_height = 500,
                    card_header(
                      class = "custom-header-color",
                      tags$h5("Pyramide des âges")),
                    full_screen = TRUE,
                    plotlyOutput(ns("pyramide_age")))
                )
                
      ),
      nav_panel("Informations", icon = icon("question-circle"),
                tags$h5("Comment effectuer une recherche?"),
                br(),
                tags$p("La recherche par courbe isochrone permet de visualiser les données statistiques d'une zone délimitée par un temps de trajet à pied, à vélo ou en voiture. Pour effectuer une recherche, suivez les étapes suivantes :"),
                tags$ul(
                HTML("
             <div style='text-align: left;'>
               <b>1. </b>Cliquez sur un point sur la carte", "<br>", 
                     "<b>2. </b>Accéder au menu à droite pour compléter la recherche", "<br>", 
                     "<b>3. </b>Cliquez sur le bouton «Afficher les résultats»", "<br>",
                     "<b>4. </b>Consulter les résultats dans l'onglet <i>Statistiques", "</i><br><br>",
                     "<i>Sur mobile, accéder au menu avec l'icône directionnelle à droite</i>
             </div>"
                )
                ),
      ),
    )
  )
}

isochrone_server <- function(id) {
  moduleServer(id, function(input, output, session){
    
    ns <- NS(id)
    
    # Reactive value pour stocker les coordonnées sélectionnées
    rv <- reactiveValues(lon = NULL, lat = NULL)
    
    # Définir la valeur réactive pour stocker les données d'intersection des occupants
    iso_occupant_reactive <- reactiveVal(NULL)
    
    # Définir la valeur réactive pour stocker les données d'intersection des données socio demo
    iso_sociodemo_reactive <- reactiveVal(NULL)
    
    occupant_reactive <- reactive({
      occupant <- occupant_dataset %>% 
        read_sf_dataset()
    })
    
    #-------------------------
    # Afficher la carte Leaflet de base
    #------------------------
    output$carte_isochrone <- renderLeaflet({
      
      #occupant <- occupant_dataset %>% 
      # read_sf_dataset()
      
      leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE, minZoom = 12)) %>%
        addProviderTiles(providers$CartoDB.Positron) |>  
        addPolygons(data = sdr, fillOpacity = 0, color = "#203245", weight = 1, opacity = 1, dashArray = 5) |> 
        addCircleMarkers(data = occupant_reactive(), weight = 1, radius = 5, color = "white", fillColor = "#203245", opacity = 1, fillOpacity =1) 
    })
    
    #----------------------------
    #clic sur un point
    #----------------------------
    observeEvent(input$carte_isochrone_click, {
      
      #occupant <- occupant_dataset %>% 
      # read_sf_dataset()
      
      click <- input$carte_isochrone_click
      click_point <- st_sfc(st_point(c(click$lng, click$lat)), crs = st_crs(occupant_reactive()))
      
      
      # Transformer les coordonnées en un système projeté
      occupant_utm <- st_transform(occupant_reactive(), crs = 32632)
      click_point_utm <- st_transform(click_point, crs = 32632)
      
      # Trouver le point d'intérêt le plus proche à 50 metres pour offrir un petit buffer
      nearest_point_index <- st_nearest_feature(click_point_utm, occupant_utm)
      nearest_point <- occupant_utm[nearest_point_index, ]
      
      # Calculer la distance
      distance <- st_distance(click_point_utm, nearest_point, by_element = TRUE)
      
      # Vérifier si la distance est inférieure à un seuil
      if (distance < units::set_units(50, "meters")) {
        rv$lon <- click$lng
        rv$lat <- click$lat
        
        # Mettre à jour la carte avec le nouveau marqueur
        
        # Zoomer sur la courbe isochrone
        #occupant <- occupant_dataset %>% 
        # read_sf_dataset()
        
        leafletProxy("carte_isochrone") %>%
          clearMarkers() %>%  # Supprimer les marqueurs précédents
          addCircleMarkers(data = occupant_reactive(), weight = 1, radius = 5, color = "white", fillColor = "#203245", opacity = 1, fillOpacity =1) |> 
          addAwesomeMarkers(lng = rv$lon, lat = rv$lat, icon = icon.glyphicon)
        
        
      } else {
        # Afficher un message modal si le clic est hors d'un point valide
        showModal(modalDialog(
          title = h4("Attention"),
          "La sélection est invalide. Veuillez cliquer sur l'un des points affichés sur la carte.",
          easyClose = TRUE,
          footer = modalButton("Fermer")
        ))
      }
    })
    
    # Écouteur pour le bouton 'action_isochrone'
    iso_occupant_reactive <- eventReactive(input$action_isochrone, {
      if (is.null(rv$lon) || is.null(rv$lat)) {
        showModal(modalDialog(
          title = "Attention",
          "Veuillez d'abord sélectionner un point sur la carte."
        ))
        return(NULL)
      } else {
        # Convertir le temps en secondes
        range <- as.numeric(input$temps_transport) * 60
        
        # Appeler openrouteservice API pour obtenir l'isochrone
        isochrone <- tryCatch({
          ors_isochrones(
            api_key = ors_api_key,
            locations = c(rv$lon, rv$lat),
            range = range,
            profile = input$type_transport,
            output = "sf"
          ) 
          
        }, error = function(e) {
          showModal(modalDialog(
            title = "Erreur",
            "Impossible de récupérer les isochrones : ", e$message
          ))
          return(NULL)
        })
        
        if (is.null(isochrone)) {
          showModal(modalDialog(
            title = "Erreur",
            "Aucune isochrone trouvée pour les paramètres fournis."
          ))
          return(NULL)
        }
        
        
        #calculer la superficie avec st_area
        #isochrone_socio_demo <- isochrone %>% mutate(iso_superficie = as.numeric(st_area(isochrone))) 
        
        
        
        #intersection avec les occupants
        sf_use_s2(FALSE)
        withProgress(message = 'Calcul en cours...', value = 0, {
          isochrone_socio_demo <- calcul_socio_demo(iso = isochrone, onProgress = function(progress) {
            incProgress(progress)
          })
        })
        #print(isochrone_socio_demo$iso_superficie) #NULL!
      }
    })
    
    observe({
      req(iso_occupant_reactive()) # s'assurer que iso_occupant_reactive a été exécuté et a retourné une valeur non-NULL
      
      isochrone_info <- iso_occupant_reactive()
      
      #occupant <- occupant_dataset %>% 
      # read_sf_dataset()
      
      #intersection de données
      sf_use_s2(FALSE)
      occupant_iso <- st_intersection(occupant_reactive(), isochrone_info)
      
      # Zoomer sur la courbe isochrone
      #isochrone_box <- st_bbox(occupant_iso) |> as.vector()
      
      # Mise à jour de la carte avec les données de l'isochrone
      leafletProxy("carte_isochrone") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygons(data = sdr, fillOpacity = 0, color = "#203245", weight = 1, opacity = 1, dashArray = 5) |> 
        addPolygons(data = isochrone_info, fillColor = "#203245", fillOpacity = 0.1, color = "#203245", weight = 1) %>%
        addCircleMarkers(data = occupant_reactive(), color = "white", weight = 1, fillColor = "#203245", fillOpacity = 1, radius = 5) %>%
        addCircleMarkers(data = occupant_iso, color = "white", weight = 1, fillColor = "#0065FF", fillOpacity = 1, radius = 5) %>%
        addAwesomeMarkers(lng = rv$lon, lat = rv$lat, icon = icon.glyphicon)  
      #fitBounds(isochrone_box[1], isochrone_box[2], isochrone_box[3], isochrone_box[4], options = list(maxZoom = 15))
      
      
    })
    
    
    
    #----------------------
    # Fonction pour les données socio-demo
    #----------------------
    displaySocioDemo <- function(isochrone) {
      
      req(iso_occupant_reactive())
      
      socio_demo_df <- iso_occupant_reactive() %>% 
        st_drop_geometry() 
      
      #unnest les lists
      socio_demo_df <- socio_demo_df%>% 
        unnest(cols = nom_caracteristique, iso_chiffre_total, iso_homme_total, iso_femme_total)
      
      #retirer les blancs des valeurs de nom_caracteristique qui sont au début de chaque valeur seulement
      socio_demo_df <- socio_demo_df %>% 
        mutate(nom_caracteristique = str_trim(nom_caracteristique, side = "left"))
      
    }
    
    #----------------------
    #graphiques
    #---------------------
    
    #pyramide des âges
    #-------------------------------
    output$pyramide_age <- renderPlotly({
      
      #filter les données de nom_caracteristique pour les groupes d'age
      age_df <- displaySocioDemo() %>%
        filter(str_detect(nom_caracteristique, "^\\d+ à \\d+ ans$") | 
                 str_detect(nom_caracteristique, "^\\d+ ans et plus$"))
      
      
      #mutate nom_caracteristique en factor avec comme levels les groupes dâge en ordre croissant
      age_df <- age_df %>% 
        mutate(nom_caracteristique = factor(nom_caracteristique, levels = c("0 à 14 ans", "15 à 29 ans", "30 à 44 ans", "45 à 59 ans", "60 à 74 ans", "75 ans et plus"))) 
      
      plot_ly() %>%
        add_bars(data = age_df, x = ~iso_homme_total * -1, y = ~nom_caracteristique, name = 'Hommes',
                 orientation = 'h', marker = list(color = '#111c26')) %>%
        add_bars(data = age_df, x = ~iso_femme_total, y = ~nom_caracteristique, name = 'Femmes',
                 orientation = 'h', marker = list(color = '#0065FF')) %>% 
        layout(
          font = f,
          xaxis = list(title = "",
                       tickmode = 'array', tickvals = c(-100000, -10000, -1000,0,  1000, 10000, 100000),
                       ticktext = c('100000', '10000', '1000', '0', '1000', '10000', '100000')
          ),
          paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
          yaxis = list(title = ""),
          bargap = 0.1,
          barmode = 'overlay',
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            y = -0.1))
      
    })
    
    #scolarité
    #-------------------------------
    output$test_pie_3 <- renderPlotly({
      
      req(displaySocioDemo())
      
      scolarite <- displaySocioDemo() %>%
        filter(nom_caracteristique %in% c("Aucun diplôme", "Secondaire", "Collégial/DEP", "Universitaire")) %>% 
        group_by(nom_caracteristique, iso_chiffre_total) %>%
        summarise() %>% 
        ungroup() %>% 
        mutate(pourcentage = round(iso_chiffre_total/sum(iso_chiffre_total)*100, 1), na.rm = TRUE) %>% 
        select(nom_caracteristique, iso_chiffre_total, pourcentage)
      
      write_csv(scolarite, "scolarite7.csv")
      
      plot_ly(scolarite, labels = ~nom_caracteristique, values = ~pourcentage, type = 'pie', hole = 0.6,
              textinfo = 'label+percent', # Affiche les labels et pourcentages
              textposition = 'outside', # Position du texte à l'extérieur
              textfont = list(color = 'black'),
              marker = list(colors = c("#111c26", "#6FC9FD", "#203245", "#FF5302"))) %>%
        layout(font = f,
               title = list(text = ""),
               showlegend = FALSE,
               xaxis = list(title = ""), yaxis = list(title = ""),
               margin = list(r = -500),
               paper_bgcolor = 'rgba(0,0,0,0)'
        ) %>%
        config(locale = "fr-ch")
      
    })
    
    #faire une diagrame en donut avec x et plotly
    output$test_pie_2 <- renderPlotly({
      
      req(displaySocioDemo())
      
      #test <- st_read(con, "isochrones") %>% slice(1)
      
      
      
      #test$nom_caracteristique <- str_trim(test$nom_caracteristique , side = "left")
      
      locat_proprio <- displaySocioDemo() %>% 
        filter(nom_caracteristique %in% c("1 personne", "2 personnes", "3 personnes", "4 personnes", "5 personnes ou plus")) %>% 
        group_by(nom_caracteristique, iso_chiffre_total) %>%
        summarise() %>% 
        ungroup() %>% 
        mutate(pourcentage = round(iso_chiffre_total/sum(iso_chiffre_total)*100, 1), na.rm = TRUE) %>% 
        select(nom_caracteristique, iso_chiffre_total, pourcentage)
      
      #y <- tibble(type_local = c("Commerce de gros", "Commercial", "Industriel", "Bureau"), total = c(9, 40, 20, 31))
      
      plot_ly(locat_proprio, labels = ~nom_caracteristique, values = ~iso_chiffre_total, type = 'pie', hole = 0.6,
              textinfo = 'label+percent', # Affiche les labels et pourcentages
              insidetextorientation = 'radial', # Orientation du texte à l'intérieur
              textfont = list(color = 'black'),
              #textposition = 'outside', # Position du texte à l'extérieur
              marker = list(colors = c("#111c26", "#0065FF", "#6FC9FD", "#203245", "#FF5302"))) %>%
        layout(font = f, title = list(text = ""),
               xaxis = list(title = ""), yaxis = list(title = ""), showlegend = FALSE,
               margin = list(r = -500),paper_bgcolor = 'rgba(0,0,0,0)'
        ) %>%
        config(locale = "fr-ch")
      
    })
    
    #test pie 4
    #----------------------
    output$pie_fonction <- renderPlotly({
      
      req(iso_occupant_reactive())
      
      socio_demo_df <- iso_occupant_reactive() %>% 
        st_drop_geometry() 
      
      #intersection de données (pas efficace car déja utilisé pour la carte proxy!)
      sf_use_s2(FALSE)
      occupant_iso <- st_intersection(iso_occupant_reactive(), occupant_reactive()) %>% st_drop_geometry()
      
      #test pie 2
      #y <- tibble(type_local = c("Commerce de gros", "Commercial", "Industriel", "Bureau"), total = c(9, 40, 20, 31))
      y <- occupant_iso %>% group_by(fonction) %>% summarise(total = n()) %>% ungroup()
      
      plot_ly(y, labels = ~fonction, values = ~total, type = 'pie', hole = 0.6,
              textinfo = 'label+percent', # Affiche les labels et pourcentages
              insidetextorientation = 'radial', # Orientation du texte à l'intérieur
              textfont = list(color = 'black'),
              #textposition = 'outside', # Position du texte à l'extérieur
              marker = list(colors = c("#0065FF","#FF5302","gray", "#111c26","#ffbb9c", "#6FC9FD"))) %>%
        layout(font = f, 
               title = list(text = ""),
               xaxis = list(title = ""), yaxis = list(title = ""), 
               margin = list(r = -500),
               showlegend = FALSE, 
               paper_bgcolor = 'rgba(0,0,0,0)'
        ) %>%
        config(locale = "fr-ch")
      
    })
    
    
    #socio demo box
    #---------------------
    observe({
      
      iso_socio_demo <- displaySocioDemo()
      
      socio_demo_pop_2021 <- iso_socio_demo  |> 
        filter(nom_caracteristique == "Population, 2021") %>% 
        distinct(nom_caracteristique, .keep_all = TRUE) %>% 
        mutate(iso_chiffre_total = as.integer(iso_chiffre_total))
      
      socio_densite_2021 <- iso_socio_demo  |> 
        filter(nom_caracteristique == "Densité de la population au kilomètre carré") %>% 
        distinct(nom_caracteristique, .keep_all = TRUE) 
      
      socio_age_moyen <- iso_socio_demo  |>
        filter(nom_caracteristique == "Âge moyen de la population") %>% 
        distinct(nom_caracteristique, .keep_all = TRUE)
      
      socio_revenu <- iso_socio_demo  |>
        filter(nom_caracteristique == "Revenu total médian des ménages en 2020 ($)") %>% 
        distinct(nom_caracteristique, .keep_all = TRUE)
      
      output$population_2021 <- renderText({
        paste0(format(socio_demo_pop_2021$iso_chiffre_total, big.mark = " "))
      })
      
      output$densite_pop <- renderText({
        paste0(format(socio_densite_2021$iso_chiffre_total, big.mark = " "), " km²")
      })
      
      output$age_moyen <- renderText({
        paste0(as.integer(socio_age_moyen$iso_chiffre_total), " ans")
      })
      
      output$revenu <- renderText({
        paste0(format(as.integer(socio_revenu$iso_chiffre_total), big.mark = " "), "$")
      })
      
    })
    
    # Nombre d'entreprise par usage
    #--------------------------------
    observeEvent(iso_occupant_reactive(), {
      
      #occupant <- occupant_dataset %>% 
      # read_sf_dataset()
      
      req(iso_occupant_reactive())
      
      #intersection de données (pas efficace car déja utilisé pour la carte proxy!)
      sf_use_s2(FALSE)
      occupant_iso <- st_intersection(iso_occupant_reactive(), occupant_reactive())
      
      # Préparation des données pour Plotly
      occupant_graph_count <- occupant_iso  |> 
        st_drop_geometry() |> 
        group_by(usage_1) |> 
        count() |> 
        ungroup() |> 
        mutate(pourcentage = n / sum(n)) |> 
        arrange(n)
      
      # Création du graphique Plotly
      output$typologie_entreprise_presente <- renderPlotly({
        
        plot_ly(occupant_graph_count, y = ~usage_1, x = ~n, type = 'bar', orientation = 'h', marker = list(color = "#203245")) |>
          layout(font = f, xaxis = list(title = "", ',.0f'), yaxis = list(title = "", categoryorder = "array", categoryarray = occupant_graph_count$usage_1)) |>
          config(locale = "fr-ch")
        
      })
      
      
      
      
      #faire une diagrame en donut avec x et plotly
      output$pie_vacance <- renderPlotly({
        
        req(iso_occupant_reactive())
        
        socio_demo_df <- iso_occupant_reactive() %>% 
          st_drop_geometry() 
        
        #intersection de données (pas efficace car déja utilisé pour la carte proxy!)
        sf_use_s2(FALSE)
        occupant_iso <- st_intersection(iso_occupant_reactive(), occupant_reactive()) %>% st_drop_geometry()
        
        #test pie
        #x <- tibble(occupation = c("Vacant", "Occupé"), total = c(9, 100))
        x <- occupant_iso %>% group_by(vacance) %>% summarise(total = n()) %>% ungroup() 
          #mutate(occupation = ifelse(vacance == "VACANT", "VACANT", "OCCUPÉ"))
        
        plot_ly(x, labels = ~vacance, values = ~total, type = 'pie', 
                textinfo = 'label+percent', # Affiche les labels et pourcentages
                insidetextorientation = 'radial', # Orientation du texte à l'intérieur
                textfont = list(color = 'black'),
                #textposition = 'outside', # Position du texte à l'extérieur
                hole = 0.6, marker = list(colors = c("#111c26", "#6FC9FD"))) |>
          layout(font = f, title = list(text = ""),
                 xaxis = list(title = ""), yaxis = list(title = ""), 
                 margin = list(r = -500),
                 showlegend= FALSE, paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%  
          config(locale = "fr-ch")
        
      })
      
      #compter le nombre d'entreprises
      entreprise_count <- sum(occupant_graph_count$n)
      
      output$nombre_entreprise <- renderText({
        paste0(format(entreprise_count, big.mark = " "))
      })
      
      
      #calcuer la densite entreprise dans l'isochrone
      iso_occupant_densite <- iso_occupant_reactive() %>% 
        mutate(iso_superficie = as.integer(st_area(geometry)))
      
      print(iso_occupant_densite $iso_superficie) #NULL!
      
      iso_km <- unique(iso_occupant_densite$iso_superficie) / 1000000
      
      entreprise_densite <- round(entreprise_count / iso_km, 0)
      
      
      
      output$densite_entreprise <- renderText({
        paste0(format(entreprise_densite, big.mark = " "), " km²")
      })
      
    })
    
    
  })
}