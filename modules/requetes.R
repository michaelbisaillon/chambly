requete_UI <- function(id, session){
  
  ns <- NS(id)
  
  ui <- page_sidebar(
    sidebar = sidebar(title = tags$h5("Menu de recherche"), position = "left", width = 500,  
                      #br(),
                      #br(),
                      accordion(id = ns("type_requete"), 
                                multiple = FALSE,
                                accordion_panel(title = "Recherche par entreprise", icon = bsicons::bs_icon("shop"), open = TRUE,
                                                  pickerInput(ns("barre_recherche_entreprise"), label = '', choices = NULL, multiple = TRUE, 
                                                            options = list(placeholder = "Ex: Animalerie Chambly", `actions-box` = TRUE, `select-all-text` = "Sélectionner tout", `deselect-all-text` =  "Désélectionner tout", 
                                                                           `none-selected-text` = "Désélectionner tout", `live-search` = TRUE))
                                                
                                ),
                                accordion_panel(title = "Recherche par adresse", icon = bsicons::bs_icon("building-fill"),
                                                pickerInput(ns("barre_recherche_rue"), label = '', choices = NULL, multiple = FALSE, options = list(placeholder = "Sélectionner une rue", `live-search` = TRUE)),
                                                pickerInput(ns("barre_recherche_no_civique"), label = '', choices = NULL, multiple = TRUE, 
                                                            options = list(placeholder = "Sélectionner un no. civique", `actions-box` = TRUE, `select-all-text` = "Sélectionner tout", `deselect-all-text` =  "Désélectionner tout", `none-selected-text` = "Sélectionner un numéro civique"))
                                ),
                                accordion_panel(title = "Recherche par plage d'adresse", icon = bsicons::bs_icon("arrows"),
                                                pickerInput(ns("barre_recherche_rue_plage"), label = '', choices = NULL, multiple = FALSE, options = list(placeholder = "Rechercher une rue", `live-search` = TRUE)),
                                                numericInput(ns("barre_recherche_no_civique_min"), "Numéro civique minimum", value = NULL),
                                                numericInput(ns("barre_recherche_no_civique_max"), "Numéro civique maximum",  value = NULL),
                                                checkboxGroupInput(ns("barre_recherche_no_civique_pair"), "Numéros civiques pairs/impairs", inline = TRUE, choices = NULL, selected = NULL),
                                ),
                                accordion_panel(title = "Recherche par usage", icon = bsicons::bs_icon("search"),
                                                pickerInput(ns("barre_recherche_usage_1"), label = "", choices = NULL, multiple = TRUE, 
                                                            options = list(placeholder = "Sélectionner un usage 1", `actions-box` = TRUE, `select-all-text` = "Sélectionner tout", `deselect-all-text` =  "Désélectionner tout", `none-selected-text` = "Sélectionner un usage 1", `live-search` = TRUE)),
                                                pickerInput(ns("barre_recherche_usage_2"), label = "", choices = NULL, multiple = TRUE, 
                                                            options = list(placeholder = "Sélectionner un usage 2", `actions-box` = TRUE, `select-all-text` = "Sélectionner tout", `deselect-all-text` =  "Désélectionner tout", `none-selected-text` = "Sélectionner un usage 2", `live-search` = TRUE)),
                                                pickerInput(ns("barre_recherche_usage_3"), label = "", choices = NULL, multiple = TRUE, 
                                                            options = list(placeholder = "Sélectionner un usage 3", `actions-box` = TRUE, `select-all-text` = "Sélectionner tout", `deselect-all-text` =  "Désélectionner tout", `none-selected-text` = "Sélectionner un usage 3", `live-search` = TRUE))
                                ),
                                accordion_panel(title = "Recherche par SCIAN", icon = bsicons::bs_icon("search"),
                                                pickerInput(ns("barre_recherche_scian"), label = "", choices = NULL, multiple = TRUE, 
                                                            options = list(placeholder = "Sélectionner un SCIAN", `actions-box` = TRUE, `select-all-text` = "Sélectionner tout", `deselect-all-text` =  "Désélectionner tout", `none-selected-text` = "Sélectionner un SCIAN", `live-search` = TRUE)),
                                )
                                
                                #)
                      )
    ),
    layout_columns(gap = "0px",
                   #col_widths = c(12),
                   col_heights = c(12),
    navset_card_pill(
      nav_panel("Carte",  icon = icon("map"), 
                card_body(class = "p-0", fill = TRUE, fillable = TRUE,
                          height = "72vh",
                          #fillable = TRUE,
                          leafletOutput(ns("carte_recherche"), width = "100%")
                )
      ),
      nav_panel("Tableau", icon = icon("border-all"), 
                card_body(
                  height = "65vh",
                #tags$style(HTML(paste0("#", ns("tableau_recherche"), " .dataTables_scrollBody {max-height: 72vh !important;}"))),
                DT::dataTableOutput(ns("tableau_recherche"))
                )
                
      ),
      nav_panel("Informations", icon = icon("question-circle"),
                tags$h5("Comment effectuer une recherche?"),
                tags$p("La recherche par locaux ou entreprise permet de trouver des informations sur les établissements situés sur le territoire de la Ville de Chambly. Les informations disponibles sont les suivantes :"),
                tags$ul(
                #br(),
                tags$li("Pour effectuer une recherche, vous pouvez utiliser les différents filtres disponibles dans le menu de recherche situé à gauche. Vous pouvez faire une recherche par entreprise, par adresse, par plage d'adresse, par usage ou encore par SCIAN."),
                #br(),
                tags$li("Les résultats s'affichent sur la carte et dans le tableau. Vous pouvez cliquer sur un marqueur pour obtenir plus d'informations sur un établissement. Il est possible d'exporter le tableau en format Excel, CSV ou encore de copier le tableau dans le presse-papier.")
                )
      ),
      card_footer(class = "fs-6",
                  textOutput(ns("nombre_resultats")))
    )
    )
    
  )
}

requete_server <- function(id, data) {
  moduleServer(id, function(input, output, session){
    
    query_entreprise_reactive <- reactive({
      occupant_dataset %>% 
        distinct(entreprise) %>% 
        arrange(entreprise) %>% 
        collect() %>% 
        pull(entreprise)
    })
    
    query_rue_reactive <- reactive({
      occupant_dataset %>% 
        distinct(rue) %>% 
        #select(rue) %>% 
        arrange(rue) %>% 
        collect() %>% 
        pull(rue)
    })
    
    query_usage_1_reactive <- reactive({
      occupant_dataset %>% 
        distinct(usage_1) %>%
        arrange(usage_1) %>% 
        collect() %>% 
        pull(usage_1)
    })
    
    query_usage_2_reactive <- reactive({
      occupant_dataset %>% 
        distinct(usage_2) %>% 
        arrange(usage_2) %>% 
        collect() %>% 
        pull(usage_2)
    })
    
    query_usage_3_reactive <- reactive({
      occupant_dataset %>% 
        distinct(usage_3) %>% 
        arrange(usage_3) %>% 
        collect() %>% 
        pull(usage_3)
    })
    
    query_scian_reactive <- reactive({
      occupant_dataset %>% 
        distinct(scian) %>% 
        arrange(scian) %>% 
        collect() %>% 
        pull(scian)
    })
    
    #-----------------------
    #updates inputs
    #-----------------------
    
    # Exemple d'utilisation d'un objet réactif dans une mise à jour d'input
    observe({
      updateSelectizeInput(session, "barre_recherche", choices = c(query_usage_1_reactive(), query_usage_2_reactive(), query_usage_3_reactive(), query_entreprise_reactive()), server = TRUE, options = list(maxOptions = 5))
    })
    
    
    #update recherche entreprise
    observe({
      updatePickerInput(session, "barre_recherche_entreprise", choices = query_entreprise_reactive())
    })
    
    #update recherche adresse
    observe({
      updatePickerInput(session, "barre_recherche_rue", choices = query_rue_reactive())
    })
    
    # Observer la sélection de la rue pour mettre à jour les numéros civiques
    observeEvent(input$barre_recherche_rue, {
      #if (input$barre_recherche_rue != "") {
        no_civiques <- occupant_dataset %>%
          filter(rue == input$barre_recherche_rue) %>%
          #select(no_civique) %>%
          distinct(no_civique) %>%
          arrange(no_civique) %>%
          collect() %>%
          pull(no_civique)
      #} else {
       # no_civiques <- character(0)  # Pas de rue sélectionnée, donc pas de numéros civiques
      #}
      
      # Mettre à jour le selectizeInput pour les numéros civiques
      updatePickerInput(session, "barre_recherche_no_civique", choices = no_civiques)
    })
    
    
    #update recherche adresse plage
    observe({
      updatePickerInput(session, "barre_recherche_rue_plage", choices = query_rue_reactive())
    })
    
    # Observer les changements de sélection de rue
    observe({
      
      obj <- occupant_dataset %>%
        filter(rue %in% input$barre_recherche_rue_plage) %>%
        collect()
      
      # Mettre à jour les valeurs min et max pour les numéros civiques
      updateNumericInput(session, "barre_recherche_no_civique_min", value = min(obj$no_civique, na.rm = TRUE))
      updateNumericInput(session, "barre_recherche_no_civique_max", value = max(obj$no_civique, na.rm = TRUE))
      
      # Mettre à jour les choix pour les numéros civiques pairs et impairs
      updateCheckboxGroupInput(session, "barre_recherche_no_civique_pair", choices = c("Pairs" = "pair", "Impairs" = "impair"), selected = c("pair", "impair"))
      #}
    })
    
    #update recherche usage
    observe({
      updatePickerInput(session, "barre_recherche_usage_1", choices = query_usage_1_reactive())
    })
    
    # Observer les changements de sélection de `usage_1`
    observe({
      # Filtrez le dataset Arrow pour les objets correspondants à `usage_1`
      obj <- occupant_dataset %>%
        filter(usage_1 %in% input$barre_recherche_usage_1) %>%
        collect()
      
      # Mettez à jour les choix pour `usage_2` en fonction de la sélection `usage_1`
      updatePickerInput(session, "barre_recherche_usage_2", choices = unique(sort(obj$usage_2)))
    })
    
    # Observer les changements de sélection de `usage_2`
    observe({
      # Filtrez le dataset Arrow pour les objets correspondants à `usage_2`
      obj <- occupant_dataset %>%
        filter(usage_2 %in% input$barre_recherche_usage_2) %>%
        collect()
      
      # Mettez à jour les choix pour `usage_3` en fonction de la sélection `usage_2`
      updatePickerInput(session, "barre_recherche_usage_3", choices = unique(sort(obj$usage_3)))
    })
    
    #update recherche scian
    observe({
      updatePickerInput(session, "barre_recherche_scian", choices = query_scian_reactive())
    })
    
    occupant_recherche <- reactive({
      # Commencez avec l'ensemble de données complet
      filtered_data <- occupant_dataset |> read_sf_dataset()
      
      req(input$type_requete %in% c("Recherche par entreprise", "Recherche par adresse", "Recherche par plage d'adresse", "Recherche par usage", "Recherche par SCIAN"))   
      
      if (input$type_requete == "Recherche par entreprise") {
        
        filtered_data <- filtered_data %>%
          filter(entreprise %in% input$barre_recherche_entreprise) 
        
      } else if (input$type_requete == "Recherche par adresse") {
        
        filtered_data <- filtered_data %>%
          filter(rue %in% input$barre_recherche_rue & no_civique %in% input$barre_recherche_no_civique)
        
      } else if(input$type_requete == "Recherche par plage d'adresse") {
        
        filtered_data <- filtered_data %>%
          filter(rue %in% input$barre_recherche_rue_plage) %>% 
          filter(no_civique >= input$barre_recherche_no_civique_min & no_civique <= input$barre_recherche_no_civique_max) %>%
          filter(odd %in% input$barre_recherche_no_civique_pair) 
        
        
      } else if(input$type_requete == "Recherche par usage"){
        
        filtered_data <- filtered_data %>%
          filter(usage_1 %in% input$barre_recherche_usage_1 &
                   usage_2 %in% input$barre_recherche_usage_2 &
                   usage_3 %in% input$barre_recherche_usage_3) 
        
      } else if(input$type_requete == "Recherche par SCIAN") {
        filtered_data <- filtered_data %>%
          filter(scian %in% input$barre_recherche_scian) 
        
      }
      
      
      return(filtered_data)
    })
    
    
    #---------------------------
    #Affichage du nombre de résultats trouvés
    #---------------------------
    
    # Utilisez un bloc renderText pour mettre à jour output$nombre_resultats basé sur processed_occupant
    output$nombre_resultats <- renderText({
      
      if (is.null(occupant_recherche()) || nrow(occupant_recherche()) == 0) {
      occupant <- occupant_dataset %>% 
        read_sf_dataset() |> count()
      # Si occupant_recherche est nul ou vide, renvoyez le jeu de données 'occupant'
      occupant_recherche <- occupant # Assurez-vous que 'occupant' est le nom de votre dataframe par défaut
    } else {
      #req(occupant_recherche())
      # Sinon, traitez occupant_recherche et retournez le résultat
      occupant_recherche <- occupant_recherche() |> count()
      
    }
      
      n_occupant <- occupant_recherche # Utilisez la valeur reactive
      
      paste0(n_occupant$n, " ", "élément(s) trouvé(s)")
      
    })
    
    #---------------------------
    #carte leaflet
    #---------------------------
    output$carte_recherche <- renderLeaflet({
      
      occupant <- occupant_dataset %>% 
        read_sf_dataset()

      popup <- paste(
        '<div style="font-family: Onest, sans-serif; color: #203245;">', #appliquer la police au popup
        "<h5>", occupant$entreprise, "<br></h5>",
        "<b>", occupant$no_civique, occupant$rue, "<br>", "Chambly,", "Québec", occupant$code_postal, "</b><br>",
        "<b>","Usage: ", "</b>", occupant$usage_3, "<br><br>",
        "<b>","Téléphone: ", "</b>", occupant$telephone, "<br>",
        "<b>","Courriel: ", "</b>", occupant$courriel , "<br>"
      )
      
      leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE, minZoom = 12)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = sdr, fillOpacity = 0, color = "#203245", weight = 1, opacity = 1, dashArray = 5) %>% 
        addCircleMarkers(data = occupant, weight = 1, radius = 5, color = "white", fillColor = "#203245", opacity = 1, fillOpacity = 1, popup = popup) 
    })
    
    observe({
      
      
      if (is.null(occupant_recherche()) || nrow(occupant_recherche()) == 0) {
        
        occupant <- occupant_dataset %>% 
          read_sf_dataset()
        # Si occupant_recherche est nul ou vide, renvoyez le jeu de données 'occupant'
        occupant_recherche <- occupant # Assurez-vous que 'occupant' est le nom de votre dataframe par défaut
      } else {
        req(occupant_recherche())
        # Sinon, traitez occupant_recherche et retournez le résultat
        occupant_recherche <- occupant_recherche() 
        
      }
      
      popup <- paste(
        '<div style="font-family: Onest, sans-serif; color: #203245;">', #appliquer la police au popup
        "<h5>",  occupant_recherche$entreprise, "<br></h5>",
        "<b>",  occupant_recherche$no_civique,  occupant_recherche$rue, "<br>", "Chambly,", "Québec",  occupant_recherche$code_postal, "</b><br>",
        occupant_recherche$usage_3, "<br><br>",
        "<b>","Téléphone: ", "</b>",  occupant_recherche$telephone, "<br>",
        "<b>","Courriel: ", "</b>",  occupant_recherche$courriel, "<br>"
      )
      
      recherche_box <-  occupant_recherche %>% st_bbox() %>% as.vector()
      
      leafletProxy("carte_recherche", data =  occupant_recherche) %>%
        clearMarkers() %>%
        addCircleMarkers(weight = 1, radius = 5, color = "white", fillColor = "#203245", opacity = 1, fillOpacity = 1, popup = popup) %>%
        fitBounds(recherche_box[1], recherche_box[2], recherche_box[3], recherche_box[4], options = list(maxZoom = 19))
      
      
    })
    
    #---------------------------
    #tablau de recherche
    #---------------------------
    output$tableau_recherche <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      if (is.null(occupant_recherche()) || nrow(occupant_recherche()) == 0) {
        occupant <- occupant_dataset %>% 
          read_sf_dataset()  |> st_drop_geometry()
        # Si occupant_recherche est nul ou vide, renvoyez le jeu de données 'occupant'
        occupant_recherche <- occupant # Assurez-vous que 'occupant' est le nom de votre dataframe par défaut
      } else {
        req(occupant_recherche())
        # Sinon, traitez occupant_recherche et retournez le résultat
        occupant_recherche <- occupant_recherche() |> st_drop_geometry()
        
      }
      
      occupant_recherche <- occupant_recherche %>% 
        select(ville, no_civique, rue, code_postal, no_local, entreprise, fonction, etage, usage_1, usage_2, usage_3, code_scian, telephone, courriel, site_internet, enfant, grande_chaine) %>% 
        rename(Ville = ville, "No civique" = no_civique, Rue = rue, "Code postal" = code_postal, "No. local" = no_local, Entreprise = entreprise, Fonction = fonction, Étage = etage, "Usage 1" = usage_1, "Usage 2" = usage_2, "Usage 3" = usage_3, SCIAN = code_scian, Téléphone = telephone, Courriel = courriel, "Site internet" = site_internet, Enfant = enfant, "Grande chaîne" = grande_chaine)
      
      DT::datatable(occupant_recherche,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap4",
                    extensions = c("Buttons", "FixedColumns"),
                    options = list(
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list(
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'  # Texte du bouton pour 'excel'
                        )
                      )
                    ),
                    escape = FALSE)
    })
    
  })
}