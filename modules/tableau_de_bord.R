tableau_de_bord_UI <- function(id, session){
  
  ns <- NS(id)
  
  ui <- page_sidebar(
    sidebar = sidebar(position = "left", width = 500,
                      accordion(id = ns("type_theme"), multiple = FALSE,
                                accordion_panel(title = "Portrait sociodémographique", icon = bsicons::bs_icon("person-fill"), open = TRUE,
                                                pickerInput(ns("socio_sujet"), label = "", choices = c("Population", "Revenus et dépenses", "Scolarité", "Immigration"), selected = "Population", multiple = FALSE, options = list(placeholder = "Sélectionner un thème")),
                                                pickerInput(ns("socio_zone"), label = "", choices = c("Chambly"), selected = "Chambly", multiple = FALSE, options = list(placeholder = "Sélectionner une zone"))
                                ),
                                accordion_panel(title = "Portrait commercial et industriel", icon = bsicons::bs_icon("shop"), open = TRUE,
                                                pickerInput(ns("commercial_sujet"), label = "", choices = c("Mix commercial", "Vacance", "Taux de roulement"), multiple = FALSE, selected = "Mix commercial", options = list(placeholder = "Sélectionner un thème")),
                                                pickerInput(ns("commercial_zone"), label = "", choices = c("Chambly"), selected = "Chambly", multiple = FALSE, options = list(placeholder = "Sélectionner une zone"))
                                )
                      )
    ),
    conditionalPanel(
      condition = "input.type_theme == 'Portrait sociodémographique'", ns = ns,
      layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        
        
        #conditionalPanel(
        # condition = "input.type_theme == 'Portrait sociodémographique'", ns = ns,
        value_box(title = 'Population', 
                  theme = "primary",
                  #value = "31 444",
                  textOutput(ns("vb_population"))),
        value_box(title = "Densité de population",
                  theme = "primary",
                  #value = paste("1 254", "km²"),
                  textOutput(ns("vb_densite_population"))),     
        value_box(title = "Taille moyenne des ménages", 
                  #value = "2,5",
                  theme = "primary",
                  textOutput(ns("vb_nombre_personne_menage"))),
        value_box(title = 'Age moyen',
                  theme = "primary",
                  #value = "39 ans",
                  textOutput(ns("vb_age"))),
        value_box(title = "Revenu médian des ménages", 
                  theme = "primary",
                  #value = "101 000$",
                  textOutput(ns("vb_revenu_median"))),
        value_box(title = "% avec études postsecondaires",
                  theme = "primary",
                  #value = "78%",
                  textOutput(ns("vb_etudes_postsecondaires")))
        #)
      ),
      conditionalPanel(
        condition = "input.type_theme == 'Portrait sociodémographique' && input.socio_sujet == 'Population'", ns = ns,
        navset_card_pill(
          #height = 900,   # Permettre au navset de prendre toute la hauteur disponible
          nav_panel("Diagramme", icon = icon("bar-chart"),
                    #fillable = TRUE,  # Marque ce conteneur comme remplissable
                    #as_fill_carrier(
                    layout_columns(
                      col_widths = c(6, 6, 6),
                      #fill = TRUE,
                      #fillable = TRUE,  # Marque ce conteneur comme remplissable
                      card(
                        full_screen = TRUE,
                        #fill = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Population", class = "text-white")),
                        plotlyOutput(ns("diagramme_population"))
                        #height = "100%" 
                      ),
                      card(
                        full_screen = TRUE,
                        #fill = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Densité de population", class = "text-white")),
                        plotlyOutput(ns("diagramme_densite"))
                        #height = "100%" 
                      ),
                      card(
                        full_screen = TRUE,
                        #fill = TRUE,
                        card_header(
                          class = "custom-header-color",
                          tags$h5("Groupes d'âge", class = "text-white")),
                        plotlyOutput(ns("diagramme_age"))
                        #height = "100%" 
                      )
                      # )
                    )
          ),
          nav_panel("Tableau", icon = icon("border-all"),
                    #fillable = FALSE,  # Marque ce conteneur comme remplissable
                    layout_columns(
                      col_widths = c(6, 6, 6),
                      row_heights = list("auto", 1),
                      card(
                        full_screen = TRUE,
                        fill = FALSE,
                        height = 300,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Population", class = "text-white")),
                        DT::dataTableOutput(ns("tableau_population"))                      ),
                      card(
                        full_screen = TRUE,
                        fill = FALSE,
                        height = 300,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Densité de population", class = "text-white")),
                        #gt_output(ns("tableau_densite"))
                        DT::dataTableOutput(ns("tableau_densite"))
                      ),
                      card(
                        full_screen = TRUE,
                        fill = FALSE,
                        #height = 400,
                        card_header(
                          class = "custom-header-color",
                          tags$h5("Groupes d'âge", class = "text-white")),
                        #gt_output(ns("tableau_age"))
                        DT::dataTableOutput(ns("tableau_age"))
                      )
                    )
          ),
          nav_panel("Informations", icon = icon("question-circle"),
                    tags$h5("Comment effectuer une recherche?"),
                    tags$p("Le tableau de bord permet de trouver des informations sur la structure commerciale et le recensement le plus récent de la population."),
                    tags$p("Pour effectuer une recherche, vous pouvez utiliser les différents filtres disponibles dans le menu de recherche situé à gauche.
                           Il est possible de visualiser les résultats avec les diagrammes ou avec des tableaux.")
          )
        )
      ),
      conditionalPanel(
        condition = "input.type_theme == 'Portrait sociodémographique' && input.socio_sujet == 'Revenus et dépenses'", ns = ns,
        navset_card_pill(
          #height =1500,
          nav_panel("Diagramme", icon = icon("bar-chart"),
                    #fillable = TRUE,  # Marque ce conteneur comme remplissable
                    layout_columns(
                      col_widths = c(4, 8, 12),
                      #fill = TRUE,
                      #fillable = TRUE,  # Marque ce conteneur comme remplissable
                      card(
                        full_screen = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Revenu médian des ménages", class = "text-white")),
                        plotlyOutput(ns("diagramme_revenus"))
                      ),
                      card(
                        full_screen = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Dépenses des ménages (Québec)", class = "text-white")),
                        plotlyOutput(ns("diagramme_depenses_menages_2021"))
                      ),
                      card(
                        full_screen = TRUE,
                        height = 1000,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Dépenses des ménages (Québec)", class = "text-white")),
                        plotlyOutput(ns("diagramme_depenses_menages_all"))
                      )
                      
                    )
          ),
          nav_panel("Tableau", icon = icon("border-all"),
                    fillable = TRUE,  # Marque ce conteneur comme remplissable
                    layout_columns(
                      col_widths = c(6, 6),
                      card(
                        full_screen = TRUE,
                        fill = FALSE,
                        height = 300,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Revenu médian des ménages", class = "text-white")),
                        #gt_output(ns("tableau_revenus"))
                        DT::dataTableOutput(ns("tableau_revenus"))
                      ),
                      card(
                        #fill = TRUE,
                        height = 700,
                        full_screen = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Dépenses des ménages (Québec)", class = "text-white")),
                        #gt_output(ns("tableau_depenses_menages_2021"))
                        DT::dataTableOutput(ns("tableau_depenses_menages_2021"))
                      )
                    )
          ),
          nav_panel("Informations", icon = icon("question-circle"),
                    tags$h5("Comment effectuer une recherche?"),
                    tags$p("Le tableau de bord permet de trouver des informations sur la structure commerciale et le recensement le plus récent de la population."),
                    tags$p("Pour effectuer une recherche, vous pouvez utiliser les différents filtres disponibles dans le menu de recherche situé à gauche.
                           Il est possible de visualiser les résultats avec les diagrammes ou avec des tableaux.")
          )
        )
      ),
      conditionalPanel(
        condition = "input.type_theme == 'Portrait sociodémographique' && input.socio_sujet == 'Scolarité'", ns = ns,
        navset_card_pill(
          height =500,
          nav_panel("Diagramme", icon = icon("bar-chart"),
                    layout_columns(
                      col_widths = c(6,6),
                      card(
                        full_screen = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Dîplomés études postsecondaires (%)", class = "text-white")),
                        plotlyOutput(ns("diagramme_diplomes_postsecondaire"))
                      ),
                      card(
                        full_screen = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Niveau de scolarité chez les 25 ans et plus", class = "text-white")),
                        plotlyOutput(ns("diagramme_scolarite_25_ans"))
                      )
                      
                    )
          ),
          nav_panel("Tableau", icon = icon("border-all"),
                    layout_columns(
                      col_widths = c(6, 6),
                      card(
                        full_screen = TRUE,
                        fill = FALSE,
                        height = 300,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Dîplomés études postsecondaires (%)", class = "text-white")),
                        #gt_output(ns("tableau_diplomes_postsecondaire"))
                        DT::dataTableOutput(ns("tableau_diplomes_postsecondaire"))
                      ),
                      card(
                        full_screen = TRUE,
                        fill = FALSE,
                        height = 300,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Niveau de scolarité chez les 25 ans et plus", class = "text-white")),
                        #gt_output(ns("tableau_scolarite_25_ans"))
                        DT::dataTableOutput(ns("tableau_scolarite_25_ans"))
                      )
                    )
          ),
          nav_panel("Informations", icon = icon("question-circle"),
                    tags$h5("Comment effectuer une recherche?"),
                    tags$p("Le tableau de bord permet de trouver des informations sur la structure commerciale et le recensement le plus récent de la population."),
                    tags$p("Pour effectuer une recherche, vous pouvez utiliser les différents filtres disponibles dans le menu de recherche situé à gauche.
                           Il est possible de visualiser les résultats avec les diagrammes ou avec des tableaux.")
          )
        )
      ),
      conditionalPanel(
        condition = "input.type_theme == 'Portrait sociodémographique' && input.socio_sujet == 'Immigration'", ns = ns,
        navset_card_pill(
          height =500,
          nav_panel("Diagramme", icon = icon("bar-chart"),
                    layout_columns(
                      col_widths = c(6),
                      card(
                        full_screen = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("% immigrants", class = "text-white")),
                        plotlyOutput(ns("diagramme_immigration"))
                      )
                    )
          ),
          nav_panel("Tableau", icon = icon("border-all"),
                    layout_columns(
                      col_widths = c(6),
                      card(
                        full_screen = TRUE,
                        fill = FALSE,
                        height = 300,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("% immigrants", class = "text-white")),
                        #gt_output(ns("tableau_immigration"))
                        DT::dataTableOutput(ns("tableau_immigration"))
                      )
                    )
          ),
          nav_panel("Informations", icon = icon("question-circle"),
                    tags$h5("Comment effectuer une recherche?"),
                    tags$p("Le tableau de bord permet de trouver des informations sur la structure commerciale et le recensement le plus récent de la population."),
                    tags$p("Pour effectuer une recherche, vous pouvez utiliser les différents filtres disponibles dans le menu de recherche situé à gauche.
                           Il est possible de visualiser les résultats avec les diagrammes ou avec des tableaux.")
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.type_theme == 'Portrait commercial et industriel'", ns = ns,
      layout_columns(
        col_widths = c(2, 2, 2, 2, 2),
        value_box(title = "Nombre d'entreprises", 
                  theme = "primary",
                  textOutput(ns("vb_n_entreprises"))
        ),
        value_box(title = "Nombre de locaux",
                  theme = "primary",
                  textOutput(ns("vb_n_locaux"))
        ),
        value_box(title = "Type d'entreprise la plus répandue", 
                  textOutput(ns("vb_entreprise_rependue")),
                  theme = "primary"),
        value_box(title = 'Taux de roulement',
                  theme = "primary",
                  textOutput(ns("vb_taux_roulement")), 
        ),
        value_box(title = "Vacance", 
                  theme = "primary",
                  #value = "8%",
                  textOutput(ns("vb_vacance")), 
        )
      ),
      conditionalPanel(
        condition = "input.type_theme == 'Portrait commercial et industriel' && input.commercial_sujet == 'Mix commercial'", ns = ns,
        navset_card_pill(
          height =800,
          nav_panel("Diagramme", icon = icon("bar-chart"),
                    layout_columns(
                      col_widths = c(12),
                      card(
                        full_screen = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Mix commercial", class = "text-white")),
                        plotlyOutput(ns("diagramme_mix_commercial"))
                      )
                    )
          ),
          nav_panel("Tableau", icon = icon("border-all"),
                    layout_columns(
                      col_widths = c(6),
                      card(
                        full_screen = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Mix commercial", class = "text-white")),
                        #gt_output(ns("tableau_mix_commercial"))
                        DT::dataTableOutput(ns("tableau_mix_commercial"))
                      )
                    )
          ),
          nav_panel("Informations", icon = icon("question-circle"),
                    tags$h5("Comment effectuer une recherche?"),
                    tags$p("Le tableau de bord permet de trouver des informations sur la structure commerciale et le recensement le plus récent de la population."),
                    tags$p("Pour effectuer une recherche, vous pouvez utiliser les différents filtres disponibles dans le menu de recherche situé à gauche.
                           Il est possible de visualiser les résultats avec les diagrammes ou avec des tableaux.")
          )
        )
      ) ,
      conditionalPanel(
        condition = "input.type_theme == 'Portrait commercial et industriel' && input.commercial_sujet == 'Vacance'", ns = ns,
        navset_card_pill(
          height =500,
          nav_panel("Diagramme", icon = icon("bar-chart"),
                    layout_columns(
                      col_widths = c(6),
                      card(
                        full_screen = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Vacance", class = "text-white")),
                        plotlyOutput(ns("diagramme_vacance"))
                      )
                    )
          ),
          nav_panel("Tableau", icon = icon("border-all"),
                    layout_columns(
                      col_widths = c(6),
                      card(
                        full_screen = TRUE,
                        fill = FALSE,
                        height = 400,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Vacance", class = "text-white")),
                        #gt_output(ns("tableau_vacance"))
                        DT::dataTableOutput(ns("tableau_vacance"))
                      )
                    )
          ),
          nav_panel("Informations", icon = icon("question-circle"),
                    tags$h5("Comment effectuer une recherche?"),
                    tags$p("Le tableau de bord permet de trouver des informations sur la structure commerciale et le recensement le plus récent de la population."),
                    tags$p("Pour effectuer une recherche, vous pouvez utiliser les différents filtres disponibles dans le menu de recherche situé à gauche.
                           Il est possible de visualiser les résultats avec les diagrammes ou avec des tableaux.")
          )
        )
      ),
      conditionalPanel(
        condition = "input.type_theme == 'Portrait commercial et industriel' && input.commercial_sujet == 'Taux de roulement'", ns = ns,
        navset_card_pill(
          height =500,
          nav_panel("Diagramme", icon = icon("bar-chart"),
                    layout_columns(
                      col_widths = c(6),
                      card(
                        full_screen = TRUE,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Taux de roulement par fonction", class = "text-white")),
                        plotlyOutput(ns("diagramme_changement_occ"))
                      )
                    )
          ),
          nav_panel("Tableau", icon = icon("border-all"),
                    layout_columns(
                      col_widths = c(6),
                      card(
                        full_screen = TRUE,
                        fill = FALSE,
                        height = 400,
                        card_header(
                          class = "custom-header-color", 
                          tags$h5("Taux de roulement par fonction", class = "text-white")),
                        #gt_output(ns("tableau_changement_occ"))
                        DT::dataTableOutput(ns("tableau_changement_occ"))
                      )
                    )
          ),
          nav_panel("Informations", icon = icon("question-circle"),
                    tags$h5("Comment effectuer une recherche?"),
                    tags$p("Le tableau de bord permet de trouver des informations sur la structure commerciale et le recensement le plus récent de la population."),
                    tags$p("Pour effectuer une recherche, vous pouvez utiliser les différents filtres disponibles dans le menu de recherche situé à gauche.
                           Il est possible de visualiser les résultats avec les diagrammes ou avec des tableaux.")
          )
        )
      ) 
      
    )
  )
}

tableau_de_bord_server <- function(id, data) {
  moduleServer(id, function(input, output, session){
    
    ns <- NS(id)
    
    #-----------------------------
    #value box
    #-----------------------------
    
    #sociodémographique
    output$vb_population <- renderText({
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques == 1) |> 
        collect() %>% 
        filter(annee == max(annee)) %>% 
        pull(chiffre_total) %>% 
        format(big.mark = " ")
      
    })
    
    #densité de population
    output$vb_densite_population <- renderText({
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques == 6) |> 
        collect() %>% 
        filter(annee == max(annee)) %>% 
        mutate(chiffre_total = round(chiffre_total, 0)) %>%
        pull(chiffre_total) %>% 
        format(big.mark = " ")
      
    })
    
    #nombre de personnes par ménage
    output$vb_nombre_personne_menage <- renderText({
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques == 57) |> 
        collect() %>% 
        filter(annee == max(annee)) %>% 
        pull(chiffre_total) %>% 
        format(big.mark = " ")
      
    })
    
    #age moyen
    output$vb_age <- renderText({
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques == 39) |> 
        collect() %>% 
        filter(annee == max(annee)) %>% 
        mutate(chiffre_total = round(chiffre_total, 0)) %>%
        pull(chiffre_total) %>% 
        format(big.mark = " ")
      
    })
    
    #revenu median des ménages
    output$vb_revenu_median <- renderText({
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques == 243 & str_detect(theme, "Revenu")) |> 
        collect() %>% 
        filter(annee == max(annee)) %>% 
        pull(chiffre_total) %>% 
        format(big.mark = " ", scientific = FALSE) %>% 
        paste0("$")
      
    })
    
    #études postsecondaires
    output$vb_etudes_postsecondaires <- renderText({
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques == 2017 & str_detect(theme, "diplôme")) |> 
        collect() %>% 
        filter(annee == max(annee)) %>% 
        pull(taux_total) %>% 
        format(big.mark = " ") %>% 
        paste0("%")
      
    })
    
    #portrait commercial
    
    #nombre d'entreprises
    output$vb_n_entreprises <- renderText({
      
      base <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        filter(!usage_1 == "VACANT") |>
        count() %>% 
        collect() %>% 
        pull(n)
      
    })
    
    #nombre de locaux
    output$vb_n_locaux <- renderText({
      
      base <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        count() %>% 
        collect() %>% 
        pull(n)
      
    })
    
    #entreprise la plus répandue
    output$vb_entreprise_rependue <- renderText({
      
      base <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        filter(!usage_1 == "VACANT") |>
        group_by(usage_2) |> 
        count() |> 
        arrange(desc(n)) |> 
        ungroup() |> 
        collect() |> 
        slice(1) |> 
        pull(usage_2)
      
    })
    
    #taux de roulement commercial
    output$vb_taux_roulement <- renderText({
      
      base <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        filter(!fonction %in% c("Institution", "Résidentiel")) |>
        group_by(statut_1) %>% 
        count() %>% 
        ungroup() %>% 
        collect() %>% 
        mutate(pourcentage = round_half_up(n/sum(n) * 100, 1)) %>% 
        mutate(chg_occ = 100 - pourcentage) %>% 
        filter(statut_1 == "À jour") %>%
        pull(chg_occ) %>% 
        paste0(., "%")
      
    })
    
    #vacance
    output$vb_vacance <- renderText({
      
      base <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        group_by(usage_1) %>% 
        count() %>% 
        ungroup() %>%
        collect() %>% 
        mutate(pourcentage = round_half_up(n/sum(n) * 100, 1)) %>% 
        filter(usage_1 == "VACANT") %>% 
        pull(pourcentage) %>% 
        paste0(., "%")
      
    })
    
    #----------------------
    #diagrammes portrait commercial
    #---------------------
    
    output$diagramme_mix_commercial <- renderPlotly({
      
      #req(input$commercial_sujet == "Mix commercial")
      
      mix_commercial <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        filter(!usage_1 == "VACANT") |>
        group_by(usage_1) |> 
        count() |> 
        arrange(desc(n)) |> 
        ungroup() |> 
        collect() |> 
        mutate(pourcentage = round_half_up(n/sum(n), 3)) 
      
      plot_ly(mix_commercial, labels = ~usage_1,
              x = ~pourcentage,
              y = ~usage_1,
              name = "Mix commercial",
              type = "bar",
              marker = list(color = c("#0065FF"))
      ) |> 
        layout(font = f, xaxis = list(title = "", tickformat = '.0%'), yaxis = list(title = "", automargin = TRUE, categoryorder = "total ascending")) |>
        config(locale = "fr-ch")
      
      
    })
    
    #vacance
    output$diagramme_vacance <- renderPlotly({
      
      #pour attraper les fonctions vides
      reference <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        group_by(fonction) %>% 
        summarise() %>% 
        filter(!fonction %in% c("Institution", "Résidentiel")) %>%
        collect()
      
      
      vacance <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        group_by(usage_1, fonction) %>% 
        count() %>% 
        ungroup() %>%
        collect() %>% 
        group_by(fonction) %>%
        mutate(n_loc = sum(n)) %>% 
        ungroup() %>% 
        filter(usage_1 == "VACANT") %>% 
        group_by(fonction) %>% 
        summarise(n, pourcentage = round_half_up(n/n_loc, 3)) %>% 
        right_join(reference, by = "fonction") %>%
        replace_na(list(n = 0, pourcentage = 0)) 
      
      plot_ly(vacance, labels = ~fonction,
              x = ~pourcentage,
              y = ~fonction,
              name = "Vacance",
              type = "bar",
              marker = list(color = c("#0065FF"))
      ) |> 
        layout(font = f, xaxis = list(title = "", tickformat = '.0%'), yaxis = list(title = "", automargin = TRUE, categoryorder = "total ascending")) |>
        config(locale = "fr-ch")
      
    })
    
    #changement d'occupation
    output$diagramme_changement_occ <- renderPlotly({
      
      #pour attraper les fonctions vides et ou conserver les fonctions qui nous intéressent
      reference <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        group_by(fonction) %>% 
        summarise() %>% 
        filter(!fonction %in% c("Institution", "Résidentiel")) %>%
        collect()
      
      
      chang_occ_fonction <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        group_by(statut_1, fonction) %>% 
        count() %>% 
        ungroup() %>%
        group_by(fonction) %>%
        collect() %>% 
        mutate(n_loc = sum(n)) %>% 
        ungroup() %>% 
        filter(statut_1 == "À jour") %>%
        mutate(n, pourcentage = round_half_up(1 - (n/n_loc), 3)) %>% 
        right_join(reference, by = "fonction") %>%
        replace_na(list(n = 0, pourcentage = 0)) 
      
      plot_ly(chang_occ_fonction , labels = ~fonction,
              x = ~pourcentage,
              y = ~fonction,
              name = "Changement occupation",
              type = "bar",
              marker = list(color = c("#0065FF"))
      ) |> 
        layout(font = f, xaxis = list(title = "", tickformat = '.0%'), yaxis = list(title = "", automargin = TRUE, categoryorder = "total ascending")) |>
        config(locale = "fr-ch")
      
    })
    
    
    #----------------------
    #diagrammes sociodémographiques
    #---------------------
    
    #population
    output$diagramme_population <- renderPlotly({
      
      #req(input$socio_sujet == "Population")
      
      population <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques == 1) |> 
        collect()
      
      plot_ly(population, labels = ~nom_caracteristique,
              x = ~annee,
              y = ~chiffre_total,
              name = "Population",
              type = "bar",
              marker = list(color = c("#0065FF"))
      ) |> 
        layout(font = f, xaxis = list(title = ""), yaxis = list(title = "", tickformat = ',.0f', automargin = TRUE)) |>
        config(locale = "fr-ch")
      
      
    })
    
    #densite
    output$diagramme_densite <- renderPlotly({
      
      densite <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques == 6) |> 
        collect()
      
      plot_ly(densite, labels = ~nom_caracteristique,
              x = ~annee,
              y = ~chiffre_total,
              name = "Population",
              type = "bar",
              marker = list(color = c("#0065FF"))
      ) |> 
        layout(font = f, xaxis = list(title = ""), yaxis = list(title = "", tickformat = ',.0f', automargin = TRUE)) |>
        config(locale = "fr-ch")
      
      
    })
    
    #age
    output$diagramme_age <- renderPlotly({
      
      #filter les données de nom_caracteristique pour les groupes d'age
      age_df <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques %in% c(3000:3005)) |> 
        collect()
      
      
      #mutate nom_caracteristique en factor avec comme levels les groupes dâge en ordre croissant
      age_df <- age_df %>% 
        mutate(nom_caracteristique = factor(nom_caracteristique, levels = c("0 à 14 ans", "15 à 29 ans", "30 à 44 ans", "45 à 59 ans", "60 à 74 ans", "75 ans et plus"))) 
      
      fig <- plot_ly(frame = ~annee) %>%
        add_bars(data = age_df, x = ~chiffre_hommes * -1, y = ~nom_caracteristique, name = 'Hommes',
                 orientation = 'h', marker = list(color = '#111c26')) %>%
        add_bars(data = age_df, x = ~chiffre_femmes, y = ~nom_caracteristique, name = 'Femmes',
                 orientation = 'h', marker = list(color = '#0065FF')) %>% 
        layout(
          font = f,
          xaxis = list(title = "Hommes          Femmes",
                       tickmode = 'array', tickvals = c(-100000, -10000, -1000,0,  1000, 10000, 100000),
                       ticktext = c('100000', '10000', '1000', '0', '1000', '10000', '100000')
          ),
          paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
          yaxis = list(title = ""),
          bargap = 0.1,
          barmode = 'overlay',
          showlegend = FALSE
        )
      
      fig <- fig |> 
        animation_button(visible = FALSE) |> 
        config(locale = 'fr-ch')
      
      fig
      
    })
    
    #revenus
    output$diagramme_revenus <- renderPlotly({
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques  %in% c(243, 742)  & str_detect(theme, "Revenu")) |> 
        collect()
      
      
      plot_ly(base, labels = ~nom_caracteristique,
              x = ~annee,
              y = ~chiffre_total,
              name = "Revenus médian des ménages",
              type = "bar",
              marker = list(color = c("#0065FF"))
      ) |> 
        layout(font = f, xaxis = list(title = ""), yaxis = list(title = "", tickformat = ',.0f', automargin = TRUE, ticksuffix = "$")) |>
        config(locale = "fr-ch")
      
      
    })
    
    output$diagramme_depenses_menages_2021 <- renderPlotly({
      
      depenses_menages_2021 <- arrow::open_dataset("data/depenses_menages_agg_2021.parquet", format = "parquet") |> 
        collect()
      
      
      
      plot_ly(depenses_menages_2021, 
              x = ~montant,
              y = ~indicateur_depense,
              name = depenses_menages_2021$indicateur_depense,
              type = "bar",
              orientation = 'h',
              marker = list(color = c("#0065FF"))
      ) |> 
        layout(font = f, xaxis = list(title = "", ticksuffix = "$", tickformat = ',.0f'), yaxis = list(title = "", categoryorder = "total ascending"), showlegend = FALSE) |>
        config(locale = "fr-ch")
      
      
    }) 
    
    output$diagramme_depenses_menages_all <- renderPlotly({
      
      depenses_menages <- arrow::open_dataset("data/depenses_menages.parquet", format = "parquet") |> 
        collect()
      
      depenses_plot <- depenses_menages |>
        ggplot(aes(x = annee)) +
        geom_line(aes(y = montant), size = 1.5, color = "#0065FF") +
        facet_wrap(~indicateur_depense, scales = "free_y") +
        labs(
          x = "", y = "" ) +
        scale_x_continuous(breaks = seq(min(depenses_menages$annee), max(depenses_menages$annee), by = 1)) +
        scale_y_continuous(labels = function(x) paste0(format(x, big.mark = " ", scientific = FALSE), " $")) +
        theme_minimal() +
        theme(legend.position = "bottom",
              axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
              plot.caption = element_text(hjust = 0, vjust = 1, size = 8, face = "italic"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
      ggplotly(depenses_plot) |> layout(font = f)
      
      
    })  
    
    #scolarité
    
    #% postsecondaires
    output$diagramme_diplomes_postsecondaire <- renderPlotly({
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques %in% c(2017, 1701) & str_detect(theme, "diplôme")) |> 
        mutate(taux_total = ifelse(annee == 2016 & id_caracteristiques == 1701, 74.1, taux_total)) |> #ajout manuel du taux total 2016 avec id 1698
        mutate(taux_total = taux_total * 0.01) |> 
        collect()
      
      plot_ly(base, labels = ~nom_caracteristique,
              x = ~annee,
              y = ~taux_total,
              name = "Scolarité postsecondaire",
              type = "bar",
              marker = list(color = c("#0065FF"))
      ) |> 
        layout(font = f, xaxis = list(title = ""), yaxis = list(title = "", tickformat = ',.0%', automargin = TRUE)) |>
        config(locale = "fr-ch")
      
      
    })
    
    #niveau scolarité 25 ans et plus
    output$diagramme_scolarite_25_ans <- renderPlotly({
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques %in% c(2015, 2016, 2018, 2024, 1699, 1700,  1702, 1707) & str_detect(theme, "diplôme")) |> 
        mutate(taux_total = ifelse(annee == 2016 & id_caracteristiques == 1699, 7.7, taux_total)) |> #ajout manuel du taux total 2016 avec id 
        mutate(taux_total = ifelse(annee == 2016 & id_caracteristiques == 1700, 18.2, taux_total)) |> #ajout manuel du taux total 2016 avec id 
        mutate(taux_total = ifelse(annee == 2016 & id_caracteristiques == 1702, 46.1, taux_total)) |> #ajout manuel du taux total 2016 avec id 
        mutate(taux_total = ifelse(annee == 2016 & id_caracteristiques == 1707, 28, taux_total)) |> #ajout manuel du taux total 2016 avec id 
        mutate(nom_caracteristique = ifelse(id_caracteristiques == 1707, "Universitaire", nom_caracteristique)) |> #correction manuelle du nom universitaire 2016
        mutate(nom_caracteristique = ifelse(id_caracteristiques %in% c(2015, 1699), "Aucun diplôme", nom_caracteristique)) |> #correction manuelle du nom universitaire 2016
        mutate(taux_total = taux_total * 0.01) |>
        select(annee, nom_caracteristique, taux_total) |>
        collect() |> 
        pivot_wider(names_from = nom_caracteristique, values_from = taux_total) 
      
      
      plot_ly(base, labels = ~base$`Aucun diplôme`,
              x = ~annee,
              y = ~`Aucun diplôme`,
              name = "Aucun diplôme",
              type = "bar",
              marker = list(color = c("#203245"))
      ) |> 
        add_trace(y = ~base$Secondaire,
                  name = "Secondaire",
                  marker = list(color = c("#0065FF"))
        ) |>
        add_trace(y = ~`Collégial/DEP`,
                  name = "Collégial/DEP",
                  marker = list(color = c("#3797FE"))
        ) |>
        add_trace(y = ~`Universitaire`,
                  name = "Universitaire",
                  marker = list(color = c("#6FC9FD"))
        ) |>
        layout(font = f, xaxis = list(title = ""), yaxis = list(title = "", tickformat = ',.0%', automargin = TRUE)) |>
        config(locale = "fr-ch")
      
      
    })
    
    #immigration
    
    #% immigrants
    
    output$diagramme_immigration <- renderPlotly({
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques %in% c(1140, 1142, 1527, 1529) & str_detect(theme, "immigrant")) |>  
        select(annee, nom_caracteristique, chiffre_total) |>
        collect() |> 
        pivot_wider(names_from = nom_caracteristique, values_from = chiffre_total) |> 
        clean_names() |> 
        mutate(taux_total = immigrants /  total_statut_dimmigrant_et_periode_dimmigration_pour_la_population_dans_les_menages_prives_donnees_echantillon_25_percent)
      
      
      plot_ly(base, labels = ~taux_total,
              x = ~annee,
              y = ~taux_total,
              name = "Immigrants",
              type = "bar",
              marker = list(color = c("#0065FF"))
      ) |> 
        layout(font = f, xaxis = list(title = ""), yaxis = list(title = "", tickformat = ',.0%', automargin = TRUE)) |>
        config(locale = "fr-ch")
      
    })
    
    
    
    
    #----------------------
    #tableau
    #---------------------
    
    #portrait commercial
    
    #mix commercial
    #output$tableau_mix_commercial <- render_gt({
    output$tableau_mix_commercial <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      base<- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        filter(!usage_1 == "VACANT") |>
        group_by(usage_1) |> 
        count() |> 
        arrange(desc(n)) |> 
        ungroup() |> 
        collect() |> 
        mutate(pourcentage = round_half_up(n/sum(n) * 100, 1)) %>% 
        mutate(n = format(n, big.mark = " "), pourcentage = paste(pourcentage, "%")) %>% 
        rename(`Type d'entreprise` = usage_1, Nombre = n, Pourcentage = pourcentage) 
      
      DT::datatable(base,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap5",
                    extensions = c("Buttons", "FixedColumns"), # Décommentez cette ligne si vous voulez utiliser les boutons
                    options = list(
                      paging = FALSE,
                       columnDefs = list(
                         list(className = 'dt-center', targets = c(1, 2))  # Center all columns
                       ),
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list( # Décommentez cette section si vous voulez ajouter des boutons
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'
                        )
                      )
                    ),
                    escape = FALSE)
      
      
      #gt() |> 
      #opt_interactive(use_compact_mode = FALSE, use_pagination = FALSE, use_pagination_info = FALSE) |> 
      #fmt_percent(columns = 3, sep_mark = " ", decimals = 1) |>
      #cols_label(
      #  usage_1 = "Type d'entreprise",
      #  n = "Nombre",
      #  pourcentage = "%"
      #) |> 
      #opt_table_font(
      #  font = "onest"
      #) |> 
      #cols_align(
      #  align = "center",
      #  column = 2:3
      #)
      
      #mix_commercial
      
    })
    
    #vacance
    #output$tableau_vacance <- render_gt({
    output$tableau_vacance <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      #pour attraper les fonctions vides
      reference <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        group_by(fonction) %>% 
        summarise() %>% 
        filter(!fonction %in% c("Institution", "Résidentiel")) %>%
        collect()
      
      
      base <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        group_by(usage_1, fonction) %>% 
        count() %>% 
        ungroup() %>%
        collect() %>% 
        group_by(fonction) %>%
        mutate(n_loc = sum(n)) %>% 
        ungroup() %>% 
        filter(usage_1 == "VACANT") %>% 
        group_by(fonction) %>% 
        summarise(n, pourcentage = round_half_up(n/n_loc, 3)) %>% 
        right_join(reference, by = "fonction") %>%
        replace_na(list(n = 0, pourcentage = 0)) %>% 
        mutate(pourcentage = round_half_up(n/sum(n) * 100, 1)) %>% 
        mutate(n = format(n, big.mark = " "), pourcentage = paste(pourcentage, "%")) %>% 
        rename(Fonction = fonction, Nombre = n, Pourcentage = pourcentage) 
      
      DT::datatable(base,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap5",
                    extensions = c("Buttons", "FixedColumns"), # Décommentez cette ligne si vous voulez utiliser les boutons
                    options = list(
                      paging = FALSE,
                       columnDefs = list(
                         list(className = 'dt-center', targets = c(1, 2))  # Center all columns
                       ),
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list( # Décommentez cette section si vous voulez ajouter des boutons
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'
                        )
                      )
                    ),
                    escape = FALSE)
    })
    
    #output$tableau_changement_occ <- render_gt({
    output$tableau_changement_occ  <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      #pour attraper les fonctions vides et ou conserver les fonctions qui nous intéressent
      reference <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        group_by(fonction) %>% 
        summarise() %>% 
        filter(!fonction %in% c("Institution", "Résidentiel")) %>%
        collect()
      
      
      base <- arrow::open_dataset("data/occupant_final.parquet", format = "parquet") |> 
        group_by(statut_1, fonction) %>% 
        count() %>% 
        ungroup() %>%
        group_by(fonction) %>%
        collect() %>% 
        mutate(n_loc = sum(n)) %>%
        mutate(n_loc_changement = n_loc - n) %>% 
        ungroup() %>% 
        filter(statut_1 == "À jour") %>%
        mutate(n, pourcentage = round_half_up(1 - (n/n_loc), 3)) %>% 
        right_join(reference, by = "fonction") %>%
        replace_na(list(n = 0, pourcentage = 0)) %>% 
        select(fonction, n_loc_changement, pourcentage) %>% 
        mutate(pourcentage = round_half_up(n_loc_changement/sum(n_loc_changement) * 100, 1)) %>% 
        mutate(n = format(n_loc_changement, big.mark = " "), pourcentage = paste(pourcentage, "%")) %>% 
        rename(Fonction = fonction, Nombre = n_loc_changement, Pourcentage = pourcentage) %>% 
        select(-n)
      
      DT::datatable(base,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap5",
                    extensions = c("Buttons", "FixedColumns"), # Décommentez cette ligne si vous voulez utiliser les boutons
                    options = list(
                      paging = FALSE,
                       columnDefs = list(
                         list(className = 'dt-center', targets = c(1, 2))  # Center all columns
                       ),
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list( # Décommentez cette section si vous voulez ajouter des boutons
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'
                        )
                      )
                    ),
                    escape = FALSE)
      
    })
    
    
    #population
    #output$tableau_population <- render_gt({
    output$tableau_population <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      #req(input$socio_sujet == "Population")
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques == 1) |> 
        select(annee, chiffre_total) |>
        arrange(annee) |>  
        collect() %>% 
        mutate(chiffre_total = format(chiffre_total, big.mark = " ")) %>% 
        pivot_wider(names_from = annee, values_from = chiffre_total) 

      DT::datatable(base,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap5",
                    extensions = c("Buttons", "FixedColumns"), # Décommentez cette ligne si vous voulez utiliser les boutons
                    options = list(
                      paging = FALSE,
                       columnDefs = list(
                         list(className = 'dt-center', targets = "_all")  # Center all columns
                       ),
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list( # Décommentez cette section si vous voulez ajouter des boutons
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'
                        )
                      )
                    ),
                    escape = FALSE)
      
    })
    
    #densite
    #output$tableau_densite <- render_gt({
    output$tableau_densite <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      #req(input$socio_sujet == "Population")
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques == 6) |> 
        select(annee, chiffre_total) |>
        arrange(annee) |>  
        collect() |> 
        mutate(chiffre_total = format(round_half_up(chiffre_total, 0), big.mark = " ")) %>% 
        pivot_wider(names_from = annee, values_from = chiffre_total) 
      
      DT::datatable(base,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap5",
                    extensions = c("Buttons", "FixedColumns"), # Décommentez cette ligne si vous voulez utiliser les boutons
                    options = list(
                      paging = FALSE,
                      columnDefs = list(
                        list(className = 'dt-center', targets = "_all")  # Center all columns
                      ),
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list( # Décommentez cette section si vous voulez ajouter des boutons
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'
                        )
                      )
                    ),
                    escape = FALSE)
      
    })
    
    #age
    #output$tableau_age <- render_gt({
    output$tableau_age <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      #req(input$socio_sujet == "Population")
      
      #filter les données de nom_caracteristique pour les groupes d'age
      age_df <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        select(annee, id_caracteristiques, nom_caracteristique, chiffre_total) |> 
        filter(id_caracteristiques %in% c(3000:3005)) |> 
        select(-id_caracteristiques) |>
        arrange(annee) |> 
        collect()
      
      
      #mutate nom_caracteristique en factor avec comme levels les groupes dâge en ordre croissant
      base <- age_df %>% 
        mutate(nom_caracteristique = factor(nom_caracteristique, levels = c("0 à 14 ans", "15 à 29 ans", "30 à 44 ans", "45 à 59 ans", "60 à 74 ans", "75 ans et plus"))) |> 
        mutate(chiffre_total = format(round_half_up(chiffre_total, 0), big.mark = " ")) %>% 
        pivot_wider(names_from = annee, values_from = chiffre_total) %>% 
        rename(`Groupes d'âge` = nom_caracteristique)
      
      DT::datatable(base,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap5",
                    extensions = c("Buttons", "FixedColumns"), # Décommentez cette ligne si vous voulez utiliser les boutons
                    options = list(
                      paging = FALSE,
                      # columnDefs = list(
                      #   list(className = 'dt-center', targets = c(1, 2))  # Center all columns
                      # ),
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list( # Décommentez cette section si vous voulez ajouter des boutons
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'
                        )
                      )
                    ),
                    escape = FALSE)
      
      
    })
    
    #revenus
    #output$tableau_revenus <- render_gt({
    output$tableau_revenus <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques  %in% c(243, 742)  & str_detect(theme, "Revenu")) |> 
        select(annee, chiffre_total) |>
        arrange(annee) |>  
        collect() %>% 
        mutate(chiffre_total = paste(format(chiffre_total, big.mark = " "), "$")) %>% 
        pivot_wider(names_from = annee, values_from = chiffre_total)  
      
      DT::datatable(base,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap5",
                    extensions = c("Buttons", "FixedColumns"), # Décommentez cette ligne si vous voulez utiliser les boutons
                    options = list(
                      paging = FALSE,
                      columnDefs = list(
                        list(className = 'dt-center', targets = "_all")  # Center all columns
                      ),
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list( # Décommentez cette section si vous voulez ajouter des boutons
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'
                        )
                      )
                    ),
                    escape = FALSE)
    
    })
    #revenus
    #output$tableau_depenses_menages_2021 <- render_gt({
    output$tableau_depenses_menages_2021 <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      
      
      base <- arrow::open_dataset("data/depenses_menages_agg_2021.parquet", format = "parquet") |> 
        select(indicateur_depense, montant, pourcentage) |>
        arrange(desc(pourcentage)) |> 
        collect() %>% 
        mutate(pourcentage = round_half_up(pourcentage, 1)) |> 
        mutate(montant = paste(format(montant, big.mark = " "), "$"), pourcentage = paste(pourcentage, "%")) %>% 
        rename(`Indicateur de dépense` = indicateur_depense, Montant = montant, Pourcentage = pourcentage)
      
      DT::datatable(base,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap5",
                    extensions = c("Buttons", "FixedColumns"), # Décommentez cette ligne si vous voulez utiliser les boutons
                    options = list(
                      paging = FALSE,
                       columnDefs = list(
                         list(className = 'dt-center', targets = c(1, 2))  # Center all columns
                       ),
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list( # Décommentez cette section si vous voulez ajouter des boutons
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'
                        )
                      )
                    ),
                    escape = FALSE)
      
    })
    
    #% postsecondaires
    #output$tableau_diplomes_postsecondaire <- render_gt({
    output$tableau_diplomes_postsecondaire  <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques %in% c(2017, 1701) & str_detect(theme, "diplôme")) |> 
        mutate(taux_total = ifelse(annee == 2016 & id_caracteristiques == 1701, 74.1, taux_total)) |> #ajout manuel du taux total 2016 avec id 1698
        select(annee,  taux_total) |>
        collect() %>% 
        mutate(taux_total = paste(round_half_up(taux_total, 1), "%")) |> 
        #mutate(montant = format(montant, big.mark = " "), pourcentage = paste(pourcentage, "%")) %>% 
        rename(`Année` = annee, `Taux de diplomation postsecondaire` = taux_total) 
      
      DT::datatable(base,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap5",
                    extensions = c("Buttons", "FixedColumns"), # Décommentez cette ligne si vous voulez utiliser les boutons
                    options = list(
                      paging = FALSE,
                      columnDefs = list(
                        list(className = 'dt-center', targets = c(1))  # Center all columns
                      ),
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list( # Décommentez cette section si vous voulez ajouter des boutons
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'
                        )
                      )
                    ),
                    escape = FALSE)
      
      
    })
    
    #niveau scolarité 25 ans et plus
    #output$tableau_scolarite_25_ans <- render_gt({
    output$tableau_scolarite_25_ans  <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques %in% c(2015, 2016, 2018, 2024, 1699, 1700,  1702, 1707) & str_detect(theme, "diplôme")) |> 
        mutate(taux_total = ifelse(annee == 2016 & id_caracteristiques == 1699, 7.7, taux_total)) |> #ajout manuel du taux total 2016 avec id 
        mutate(taux_total = ifelse(annee == 2016 & id_caracteristiques == 1700, 18.2, taux_total)) |> #ajout manuel du taux total 2016 avec id 
        mutate(taux_total = ifelse(annee == 2016 & id_caracteristiques == 1702, 46.1, taux_total)) |> #ajout manuel du taux total 2016 avec id 
        mutate(taux_total = ifelse(annee == 2016 & id_caracteristiques == 1707, 28, taux_total)) |> #ajout manuel du taux total 2016 avec id 
        mutate(nom_caracteristique = ifelse(id_caracteristiques == 1707, "Universitaire", nom_caracteristique)) |> #correction manuelle du nom universitaire 2016
        mutate(nom_caracteristique = ifelse(id_caracteristiques %in% c(2015, 1699), "Aucun diplôme", nom_caracteristique)) |> #correction manuelle du nom universitaire 2016
        mutate(taux_total = taux_total) |>
        select(annee, nom_caracteristique, taux_total) |>
        collect() %>% 
        mutate(taux_total = paste(round_half_up(taux_total, 1), "%")) %>% 
        pivot_wider(names_from = nom_caracteristique, values_from = taux_total) %>% 
        rename(`Année` = annee)
      
      DT::datatable(base,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap5",
                    extensions = c("Buttons", "FixedColumns"), # Décommentez cette ligne si vous voulez utiliser les boutons
                    options = list(
                      paging = FALSE,
                      columnDefs = list(
                        list(className = 'dt-center', targets = c(1:4))  # Center all columns
                      ),
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list( # Décommentez cette section si vous voulez ajouter des boutons
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'
                        )
                      )
                    ),
                    escape = FALSE)
    })
    
    # immigration
    
    #output$tableau_immigration <- render_gt({
    output$tableau_immigration  <- DT::renderDataTable(server = FALSE,{ #pour charger toutes les données
      
      base <- arrow::open_dataset("data/recensement_chambly_2021_2016_2011.parquet", format = "parquet") |> 
        filter(id_caracteristiques %in% c(1140, 1142, 1527, 1529) & str_detect(theme, "immigrant")) |>  
        select(annee, nom_caracteristique, chiffre_total) |>
        collect() |> 
        pivot_wider(names_from = nom_caracteristique, values_from = chiffre_total) |> 
        clean_names() |> 
        mutate(taux_total = immigrants /  total_statut_dimmigrant_et_periode_dimmigration_pour_la_population_dans_les_menages_prives_donnees_echantillon_25_percent * 100) |> 
        select(annee, taux_total) %>% 
        mutate(taux_total = paste(round_half_up(taux_total, 1), "%")) %>% 
        pivot_wider(names_from = annee, values_from = taux_total) %>% 
        select(`2016`, `2021`) 
      
      DT::datatable(base,
                    rownames = FALSE,
                    selection = "single",
                    class = 'cell-border stripe',
                    style = "bootstrap5",
                    extensions = c("Buttons", "FixedColumns"), # Décommentez cette ligne si vous voulez utiliser les boutons
                    options = list(
                      paging = FALSE,
                      columnDefs = list(
                        list(className = 'dt-center', targets = "_all")  # Center all columns
                      ),
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"),
                      dom = 'Brtip',
                      buttons = list( # Décommentez cette section si vous voulez ajouter des boutons
                        list(
                          extend = 'excel',
                          text = 'Exporter en Excel'
                        )
                      )
                    ),
                    escape = FALSE)
      
    })
    
    
    
  })
}