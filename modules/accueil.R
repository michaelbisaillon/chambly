accueil_UI <- function(id) {
  
  ns <- NS(id)
  
  #ui <- fluidPage(
  ui <- page_fillable(
    padding = "0px",
    gap = "0px",
    fillable_mobile = TRUE,
    layout_columns(gap = "0px",
      col_widths = c(12, 12, 8, 4, 12),
      card(class = "accueil-normal-card",
        min_height = "500px",
        max_height = "825px",
        card_body(class = "p-0",
                  tags$img(src = "tim-trad-r2NBj6GAJzw-unsplash2.jpg", style = "width: 100%; height: 100%; object-fit: cover;")
        )
      ),
      card(class = "accueil-card",
           card_body(padding = "100px",
                     column(6,
                            tags$h1("VilleInnov est une firme d’urbanisme, fondée par trois passionnés par les
                  sciences de la donnée, l'aménagement du territoire et le design urbain"),
                            br(),
                            tags$h4("Sa mission est de fournir des solutions concrètes, simples et
                  économiques pour une connaissance approfondie du territoire, pour les
                  municipalités, les organismes de développement économique et les
                  promoteurs immobiliers. Grâce à notre expertise, nous sommes en mesure
                  de collecter, d’analyser la données et anticiper les possibilités de
                  développement commercial qui peuvent orienter des stratégies
                  d'urbanisme.")
                     )
           )
      ),
      card(class = "accueil-normal-card",
        card_body(padding = "100px",
        tags$h1("Le cas de Chambly, une démonstration de notre savoir-faire"),
        br(),
        tags$h5("Nous avons sélectionné la ville de Chambly pour vous démontrer
                     l'efficacité de nos outils d'analyse de données. Nous avons analysé
                     les données de la ville pour vous offrir une vision claire et
                     précise de son territoire. Nous avons mis en place des outils
                     d'analyse de données pour vous permettre de visualiser les
                     opportunités de développement commercial et industriel."),
        br(),
        tags$h5("L'outil comprends les modules suivants:"),
        tags$li("Un module de recherche pour effectuer des extractions de données"),
        tags$li("Un module pour estimer un marché à l'aide d'une courbe isochrone"),
        tags$li("Un tableau de bord pour suivre les indicateurs économiques de la ville (À venir)")
        )
      ),
      
      card(class = "accueil-normal-card",
        card_body(class = "p-0", fillable = TRUE,
        tags$img(src = "maarten-van-den-heuvel--WWFGrfm7qk-unsplash.jpg", style = "width: 100%; height: 100%; object-fit: cover;")
        )
      ),
      card(class = "footer-card",
           card_body(padding = "100px",
           tags$h5("Tous droits réservés © 2024 VilleInnov")
           )
           
      )
    )
  )


}


accueil_server <- function(id, data) {
  moduleServer(id, function(input, output, session){
    
  })
}

