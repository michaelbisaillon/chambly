#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

custom_theme <- bs_theme(
  version = 5,
  bg = "white",fg = "#111c26", # Couleurs pour le mode clair
  primary = "#111c26",   secondary = "#304154",
  #primary = "#111c26",   secondary = "#FF5302",
  base_font = "Onest",
  code_font = "Onest",
  heading_font = "Onest",
)


#bs_theme_preview(theme = custom_theme, with_themer = FALSE)

#bs_theme_preview(theme)


# Define UI for application that draws a histogram
page_fluid(
  # useshinyjs(),
  tags$link(rel = "stylesheet", type = "text/css", href = "style2.css"), #feuille style
  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Onest:wght@100..900&display=swap"), #font
  theme = custom_theme,
  page_navbar(
    fillable_mobile = TRUE, bg = "white",
    #title = tags$a(href = "https://luxuriant.shinyapps.io/chambly/", 
                   #tags$img(src = "VilleInnov_logo_FINAL_BLACK.png", height = 60)), 
    tags$head(tags$title("VilleInnov - Chambly")),
    nav_spacer(),
    #nav_panel(title = HTML("<b><h4>", "Accueil", "</b></h3>"), 
     #         tabPanel("Accueil",
      #                 accueil_UI("accueil"))),
    #nav_menu(title = HTML("<b><h4>", "Recherches", "</b></h3>"), 
     #        tabPanel("Requêtes",
      #                requete_UI("requete"))),
    nav_menu(title = HTML("<b><h5>", "Analyses", "</b></h3>"), align = "right",
             tabPanel("Requêtes de données",
                      requete_UI("requete")
                      ),
             tabPanel("Tableau de bord",
                      tableau_de_bord_UI("tableau_de_bord")
             ),
             tabPanel("Carte interactive",
                      carte_interactive_UI("carte_interactive")
             ),
             tabPanel("Marché par rayon d'action",
                      isochrone_UI("isochrone")
                      
             )
    ),
    nav_menu(title = HTML("<b><h5>", "Publications et références", "</b></h3>"), align = "right",
             tabPanel("Nos publications",
                      page_fillable(
                        card(
                          tags$h3("Place des Lilas, un exemple de projet de redéveloppement"),
                          "Le document a pour objectif de mettre en lumière le potentiel de redéveloppement de la Place Les Lilas situé au 1101 boulevard Brassard.",
                          #tags$a("Consulter le document", href = "https://docs.google.com/presentation/d/1wPcebm8FwOtEcttm1paM2dAFZ-efJJ1czbqKyTSLgHo/edit?pli=1#slide=id.g1f3d03fb852_0_0"),
                          
                          #inséer une image
                          layout_columns(gap = "0px",
                                         col_widths = c(6, 6, 6, 6),
                                         card(class = "accueil-normal-card",
                                              card_body(padding = "0px",
                                                        tags$img(src = "place_des_lilas.jpg", style = "width: 100%; height: 100%; object-fit: cover;")
                                              )
                                         ),
                                         card(class = "accueil-normal-card",
                                              card_body(padding = "0px",
                                                        tags$img(src = "place_des_lilas3.jpg", style = "width: 100%; height: 100%; object-fit: cover;"),
                                              )
                                         ),
                                         card(class = "accueil-normal-card",
                                              card_body(padding = "0px",
                                                        tags$img(src = "place_des_lilas2.jpg", style = "width: 100%; height: 100%; object-fit: cover;"),
                                              )
                                         ),
                                         card(
                                           card_body(padding = "100px",
                                                     tags$a("Consulter le reste du document", href = "https://docs.google.com/presentation/d/1wPcebm8FwOtEcttm1paM2dAFZ-efJJ1czbqKyTSLgHo/edit?pli=1#slide=id.g1f3d03fb852_0_0"),
                                                     
                                           )
                                         )
                          )
                        )
                      )
             )
    )
    
  )
)
