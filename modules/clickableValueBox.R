library(shiny)
library(bslib)


# Supposons que c'est votre fonction UI générique ou directement dans l'UI de Shiny
clickableBoxUI <- function(id) {
  ns <- NS(id)
  tags$div(
    id = ns("clickable_box"),
    class = "well", # ou une autre classe Bootstrap pour le style
    style = "cursor: pointer;", # rend le div cliquable
    "Cliquez-moi", # contenu de la boîte
    tags$br(),
    "Plus d'informations ici"
  )
}

clickableBox <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input[[ns("clickable_box")]], {
    showModal(modalDialog(
      title = "Informations",
      "Vous avez cliqué sur la boîte.",
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
}

