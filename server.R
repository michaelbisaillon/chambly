#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Server
server <- function(input, output, session) {
  
  
  
  
  #appel des modules
  accueil_server("accueil")
  isochrone_server("isochrone")
  requete_server("requete")
  tableau_de_bord_server("tableau_de_bord")
  carte_interactive_server("carte_interactive")
  
  
}

