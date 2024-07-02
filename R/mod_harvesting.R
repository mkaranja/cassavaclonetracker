#' harvesting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_harvesting_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' harvesting Server Functions
#'
#' @noRd 
mod_harvesting_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_harvesting_ui("harvesting_1")
    
## To be copied in the server
# mod_harvesting_server("harvesting_1")
