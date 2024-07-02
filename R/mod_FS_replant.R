#' FS_replant UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_FS_replant_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' FS_replant Server Functions
#'
#' @noRd 
mod_FS_replant_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_FS_replant_ui("FS_replant_1")
    
## To be copied in the server
# mod_FS_replant_server("FS_replant_1")
