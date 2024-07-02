#' FS_sale UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_FS_sale_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' FS_sale Server Functions
#'
#' @noRd 
mod_FS_sale_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_FS_sale_ui("FS_sale_1")
    
## To be copied in the server
# mod_FS_sale_server("FS_sale_1")
