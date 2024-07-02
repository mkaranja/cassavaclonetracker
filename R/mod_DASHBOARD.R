#' DASHBOARD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DASHBOARD_ui <- function(id){
  ns <- NS(id)
  tagList(


    fluidRow(
      selectInput(ns("station"), "Select Station", choices = c("")),

      bslib::layout_column_wrap(
        width = "250px",
        !!!list(
          bslib::value_box(
            title = "Number of Varieties Received",
            value = textOutput(ns("nvarieties")),
            p(textOutput(ns("nboxes"))),
            p(textOutput(ns("ncuttings")))
          ),

          bslib::value_box( title = "", value = textOutput(ns("ready_2_exit")), p("Number ready to Exit Rapid Propagation")),
          bslib::value_box( title = "", value = "", p("")),
          bslib::value_box( title = "", value = "",  p("Sales of cassava cuttings")),
          bslib::value_box( title = "", value = "",  p("Sales of cassava cuttings"))

        )
      ),


      bslib::layout_column_wrap(
        width = 1/3,
        height = 400,
        bslib::card(full_screen = TRUE,
                    bslib::card_header("Plant Material Current Status")
                    # drill-down plot
                    # detailed table in a pop-up window
                    ),
        bslib::card(full_screen = TRUE,
                    bslib::card_header("Plantlets ready for recut"),
                    tableOutput(ns("recuts_table"))
                    ),
        bslib::card(full_screen = TRUE,
                    bslib::card_header("Plantlets ready to Exit Rapid Propagation"),
                    tableOutput(ns("exit_table")))
      )
    )
  )
}

#' DASHBOARD Server Functions
#'
#' @noRd
mod_DASHBOARD_server <- function(id, r, admin, location){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      if(isTRUE(r$nav == "Dashboard")){
        stations <- load_data("SELECT location FROM tblLocation")

        if(admin){
          updateSelectInput(session, "station", "Select Station", choices = c("All", stations$location))
        } else {
          updateSelectInput(session, "station", "Select Station", choices = c(location))
        }
      }
    })

    received <- reactive({
      if(input$station == "All"){
        dt <- load_data(glue::glue("SELECT * FROM tblIntroduction"))
      } else {
        dt <- load_data(glue::glue("SELECT * FROM tblIntroduction WHERE location='{location}'"))
      }
      dt
    })

    output$nvarieties <- renderText({
      unique(received()$variety)
    })

    output$nboxes <- renderText({
      nrow(received())
    })

    output$nvariety <- renderText({
      SUM(received()$number_of_thick_mini_cuttings)
    })


  })
}

## To be copied in the UI
# mod_DASHBOARD_ui("DASHBOARD_1")

## To be copied in the server
# mod_DASHBOARD_server("DASHBOARD_1")
