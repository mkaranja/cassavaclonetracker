#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # router$server(input, output, session)

  # authentication
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      "users/user_information.sqlite",
      passphrase = "passphrase_wihtout_keyring"
    ),
    inputs_list = list(
      location = list(
        fun = "selectInput",
        args = list( choices = c("KAKAMEGA", "MAGOS", "MTWAPA", "MUGUGA", "NJORO", "OYANI", "TANGAKONA", "WEFI"))
      ),

      station_admin = list(
        fun = "selectInput",
        args = list( choices = c(FALSE, TRUE))
      )
    ),
    timeout = 15,
    keep_token = TRUE
  )



  observeEvent(res_auth$user, {
    # pop up window
    shiny::showModal(
      shinyWidgets::sendSweetAlert(
        session = session,
        title = HTML("<h2>Welcome to Cassava Clone Tracker!</h2>"),
        text = HTML("<h6>Before you start, remember to generate and print your barcode labels via 'Generate Barcode' > 'New Barcode Labels'.</h6>"),
        html = TRUE,
        width = 600,
        closeOnClickOutside = TRUE
      )
    )
  })

  output$user <- renderText({
    paste(stringr::str_to_title(res_auth$user), "|", res_auth$location)
  })

  # show/ hide manage tab
   observeEvent(res_auth$admin, {
     if(res_auth$admin){
        shinyjs::show(selector = "#nav li a[data-value=manage]")
      } else {
        shinyjs::hide(selector = "#nav li a[data-value=manage]")
      }
   })

  # show /hide labels tab
   observeEvent(res_auth$station_admin, {
    if(res_auth$station_admin){
      shinyjs::show(selector = "#nav li a[data-value=labels]")
    } else {
      shinyjs::hide(selector = "#nav li a[data-value=labels]")
    }
   })

  # update selected tabs
  r <- reactiveValues(
    nav = NULL,
    rapid_tabs = NULL,
    pencil_tabs = NULL,
    field_tabs = NULL
  )

  observeEvent(input$nav, { r$nav <- input$nav })
  observeEvent(input$rapid_tabs, { r$rapid_tabs <- input$rapid_tabs })
  observeEvent(input$pencil_tabs, { r$pencil_tabs <- input$pencil_tabs })
  observeEvent(input$field_tabs, { r$field_tabs <- input$field_tabs })

  # mod_DASHBOARD_server("DASHBOARD_1", r, admin = admin, location = toupper(location))
  # mod_SEARCH_server("SEARCH_1", r, admin, location)
  #
  # MGT MODULES
  mod_MANAGE_server("location", r, user = res_auth$user, Tab = "Location")
  mod_MANAGE_server("variety", r, user = res_auth$user, Tab = "Variety")
  mod_MANAGE_server("source", r, user = res_auth$user, Tab = "Source")
  mod_LABELS_server("labels", r, location = toupper(res_auth$location))
  # mod_LABELS_replace_server("LABELS_replace_1", r)
  #
  # # RAPID PROPAGATION MODULES
  mod_RP_introduction_server("Introduction", r, location = toupper(res_auth$location), user = res_auth$user)
  mod_RP_recut_server("Recut", r, location = toupper(res_auth$location), user = res_auth$user)
  mod_RP_exit_server("Exit", r, location = toupper(res_auth$location), user = res_auth$user)

  # PENCIL STEM BLOCK
  mod_PS_plant_server("PS_plant_1", r, location = toupper(res_auth$location))
  mod_PS_survival_server("PS_survival_1", r, location = toupper(res_auth$location))
  mod_PS_harvest_server("PS_harvest_1", r, location = toupper(res_auth$location))

  # FIELD SEED BLOCK
  mod_FS_plant_server("FS_plant_1", r, location = toupper(res_auth$location))
  mod_FS_harvest_server("FS_harvest_1", r, location = toupper(res_auth$location))


}
