#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    fluidPage(
      bslib::page_navbar(
        id = "nav",
        title = "Cassava Clone Tracker", # img(src="www/cct.png", height = "40px"),
        fillable = TRUE,
        fillable_mobile = TRUE,
        theme = bslib::bs_theme(version = 5, bootswatch = "yeti"), # , success = "#73EDA0", info = "#5FD3F5", danger = "#FB7F72"  bg = "#EEF7EB", fg = "#080808", primary = "#1F6326",
        window_title = "Cassava Clone Tracker",
        position = "static-top",
        fluid = T,
        inverse = F,

        bslib::nav_spacer(),

        # tabPanel(
        #   title = "Dashboard",
        #   mod_DASHBOARD_ui("DASHBOARD_1")
        # ),

        tabPanel(
          title = "Rapid Propoagation",
          value = "rapid",

          bslib::navset_tab(
            id = "rapid_tabs",
            tabPanel(
              title = "Receive Planting Material", br(),
              mod_RP_introduction_ui("Introduction")
            ),
            tabPanel(
              title = "Re-Cutting", br(),
              mod_RP_recut_ui("Recut")
            ),
            tabPanel(
              title = "Exit Rapid Propapagtion", br(),
              mod_RP_exit_ui("Exit")
            )
          )
        ),

        tabPanel(
          title = "Pencil Stem Block",
          value = "pencil",

          bslib::navset_tab(
            id = "pencil_tabs",
            tabPanel(
              "Planting", br(),
              mod_PS_plant_ui("PS_plant_1")
            ),
            tabPanel(
              "Survival after 2 weeks", br(),
              mod_PS_survival_ui("PS_survival_1")
            ),
            tabPanel(
              "Harvest", br(),
              mod_PS_harvest_ui("PS_harvest_1")
            )
          )
        ),

        tabPanel(
          title = "Field Seed Block",
          value = "field",

          bslib::navset_tab(
            id = "field_tabs",
            tabPanel(
              "Planting", br(),
              mod_FS_plant_ui("FS_plant_1")
            ),
            tabPanel(
              "Harvest", br(),
              mod_FS_harvest_ui("FS_harvest_1")
            )
            # tabPanel(
            #   "Sale of cuttings", br()
            # ),
            # tabPanel(
            #   "Field re-planting"
            # )
          )
        ),

        # div(id = "manage_tab",
        bslib::nav_menu(
          title = "Manage",
          value = "manage",

          tabPanel(
            title = "Locations",
            value = "Location",
            mod_MANAGE_ui("location")
          ),
          tabPanel(
            title = "Varieties",
            value = "Variety",
            mod_MANAGE_ui("variety")
          ),
          tabPanel(
            title = "Source of Planting Material",
            value = "Source",
            mod_MANAGE_ui("source")
          )
        ),

        bslib::nav_menu(
          title = "Generate Barcode",
          value = "labels",

          tabPanel(
            title = "New Barcode Labels",
            mod_LABELS_ui("labels")
          )
        ),

        # tabPanel(
        #   title = "Search",
        #   icon = icon("search"),
        #   value = "search",
        #   mod_SEARCH_ui("SEARCH_1")
        # ),
        bslib::nav_spacer()
      ),

      HTML(paste("<script>var parent = document.getElementsByClassName('navbar-nav');
            parent[0].insertAdjacentHTML( 'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"disabled\"><a href=\"#\"><strong>",
                 textOutput("user"),"</strong></a></li></ul>' );</script>")),

      tags$footer(HTML("
                    <!-- Footer -->
                    <footer class='page-footer font-large indigo' style='position: fixed; bottom: 0; width: 100%; background-color: #f8f9fa;'>
                        <div class='container-fluid'> <!-- Use container-fluid for full width -->
                            <!-- Row for Footer Content -->
                            <div class='row d-flex'>
                                <!-- Copyright -->
                                <div class='footer-copyright text-left py-3' style='font-size:10pt'>
                                <span id='copyright-year'>Copyright © 2024 </span> <br>
                                For any question, please contact <a href='mailto:m.karanja@cgiar.org?Subject=Cassava%20Clone%20Tracker%20-%20Problem' target='_top'>administrator</a>.
                                </div>
                                <!-- Copyright -->
                            </div> <!-- End of Bootstrap row -->
                        </div>

                    </footer>
                    <!-- Footer -->"
      )
      )

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "cct"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    shinyFeedback::useShinyFeedback()
  )
}
