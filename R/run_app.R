#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(
        enable_admin = TRUE,
        fab_position = "bottom-right",
        theme = bslib::bs_theme(version = 5, bootswatch = "yeti"),

        tags_bottom = tags$div(
          tags$p("For any question, please  contact ",
            tags$a(
              href = "mailto:m.karanja@cgiar.org?Subject=Cassava%20Clone%20Tracker%20-%20Problem", # href = "mailto:m.karanja@cgiar.org?Subject=Cassava%20Clone%20Tracker%20-%20Problem&cc=m.ferguson@cgiar.org,tm.shah@cgiar.org",
              target="_top", "administrator"
            )
          )
        ),
        app_ui),
      server = app_server,
      onStart = onStart,
      options = options(shiny.autoreload = TRUE),
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
