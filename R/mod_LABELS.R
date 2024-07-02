#' LABELS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_LABELS_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarLayout(
      sidebarPanel(
        width = 3,
        column(4, offset = 8, align="right", shinyWidgets::actionBttn(ns("help"), "", size = "xs", style = "material-circle", color = "default", icon = icon("question"))),

        bslib::tooltip(selectInput(ns("variety"), "Variety name", choices = c(""), width = "100%"), "Variety name is required", placement = "auto"),
        selectInput(ns("number"), "Number of labels needed (Multiples of 32 to avoid label wastage)", choices = c(32, 64, 96, 128, 160, 192, 224, 256, 288, 320)),
        bslib::tooltip(
          shinyjs::disabled(shinyFeedback::loadingButton(ns("generate"), "Generate Barcode Labels", loadingLabel = "Generate Barcode Labels")),
          "Activated after selecting variety and adding number of labels needed.",
          placement = "auto"
        ),
        br(),br(),
        p("To be printed on A4 page size."),

      ),

      mainPanel(
        width = 8,

        shinyWidgets::prettySwitch(ns("unassigned_ids"), "View unassigned box identities (select variety on left panel)", status = "primary", fill = T),
        div(id = ns("unassigned_labels"),
          bslib::card(
            bslib::card_header(textOutput(ns("unassigned_title"))),
            DT::DTOutput(ns("unassigned_table")), br()
          ),
          column(1, shinyjs::hidden(downloadButton(ns("unassigned_pdf"), "PDF", class = "pull-right btn btn-danger"))), br()

        ) |> shinyjs::hidden(),

        div(id = ns("show_labels"),
          bslib::card(
            bslib::card_header("NEW LABELS"),
            DT::DTOutput(ns("table"))
          )
          ) |> shinyjs::hidden(), br(),


        fluidRow(
          column(1, shinyjs::hidden(downloadButton(ns("download_pdf"), "PDF", class = "pull-right btn btn-danger")))#,
          #column(1, offset = 10, actionButton(ns("clear"), "Clear", class = "pull-right btn btn-primary"))
        ),
        fluidRow(textOutput(ns("downloading_labels")))
      )
    )

  )
}

#' LABELS Server Functions
#'
#' @noRd
mod_LABELS_server <- function(id,r, location){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # uses 'helpfiles' directory by default
    observeEvent( input$help , {
      if(isTRUE(r$nav == "New Barcode Labels")){
        showModal(modalDialog(
          shiny::includeMarkdown("helpfiles/labels.md"),
          easyClose = TRUE,
          size = "l"
        ))
      }
    })


    labels_data <- reactiveVal(
      data.frame(
        box_id = character(0),
        variety = character(0),
        location = character(0)
      )
    )

    observe({
      if(isTRUE(r$nav == "New Barcode Labels")){
          if(isTRUE(!is.null(location))){
            variety <- load_data("SELECT variety FROM tblVariety;")$variety
            updateSelectInput(session, "variety", "Variety name", choices = c("", toupper(variety)))
          }
      }
    })


    # -----------------------------------------------------------------------

    observe({
      if(isTRUE(r$nav == "New Barcode Labels")){
        if(isTRUE(!is.null(location))){
          if(isFALSE(input$variety == "")){
            if(isFALSE(input$number == 0)){
              if(isFALSE(is.na(input$number))){
                shinyjs::enable("generate")
              }
            }
          }
        }
      }
    })

    # https://github.com/awkena/qrlabelr

    # text indicator that pdf finished making

    pdf_file_name <- reactiveVal()

    observeEvent( input$generate , {

      if(isTRUE(r$nav == "New Barcode Labels")){
        if(isTRUE(!is.null(location))){
          if(isFALSE(input$variety == "")){
            # if(isFALSE(input$number == 0)){
            #   if(isFALSE(is.na(input$number))){

                # generate pdf labels
                # generate ids
                dt1 <- load_data(paste0("SELECT * FROM tblBoxIdentity WHERE location='", location,"' AND  variety='", input$variety, "';")) |>
                  dplyr::filter( substr(Sys.Date(), 3, 4) == substr(box_id, 4,5))

                if(nrow(dt1)>0){
                  last_box_id <-  as.integer(max(stringr::str_sub(dt1$box_id, -6,-1)))
                } else {
                  last_box_id <- 0
                }

                df1 <- data.frame(
                  variety = input$variety,
                  location = location
                )

                df2 <- as.data.frame(df1)[rep(row.names(df1), as.integer(input$number)),] |>
                  tibble::rowid_to_column()

                df2$box_id <- gsub(" ", "", toupper(paste0(substr(location,1,3), substr(Sys.Date(), 3, 4), input$variety, stringr::str_pad((df2$rowid + last_box_id), 6, pad = 0))))
                df2$rowid <- NULL
                df2 <- df2 |>
                  dplyr::select(box_id, variety, location)

                labels_data(df2) # save data

                # --------------------------------------------------------------

                output$table <- DT::renderDT({
                  dt <- df2
                  colnames(dt) <-  stringr::str_to_title(gsub("_", " ", names(dt)))
                  DT::datatable(
                    data = dt,
                    rownames = FALSE,
                    escape = FALSE,
                    selection = "none",
                    options = list(searching = F,
                                   lengthChange = FALSE,
                                   pageLength = 20,
                                   autoWidth = FALSE,
                                   initComplete = DT::JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#828783', 'color': '#fff'});",
                                     "}"))
                  )
                })


                # --------------------------------------------

                out <- tempfile(fileext = ".pdf", pattern = paste0(location,"_"))
                pdf_file_name(out)

                baRcodeR::custom_create_PDF(
                  user=FALSE,
                  Labels = df2$box_id,
                  name = pdf_file_name(),
                  type = "matrix",
                  ErrCorr = "L",
                  Fsz = 9,
                  Across = TRUE,
                  ERows = 0,
                  ECols = 0,
                  trunc = TRUE,
                  numrow = 8,
                  numcol = 4,
                  page_width = 8.5,
                  page_height = 11,
                  width_margin = 0.0, # The width margin of the page (in inches). Default is 0.25.
                  height_margin = 0.0, # The height margin of the page (in inches). Default is 0.5.
                  label_width = 2.5,
                  label_height = 1.25,
                  x_space = .01,
                  y_space = .5,
                  alt_text = paste0("VTY: ", df2$variety, "\n", "LOC: ", df2$location),
                  denote = c("\n", "")
                )
                # save in database
                DBI::dbWriteTable(conn, "tblBoxIdentity", df2, append = T)

                shinyFeedback::resetLoadingButton("generate")

                shinyjs::show("show_labels")
                shinyjs::show("download_pdf")
                shinyjs::show("download_excel")
                shinyWidgets::show_alert("", "Labels have been successfully generated! Click the 'PDF' button to download a PDF file containing the generated labels to your computer.", type = "success")

            #   }
            # }
          }
        }
      }
    })


    # -------------------------------------------------------------------------

    # make file available for download

    output$download_pdf <- shiny::downloadHandler(
      filename = function() { paste0(location,"-",input$variety, "-labels.pdf")},
      content = function(file){
        file.copy(paste0(pdf_file_name(), ".pdf"), file)
      },
      contentType = "application/pdf"
    )


    # --------------------- unassigned box identities

    output$unassigned_title <- renderText({

        ifelse(input$variety == "", "Select Variety from the left panel to show labels of only that variety. ", paste(input$variety, " unassigned labels."))
    })

    unassigned_ids <- reactive({
      input$variety
      input$unassigned_ids
      if(isTRUE(input$variety == "")){
        load_data(glue::glue("SELECT t1.box_id, t1.variety
        FROM tblBoxIdentity t1
        LEFT JOIN tblAssignedBoxIdentity t2 ON t1.box_id = t2.box_id
        WHERE t2.box_id IS NULL AND t1.location='{location}';
        ")
        )
      } else {
        load_data(glue::glue("SELECT t1.box_id, t1.variety
        FROM tblBoxIdentity t1
        LEFT JOIN tblAssignedBoxIdentity t2 ON t1.box_id = t2.box_id
        WHERE t2.box_id IS NULL AND t1.variety='{input$variety}' AND t1.location='{location}';
        ")
        )
      }

    })

    observeEvent( input$unassigned_ids , {
      if(input$unassigned_ids){
        shinyjs::show("unassigned_labels")
      } else {
        shinyjs::hide("unassigned_labels")
      }

      # if there is unassigned data, show download pdf button
      if(nrow(unassigned_ids())>0){
        shinyjs::show("unassigned_pdf")
      } else {
        shinyjs::hide("unassigned_pdf")
      }
    })



    output$unassigned_table <- DT::renderDT({

      DT::datatable(
        data = unassigned_ids(),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(searching = F,
                       lengthChange = FALSE,
                       pageLength = 20,
                       autoWidth = FALSE,
                       initComplete = DT::JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#828783', 'color': '#fff'});",
                         "}"))
      )
    })

  })
}

## To be copied in the UI
# mod_LABELS_ui("LABELS_1")

## To be copied in the server
# mod_LABELS_server("LABELS_1")
