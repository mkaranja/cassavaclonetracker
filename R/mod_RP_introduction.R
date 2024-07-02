#' RP_introduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_RP_introduction_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$script(src = "jsQR.js"),
    sidebarLayout(
      sidebarPanel(
        width = 5,
        column(1, offset = 11, align="right", shinyWidgets::actionBttn(ns("help"), "", size = "xs", style = "material-circle", color = "default", icon = icon("question"))),
        dateInput(ns("receiving_date"), "Receiving date (required)", value = Sys.Date(), max = Sys.Date()),
        selectInput(ns("variety"), "Variety name (required)", choices = c("")),
        selectInput(ns("source"), "Source of planting material (required)", choices = c("")), br(),
        numericInput(ns("number_of_thick_mini_cuttings"), "Number of thick mini-cuttings (required)", min = 1, value = 0),
        # shinyWidgets::prettyRadioButtons(ns("scanner"), "Select barcode scanner to use:", choices = c("Device camera", "External scanner"), inline = T, outline = TRUE),
        actionButton(ns("assign_barcode"), "Assign Barcode (scan)", icon = icon("camera"), width="100%"),
        # textInput(ns("assign_barcode2"), "", placeholder = "Scan barcode using Externer scanner", value = "") |> shinyjs::hidden(),
        textOutput(ns("scan_result")),# |> shinyjs::hidden(),
        fluidRow(
          div(
            tags$video(id = ns("video"), width=300, height=300) |> shinyjs::hidden(),
            tags$canvas(id = ns("canvas"), width=300, height=300) |> shinyjs::hidden()
          ), br(),
          shinyjs::hidden(
            div( id = ns("status"),
                 bslib::card(
                   class="border border-danger bg-danger bg-gradient text-white rounded-end",
                   bslib::card_body(textOutput(ns("show_status")))
                 )
            ))
        )
      ),

      mainPanel(
        width = 5,
        fluidRow( DT::DTOutput(ns("table")) |> shinyjs::hidden()), br(),
        fluidRow(
          column(3, shinyjs::hidden(actionButton(ns("submit"), "Submit", icon = icon("paper-plane"), class = "pull-right btn btn-primary"))),
          column(2, align="right", shinyjs::hidden(actionButton(ns("clear"), "Clear", icon = icon("eraser"))))
        ), br(), br()
      )
    )
  )
}

#' RP_introduction Server Functions
#'
#' @noRd
mod_RP_introduction_server <- function(id, r, location, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    barcode_labels <- reactiveVal(NULL)
    qr_code <- reactiveVal(NULL)
    qr_status <- reactiveVal(NULL)

    # ------------------- help button ---------------------------------------
    observeEvent( input$help , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Receive Planting Material")){
          showModal(modalDialog(
            shiny::includeMarkdown("./helpfiles/receive_cuttings.md"),
            easyClose = TRUE,
            size = "l"
          ))
        }
      }
    })

    # ---------------initiate an empty data frame ------------------------------
    intro_data <- reactiveVal(
      data.table::data.table(
        box_id = character(0),
        variety = character(0),
        original_source = character(0),
        receiving_date = lubridate::ymd(character(0)),
        number_of_thick_mini_cuttings = character(0),
        location = character(0),
        added_by = character(0),
        source_box_id = character(0)
      )
    )


    # ---------------------update variety and source of planting material

    observe({
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Receive Planting Material")){
          #if(isTRUE(!is.null(location))){
          variety <- load_data("SELECT variety FROM tblVariety;")$variety
          source <- load_data("SELECT source FROM tblSource;")$source

          updateSelectInput(session, "variety", "Variety name (required)", choices = c("", toupper(variety)))
          updateSelectInput(session, "source", "Source of planting material (required)", choices = c("", toupper(source)))
          # }
        }
      }
    })





    # Assign barcode
    observeEvent(input$assign_barcode, {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Receive Planting Material")){
          if(isTRUE(input$variety == "")){
            shinyWidgets::show_alert("", text = "Variety is required", type = "warning", width = 600)
          } else if(isTRUE(input$source == "")){
            shinyWidgets::show_alert("", text = "Source is required", type = "warning", width = 600)
          } else if(isTRUE(input$number_of_thick_mini_cuttings == 0)){
            shinyWidgets::show_alert("", text = "Enter the number of mini cutting", type = "warning", width = 600)
          } else if(isTRUE(is.na(input$number_of_thick_mini_cuttings))){
            shinyWidgets::show_alert("", text = "Number of mini cutting should be numeric", type = "warning", width = 600)
          } else {
            input$scanner
            qr_code(NULL)
            shinyjs::hide("status")
            shinyjs::disable("assign_barcode")
            shinyjs::show("canvas")
            shinyjs::runjs("
                 navigator.mediaDevices.getUserMedia({ video: { facingMode: 'environment' } }).then(function(stream) {
                    var video = document.getElementById('Introduction-video');
                    video.srcObject = stream;
                    video.play();
                    var canvas = document.getElementById('Introduction-canvas');
                    var context = canvas.getContext('2d');
                    var captureInterval = setInterval(function() {
                      context.drawImage(video, 0, 0, 300, 300);
                      var imageData = context.getImageData(0, 0, 300, 300);
                      var code = jsQR(imageData.data, imageData.width, imageData.height, { inversionAttempts: 'dontInvert' });
                      if (code) {
                        // QR code detected
                        console.log('QR code detected:', code.data);
                        // Clear the canvas
                        context.clearRect(0, 0, canvas.width, canvas.height);
                        // Draw the video frame again
                        context.drawImage(video, 0, 0, 300, 300);
                        // Draw a green rectangle around the detected QR code
                        context.beginPath();
                        context.rect(code.location.topLeftCorner.x, code.location.topLeftCorner.y, code.location.bottomRightCorner.x - code.location.topLeftCorner.x, code.location.bottomRightCorner.y - code.location.topLeftCorner.y);
                        context.lineWidth = 5;
                        context.strokeStyle = 'green';
                        context.stroke();
                        // Process the QR code result
                        Shiny.setInputValue('Introduction-qrResult', code.data);
                      }
                    }, 500); // Increased interval for better performance
                 }).catch(function(err) {
                    console.error('Error accessing camera: ', err);
                 });
                ")
          }
        }
      }
    })


    observeEvent(input$qrResult, {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Receive Planting Material")){
          if(isTRUE(input$variety != "")){
            if(isTRUE(input$source != "")){
              if(isTRUE(input$number_of_thick_mini_cuttings > 0)){
                if(isTRUE(!is.na(input$number_of_thick_mini_cuttings))){
                  input$scanner
                    if(!is.null(input$qrResult)){
                      qr_code(input$qrResult)
                    }
                }
              }
            }
          }
        }
      }
    })


    observeEvent( qr_code(), {
      input$qrResult
      input$assign_barcode
      # input$assign_barcode2

      if(isTRUE(length(qr_code())>0)){
        shinyjs::runjs("
             var video = document.getElementById('Introduction-video');
             var stream = video.srcObject;
               if (stream) {
                 var tracks = stream.getTracks();
                 tracks.forEach(function(track) {
                   track.stop();
                 });
                 video.srcObject = null;
               }
             ")

        shinyjs::hide("canvas")

        output$scan_result <- renderPrint({
          paste("[",input$qrResult, "]")
        })

        # save scanned barcode in an object
        box_id <- qr_code() #ifelse(length(qr_code())>0, qr_code(), input$assign_barcode2)

        dt <- load_data(glue::glue("SELECT tblBoxIdentity.box_id, tblAssignedBoxIdentity.date
                                      FROM tblBoxIdentity
                                      LEFT JOIN tblAssignedBoxIdentity
                                      ON tblBoxIdentity.box_id = tblAssignedBoxIdentity.box_id
                                      WHERE tblBoxIdentity.location='{location}' AND tblBoxIdentity.box_id='{box_id}';"))

        status <- ifelse(nrow(dt)==0, "Invalid barcode. Please scan another barcode.",
                         ifelse(nrow(dt)>0 & is.na(dt$date) & isFALSE(grepl(tolower(input$variety), tolower(box_id))), "Only scan barcodes of the selected variety.",
                                ifelse(nrow(dt)>0 & !is.na(dt$date) & isFALSE(grepl(tolower(input$variety), tolower(box_id))), "Only scan barcodes of the selected variety.",
                                       ifelse(nrow(dt)>0 & !is.na(dt$date), "Barcode already assigned. Please select another barcode.",
                                              ifelse(nrow(dt)>0 & is.na(dt$date) & isTRUE(grepl(tolower(input$variety), tolower(box_id))), "Proceed",
                                                     "Invalid barcode. Please scan another barcode.")))))

        if(nrow(intro_data())>0){
          if(box_id %in% intro_data()$box_id){
            status <- "Barcode already assigned. Please select another barcode."
          }
        }

        if(status == "Proceed"){
          shinyjs::hide("status")
          shinyjs::show("table")
          shinyjs::show("submit")
          shinyjs::show("clear")

          qr_status(NULL)
          intro_data(rbind(intro_data(), new_box_data()))
          qr_code(NULL)
          updateNumericInput(session, "number_of_thick_mini_cuttings", "Number of thick mini-cuttings (required)", min = 1, value = 0)

        } else {
          shinyjs::show("status")
          qr_status(status)
        }
        shinyjs::enable("assign_barcode")

      }
    })


    output$show_status <- renderText({
      qr_status()
    })


    new_box_data <- reactive({
        data.table::data.table(
          box_id = ifelse(qr_code() == "", "", qr_code()),
          variety = input$variety,
          original_source = input$source,
          receiving_date = input$receiving_date,
          number_of_thick_mini_cuttings = input$number_of_thick_mini_cuttings,
          location = location,
          added_by = user,
          source_box_id = ""
        )
    })


    # show table

    output$table <- DT::renderDT({
      qr_code()
      input$clear
      intro_data <- intro_data()[, c("box_id", "number_of_thick_mini_cuttings")]
      colnames(intro_data) <- stringr::str_to_title(gsub("_", " ", names(intro_data)))

      DT::datatable(intro_data)
    })
    #
    # # ------------------------------SAVE/ SUBMIT -------------------------------------------------

    observeEvent( input$submit , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Receive Planting Material")){

          if(nrow(intro_data())>0){
            shinyjs::disable("submit")
            df <- data.frame( box_id = intro_data()$box_id )
            recut_count <- data.frame( box_id = intro_data()$box_id, cut_number = 0, last_recut = Sys.Date())

            DBI::dbWriteTable(conn, "tblIntroduction", intro_data(), append = T)
            DBI::dbWriteTable(conn, "tblAssignedBoxIdentity", df, append = T)
            DBI::dbWriteTable(conn, "tblRecutCount", recut_count, append = T)

            labels <- isolate(intro_data()[,c(1:2)]) |>
              dplyr::mutate(label=box_id)
            barcode_labels(rbind(barcode_labels(), labels))

            # clear temp data
            intro_data(
              data.frame(
                box_id = character(0),
                variety = character(0),
                original_source = character(0),
                receiving_date = character(0),
                number_of_thick_mini_cuttings = character(0),
                location = character(0),
                added_by = character(0),
                source_box_id = character(0)
              )
            )

            # Reset
            updateSelectInput(session, "variety", "Variety name (required)", choices = c("", toupper(load_data("SELECT variety FROM tblVariety;")$variety)))
            updateSelectInput(session, "source", "Source of planting material (required)", choices = c("", toupper(load_data("SELECT source FROM tblSource;")$source)))
            updateNumericInput(session, "number_of_thick_mini_cuttings", "Number of cuttings", min = 1, value = 0)
            shinyjs::hide("table")
            shinyjs::hide("submit")
            shinyjs::hide("clear")
            shinyjs::hide("status")
            shinyWidgets::show_alert("", "Data saved successfully.", type="success", width = 600)
          }
        }
      }
    })


    # ---------------------------- CLEAR ----------------------

    observeEvent( input$clear , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Receive Planting Material")){

          # clear temp data
          intro_data(
            data.frame(
              box_id = character(0),
              variety = character(0),
              source = character(0),
              receiving_date = character(0),
              number_of_thick_mini_cuttings = character(0),
              location = character(0),
              added_by = character(0)
            )
          )

          barcode_labels(NULL)

          # Reset
          updateSelectInput(session, "variety", "Variety name (required)", choices = c("", toupper(load_data("SELECT variety FROM tblVariety;")$variety)))
          updateSelectInput(session, "source", "Source of planting material (required)", choices = c("", toupper(load_data("SELECT source FROM tblSource;")$source)))
          updateNumericInput(session, "number_of_thick_mini_cuttings", "Number of thick mini-cuttings (required)", min = 1, value = 0)

          shinyjs::hide("table")
          shinyjs::hide("submit")
          shinyjs::hide("download_barcode")
          shinyjs::hide("show_labels")
          shinyjs::hide("clear")
          shinyjs::hide("barcode")
        }
      }

    })




  })
}

## To be copied in the UI
# mod_RP_introduction_ui("Introduction")

## To be copied in the server
# mod_RP_introduction_server("Introduction")
