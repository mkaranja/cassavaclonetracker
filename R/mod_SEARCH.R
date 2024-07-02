#' SEARCH UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SEARCH_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bslib::card(
        bslib::card_header(fluidRow(column(2, actionButton(ns("search_box_id"), "Scan Barcode", icon = icon("camera"))), column(3, align="left", textOutput(ns("scanned_qr_code"))))),
        fluidRow(

         div(
           tags$video(id = ns("video"), width=300, height=300) |> shinyjs::hidden(),
           tags$canvas(id = ns("canvas"), width=300, height=300) #|> shinyjs::hidden()
         ),

         DT::DTOutput(ns("table")) |> shinycssloaders::withSpinner(),
         div( id = ns("status"),
              bslib::card(class="border border-danger bg-danger bg-gradient text-white rounded-end",
                          bslib::card_body(textOutput(ns("show_status")))
              )
         ) |> shinyjs::hidden()
        )
      )
    )
  )
}

#' SEARCH Server Functions
#'
#' @noRd
mod_SEARCH_server <- function(id, r, admin, location){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    search_data <- reactiveVal(
      data.frame(
        box_id = character(0)
      )
    )

    qr_code <- reactiveVal(NULL)

    observeEvent( input$search_box_id , {
      if(isTRUE(r$nav == "search")){

        qr_code(NULL)
        shinyjs::hide("status")
        shinyjs::show("canvas")

        shinyjs::runjs("
                 navigator.mediaDevices.getUserMedia({ video: { facingMode: 'environment' } }).then(function(stream) {
                    var video = document.getElementById('SEARCH_1-video');
                    video.srcObject = stream;
                    video.play();
                    var canvas = document.getElementById('SEARCH_1-canvas');
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
                        Shiny.setInputValue('SEARCH_1-qrResult', code.data);
                      }
                    }, 500); // Increased interval for better performance
                 }).catch(function(err) {
                    console.error('Error accessing camera: ', err);
                 });
                ")

      }
    })

    observeEvent( input$qrResult , {
      if(!is.null(input$qrResult)){
        qr_code(input$qrResult)
      }
    })

    observeEvent( qr_code(),{
      if(isTRUE(r$nav == "search")){
        if(isFALSE(is.null(location))){
          if(length(qr_code()>0)){

            shinyjs::runjs("
                var video = document.getElementById('SEARCH_1-video');
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
            shinyjs::show("scanned_qr_code")

            output$scanned_qr_code <- renderText({
              if(length(qr_code())>0){
                qr_code()
              }
            })

            if(admin){
              dt <- load_data(glue::glue("SELECT tblBoxIdentity.box_id, tblBoxIdentity.variety, tblIntroduction.receiving_date AS Received_Date, tblRecutCount.cut_number,
                      tblExitRPDetails.destination, tblExitRPDetails.sent_to, tblExitRPDetails.number_of_plantlets, tblPencilPlanting.number_of_plantlets, tblPencilSurvival.survival_date,
                      tblPencilSurvival.number_surviving, tblPencilHarvest.last_update, tblPencilHarvest.number_sent_to_field_block, tblFieldPlanting.date_of_planting,
                      tblFieldPlanting.number_of_SAH, tblFieldPlanting.number_of_pencil_stems
                      FROM tblBoxIdentity
                      LEFT JOIN tblIntroduction
                      ON tblBoxIdentity.box_id = tblIntroduction.box_id
                      LEFT JOIN tblRecutCount
                      ON tblIntroduction.box_id = tblRecutCount.box_id
                      LEFT JOIN tblExitRPDetails
                      ON tblRecutCount.box_id = tblExitRPDetails.box_id
                      LEFT JOIN tblPencilPlanting
                      ON tblExitRPDetails.box_id = tblPencilPlanting.box_id
                      LEFT JOIN tblPencilSurvival
                      ON tblPencilSurvival.box_id = tblPencilPlanting.box_id
                      LEFT JOIN tblPencilHarvest
                      ON tblPencilSurvival.box_id = tblPencilHarvest.box_id
                      LEFT JOIN tblFieldPlanting
                      ON tblPencilHarvest.box_id = tblFieldPlanting.box_id
                      WHERE tblBoxIdentity.box_id='{qr_code()}'"))
            } else {
              dt <- load_data(glue::glue("SELECT tblBoxIdentity.box_id, tblBoxIdentity.variety, tblIntroduction.receiving_date, tblRecutCount.cut_number,
                      tblExitRPDetails.destination, tblExitRPDetails.sent_to, tblExitRPDetails.number_of_plantlets, tblPencilPlanting.number_of_plantlets, tblPencilSurvival.survival_date,
                      tblPencilSurvival.number_surviving, tblPencilHarvest.last_update, tblPencilHarvest.number_sent_to_field_block, tblFieldPlanting.date_of_planting,
                      tblFieldPlanting.number_of_SAH, tblFieldPlanting.number_of_pencil_stems
                      FROM tblBoxIdentity
                      LEFT JOIN tblIntroduction
                      ON tblBoxIdentity.box_id = tblIntroduction.box_id
                      LEFT JOIN tblRecutCount
                      ON tblIntroduction.box_id = tblRecutCount.box_id
                      LEFT JOIN tblExitRPDetails
                      ON tblRecutCount.box_id = tblExitRPDetails.box_id
                      LEFT JOIN tblPencilPlanting
                      ON tblExitRPDetails.box_id = tblPencilPlanting.box_id
                      LEFT JOIN tblPencilSurvival
                      ON tblPencilSurvival.box_id = tblPencilPlanting.box_id
                      LEFT JOIN tblPencilHarvest
                      ON tblPencilSurvival.box_id = tblPencilHarvest.box_id
                      LEFT JOIN tblFieldPlanting
                      ON tblPencilHarvest.box_id = tblFieldPlanting.box_id
                      WHERE tblBoxIdentity.box_id='{qr_code()}' AND tblBoxIdentity.location='{location}'"))
            }

            if(nrow(dt)==0){
              shinyjs::show("status")
              output$show_status <- renderText({
                "Barcode Id NOT found."
              })

            }

            search_data(dt)

          }
        }
      }
    })

    # Search
    output$table <- DT::renderDT({
      dt <- search_data() |>
        janitor::remove_empty("cols")
      colnames(dt) <- stringr::str_to_title(gsub("_", " ", names(dt)))
      DT::datatable(dt,
                    options = list(scrollX=TRUE, scrollCollapse=TRUE))
    })
  })
}

## To be copied in the UI
# mod_SEARCH_ui("SEARCH_1")

## To be copied in the server
# mod_SEARCH_server("SEARCH_1")
