#' LABELS_replace UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_LABELS_replace_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(8, offset = 2,
           bslib::card(
             actionButton(ns("scan_barcode"), "Scan Barcode"),
             div(
               tags$video(id = ns("video"), width=400, height=400),
               tags$canvas(id = ns("canvas"), width=400, height=400) |> shinyjs::hidden()
             ), br(),
             textOutput(ns("txt")),

             DT::DTOutput(ns("table")) |> shinyjs::hidden()
           )
        )
  )
}

#' LABELS_replace Server Functions
#'
#' @noRd
mod_LABELS_replace_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    qr_code <- reactiveVal()

    labels_data <- reactiveVal(
      data.table::data.table(
        box_id = character(0),
        variety = character(0),
        location = character(0)
      )
    )


    observeEvent(input$scan_barcode, {
      if(isTRUE(r$nav == "Replace Barcode Labels")){
          shinyjs::disable("scan_barcode")
          shinyjs::show("video")
          shinyjs::runjs("
            navigator.mediaDevices.getUserMedia({ video: { facingMode: 'environment' } }).then(function(stream) {
              var video = document.getElementById('LABELS_replace_1-video');
              video.srcObject = stream;
              video.play();
              var canvas = document.getElementById('LABELS_replace_1-canvas');
              var context = canvas.getContext('2d');
              var captureInterval = setInterval(function() {
                context.drawImage(video, 0, 0, 400, 400);
                var imageData = context.getImageData(0, 0, 400, 400);
                var base64Image = canvas.toDataURL('image/png');
                Shiny.setInputValue('LABELS_replace_1-imageData', base64Image);
              }, 500); // Increased interval for better performance
            }).catch(function(err) {
              console.error('Error accessing camera: ', err);
            });
          ")
      }
    })


    observeEvent(input$imageData, { # qr_scan_result <- reactive({
      if(isTRUE(r$nav == "Replace Barcode Labels")){
        input$scan_barcode
        req(input$imageData)
        imageData <- input$imageData
        if (!is.null(imageData)) {
          # Remove the "data:image/png;base64," prefix from the base64 image data
          imageData <- gsub("data:image/png;base64,", "", imageData)
          # Convert the base64 image data to binary
          binaryImageData <- base64enc::base64decode(imageData)
          # Read the binary image data into an image object
          magickImage <- magick::image_read(binaryImageData)
          # Decode the QR code using qr_decode()
          qr_code(try(quadrangle::qr_scan(magickImage)$values$value, silent = TRUE))
        }
      }
    })


    observeEvent( qr_code() , {
      if(isTRUE(r$nav == "Replace Barcode Labels")){
        if(isTRUE(length(qr_code())>0)){
          #updateTextInput(session, "result", "Identity", value = qr_scan_result())
          #stopVideoStream(input$video)
          shinyjs::runjs("
            var video = document.getElementById('LABELS_replace_1-video');
            var stream = video.srcObject;
            if (stream) {
              var tracks = stream.getTracks();
              tracks.forEach(function(track) {
                track.stop();
              });
              video.srcObject = null;
            }
          ")

          shinyjs::hide("video")

          # save scanned barcode in an object
          dt <- load_data(paste0("SELECT box_id, variety, location FROM tblBoxIdentity WHERE box_id='", trimws(qr_code()), "';"))
          labels_data(rbind(labels_data(), dt))
        }
      }
    })

    observe( {
      if(isTRUE(r$nav == "Replace Barcode Labels")){

        if(nrow(labels_data())>0){
          shinyjs::show("table")
        }
      }

    })

    output$table <- DT::renderDT({
      DT::datatable(labels_data())
    })



  })
}

## To be copied in the UI
# mod_LABELS_replace_ui("LABELS_replace_1")

## To be copied in the server
# mod_LABELS_replace_server("LABELS_replace_1")
