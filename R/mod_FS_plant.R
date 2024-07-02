#' FS_plant UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_FS_plant_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarLayout(
      sidebarPanel(
        width = 5,

        actionButton(ns("scan_box_identity"), "Field Seed block ID (scan barcode)", icon = icon("camera")),
        textOutput(ns("show_box_id")),

        div( br(),
             tags$video(id = ns("video"), width=300, height=300) |> shinyjs::hidden(),
             tags$canvas(id = ns("canvas"), width=300, height=300) |> shinyjs::hidden()
        ),
        shinyjs::hidden(
          div( id = ns("status"),
               bslib::card(class="border border-warning bg-warning bg-gradient text-white rounded-end",
                           bslib::card_body(textOutput(ns("show_status")))
               )
          )),
        dateInput(ns("date_of_planting"), "Date of planting") |> shinyjs::disabled(),
        numericInput(ns("number_of_plantlets"), "Number of plantlets", min = 1, value=0 ) |> shinyjs::disabled(),
        column(6, actionButton(ns("add2list"), "Add to List", icon = icon("check")) |> shinyjs::disabled())
      ),

      mainPanel(
        width = 7,

        fluidRow(
          shinyWidgets::prettySwitch(ns("view_planting_ready"), "View plantlets ready for planting.", status = "primary", fill = T),
          shinyjs::hidden( DT::DTOutput(ns("planting_ready_table")))
        ),
        fluidRow(
          shinyjs::hidden( DT::DTOutput(ns("table"))),
          column(6, (actionButton(ns("submit"), "Submit", icon = icon("paper-plane"),  class = "pull-right btn btn-primary")) |> shinyjs::hidden())
        )
      )
    )
  )
}

#' FS_plant Server Functions
#'
#' @noRd
mod_FS_plant_server <- function(id, r, location){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    qr_code <- reactiveVal(NULL)
    status <- reactiveVal(NULL)
    number_available_for_planting <- reactiveVal(0)
    planting_data <- reactiveVal(
      data.frame(
        box_id = character(0),
        date_of_planting = lubridate::ymd(character(0)),
        number_of_plantlets = integer(0)
      )
    )

    observeEvent( input$scan_box_identity , {
      if(isTRUE(r$nav == "field")){
        if(isTRUE(r$field_tabs == "Planting")){
          if(isFALSE(is.null(location))){
            qr_code(NULL)
            shinyjs::hide("status")
            shinyjs::show("canvas")

            shinyjs::runjs("
                 navigator.mediaDevices.getUserMedia({ video: { facingMode: 'environment' } }).then(function(stream) {
                    var video = document.getElementById('FS_plant_1-video');
                    video.srcObject = stream;
                    video.play();
                    var canvas = document.getElementById('FS_plant_1-canvas');
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
                        Shiny.setInputValue('FS_plant_1-qrResult', code.data);
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


    observeEvent( input$qrResult , {
      if(!is.null(input$qrResult)){
        qr_code(input$qrResult)
      }
    })

    output$show_box_id <- renderText({
      if(length(qr_code())>0){
        shinyjs::hide("video")
        paste0("[", qr_code(),"]")
      }
    })


    observeEvent( qr_code() , {

      shinyjs::runjs("
                var video = document.getElementById('FS_plant_1-video');
                var stream = video.srcObject;
                if (stream) {
                  var tracks = stream.getTracks();
                  tracks.forEach(function(track) {
                    track.stop();
                  });
                  video.srcObject = null;
                }
              ")

      if(length(qr_code())>0){

        if(nrow(planting_data())>0){
          if(qr_code() %in% planting_data()$box_id){
            status <- "You have already added plants from this box."
          }
        }

        dt <- load_data(glue::glue("SELECT tblBoxIdentity.box_id, tblBoxIdentity.variety, tblIntroduction.receiving_date, tblRecutCount.cut_number,
                      tblExitRP.destination, tblExitRP.sent_to, tblExitRP.number_of_plantlets_sent, tblPencilPlanting.number_of_plantlets, tblPencilSurvival.survival_date,
                      tblPencilSurvival.number_surviving, tblPencilHarvest.last_update, tblPencilHarvest.number_sent_to_field_block, tblFieldPlanting.date_of_planting,
                      tblFieldPlanting.number_of_SAH, tblFieldPlanting.number_of_pencil_stems
                      FROM tblBoxIdentity
                      LEFT JOIN tblIntroduction
                      ON tblBoxIdentity.box_id = tblIntroduction.box_id
                      LEFT JOIN tblRecutCount
                      ON tblIntroduction.box_id = tblRecutCount.box_id
                      LEFT JOIN tblExitRP
                      ON tblRecutCount.box_id = tblExitRP.box_id
                      LEFT JOIN tblPencilPlanting
                      ON tblExitRP.box_id = tblPencilPlanting.box_id
                      LEFT JOIN tblPencilSurvival
                      ON tblPencilSurvival.box_id = tblPencilPlanting.box_id
                      LEFT JOIN tblPencilHarvest
                      ON tblPencilSurvival.box_id = tblPencilHarvest.box_id
                      LEFT JOIN tblFieldPlanting
                      ON tblPencilHarvest.box_id = tblFieldPlanting.box_id
                      WHERE tblBoxIdentity.box_id='{qr_code()}'"))

        if(nrow(harvest_data())>0 & qr_code() %in% harvest_data()$box_id){
          status <- "You have already added plants from this box."
        } else if(nrow(dt)==0){
          status <- "Invalid barcode. Scan the correct barcode"
        } else {
          status <- ifelse(is.na(dt$receiving_date), "Barcode has not been introduced for tracking plantlets. Scan the correct barcode",
                           ifelse(!is.na(dt$receiving_date) & dt$cut_number==0, "Pending: Recut",
                                  ifelse(!is.na(dt$receiving_date) & dt$cut_number>0 & dt$cut_number < 4 & is.na(dt$destination), "Pending: Recut",
                                         ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & is.na(dt$destination), "Pending: Exit from Rapid Propagation",
                                                ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & is.na(dt$number_of_plantlets) & dt$sent_to=="Pencil Block", "Pending: Planting in Pencil Block",
                                                       ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & !is.na(dt$number_of_plantlets) & dt$sent_to=="Pencil Block" & dt$number_of_plantlets_sent > dt$number_of_plantlets & is.na(dt$survival_date), "Pending: Planting in Pencil Block",
                                                              ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & !is.na(dt$number_of_plantlets) & dt$sent_to=="Pencil Block" & dt$number_of_plantlets > 0 & is.na(dt$survival_date), "Pending: Pencil Stem Block - Survival after 2 weeks",
                                                                     ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & !is.na(dt$number_of_plantlets) & dt$number_of_plantlets_sent <= dt$number_of_plantlets & is.na(dt$survival_date), "Pending: Pencil Stem Block - Survival after 2 weeks",
                                                                            ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & !is.na(dt$number_of_plantlets) & !is.na(dt$survival_date) & is.na(dt$last_update), "Pending Pencil Stem Block: Harvest",
                                                                                   ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & !is.na(dt$number_of_plantlets) & !is.na(dt$survival_date) & !is.na(dt$last_update) & dt$number_sent_to_field_block > 0, "Proceed",
                                                                                          ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & !is.na(dt$number_of_plantlets) & !is.na(dt$survival_date) & !is.na(dt$last_update) & dt$number_harvested >= dt$number_surviving, "There are no more plants to harvest from this box.",
                                                                                                 "Don't know yet")))))))))))
        }

        n <- ifelse(is.na(dt$number_of_plantlets), dt$number_of_plantlets_sent, (dt$number_of_plantlets_sent - dt$number_of_plantlets))
        number_available_for_planting( n)
        status(status)

        if(status == "Proceed"){
          shinyjs::hide("status")
          shinyjs::enable("date")
          shinyjs::enable("number_of_plantlets")
          shinyjs::enable("add2list")
        } else {
          shinyjs::show("status")
          shinyjs::disable("date")
          shinyjs::disable("number_of_plantlets")
          shinyjs::disable("add2list")
        }
        shinyjs::hide("canvas")

      }
    })


    output$show_status <- renderText({
      status()
    })


    # ----------------------------------Add plantlets to List
    planting_record <- reactive({
      data.frame(
        box_id = qr_code(),
        date_of_planting = input$date_of_planting,
        number_of_plantlets = input$number_of_plantlets
      )
    })

    observeEvent( input$add2list , {
      if(input$number_of_plantlets == 0){
        shinyWidgets::show_alert("", text = "Enter number of plantlets", type = "warning")
        shinyjs::hide("table")
      } else if(isTRUE(input$number_of_plantlets > number_available_for_planting())){
        shinyWidgets::show_alert("", text = paste("Number of plantlets Exceeds number available for planting:", number_available_for_planting()), , type = "warning")
        shinyjs::hide("table")
      } else {
        planting_data(rbind(planting_data(), planting_record())) # add new record to planting data
        updateNumericInput(session, "number_of_plantlets", "Number of plantlets", value = 0, min = 0) # reset number of plantlets
        output$show_box_id <- renderText({ NULL }) # remove the previous barcode scanned
        shinyjs::show("table")
        shinyjs::show("submit")
      }
    })


    output$table <- DT::renderDT({
      planting_data()
    })


    # Submit ----------------------
    observeEvent( input$submit , {

      DBI::dbWriteTable(conn, "tblPencilPlantingDetails", planting_data(), append=T)
      shinyWidgets::show_alert("", "Data saved successfully.", type="success", width = 600)

      updateNumericInput(session, "number_of_plantlets", "Number of plantlets", value = 0, min = 0) # reset number of plantlets
      output$show_box_id <- renderText({ NULL }) # remove the previous barcode scanned
      qr_code(NULL)
      status(NULL)
      planting_data(
        data.frame(
          box_id = character(0),
          date_of_planting = lubridate::ymd(character(0)),
          number_of_plantlets = integer(0)
        )
      )
      shinyjs::hide("status")
      shinyjs::disable("date")
      shinyjs::disable("number_of_plantlets")
      shinyjs::disable("add2list")
      shinyjs::hide("table")
      shinyjs::hide("planting_ready_table")
      shinyjs::hide("submit")

    })


    observeEvent( input$submit , {
      if(isTRUE(r$nav == "field")){
        if(isTRUE(r$field_tabs == "Planting")){
          Sys.sleep(5)
          dt <- load_data("SELECT box_id, SUM(number_of_plantlets) AS number_of_plantlets FROM tblPencilPlantingDetails GROUP BY box_id;")
          if(nrow(dt)>0){
            for (i in 1:nrow(dt)){
              tryCatch({
                update_query( glue::glue("UPDATE tblPencilPlanting SET number_of_plantlets='{dt$number_of_plantlets[i]}', last_update='{Sys.Date()}' WHERE box_id='{dt$box_id[i]}';"))
              })
            }
          } else {
            dt$last_update <- Sys.Date()
            DBI::dbWriteTable(conn, "tblPencilPlanting", dt, append=T)

          }
        }
      }
    })


    # ---------------------------------- View Planting Ready Plants
    # observeEvent( input$view_planting_ready , {
    #   if(input$view_planting_ready){
    #     shinyjs::show("planting_ready_table")
    #   } else {
    #     shinyjs::hide("planting_ready_table")
    #   }
    # })
    #
    # ready_2_plant <- reactive({
    #   dt <- load_data(glue::glue("SELECT tblBoxIdentity.box_id, tblExitRP.number_of_plantlets_sent, tblPencilPlanting.number_of_plantlets
    #                   FROM tblBoxIdentity
    #                   LEFT JOIN tblIntroduction
    #                   ON tblBoxIdentity.box_id = tblIntroduction.box_id
    #                   LEFT JOIN tblRecutCount
    #                   ON tblIntroduction.box_id = tblRecutCount.box_id
    #                   LEFT JOIN tblExitRP
    #                   ON tblRecutCount.box_id = tblExitRP.box_id
    #                   LEFT JOIN tblPencilPlanting
    #                   ON tblExitRP.box_id = tblPencilPlanting.box_id
    #                   WHERE tblBoxIdentity.location='{location}' AND tblExitRP.sent_to='Pencil Block' AND tblExitRP.number_of_plantlets_sent > 0 AND tblPencilPlanting.number_of_plantlets < tblExitRP.number_of_plantlets_sent"))
    #
    #   dt$number_available_for_planting <- dt$number_of_plantlets_sent - dt$number_of_plantlets
    #   names(dt)[names(dt) == "number_of_plantlets_sent"] <- "number_of_plantlets_sent_to_pencil_block"
    #   names(dt)[names(dt) == "number_of_plantlets"] <- "number_of_plants_planted"
    #   colnames(dt) <- stringr::str_to_title(gsub("_"," ", names(dt)))
    #   dt
    # })
    #
    # output$planting_ready_table <- DT::renderDT({
    #   ready_2_plant()
    # })
  })
}

## To be copied in the UI
# mod_FS_plant_ui("FS_plant_1")

## To be copied in the server
# mod_FS_plant_server("FS_plant_1")
