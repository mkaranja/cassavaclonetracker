#' FS_harvest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_FS_harvest_ui <- function(id){
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
        dateInput(ns("date_of_harvest"), "Date of planting") |> shinyjs::disabled(),
        numericInput(ns("number_harvested"), "Number of plantlets", min = 1, value=0 ) |> shinyjs::disabled(),
        shinyWidgets:: awesomeRadio(ns("destination"), "Destination", choices = c("Sale", "Field Seed Block"), inline = FALSE),
        column(6, actionButton(ns("add2list"), "Add to List", icon = icon("check")))
      ),

      mainPanel(
        width = 7,

        fluidRow(
          shinyWidgets::prettySwitch(ns("view_harvest_ready"), "View plantlets waiting harvestiong", status = "primary", fill = T),
          shinyjs::hidden( DT::DTOutput(ns("harvest_ready_table")))
        ),
        fluidRow(
          shinyjs::hidden( DT::DTOutput(ns("table"))),
          column(6, (actionButton(ns("submit"), "Submit", icon = icon("paper-plane"),  class = "pull-right btn btn-primary")) |> shinyjs::hidden())
        )
      )
    )
  )
}

#' FS_harvest Server Functions
#'
#' @noRd
mod_FS_harvest_server <- function(id, r, location){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    qr_code <- reactiveVal(NULL)
    status <- reactiveVal(NULL)
    survival_number <- reactiveVal(0)
    harvest_data <- reactiveVal(
      data.frame(
        box_id = character(0),
        date_of_harvest = lubridate::ymd(character(0)),
        number_harvested = integer(0),
        destination = character(0)
      )
    )

    observeEvent( input$scan_box_identity , {
      if(isTRUE(r$nav == "field")){
        if(isTRUE(r$field_tabs == "Harvest")){
          if(isFALSE(is.null(location))){
            qr_code(NULL)
            shinyjs::hide("status")
            shinyjs::show("canvas")
            shinyWidgets::updatePrettySwitch(session, "view_harvest_ready", "View plantlets ready for harvest", value = FALSE)

            shinyjs::runjs("
                 navigator.mediaDevices.getUserMedia({ video: { facingMode: 'environment' } }).then(function(stream) {
                    var video = document.getElementById('FS_harvest_1-video');
                    video.srcObject = stream;
                    video.play();
                    var canvas = document.getElementById('FS_harvest_1-canvas');
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
                        Shiny.setInputValue('FS_harvest_1-qrResult', code.data);
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
                var video = document.getElementById('FS_harvest_1-video');
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

        if(nrow(harvest_data())>0){
          if(qr_code() %in% harvest_data()$box_id){
            status <- "You have already added plants from this box."
          }
        }

        dt <- load_data(glue::glue("SELECT tblBoxIdentity.box_id, tblBoxIdentity.variety, tblIntroduction.receiving_date, tblRecutCount.cut_number,
                      tblExitRP.destination, tblExitRP.sent_to, tblExitRP.number_of_plantlets_sent, tblPencilPlanting.number_of_plantlets, tblPencilSurvival.survival_date, tblPencilSurvival.number_surviving, tblPencilHarvest.date_of_harvest, tblPencilHarvest.number_harvested
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
                      WHERE tblBoxIdentity.box_id='{qr_code()}'"))

        if(nrow(dt)==0){
          status <- "Invalid barcode. Scan the correct barcode"
        } else {
          status <- ifelse(is.na(dt$receiving_date), "Barcode has not been introduced for tracking plantlets. Scan the correct barcode",
                           ifelse(!is.na(dt$receiving_date) & dt$cut_number==0, "Recut pending",
                                  ifelse(!is.na(dt$receiving_date) & dt$cut_number>0 & dt$cut_number < 4 & is.na(dt$destination), "Recut pending",
                                         ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & is.na(dt$destination), "Box is waiting Exit from Rapid Propagation",
                                                ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & is.na(dt$number_of_plantlets) & dt$sent_to=="Pencil Block", "Pending Planting in Pencil Block",
                                                       ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & !is.na(dt$number_of_plantlets) & dt$sent_to=="Pencil Block" & dt$number_of_plantlets_sent > dt$number_of_plantlets & is.na(dt$survival_date), "Pending Planting in Pencil Block",
                                                              ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & !is.na(dt$number_of_plantlets) & dt$sent_to=="Pencil Block" & dt$number_of_plantlets > 0 & is.na(dt$survival_date), "Pending survival after 2 weeks",
                                                                     ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & !is.na(dt$number_of_plantlets) & dt$number_of_plantlets_sent <= dt$number_of_plantlets & is.na(dt$survival_date), "Pending survival after 2 weeks",
                                                                            ifelse(!is.na(dt$receiving_date) & dt$cut_number >= 4 & !is.na(dt$destination) & !is.na(dt$number_of_plantlets) & !is.na(dt$survival_date), "Proceed",
                                                                                   "Don't know yet")))))))))
        }

        n <- ifelse(is.na(dt$number_harvested), dt$number_surviving, (dt$number_surviving - dt$number_harvested))
        survival_number(n)
        status(status)

        if(status == "Proceed"){
          shinyjs::hide("status")
          shinyjs::enable("date_of_harvest")
          shinyjs::enable("number_harvested")
          #shinyjs::enable("add2list")
        } else {
          shinyjs::show("status")
          shinyjs::disable("date_of_harvest")
          shinyjs::disable("number_harvested")
          #shinyjs::disable("add2list")
          tablerDash::tablerAlert(title = "Alert", status, icon = "alert-triangle", status = "warning")

        }
        shinyjs::hide("canvas")

      }
    })


    output$show_status <- renderText({
      status()
    })


    # ----------------------------------Add plantlets to List
    harvest_record <- reactive({
      data.frame(
        box_id = character(0),
        date_of_harvest = lubridate::ymd(character(0)),
        number_harvested = integer(0),
        destination = character(0)
      )
    })

    observeEvent( input$add2list , {
      req(qr_code())
      if(input$number_harvested == 0){
        tablerDash::tablerAlert(title = "Alert", "Enter number of plantlets harvested", icon = "alert-triangle", status = "warning")
        shinyjs::hide("table")
      } else if(isTRUE(input$number_harvested > survival_number())){
        shinyWidgets::show_alert("", text = paste("Number of plantlets Exceeds surviving plantlets:", survival_number()), type = "warning")
        shinyjs::hide("table")
      } else {
        harvest_data(rbind(harvest_data(), harvest_record())) # add new record to planting data
        updateNumericInput(session, "number_harvested", "Number harvested", value = 0, min = 0) # reset number of plantlets
        output$show_box_id <- renderText({ NULL }) # remove the previous barcode scanned
        shinyjs::show("table")
        shinyjs::show("submit")
      }
    })


    output$table <- DT::renderDT({
      harvest_data()
    })


    # Submit ----------------------
    observeEvent( input$submit , {

      DBI::dbWriteTable(conn, "tblPencilHarvest", harvest_data(), append=T)
      shinyWidgets::show_alert("", "Data saved successfully.", type="success", width = 600)

      updateNumericInput(session, "number_harvested", "Number harvested", value = 0, min = 0) # reset number of plantlets
      output$show_box_id <- renderText({ NULL }) # remove the previous barcode scanned
      qr_code(NULL)
      status(NULL)
      harvest_data(
        data.frame(
          box_id = character(0),
          date_of_harvest = lubridate::ymd(character(0)),
          number_of_plantlets = integer(0)
        )
      )
      shinyjs::hide("status")
      shinyjs::disable("date_of_harvest")
      shinyjs::disable("number_harvested")
      #shinyjs::disable("add2list")
      shinyjs::hide("table")
      shinyjs::hide("harvest_ready_table")
      shinyjs::hide("submit")

    })


    # observeEvent( input$submit , {
    #   if(isTRUE(r$nav == "field")){
    #     if(isTRUE(r$field_tabs == "Survival after 2 weeks")){
    #       Sys.sleep(5)
    #       dt <- load_data("SELECT box_id, SUM(number_of_plantlets) AS number_of_plantlets FROM tblPencilPlantingDetails GROUP BY box_id;")
    #       if(nrow(dt)>0){
    #         for (i in 1:nrow(dt)){
    #           tryCatch({
    #             update_query( glue::glue("UPDATE tblPencilSurvival SET number_of_plantlets='{dt$number_of_plantlets[i]}', last_update='{Sys.Date()}' WHERE box_id='{dt$box_id[i]}';"))
    #           })
    #         }
    #       } else {
    #         dt$last_update <- Sys.Date()
    #         DBI::dbWriteTable(conn, "tblPencilSurvival", dt, append=T)
    #
    #       }
    #     }
    #   }
    # })

    # ---------------------------------- View ready to record survival
    # observeEvent( input$view_harvest_ready , {
    #   if(input$view_harvest_ready){
    #     shinyjs::show("harvest_ready_table")
    #   } else {
    #     shinyjs::hide("harvest_ready_table")
    #   }
    # })
    #
    # harvest_ready <- reactive({
    #   dt <- load_data(glue::glue("SELECT tblBoxIdentity.box_id, tblPencilPlanting.number_of_plantlets, tblPencilSurvival.number_surviving, tblPencilHarvest.number_harvested
    #                   FROM tblBoxIdentity
    #                   LEFT JOIN tblIntroduction
    #                   ON tblBoxIdentity.box_id = tblIntroduction.box_id
    #                   LEFT JOIN tblRecutCount
    #                   ON tblIntroduction.box_id = tblRecutCount.box_id
    #                   LEFT JOIN tblExitRP
    #                   ON tblRecutCount.box_id = tblExitRP.box_id
    #                   LEFT JOIN tblPencilPlanting
    #                   ON tblExitRP.box_id = tblPencilPlanting.box_id
    #                   LEFT JOIN tblPencilSurvival
    #                   ON tblPencilPlanting.box_id = tblPencilSurvival.box_id
    #                   LEFT JOIN tblPencilHarvest
    #                   ON tblPencilSurvival.box_id = tblPencilHarvest.box_id
    #                   WHERE tblBoxIdentity.location='{location}' AND tblExitRP.sent_to='Pencil Block' AND tblExitRP.number_of_plantlets_sent > 0 AND tblPencilPlanting.number_of_plantlets > 0 AND tblPencilSurvival.number_surviving > 0 AND tblPencilHarvest.number_harvested IS NULL"))
    #
    #   names(dt)[names(dt) == "number_of_plantlets"] <- "survival_number"
    #   colnames(dt) <- stringr::str_to_title(gsub("_"," ", names(dt)))
    #   dt
    # })
    #
    # output$harvest_ready_table <- DT::renderDT({
    #   harvest_ready()
    # })
  })
}

## To be copied in the UI
# mod_FS_harvest_ui("FS_harvest_1")

## To be copied in the server
# mod_FS_harvest_server("FS_harvest_1")
