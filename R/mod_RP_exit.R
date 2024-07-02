#' RP_exit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_RP_exit_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarLayout(
      sidebarPanel(
        width = 5,
        actionButton(ns("scan_box_identity"), "Box Identity (scan barcode)", icon = icon("camera")), br(),

        textOutput(ns("scanned_qr_code")) |> shinyjs::hidden(), br(),
        div( id = ns("status"),
             bslib::card(class="border border-danger bg-danger bg-gradient text-white rounded-end",
                         bslib::card_body(textOutput(ns("show_status")))
             )
        ) |> shinyjs::hidden(),

        div(
          tags$video(id = ns("video"), width=300, height=300) |> shinyjs::hidden(),
          tags$canvas(id = ns("canvas"), width=300, height=300) |> shinyjs::hidden()
        ),

        dateInput(ns("exit_date"), "Date of Exit", max = Sys.Date()) |> shinyjs::disabled(),
        numericInput(ns("number_of_plantlets"), "Number of plantlets", value=0) |> shinyjs::disabled(),
        shinyWidgets::awesomeRadio(ns("sent_to"),"Send To: ", choices = c("Pencil Block", "Field Seed Block"), inline = FALSE, status = "primary")  |> shinyjs::disabled(),
        selectInput(ns("destination"), "Destination", choices = c("")) |> shinyjs::disabled(),
        actionButton(ns("add2list"), "Add to List", icon = icon("check")) |> shinyjs::disabled()
      ),

      mainPanel(
        width = 6,

        fluidRow(
          shinyWidgets::prettySwitch(ns("view_exit_ready"), "View plantlets ready to exit rapid propagation", status = "primary", fill = T),
          DT::DTOutput(ns("exit_ready_table")) |> shinyjs::hidden(),
          DT::DTOutput(ns("table")) |> shinyjs::hidden()
          ), br(),
        fluidRow(
          column(2, shinyjs::hidden(actionButton(ns("submit"), "Submit", icon = icon("paper-plane"),  class = "pull-right btn btn-success", width = "100%"))),
          column(2, offset = 8, shinyjs::hidden(actionButton(ns("clear"), "Clear", icon = icon("eraser"), class = "pull-right btn btn-primary", width = "100%")))
        ), br(), br()
      )
    )
  )
}

#' RP_exit Server Functions
#'
#' @noRd
mod_RP_exit_server <- function(id, r, location, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    qr_status <- reactiveVal()
    qr_code <- reactiveVal()
    # available_cuttings <- reactiveVal()

    exit_data <- reactiveVal(
      data.frame(
        box_id=character(0),
        exit_date = lubridate::ymd(character(0)),
        number_of_plantlets = integer(0),
        destination = character(0),
        added_by = character(0)
      )
    )

    # Scan Barcode

    observeEvent( input$scan_box_identity , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Exit Rapid Propapagtion")){

          qr_code(NULL)
          shinyjs::hide("status")
          shinyjs::show("canvas")

          shinyjs::runjs("
                 navigator.mediaDevices.getUserMedia({ video: { facingMode: 'environment' } }).then(function(stream) {
                    var video = document.getElementById('Exit-video');
                    video.srcObject = stream;
                    video.play();
                    var canvas = document.getElementById('Exit-canvas');
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
                        Shiny.setInputValue('Exit-qrResult', code.data);
                      }
                    }, 500); // Increased interval for better performance
                 }).catch(function(err) {
                    console.error('Error accessing camera: ', err);
                 });
                ")
        }
      }
    })




    observeEvent( input$qrResult , {
      if(!is.null(input$qrResult)){
        qr_code(input$qrResult)
      }
    })

    observeEvent( qr_code(),{
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Exit Rapid Propapagtion")){
          if(isFALSE(is.null(location))){
          if(length(qr_code()>0)){

            shinyjs::runjs("
                var video = document.getElementById('Exit-video');
                var stream = video.srcObject;
                if (stream) {
                  var tracks = stream.getTracks();
                  tracks.forEach(function(track) {
                    track.stop();
                  });
                  video.srcObject = null;
                }
              ")

            shinyjs::show("scanned_qr_code")

            output$scanned_qr_code <- renderText({
              if(length(qr_code())>0){
                paste0("[", qr_code(), "]")
              }
            })


            box_id <- qr_code()

              dt <- load_data(glue::glue(
                "SELECT tblBoxIdentity.box_id, tblBoxIdentity.variety, tblAssignedBoxIdentity.date, tblIntroduction.receiving_date, tblRecutCount.cut_number, tblRecut.number_of_cuttings,
                  tblExit2Pencil.destination, tblExit2Pencil.number_of_plantlets
                  FROM tblBoxIdentity
                  LEFT JOIN tblAssignedBoxIdentity
                  ON tblBoxIdentity.box_id = tblAssignedBoxIdentity.box_id
                  LEFT JOIN tblIntroduction
                  ON tblBoxIdentity.box_id = tblIntroduction.box_id
                  LEFT JOIN tblRecutCount
                  ON tblBoxIdentity.box_id = tblRecutCount.box_id
                  LEFT JOIN tblRecut
                  ON tblBoxIdentity.box_id = tblRecut.source_box_id
                  LEFT JOIN tblExit2Pencil
                  ON tblBoxIdentity.box_id = tblExit2Pencil.box_id
                  WHERE tblBoxIdentity.location='{location}' AND tblBoxIdentity.box_id='{box_id}';"))

              dt2 <- load_data(glue::glue("SELECT box_id, SUM(number_of_plantlets) AS number_of_plantlets FROM tblExit2Pencil WHERE box_id='{box_id}' GROUP BY box_id"))

              status <- ifelse(nrow(dt) == 0, "Invalid barcode. Please scan another barcode.",
                               ifelse(nrow(dt)>0 & is.na(dt$date), "Not assigned. Please scan another barcode.",
                                      ifelse(nrow(dt)>0 & !is.na(dt$date) & !is.na(dt$receiving_date) & dt$cut_number==0, "Pending Recut",
                                             ifelse(nrow(dt)>0 & !is.na(dt$date) & !is.na(dt$receiving_date) & dt$cut_number>0 & dt$cut_number<4, "Pending Recut",
                                                    # ifelse(nrow(dt)>0 & !is.na(dt$date) & isFALSE(grepl(tolower(input$variety), tolower(box_id))), "Only add cuttings of the same variety.",
                                                    ifelse(nrow(dt)>0 & !is.na(dt$date) & !is.na(dt$receiving_date) & dt$cut_number>0 & dt$cut_number>=4 & is.na(dt$destination), "Proceed",
                                                           ifelse(nrow(dt)>0 & !is.na(dt$date) & !is.na(dt$receiving_date) & dt$cut_number>0 & dt$cut_number>=4 & !is.na(dt$destination) & dt$destination == location & is.na(dt$number_of_plantlets), "Proceed",
                                                                  ifelse(nrow(dt)>0 & !is.na(dt$date) & !is.na(dt$receiving_date) & dt$cut_number>0 & dt$cut_number>=4 & !is.na(dt$destination) & dt$destination == location  & dt$number_of_plantlets < dt$number_of_cuttings, "Proceed",
                                                                         #ifelse(nrow(dt)>0 & !is.na(dt$date) & !is.na(dt$receiving_date) & dt$cut_number>0 & dt$cut_number>=4 & !is.na(dt$destination) & dt$destination == location  & dt$number_of_plantlets > (dt$number_sent_to_pencil_stem_block + dt$number_sent_to_field_seed_block), "Proceed",
                                                                                ifelse(nrow(dt)>0 & !is.na(dt$date) & !is.na(dt$receiving_date) & dt$cut_number>0 & dt$cut_number>=4 & !is.na(dt$number_of_plantlets) & dt$destination == location  & dt$number_of_plantlets <= (dt$number_sent_to_pencil_stem_block + dt$number_sent_to_field_seed_block), "Exit from rapid propagation has been recorded.",
                                                                         ""))))))))

              if(nrow(exit_data())>0 & box_id %in% exit_data()$box_id){
                status <- "Plants from this box has been selected."
              }

              # n <- load_data("SELECT number_of_cuttings FROM tblRecut WHERE source_box_id='{box_id}' AND cut_number>=4")$number_of_cuttings
              # available_cuttings(n)

            if(isTRUE(status == "Proceed")){

              shinyjs::enable("exit_date")
              shinyjs::enable("number_of_plantlets")
              shinyjs::enable("sent_to")
              shinyjs::enable("destination")
              shinyjs::enable("add2list")
              shinyjs::hide("status")
              qr_status(NULL)
            } else {
              shinyjs::show("status")
              qr_status(status)
            }
            shinyjs::hide("canvas")
           }
          }
        }
      }
    })


    output$show_status <- renderText({
      qr_status()
    })


    # update location

    observe({
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Exit Rapid Propapagtion")){
          all_loc <- load_data("SELECT location FROM tblLocation;")$location
          updateSelectInput(session, "destination", "Destination", choices = c(all_loc), selected = location)
        }
      }
    })


    # Read box identity

    new_record <- reactive({
      data.frame(
        box_id = qr_code(),
        exit_date = input$exit_date,
        destination = input$destination,
        number_of_plantlets = input$number_of_plantlets,
        added_by = user
      )
    })


    observeEvent( input$add2list , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Exit Rapid Propapagtion")){
          if(length(qr_code())==0){
            shinyWidgets::show_alert("", "Scan the barcode", type="warning")
          } else if(input$number_of_plantlets == 0){
              shinyWidgets::show_alert("", "Enter the number of plantletss.", type="warning")
          # } else if(input$number_of_plantlets > available_cuttings()){
          #   shinyWidgets::show_alert("", paste("Number exceeds number of available cuttings:", available_cuttings()), type="warning")
          } else {
            exit_data(rbind(exit_data(), new_record()))
            shinyjs::show("table")
            shinyjs::show("submit")
            shinyjs::show("clear")
          }
        }
      }
    })


    output$table <- DT::renderDT({
      exit_data <- exit_data()
      colnames(exit_data) <- stringr::str_to_title(gsub("_", " ", names(exit_data)))
      DT::datatable(exit_data)
    })

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> SUBMIT >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


    observeEvent( input$submit , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Exit Rapid Propapagtion")){

          if(input$sent_to == "Pencil Block"){
            DBI::dbWriteTable(conn, "tblExit2Pencil", exit_data(), append=T)
          } else {
            DBI::dbWriteTable(conn, "tblExit2Field", exit_data(), append=T)
          }

          shinyWidgets::show_alert("", "Data saved successfully.", type="success", width = 600)

          shinyjs::hide("table")
          shinyjs::hide("submit")
          shinyjs::hide("clear")
          qr_status(NULL)
          qr_code(NULL)

          exit_data( # set to empty df
            data.frame(
              box_id=character(0),
              exit_date = lubridate::ymd(character(0)),
              number_of_plantlets = integer(0),
              destination = character(0),
              added_by = character(0)
            )
          )

          updateNumericInput(session, "number_of_plantlets", "Number of plantlets", value=0)
          updateSelectInput(session, "destination", "Destination", choices = c("", load_data("SELECT Location FROM tblLocation")$Location))

          shinyjs::hide("status")
          shinyjs::disable("exit_date")
          shinyjs::disable("number_of_plantlets")
          shinyjs::disable("sent_to")
          shinyjs::disable("destination")
          shinyjs::disable("add2list")
        }
      }
    })

    observeEvent( input$submit , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Exit Rapid Propapagtion")){
          Sys.sleep(5)
          dt1 <- load_data(glue::glue("SELECT * FROM tblExit2Pencil")) |>
            dplyr::mutate("Sent_to" = "Pencil Block")

          dt2 <- load_data(glue::glue("SELECT * FROM tblExit2Field"))  |>
            dplyr::mutate("Sent_to" = "Field Block")

          dt <- rbind(dt1, dt2) |>
            dplyr::group_by(destination, box_id) |>
            dplyr::summarise(number_of_plantlets = SUM(number_of_plantlets))
          dt0 <- load_data("SELECT * FROM tblExitRP")
          if(nrow(dt0)>0){
            for (i in 1:nrow(dt)){
              tryCatch({
                update_query( glue::glue("UPDATE tblExitRP SET number_of_plantlets='{dt$number_of_plantlets[i]}', last_update='{Sys.Date()}' WHERE box_id='{dt$source_box_id[i]}';"))
              })
            }
          } else {
            dt$last_update <- Sys.Date()
            DBI::dbWriteTable(conn, "tblExitRP", dt, append=T)
          }
        }
      }
    })


    # ---------------------- EXIT READY

    exit_ready <- reactive({
      if(input$view_exit_ready){
        load_data(glue::glue("SELECT t1.box_id, t1.variety, t3.number_of_plantlets
                              FROM tblBoxIdentity t1
                              LEFT JOIN tblRecutCount t2 ON t1.box_id = t2.box_id
                              LEFT JOIN tblExitRP t3 ON t1.box_id = t3.box_id
                              WHERE t1.location='{location}' AND t2.cut_number >= 4 AND t3.box_id IS NULL")
                  )

      } else {
        data.frame(NULL)
      }
    })

    observeEvent( input$view_exit_ready , {
      if(input$view_exit_ready ){
        shinyjs::show("exit_ready_table")
      } else {
        shinyjs::hide("exit_ready_table")
      }
    })

    output$exit_ready_table  <- DT::renderDT({
      dt <- exit_ready()
      colnames(dt) <- stringr::str_to_title(gsub("_", " ", names(dt)))
      dt
    })


  })
}

## To be copied in the UI
# mod_RP_exit_ui("RP_exit_1")

## To be copied in the server
# mod_RP_exit_server("RP_exit_1")
