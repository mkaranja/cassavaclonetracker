#' RP_recut UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_RP_recut_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$script(src = "jsQR.js"),
    sidebarLayout(
      sidebarPanel(
        width = 5,

        fluidRow(
           column(12, align="right", shinyWidgets::actionBttn(ns("help"), "", size = "xs", style = "material-circle", color = "default", icon = icon("question"))),
           column(12,
           actionButton(ns("scan_box_identity"), "Source Box Identity (scan barcode)", icon = icon("camera")),
           div(
             tags$video(id = ns("video"), width=300, height=300) |> shinyjs::hidden(),
             tags$canvas(id = ns("canvas"), width=300, height=300) |> shinyjs::hidden()
           ),
           textOutput(ns("source_box_id"))
        )

        ),

        shinyjs::hidden(
          div( id = ns("status"),
               bslib::card(class="border border-danger bg-danger bg-gradient text-white rounded-end",
                           bslib::card_body(textOutput(ns("show_status")))
               )
          )), br(),

        shinyjs::hidden(textInput(ns("variety"),"Variety", value = "")),
        dateInput(ns("recut_date"), "Date of recutting", value = Sys.Date(), max = Sys.Date()),
        shinyjs::disabled(numericInput(ns("number_of_cuttings"), "Number of cuttings", min = 1, value = 0)),

        fluidRow(
          column(6, shinyjs::disabled(actionButton(ns("add1"), "Add to New Box", icon = icon("check")))),
          column(6, align="right", shinyjs::hidden(actionButton(ns("clear"), "Clear", icon = icon("eraser")))),
        )
      ),

      mainPanel(
        width = 7,
        fluidRow(
          shinyWidgets::prettySwitch(ns("view_recut_ready"), "View plantlets waiting re-cut", status = "primary", fill = T),
          shinyjs::hidden( DT::DTOutput(ns("recut_ready_table2"))),
          shinyjs::hidden( DT::DTOutput(ns("table2"))),br()
          ),
        fluidRow(
          column(6, br(),
                 shinyjs::hidden(actionButton(ns("assign_new_box"), "Assign Cuttings to New Box (scan barcode)", icon = icon("camera"))),
                 shinyjs::hidden(shinyjs::disabled(textInput(ns("box_id"), "New Box Id", placeholder = "New Box ID", value = ""))), br(),
                 shinyjs::hidden(actionButton(ns("submit"), "Submit", icon = icon("paper-plane"), class = "pull-right btn btn-primary")),

                shinyjs::hidden(
                  div( id = ns("status2"),
                       bslib::card(class="border border-danger bg-danger bg-gradient text-white rounded-end",
                                   bslib::card_body(textOutput(ns("show_status2")))
                       )
                  )),
                div(
                   tags$video(id = ns("video2"), width=300, height=300) |> shinyjs::hidden(),
                   tags$canvas(id = ns("canvas2"), width=300, height=300) |> shinyjs::hidden()
                )
          )
        ), br(),br(),br()
      )
    )
  )
}

#' RP_recut Server Functions
#'
#' @noRd
mod_RP_recut_server <- function(id, r, location, user ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    source_qr <- reactiveVal(NULL)
    qr_status <- reactiveVal(NULL)
    assign_qr <- reactiveVal(NULL)
    qr_status2 <- reactiveVal(NULL)

    recut_data <- reactiveVal(
      data.frame(
        source_box_id = character(0),
        recut_date = lubridate::ymd(character(0)),
        number_of_cuttings = integer(0)
      )
    )

    observeEvent( input$help , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          showModal(modalDialog(
            shiny::includeMarkdown("helpfiles/receive_cuttings.md"),
            easyClose = TRUE,
            size = "l"
          ))
        }
      }
    })

    observe({
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          if(isTRUE(source_qr() =="")){
            shinyjs::hide("status")
          }
        }
      }
    })

    ## 1. >>>>>>>>>>>>>>>>>>>>>>>>>>>>> SCAN >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    observeEvent( input$scan_box_identity , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          if(isFALSE(is.null(location))){
            source_qr(NULL)
            shinyjs::hide("status")
            shinyjs::show("canvas")
            shinyjs::hide("video")

            shinyWidgets::updatePrettySwitch(session, "view_recut_ready", label = "View plantlets waiting re-cut", value = FALSE)

            shinyjs::runjs("
                 navigator.mediaDevices.getUserMedia({ video: { facingMode: 'environment' } }).then(function(stream) {
                    var video = document.getElementById('Recut-video');
                    video.srcObject = stream;
                    video.play();
                    var canvas = document.getElementById('Recut-canvas');
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
                        Shiny.setInputValue('Recut-qrResult', code.data);
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
        source_qr(input$qrResult)
      }
    })



    ## 1b. >>>>>>>>>>>>>>>>>>>>>>>>>> CHECK SCANNED QR CODE


    observeEvent(source_qr(),{
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          if(isFALSE(is.null(location))){
            if(isTRUE(length(source_qr())>0)){

              location
              input$add1
              input$submit
              input$clear

              shinyjs::runjs("
                var video = document.getElementById('Recut-video');
                var stream = video.srcObject;
                if (stream) {
                  var tracks = stream.getTracks();
                  tracks.forEach(function(track) {
                    track.stop();
                  });
                  video.srcObject = null;
                }
              ")

              shinyjs::show("source_box_id")

              output$source_box_id <- renderText({
                if(!is.null(source_qr())){
                  paste0("[",source_qr(),"]")
                }
              })

              box_id <- source_qr()

              dt <- load_data(glue::glue(
                "SELECT tblBoxIdentity.box_id, tblBoxIdentity.variety, tblAssignedBoxIdentity.date, tblIntroduction.receiving_date, tblRecutCount.cut_number, tblExitRP.number_of_plantlets
                FROM tblBoxIdentity
                LEFT JOIN tblAssignedBoxIdentity
                ON tblBoxIdentity.box_id = tblAssignedBoxIdentity.box_id
                LEFT JOIN tblIntroduction
                ON tblBoxIdentity.box_id = tblIntroduction.box_id
                LEFT JOIN tblRecutCount
                ON tblBoxIdentity.box_id = tblRecutCount.box_id
                LEFT JOIN tblExitRP
                ON tblBoxIdentity.box_id = tblExitRP.box_id
                WHERE tblBoxIdentity.location='{location}' AND tblBoxIdentity.box_id='{box_id}';"))

              status <- ifelse(nrow(dt) == 0, "Invalid barcode. Please scan another barcode.",
                               ifelse(nrow(dt)>0 & is.na(dt$date), "Not assigned. Please scan another barcode.",
                                      ifelse(nrow(dt)>0 & !is.na(dt$date) & !is.na(dt$receiving_date) & dt$cut_number==0, "Proceed",
                                             ifelse(nrow(dt)>0 & !is.na(dt$date) & !is.na(dt$receiving_date) & dt$cut_number>0 & dt$cut_number<4, "Proceed",
                                                    ifelse(nrow(dt)>0 & !is.na(dt$date) & isFALSE(grepl(tolower(input$variety), tolower(box_id))), "Add cuttings of the same variety.",
                                                           ifelse(nrow(dt)>0 & !is.na(dt$date) & !is.na(dt$receiving_date) & dt$cut_number>0 & dt$cut_number>=4 & is.na(dt$number_of_plantlets), "Recutting process is complete. Plants are waiting Exit from rapid propagation",
                                                                  ifelse(nrow(dt)>0 & !is.na(dt$date) & !is.na(dt$receiving_date) & dt$cut_number>0 & dt$cut_number>=4 & !is.na(dt$number_of_plantlets), "Completed rapid propagation phase.",
                                                                         "")))))))
              qr_status(status)

              if(status == "Proceed"){
                qr_status(NULL)
                shinyjs::hide("status")
                shinyjs::enable("number_of_cuttings")
                updateTextInput(session, "variety","Variety", value = dt$variety)
                shinyjs::hide("status")
                shinyjs::enable("add1")

              } else {
                shinyjs::disable("add1")
                shinyjs::disable("number_of_cuttings")
                shinyjs::show("status")

                output$source_box_id <- renderText({NULL})
              }

              shinyjs::hide("canvas")

            }
          }
        }
      }
    })


    ## >>>>>>>>>>> SHOW STATUS-ERROR OF QR SCANNED


    output$show_status <- renderText({
        qr_status()
    })

    new_recut <- reactive({
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          data.table::data.table(
            source_box_id = ifelse(is.null(source_qr()), "", source_qr()),
            recut_date = input$recut_date,
            number_of_cuttings = input$number_of_cuttings
          )
        }
      }
    })


    ## >>>>>>>>>>>>>>>>>>> ADD RECORD TO NEW BOX

    observeEvent(input$add1, {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          if(isFALSE(length(source_qr()) > 0)){
            shinyWidgets::show_alert("", text = "Source Box ID is required", type = "warning", width = 600)
          } else  if(isTRUE(input$number_of_cuttings == 0)){
            shinyWidgets::show_alert("", text = "Enter number of cuttings", type = "warning", width = 600)
          } else if(is.na(input$number_of_cuttings)){
            shinyWidgets::show_alert("", text = "Number of cuttings must be numeric", type = "warning", width = 600)
          } else {
            shinyjs::show("table2")

            if(nrow(recut_data())>0){
              current_variety <- load_data(glue::glue("SELECT variety FROM tblIntroduction WHERE box_id='{source_qr()}';"))$variety
              existing_variety <- load_data(glue::glue("SELECT variety FROM tblIntroduction WHERE box_id='{recut_data()$source_box_id[1]}';"))$variety

              if(!current_variety == existing_variety){
                shinyWidgets::show_alert("", text = "Source Box ID must be selected from the same variety", type = "warning", width = 600)
              } else {
                recut_data(rbind(recut_data(), new_recut()))
                source_qr(NULL)
                updateNumericInput(session, "number_of_cuttings", "Number of cuttings", min = 1, value = 0)
              }

            } else {
              recut_data(rbind(recut_data(), new_recut()))
              source_qr(NULL)
              # dt <- load_data(glue::glue("SELECT box_id FROM tblIntroduction WHERE location='{location}';"))
              updateNumericInput(session, "number_of_cuttings", "Number of cuttings", min = 1, value = 0)
            }

            shinyjs::show("assign_new_box")
            shinyjs::show("clear")
            shinyjs::hide("canvas")
            shinyjs::hide("source_box_id")
          }
        }
      }
    })


    output$table2 <- DT::renderDT({
      input$add1
      input$clear
      dt <- recut_data()
      colnames(dt) <- stringr::str_to_title(gsub("_", " ", names(dt)))
      dt
    })

    observeEvent( input$assign_new_box , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          if(isFALSE(is.null(location))){
            if(nrow(recut_data()>0)){
              input$assign_new_box
              assign_qr(NULL)
              shinyjs::hide("status2")
              shinyjs::show("canvas2")

              shinyjs::runjs("
                 navigator.mediaDevices.getUserMedia({ video: { facingMode: 'environment' } }).then(function(stream) {
                    var video = document.getElementById('Recut-video2');
                    video.srcObject = stream;
                    video.play();
                    var canvas = document.getElementById('Recut-canvas2');
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
                        Shiny.setInputValue('Recut-qrResult2', code.data);
                      }
                    }, 500); // Increased interval for better performance
                 }).catch(function(err) {
                    console.error('Error accessing camera: ', err);
                 });
                ")
            }
          }
        }
      }
    })


    observeEvent( input$qrResult2 , {
      if(!is.null(input$qrResult2)){
        assign_qr(input$qrResult2)
      }
    })


    observeEvent( assign_qr() , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          if(isFALSE(is.null(location))){
            if(isTRUE(length(assign_qr())>0)){

              shinyjs::runjs("
                        var video = document.getElementById('Recut-video2');
                        var stream = video.srcObject;
                        if (stream) {
                          var tracks = stream.getTracks();
                          tracks.forEach(function(track) {
                            track.stop();
                          });
                          video.srcObject = null;
                        }
                      ")

              box_id <- assign_qr()

              dt <- load_data(glue::glue("SELECT tblBoxIdentity.box_id, tblAssignedBoxIdentity.date
                                      FROM tblBoxIdentity
                                      LEFT JOIN tblAssignedBoxIdentity
                                      ON tblBoxIdentity.box_id = tblAssignedBoxIdentity.box_id
                                      WHERE tblBoxIdentity.location='{location}' AND tblBoxIdentity.box_id='{box_id}';"))
              status <- ifelse(nrow(dt)==0, "Invalid barcode. Please scan another barcode.",
                               ifelse(nrow(dt)>0 & !is.na(dt$date), "Barcode already assigned Please select another barcode",
                                      ifelse(nrow(dt)>0 & is.na(dt$date) & isTRUE(grepl(tolower(input$variety), tolower(box_id))), "Proceed",
                                             ifelse(nrow(dt)>0 & is.na(dt$date) & isFALSE(grepl(tolower(input$variety), tolower(box_id))), "Only scan barcodes of the selected variety.",
                                                    "Invalid barcode. Please scan another barcode."))))

              if(nrow(recut_data())>0){
                if(box_id %in% recut_data()$source_box_id){
                  status <- "Barcode already assigned Please select another barcode."
                }
              }

              if(status == "Proceed"){
                shinyjs::hide("status2")
                shinyjs::show("submit")
                qr_status2(NULL)
                updateTextInput(session, "box_id", "", placeholder = "New Box ID", value = box_id)
                shinyjs::show("box_id")
              } else {
                shinyjs::hide("box_id")
                shinyjs::hide("submit")
                shinyjs::show("status2")
                qr_status2(status)
              }
              shinyjs::hide("canvas2")

            }
          }
        }
      }
    })


    output$var <- renderText({
      input$variety
    })

    output$show_status2 <- renderText({
      qr_status2()
    })


    # # ------------------------------ SAVE/ SUBMIT ----------------------------------------------------------------

    new_record <- reactive({
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          if(nrow(recut_data())>0){
            if(length(assign_qr())>0){
              dt <- recut_data() |>
                dplyr::left_join(load_data("SELECT box_id, cut_number FROM tblRecutCount"), dplyr::join_by(source_box_id==box_id))
              dt$cut_number <- dt$cut_number+1
              dt$box_id <- assign_qr()
              dt$added_by <- user
              dt
            }
          }
        }
      }
    })



    new_intro <- reactive({
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          dt <- new_record() |>
            dplyr::left_join(
              load_data("SELECT box_id, original_source, variety FROM tblIntroduction;"),
              dplyr::join_by(source_box_id==box_id)
            )

          df <- dt |> # data.frame(box_id = input$box_id,number_of_cuttings = dt$number_of_cuttings)
            dplyr::group_by(box_id) |>
            dplyr::summarise(number_of_cuttings = sum(number_of_cuttings))

          intro <- data.frame(
            box_id = df$box_id, # grouped new Id
            variety = unique(dt$variety),
            original_source = unique(dt$original_source),
            receiving_date = unique(dt$recut_date),
            number_of_thick_mini_cuttings = df$number_of_cuttings, # derived from sum
            location = location,
            added_by = user,
            source_box_id = stringr::str_c(dt$source_box_id, collapse = ",")
          )
          intro
        }
      }
    })

    complete_recut <- reactiveVal(NULL) # save complete recut


    observeEvent( input$submit , {

      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          if(nrow(new_record())>0){

            # Sys.sleep(2)

            shinyWidgets::progressSweetAlert(session, id="submitting", title = "Please wait ...", status = "primary", display_pct=T, value=0)

            # shinyjs::disable("submit")
            shinyWidgets::updateProgressBar(session = session, id = "submitting",value = 0) # 0

            DBI::dbWriteTable(conn,"tblRecut", new_record(), append = T)
            recut_count <- data.frame( box_id = unique(new_record()$box_id), cut_number = 0, last_recut = Sys.Date()) # add new assigned Ids to recut count table with count 0

            DBI::dbWriteTable(conn, "tblRecutCount", recut_count, append = T)
            shinyWidgets::updateProgressBar(session = session, id = "submitting",value = 25) # 1

            # Save New BoxID in tblAssignedBoxIdentity
            df <- data.frame( box_id = unique(new_record()$box_id) )
            DBI::dbWriteTable(conn, "tblAssignedBoxIdentity", df, append=T)
            shinyWidgets::updateProgressBar(session = session, id = "submitting",value = 50) # 2

            # Save Recuts into Introduction
            DBI::dbWriteTable(conn, "tblIntroduction", new_intro(), append = T)
            end <- Sys.time()
            # DBI::dbWriteTable(conn = conn, "tblIntroduction", intro_data(), append=T)
            shinyWidgets::updateProgressBar(session = session, id = "submitting",value = 75) # 3

            # clear temp data
            recut_data(
              data.frame(
                box_id = character(0),
                recut_date = character(0),
                number_of_cuttings = character(0)
              )
            )

            # Reset
            output$source_box_id <- renderText({NULL })

            updateTextInput(session, "box_id", "", placeholder = "New Box ID", value = "")
            shinyjs::hide("canvas2")

            shinyjs::hide("table2")
            shinyjs::hide("clear")
            shinyjs::hide("submit")
            shinyjs::hide("assign_new_box")
            shinyjs::hide("box_id")

            shinyWidgets::updateProgressBar(session = session, id = "submitting",value = 100) # 4
            shinyWidgets::closeSweetAlert(session = session)
            shinyWidgets::show_alert("", "Data saved successfully.", type="success", width = 600)
          }
        }
      }

    })



     observeEvent( input$submit , {
       if(isTRUE(r$nav == "rapid")){
         if(isTRUE(r$rapid_tabs == "Re-Cutting")){
           Sys.sleep(5)
           recut_count <- load_data("SELECT source_box_id, COUNT(*) AS cut_number FROM tblRecut GROUP BY source_box_id;")

           for (i in 1:nrow(recut_count)){
             tryCatch({
               update_query( glue::glue("UPDATE tblRecutCount SET cut_number='{recut_count$cut_number[i]}' WHERE box_id='{recut_count$source_box_id[i]}';"))
             })
           }
         }
       }
    })

    # ---------------------------- CLEAR ----------------------

    observeEvent( input$clear , {
      if(isTRUE(r$nav == "rapid")){
        if(isTRUE(r$rapid_tabs == "Re-Cutting")){
          # clear temp data
          recut_data(
            data.frame(
              box_id = character(0),
              recut_date = character(0),
              number_of_cuttings = character(0)
            )
          )

          # Reset
          updateTextInput(session, "box_id", "", placeholder = "New Box ID", value = "")

          shinyjs::hide("table2")
          shinyjs::hide("clear")
          shinyjs::hide("submit")
          shinyjs::hide("assign_new_box")
          shinyjs::hide("box_id")
        }
      }
    })


    # ------------------------------WAITING RECUT -----------------------

     recut_ready <- reactive({
       if(input$view_recut_ready){
         dt <- load_data(glue::glue("SELECT tblIntroduction.box_id, tblRecutCount.cut_number, tblRecutCount.last_recut
                      FROM tblIntroduction
                      LEFT JOIN tblRecutCount
                      ON tblIntroduction.box_id = tblRecutCount.box_id
                      WHERE tblIntroduction.location='{location}' AND tblRecutCount.cut_number < 4;")
          )
         colnames(dt) <- stringr::str_to_title(gsub("_", " ", c("box_id", "current_cut_number", "last_recut_date")))
       } else {
         dt <- data.frame(NULL)
       }
       dt
     })

     observeEvent( input$view_recut_ready , {
       if(input$view_recut_ready ){
         shinyjs::show("recut_ready_table2")
       } else {
         shinyjs::hide("recut_ready_table2")
       }
     })

     output$recut_ready_table2  <- DT::renderDT({
       recut_ready()
     })





  })
}

## To be copied in the UI
# mod_RP_recut_ui("Recut")

## To be copied in the server
# mod_RP_recut_server("Recut")
