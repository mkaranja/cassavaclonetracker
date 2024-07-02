#' MANAGE UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MANAGE_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(10, offset = 1,
           fluidRow(
             bslib::card(
               bslib::card_header(h5(textOutput(ns("tab1")))),
               fluidRow(
                 DT::DTOutput(ns("table"))
               )
             ),
             column(12,
                    fluidRow(
                      column(2, textInput(ns("new_id"), label = textOutput(ns("new_id1")), value = "", width = "100%")),
                      column(4, textInput(ns("new_desc"), label = "Description", value = "", width = "100%")),
                      column(1, br(), actionButton(ns("Add"), label = "Add New"))
                    ),
                    fluidRow(
                      column(2, shinyjs::disabled(textInput(ns("update_id"), label = "", value = "", width = "100%"))),
                      column(4, textInput(ns("update_desc"), "", value = "", width = "100%")),
                      column(1, br(), shinyjs::disabled(actionButton(ns("Update"), "Update"))),
                      column(1, br(), shinyjs::disabled(actionButton(ns("Delete"), "Delete")))
                    )
             )
           )
        )
  )
}

#' MANAGE Server Functions
#'
#' @noRd
mod_MANAGE_server <- function(id, r, user, Tab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$col <- renderText({
      r$nav
    })

    output$tab1 <- renderText({ toupper(Tab) })

    data_input <- reactive({
      if(isTRUE(r$nav == Tab)){
        input$Update
        dt <- load_data(glue::glue("SELECT * FROM tbl{Tab};"))
        colnames(dt) <- stringr::str_to_title(names(dt))
        dt
      }

    })

    output$table <- DT::renderDT({
      input$Add
      input$Update
      input$Delete

      dt <- load_data(glue::glue("SELECT * FROM tbl{Tab};")) |>
        dplyr::select(dplyr::everything(), date)
      colnames(dt) <- gsub("_", " ", stringr::str_to_title(names(dt)))

      DT::datatable(
        dt,
        rownames = FALSE,
        escape = FALSE,
        selection = "single",
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


    output$new_id1 <- renderText({ Tab})


    # ADD NEW RECORD -------------------------------------------------------------------------------------

    new_record <- reactive({
      if(isTRUE(r$nav == Tab)){
        dt <- data.frame(
          id = toupper(input$new_id),
          description = ifelse(input$new_desc == "", "", input$new_desc),
          added_by = user,
          date = Sys.Date()
        )
        colnames(dt)[1] <- tolower(Tab)
        dt
      }
    })


    observeEvent(input$Add, {
      if(isTRUE(r$nav == Tab )){
        if(input$new_id == ""){
          shinyWidgets::show_alert("", paste("Enter value for ", names(data_input())[1] ))
        } else if(tolower(new_record()[[1]] )%in% tolower(load_data(glue::glue("SELECT * FROM tbl{Tab};"))[[1]])){
          shinyWidgets::show_alert(title = "", text = paste(Tab, "Exists"), type = "error")
        } else {
          tryCatch({
            DBI::dbWriteTable(conn, glue::glue("tbl{Tab}"), new_record(), append = T )
            shinyWidgets::show_alert(title = "", text = paste(Tab, "Added"), type = "success")
            updateTextInput(session, "new_id", label = Tab, value = "")
            updateTextInput(session, "new_desc", label = "Description", value = "")
          })
        }
      }
    })



    # UPDATE RECORD ------------------------------------------------------------------------------------

    observeEvent(input$table_row_last_clicked,{
      if(isTRUE(r$nav == Tab )){

        row <- input$table_row_last_clicked
        df <- data_input()

        shinyjs::enable("Update")
        shinyjs::enable("Delete")

        updateTextInput(session, "update_id", label = "", value = df[row, 1])
        updateTextInput(session, "update_desc", label = "", value = df[row, 2])
      }
    })

    observeEvent(input$Update,{
      if(isTRUE(r$nav == Tab )){
        if(input$update_id == ""){
          shinyWidgets::show_alert(title = "", text = paste("Please select a value of ", Tab, "from the table"), type = "warning")
        } else if (input$update_desc == ""){
          shinyWidgets::show_alert(title = "", text = paste("Please enter the value of ", Tab, "Description"), type = "warning")
        } else {
          Id = colnames(data_input())[1]

          tryCatch({
            query <- glue::glue("UPDATE tbl{Tab} SET description='{input$update_desc}' WHERE {Id}='{input$update_id}';")
            update_query(query)

            shinyWidgets::show_alert(title = "", text = paste(Tab, "Updated"), type = "success")

            updateTextInput(session, "update_id", label = Tab, value = "")
            updateTextInput(session, "update_desc", label = "", value = "")
          })
        }
      }
    })

    # DELETE RECORD -------------------------------------------------------------------------------------


    observeEvent( input$Delete , {
      if(isTRUE(r$nav == Tab )){
        r <- input$table_row_last_clicked
        colName <- colnames(data_input())[1]
        Val <- data_input()[r,1]
        tryCatch({
          query <- glue::glue("DELETE FROM tbl{Tab} WHERE {colName}='{Val}';")
          delete_query(query)
          shinyWidgets::show_alert(title = "", text = paste(Tab, "Deleted"), type = "success")
        })
      }
    })

  })
}

## To be copied in the UI
# mod_MANAGE_ui("MANAGE_1")

## To be copied in the server
# mod_MANAGE_server("MANAGE_1")
