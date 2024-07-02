#' pool
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#

# Check if the app is being run locally or on the server
# https://stackoverflow.com/questions/68846492/deploying-shiny-r-app-to-shinyapps-io-gives-sql-connectivity-error-for-the-conne
is_local <- Sys.getenv("SHINY_PORT") == ""

# A function to connect to the SQL database
dbConnector <- function() {
  # Local connect
  if (is_local) {
    DBI::dbConnect(
      odbc::odbc(),
      Driver   = "ODBC Driver 17 for SQL Server",
      Server   = "*****",
      Database = "CassavaCloneTracker",
      UID      = "*****",
      PWD      = "*****",
      port     = 41433
    )
  } else {
    # Remote connect
    DBI::dbConnect(
      odbc::odbc(),
      Driver      = "FreeTDS",
      Server      = "*****",
      Database    = "CassavaCloneTracker",
      UID         = "*****",
      PWD         = "*****",
      Port        = 41433,
      TDS_Version = 7.4
    )
  }
}


conn <- dbConnector()

load_data <- function(query){
  conn <- dbConnector()
  data <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)
  data
}

save_data <- function(table, data){
  tryCatch({
      # Construct the update query by looping over the data fields
      query <- sprintf(
        "INSERT INTO %s (%s) VALUES ('%s');",
        table,
        paste(names(data), collapse = ", "),
        paste(data, collapse = "', '")
      )
      # Submit the update query and disconnect
      DBI::dbGetQuery(conn, query)
      DBI::dbDisconnect(conn)
    },
    error = function(e) {
    }
  )
}


update_query <- function(query){
  conn <- dbConnector()
  DBI::dbExecute(conn, query)
}


delete_query <- function(query){
  conn <- dbConnector()
  DBI::dbExecute(conn, query)
}


# custom shinymanager authentication function to use own database instead of local sqlite db
my_custom_check_creds <- function() {

  # finally one function of user and password
  function(user, password) {
    con <- conn # DBI::dbConnect(odbc::odbc(), Driver = 'ODBC Driver 17 for SQL Server', Server = "*****",
                 #         Database = "CassavaCloneTracker", UID = "*****", PWD = "*****", Port = 41433)

    on.exit(DBI::dbDisconnect(con))

    req <- glue::glue_sql("SELECT * FROM tblUserInformation WHERE \"user\" = ({user}) AND \"password\" = ({password})",
                          user = user, password = password, .con = con
    )

    req <- DBI::dbSendQuery(con, req)
    res <- DBI::dbFetch(req)
    if (nrow(res) > 0) {
      list(result = TRUE, user_info = list(user = user, admin = res$admin, station_adim = res$station_admin, location = res$location))
    } else {
      list(result = FALSE)
    }
  }
}


