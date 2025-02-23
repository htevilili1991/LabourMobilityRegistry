library(shiny)
library(bs4Dash)
library(highcharter)
library(DT)
library(leaflet)
library(sf)
library(dplyr)
library(readxl)
library(jsonlite)
library(tidyr)
library(RSQLite)
library(DBI)
library(shinyjs)

# Define color palette based on NSDP Logo colors
color_palette <- list(
  black = "#000000",
  celadon = "#A9DABA",
  baby_powder = "#F3F4EF",
  rosewood = "#6F1819",
  sea_green = "#108F46",
  jasmine = "#F6D67B",
  earth_yellow = "#D3A766",
  fire_brick = "#B73334"
)

user_credentials <- data.frame(
  username = c("admin"),
  password = c("admin123"),
  stringsAsFactors = FALSE
)

ui <- dashboardPage(
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  title = "Labour Mobility Schemes Registry",
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Labour Mobility",
      image = "logo.png"
    ),
    controlbarIcon = icon("filter"),
    rightUi = tags$li(
      class = "dropdown",
      uiOutput("auth_buttons")
    )
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Data Exploration", tabName = "data_exploration", icon = icon("database")),
      menuItem("Upload Data", tabName = "upload_data", icon = icon("upload"))
    )
  ),
  
  controlbar = dashboardControlbar(
    overlay = TRUE,
    pinned = NULL
  ),
  
  footer = dashboardFooter(
    right = "© 2025 Vanuatu Bureau of Statistics. All rights reserved."
  ),
  
  dashboardBody(
    
    tags$style(HTML(paste0("
    .modal-dialog {
      max-width: 90% !important;
    }
    .nav-sidebar .nav-item > .nav-link.active {
      background-color: ", color_palette$sea_green, " !important;
      color: ", color_palette$baby_powder, " !important;
    }
    .nav-sidebar .nav-item > .nav-link:hover {
      background-color: ", color_palette$celadon, " !important;
      color: ", color_palette$black, " !important;
    }
     .card:hover {
      box-shadow: 0 4px 15px rgba(0, 0, 0, 0.3);
      transition: box-shadow 0.3s ease-in-out;
     }
    /* General dark mode text color */
    .dark-mode body, .dark-mode .content-wrapper, .dark-mode .main-header {
      color: ", color_palette$baby_powder, " !important;
    }
    
    /* Specific for DataTable */
    .dark-mode .dataTables_wrapper .dataTables_length, 
    .dark-mode .dataTables_wrapper .dataTables_filter, 
    .dark-mode .dataTables_wrapper .dataTables_info, 
    .dark-mode .dataTables_wrapper .dataTables_paginate {
      color: ", color_palette$baby_powder, " !important;
    }
    
    /* For table cells */
    .dark-mode .dataTable td, .dark-mode .dataTable th {
      color: ", color_palette$baby_powder, " !important;
    }
    
    .dataTables_scrollBody {
    max-height: 400px;
    overflow-y: auto;
    overflow-x: auto;
    }
  
  .modal-dialog {
    max-width: 400px !important; /* Set the width of the modal */
    margin: auto; /* Center the modal horizontally */
  }
    
  "))),
    useShinyjs(),
    hidden(
      div(
        id = "main_content",
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = "Travel Reason Distribution",
            width = 6,
            collapsible = FALSE,
            maximizable = TRUE,
            highchartOutput("reasonDistributionPlot", height = "400px")
          ),
          box(
            title = "Age Distribution",
            width = 6,
            collapsible = FALSE,
            maximizable = TRUE,
            highchartOutput("ageDistributionPlot", height = "400px")
          )
        )
      ),
      
      # Data Exploration Tab
      tabItem(
        tabName = "data_exploration",
        fluidRow(
          box(
            title = "Data Exploration",
            width = 12,
            collapsible = FALSE,
            maximizable = TRUE,
            DTOutput("dataset"),
            downloadButton("downloadFiltered", "Download Filtered Table", class = "btn-success")
          )
        )
      ),
      
      tabItem(
        tabName = "upload_data",
        fluidRow(
          box(
            collapsible = FALSE,
            title = "Upload New Data",
            width = 6,
            fileInput("file1", "Choose CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            actionButton("upload", "Upload Data", class = "btn-primary"),
            tags$br(),
            tags$strong("Note:"),
            tags$p("Ensure your CSV file has the following columns in this exact order or simply download the template below:"),
            tags$ul(
              tags$li("surname"),
              tags$li("given_name"),
              tags$li("nationality"),
              tags$li("country_of_residence"),
              tags$li("document_type"),
              tags$li("document_no"),
              tags$li("dob"),
              tags$li("age"),
              tags$li("sex"),
              tags$li("travel_date"),
              tags$li("direction"),
              tags$li("accommodation_address"),
              tags$li("note"),
              tags$li("travel_reason"),
              tags$li("border_post"),
              tags$li("destination_coming_from")
            ),
            downloadButton('downloadTemplate', 'Download CSV Template', class = "btn-success")
          ),
          box(
            collapsible = FALSE,
            title = "Upload Status",
            width = 6,
            verbatimTextOutput("uploadMessage")
          )
        )
        
      )
    )
      ))
  )
  
)

server <- function(input, output, session) {
  
  # Track login status
  logged_in <- reactiveVal(FALSE)
  
  # Show login modal when the app is loaded or when the login button is clicked
  observe({
    showModal(
      modalDialog(
        title = "Login",
        textInput("username", "Username"),
        passwordInput("password", "Password"),
        footer = tagList(
          actionButton("submit_login", "Login", class = "btn-primary"),
          modalButton("Cancel")
        ),
        easyClose = FALSE
      )
    )
  })
  
  # Show login modal when the login button is clicked
  observeEvent(input$login, {
    showModal(
      modalDialog(
        title = "Login",
        textInput("username", "Username"),
        passwordInput("password", "Password"),
        footer = tagList(
          actionButton("submit_login", "Login", class = "btn-primary"),
          modalButton("Cancel")
        ),
        easyClose = FALSE
      )
    )
  })
  
  # Handle login action after submitting credentials
  observeEvent(input$submit_login, {
    if (input$username %in% user_credentials$username) {
      valid_password <- user_credentials$password[user_credentials$username == input$username]
      if (input$password == valid_password) {
        logged_in(TRUE)
        removeModal()
        shinyjs::show("main_content")
      } else {
        showNotification("Invalid password. Please try again.", type = "error")
      }
    } else {
      showNotification("Invalid username. Please try again.", type = "error")
    }
  })
  
  # Hide dashboard content if not logged in
  observe({
    if (!logged_in()) {
      shinyjs::hide("main_content")
    }
  })
  
  # Output for login/logout button
  output$auth_buttons <- renderUI({
    if (logged_in()) {
      tagList(
        actionButton("logout", "Logout", class = "btn-danger")
      )
    } else {
      tagList(
        actionButton("login", "Login", class = "btn-primary")
      )
    }
  })
  
  # Handle logout action
  observeEvent(input$logout, {
    logged_in(FALSE)
    shinyjs::hide("main_content")
    showModal(modalDialog(
      title = "Login",
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      footer = tagList(
        actionButton("submit_login", "Login", class = "btn-primary"),
        modalButton("Cancel")
      ),
      easyClose = FALSE
    ))
  })

  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), "data/db.sqlite")
  
  # Create a function to safely close the connection
  onClose <- function() {
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  }
  
  # Close connection when session ends
  session$onSessionEnded(onClose)
  
  # Reactive data fetching
  travel_data <- reactive({
    if (DBI::dbIsValid(con)) {
      dbGetQuery(con, "SELECT * FROM dataset")
    } else {
      stop("Database connection is not valid")
    }
  })
  
  # Render DataTable
  output$dataset <- renderDT({
    datatable(travel_data(), 
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                scrollY = "500px"
              ),
              editable = 'cell'
    )
  })
  
  # Download handler for filtered data
  output$downloadFiltered <- downloadHandler(
    filename = function() {
      paste("filtered_travel_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the indices of the rows currently shown after filtering
      filtered_rows <- input$dataset_rows_all
      if (!is.null(filtered_rows)) {
        # Use these indices to subset the data before writing to CSV
        filtered_data <- travel_data()[filtered_rows, ]
        write.csv(filtered_data, file, row.names = FALSE)
      } else {
        # If no filtering has been applied, download all data
        write.csv(travel_data(), file, row.names = FALSE)
      }
    }
  )
  
  observeEvent(input$dataset_cell_edit, {
    info <- input$dataset_cell_edit
    
    # Get the edited data from the table
    new_value <- info$value
    row_id <- info$row + 1 # Adjust for R's 1-based indexing
    column_name <- colnames(travel_data())[info$col] # Get the column name
    
    # Construct the SQL query to update the database
    query <- sprintf(
      "UPDATE dataset SET %s = '%s' WHERE rowid = %d",
      column_name, new_value, row_id
    )
    
    # Update the database
    tryCatch({
      dbExecute(con, query)
      showNotification("Database updated successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error updating database:", e$message), type = "error")
    })
    
    # Re-fetch data to update reactive
    travel_data <- reactive({
      dbGetQuery(con, "SELECT * FROM dataset")
    })
    
  })
  
  # New data upload functionality
  uploaded_data <- reactiveVal(NULL)
  
  observeEvent(input$upload, {
    req(input$file1)
    inFile <- input$file1
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep, stringsAsFactors = FALSE)
    
    expected_columns_count <- 16 # Based on your expected_columns length
    
    if (ncol(df) != expected_columns_count) {
      output$uploadMessage <- renderText({
        paste("Error: The number of columns in your CSV does not match the expected count of", expected_columns_count, ". Please review your CSV file.")
      })
    } else {
      output$uploadMessage <- renderText({
        "Data uploaded successfully!"
      })
      
      # Here you would typically write the data to your database
      tryCatch({
        dbWriteTable(con, "dataset", df, append = TRUE, row.names = FALSE)
        uploaded_data(df)  # Update reactive value with new data
      }, error = function(e) {
        output$uploadMessage <- renderText({
          paste("Error during upload:", e$message)
        })
      })
    }
  })
  
  # Function to generate an empty CSV template
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste('labour_mobility_template_', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      # Define the expected columns
      expected_columns <- c("surname","given_name","nationality","country_of_residence","document_type","document_no","dob","age","sex","travel_date","direction","accommodation_address","note","travel_reason","border_post","destination_coming_from")
      
      # Create a data frame with a single row of NA values for each column
      df <- data.frame(matrix(ncol = length(expected_columns), nrow = 1))
      colnames(df) <- expected_columns
      df[1,] <- NA  # Fill with NA to make it clear that data should be entered here
      
      # Write to CSV
      write.csv(df, file, row.names = FALSE)
    }
  )
}




shinyApp(ui, server)
