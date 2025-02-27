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

# User credentials with roles
user_credentials <- data.frame(
  username = c("admin", "viewer"),
  password = c("admin123", "view123"),
  role = c("admin", "viewer"),
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
    controlbarIcon = icon("filter"),  # Re-add controlbar icon for toggling
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
      uiOutput("upload_menu")  # Dynamically render Upload Data menu item
    )
  ),
  
  footer = dashboardFooter(
    right = "© 2025 Vanuatu Bureau of Statistics. All rights reserved."
  ),
  
  dashboardBody(
    tags$style(HTML(paste0("
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
    
    /* Hide edit buttons by default */
    #submitChanges, #cancelChanges {
      display: none;
    }
    /* Initially hide controlbar */
    .controlbar {
      display: none;
    }
    "))),
    useShinyjs(),
    hidden(
      div(
        id = "main_content",
        tabItems(
          # Overview Tab with Controlbar
          tabItem(
            tabName = "overview",
            dashboardControlbar(
              id = "overview_controlbar",
              overlay = TRUE,
              pinned = NULL,
              selectInput(
                "year_filter",
                "Filter by Year",
                choices = NULL,  # Will be populated dynamically in the server
                selected = NULL,
                multiple = FALSE
              ),
              selectInput(
                "direction_filter",
                "Filter by Direction",
                choices = NULL,  # Will be populated dynamically in the server
                selected = NULL,
                multiple = FALSE
              ),
              selectInput(
                "sex_filter",
                "Filter by Sex",
                choices = NULL,  # Will be populated dynamically in the server
                selected = NULL,
                multiple = FALSE
              ),
              actionButton(
                "reset_filters",
                "Reset Filters",
                class = "btn btn-warning",
                style = "margin-top: 10px; width: 100%;"
              )
            ),
            fluidRow(
              column(
                width = 4,
                valueBox(
                  value = textOutput("totalTravelers"),
                  subtitle = "Total Travelers",
                  icon = icon("users"),
                  color = "success",
                  elevation = 2,
                  width = 12
                )
              ),
              column(
                width = 4,
                valueBox(
                  value = textOutput("avgAge"),
                  subtitle = "Average Age",
                  icon = icon("calendar"),
                  color = "info",
                  elevation = 2,
                  width = 12
                )
              ),
              column(
                width = 4,
                valueBox(
                  value = textOutput("activePrograms"),
                  subtitle = "Active Programs",
                  icon = icon("briefcase"),
                  color = "warning",
                  elevation = 2,
                  width = 12
                )
              )
            ),
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
          
          # Data Exploration Tab (no controlbar)
          tabItem(
            tabName = "data_exploration",
            fluidRow(
              box(
                title = "Data Exploration",
                width = 12,
                collapsible = FALSE,
                maximizable = TRUE,
                DTOutput("dataset"),
                downloadButton("downloadFiltered", "Download Filtered Table", class = "btn-success"),
                uiOutput("update_button"),  # Dynamically render Update button
                actionButton("submitChanges", "Submit", class = "btn-primary"),
                actionButton("cancelChanges", "Cancel", class = "btn-secondary")
              )
            )
          ),
          
          # Upload Data Tab (no controlbar)
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
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Track login status and user role
  logged_in <- reactiveVal(FALSE)
  user_role <- reactiveVal(NULL)
  
  # Show login modal when the app is loaded
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
        easyClose = FALSE,
        size = "s"
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
        easyClose = FALSE,
        size = "s"
      )
    )
  })
  
  # Handle login action after submitting credentials
  observeEvent(input$submit_login, {
    if (input$username %in% user_credentials$username) {
      valid_password <- user_credentials$password[user_credentials$username == input$username]
      if (input$password == valid_password) {
        logged_in(TRUE)
        user_role(user_credentials$role[user_credentials$username == input$username])
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
    user_role(NULL)
    shinyjs::hide("main_content")
    showModal(modalDialog(
      title = "Login",
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      footer = tagList(
        actionButton("submit_login", "Login", class = "btn-primary"),
        modalButton("Cancel")
      ),
      easyClose = FALSE,
      size = "s"
    ))
  })
  
  # Conditionally render Upload Data menu item based on role
  output$upload_menu <- renderUI({
    if (logged_in() && user_role() == "admin") {
      menuItem("Upload Data", tabName = "upload_data", icon = icon("upload"))
    }
  })
  
  # Conditionally render Update button based on role
  output$update_button <- renderUI({
    if (logged_in() && user_role() == "admin") {
      actionButton("updateData", "Update", class = "btn-primary")
    }
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
  
  # Reactive data fetching with year extraction from text-type travel_date
  raw_data <- reactiveVal()
  observe({
    if (DBI::dbIsValid(con)) {
      data <- dbGetQuery(con, "SELECT rowid, * FROM dataset")
      # Extract year from travel_date (text type, format "MM/DD/YYYY")
      if (!is.null(data$travel_date)) {
        data$year <- tryCatch({
          as.numeric(format(as.Date(data$travel_date, format = "%m/%d/%Y"), "%Y"))
        }, error = function(e) {
          warning("Error parsing travel_date: ", e$message)
          rep(NA, length(data$travel_date))  # Return NA for all rows if parsing fails
        })
      } else {
        data$year <- NA
      }
      raw_data(data)
    } else {
      stop("Database connection is not valid")
    }
  })
  
  # Show/hide controlbar based on active tab
  observe({
    if (input$sidebar == "overview") {
      shinyjs::show("overview_controlbar")
    } else {
      shinyjs::hide("overview_controlbar")
    }
  })
  
  # Populate year filter choices (Overview tab only)
  observe({
    data <- raw_data()
    if (!is.null(data)) {
      years <- sort(unique(data$year[!is.na(data$year)]), na.last = TRUE)  # Exclude NA years
      updateSelectInput(session, "year_filter",
                        choices = c("All", years),
                        selected = "All")
    }
  })
  
  # Populate direction filter choices (Overview tab only)
  observe({
    data <- raw_data()
    if (!is.null(data)) {
      directions <- sort(unique(data$direction[!is.na(data$direction)]), na.last = TRUE)  # Exclude NA directions
      updateSelectInput(session, "direction_filter",
                        choices = c("All", directions),
                        selected = "All")
    }
  })
  
  # Populate sex filter choices (Overview tab only)
  observe({
    data <- raw_data()
    if (!is.null(data)) {
      sexes <- sort(unique(data$sex[!is.na(data$sex)]), na.last = TRUE)  # Exclude NA sexes
      updateSelectInput(session, "sex_filter",
                        choices = c("All", sexes),
                        selected = "All")
    }
  })
  
  # Filtered data for Overview tab only
  filtered_data_overview <- reactive({
    data <- raw_data()
    if (is.null(data) || is.null(input$year_filter) || is.null(input$direction_filter) || is.null(input$sex_filter)) return(data)
    
    filtered <- data
    if (input$year_filter != "All") {
      filtered <- filtered %>% filter(year == as.numeric(input$year_filter))
    }
    if (input$direction_filter != "All") {
      filtered <- filtered %>% filter(direction == input$direction_filter)
    }
    if (input$sex_filter != "All") {
      filtered <- filtered %>% filter(sex == input$sex_filter)
    }
    filtered
  })
  
  # Reset filters when the reset button is clicked (Overview tab only)
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "year_filter", selected = "All")
    updateSelectInput(session, "direction_filter", selected = "All")
    updateSelectInput(session, "sex_filter", selected = "All")
  })
  
  # Reactive value to track edit mode (Data Exploration tab)
  edit_mode <- reactiveVal(FALSE)
  
  # Render DataTable with unfiltered data (Data Exploration tab)
  output$dataset <- renderDT({
    datatable(
      raw_data(),  # Use unfiltered raw_data for Data Exploration
      editable = edit_mode(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "500px"
      )
    )
  })
  
  # Download handler for unfiltered data (Data Exploration tab)
  output$downloadFiltered <- downloadHandler(
    filename = function() {
      paste("filtered_travel_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      filtered_rows <- input$dataset_rows_all
      if (!is.null(filtered_rows)) {
        filtered_data <- raw_data()[filtered_rows, ]  # Use unfiltered raw_data
        write.csv(filtered_data, file, row.names = FALSE)
      } else {
        write.csv(raw_data(), file, row.names = FALSE)
      }
    }
  )
  
  # Reactive value to store edited data (Data Exploration tab)
  edited_data <- reactiveVal()
  
  # Update button functionality: toggle edit mode and show buttons (only for admin)
  observeEvent(input$updateData, {
    if (user_role() == "admin") {
      edit_mode(TRUE)  # Enable editing
      shinyjs::show("submitChanges")
      shinyjs::show("cancelChanges")
      shinyjs::disable("updateData")  # Disable Update button while editing
      shinyjs::disable("downloadFiltered")  # Disable Download while editing
    }
  })
  
  # Capture edited data from the main table (Data Exploration tab)
  observeEvent(input$dataset_cell_edit, {
    if (user_role() == "admin") {
      info <- input$dataset_cell_edit
      temp_data <- raw_data()  # Use unfiltered raw_data
      temp_data[info$row, info$col] <- info$value
      edited_data(temp_data)
    }
  })
  
  # Submit changes to database (Data Exploration tab)
  observeEvent(input$submitChanges, {
    if (user_role() == "admin" && !is.null(edited_data())) {
      updated_data <- edited_data()
      
      tryCatch({
        dbWriteTable(con, "dataset", updated_data[, -1], overwrite = TRUE)  # Exclude rowid
        raw_data(updated_data)  # Update raw data
        showNotification("Database updated successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error updating database:", e$message), type = "error")
      })
    } else {
      showNotification("No changes detected or insufficient permissions.", type = "warning")
    }
    edit_mode(FALSE)  # Disable editing
    shinyjs::hide("submitChanges")
    shinyjs::hide("cancelChanges")
    shinyjs::enable("updateData")  # Re-enable Update button
    shinyjs::enable("downloadFiltered")  # Re-enable Download button
  })
  
  # Cancel changes (Data Exploration tab)
  observeEvent(input$cancelChanges, {
    if (user_role() == "admin") {
      edited_data(NULL)  # Reset edited data
      edit_mode(FALSE)  # Disable editing
      shinyjs::hide("submitChanges")
      shinyjs::hide("cancelChanges")
      shinyjs::enable("updateData")  # Re-enable Update button
      shinyjs::enable("downloadFiltered")  # Re-enable Download button
      raw_data(dbGetQuery(con, "SELECT rowid, * FROM dataset"))  # Refresh from database
    }
  })
  
  # New data upload functionality (only for admin, Upload Data tab)
  uploaded_data <- reactiveVal(NULL)
  
  observeEvent(input$upload, {
    if (user_role() == "admin") {
      req(input$file1)
      inFile <- input$file1
      df <- read.csv(inFile$datapath, header = input$header, sep = input$sep, stringsAsFactors = FALSE)
      
      expected_columns_count <- 16
      if (ncol(df) != expected_columns_count) {
        output$uploadMessage <- renderText({
          paste("Error: The number of columns in your CSV does not match the expected count of", expected_columns_count, ". Please review your CSV file.")
        })
      } else {
        output$uploadMessage <- renderText({
          "Data uploaded successfully!"
        })
        tryCatch({
          dbWriteTable(con, "dataset", df, append = TRUE, row.names = FALSE)
          uploaded_data(df)
          raw_data(dbGetQuery(con, "SELECT rowid, * FROM dataset"))  # Refresh data
        }, error = function(e) {
          output$uploadMessage <- renderText({
            paste("Error during upload:", e$message)
          })
        })
      }
    }
  })
  
  # Function to generate an empty CSV template (Upload Data tab)
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste('labour_mobility_template_', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      expected_columns <- c("surname","given_name","nationality","country_of_residence","document_type","document_no","dob","age","sex","travel_date","direction","accommodation_address","note","travel_reason","border_post","destination_coming_from")
      df <- data.frame(matrix(ncol = length(expected_columns), nrow = 1))
      colnames(df) <- expected_columns
      df[1,] <- NA
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Overview tab indicators and charts with filtered data
  output$totalTravelers <- renderText({
    data <- filtered_data_overview()  # Use filtered data for Overview
    format(nrow(data), big.mark = ",")
  })
  
  output$avgAge <- renderText({
    data <- filtered_data_overview()  # Use filtered data for Overview
    paste(round(mean(as.numeric(data$age), na.rm = TRUE), 1), "years")
  })
  
  output$activePrograms <- renderText({
    data <- filtered_data_overview()  # Use filtered data for Overview
    length(unique(data$travel_reason))
  })
  
  output$reasonDistributionPlot <- renderHighchart({
    data <- filtered_data_overview()  # Use filtered data for Overview
    reason_counts <- as.data.frame(table(data$travel_reason))
    colnames(reason_counts) <- c("Reason", "Count")
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Distribution by Travel Reason") %>%
      hc_xAxis(categories = reason_counts$Reason) %>%
      hc_yAxis(title = list(text = "Count")) %>%
      hc_add_series(
        name = "Travelers",
        data = reason_counts$Count,
        color = color_palette$sea_green
      ) %>%
      hc_plotOptions(
        column = list(
          dataLabels = list(
            enabled = TRUE,
            format = '{point.y}'
          )
        )
      )
  })
  
  output$ageDistributionPlot <- renderHighchart({
    data <- filtered_data_overview()  # Use filtered data for Overview
    hchart(
      data$age,
      type = "histogram",
      name = "Travelers",
      color = color_palette$celadon
    ) %>%
      hc_title(text = "Age Distribution") %>%
      hc_xAxis(title = list(text = "Age")) %>%
      hc_yAxis(title = list(text = "Count"))
  })
}

shinyApp(ui, server)