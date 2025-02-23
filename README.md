# Labour Mobility Schemes Registry

## Overview
The **Labour Mobility Schemes Registry** is a Shiny application built using R, designed to manage and visualize data related to labour mobility schemes. This application allows users to explore data, generate visual reports, and upload new datasets in a secure and interactive environment.

## Features
- **Dashboard Overview**: Visual representation of key labour mobility indicators.
- **Data Exploration**: Interactive data table for browsing and filtering dataset records.
- **Data Uploading**: Allows administrators to upload new CSV files into the database.
- **User Authentication**: Secure login system to restrict access to authorized users.
- **Dynamic Visualizations**: Highcharts-based plots for analyzing trends in labour mobility.
- **SQLite Database Integration**: Efficient storage and retrieval of data.
- **Dark Mode Support**: Improved UI experience for users.

## Technology Stack
- **R Shiny**: For building the web application.
- **bs4Dash**: For an enhanced dashboard UI.
- **highcharter**: For interactive visualizations.
- **DT**: For interactive data tables.
- **leaflet**: For geographic mapping (if required in the future).
- **SQLite (via RSQLite and DBI)**: For database storage.
- **shinyjs**: For improved UI functionality.

## Installation & Setup
### Prerequisites
Ensure you have R and the required packages installed:
```r
install.packages(c("shiny", "bs4Dash", "highcharter", "DT", "leaflet", "sf", "dplyr", "readxl", "jsonlite", "tidyr", "RSQLite", "DBI", "shinyjs"))
```

### Running the Application
1. Clone this repository:
   ```sh
   git clone https://github.com/your-repo/labour-mobility-registry.git
   cd labour-mobility-registry
   ```
2. Open R or RStudio and set the working directory:
   ```r
   setwd("path/to/labour-mobility-registry")
   ```
3. Run the application:
   ```r
   shiny::runApp()
   ```

## Usage
### Login Credentials
- **Username**: `admin`
- **Password**: `admin123`

After logging in, you can explore data, upload new records, and interact with the dashboard.

### Uploading Data
- Navigate to the `Upload Data` tab.
- Choose a CSV file matching the required format.
- Click `Upload Data` to add records to the database.

### Exploring Data
- Navigate to the `Data Exploration` tab.
- Use the interactive table to filter and analyze records.
- Download filtered data using the `Download Filtered Table` button.

## Database Structure
The application connects to an SQLite database (`/db.sqlite`) to store and retrieve information. The main table structure includes:

- **workers**: Stores details such as `worker_id`, `first_name`, `last_name`, `gender`, `dob`, `nationality`, `passport_number`, and `visa_type`.
- **employment**: Tracks job placements with fields like `worker_id`, `employer`, `job_title`, `start_date`, `end_date`, `status`, and `country`.
- **payments**: Contains records of worker payments, including `worker_id`, `amount_paid`, `payment_date`, and `currency`.

## Contributing
If you would like to contribute to this project, please fork the repository and submit a pull request with your changes.

## License
© 2025 Vanuatu Bureau of Statistics. All rights reserved.

