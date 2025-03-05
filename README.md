# Labour Mobility Schemes Registry

**Version**: 2.0  
**Updated**: February 27, 2025  
**Developed by**: Vanuatu Bureau of Statistics  
**Hosted on**: [https://htevilili.shinyapps.io/LabourMobilityRegistry/](https://htevilili.shinyapps.io/LabourMobilityRegistry/)  

The Labour Mobility Schemes Registry is a Shiny-based web application designed to manage, visualize, and explore labour mobility data for the Vanuatu Bureau of Statistics. This hosted version (2.0) introduces enhanced security, usability, and interactivity features.

---

## Overview

This application provides a dashboard interface to:
- Visualize key statistics and distributions (e.g., travel reasons, age).
- Explore and edit labour mobility data stored in an SQLite database.
- Upload new data in a structured CSV format.

Built with R, Shiny, and `bs4Dash`, it leverages modern libraries for responsive design and interactive data visualization.

---

## Features

### New in Version 2.0
1. **Role-Based Access Control**
   - Two user roles: `admin` (full access) and `viewer` (read-only).
   - Restricted access to data uploads and edits for `admin` users only.

2. **Enhanced Overview Tab**
   - Filterable controlbar with options for Year, Direction, and Sex.
   - Interactive value boxes: Total Travelers, Average Age, and Active Programs.
   - Charts (Travel Reason and Age Distribution) update dynamically with filters.
![Screenshot 2025-02-28 104351](https://github.com/user-attachments/assets/df7419f2-c6e3-4aeb-a808-2aae83a05263)

3. **Improved Data Exploration Tab**
   - Toggleable edit mode via an "Update" button (admin-only).
   - "Submit" and "Cancel" buttons to manage changes, ensuring data integrity.
![Screenshot 2025-02-28 104402](https://github.com/user-attachments/assets/32325bba-7323-4fe6-b0c3-d8bb53cce329)

4. **Controlbar Management**
   - Tab-specific controlbar, visible only in the Overview tab for context-aware filtering.
![image](https://github.com/user-attachments/assets/c2042c8a-9a1e-495b-b43b-bc6e4df24f1d)

5. **Data Handling Improvements**
   - Year extraction from `travel_date` for filtering (format: MM/DD/YYYY).
   - Robust error handling for date parsing and database operations.

6. **UI/UX Enhancements**
   - Smaller login modal for better usability.
   - Edit buttons hidden by default, with improved state management during edits.

### Core Features
- Interactive dashboard with `highcharter` visualizations and `DT` data tables.
- Data upload functionality with CSV template download
![Screenshot 2025-02-28 104415](https://github.com/user-attachments/assets/a100890f-4d34-4462-bf4b-fc51940caf66)
- SQLite database integration for persistent storage.
- Responsive design with `bs4Dash` styling based on NSDP logo colors.

---

## Accessing the Application

The application is hosted on Shinyapps.io and can be accessed at:  
[https://htevilili.shinyapps.io/LabourMobilityRegistry/](https://htevilili.shinyapps.io/LabourMobilityRegistry/)  

### Login
- The app supports two user roles: `admin` and `viewer`.
- Contact the Vanuatu Bureau of Statistics IT team for credentials specific to your role.

---

## Usage

1. **Visit the App**
   - Navigate to the hosted URL provided above.
   - A login modal will appear upon loading.

2. **Login**
   - Use your assigned `admin` or `viewer` credentials.
   - Contact the developer below to request temporary credentials.

3. **Navigate Tabs**
   - **Overview**: View filtered statistics and charts (filters available in the controlbar).
   - **Data Exploration**: Browse and (for admins) edit the dataset.
   - **Upload Data**: (Admin-only) Upload new CSV data matching the required structure.

4. **Data Upload**
   - Download the CSV template from the Upload Data tab.
   - Ensure your CSV has the exact column order (see below).
     ![Screenshot 2025-02-28 104415](https://github.com/user-attachments/assets/5b3d1971-93fc-4dc8-bd74-06ec55a4bdfc)

---

## Data Structure

The application expects uploaded CSV data to match the following column structure (in this order):

| Column Name             | Description                          |
|-------------------------|--------------------------------------|
| `surname`              | Traveler's surname                  |
| `given_name`           | Traveler's given name               |
| `nationality`          | Traveler's nationality              |
| `country_of_residence` | Country of residence                |
| `document_type`        | Type of travel document             |
| `document_no`          | Document number                     |
| `dob`                  | Date of birth (MM/DD/YYYY)          |
| `age`                  | Age of traveler                     |
| `sex`                  | Sex (e.g., M/F)                     |
| `travel_date`          | Travel date (MM/DD/YYYY)            |
| `direction`            | Travel direction (e.g., In/Out)     |
| `accommodation_address`| Address of accommodation            |
| `note`                 | Additional notes                    |
| `travel_reason`        | Reason for travel                   |
| `border_post`          | Border post used                    |
| `destination_coming_from` | Origin/destination of travel     |

---

## Customization

- **Hosted Version**: Customization requires updates to the source code and redeployment. Contact the development team for modifications.
- **Color Palette**: The current theme uses NSDP logo colors, adjustable in the source code.

---

## Troubleshooting

- **Login Issues**: Verify your credentials with the IT team.
- **Data Upload Errors**: Ensure your CSV matches the required structure (download the template for reference).
- **App Not Loading**: Check the Shinyapps.io status or contact support.

---

## Support

For assistance, credential requests, or feedback, please contact:  
**Herman Tevilili**  
htevilili@gmail.com  
