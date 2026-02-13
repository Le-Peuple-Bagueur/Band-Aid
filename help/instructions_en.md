## Band-Aid App - User Guide

Welcome to Band-Aid! This guide explains how to use each feature of the app.

### Table of Contents
1. [Upload Your Data](#upload-your-data)
2. [Apply Filters](#apply-filters)
3. [View Your Data in a Table](#view-your-data-in-a-table)
4. [Create a Map](#create-a-map)

---

### Upload Your Data

The **Upload** section is your starting point. This is where you load your GameBird data into the app.

#### How to Upload:
1. Click the **Upload** tab when the app starts
2. Click **Browse** or **Choose File**
3. Select your GameBird CSV file from your computer
4. The app automatically reads your file and prepares it for analysis. 

#### Automatic Lookup Merging:
- The app will automatically merge any lookup files in the **Look Ups** folder
- These might include reference tables for species codes, age, or other reference data
- You don't need to do anything—this happens automatically! 
- **the main file upload + lookup table merge take a whooping 10-15 minutes**. If you always use the same gamebird subset (e.g., regional data associated with one permit) **I strongly suggest that after the first upload you create that subset and download it** (download link under the data table once it's filtered). Only upload that subset for faster operations.
- - Once uploaded, your data will be processed and the Filters will become available

### Apply Filters
The **Filters** section lets you refine your data by selecting specific records.

#### Available Filters:
The app will automatically create filters based on the columns in your data. Common filters include:
- **Date range**: Select start and end dates
- **Species**: Choose one or more bird species
- **Numeric columns**: Set minimum and maximum values (e.g., time, count)
- **Text columns**: Search or select from available values

#### How to Use Filters:
1. Click the **Filters** tab
2. You'll see different filter options for each data column
3. **For dropdown filters**: Click to expand and select values you want
4. **For numeric filters**: Enter min/max values or use sliders
5. **For date filters**: Pick your date range from the calendar
6. Click **Apply** to filter your data
7. The table will update to show only matching records

#### Filter Tips:
- You can combine multiple filters (AND logic)
- The number of matching records is shown at the bottom
- Clear any filter by resetting it to blank/all

---

### View Your Data in a Table
The **Table** section displays your filtered data in a structured format.

#### Features:

##### **View Your Data**
- Scroll horizontally to see all columns
- Click column headers to sort data
- Use the search box to find specific records

##### **Merge with a Station File** (Optional)
1. Expand the **"Add Station Names"** section
2. Upload a CSV/Excel file with station names and details
3. Once you selected the latitude anf longitude fields in your station file, the merge runs automatically
4. The app will match records and add the new information to your table

##### **Download Your Data**
1. Once you've filtered and customized your data, you can download it
2. Click the **Download** button located under the data table
3. Choose your format:
   - **CSV**: Simple comma-separated format (opens in Excel)
   - **XLSX**: Excel spreadsheet format
4. The file will download to your computer's Downloads folder

##### **Table Navigation**
- Use pagination controls at the bottom to move between pages
- Change the number of rows displayed per page
- Export buttons are at the top right of the table

---

### Create a Map
The **Map** section visualizes your observations geographically.

#### What You'll See:
- An interactive map showing:
  - **Encounter markers** (points where birds were collected)
  - **Station markers** (larger black symbols showing station locations)
  - Legend explaining the symbols and colors

#### Map Controls:
##### **Species Selector**
1. Click the species checkbox menu on the left
2. Select which species you want to display
3. Species are listed alphabetically by name
4. Only selected species will appear as markers on the map

##### **Station Selector**
1. Click the station checkbox menu on the left
2. Choose which stations to show
3. **"No Station"** option shows observations without a specific station assignment
4. Station markers appear as larger black symbols

##### **Interactive Features**
- **Zoom**: Use your mouse wheel or pinch on touch devices
- **Pan**: Click and drag the map to move around

##### **Station Marker Mode** 
The map can show station markers in two ways:
- **Centroid mode**: Shows the center point of all observations at that station
- **Most recent**: Shows the location of the most recent observation at that station

#### Export Your Map:
1. Once you've customized the map (selected species/stations)
2. Click the **Download Map** button
3. Choose your format:
   - **JPEG**: High-quality image format (recommended for sharing)
   - **PNG**: High-resolution image with transparency
4. The map image will download to your computer download folder

Contact your administrator if you need further assistance.
