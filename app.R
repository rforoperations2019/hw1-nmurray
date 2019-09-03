#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
load("ny_brewery.Rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Wineries, Distilleries, and Breweries in New York"),
   
   # Sidebar 
   sidebarLayout(
     
      sidebarPanel(
        
        # Show data table ---------------------------------------------
        checkboxInput(inputId = "show_data",
                      label = "Show data table",
                      value = TRUE),
        
        # Select Timeframe --------------------------------------------
        dateRangeInput(inputId = "show_date_range",
                       label = "Date Range",
                       start = "1970-10-08",
                       end = "2022-09-30",
                       format = "mm/dd/yyyy",
                       startview = "year"),
        # Select Input (License type)-----------------------------
        selectInput(inputId = "license_type_Name", 
                    label = "Select License Type",
                    choices = c("BREWER",
                                "DISTILLER \"A-1\"",
                                "DISTILLER \"B-1\"",
                                "DISTILLER \"B\"",
                                "DISTILLER \"C\"" ,
                                "FARM BREWER",
                                "FARM DISTILLER \"D\"",
                                "FARM WINERY",
                                "MICRO BREWER",
                                "RESTAURANT BREWER",
                                "WINERY",
                                "WINERY / FARM WINERY RETAIL"),
                    selected = "License.Type.Name",
                    multiple = TRUE),
        
        # Select Input (County of License) -----------------------------
        selectInput(inputId = "license_type_Name",
                    label = "Select County of Licensee",
                    choices = c("ALBANY","ALLEGANY","BRONX","BROOME","CATTARAUGUS", "CAYUGA",
                                "CHAUTAUQUA", "CHEMUNG", "CHENANGO", "CLINTON", "COLUMBIA","CORTLAND",
                                "DELAWARE", "DUTCHESS", "ERIE", "ESSEX", "FRANKLIN", "FULTON",
                                "GENESEE", "GREENE", "HERKIMER", "JEFFERSON", "KINGS",  "LEWIS",
                                "LIVINGSTON", "MADISON", "MONROE", "MONTGOMERY", "NASSAU", "NEW YORK",
                                "NIAGARA", "ONEIDA", "ONONDAGA", "ONTARIO", "ORANGE", "ORLEANS",
                                "OSWEGO", "OTSEGO", "PUTNAM", "QUEENS", "RENSSELAER", "RICHMOND",
                                "ROCKLAND", "SARATOGA", "SCHENECTADY", "SCHOHARIE", "SCHUYLER", "SENECA",
                                "ST LAWRENCE", "STEUBEN", "SUFFOLK", "SULLIVAN","TIOGA","TOMPKINS",
                                "ULSTER", "WARREN", "WASHINGTON", "WAYNE", "WESTCHESTER", "WYOMING"),
                    multiple = TRUE)
        
        #ADD ANOTHER INPUT HERE---------------------------------------
        
        #PLOT oor FIGURE 1--------------------------------------------
        
        #PLOT oor FIGURE 2--------------------------------------------
        
        #PLOT oor FIGURE 3--------------------------------------------

       ),

      mainPanel(
        # Show data table ---------------------------------------------
        DT::dataTableOutput(outputId = "brewtable")
      )
   )
)

server <- function(input, output) {
   
  # Print data table if checked -------------------------------------
  output$brewtable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = df_brew[, 1:7],
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

