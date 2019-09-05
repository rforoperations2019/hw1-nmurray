
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)

load("ny_brewery.Rdata")

# Define UI f
ui <- fluidPage(
   
   # Application title
   titlePanel("Wineries, Distilleries, and Breweries in New York"),
   
   # Sidebar 
   sidebarLayout(
     # Inputs: Select variables to plot ------------------------------
      sidebarPanel(
        
        # # Show data table ---------------------------------------------
        checkboxInput(inputId = "show_data",
                       label = "Show data table",
                       value = TRUE),

        # # Inputs Plot Times Series Data for License Info
        # # Select License Issue --------------------------------------------
        # dateRangeInput(inputId = "date_issued",
        #                label = "Date License Issued",
        #                start = "1970-10-08",
        #                end = "2022-09-30",
        #                format = "mm/dd/yyyy",
        #                startview = "year"),
        # 
        # # Select License Expiration --------------------------------------------
        # dateRangeInput(inputId = "date_expire",
        #                label = "Date License Set to Expire",
        #                start = "1970-10-08",
        #                end = "2022-09-30",
        #                format = "mm/dd/yyyy",
        #                startview = "year"),
        
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
                    selected = "BREWER",
                     multiple = TRUE),
        
        #Select Licensee County---------------------------------------
        selectInput(inputId = "licensee_county",
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
                                "ULSTER", "WARREN", "WASHINGTON", "WAYNE", "WESTCHESTER", "WYOMING")),
        
        # Select fill for expiration date plot------------------------------------------------
        selectInput(inputId = "license_type_code", 
                    label = "License Code Type",
                    choices = c("D","DA", "DB",
                                "DC", "DD", "DW",
                                "FD", "FW", "MI",
                                "MR", "WA"),
                    selected = "FW",
                    multiple = TRUE),
        
        # Select fill for License Issue date  plot ------------------------
        checkboxGroupInput(inputId = "office_name",
                           label = "Select Name of Agency Office):",
                           choices = c("Albany", "Buffalo", "New York City"),
                           selected = "Buffalo")
        

       ),
   # Output: -------------------------------------------------------
      mainPanel(
        # Show data table ---------------------------------------------
        DT::dataTableOutput(outputId = "brewtable"),
        
        # Show License PLot--------------------------------------------
        plotOutput(outputId = "license_plot"),
             
        #PLOT or FIGURE 2: Show Expiration Date Scatterplot--------------------------------------------
        plotOutput(outputId = "expiration_plot"),
        
        #PLOT or FIGURE 3--------------------------------------------
        plotOutput(outputId = "issue_plot")
      )
   )
)

server <- function(input, output, session) {
  
  #Create a subset to filter for License Name Type------------------------------------
  dfbrew_subset <- reactive({
    req(output$licensee_county)
    filter(df_brew, County.Name..Licensee. %in% licensee_county)
  })

  # # Show Bar PLot For County Level Info-------------------------------
  output$license_plot <- renderPlot({
    
    ggplot(df_brew, aes(input$license_type_Name))+ 
      geom_bar(aes(fill = input$licensee_county))+
      theme(axis.text.x = element_text(angle = 90))
    
    }
   )
  # # Show Time Series plot Plot for Expiration Date by License Type Code -----------------------------
  output$expiration_plot <- renderPlot({
     ggplot(df_brew)+
     geom_bar(mapping = aes(x = License.Expiration.Date, fill = input$license_type_code), position = "dodge") +
     theme(axis.text.x = element_text(angle = 90)) +
     scale_x_date(date_labels = "%b-%Y")
    }
   )
  # Show Denisty Plot for License Issue Date By Agency Zone Office Name
  output$issue_plot <- renderPlot({
    ggplot(df_brew, aes(x=License.Original.Issue.Date, fill=input$office_name)) +
      geom_density() + 
      labs(title="License Issue Date by Office Zone", x="Date Issued")
    }
  )
  #Print data table if checked -------------------------------------
  output$brewtable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = dfbrew_subset()[, 1:5],
                    options = list(pageLength = 5),
                    rownames = FALSE)
    }
  )
}
  # Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)

