
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(shinythemes)

load("ny_brewery.Rdata")

# Define UI f
ui <- fluidPage(

   # Application title
   titlePanel("Wineries, Distilleries, and Breweries in New York"),

   
   # Sidebar 
   sidebarLayout(
     
     
     # Inputs: Select variables to plot ------------------------------
      sidebarPanel(
        
        h4("Data Output Displays By County"),
        
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
        
        #Select Brewery Name---------------------------------------
        selectInput(inputId = "premise",
                    label = "Select Brewery Name",
                    choices = premises),
        
        
        # Expiration Date Range Input ---------------------------
        dateInput(inputId = "date_expire",
                  label = "Date License Set to Expire",
                  max =   "2022-05-31"),
        
        
        # # Show data table ---------------------------------------------
        checkboxInput(inputId = "show_data",
                      label = "Show data table",
                      value = TRUE),
        
        
        h5("Data Output Displays By License"),
        
        
        # Select Input (License type)-----------------------------
        radioButtons(inputId = "license_type_Name", 
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
                    selected = "BREWER"),

    
     # Select sample size ----------------------------------------------------
     numericInput(inputId = "n_samp", 
                  label = "Number of Counties:", 
                  min = 1, max = nrow(df_brew), 
                  value = 50),
     
     ## Download Button--------------------------------------------------------
     downloadButton("downloadData", "Download All Data")
     
   ),

   # Output: -------------------------------------------------------
      mainPanel(
        fluidRow(column(6,
                        # Show License PLot--------------------------------------------
                        plotOutput(outputId = "license_plot") ),
                 column(6,
                        #PLOT or FIGURE 2: Show Expiration Date Scatterplot--------------------------------------------
                        plotOutput(outputId = "expiration_plot"))),
        fluidRow(column(12,
                        #PLOT or FIGURE 3--------------------------------------------
                        plotOutput(outputId = "issue_plot"))),
        fluidRow(column(12,
                        # Show data table ---------------------------------------------
                        plotOutput(outputId = "dotplot"))),
        fluidRow(column(12,
                        # Show data table ---------------------------------------------
                        DT::dataTableOutput(outputId = "brewtable")))

      )
   )
   )
   

server  <- function(input, output, session){
  
  #Output for download-----------------------------------------
  data_to_download <- df_brew
  
  #Create a subset to filter for County------------------------------------
  dfbrew_subset <- reactive({
    req(input$licensee_county)
    filter(df_brew, County.Name..Licensee. %in% input$licensee_county)
    }
  )
 # Create Subset to filter by License Type--------------------------------
  license_subset <- reactive({
    req(input$license_type_Name)
    filter(df_brew, License.Type.Name %in% input$license_type_Name)
    }
  )
  
  # Create Subset to filter by Expiration Date---------------------------
  expire_subset <- reactive({
    req(input$date_expire)
    filter(df_brew, License.Expiration.Date %in% input$date_expire)
    }
  )
  
  # Update the maximum allowed n_samp for selected Counties ------
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(50, nrow(dfbrew_subset())),
                       max = nrow(dfbrew_subset())
                       )
    }
  )
  
  # Create new df that is n_samp obs from selected type movies ------
  counties_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(dfbrew_subset(), input$n_samp)
  })
  
  # # Show Bar PLot Of License Type by County Level Info-------------------------------
  output$license_plot <- renderPlot({
    
    ggplot(dfbrew_subset(), aes(License.Type.Name))+ 
      geom_bar(aes(fill = County.Name..Licensee.))+
      theme(axis.text.x = element_text(angle = 90))+ 
      labs(x = paste("License Type"),
           y = "Count",
           title = paste("License Type By County"))+
      scale_color_brewer(palette="Dark2")
    
    }
   )
  
  # Show Density Plot for License Issue Date By Agency Zone Office Name
  output$issue_plot <- renderPlot({
    ggplot(counties_sample(), aes(x=License.Original.Issue.Date, fill=Agency.Zone.Office.Name)) +
      geom_density() + 
      labs(title="License Issue Date by Office Zone", x="Date Issued")
    }
  )
  
  # # Show Time Series plot Plot for Expiration Date by License Type (Changes with county data)-----------------------------
  output$expiration_plot <- renderPlot({
    
     ggplot(license_subset())+
     geom_bar(mapping = aes(x = License.Expiration.Date, fill = input$license_type_Name),position = "dodge") +
     theme(axis.text.x = element_text(angle = 90)) +
     scale_x_date(date_labels = "%b-%Y")+
      labs(x = paste("License Expiration Date"),
           title = paste("License Type By Expiration Date"))
    }
  )
  
  ## Show Dotplot for reactive expiration date 
  
  # days_left_expire <-reactive({as.numeric(order(df_brew$License.Expiration.Date -Sys.Date()))})
  days_left_expire <-reactive({as.numeric(order(input$date_expire -Sys.Date()))})
  # days_left_expire <-as.numeric(order(dfbrew_subset()$License.Expiration.Date -Sys.Date()))
  
  output$dotplot <- renderPlot({
    
    
    dotchart(days_left_expire(), labels = input$premise,
             cex = .7, xlim = c(1,1000),
             main = "Days Left until License Expires", 
             xlab = "days")
    }
  )

  #Print data table if checked -------------------------------------
  output$brewtable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(counties_sample()[,1:5],
                    options = list(pageLength = 5),
                    rownames = FALSE)
    }
  )
  
  # Download All  Data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), '.csv', sep='')
      },
    content = function(con) {
      write.csv(data_to_download, con)
    }
  )
  
  
}
# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)