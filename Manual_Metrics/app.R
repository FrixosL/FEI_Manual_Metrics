#### Load packages and settings ####
# Load require packages
library(shiny) # Framework for web apps
library(tidyverse) # Colletion of multiple plugins for various applications
library(lubridate) # Plugin to manipulate "date" data
library(readr) # Plugin to load CSV files
library(shinyWidgets) # Plugin for web application widgets

# Settings: Increase maximum file upload size to 30 megabytes
options(shiny.maxRequestSize = 50*1024^2)

#### UI Function ####
# Define UI for application. Anything in this section dictates what the 
# application will display
ui <- fluidPage(
    
    # Application title
    titlePanel("Input settings"),
    
    # Sidebar (settings)
    sidebarLayout(
        sidebarPanel(
            
            # Input: Select a file ----
            # inputID allows you to link the input/output with computation part
            fileInput(inputId = "Monthly_Data",
                      # Label of the input widget
                      label = "Step 1: Import CSV file with monthly data",
                      # Doesn't allow multiple files uploaded at once
                      multiple = FALSE,
                      # Describes what file types are accepted (only CSVs atm)
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Select box: choose the column with transaction date
            selectInput(inputId = "Monthly_ID_col", 
                        label = "Step 2: Select the column with the unique customer ID (email, name)",
                        choices = ""),
            # Select box: choose the column with transaction date
            selectInput(inputId = "Monthly_Date_col", 
                        label = "Step 3: Select the column with the transaction date",
                        choices = ""),
            # Select box: choose the column with transaction date
            selectInput(inputId = "Monthly_Amount_col",
                        label = "Step 4: Select the column with the transaction amount",
                        choices = ""),
            
            # Input: Select a file ----
            # inputID allows you to link the input/output with computation part
            fileInput(inputId = "Annual_Data",
                      # Label of the input widget
                      label = "Step 5: Import CSV file with annual data",
                      # Doesn't allow multiple files uploaded at once
                      multiple = FALSE,
                      # Describes what file types are accepted (only CSVs atm)
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Select box: choose the column with transaction date
            selectInput(inputId = "Annual_ID_col", 
                        label = "Step 6: Select the column with the unique customer ID (email, name)",
                        choices = ""),
            # Select box: choose the column with transaction date
            selectInput(inputId = "Annual_Date_col", 
                        label = "Step 7: Select the column with the transaction date",
                        choices = ""),
            # Select box: choose the column with transaction date
            selectInput(inputId = "Annual_Amount_col",
                        label = "Step 8: Select the column with the transaction amount",
                        choices = ""),
            
            dateInput(inputId = "Start_date", 
                      label = "Step 9: Select the start date for the metrics", 
                      value = "2020-01-01"),
            
            numericInput(inputId = "Duration", 
                         label = "Step 10: Select the number of months until the end date of metrics table", 
                         value = 12),
            
            # Button: Download the transformed data
            strong("Step 11: Download the Metrics table"),
            downloadButton(outputId ="downloadData",
                           label = "Download"),
            ),
        
        
        # Main panel display (right-hand section)
        mainPanel(
            # Output: Tabs with tables showing data before and after ----
            tabsetPanel(type = "tabs",
                        # Create the "Import Data" tab and puts the table in it
                        # Shows the imported data in their raw format
                        tabPanel("Monthly Data", dataTableOutput("Monthly_Table")),
                        # Create the "Import Data" tab and puts the table in it
                        # Shows the imported data in their raw format
                        tabPanel("Annual Data", dataTableOutput("Annual_Table")),
                        # Create the "Export Data" tab and puts the table in it
                        # Shows the data after being transformed
                        tabPanel("Metrics Table", dataTableOutput("Metrics_Table")))
    
            
        )
    )
)

#### Server Function ####
# Define server logic to display and download selected file ----
server <- function(input, output, session) {
    
    End_date <- reactive({
        
        # Makes this a required input
        req(input$Start_date)
        
        End_date <- ymd(input$Start_date) + months(input$Duration)
        
    })
    
    # Names the uploaded files as "Monthly_Data"
    Monthly_Data <- reactive({
        
        # Makes this a required input
        req(input$Monthly_Data)
        
        # Reads the CSV file uploaded before
        df <- read.csv(input$Monthly_Data$datapath,
                       header = TRUE,
                       sep = ",")
        
    })
    
    # Reactive data table for imported data (first tab)
    output$Monthly_Table <- renderDataTable({
        
        req(input$Monthly_Data)
        
        return(Monthly_Data())
        
    })
    
    observe({
        
        updateSelectInput(inputId = "Monthly_ID_col",
                          choices = colnames(Monthly_Data())
        )
    })
    
    observe({
        
        updateSelectInput(inputId = "Monthly_Date_col",
                          choices = colnames(Monthly_Data())
        )
    })
    
    observe({
        
        updateSelectInput(inputId = "Monthly_Amount_col",
                          choices = colnames(Monthly_Data())
        )
    })
    
    # Names the uploaded files as "Raw_Data"
    Annual_Data <- reactive({
        
        # Makes this a required input
        req(input$Annual_Data)
        
        # Reads the CSV file uploaded before
        df <- read.csv(input$Annual_Data$datapath,
                       header = TRUE,
                       sep = ",")
        
    })
    
    # Reactive data table for imported data (first tab)
    output$Annual_Table <- renderDataTable({
        
        req(input$Annual_Data)
        
        return(Annual_Data())
        
    })
    
    observe({
        
        updateSelectInput(inputId = "Annual_ID_col",
                          choices = colnames(Annual_Data())
        )
    })
    
    observe({
        
        updateSelectInput(inputId = "Annual_Date_col",
                          choices = colnames(Annual_Data())
        )
    })
    
    observe({
        
        updateSelectInput(inputId = "Annual_Amount_col",
                          choices = colnames(Annual_Data())
        )
    })
    
    # Saves the data to "Clean_Data"
    Clean_Monthly_Data <- reactive({
        
        Clean_Monthly_Data <- Monthly_Data() %>% 
            mutate(Accrual_Amount = parse_number(as.character(.data[[input$Monthly_Amount_col]])),
                   Date_Accrual = dmy(.data[[input$Monthly_Date_col]]),
                   Customer_ID = as.character(.data[[input$Monthly_ID_col]])) %>% 
            select(Accrual_Amount, Date_Accrual, Customer_ID)
    })
    
    Clean_Annual_Data <- reactive({
        
        Clean_Annual_Data <- Annual_Data() %>% 
            mutate(Accrual_Amount = parse_number(as.character(.data[[input$Annual_Amount_col]])),
                   Date_Accrual = dmy(.data[[input$Annual_Date_col]]),
                   Customer_ID = as.character(.data[[input$Annual_ID_col]])) %>% 
            select(Accrual_Amount, Date_Accrual, Customer_ID)
    })
    
    
    Metrics_Data <- reactive({
        
        Merged_Data <- Clean_Annual_Data() %>% 
            bind_rows(Clean_Monthly_Data())
        
        Select_Data <- Merged_Data
        
        MRR <- Select_Data %>%
            mutate(Year = year(Date_Accrual),
                   Month = month(Date_Accrual)) %>% 
            group_by(Customer_ID, Year, Month) %>% 
            summarise(MRR = sum(Accrual_Amount))
        
        Unique_Names <- Select_Data %>% 
            summarise(Customer_ID = unique(Customer_ID)) %>% 
            mutate(Date_Accrual = ymd(End_date()))
        
        Accrual_month <- rep(0:(input$Duration - 1), nrow(Unique_Names))
        
        # Duplicate the data based on input$Period (frequency of transactions)
        Expanded_Data <- Unique_Names %>% 
            slice(rep(1:n(), each = input$Duration)) %>% 
            # Divide the payment amount by the frequency of transactions
            cbind(Accrual_month) %>%
            # Create new variable "Accrual Date" by spreading costs over the next x months
            mutate(Date_Accrual = Date_Accrual - (Accrual_month*months(1)),
                   Year = year(Date_Accrual),
                   Month = month(Date_Accrual)) %>% 
            select(!c(Accrual_month, Date_Accrual))
        
        Joint_Data <- Expanded_Data %>% 
            left_join(MRR, 
                      by = c("Customer_ID","Year", "Month")) %>% 
            mutate(MRR = if_else(is.na(MRR), 0, MRR)) %>% 
            group_by(Customer_ID) %>%
            mutate(lag_MRR = lead(MRR),
                   lag_MRR = if_else(is.na(lag_MRR), 0, lag_MRR),
                   Churn_Amount = if_else(MRR == 0, lag_MRR - MRR, 0),
                   Upgrade_Amount = if_else(lag_MRR == 0, 0,if_else(MRR > lag_MRR, MRR - lag_MRR, 0)),
                   Downgrade_Amount = if_else(MRR == 0, 0,if_else(MRR < lag_MRR, lag_MRR - MRR, 0)),
                   Count = if_else(MRR > 0, 1, 0),
                   Churn_Customers = if_else(Count == 0, lead(Count) - Count, 0)) %>%
            ungroup() %>% 
            group_by(Year, Month) %>% 
            summarise(MRR = sum(MRR),
                      Churn_MRR = sum(Churn_Amount),
                      MRR_Upgrade = sum(Upgrade_Amount),
                      MRR_Downgrade = sum(Downgrade_Amount),
                      Total_Customers = sum(Count),
                      Churn_Customers = sum(Churn_Customers)) %>%
            ungroup() %>% 
            mutate(ARR = MRR*12,
                   Total_Churn = Churn_MRR + MRR_Downgrade - MRR_Upgrade,
                   MRR_Churn_Rate = Total_Churn / lag(MRR),
                   Customer_Churn_Rate = Churn_Customers / lag(Total_Customers),
                   ARPU = MRR / Total_Customers,
                   CLV = ARPU / Customer_Churn_Rate,
                   ARPU = if_else(is.na(ARPU), 0, ARPU),
                   CLV = if_else(is.na(CLV), 0, if_else(is.infinite(CLV), MRR, CLV)),
                   Customer_Churn_Rate = if_else(is.na(Customer_Churn_Rate), 0, Customer_Churn_Rate),
                   MRR_Churn_Rate = if_else(is.na(MRR_Churn_Rate), 0, MRR_Churn_Rate))
        
    })
    
    # Reactive data table for exported data
    output$Metrics_Table <- renderDataTable({
        
        return(Metrics_Data())
        
    })
    

    # Download csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = "Metrics.csv",
        content = function(file) {
            write.csv(Metrics_Data(), file, row.names = FALSE)
        }
    )
}

#### Run the application ####
shinyApp(ui = ui, server = server)