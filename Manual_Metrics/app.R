#### Load packages and settings ####
# Load require packages
library(shiny) # Framework for web apps
library(tidyverse) # Colletion of multiple plugins for various applications
library(lubridate) # Plugin to manipulate "date" data
library(readr) # Plugin to load CSV files
library(shinyWidgets) # Plugin for web application widgets

# Settings: Increase maximum file upload size to 30 megabytes
options(shiny.maxRequestSize = 250*1024^2)

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
                      label = "Step 1: Import CSV file with monthly transactions",
                      # Doesn't allow multiple files uploaded at once
                      multiple = FALSE,
                      # Describes what file types are accepted (only CSVs atm)
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Select box: choose the column with ID column for monthly data
            selectizeInput(inputId = "Monthly_ID_col", 
                           label = "Step 2: Select the column with the unique customer ID for monthly transactions (email, name)",
                           choices = ""),
            # Select box: choose the column with the mothly trnascation date
            selectizeInput(inputId = "Monthly_Date_col", 
                        label = "Step 3: Select the column with the transaction date for monthly transactions",
                        choices = ""),
            # Select box: choose the column with transaction amount
            selectizeInput(inputId = "Monthly_Amount_col",
                        label = "Step 4: Select the column with the amount for monthly transactions",
                        choices = ""),
            
            # Input: Select a file ----
            # inputID allows you to link the input/output with computation part
            fileInput(inputId = "Annual_Data",
                      # Label of the input widget
                      label = "Step 5: Import CSV file with annual transactions (accrual)",
                      # Doesn't allow multiple files uploaded at once
                      multiple = FALSE,
                      # Describes what file types are accepted (only CSVs atm)
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Select box: choose the column with unique customer ID for annual data
            selectizeInput(inputId = "Annual_ID_col", 
                        label = "Step 6: Select the column with the unique customer ID for annual transactions (email, name)",
                        choices = ""),
            # Select box: choose the column with the accrual transaction date for annual data
            selectizeInput(inputId = "Annual_Date_col", 
                        label = "Step 7: Select the column with the accrual date for annual transactions",
                        choices = ""),
            # Select box: choose the column with the accrual transaction amount for annual data
            selectizeInput(inputId = "Annual_Amount_col",
                        label = "Step 8: Select the column with the accrual amount for annual transactions",
                        choices = ""),
            
            # Select box: choose the column to be filtered
            selectizeInput(inputId = "Filter_Col_1", 
                        label = "Step 9: Select the first column to filter",
                        choices = ""),
            
            # Select box: choose the filters to be applied on the column selected above
            selectizeInput(inputId = "Filter_Var_1",
                        label = "Step 10: Select what to be included in the data",
                        choices = "",
                        multiple = TRUE),
            
            # Select box: choose the second column to be filtered
            selectizeInput(inputId = "Filter_Col_2", 
                        label = "Step 11: Select the second column to filter",
                        choices = ""),
            
            # Select box: choose the filters to be applied on the column selected above
            selectizeInput(inputId = "Filter_Var_2",
                        label = "Step 12: Select what to be included in the data",
                        choices = "",
                        multiple = TRUE),
            
            # Data input box: Allows the user to select a data that the metrics table will begin from
            dateInput(inputId = "Start_date", 
                      label = "Step 13: Select the start date for the metrics table",
                      # default date
                      value = "2019-01-01"),
            
            # Number input: allows the user to input the number of months that the metrics table covers
            numericInput(inputId = "Duration", 
                         label = "Step 14: Select the number of months until the end date of metrics table",
                         # default to 24 months
                         value = 24),
            
            # Button: Download the transformed data
            strong("Step 15: Download the Metrics table"),
            downloadButton(outputId ="downloadData",
                           label = "Download"),
            ),
        
        
        # Main panel display (right-hand section)
        mainPanel(
            # Output: Tabs with tables showing data before and after ----
            tabsetPanel(type = "tabs",
                        # Create the "Monthly Data" tab and puts the table in it
                        # Shows the imported data in their raw format
                        tabPanel("Monthly Data", dataTableOutput("Monthly_Table")),
                        # Create the "Annual Data" tab and puts the table in it
                        # Shows the imported data in their raw format
                        tabPanel("Annual Data", dataTableOutput("Annual_Table")),
                        # Create the "Metrics Table" tab and puts the table in it
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
        
        # Calculates the end date of the metrics table based on the start data
        # selected by the user and the number of months
        End_date <- ymd(input$Start_date) + months(input$Duration)
        
    })
    
    # Names the uploaded files as "Monthly_Data"
    Monthly_Data <- reactive({
        
        # Makes this a required input
        req(input$Monthly_Data)
        
        # Reads the CSV file uploaded before
        df <- read_csv(input$Monthly_Data$datapath,
                       col_names = TRUE)
        
    })
    
    # Reactive data table for Monthly data (first tab)
    output$Monthly_Table <- renderDataTable({
        
        req(input$Monthly_Data)
        
        return(Monthly_Data())
        
    })
    
    # Creates the dynamic selection options for the drop-down select box in Step 2
    # Once a file is uploaded, this function uses all the column names in the file
    # as possible selections.
    # THe UI (what the user sees) and the Server (how it's computed) are linked
    # based on the inputID.
    observe({
        
        updateSelectizeInput(inputId = "Monthly_ID_col",
                          choices = colnames(Monthly_Data()),
                          server = TRUE)
    })
    
    # Creates the dynamic selection options for the drop-down select box in Step 3
    observe({
        
        updateSelectizeInput(inputId = "Monthly_Date_col",
                          choices = colnames(Monthly_Data()),
                          server = TRUE)
    })
    
    # Creates the dynamic selection options for the drop-down select box in Step 4
    observe({
        
        updateSelectizeInput(inputId = "Monthly_Amount_col",
                          choices = colnames(Monthly_Data()),
                          server = TRUE)
    })
    
    # Names the uploaded files as "Annual_Data"
    Annual_Data <- reactive({
        
        # Makes this a required input
        req(input$Annual_Data)
        
        # Reads the CSV file uploaded before
        df <- read_csv(input$Annual_Data$datapath,
                       col_names = TRUE)
        
    })
    
    # Reactive data table for Annual data (second tab)
    output$Annual_Table <- renderDataTable({
        
        req(input$Annual_Data)
        
        return(Annual_Data())
        
    })
    
    # Creates the dynamic selection options for the drop-down select box in Step 6
    observe({
        
        updateSelectizeInput(inputId = "Annual_ID_col",
                          choices = colnames(Annual_Data()),
                          server = TRUE)
    })
    
    # Creates the dynamic selection options for the drop-down select box in Step 7
    observe({
        
        updateSelectizeInput(inputId = "Annual_Date_col",
                          choices = colnames(Annual_Data()),
                          server = TRUE)
    })
    
    # Creates the dynamic selection options for the drop-down select box in Step 8
    observe({
        
        updateSelectizeInput(inputId = "Annual_Amount_col",
                          choices = colnames(Annual_Data()),
                          server = TRUE)
    })
    
    # Creates a new dataset called "Clean_Monthly_Data" 
    Clean_Monthly_Data <- reactive({
        
        # Uses "Monthly_Data" to create new dataset
        Clean_Monthly_Data <- Monthly_Data() %>%
            # Creates a new column called "Accrual_Amount" based on the 
            # selection of the user in Step 4
            mutate(Accrual_Amount = parse_number(as.character(.data[[input$Monthly_Amount_col]])),
                   # Creates a new column called "Date_Accrual" based on the 
                   # selection of the user in Step 3
                   Date_Accrual = dmy(.data[[input$Monthly_Date_col]]),
                   # Creates a new column called "Customer_ID" based on the 
                   # selection of the user in Step 2
                   Customer_ID = as.character(.data[[input$Monthly_ID_col]]),
                   # Creates a new column called "Filter_1" based on the 
                   # selection of the user in Step 9
                   Filter_1 = .data[[input$Filter_Col_1]],
                   # Creates a new column called "Filter_2" based on the 
                   # selection of the user in Step 11
                   Filter_2 = .data[[input$Filter_Col_2]]) %>% 
            select(Accrual_Amount, Date_Accrual, Customer_ID, Filter_1, Filter_2)
    })
    
    # Same as above but for Annual Data
    Clean_Annual_Data <- reactive({
        
        Clean_Annual_Data <- Annual_Data() %>% 
            mutate(Accrual_Amount = parse_number(as.character(.data[[input$Annual_Amount_col]])),
                   Date_Accrual = .data[[input$Annual_Date_col]],
                   Customer_ID = as.character(.data[[input$Annual_ID_col]]),
                   Filter_1 = .data[[input$Filter_Col_1]],
                   Filter_2 = .data[[input$Filter_Col_2]]) %>% 
            select(Accrual_Amount, Date_Accrual, Customer_ID, Filter_1, Filter_2)
    })
    
    # Creates the dynamic selection options for the drop-down 
    # select box in Step 9. Includes all column names from both data sets.
    observe({
        
        updateSelectizeInput(inputId = "Filter_Col_1",
                          choices = flatten_chr(list(colnames(Annual_Data()), colnames(Monthly_Data()))),
                          server = TRUE)
    })
    
    # Creates the dynamic selection options for the drop-down 
    # select box in Step 10 based on the selection in Step 9.
    # Includes all the unique variables from the column selected above
    observe({
        
        updateSelectizeInput(inputId = "Filter_Var_1",
                             choices = flatten_chr(list(unique(Annual_Data()[[input$Filter_Col_1]]),unique(Monthly_Data()[[input$Filter_Col_1]]))),
                             server = TRUE)
        
        })
    
    observe({
        
        updateSelectizeInput(inputId = "Filter_Col_2",
                             choices = flatten_chr(list(colnames(Annual_Data()), colnames(Monthly_Data()))),
                             server = TRUE
        )
    })
    
    observe({
        
        updateSelectizeInput(inputId = "Filter_Var_2",
                             choices = flatten_chr(list(unique(Annual_Data()[[input$Filter_Col_2]]),unique(Monthly_Data()[[input$Filter_Col_2]]))),
                             server = TRUE
                          )
    })
    
    # Combines annual and monthly data and then applies filters
    Select_Data <- reactive({
        
        Merged_Data <- Clean_Annual_Data() %>% 
            bind_rows(Clean_Monthly_Data()) %>%
            filter(Filter_1 %in% input$Filter_Var_1,
                   Filter_2 %in% input$Filter_Var_2)
        
    })
    

    Metrics_Data <- reactive({
        
        MRR <- Select_Data() %>%
            mutate(Year = year(Date_Accrual),
                   Month = month(Date_Accrual)) %>% 
            group_by(Customer_ID, Year, Month) %>% 
            summarise(MRR = sum(Accrual_Amount))
        
        Unique_Names <- Select_Data() %>% 
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