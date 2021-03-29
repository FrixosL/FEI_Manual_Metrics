library(tidyverse)
library(readr)
library(lubridate)

end_date = "2021/03/01"
duration = 24
start_date = ymd(end_date) - months(duration)


Starter_Annual_Flatline <- read_csv("C:/Users/FrixosLarkos/OneDrive - FE International/Hunter.io/Financials/Metrics/Manual Metrics/Current/Annual/Flatline CSVs/Starter Annual - Flatline.csv", 
                                    col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                     Date_Accrual = col_date(format = "%d/%m/%Y")))

Select_Data <- Starter_Annual_Flatline %>% 
  select(customer_email, seller_message, status, Annual.Monthly..Hardcoded., Date_Accrual, Accrual_Amount)

Mutated_Data <- Select_Data %>% 
  group_by(customer_zemail) %>% 
  mutate(Lagged_Amount = lag(Accrual_Amount) - 1,
         Net = Accrual_Amount - Lagged_Amount)


Unique_Data <- Select_Data %>% 
  summarise(emails = unique(customer_email)) %>% 
  mutate(Date_Accrual = ymd(start_date))


Accrual_month <- rep(0:(duration - 1), nrow(Unique_Data))

# Duplicate the data based on input$Period (frequency of transactions)
Computed_Data <- Unique_Data %>% 
  slice(rep(1:n(), each = duration)) %>% 
  # Divide the payment amount by the frequency of transactions
  cbind(Accrual_month) %>%
  # Create new variable "Accrual Date" by spreading costs over the next x months
  mutate(Date_Accrual = Date_Accrual + (Accrual_month*months(1)),
         Accrual_Amount = 0) %>% 
  select(!c(Accrual_month))


# Plan:
# 1) Import flatlined Annual transactions
# 2) Import monthly transactions
# 3) Unify all data into a single data frame
# 4) Create a new dataframe with all unique emails/name
# Duplicate each line to have a line for each month for the period under consideration
# Create an amount variable equal to zero to fill empty months
# 5) Summarise previous consolidated data by email, year and month to calculate MRR and number of clients
# 7) Left_join the two dfs by email and date
# 6) group by email and lag variables to calculate churns, upgrades, downgrades