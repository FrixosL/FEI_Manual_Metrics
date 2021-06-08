# Load packages
library(tidyverse)
library(readr)
library(lubridate)

# Set variables
end_date = "2022/03/01"
duration = 60
start_date = ymd(end_date) - months(duration)

# Load Annual data
Annual_Data <- read_csv("C:/Users/FrixosLarkos/Downloads/Annual Plans vMay.csv",
                        col_types = cols(Date = col_date(format = "%d/%m/%Y"),
                                         Date_Accrual = col_date(format = "%d/%m/%Y"))) %>% 
  select(customer_email, seller_message, status, Period, Date_Accrual, Accrual_Amount, Plan)

# Load Monthly data
Monthly_Data <- read_csv("C:/Users/FrixosLarkos/Downloads/Monthly Plans vMay.csv",
                         col_types = cols(date = col_date(format = "%d/%m/%Y"))) %>%
  mutate(Date_Accrual = ymd(paste0(year(date),"-",month(date),"-",1))) %>% 
  select(customer_email, seller_message, status, Period, Date_Accrual, Accrual_Amount=net_converted_amount, Plan)


# Merge Annual and Monthly data
Merged_Data <- Annual_Data %>% 
  bind_rows(Monthly_Data)

# Select relevant variables only (for simplification)
Select_Data <- Merged_Data %>% 
  filter(status == "Paid",
         Period == "Monthly",
         Plan == "Enterprise")

# Mutate data into appropriate format to calculate the desired metrics
MRR <- Select_Data %>%
 mutate(Year = year(Date_Accrual),
        Month = month(Date_Accrual)) %>% 
 group_by(customer_email, Year, Month) %>% 
 summarise(MRR = sum(Accrual_Amount))

#Churn_MRR <- Select_Data %>%
# group_by(customer_email)
# mutate(Lag_Accrual_Amount = lag(Accrual_Amount),
#        Churn_Amount = if_else(Accrual_Amount == 0, Lag_Accrual_Amount - Accrual_month,0)) %>% 
# group_by(year(Date_Accrual), month(Date_Accrual)) %>% 
# summarise(MRR = sum(Churn_Amount))


# Create an empty dataframe with the appropriate names and dates to join mutated data into
Unique_Names <- Select_Data %>% 
  summarise(emails = unique(customer_email)) %>% 
  mutate(Date_Accrual = ymd(end_date))

Accrual_month <- rep(0:(duration - 1), nrow(Unique_Names))

# Duplicate the data based on input$Period (frequency of transactions)
Expanded_Data <- Unique_Names %>% 
  slice(rep(1:n(), each = duration)) %>% 
  # Divide the payment amount by the frequency of transactions
  cbind(Accrual_month) %>%
  # Create new variable "Accrual Date" by spreading costs over the next x months
  mutate(Date_Accrual = Date_Accrual - (Accrual_month*months(1)),
         Year = year(Date_Accrual),
         Month = month(Date_Accrual)) %>% 
  select(!c(Accrual_month, Date_Accrual))

Joint_Data <- Expanded_Data %>% 
  left_join(MRR, 
            by = c("emails"="customer_email","Year", "Month")) %>% 
  mutate(MRR = if_else(is.na(MRR), 0, MRR)) %>% 
  group_by(emails) %>%
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

write.csv(Joint_Data, "Monthly_Enterprise.csv", row.names = FALSE)

# Plan:
# Custom, Enterprise, Total
# 
# 1) Import flatlined Annual transactions
# 2) Import monthly transactions
# 3) Unify all data into a single data frame
# 4) Create a new dataframe with all unique emails/name
# Duplicate each line to have a line for each month for the period under consideration
# Create an amount variable equal to zero to fill empty months
# 5) Summarise previous consolidated data by email, year and month to calculate MRR and number of clients
# 7) Left_join the two dfs by email and date
# 6) group by email and lag variables to calculate churns, upgrades, downgrades