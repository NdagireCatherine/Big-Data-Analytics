### NDAGIRE CATHERINE - B27015 ###

library(readr)
library(xts)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
install.packages("tseries")
library(tseries)

#Import the csv file/dataset
IDA <- read_csv("Year2-Sem1/Big_Data_Analytics/ida_credits_to_uganda_09-21-2025 - Copy.csv")

#View the dataset
View(IDA)

### 1. Run a time-series analysis on that explains the trend of World Bank fund disbursement to Uganda (hint: use the variable "Disbursed Amount (US$)" 
#Creating a name for the variable column
Disbursed_Amount <- IDA$`Disbursed Amount (US$)`
date_disbursed <- as.Date(IDA$`Board Approval Date`, format = "%m/%d/%Y")

# Create the xts object
time_series <- xts(Disbursed_Amount, order.by = date_disbursed)


# Plot the time series
ggplot(time_series, aes(x = date_disbursed, y = Disbursed_Amount)) +
  geom_line() +
  labs(
    title = "Disbursed Amount Over Time",
    x = "Date",
    y = "Disbursed Amount (US$)"
  ) +
  theme_minimal()

adf.test(time_series)

### 2.Describe the overall "Credit Status" of Uganda based on the provided dataset.
# Plot a bar plot to show the Credit status
g <- ggplot(IDA, aes(x = `Credit Status`))
g + geom_bar(aes(fill=`Credit Status`), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Credit Status of Uganda") 


### 3. Explain the "Original principal amount" Uganda borrowed from the World Bank, what patterns can you deduce from it?
# Descriptive statistics of Original Principal Amount
summary(IDA$`Original Principal Amount (US$)`)


# Histogram to identify the amount of loans mostly borrowed by Uganda from the World Bank
ggplot(data = IDA, aes(x = `Original Principal Amount (US$)`)) +
  geom_histogram() +
  labs(x="`Original Principal Amount (US$)`", 
       title="Distribution of `Original Principal Amount (US$)`")

# Filtering the top ten projects that have the highest amount of the Original Principal Amount
top_10_projects <- IDA %>%
  # Group the data by `Project Name` to calculate the sum for each project
  group_by(`Project Name`) %>%
  summarise(
    `Total Principal` = sum(`Original Principal Amount (US$)`),
    .groups = 'drop'
  ) %>%
  # Arrange the data in descending order of the total principal
  arrange(desc(`Total Principal`)) %>%
  # Select only the top 10 rows
  slice_head(n = 10)

# Plotting the Original Principal Amount of the top 10 Projects using a bar plot
ggplot(top_10_projects, aes(x = reorder(`Project Name`, `Total Principal`), y = `Total Principal`)) +
  geom_bar(stat = "identity", fill = "green4") +
  labs(
    title = "Top 10 Projects by Original Principal Amount",
    x = "Project Name",
    y = "Total Principal (Millions USD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





