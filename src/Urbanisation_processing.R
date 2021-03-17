# Get the predicted percentage of population 65+ assuming constant fertility
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(openxlsx)

YEARS = c("2020", "2040")

# Load in UN Urbanisation data
file = "rsc/WorldPopulation/WUP2018-F21-Proportion_Urban_Annual.xlsx"
urban.data <- as_tibble(read.xlsx(file, sheet="Data")) 
clean <- urban.data %>%
    slice(10:n())
colnames(clean) <- urban.data[9, ]

# Extraction regions and colums of interest
focus <- clean %>%
    filter(Type == "Country/Area") %>%
    filter(`Reference date (as of 1 July)` %in% YEARS) %>%
    summarise(
        Country = `Region, subregion, country or area *`,
        Year = `Reference date (as of 1 July)`,
        `% 65+` = `65+`
    ) %>%
    mutate(across(Year:`% 65+`, as.numeric))

# Basic plot
p <- ggplot(focus) + 
    geom_line(aes(x=Year, y=`% 65+`, group=Country))

# Upload to drive
ss <- drive_get("Met Office Hackathon 2021/Sheets Testing - Dynamic")
sheet_write(focus, ss, sheet="% Pop 65+")
