# Get the predicted percentage of population 65+ assuming constant fertility
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(openxlsx)

YEARS = c("1986", "2020", "2040")

# Load in UN Urbanisation data
file = "rsc/WorldPopulation/WUP2018-F21-Proportion_Urban_Annual.xlsx"
urban.data <- as_tibble(read.xlsx(file, sheet="Data")) 
clean <- urban.data %>%
    slice(10:n())
colnames(clean) <- urban.data[9, ]

# Read in row indexes of countries
country_indexes = read_csv("rsc/WorldPopulation/urban_indexes.csv")

# Extraction regions and colums of interest
focus <- clean %>%
    filter(Index %in% unlist(country_indexes)) %>%
    mutate(Country = `Region, subregion, country or area`) %>%
    select(Country, YEARS) %>%
    gather(YEARS, key="Year", value="% Urbanised") %>%
    mutate(`% Urbanised` = round(`% Urbanised`, 3))


# Basic plot
p <- ggplot(focus) + 
    geom_line(aes(x=Year, y=`% Urbanised`, group=Country))

# Upload to drive
ss <- drive_get("Met Office Hackathon 2021/Sheets Testing - Dynamic")
sheet_write(focus, ss, sheet="% Urbanised")
