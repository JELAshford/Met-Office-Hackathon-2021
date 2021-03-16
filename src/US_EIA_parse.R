# Read in the data from Ember and investigate
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(openxlsx)

extract.sources <- function(s) {
    # Get data for this country
    country.data <- eia.data[s:(s+13),]
    colnames(country.data) <- c("Type", 1980:2019)
    # Keep only the columns that are of interest
    out <- country.data %>%
        filter(str_detect(Type, REQUEST)) %>%
        mutate(Type = str_remove_all(Type, "    ")) %>%
        mutate(Country = country.data$Type[1]) %>%
        relocate(Country)
}

FILE = "rsc/US_EIA_Capacity.xlsx"
REQUEST = "Capacity" #"Geothermal|Nuclear|Fossil fuels"

# Read in the file
eia.sheets <- getSheetNames(FILE)
eia.data <- as_tibble(read.xlsx(FILE, sheet=eia.sheets[1])) %>%
    select(-US_EIA_Capacity)

# First filter: remove filler lines and expand first column
start <- which(!str_detect(eia.data$X2, "    "))
wanted_data <- bind_rows(map(start, extract.sources))

# Make a sheet
ss <- drive_get("Met Office Hackathon 2021/Sheets Testing - Dynamic")
sheet_write(wanted_data, ss, sheet="EIA_Total")
