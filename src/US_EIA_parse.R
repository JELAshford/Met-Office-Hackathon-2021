# Read in the data from Ember and investigate
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(openxlsx)

FILE = "rsc/US_EIA_Capacity.xlsx"

# Read in the file
eia.sheets <- getSheetNames(FILE)
eia.data <- as_tibble(read.xlsx(FILE, sheet=eia.sheets[1])) %>%
    select(-US_EIA_Capacity)

# First filter: remove filler lines and expand first column
start <- which(!str_detect(eia.data$X2, "    "))
for (s in start) {
    country.data <- eia.data[s:(s+13),]
    print(this)    
}


out <- eia.data %>% 
    filter(!str_detect(X2, "    "))

    filter(str_detect(X2, "Geothermal|Nuclear|Fossil fuels")) %>%
    mutate(X2 = str_remove_all(X2, "    "))
