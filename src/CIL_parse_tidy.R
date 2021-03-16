# Read in the data from Ember and investigate
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(openxlsx)

extract.medians <- function(model.data, val.name) {
    out <- model.data %>%
        select(Country, contains("median")) %>%
        rename_with(function(x) {str_remove(x, "_median")}, contains("median")) %>%
        gather(`1986`:`2080`, key="year", value="value")
    colnames(out)[3] <- val.name
    return(out)
}

combine.clean.countries <- function(data.list) {
    data.list %>%
        reduce(full_join, by=c("Country", "year")) %>%
        select(Country...1, contains("median")) %>%
        rename(Country = Country...1)
}

cil.sheets = c(
    "Temp - Summer Average",
    "Temp - Winter Average",
    "Temp - Max >95F",
    "Temp - Min <32F",
    "Damages - Mortality"
)

column_names = c(
    "Country",
    "1986_median",
    "2020_lower20th",
    "2020_median",
    "2020_upper20th",
    "2040_lower20th",
    "2040_median",
    "2040_upper20th",
    "2080_lower20th",
    "2080_median",
    "2080_upper20th"
)

# Read in the CIL world data


# Storage
all_8.5 <- list()
all_4.5 <- list()
# Iterate over the sheets
# sheet = cil.sheets[1]
for (sheet in cil.sheets) {

    # Read in cli data, at this sheet
    cli.data <- as_tibble(read.xlsx("rsc/CIL_World.xlsx", sheet=sheet))
    colnames(cli.data) <- column_names

    # Data always starts two rows above the name-row, and there's a one row separator
    start_8.5_row <- which(cli.data[,1] == "RCP 8.5")
    start_4.5_row <- which(cli.data[,1] == "RCP 4.5")
    # Get individual models
    rcp_8.5 <- cli.data[(start_8.5_row+2):(start_4.5_row-3),]
    rcp_4.5 <- cli.data[(start_4.5_row+2):nrow(cli.data),]


    # For each model: extract the medians, rename, and add to storage
    all_8.5 <- c(all_8.5, extract.medians(rcp_8.5, val.name=sheet))
    all_4.5 <- c(all_4.5, extract.medians(rcp_4.5, val.name=sheet))
}

all_8.5 %>%
    reduce(full_join, by=c(Country, year))


# # Combine all the data, remove duplicate countries, and gather
# final_8.5 <- combine.clean.countries(all_8.5)
# final_4.5 <- combine.clean.countries(all_4.5)

# # Upload the data
# ss <- drive_get("Met Office Hackathon 2021/Sheets Testing - Dynamic")
# # Rewrite data in a sheet
# final_8.5 %>%
#     sheet_write(ss, sheet="RCP 8.5")
# final_4.5 %>%
#     sheet_write(ss, sheet="RCP 4.5")