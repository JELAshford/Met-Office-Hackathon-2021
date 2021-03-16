# Read in the data from Ember and investigate
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(openxlsx)

extract.medians <- function(model.data) {
    model.data %>%
        select(Country, contains("median")) %>%
        rename_with(function(x) {paste0(x, "_", sheet)}, contains("median"))
}

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

cil.sheets = getSheetNames("rsc/CIL_World.xlsx")

# Storage
all_8.5 <- NULL 
all_4.5 <- NULL
# Iterate over the sheets
for (sheet in cil.sheets) {
    # Read in data for this sheet
    cli.data <- as_tibble(read.xlsx("rsc/CIL_World.xlsx", sheet=sheet))
    colnames(cli.data) <- column_names
    # Data always starts two rows above the name-row, and there's a one row separator
    start_8.5_row <- which(cli.data[,1] == "RCP 8.5")
    start_4.5_row <- which(cli.data[,1] == "RCP 4.5")
    # Get individual models
    rcp_8.5 <- cli.data[(start_8.5_row+2):(start_4.5_row-3),]
    rcp_4.5 <- cli.data[(start_4.5_row+2):nrow(cli.data),]
    # For each model: extract the medians, rename, and add to storage
    if (is.null(all_8.5)) {
        all_8.5 <- extract.medians(rcp_8.5)
        all_4.5 <- extract.medians(rcp_4.5)
    } else {
        all_8.5 <- left_join(all_8.5, extract.medians(rcp_8.5), by="Country")
        all_4.5 <- left_join(all_4.5, extract.medians(rcp_4.5), by="Country")
    }
}

# Upload the data
ss <- drive_get("Met Office Hackathon 2021/Sheets Testing - Dynamic")
# Rewrite data in a sheet
sheet_write(all_8.5, ss, sheet="RCP 8.5")
sheet_write(all_4.5, ss, sheet="RCP 4.5")