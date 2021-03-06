# Read in the data from Ember and investigate
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(openxlsx)

extract.medians <- function(model.data, val.name) {
    out <- model.data %>%
        select(Country, contains("median")) %>%
        rename_with(function(x) {str_remove(x, "_median")}, contains("median")) %>%
        gather(`1986`:`2080`, key="Year", value="value")
    colnames(out)[3] <- val.name
    return(out)
}

subtract.1986 <- function(tibble) {
    out <- tibble %>%
        mutate(combo = paste0(Country, Type)) %>%
        mutate(target = Absolute[match(combo, combo[which(Year == "1986")])]) %>%
        mutate(Value = as.numeric(Absolute) - as.numeric(target)) %>%
        select(-combo, -target)
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
all_values <- NULL
# Iterate over the sheets
for (sheet in cil.sheets) {
    # Read in cli data, at this sheet
    cli.data <- as_tibble(read.xlsx("rsc/CIL_World.xlsx", sheet=sheet))
    colnames(cli.data) <- column_names
    # Data always starts two rows above the name-row, with a one-row separator
    start_8.5_row <- which(cli.data[,1] == "RCP 8.5")
    start_4.5_row <- which(cli.data[,1] == "RCP 4.5")
    # Get individual models
    rcp_8.5 <- cli.data[(start_8.5_row+2):(start_4.5_row-3),]
    rcp_4.5 <- cli.data[(start_4.5_row+2):nrow(cli.data),]
    # For each model: extract the medians, reshape, and add to storage
    medians_8.5 <- extract.medians(rcp_8.5, val.name=sheet)
    medians_4.5 <- extract.medians(rcp_4.5, val.name=sheet)
    # Combine
    medians <- bind_rows(
        mutate(medians_8.5, RCP = "RCP85"),
        mutate(medians_4.5, RCP = "RCP45")
    )
    if (is.null(all_8.5)) {
        all_8.5 <- medians_8.5
        all_4.5 <- medians_4.5
        all_values <- medians
    } else {
        all_8.5 <- left_join(all_8.5, medians_8.5, by=c("Country", "Year"))
        all_4.5 <- left_join(all_4.5, medians_4.5, by=c("Country", "Year"))
        all_values <- left_join(all_values, medians, by=c("Country","Year", "RCP"))
    }
}
# Finish Tidy - gather by data type
final_8.5 <- gather(all_8.5, -c(Country, Year), key="Type", value="Absolute")
final_4.5 <- gather(all_4.5, -c(Country, Year), key="Type", value="Absolute")
final_values <- gather(all_values, -c(Country, Year, RCP), key="Type", value="Absolute")

# Subtract matching 1986 value
final_8.5 <- subtract.1986(final_8.5)
final_4.5 <- subtract.1986(final_4.5)
final_values <- subtract.1986(final_values)

# Upload the data
ss <- drive_get("Met Office Hackathon 2021/Sheets Testing - Dynamic")
sheet_write(final_8.5, ss, sheet="RCP 8.5")
sheet_write(final_4.5, ss, sheet="RCP 4.5")
sheet_write(final_values, ss, sheet="Combined RCP")
