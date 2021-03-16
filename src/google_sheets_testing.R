library(googlesheets4)
library(googledrive)

# Get the sheet
ss <- drive_get("Met Office Hackathon 2021/Sheets Testing")

# Rewrite data in a sheet
head(iris) %>%
    sheet_write(ss, sheet="iris")