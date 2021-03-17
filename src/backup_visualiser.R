# Read in data from file
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(openxlsx)
library(plotly)

# Get the data from file
ss <- drive_get("Met Office Hackathon 2021/Sheets Testing v0")
o <- read_sheet(ss, sheet="merged")

