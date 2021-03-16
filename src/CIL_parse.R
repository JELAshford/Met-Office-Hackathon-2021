# Read in the data from Ember and investigate
library(tidyverse)
library(openxlsx)


sheets = c(
    "Temp - Summer Average",
    "Temp - Winter Average",
    "Temp - Max >95F",
    "Temp - Min <32F",
    "Damages - Mortality"
)

# Read in the CIL world data
cli.data <- as_tibble(read.xlsx("rsc/CIL_World.xlsx", sheet="Temp - Summer Average"))

# Separate the data from the two models into different tibbles

# Data always starts two rows above the name-row, and there's a one row separator
start_8.5_row <- which(cli.data[,1] == "RCP 8.5")
start_4.5_row <- which(cli.data[,1] == "RCP 4.5")

# Get individual models
rcp_8.5 <- cli.data[(start_8.5_row-2):(start_4.5_row-3),]
rcp_4.5 <- cli.data[(start_4.5_row-2):nrow(cli.data),]

# 
