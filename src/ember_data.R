# Read in the data from Ember and investigate
library(tidyverse)
library(openxlsx)

# Ember data
ember.data <- as_tibble(read.xlsx("rsc/EmberData.xlsx", sheet=2))

# Sort out the data
clean.data <- ember.data %>%
    select(-Region, -Source, -Link)