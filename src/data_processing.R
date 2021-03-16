# Initial data processing and loading testing
library(tidyverse)
library(openxlsx) # read.xlsx(, sheet = 1, startRow=, ) https://www.rdocumentation.org/packages/openxlsx/versions/4.2.3/topics/read.xlsx

# Goals of interest
# goal = "Energy intensity level of primary energy (MJ/$2011 PPP GDP)"
goal = "Access to electricity (% of population)"
year = "1990"

# Load in SGD data and visualise
data <- read_csv("rsc/SDG_csv/SDGData.csv") %>%
    select(-X35)

# Get data for this goal
ind.names <- unique(data$`Indicator Name`)
goal.data <- filter(data, `Indicator Name` == goal) %>%
    select(-`Indicator Name`, -`Indicator Code`) %>%
    gather(`1990`:`1992`, key="year", value="value") %>%
    select(-`Country Code`)


# Make world plot
world <- map_data("world")
world.map <- ggplot() +
    geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color="#2b2b2b", fill=NA, size=0.15) + 
    geom_map(
        data = goal.data, map = world,
        aes(fill = value, map_id=`Country Name`)) +
    facet_wrap(year ~ .) + 
    theme_minimal() + 
    ggtitle(goal) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())