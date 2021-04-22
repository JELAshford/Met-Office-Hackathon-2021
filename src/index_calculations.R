# Calculate a risk to power systems from heat event index 
# team Hot Stuff for the MetOffice Climate data hackathon

library(tidyverse)
library(lubridate)
library(readxl)
library(googlesheets4)
library(googledrive)

###############################################################################
# we used 2 sheets for data, 
# 1 dynamic that got updated constantly with new data, to read in from
# 1 static that was output to, that the tableau is based on

#dynamic 
sheet <- "152QIeAWqiv1uvQkk67G211m-GS9Wvmb1JedVngFsIPs"
dynamic_data <- drive_get(id = sheet) 
input_data <- googlesheets4::read_sheet(dynamic_data,sheet = "RCP 4.5")
clean_input <- input_data %>% mutate(Value = as.numeric(Value)) %>%
  pivot_wider(names_from = Type,values_from = Value)%>%
  mutate(Year = as.numeric(Year))

#static
sheet <- "1ev0xA8pSKBMj77ufOBwppfJWWba26nMmBn5BgMsokCA"
static_data <- drive_get(id = sheet) 
final_data <- googlesheets4::read_sheet(static_data,sheet = "merged")
#clean_google_drive <- final_data %>% mutate(Value = as.numeric(Value)) #%>% pivot_wider(names_from = Type,values_from = Value)

###############################################################################
# GEt DATA INPUTS: 
###############################################################################

#age data
age_data <- googlesheets4::read_sheet(dynamic_data,sheet = "% Pop 65+")
clean_age <- age_data %>% mutate(`% 65+` = as.numeric(`% 65+`))#final_data %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Type,values_from = Value)
#urbanisation data
urbanisation_data <- googlesheets4::read_sheet(dynamic_data,sheet="% Urbanised")
clean_urban <- urbanisation_data %>% mutate(`% Urbanised` = as.numeric(`% Urbanised`),Year = as.numeric(Year))
#electricity generation
generation_data <- read_excel("ger.xlsx",sheet=2)
generation_2019 <- electric_data %>% filter(Year ==2019) 
clean_generation <- generation_2019 %>% select(Country,Variable,`Value (TWh)`) %>%
  pivot_wider(names_from = Variable,values_from = `Value (TWh)`) %>%
  mutate(Coal = Coal / Demand,
         Hydro = Hydro / Demand,
         Gas = Gas / Demand,
         `Other fossil` = `Other fossil` / Demand,
         Wind = Wind / Demand,
         Solar = Solar / Demand,
         Nuclear = Nuclear / Demand,
         `Other renewables` =`Other renewables`/Demand,
        `Net imports` = `Net imports`/Demand,
        `Biomass and waste` = `Biomass and waste` / Demand)
#temperature data
temperature_data_files <- c("maxav_2020.csv","maxav_2040.csv","maxav_2060.csv")
temp_data <- tibble()
for (file in temperature_data_files) {
  temp_data <- rbind(data,read_csv(file))
}
clean_temp <- temp_data %>% group_by(Year,Country,Statistics) %>%
  summarise(max_monthly_temp = mean(`Monthly Max-Temperature - (Celsius)`,rm.na=T)) %>%
  ungroup() %>% group_by(Year,Country) %>%
  mutate(max_yearly_temp = max(max_monthly_temp),
         Year = case_when(Year =="2020-2039" ~ 2020,
                          Year == "2040-2059" ~ 2040,
                          Year == "2060-2079" ~ 2060)) %>% 
  select(Country,Year,max_yearly_temp) %>% unique()
#capacity data
capacity_data <- googlesheets4::read_sheet(online_data,sheet = "EIA") 
clean_capacity <- capacity_data %>%
  select(Country,Type,`2018`) %>%
  mutate(Year = 2018)%>%
  rename(Value = "2018") %>%
  pivot_wider(names_from = Type,values_from = Value) %>% select(-Year) %>%
  mutate(Country = str_replace(Country,"United States","United States of America"))
#max capacity for peak demand
peak_data <- googlesheets4::read_sheet(dynamic_data,sheet = "EIA_Total") 
clean_peak <- peak_data %>%
  select(Country,Type,`2018`) %>%
  mutate(Year = 2018) %>%
  rename(Value = "2018") %>%
  mutate(Value = str_replace(Value,"--","0"),
         Value = as.numeric(Value)) %>%
  pivot_wider(names_from = Type,values_from = Value) %>% 
  select(-Year)%>%
  mutate(Country = str_replace(Country,"United States","United States of America"))
#access to electricity data
access <- read_excel("Goal7.xlsx")
clean_access <- access %>% filter(TimePeriod == 2017) %>% 
  select(GeoAreaName,Value, Location) %>%
  pivot_wider(names_from = Location, values_from = Value) %>%
  rename(Country = "GeoAreaName") %>%
  select(Country, RURAL) %>%
  mutate(RURAL = as.numeric(RURAL),
         Country = str_replace(Country,"United Kingdom of Great Britain and Northern Ireland","United Kingdom"))

###############################################################################
# JOIN UP AND CALCULATE INDEX FROM DATA
###############################################################################

# join all data together
clean <- left_join(clean_age,clean_capacity,all=T) %>%
  left_join(.,clean_peak,all=T) %>%
  left_join(.,clean_urban,all=T,by=c("Country","Year")) %>%
  left_join(.,clean_access,all=T,by=c("Country"))
alltogether <- left_join(clean,clean_input,all=T,by=c("Country","Year")) 
# select the parts we are interested in
want <- alltogether %>% 
  select(Country, 
         Year,
         `Fossil fuels (million kW)`,
         `Capacity (million kW)`,
         `Temp - Summer Average`,
         `Temp - Max more 95F`,
         `% 65+`,
         `% Urbanised`,
         RURAL
         ) %>%
  mutate(#wrangling into numeric nice formats
         `Fossil fuels (million kW)`= str_replace(`Fossil fuels (million kW)`,"--","0"),
         `Fossil fuels (million kW)`= as.numeric(`Fossil fuels (million kW)`),
         `Nuclear (million kW)`= str_replace(`Fossil fuels (million kW)`,"--","0"),
         `Nuclear (million kW)`= as.numeric(`Fossil fuels (million kW)`),
         `Capacity (million kW)`= str_replace(`Capacity (million kW)`,"--","0"),
         `Capacity (million kW)`= as.numeric(`Capacity (million kW)`),
         #Farenheit to celcius
         `Temp - Summer Average` = ((as.numeric(`Temp - Summer Average`)-32)*(5/9)),
         #all thermal generation types = fossil + nuclear
         `Thermal fuels (million kW)` = `Nuclear (million kW)` + `Fossil fuels (million kW)`,
         #split country type
         country_type = case_when(`Temp - Summer Average`<18 ~"Cold",
                                  T~"Hot"),
         #temperatures
         heatwave_temperature = `Temp - Summer Average`+10, #summer mean + 15 for heatwave temperature
         heatwave_temp_above35 = heatwave_temperature-35,   # degrees by which heatwave is above 35
         heatwave_temp_above25 = heatwave_temperature-25,   # degrees heatwave is above 25 for cooling demand scale
         heatwave_temp_above15 = heatwave_temperature-15,   # degrees heatwave is above 15 for thermal efficiency scale
         #scale factors 
         demand_multiplier = 0.006,#0.006,
         efficiency_multiplier = 0.003,#0.003,
         transmission_multiplier = 0.004,#0.0039,
         # resulting change = scale factors * degree above 0 point * total capacity
         change_demand = (heatwave_temp_above25+(2*(`% Urbanised`/100)))*demand_multiplier*`Capacity (million kW)`,
         change_thermal = heatwave_temp_above15*efficiency_multiplier*`Thermal fuels (million kW)`,
         change_trans = heatwave_temperature* transmission_multiplier*`Capacity (million kW)`*(1.1-(`% Urbanised`/100))*(RURAL/100),
         #final values after modification for sense checking
         final_demand = `Capacity (million kW)` + change_demand,
         final_thermal = `Thermal fuels (million kW)` - change_thermal,
         final_transmission = `Capacity (million kW)`- change_trans,
         #total change is all changes added together
         change = change_demand + change_thermal+change_trans,
         change_as_percent = change/`Capacity (million kW)`)

###############################################################################
# FORMAT AND CLEAN OUTPUTS: 
###############################################################################

# get indeces we are interested in displaying for the viz
look <- want %>% 
  mutate(population_risk = ((`% 65+`/100)* change_as_percent)*100,
         change_as_percent = change_as_percent*100,
         change_demand_aspercent = (change_demand /`Capacity (million kW)`)*100,
         change_thermal_aspercent = (change_thermal / `Capacity (million kW)`)*100,
         change_tran_aspercent = (change_trans / `Capacity (million kW)`)*100) %>%
  select(Country,Year,change_as_percent,`Temp - Max more 95F`,change_demand_aspercent,
         change_thermal_aspercent,change_tran_aspercent,`% 65+`,population_risk) 

# mutate into a tableau suitable format
for_tableau <- look %>%
  pivot_longer(cols = c(change_as_percent,`Temp - Max more 95F`,
                        change_demand_aspercent,change_thermal_aspercent,change_tran_aspercent,population_risk),
               names_to = "Type",values_to = "Value") %>%
  mutate(RCP = "RCP45",
         Absolute = NA,
         Type = case_when(Type == "change_as_percent"~ "Potential Supply-Demand gap (% of Total Capacity)",
                          Type == "change_demand_aspercent" ~ "Demand increase (% of Total Capacity)",
                          Type == "change_thermal_aspercent" ~"Production decrease (% of Total Capacity)",
                          Type == "change_tran_aspercent" ~ "Transmission decrease (% of Total Capacity)",
                          Type == "Temp - Max more 95F" ~ "median n days over 35 degrees per year",
                          Type == "population_risk" ~ "Risk to Population",
                          T ~ Type)) %>%
  filter(!is.na(Year) & !is.na(Value), !(Year %in% c(2080,1986)))

###########################################################
# Write outputs
###########################################################

towrite <- rbind(final_data,for_tableau) %>%
  filter(!(Type %in% c("Temp - Winter Average","Temp - Min less 32F","Damages - Mortality")))

googlesheets4::sheet_write(for_tableau, static_data,sheet = "merged")

