setwd("D:/OneDrive/Postdoctor/Project")
library(arrow)
library(dplyr)
library(tidyverse)

#### read data ####
## data 1 cpue
cpue_data = read.csv("Data/Final_data/all_state_cpue_6Feb24.csv")
## data 2 recreational catch
mn_state_creel = read.csv("Data/Final_data/creel/MN_Output.csv")
wi_state_creel = read.csv("Data/Final_data/creel/WI_Output.csv")
sd_state_creel = read.csv("Data/Final_data/creel/SD_Output.csv")
rec_catch_data = rbind(mn_state_creel,wi_state_creel,sd_state_creel)
## data 3 environment 
env_data = read_feather("Data/Final_data/lake_temperature_metrics_GLM_NLDAS.feather")
#env_data = read.csv("Data/Final_data/annual_metrics_glm3_pb0nldas.csv")
## stocking and tribal catch 
stocking_tribal_catch_wi = read.csv("Data/Final_data/tribal_stocking/WI.csv")
stocking_tribal_catch_sd = read.csv("Data/Final_data/tribal_stocking/SD.csv")
stocking_tribal_catch_mn = read.csv("Data/Final_data/tribal_stocking/MN.csv")
stocking_tribal_catch = rbind(stocking_tribal_catch_mn ,stocking_tribal_catch_wi ,stocking_tribal_catch_sd )
stocking_tribal_catch = stocking_tribal_catch[!is.na(stocking_tribal_catch$site_id),]

#### data processing ####
## recreational catch convert length(inch) to length(cm) and to weight(kg)
rec_catch_data$length_cm = rec_catch_data$Mean_Length_Harvest*2.54

species_params <- data.frame(
  Common = c("Northern Pike", "Bluegill", "Smallmouth Bass", "Largemouth Bass", "Yellow Perch", "Black Crappie", "Cisco", "Walleye"),
  a = c(0.00447, 0.01413, 0.01096 , 0.01047, 0.01175, 0.01072 , 0.00575, 0.00631), 
  b = c(3.08, 3.13, 3.05, 3.08, 3.05, 3.06, 3.15, 3.15)  
)

rec_catch_data <- rec_catch_data %>%
  left_join(species_params, by = "Common") %>%
  mutate(
    estimated_weight_kg = a * length_cm^b/1000 * Annual_Harvest,  # Apply the length-weight formula
    estimated_weight_per_fish_kg = a * length_cm^b/1000
  )

## calcualte annual cpue for each lake-species and sample method
# Convert the date column to Date type and extract the year
cpue_data <- cpue_data %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         year = format(date, "%Y"))

# Group by nhdhr_id, species_1, sampling_method, and year and summarize to calculate the sum of total_effort_1 and count for each group
annual_cpue <- cpue_data %>%
  group_by(nhdhr_id, species_1, sampling_method, year) %>%
  summarize(
    annual_total_effort_1 = sum(total_effort_1, na.rm = TRUE),
    annual_count = sum(count, na.rm = TRUE),
    total_effort_1_units = first(na.omit(total_effort_1_units)),
    mean_cpue_from_average_cpue = mean(cpue),
    .groups = 'drop'  # This option removes the grouping structure from the resulting tibble
  )

annual_cpue$mean_cpue_from_sum = annual_cpue$annual_count/annual_cpue$annual_total_effort_1

# how many years have data for each lake species and sampling method
annual_cpue = annual_cpue[annual_cpue$mean_cpue_from_sum>0,]
annual_cpue = annual_cpue[annual_cpue$year>1979,]
annual_cpue <- annual_cpue %>%
  group_by(nhdhr_id, species_1, sampling_method) %>%
  mutate(years_with_data_count = n_distinct(year)) %>%
  ungroup()  # Remove the grouping structure

# remove an site_id
annual_cpue = annual_cpue[complete.cases(annual_cpue),]

# only include the data with more than 4 years survey
annual_cpue = annual_cpue[annual_cpue$years_with_data_count>3,]

annual_cpue <- annual_cpue %>%
  mutate(species = case_when(
    species_1 == "largemouth_bass" ~ "Largemouth Bass",
    species_1 == "smallmouth_bass" ~ "Smallmouth Bass",
    species_1 == "black_crappie"   ~ "Black Crappie",
    species_1 == "bluegill"        ~ "Bluegill",
    species_1 == "walleye"         ~ "Walleye",
    species_1 == "northern_pike"   ~ "Northern Pike",
    species_1 == "yellow_perch"    ~ "Yellow Perch",
    species_1 == "cisco"           ~ "Cisco",
    TRUE                            ~ species_1  # Default case to handle any unspecified species
  ))
annual_cpue$stock_id = paste0(annual_cpue$nhdhr_id,annual_cpue$species)

# how many sampling methods are there for each stock_id 
# Count unique sampling methods for each stock_id
sampling_methods_per_stock <- annual_cpue %>%
  group_by(stock_id) %>%
  summarise(Unique_Sampling_Methods = n_distinct(sampling_method), .groups = 'drop')
#View(sampling_methods_per_stock)

# only include the data with the highest survey years
select_sampling_method_result <- annual_cpue %>%
  group_by(stock_id) %>%
  arrange(stock_id, desc(years_with_data_count)) %>%  # Order by years_with_data_count descending
  slice(1) %>%  # Take the first row in each group, which has the highest years_with_data_count
  select(stock_id, sampling_method, years_with_data_count)  # Select only the relevant columns

select_sampling_method_result$stock_id_sampling_method = paste0(select_sampling_method_result$stock_id,select_sampling_method_result$sampling_method)
annual_cpue$stock_id_sampling_method = paste0(annual_cpue$stock_id,annual_cpue$sampling_method)

annual_cpue = annual_cpue[annual_cpue$stock_id_sampling_method%in%select_sampling_method_result$stock_id_sampling_method,]

## merge data
# make the values in different data frame consistent
rec_catch_data <- rec_catch_data %>%
  mutate(site_id = sub("nhdhr_", "", site_id))

rec_catch_data <- rec_catch_data %>%
  mutate(Year = as.character(Year))

# calculate the survey number 
rec_catch_data <- rec_catch_data %>%
  filter(!is.na(estimated_weight_kg))

rec_catch_data <- rec_catch_data %>%
  filter(estimated_weight_kg>0)

rec_catch_data <- rec_catch_data %>%
  group_by(site_id, Taxa) %>%
  mutate(num_of_year = n_distinct(Year[!is.na(estimated_weight_kg)])) %>%
  ungroup()

# "Combinations with more than 1 num_of_year: 1642"
rec_catch_data = rec_catch_data[rec_catch_data$num_of_year>1,]

# merge the recreational catch and the cpue
rec_catch_data$stock_id =  paste0(rec_catch_data$site_id,rec_catch_data$Common)

rec_catch_data <- rec_catch_data %>%
  group_by(stock_id, Year) %>%
  mutate(total_estimated_weight_kg = sum(estimated_weight_kg, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(stock_id, Year, .keep_all = TRUE) %>%
  mutate(estimated_weight_kg = total_estimated_weight_kg) %>%
  select(-total_estimated_weight_kg)

# only include the qualified stock_id
qualified_stock_id = unique(rec_catch_data$stock_id)[unique(rec_catch_data$stock_id)%in%annual_cpue$stock_id]

# Create a new data frame with expanded stock_id and Year
new_data_frame <- data.frame(stock_id = rep(qualified_stock_id, each=length(1980:2021)))
new_data_frame$Year <- rep(1980:2021, length(qualified_stock_id))

new_data_frame$Year = as.character(new_data_frame$Year)

new_data_frame <- left_join(new_data_frame,unique(rec_catch_data[,c("stock_id","site_id")]),"stock_id")
new_data_frame <- left_join(new_data_frame,unique(rec_catch_data[,c("stock_id","Common")]),"stock_id")
new_data_frame <- left_join(new_data_frame,unique(rec_catch_data[,c("stock_id","num_of_year")]),"stock_id")

rec_catch_data <- left_join(new_data_frame,rec_catch_data[,c("stock_id","Year","estimated_weight_kg")],by=c("stock_id","Year"))

cpue_rec_catch <- left_join(rec_catch_data,unique(annual_cpue[,c("stock_id","years_with_data_count","sampling_method","total_effort_1_units")]),"stock_id")
cpue_rec_catch <- left_join(cpue_rec_catch, annual_cpue[,c("mean_cpue_from_sum","stock_id","year")], by = c("stock_id", "Year"="year"))

# merge the GDDs
env_data <- env_data %>%
  mutate(site_id = sub("nhdhr_", "", site_id))
env_data = env_data[,c("site_id","year","gdd_wtr_5c")]
env_data$year <- as.character(env_data$year)

# include the qualified site_id

cpue_rec_catch_env = left_join(cpue_rec_catch, env_data, by = c("site_id" , "Year"="year"))

# merge the tribal harvest and stocking
stocking_tribal_catch <- stocking_tribal_catch %>%
  mutate(site_id = sub("nhdhr_", "", site_id))

stocking_tribal_catch <- stocking_tribal_catch %>%
  mutate(species = case_when(
    species == "LB" ~ "Largemouth Bass",
    species == "SB" ~ "Smallmouth Bass",
    species == "BC"   ~ "Black Crappie",
    species == "BG"        ~ "Bluegill",
    species == "WE"         ~ "Walleye",
    species == "NP"   ~ "Northern Pike",
    species == "YP"    ~ "Yellow Perch",
    species == "CC"           ~ "Cisco",
    TRUE                            ~ species  # Default case to handle any unspecified species
  ))

stocking_tribal_catch$stock_id = paste0(stocking_tribal_catch$site_id,stocking_tribal_catch$species)

stocking_tribal_catch <- stocking_tribal_catch[,c("tri_harvest","stocking_weight","year","stock_id")]
stocking_tribal_catch$year = as.character(stocking_tribal_catch$year)
stocking_tribal_catch = unique(stocking_tribal_catch)
stocking_tribal_catch_df <- stocking_tribal_catch %>%
  group_by(year, stock_id) %>%
  mutate(
    total_tri_harvest = sum(tri_harvest, na.rm = TRUE),
    total_stocking_weight = sum(stocking_weight, na.rm = TRUE)
  ) %>%
  ungroup()
stocking_tribal_catch_df <- select(stocking_tribal_catch_df, -tri_harvest, -stocking_weight)
stocking_tribal_catch_df = unique(stocking_tribal_catch_df)

all_data =  left_join(cpue_rec_catch_env, stocking_tribal_catch_df , by = c("stock_id" , "Year"="year"))
names(all_data)[c(12,13)] = c("tri_harvest","stocking_weight")

## data save
write.csv(all_data,"Data/Final_data/all_processed_spm_input_data.csv",row.names = FALSE)
