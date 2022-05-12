library(readxl)
library(tidyverse)

#Loading clean data
df <- read_excel("~/Desktop/Thesis/Data/full_datasets/full_data_clean_excel.xlsx")

#Drop Uganda and 2008 Senegal
df <- df %>%
  drop_na(wb_percent_internet) %>%
  drop_na(iiag_overall_gov)

#Adding round column
df <- df %>%
  mutate(round =
           case_when(year == 2008 | year == 2009 ~ "Round 4 (2008 - 2009)",
                     year == 2011 | year == 2012 | year == 2013 ~ "Round 5 (2011 - 2013)",
                     year == 2014 | year == 2015 | year == 2016 ~ "Round 6 (2014 - 2016)",
                     year == 2018 | year == 2019 ~ "Round 7 (2018 - 2019)"
           ))
table(df$round)

#Adding edu_cat column
table(df$edu)
df <- df %>%
  mutate(edu_cat =
           case_when(edu == "Informal schooling only" | edu == "No formal schooling" | edu == "Some primary schooling" |
                       edu == "Primary school completed" ~ "1. Low education",
                     edu == "Some secondary/high school" | edu == "Secondary/high school completed" ~ "2. Medium education",
                     edu == "Post-secondary qualification, other than university" | edu == "Some university" |
                       edu == "University completed" | edu == "Post-graduate" ~ "3. High education"))
table(df$edu_cat)

#Adding spi_health_cat column
table(df$spi_health)
df <- df %>%
  mutate(spi_health_cat =
           case_when(spi_health >= 0 & spi_health <= .667 ~ "1. Low SPI health",
                     spi_health > .667 & spi_health <= .715 ~ "2. Medium SPI health",
                     spi_health > .715 & spi_health <= 1.0 ~ "3. High SPI Health"
           ))
table(df$spi_health_cat)

#Adding spi_edu_cat column
table(df$spi_edu)
df <- df %>%
  mutate(spi_edu_cat =
           case_when(spi_edu >= 0 & spi_edu <= .333 ~ "1. Low SPI education",
                     spi_edu > .333 & spi_edu <= .455 ~ "2. Medium SPI education",
                     spi_edu > .455 & spi_edu <= 1.0 ~ "3. High SPI education"
           ))
table(df$spi_edu_cat)

#Adding spi_bus_cat column
table(df$spi_business)
df <- df %>%
  mutate(spi_bus_cat =
           case_when(spi_business == 0 ~ "1. Low SPI business",
                     spi_business == 0.5 ~ "2. Medium SPI business",
                     spi_business == 1.0 ~ "3. High SPI business"
           ))
table(df$spi_bus_cat)

#Adding freedom_cat
table(df$freedom_house)
df <- df %>%
  mutate(freedom_house_cat =
           case_when(freedom_house >= 0 & freedom_house <= 49 ~ "1. Low FIW",
                     freedom_house > 49 & freedom_house <= 66 ~ "2. Medium FIW",
                     freedom_house > 66 & freedom_house <= 100 ~ "3. High FIW"
           ))
table(df$freedom_house_cat)

#Adding iiag_gov_cat
table(df$iiag_overall_gov)
df <- df %>%
  mutate(iiag_gov_cat =
           case_when(iiag_overall_gov >= 0 & iiag_overall_gov <= 48.7 ~ "1. Low governance",
                     iiag_overall_gov > 48.7 & iiag_overall_gov <= 57 ~ "2. Medium governance",
                     iiag_overall_gov > 57 & iiag_overall_gov <= 100 ~ "3. High governance"
           ))
table(df$iiag_gov_cat)

#Adding internet_cat
table(df$wb_percent_internet)
df <- df %>%
  mutate(internet_cat =
           case_when(wb_percent_internet >= 0 & wb_percent_internet <= 8 ~ "1. Low internet",
                     wb_percent_internet > 8 & wb_percent_internet <= 20 ~ "2. Medium internet",
                     wb_percent_internet > 20 & wb_percent_internet <= 100 ~ "3. High internet"
           ))

table(df$internet_cat)

#Ordering variables
df <- df %>%
  mutate(gov_handling_basic_health = replace(gov_handling_basic_health, gov_handling_basic_health == "Very badly", "1. Very badly")) %>%
  mutate(gov_handling_basic_health = replace(gov_handling_basic_health, gov_handling_basic_health == "Fairly badly", "2. Fairly badly")) %>%
  mutate(gov_handling_basic_health = replace(gov_handling_basic_health, gov_handling_basic_health == "Fairly well", "3. Fairly well")) %>%
  mutate(gov_handling_basic_health = replace(gov_handling_basic_health, gov_handling_basic_health == "Very well", "4. Very well"))

table(df$gov_handling_basic_health)

df <- df %>%
  mutate(gov_handling_edu_needs = replace(gov_handling_edu_needs, gov_handling_edu_needs == "Very badly", "1. Very badly")) %>%
  mutate(gov_handling_edu_needs = replace(gov_handling_edu_needs, gov_handling_edu_needs == "Fairly badly", "2. Fairly badly")) %>%
  mutate(gov_handling_edu_needs = replace(gov_handling_edu_needs, gov_handling_edu_needs == "Fairly well", "3. Fairly well")) %>%
  mutate(gov_handling_edu_needs = replace(gov_handling_edu_needs, gov_handling_edu_needs == "Very well", "4. Very well"))

table(df$gov_handling_edu_needs)

df <- df %>%
  mutate(gov_handling_corruption = replace(gov_handling_corruption, gov_handling_corruption == "Very badly", "1. Very badly")) %>%
  mutate(gov_handling_corruption = replace(gov_handling_corruption, gov_handling_corruption == "Fairly badly", "2. Fairly badly")) %>%
  mutate(gov_handling_corruption = replace(gov_handling_corruption, gov_handling_corruption == "Fairly well", "3. Fairly well")) %>%
  mutate(gov_handling_corruption = replace(gov_handling_corruption, gov_handling_corruption == "Very well", "4. Very well"))

table(df$wb_income)

df <- df %>%
  mutate(present_living_condit = replace(present_living_condit, present_living_condit == "Very bad", "1. Very bad")) %>%
  mutate(present_living_condit = replace(present_living_condit, present_living_condit == "Fairly bad", "2. Fairly bad")) %>%
  mutate(present_living_condit = replace(present_living_condit, present_living_condit == "Neither good nor bad", "3. Neither good nor bad")) %>%
  mutate(present_living_condit = replace(present_living_condit, present_living_condit == "Fairly good", "4. Fairly good")) %>%
  mutate(present_living_condit = replace(present_living_condit, present_living_condit == "Very good", "5. Very good"))

table(df$present_living_condit)

df <- df %>%
  mutate(sat_dem = replace(sat_dem, sat_dem == "The country is not a democracy", "1. The country is not a democracy")) %>%
  mutate(sat_dem = replace(sat_dem, sat_dem == "Not at all satisfied", "2. Not at all satisfied")) %>%
  mutate(sat_dem = replace(sat_dem, sat_dem == "Not very satisfied", "3. Not very satisfied")) %>%
  mutate(sat_dem = replace(sat_dem, sat_dem == "Fairly satisfied", "4. Fairly satisfied")) %>%
  mutate(sat_dem = replace(sat_dem, sat_dem == "Very satisfied", "5. Very satisfied"))

table(df$sat_dem)

#Dropping columns not of interest
df <- df %>%
  select(country, year, round, afdb_aidi_index, age, edu, edu_cat, freedom_house, freedom_house_cat, gender,
         gov_handling_basic_health, gov_handling_edu_needs,
         iiag_overall_gov, iiag_gov_cat, information_business_registry, 
         information_school_budget,
         internet_cat, present_living_condit,sat_dem, spi_business, 
         spi_bus_cat, spi_edu, spi_edu_cat,
         spi_health, spi_health_cat, URBRUR, wb_income, wb_percent_internet)

#Save as csv
write_csv(df, "~/Desktop/Thesis/Data/full_datasets/df.csv")
