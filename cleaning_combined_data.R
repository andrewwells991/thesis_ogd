library(tidyverse)
library(readxl)

#Loading afrobar data
full_data <- read_excel("Desktop/Thesis/Data/Combined data/full_data.xlsx")


#Label ordinal/nominal data
full_data$URBRUR <- factor(full_data$URBRUR,
                    levels = c(1,2),
                    labels = c("Urban", "Rural"))

full_data$country_direction <- factor(full_data$country_direction,
                               levels = c(1,2),
                               labels = c("Going in the wrong direction", "Going in the right direction"))

full_data$country_econ_condit <- factor(full_data$country_econ_condit,
                                 levels = c(1,2,3,4,5),
                                 labels = c("Very bad", "Fairly bad", "Neither good nor bad",
                                            "Fairly good", "Very good"))

full_data$present_living_condit <- factor(full_data$present_living_condit,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Very bad", "Fairly bad", "Neither good nor bad",
                                              "Fairly good", "Very good"))

full_data$living_condit_v_oth <- factor(full_data$living_condit_v_oth,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Very bad", "Fairly bad", "Neither good nor bad",
                                              "Fairly good", "Very good"))

full_data$radio_news <- factor(full_data$radio_news,
                                 levels = c(0,1,2,3,4),
                                 labels = c("Never", "Less than once a month", "A few times a month",
                                            "A few times a week", "Everyday"))

full_data$tv_news <- factor(full_data$tv_news,
                        levels = c(0,1,2,3,4),
                        labels = c("Never", "Less than once a month", "A few times a month",
                                   "A few times a week", "Everyday"))

full_data$newspaper_news <- factor(full_data$newspaper_news,
                        levels = c(0,1,2,3,4),
                        labels = c("Never", "Less than once a month", "A few times a month",
                                   "A few times a week", "Everyday"))

full_data$internet_news <- factor(full_data$internet_news,
                        levels = c(0,1,2,3,4),
                        labels = c("Never", "Less than once a month", "A few times a month",
                                   "A few times a week", "Everyday"))

full_data$social_med_news <- factor(full_data$social_med_news,
                        levels = c(0,1,2,3,4),
                        labels = c("Never", "Less than once a month", "A few times a month",
                                   "A few times a week", "Everyday"))

full_data$information_school_budget <- factor(full_data$information_school_budget,
                                      levels = c(0,1,2,3),
                                      labels = c("Not at all likely", "Not very likely", "Somewhat likely",
                                                 "Very likely"))

full_data$information_local_gov_dev_plan <- factor(full_data$information_local_gov_dev_plan,
                                       levels = c(0,1,2,3),
                                       labels = c("Not at all likely", "Not very likely", "Somewhat likely",
                                                  "Very likely"))

full_data$information_business_registry <- factor(full_data$information_business_registry,
                                            levels = c(0,1,2,3),
                                            labels = c("Not at all likely", "Not very likely", "Somewhat likely",
                                                       "Very likely"))

full_data$sat_dem <- factor(full_data$sat_dem,
                                     levels = c(0, 1,2,3,4),
                                     labels = c("The country is not a democracy", "Not at all satisfied", 
                                                "Not very satisfied", "Fairly satisfied", "Very satisfied"))

full_data$bribe_school_services <- factor(full_data$bribe_school_services,
                                   levels = c(0,1,2,3),
                                   labels = c("Never", "Once or twice", "A few times",
                                              "Often"))

full_data$bribe_med_services <- factor(full_data$bribe_med_services,
                                   levels = c(0,1,2,3),
                                   labels = c("Never", "Once or twice", "A few times",
                                              "Often"))

full_data$gov_handling_basic_health <- factor(full_data$gov_handling_basic_health,
                                levels = c(1,2,3,4),
                                labels = c("Very badly", "Fairly badly", "Fairly well", "Very well"))

full_data$gov_handling_edu_needs <- factor(full_data$gov_handling_edu_needs,
                                       levels = c(1,2,3,4),
                                       labels = c("Very badly", "Fairly badly", "Fairly well", "Very well"))

full_data$gov_handling_corruption <- factor(full_data$gov_handling_corruption,
                                       levels = c(1,2,3,4),
                                       labels = c("Very badly", "Fairly badly", "Fairly well", "Very well"))

full_data$better_worse_access_medical_care <- factor(full_data$better_worse_access_medical_care,
                                              levels = c(1,2,3,4,5),
                                              labels = c("Much worse", "Worse", "Same", "Better", "Much better"))

full_data$better_worse_gov_effectiveness_edu_needs <- factor(full_data$better_worse_gov_effectiveness_edu_needs,
                                              levels = c(1,2,3,4,5),
                                              labels = c("Much worse", "Worse", "Same", "Better", "Much better"))

full_data$edu <- factor(full_data$edu,
                                                                      levels = c(0,1,2,3,4,5,6,7,8,9),
                                                                      labels = c("No formal schooling", "Informal schooling only", "Some primary schooling", 
                                                                                 "Primary school completed", "Some secondary/high school",
                                                                                 "Secondary/high school completed","Post-secondary qualification, other than university",
                                                                                 "Some university", "University completed", "Post-graduate"))

full_data$gender <- factor(full_data$gender,
                                                       levels = c(1,2),
                                                       labels = c("Male", "Female"))

#Save as csv
write_csv(full_data, "~/Desktop/Thesis/Data/Afrobarometer/1. CSVs/full_data_clean.csv")

#Loading clean data
df <-  read_excel("Desktop/Thesis/Data/full_datasets/full_data_clean_excel.xlsx")

#Adding round column
df <- df %>%
  mutate(round =
           case_when(year == 2008 | year == 2009 ~ "Round 4 (2008 - 2009)",
                     year == 2011 | year == 2012 | year == 2013 ~ "Round 5 (2011 - 2013)",
                     year == 2014 | year == 2015 | year == 2016 ~ "Round 6 (2014 - 2016)",
                     year == 2018 | year == 2019 ~ "Round 7 (2018 - 2019)"
                     ))

table(df$spi_health)

#Adding spi_health_cat column
df <- df %>%
  mutate(spi_health_cat =
           case_when(spi_health >= .5 & spi_health <= .667 ~ "1. Low SPI health",
                     spi_health > .667 & spi_health <= .723 ~ "2. Medium SPI health",
                     spi_health > .723 & spi_health <= 1.0 ~ "3. High SPI Health"
           ))
table(df$spi_health_cat)

#Adding spi_health_cat_perc column
df <- df %>%
  mutate(spi_health_cat_perc == spi_health_cat/374.76,
         )


#Adding spi_edu_cat column
table(df$spi_edu)
df <- df %>%
  mutate(spi_edu_cat =
           case_when(spi_edu >= 0 & spi_edu <= .333 ~ "1. Low SPI education",
                     spi_edu > .333 & spi_edu <= .455 ~ "2. Medium SPI education",
                     spi_edu > .455 & spi_edu <= 1.0 ~ "3. High SPI education"
           ))

table(df$spi_edu_cat)

#Adding spi_corruption_cat column
table(df$spi_openness_score)
df <- df %>%
  mutate(spi_openness_cat =
           case_when(spi_openness_score >= 0 & spi_openness_score <= .23 ~ "1. Low SPI openness",
                     spi_openness_score > .23 & spi_openness_score <= .35 ~ "2. Medium SPI openness",
                     spi_openness_score > .35 & spi_openness_score <= 1.0 ~ "3. High SPI openness"
           ))

#Adding freedom_cat
table(df$freedom_house)
df <- df %>%
  mutate(freedom_house_cat =
           case_when(freedom_house >= 0 & freedom_house <= 48 ~ "1. Low FIW",
                     freedom_house > 48 & freedom_house <= 66 ~ "2. Medium FIW",
                     freedom_house > 66 & freedom_house <= 100 ~ "3. High FIW"
           ))

table(df$freedom_house_cat)

#Adding odb_health_cat
table(df$iiag_overall_gov)

df <- df %>%
  mutate(iiag_gov_cat =
           case_when(iiag_overall_gov >= 0 & iiag_overall_gov <= 49 ~ "1. Low governance",
                     iiag_overall_gov > 49 & iiag_overall_gov <= 57 ~ "2. Medium governance",
                     iiag_overall_gov > 57 & iiag_overall_gov <= 100 ~ "3. High governance"
           ))

table(df$iiag_gov_cat)

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

table(df$gov_handling_corruption)

df <- df %>%
  mutate(wb_income = replace(wb_income, wb_income == "Low income", "1. Low income")) %>%
  mutate(wb_income = replace(wb_income, wb_income == "Lower middle income", "2. Lower middle income")) %>%
  mutate(wb_income = replace(wb_income, wb_income == "Upper middle income", "3. Upper middle income")) %>%
  mutate(wb_income = replace(wb_income, wb_income == "High income", "4. High income"))

table(df$wb_income)











