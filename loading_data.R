library(readxl)
library(tidyverse)

#Loading clean data
df <- read_csv("~/Desktop/Thesis/Data/full_datasets/df.csv")

#Setting/factorizing data
df$information_school_budget <- as.factor(df$information_school_budget)
df$information_business_registry <- as.factor(df$information_business_registry)
df$gov_handling_basic_health <- as.factor(df$gov_handling_basic_health)
df$gov_handling_edu_needs <- as.factor(df$gov_handling_edu_needs)
df$round <- as.factor(df$round)
df$spi_health_cat <- as.factor(df$spi_health_cat)
df$spi_edu_cat <- as.factor(df$spi_edu_cat)
df$spi_bus_cat <- as.factor(df$spi_bus_cat)
df$iiag_gov_cat <- as.factor(df$iiag_gov_cat)


table(df$wb_income)
df <- df %>%
  mutate(wb_income = replace(wb_income, wb_income == "Low income", "1. Low income")) %>%
  mutate(wb_income = replace(wb_income, wb_income == "Lower middle income", "2. Lower middle income")) %>%
  mutate(wb_income = replace(wb_income, wb_income == "Upper middle income", "3. Upper middle income")) %>%
  mutate(wb_income = replace(wb_income, wb_income == "High income", "4. High income"))

table(df$wb_income)

#Libraries
library(tidyverse)
library(table1)
library(ordinal)
library(stargazer)
