library(tidyverse)
library(readxl)
library(ordinal)  #ordinal regression package
library(rcompanion) #pseudo R square 
library(MASS) #plyr method (for getting data that allows the test of proportional odds)
library(brant)# test of proportional odds
library(stargazer)
library(gtsummary)
library(sjPlot)

setwd("~/Desktop/Thesis/Analysis/Regression tables")

## SPI Health categorical
spi_health_cat1 <- clm(gov_handling_basic_health ~ spi_health_cat + round, 
                data = df)
summary(spi_health_cat1)

spi_health_cat2 <- clm(gov_handling_basic_health ~ spi_health_cat + round +
                         age + gender + URBRUR + edu_cat + present_living_condit + sat_dem, 
                       data = df)
summary(spi_health_cat2)

spi_health_cat3 <- clm(gov_handling_basic_health ~ spi_health_cat + round +
                 age + gender + URBRUR + edu_cat + present_living_condit + sat_dem + internet_cat + 
                   iiag_gov_cat + wb_income + freedom_house_cat, 
               data = df)
summary(spi_health_cat3)

#Chi square
anova(spi_health_cat2, type="III")

#Odds ratio table
OR_spi_health1 <- exp(coef(spi_health_cat1))
OR_spi_health2 <- exp(coef(spi_health_cat2))
OR_spi_health3 <- exp(coef(spi_health_cat3))

stargazer(
  spi_health_cat1,
  spi_health_cat2,
  spi_health_cat3,
  dep.var.labels = "Opinion of government handling of basic health", model.numbers = FALSE,
  coef = list(OR_spi_health1, OR_spi_health2, OR_spi_health3), t.auto = F,
  p.auto = F, 
  covariate.labels = c("Medium SPI health (Ref. category: Low SPI health)", "High SPI health",
                       "Round 5 (2011 - 2013) (Ref. category: Round 4 (2008 - 2009))", 
                       "Round 6 (2014 - 2016)", "Round 7 (2018 - 2019)", 
                       "Age", "Male (Ref. category: Female)", "Urban (Ref. category: Rural)",
                       "Medium education (Ref. category: Low education)",
                       "High education",
                       "Fairly bad living condition (Ref. category: Very bad living condition)",
                       "Neither good nor bad living condition",
                       "Fairly good living condition",
                       "Very good living condition",
                       "Not at all satisfied with democracy (Ref. category: Not a democracy)",
                       "Not very satisfied with democracy",
                       "Fairly satisfied with democracy",
                       "Very satisfied with democracy",
                       "Medium percentage internet use (Ref. category: Low percentage internet use)",
                       "High percentage internet use",
                       "Medium national governance level (Ref. category: Low national governance level)",
                       "High national governance level",
                       "Lower middle income country (Ref. category: Low income country)",
                       "Upper middle income country",
                       "High income country",
                       "Medium Freedom in the World Index (Ref. category: Low Freedom in the World Index)",
                       "High Freedom in the World Index"), omit = c("Constant"),
  type = "html", out = "health_models.htm", report = "vc*" )

tbl_regression(spi_health_cat3, exponentiate = TRUE)

plot_model(spi_health_cat1)

## SPI Eduction categorical
spi_edu_cat1 <- clm(gov_handling_edu_needs ~ spi_edu_cat + round, 
                       data = df)
summary(spi_edu_cat1)

spi_edu_cat2 <- clm(gov_handling_edu_needs ~ spi_edu_cat + round +
                         age + gender + URBRUR + edu_cat + present_living_condit + sat_dem, 
                       data = df)
summary(spi_edu_cat2)

spi_edu_cat3 <- clm(gov_handling_edu_needs ~ spi_edu_cat + round +
                      age + gender + URBRUR + edu_cat + present_living_condit + sat_dem + 
                      internet_cat + iiag_gov_cat + wb_income + freedom_house_cat, 
                       data = df)
summary(spi_edu_cat3)

#Odds ratio
exp(coef(spi_edu_cat2) [4])
exp(coef(spi_edu_cat2) [5])

#Chi square
anova(spi_cat2, type="III")

#Odds ratio table
OR_4 <- exp(coef(spi_edu_cat1))
OR_5 <- exp(coef(spi_edu_cat2))
OR_6 <- exp(coef(spi_edu_cat3))

stargazer(
  spi_edu_cat1,
  spi_edu_cat2,
  spi_edu_cat3,
  dep.var.labels = "Opinion of government handling of education needs", model.numbers = FALSE,
  coef = list(OR_4, OR_5, OR_6), t.auto = F,
  p.auto = F,
  covariate.labels = c("Medium SPI education (Ref. category: Low SPI education)", "High SPI education",
                       "Round 5 (2011 - 2013) (Ref. category: Round 4 (2008 - 2009)", 
                       "Round 6 (2014 - 2016)", "Round 7 (2018 - 2019)", 
                       "Age", "Male (Ref. category: Female)", "Urban (Ref. category: Rural)",
                       "Medium education (Ref. category: Low education)",
                       "High education",
                       "Fairly bad living condition (Ref. category: Very bad living condition)",
                       "Neither good nor bad living condition",
                       "Fairly good living condition",
                       "Very good living condition",
                       "Not at all satisfied with democracy (Ref. category: Not a democracy)",
                       "Not very satisfied with democracy",
                       "Fairly satisfied with democracy",
                       "Very satisfied with democracy",
                       "Medium percentage internet use (Ref. category: Low percentage internet use)",
                       "High percentage internet use",
                       "Medium national governance level (Ref. category: Low national governance level)",
                       "High national governance level",
                       "Lower middle income country (Ref. category: Low income country)",
                       "Upper middle income country",
                       "High income country",
                       "Medium Freedom in the World Index (Ref. category: Low Freedom in the World Index)",
                       "High Freedom in the World Index"), omit = c("Constant"),
  type = "html", out = "edu_models.htm", report = "vc*" )

table(df$spi_edu_cat)
