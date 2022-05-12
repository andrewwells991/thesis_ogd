library(readr)
library(tidyverse)

setwd("~/Desktop/Thesis/Data/Afrobarometer/1. CSVs")

#Import csv's

benin_afrobar <- read_csv("benin_afrobar.csv")

botswana_afrobar <- read_csv("botswana_afrobar.csv")

burkina_faso_afrobar <- read_csv("burkina_faso_afrobar.csv")

burundi_afrobar <- read_csv("burundi_afrobar.csv")

cabo_verde_afrobar <- read_csv("cabo_verde_afrobar.csv")

cameroon_afrobar <- read_csv("cameroon_afrobar.csv")

cote_divoire_afrobar <- read_csv("cote_divoire_afrobar.csv")

eswatini_afrobar <- read_csv("eswatini_afrobar.csv")

#Joining df's
afrobar <- full_join(benin_afrobar, botswana_afrobar)
afrobar <- full_join(afrobar, burkina_faso_afrobar)
afrobar <- full_join(afrobar, burundi_afrobar)
afrobar <- full_join(afrobar, cabo_verde_afrobar)
afrobar <- full_join(afrobar, cameroon_afrobar)
afrobar <- full_join(afrobar, cote_divoire_afrobar)
afrobar <- full_join(afrobar, eswatini_afrobar)

write_csv(afrobar, "~/Desktop/Thesis/Data/Afrobarometer/1. CSVs/draft_afrobar.csv")

table(afrobar$age)
