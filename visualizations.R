library(ggplot2)

t <- c("1. Very badly", "2. Fairly badly", "3. Fairly well", "4. Very well")

df_viz <- df %>% 
  rename(Round = round)

##Health

#Gov handling health vs. SPI health (by round)
plot_data_spi_health_round <- group_by(df_viz, spi_health_cat) %>%
  mutate(group_size = n()) %>%
  group_by(spi_health_cat, gov_handling_basic_health, Round) %>%
  summarise(perc = n()/max(group_size))

ggplot(plot_data_spi_health_round, aes(x= gov_handling_basic_health, fill = Round, y = perc)) + 
  scale_x_discrete(limits = t, guide = guide_axis(angle = 45)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ spi_health_cat) +
  xlab("Opinion of government handling basic health") + 
  ylab("Proportion") +
  theme(legend.title=element_blank()) +
  theme_minimal()

#Gov handling health vs. SPI health (by governance)
plot_data_spi_health_gov <- group_by(df, spi_health_cat) %>%
  mutate(group_size = n()) %>%
  group_by(spi_health_cat, gov_handling_basic_health, iiag_gov_cat) %>%
  summarise(perc = n()/max(group_size))

ggplot(plot_data_spi_health_gov, aes(x= gov_handling_basic_health, fill = iiag_gov_cat, y = perc)) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ spi_health_cat) +
  xlab("Opinion of government handling basic health") + 
  ylab("Proportion") +
  theme(legend.title=element_blank()) +
  theme_minimal()

##Education

#Gov handling edu vs. SPI edu (by round)
plot_data_spi_edu_round <- group_by(df, spi_edu_cat) %>%
  mutate(group_size = n()) %>%
  group_by(spi_edu_cat, gov_handling_edu_needs, round) %>%
  summarise(perc = n()/max(group_size))

ggplot(plot_data_spi_edu_round, aes(x= gov_handling_edu_needs, fill = round, y = perc)) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ spi_edu_cat) +
  xlab("Opinion of government handling education needs") + 
  ylab("Proportion") +
  theme(legend.title=element_blank()) +
  theme_minimal()

#Gov handling edu vs. SPI edu (by governance)

df_viz <- df %>% 
  rename(Governance = iiag_gov_cat)

plot_data_spi_edu_gov <- group_by(df_viz, spi_edu_cat) %>%
  mutate(group_size = n()) %>%
  group_by(spi_edu_cat, gov_handling_edu_needs, Governance) %>%
  summarise(perc = n()/max(group_size))

ggplot(plot_data_spi_edu_gov, aes(x= gov_handling_edu_needs, fill = Governance, y = perc)) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ spi_edu_cat) +
  xlab("Opinion of government handling education needs") + 
  ylab("Proportion") +
  theme(legend.title=element_blank()) +
  theme_minimal()

##################################################################

#Gov handling health vs. SPI health (by income)
plot_data_spi_health <- group_by(df, spi_health_cat) %>%
  mutate(group_size = n()) %>%
  group_by(spi_health_cat, gov_handling_basic_health, wb_income) %>%
  summarise(perc = n()/max(group_size))

ggplot(plot_data_spi_health, aes(x= gov_handling_basic_health, fill = wb_income, y = perc)) + 
  scale_x_discrete(limits = t, guide = guide_axis(angle = 45)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ spi_health_cat) +
  theme_minimal()

#Gov handling health vs. SPI health (by income)
plot_data_spi_health <- group_by(df, wb_income) %>%
  mutate(group_size = n()) %>%
  group_by(wb_income, spi_health_cat, gov_handling_basic_health) %>%
  summarise(perc = n()/max(group_size))

ggplot(plot_data_spi_health, aes(x= gov_handling_basic_health, fill = spi_health_cat, y = perc)) + 
  scale_x_discrete(limits = t, guide = guide_axis(angle = 45)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ wb_income) +
  theme_minimal()

#Gov handling health vs. SPI health (by FIW)
plot_data_spi_health <- group_by(df, freedom_house_cat) %>%
  mutate(group_size = n()) %>%
  group_by(freedom_house_cat, spi_health_cat, gov_handling_basic_health) %>%
  summarise(perc = n()/max(group_size))

ggplot(plot_data_spi_health, aes(x= gov_handling_basic_health, fill = spi_health_cat, y = perc)) + 
  scale_x_discrete(limits = t, guide = guide_axis(angle = 45)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ freedom_house_cat) +
  theme_minimal()

#Gov handling health vs. SPI health (by IIAG gov)
plot_data_spi_health <- group_by(df, iiag_gov_cat) %>%
  mutate(group_size = n()) %>%
  group_by(iiag_gov_cat, spi_health_cat, gov_handling_basic_health) %>%
  summarise(perc = n()/max(group_size))

ggplot(plot_data_spi_health, aes(x= gov_handling_basic_health, fill = spi_health_cat, y = perc)) + 
  scale_x_discrete(limits = t, guide = guide_axis(angle = 45)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ iiag_gov_cat) +
  theme_minimal()

#Gov handling health vs. sat dem (by urbanity)
plot_data_spi_health <- group_by(df, URBRUR) %>%
  mutate(group_size = n()) %>%
  group_by(URBRUR, sat_dem, gov_handling_basic_health) %>%
  summarise(perc = n()/max(group_size))

ggplot(plot_data_spi_health, aes(x= gov_handling_basic_health, fill = sat_dem, y = perc)) + 
  scale_x_discrete(limits = t, guide = guide_axis(angle = 45)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ URBRUR) +
  theme_minimal()

#Gov handling edu vs. SPI edu
plot_data_spi_edu <- group_by(df, spi_edu_cat) %>%
  mutate(group_size = n()) %>%
  group_by(spi_edu_cat, gov_handling_edu_needs, round) %>%
  summarise(perc = n()/max(group_size))

ggplot(plot_data_spi_edu, aes(x= gov_handling_edu_needs, fill = round, y = perc)) + 
  scale_x_discrete(limits = t, guide = guide_axis(angle = 45)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ spi_edu_cat) +
  theme_minimal()

#Gov handling edu vs. sat dem (by urbanity)
plot_data_edu <- group_by(df, URBRUR) %>%
  mutate(group_size = n()) %>%
  group_by(URBRUR, sat_dem, gov_handling_edu_needs) %>%
  summarise(perc = n()/max(group_size))

ggplot(plot_data_edu, aes(x= gov_handling_edu_needs, fill = sat_dem, y = perc)) + 
  scale_x_discrete(limits = t, guide = guide_axis(angle = 45)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ URBRUR) +
  theme_minimal()









