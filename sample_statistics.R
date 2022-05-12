library(table1)

# Sample statistic health

table1::label(df$spi_health_cat) <- "SPI health category" 
table1::label(df$gov_handling_basic_health) <- "Gov handling basic health" 
table1::label(df$gender) <- "Gender" 
table1::label(df$URBRUR) <- "Urbanity" 
table1::label(df$edu_cat) <- "Education"
table1::label(df$present_living_condit) <- "Present living condition"
table1::label(df$sat_dem) <- "Satisfaction with democracy"
table1::label(df$internet_cat) <- "National percent of internet use"
table1::label(df$iiag_gov_cat) <- "National governance"
table1::label(df$wb_income) <- "National income"
table1::label(df$freedom_house_cat) <- "National Freedom in the World (FIW) index"
table1::label(df$round) <- "Afro Barometer round"
table_1 <- table1( ~ spi_health_cat + gov_handling_basic_health + 
                     gender + URBRUR + edu_cat + present_living_condit + sat_dem + internet_cat + 
                     iiag_gov_cat + wb_income + freedom_house_cat + round
                   | spi_health_cat, data = df)
table_1

# Sample statistic education

table1::label(df$spi_edu_cat) <- "SPI education category" 
table1::label(df$gov_handling_edu_needs) <- "Gov handling education needs" 

table_2 <- table1( ~ spi_edu_cat + gov_handling_edu_needs + 
                     gender + URBRUR + edu_cat + present_living_condit + sat_dem + internet_cat + 
                     iiag_gov_cat + wb_income + freedom_house_cat + round
                   | spi_edu_cat, data = df)
table_2

# Sample statistic governance/corruption

table1::label(df$spi_openness_cat) <- "SPI openness category" 
table1::label(df$gov_handling_corruption) <- "Gov handling corruption" 
table1::label(df$gender) <- "Gender" 
table1::label(df$wb_income) <- "National income category" 
table1::label(df$round) <- "Afro Barometer round"
table_3 <- table1( ~ spi_openness_cat + gov_handling_corruption + 
                     gender + wb_income + round| spi_openness_cat, data = df)
table_3

