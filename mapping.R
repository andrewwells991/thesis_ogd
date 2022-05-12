#Mapping

pacman::p_load(sf, leaflet,
               dplyr, qtm, ggplot2, raster, tmap, tmaptools)

setwd("~/Documents/GitHub/16-FernandaCandidoGomes-GeocodingwithSF")

# Save the shapefile as a data frame using the read_sf() function
world <- sf::read_sf("ne_50m_admin_0_countries.shp") #Look for the .shp
world

library(readxl)
map_spi <- read_excel("~/Desktop/Thesis/Data/shape_files/map_spi.xlsx")

africa <- world %>%
  filter(SOVEREIGNT == "Angola" | SOVEREIGNT == "Benin" | SOVEREIGNT == "Botswana" | 
           SOVEREIGNT == "Burkina Faso" | SOVEREIGNT == "Burundi" | SOVEREIGNT == "Cabo Verde" | 
           SOVEREIGNT == "Cameroon" | SOVEREIGNT == "Central African Republic" | SOVEREIGNT == "Chad" | 
           SOVEREIGNT == "Comoros" | SOVEREIGNT == "Democratic Republic of the Congo" | SOVEREIGNT == "Djibouti" | 
           SOVEREIGNT == "Equatorial Guinea" |
           SOVEREIGNT == "Eritrea" | SOVEREIGNT == "eSwatini" | SOVEREIGNT == "Ethiopia" | 
           SOVEREIGNT == "Gabon" | SOVEREIGNT == "Gambia" | SOVEREIGNT == "Ghana" | 
           SOVEREIGNT == "Guinea" | SOVEREIGNT == "Guinea-Bissau" | SOVEREIGNT == "Lesotho" |
           SOVEREIGNT == "Kenya" | SOVEREIGNT == "Liberia" | SOVEREIGNT == "Madagascar" | 
           SOVEREIGNT == "Ivory Coast" | SOVEREIGNT == "Malawi" | SOVEREIGNT == "Mali" | 
           SOVEREIGNT == "Mauritius" | SOVEREIGNT == "Mauritania" | SOVEREIGNT == "Mozambique" | 
           SOVEREIGNT == "Namibia" | SOVEREIGNT == "Niger" | SOVEREIGNT == "Nigeria" |
           SOVEREIGNT == "Republic of the Congo" |SOVEREIGNT == "Rwanda" | 
           SOVEREIGNT == "São Tomé and Principe" | SOVEREIGNT == "Senegal" |
           SOVEREIGNT == "Seychelles" |SOVEREIGNT == "Sierra Leone" |SOVEREIGNT == "South Africa" | 
           SOVEREIGNT == "Somalia" |
           SOVEREIGNT == "South Sudan" |SOVEREIGNT == "Togo" |SOVEREIGNT == "United Republic of Tanzania" |
           SOVEREIGNT == "Uganda" |
           SOVEREIGNT == "Zambia" |SOVEREIGNT == "Zimbabwe" |
           SOVEREIGNT == "Algeria" |
           SOVEREIGNT == "Egypt" | SOVEREIGNT == "Libya" | SOVEREIGNT == "Morocco" | 
           SOVEREIGNT == "Somaliland" | SOVEREIGNT == "Sudan" | SOVEREIGNT == "Tunisia" |
           SOVEREIGNT == "Western Sahara" )

qtm(africa)

#Algeria
#Egypt
#Libya
#Morocco
#Somaliland
#Sudan
#Tunisia
#Western Sahara

#Joining data
map_df <- left_join(africa, map_spi, by = "SOVEREIGNT")

map_df <- map_df %>% 
  rename("SPI Health 08" = spi_h_08) %>%
  rename("SPI Health 18" = spi_h_18) %>%
  rename("SPI Edu 08" = spi_e_08) %>%
  rename("SPI Edu 18" = spi_e_18)


#Transform to sf
df <- st_as_sf(map_df)

#Mapping

#SPI Health 2008
tm_shape(df) +
  tm_polygons("SPI Health 08", id = "SOVEREIGNT", 
              breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
              legend.hist = TRUE,
              palette = "Greens") +
  tm_text("NAME", size = 1/2) +
  tm_fill()

#SPI Health 2018
tm_shape(df) +
  tm_polygons("SPI Health 18", id = "SOVEREIGNT",
              breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
              legend.hist = TRUE,
              palette = "Greens") +
  tm_text("NAME", size = 1/2) +
  tm_fill()

#SPI Edu 2008
tm_shape(df) +
  tm_polygons("SPI Edu 08", id = "SOVEREIGNT",
              breaks = c(0, 0.125, 0.25, 0.5, 0.625, 0.75),
              legend.hist = TRUE,
              palette = "Blues") +
  tm_text("NAME", size = 1/2) +
  tm_fill()

#SPI Edu 2018
tm_shape(df) +
  tm_polygons("SPI Edu 18", id = "SOVEREIGNT",
              breaks = c(0, 0.125, 0.25, 0.5, 0.625, 0.75),
              legend.hist = TRUE,
              palette = "Blues") +
  tm_text("NAME", size = 1/2) +
  tm_fill()








