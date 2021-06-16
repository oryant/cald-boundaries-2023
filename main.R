# Load packages
library(ggplot2)
library(sf)
library(tidyverse)
library(data.table)
library(flextable)
library(ggspatial)

# Load shapefiles
shp_wards <- read_sf('./shapefiles/Wards_polygon.shp', query = "SELECT NAME FROM WARDS_polygon") # Calderdale ward shapefile
shp_newconst <- read_sf('./shapefiles/proposed_const.shp', 
                        query = "SELECT Constituen AS Constituency, Electorate FROM proposed_const WHERE Constituen IN (\'Halifax\', \'Calder Valley\', \'Batley and Hipperholme\')") # proposed constituencies

# Load ward data
wards <- fread('./data/calderdale_wards.csv', select=c('Ward', 'ONS Code', '2019const', 'newconst', 'abbreviation')) # 

# Load electorate from Electoral Commission ward data
bc_wards <- read_sf('./shapefiles/existing_and_prospective_wards_for_2023_review.shp', query = "SELECT ons_code, electorate AS Electorate, popdensity FROM existing_and_prospective_wards_for_2023_review") %>%
  as.data.frame() %>%
  subset(select = -c(geometry))  

# Join the data together
wards <- inner_join(bc_wards, wards, by=c('ons_code' = 'ONS Code')) %>%
  as.data.frame()

# Add ONS codes and electorate to wards shapefile
shp_wards <- full_join(shp_wards, wards, by = c("NAME" = "Ward"))

# Make it a shapefile again
shp_wards <- st_sf(shp_wards, sf_column_name = 'geometry')

# Compute centroids, so we can add labels
shp_wards <- cbind(shp_wards, st_coordinates(st_centroid(shp_wards)))

# Import Polling stations
shp_polling_stations <- read_sf('./shapefiles/polling_districts.geojson')

# Define constituency colours
pal_colours <- c(
  "Calder Valley" = "Blue",
  "Halifax" = "Red",
  "Batley and Hipperholme" = "Grey"
  )

pal_shapes <- c(
  "Polling station" = 18
)

# Define plot theme
mytheme <- theme( 
  text = element_text(family = "Avenir Next"),
  panel.grid = element_blank(),
  panel.background = element_blank(),
  plot.title = element_text(size = 18, face = "bold"),
  plot.subtitle = element_text(size = 12),
  axis.title = element_blank(),
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  legend.key = element_blank()
)

# Nudge labels slightly
shp_wards$nudge_y <- 0
shp_wards$nudge_x <- 0
shp_wards$nudge_y[shp_wards$abbreviation == "SB"] <- 200
shp_wards$nudge_x[shp_wards$abbreviation == "GS"] <- 650
shp_wards$nudge_x[shp_wards$abbreviation == "TOW"] <- 250
shp_wards$nudge_y[shp_wards$abbreviation == "BRI"] <- 200

# Plot current constituencies
plot_curr_const <- ggplot(shp_wards)+
  mytheme+ 
  geom_sf(aes(fill = X2019const, alpha = popdensity), col = "grey20")+
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )+
  scale_fill_manual(
    values = pal_colours,
    limits = names(pal_colours)[1:2], # We don't need Batley and Hipperholme for this map
  )+
  scale_alpha(range = c(0.75, 1), guide = "none")+
  geom_sf(data = shp_polling_stations, aes(shape = "Polling station"), size = 1, fill = "grey20", col = "grey20")+ # Plot polling stations on map
  scale_shape_manual(values = pal_shapes, breaks = names(pal_shapes))+
  geom_label(
    data = shp_wards, 
    size = 2.5,
    aes(X, Y, label = abbreviation),
    nudge_x = shp_wards$nudge_x,
    nudge_y = shp_wards$nudge_y
  )+
  labs(title = "Current constituency boundaries")


# Plot new constituencies
plot_new_const <- ggplot(shp_wards)+
  mytheme+ 
  geom_sf(data = shp_newconst, col = "grey75", alpha = 0, linetype = "dashed")+
  geom_sf(aes(fill = newconst, alpha = popdensity), col = "grey20")+
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )+
  scale_fill_manual(
      values = pal_colours,
      limits = names(pal_colours)
  )+
  scale_alpha(range = c(0.75, 1), guide = "none")+
  geom_sf(data = shp_polling_stations, aes(shape = "Polling station"), size = 1.5, fill = "grey20", col = "grey20")+ # Plot polling stations on map
  scale_shape_manual(values = pal_shapes, breaks = names(pal_shapes))+
  geom_label(
    data = shp_wards, 
    size = 2.5,
    aes(X, Y, label = abbreviation),
    nudge_x = shp_wards$nudge_x,
    nudge_y = shp_wards$nudge_y
    )+
  annotation_scale(location = "br", width_hint = 0.25, style = "ticks", unit_category = "imperial") +
  labs(title = "Proposed constituency arrangement from 2023",
       subtitle = "Boundary Commission for England initial proposal, 8 June 2021")

# Create a table of electorate per constituency

library(gridExtra)

tbl_wards <- wards %>% select(Ward, Electorate) %>% tableGrob()

# Place plot and table side-by-side
grid.arrange(plot_new_const, tbl_wards,
             nrow=1,
             as.table=T
             )
