library(rgdal)
library(maptools)
library(plyr)
library(ggplot2)
library(choroplethr)
library(R6)
library(dplyr)

# Initialize map data, layer is the name of the .shp file
oregon.map <- readOGR(dsn="Data", layer="wildlife_mgmt_units2008")
oregon.map@data$id = rownames(oregon.map@data)
oregon.map.point = fortify(oregon.map, region = "id")
oregon.df <- inner_join(oregon.map.point, oregon.map@data, by = "id")

# verify we can plot it. Added color because it was all showing up the same without it
ggplot(oregon.df, aes(x = long, y = lat, group = group, fill = UNIT_NAME)) + 
  geom_polygon() +
  guides(fill = FALSE)

# Extract regions to join unit name from hunting stats to unit number
# regions <- oregon.df %>%
#   select(UNIT_NUM, UNIT_NAME) %>%
#   unique()
# 
# regions$UNIT_NAME <- as.character(regions$UNIT_NAME)

unit.areas <- read.csv("Data/wmuArea.csv", stringsAsFactors = FALSE)

unit.areas$UNIT_NAME <- gsub(pattern = "MTN$", replacement = "MOUNTAIN", x = unit.areas$UNIT_NAME)


# inner_join(regions, unit.areas)

######################################################################
# Now start messing with the hunting stats
# Load the hunting stats
deer.rifle <- read.csv("Data/Deer_Rifle_2016.csv",
                       stringsAsFactors = FALSE)

names(deer.rifle) <- c("SeasonType", 
                       "Zone", 
                       "UnitName", 
                       "SeasonTagHunt", 
                       "TotalHunters", 
                       "Does", 
                       "TotalBucks", 
                       "Spikes", 
                       "Forks", 
                       "ThreePoint", 
                       "FourPoint", 
                       "HarvestTotal", 
                       "SuccessRate")

# Fix Unit Names with so they match up
deer.rifle$UnitName <- gsub(pattern = "'", replacement = "", x = deer.rifle$UnitName)
deer.rifle$UnitName <- gsub(pattern = "^Mt.", replacement = "Mount", x = deer.rifle$UnitName)
deer.rifle$UnitName <- gsub(pattern = "Mt.$", replacement = "Mountain", x = deer.rifle$UnitName)
deer.rifle$UnitName <- toupper(deer.rifle$UnitName)

# Find stats I'm interested in 
four.point.per.mile <- deer.rifle %>%
  group_by(UnitName) %>%
  summarise(FourPoint = sum(FourPoint)) %>% 
  mutate(UNIT_NAME = toupper(UnitName)) %>%
  inner_join(unit.areas) %>%
  mutate(FourPointPerMile = FourPoint / Sq_mi) %>%
  select(region = UNIT_NUM, FourPointPerMile)

# For dealing with Crater Lake/Warm Springs
four.point.per.mile <- rbind(four.point.per.mile,
                    c(0, 0))


head(four.point.per.mile)


oregon.df$region <- oregon.df$UNIT_NUM

# create the class, inheriting from the base Choropleth object
ORChoropleth = R6Class("ORChoropleth",
                       inherit = choroplethr:::Choropleth,
                       public = list(
                              
                              # initialize with a world map
                              initialize = function(user.df)
                              {
                                super$initialize(oregon.df, user.df)
                              }
                            )
                     )


or.4point <- ORChoropleth$new(four.point)
or.4point$render()
or.4point

###############################################################
# Trying some ggplot2 things

four.point.gg <- deer.rifle %>%
  group_by(UnitName) %>%
  summarise(value = sum(FourPoint)) %>% 
  mutate(UNIT_NAME = toupper(UnitName))

oregon.df.four.point <- oregon.df %>% 
  inner_join(four.point.gg)

ggplot(oregon.df, aes(x = long, y = lat, group = UNIT_NAME, fill = value, color = "black")) +
  geom_polygon() +
  guides(color = FALSE) +
  labs(fill = "Total Bucks") +
  ggtitle(expression(atop("Total 4+ Point Bucks Taken (2016)", 
                          atop(italic("Controlled and General Hunts, Rifle Only"), "")))) +
  theme(plot.title = element_text(size = 25, face = "bold", colour = "black", vjust = -1),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

####################################################

four.point.per.mile <- deer.rifle %>%
  group_by(UnitName) %>%
  summarise(FourPoint = sum(FourPoint)) %>% 
  mutate(UNIT_NAME = toupper(UnitName)) %>%
  inner_join(unit.areas) %>%
  mutate(FourPointPerMile = FourPoint / Sq_mi) %>%
  select(UNIT_NAME, FourPointPerMile)

four.point.per.mile.general <- deer.rifle %>%
  filter(SeasonType == "General") %>%
  group_by(UnitName) %>%
  summarise(FourPoint = sum(FourPoint)) %>% 
  mutate(UNIT_NAME = toupper(UnitName)) %>%
  inner_join(unit.areas) %>%
  mutate(FourPointPerMile = FourPoint / Sq_mi) %>%
  select(UNIT_NAME, FourPointPerMile)


oregon.df.four.point.per.mile <- oregon.df %>%
  left_join(four.point.per.mile)

oregon.df.four.point.per.mile.general <- oregon.df %>%
  left_join(four.point.per.mile.general)

ggplot(oregon.df.four.point.per.mile, 
       aes(x = long, y = lat, group = UNIT_NAME, fill = FourPointPerMile)) +
  geom_polygon(, color = "black") +
  guides(color = FALSE) +
  labs(fill = "4pt Bucks \nPer Square Mile") +
  ggtitle(expression(atop("4+ Point Bucks Taken per Square Mile (2016)", 
                          atop(italic("Controlled and General Hunts, Rifle Only"), "")))) +
  theme(plot.title = element_text(size = 25, face = "bold", colour = "black", vjust = -1),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) +
  scale_fill_gradient(low = "white", high = "green4")

oregon.df.four.point.per.mile <- oregon.df %>%
  left_join(four.point.per.mile)

ggplot(oregon.df.four.point.per.mile.general, 
       aes(x = long, y = lat, group = UNIT_NAME, fill = FourPointPerMile)) +
  geom_polygon(, color = "black") +
  guides(color = FALSE) +
  labs(fill = "4pt Bucks \nPer Square Mile") +
  ggtitle(expression(atop("4+ Point Bucks Taken per Square Mile (2016)", 
                          atop(italic("General Season Rifle Hunts Only"), "")))) +
  theme(plot.title = element_text(size = 25, face = "bold", colour = "black", vjust = -1),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) +
  scale_fill_gradient(low = "white", high = "green4")

