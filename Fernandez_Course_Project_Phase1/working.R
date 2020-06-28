library(tidyverse)
library(lubridate)
#library(ggmap)
library(sf)
library(tmap)

# library("rnaturalearth")
# library("rnaturalearthdata")

# Set global theme
theme_set(theme_bw())

# world <- ne_countries(scale = "medium", returnclass = "sf")

df <- read_csv("chicago2.csv")
df <- df %>%
  mutate(Date = mdy_hms(Date))
# Read in shapefile
sdf <- st_read("police_beat_bndry/police_beat_bndry.shp", stringsAsFactors = FALSE)

# Group by Beat ID
by_beat <- df %>%
  group_by(Beat) %>%
  tally() %>%
  rename("Num.of.Crimes" = n)

# Merge by_beat and sdf by Beat ID
df1 <- sf::merge(x = by_beat, y = sdf[ , c("beat_num", "geometry")],
             by.x = "Beat", by.y = "beat_num", all.x = TRUE)
# Convert df1 to sf object
df1 <- st_as_sf(df1)

# Map crimes by beat
ggplot(world) +
  #geom_sf(fill = "antiquewhite1") +
  geom_sf(data = df1, aes(fill = Num.of.Crimes)) +
  coord_sf(xlim = c(-87.97, -87.47), ylim = c(41.62, 42.05), expand = FALSE)



library(sp)
library(RColorBrewer)
mycolours <- brewer.pal(8, "YlOrRd")
spplot(df1,"Num.of.Crimes", par.settings = list(axis.line = list(col ="transparent")),
       main = "Number of Crimes by Beat", cuts = 5, col ="transparent",
       col.regions = mycolours)


library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf()

DT = data.table(x=c(1,NaN,NA,3), y=c(NA_integer_, 1:3), z=c("a", NA_character_, "b", "c"))


na.omit(DT, cols="x")

install.packages("fmsb")
library(fmsb)

r <- df %.>%
  group_by(., hr, day.of.week) %.>%
  tally(.) %.>%
  rename(., "Num.of.Crimes" = n) #%.>%
  # ggplot(., aes(x = hr, y = Num.of.Crimes,
  #               group = day.of.week,
  #               color = day.of.week)) %.>%
  # geom_line(size = 1.25) %.>%
  # geom_point(size = 2.1) %.>%
  # theme(legend.position="bottom") %.>%
  # ggtitle("Number of Crimes by Hour and Day of Week") %.>%
  # xlab("Hour of Day") %.>%
  # ylab("Count")
radarchart(r)
