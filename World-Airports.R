library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(rio)
library(extrafont)
library(ggspatial)

file_path <- "E:\\airports.txt"
air <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)
colnames(air) <- c("id","name","city","country","iata","icao",
                   "lat","lon","alt","tzoffset","dst","tz",
                   "type","source")

air$lon <- as.numeric(air$lon)
air$lat <- as.numeric(air$lat)

air <- air %>% filter(!is.na(lon) & !is.na(lat))
air <- air %>% filter(iata != "\\N")
air_sf <- st_as_sf(air, coords = c("lon","lat"), crs = 4326, remove = FALSE)

world <- ne_countries(scale = "medium", returnclass = "sf")

target_crs_robinson <- "+proj=robin"
world_robin <- st_transform(world, crs = target_crs_robinson)
air_robin <- st_transform(air_sf, crs = target_crs_robinson)

graticules_robinson <- st_graticule(
  lat = seq(-80, 80, by = 10),
  lon = seq(-180, 180, by = 20),
  crs = 4326
) |> st_transform(crs = target_crs_robinson)

p <- ggplot() +
  geom_sf(data = world_robin, fill = "#333333", color = "#FF0000", linewidth = 0.2) +
  geom_sf(data = graticules_robinson, color = "gray50", linewidth = 0.3, linetype = "solid") +
  geom_sf_text(data = air_robin, aes(label = "\u2708"), size = 1.3, family = "Times New Roman") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 25, color = "white", hjust = 0.5)
  ) +
  labs(title = "Airports of the World")

print(p)

ggsave("E:/World_Airports_Map_Graticules.png", plot = p, width = 14, height = 8, dpi = 600, bg = "black")
