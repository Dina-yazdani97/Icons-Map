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


iran <- st_read("F:/30DayMapChallenge/gadm41_IRN_1.shp")
air_iran <- air_sf[iran, ]

iran_utm <- st_transform(iran, 32639)
air_iran_utm <- st_transform(air_iran, 32639)

p <- ggplot() +
  geom_sf(data = iran_utm, fill = "black", color = "red4", linewidth = 0.8) +
  geom_sf_text(
    data = air_iran_utm,
    aes(label = "\u2708"),
    size = 5,
    color = "#00EEEE",
    family = "Times New Roman"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 30, color = "white", hjust = 0.4, vjust = -1)
  ) +
  labs(
    title = "Airports of Iran"
  )
print(p)

ggsave("E:/Iran_Airports_Map.png", plot = p, width = 10, height = 8, dpi = 300, bg = "black")
