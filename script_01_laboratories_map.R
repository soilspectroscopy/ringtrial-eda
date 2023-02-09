
## Loading packages
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("readxl")
library("magick")

# Mounted disk for storing big files
mnt.dir <- "~/projects/mnt-ringtrial/"

theme_set(theme_void())

world <- ne_countries(scale = "medium", returnclass = "sf")
world.robins <- st_transform(world, crs = "+proj=robin +lon_0=0w")

points <- read_xlsx(paste0(mnt.dir, "MIR_returns_map.xlsx"))
points.sf <- st_as_sf(points, coords = c("Longitude", "Latitude"), crs = 4326)
points.robins <- st_transform(points.sf, crs = "+proj=robin +lon_0=0w")

# ocean <- ne_download(scale = 10, type = 'ocean', category = 'physical')
# 
# ggplot() +
#   geom_sf(data = ocean, color = NA, fill = "aliceblue")

logo <- image_read(paste0(mnt.dir, "logos/soilspec4gg-logo.png"))

png(filename = "outputs/map_ring_trial.png",
    width = 8, height = 6, units = "in", res = 300)
ggplot() +
  # annotation_custom(t, xmin = 1000, xmax = 1000, ymin = 1000, ymax = 1000) +
  geom_sf(data = world.robins, color = NA, fill = "lightgray") +
  geom_sf(data = points.robins, color = "darkblue") +
  labs(title = "MIR soil spectroscopy ring trial laboratories (n=20)",
       caption = "SoilSpec4GG is a USDA-funded FACT-CIN NIFA Award #2020-67021-32467 project.") +
  theme(panel.grid.major = element_line(color = "black", linetype = "dashed", size = 0.1),
        # plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

grid::grid.raster(logo, x = 0.5, y = 0.85, just = c('center', 'bottom'), width = unit(2, 'inches'))
dev.off()
