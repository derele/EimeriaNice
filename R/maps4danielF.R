library(sf)
library(scico)
library(sp)
library(raster)
library(stars)

SOTA <- read.csv("https://raw.githubusercontent.com/derele/Mouse_Eimeria_Field/master/data_products/SOTA_Data_Product.csv")


table(grepl("ZZ", SOTA$Mouse_ID))

SOTA$Apodemus <- ifelse(grepl("ZZ", SOTA$Mouse_ID), "Apodemus agrarius", "Mus musculus")


## remove mice with missing geocoordinate information and transform
filter(SOTA, !is.na(Longitude) & !is.na(Latitude)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4236) %>%
    st_transform(crs = 3035) ->
mice_sp 

## extract human fpi around each mouse
hpfi_1000m_df <- raster::extract(human_fpi, mice_sp, buffer = 1000,
                                 fun = mean, na.rm = TRUE, sp = TRUE)

mice_sp$human_fpi_1000m <- hpfi_1000m_df$HFP2009_int_3035

table(grepl("ZZ", mice_sp$Mouse_ID))

mice_sf <- st_as_sf(mice_sp, coords = c("coords.x1", "coords.x2"), crs = 3035)

table(grepl("ZZ", mice_sf$Mouse_ID))

grepl("ZZ", mice_sf$Mouse_ID)

table(mice_sf$Apodemus)

b <- as(extent(4400000, 4700000, 3100000, 3400000), 'SpatialPolygons')

human_fpi <- raster("/home/ele/git_projects/AA_Fox/input_data/tifs/HFP2009_int_3035.tif")

human_fpi_crop <- st_as_stars(crop(human_fpi, b))

human_fpi_agg_1000m <- st_as_stars(terra::aggregate(crop(human_fpi, b), fact = 10,
                                                    fun = "mean"))


## shape of federal states
boundaries <- 
    st_read("/home/ele/git_projects/AA_Fox/input_data/VG250_Bundeslaender_esri.geojson") %>% 
    st_transform(crs = st_crs(mice_sf)) %>% 
    filter(GEN %in% c("Berlin", "Brandenburg"))

theme_set(theme_void(base_family = "Open Sans", base_size = 15))
theme_update(
  legend.position = "top", legend.justification = "left", 
  axis.text = element_text(color = "black", size = rel(.8), margin = margin(rep(5, 4))),
  axis.ticks.length = unit(.4, "lines"), axis.ticks = element_line(color = "grey75"),
  plot.tag = element_text(face = "bold"),
  plot.margin = margin(rep(5, 4))
)

map_study <- 
  ggplot() +
  geom_stars(data = human_fpi_crop) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "black",
              alpha = .15) +
  ## state boundaries
  geom_sf(data = boundaries, fill = NA, color = "black") +
  ## 1000m buffer
##    geom_sf(data = mice_sf, size = 1.5, shape = 21,
##            stroke = 1.2, fill = "white", color = "white") +
##    geom_sf(data = mice_sf, size = 1.2, shape = 21,
##            stroke = .8, fill = "white", color = "black") +
    geom_sf(data = mice_sf, aes(color = human_fpi_1000m),
            shape = 16, size = 3, alpha = .7) +
    geom_sf(data = mice_sf, size = 1.2, shape = 21, stroke = 0,
            fill = ifelse(mice_sf$Apodemus%in%"Apodemus agrarius", "red", "white")) +
  ggspatial::annotation_scale(
    location = "bl", text_family = "Open Sans", text_cex = 1.2
  ) +
  ggspatial::annotation_north_arrow(location = "tr") +
  coord_sf(xlim = c(4410000, 4650000), ylim = c(3150000, 3387000)) +
  scale_fill_gradient(low = "grey30", high = "grey97", guide = "none") +
  scale_color_scico(
    palette = "batlow", begin = .1,
    name = "Human Footprint Index (2009)", limits = c(0, 50), breaks = 1:9*5, 
    guide = guide_colorsteps(barwidth = unit(18, "lines"), barheight = unit(.6, "lines"),
                             title.position = "top", title.hjust = 0, show.limits = TRUE)
  ) +
  labs(x = NULL, y = NULL)

#map_study
ggsave("map_mice.png", width = 6.6, height = 7, bg = "white", dpi = 600)

