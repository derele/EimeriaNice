library(sf)
library(scico)
library(sp)
library(raster)
library(stars)
library(tidyverse)
library(RColorBrewer)
library(patchwork)

SOTA <- read.csv("https://raw.githubusercontent.com/derele/Mouse_Eimeria_Field/master/data_products/SOTA_Data_Product.csv")

## remove non-mus
## SOTA <- SOTA[!grepl("ZZ", SOTA$Mouse_ID),]

## remove mice with missing geocoordinate information and transform
dplyr::filter(SOTA, !is.na(Longitude) & !is.na(Latitude) &
                    !is.na(HI) &
                    Year > 2014) ->
    SOTA

SOTA %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4236) %>%
    st_transform(crs = 3035) ->
mice_sp 


## extract human fpi around each mouse

b <- as(extent(4400000, 4700000, 3100000, 3400000), 'SpatialPolygons')

human_fpi <- raster("/home/ele/git_projects/AA_Fox/input_data/tifs/HFP2009_int_3035.tif")

human_fpi_crop <- st_as_stars(crop(human_fpi, b))

human_fpi_agg_1000m <- st_as_stars(terra::aggregate(crop(human_fpi, b), fact = 10,
                                                    fun = "mean"))

hpfi_1000m_df <- raster::extract(human_fpi, mice_sp, buffer = 1000,
                                 fun = mean, na.rm = TRUE, sp = TRUE)

mice_sp$human_fpi_1000m <- hpfi_1000m_df$HFP2009_int_3035

SOTA$human_fpi_1000m <- hpfi_1000m_df$HFP2009_int_3035


mice_sf <- st_as_sf(mice_sp, coords = c("coords.x1", "coords.x2"), crs = 3035)

## shape of federal states
boundaries <- 
    st_read("/home/ele/git_projects/AA_Fox/input_data/VG250_Bundeslaender_esri.geojson") %>% 
    st_transform(crs = st_crs(mice_sf)) %>% 
    filter(GEN %in% c("Berlin", "Brandenburg"))

theme_set(theme_void(base_family = "Open Sans", base_size = 15))
theme_update(
  legend.position = "bottom", legend.justification = "left", 
  axis.text = element_text(color = "black", size = rel(.8), margin = margin(rep(5, 4))),
  axis.ticks.length = unit(.4, "lines"), axis.ticks = element_line(color = "grey75"),
  plot.tag = element_text(face = "bold"),
  plot.margin = margin(rep(5, 4))
)


map_study <- 
  ggplot() +
    geom_stars(data = human_fpi_crop) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf,
                  ymax = Inf), fill = "black",
              alpha = .15) +
  ## state boundaries
    geom_sf(data = boundaries, fill = NA, color = "black") +
    geom_sf(data = mice_sf, color = "white",
            size = 3.6) +
    geom_sf(data = mice_sf, color = "black",
            size = 3.2) +
    geom_sf(data = mice_sf, aes(color = HI),
            size = 3) +
    ggspatial::annotation_scale(
                   location = "bl", text_family = "Open Sans",
                   text_cex = 1.2) +
  ggspatial::annotation_north_arrow(location = "tr") +
  coord_sf(xlim = c(4410000, 4650000),
           ylim = c(3150000, 3387000)) +
  scale_fill_gradient(low = "grey30", high = "grey97",
                      guide = "none") +
  scale_colour_gradient(low = "blue", high="red",
                        guide = "none") +
  scale_shape_discrete(guide = "none")+
  labs(x = NULL, y = NULL)


HIvsHFPI <- ggplot(SOTA, aes(HI, human_fpi_1000m, color=HI)) +
    geom_point(color = "white", size = 3.6) +
    geom_point(color = "black", size = 3.2) +
    geom_point(size=3)+
    scale_colour_gradient(low = "blue", high="red",
                          guide = "none") +
    theme_minimal()+
    stat_smooth()+
    labs(x = "Hybrid index", y = "Human footprint index")


HIvsHFPIScale <- ggplot(SOTA, aes(HI, human_fpi_1000m, color=HI)) +
    geom_point(color = "white", size = 3.6) +
    geom_point(color = "black", size = 3.2) +
    geom_point(size=3)+
    scale_colour_gradient(low = "blue", high="red") +
    theme_minimal()+
    stat_smooth()+
    labs(x = "Hybrid index", y = "Human footprint index")




map_study + HIvsHFPI

ggsave("Heisen_map_mice.png", width = 12, height = 7, bg = "white", dpi = 600)

map_study + HIvsHFPIScale

ggsave("Heisen_map_mice.svg")


##  scale_colour_gradient(low = "blue", high="red")  is better!

## ##  color palette for HI from Finn
## r <- c(0,    64, 128, 179, 217, 255)
## g <- c(0,    12,  25,  25,  12,   0)
## b <- c(255, 249, 243, 191,  95,   0)

## beach <- function (n, name = c("beach.colors")){
##     beach.colors = rgb(r,g,b,maxColorValue = 255)
##     name = match.arg(name)
##     orig = eval(parse(text = name))
##     rgb = t(col2rgb(orig))
##     temp = matrix(NA, ncol = 3, nrow = n)
##     x = seq(0, 1, , length(orig))
##     xg = seq(0, 1, , n)
##     for (k in 1:3) {
##         hold = spline(x, rgb[, k], n = n)$y
##         hold[hold < 0] = 0
##         hold[hold > 255] = 255
##         temp[, k] = round(hold)
##     }
##     palette = rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
##     palette
## }
