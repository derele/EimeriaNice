library(sf)
library(scico)
library(sp)
library(raster)
library(stars)
library(tidyverse)
library(RColorBrewer)
library(patchwork)

SOTA <- read.csv("https://raw.githubusercontent.com/derele/Mouse_Eimeria_Field/master/data_products/SOTA_Data_Product.csv")


mouseIDsent <- c("AA_0692",
                 "AA_0695",
                 "AA_0699",
                 "AA_0708",
                 "AA_0724",
                 "AA_0729",
                 "AA_0731",
                 "AA_0744",
                 "AA_0754",
                 "AA_0772",
                 "AA_0773",
                 "AA_0775",
                 "AA_0800",
                 "AA_0823",
                 "AA_0827",
                 "AA_0836",
                 "AA_0846",
                 "AA_0847",
                 "AA_0850",
                 "AA_0854")

miceSent <- SOTA[SOTA$Mouse_ID%in%mouseIDsent, ]
write.csv(miceSent, "miceSentJamie.csv", row.names=FALSE)

## remove non-mus
SOTA <- SOTA[!grepl("ZZ", SOTA$Mouse_ID),]

SOTA$sent <- SOTA$Mouse_ID%in%mouseIDsent

## remove mice with missing geocoordinate information and transform
dplyr::filter(SOTA, !is.na(Longitude) & !is.na(Latitude) &
                    !is.na(HI)) ->
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
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "black",
              alpha = .15) +
  ## state boundaries
  geom_sf(data = boundaries, fill = NA, color = "black") +
    ## geom_sf(data = mice_sf, aes(fill = sent),
    ##         shape = 16, size = 4, alpha = .7) +
    geom_sf(data = mice_sf, aes(color = HI, shape = sent,
                                size=ifelse(sent, 3, 1)),
            size = 3, alpha = .7) +
  ggspatial::annotation_scale(
    location = "bl", text_family = "Open Sans", text_cex = 1.2
  ) +
  ggspatial::annotation_north_arrow(location = "tr") +
  coord_sf(xlim = c(4410000, 4650000), ylim = c(3150000, 3387000)) +
  scale_fill_gradient(low = "grey30", high = "grey97", guide = "none") +
  scale_color_distiller(
    palette = "RdBu",
    name = "", limits = c(0, 1),
    ##    guide = guide_colorbar(barwidth = unit(18, "lines"), direction="horizontal")
    guide = "none"
  ) +
  scale_shape_discrete(guide = "none")+
  labs(x = NULL, y = NULL)


HIvsHFPI <- ggplot(SOTA, aes(HI, human_fpi_1000m, shape=sent,
                             color=HI, size=ifelse(sent, 3, 1))) +
    geom_point()+
    scale_color_distiller(
        palette = "RdBu",
        name = "", limits = c(0, 1),
        ## guide = guide_colorbar(barwidth = unit(18, "lines"), direction="horizontal")
        guide="none"
    ) +
    theme_minimal()+
    scale_size_continuous(guide = "none")+
    scale_shape_discrete(position = "top")+
    labs(x = "Hybrid index", y = "Human footprint index")

#map_study

map_study + HIvsHFPI

ggsave("map_mice.png", width = 12, height = 7, bg = "white", dpi = 600)

