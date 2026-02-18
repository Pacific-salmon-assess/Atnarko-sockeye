# The go to spot for fire perimeters is the National Burned Area Composite. You’ll find every agency and satellite mapped fire here going back to the 70’s (but data is most reliable from ~1985 onwards). There’s also metadata associated with each fire such as ignition cause.
# 
#  https://cwfis.cfs.nrcan.gc.ca/datamart/metadata/nbac
# 
# NBAC is a good resource, but it has some discrepancies with the provincial dataset, so I usually look at both:
#   https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-historical
# 
# I'm not sure what other disturbances you're considering, but you might also look at timber harvest history:
#   https://catalogue.data.gov.bc.ca/dataset/harvested-areas-of-bc-consolidated-cutblocks-
# 
# Or check out what events have been documented in the area from the Canadian Landslide Database: https://zenodo.org/records/17219072


buffer <- 25000

library(dbplyr)
library(bcdata)
library(dplyr)
library(bcmaps)
library(sf)
library(sp)
library(ggplot2)
library(ggspatial) # this is for adding the arrows
# library(rgdal) #use this for data conversion
library(ggrepel) # to offset labels using geom_sf_label_repel  --> Not done here
library(riverdist) # to snap points to River --> Not done here
# library(bcmapsdata)
library(viridis)
library(ggnewscale)
library(tidyr)
library(cowplot)

pfma <- st_read("data/spatial/PFMA/DFO_PFMA_SUBAREAS_SP/DFO_SBAREA_polygon.shp")
nux <- st_read("data/spatial/Nuxalk/CCFN_NX_SpatialPlanning_Area.shp")
wildfires <- st_read("data/covariates/NBAC Wildfire/NBAC_1972to2024_20250506.shp")

cc_pfma <- pfma[pfma$MGMT_AREA>=6 & pfma$MGMT_AREA<=10,]
catch_area <- cc_pfma %>% st_buffer(dist = 20000) %>% st_bbox() %>% st_as_sfc()
cc_extent <- cc_pfma %>% st_buffer(dist = 20000) %>% st_bbox()
nux_extent <- nux %>% st_buffer(dist = 1000) %>% st_bbox()

plot_area_nux <- cc_extent %>%
  st_bbox() %>%                 # Turn into a square
  st_as_sfc(crs=st_crs(nux))
st_crs(plot_area_nux) <- st_crs(nux)

plot_area_nux2 <- nux_extent %>%
  st_bbox() %>%                 # Turn into a square
  st_as_sfc(crs=st_crs(nux))
st_crs(plot_area_nux2) <- st_crs(nux)
plot_area_all <- st_union(plot_area_nux, plot_area_nux2) %>%
  st_bbox() %>%
  st_as_sfc()

bchres <- bc_bound_hres()
bc_fish <- bchres %>% st_intersection(plot_area_all)
cc_sub <- cc_pfma %>% st_difference(bchres)
cc_sub_union <- cc_sub %>% st_union() %>% st_sf() # collapse cc_sub to one polygon

coast_line <- bchres %>% st_intersection(plot_area_all)
catch_area <- cc_pfma %>% st_intersection(plot_area_all) %>% st_difference(bchres)

bc_fish <- bchres %>% st_intersection(plot_area_all)

alaska <- st_transform(USAboundaries::us_states(states = "Alaska", resolution = "high"),st_crs(plot_area_nux))
alaska_full <- alaska %>% st_intersection(bchres)
alaska <- alaska  %>% st_intersection(plot_area_all)
catch_area_union <- catch_area %>% group_by(MGMT_AREA) %>% summarize(geometry = st_union(geometry)) %>% st_sf() # collapse cc_sub to one polygon


nux_shp <- nux %>% sf::st_intersection(plot_area_all)
nux_shp_union <- nux_shp %>% st_union() %>% st_sf() # collapse cc_sub to one polygon

nux_area <- nux_shp %>% st_buffer(dist = 1000) %>% st_bbox() %>% st_as_sfc()
nux_fish <- bc_fish %>% sf::st_intersection(nux_area)

nux_map <- ggplot() +
  geom_sf(data = nux_shp_union, fill = "lightblue1") +
  geom_sf(data = bc_fish, fill = "white") +
  geom_sf(data = nux_shp_union, fill = "yellow",alpha=0.2) +
  coord_sf(expand = FALSE) +                                    #Expands box to axes
  xlab('Longitude') + ylab('Latitude') +                        #Axis titles
  annotation_scale(location = "bl", width_hint = 0.5) +         #Rose Compass
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(1,"cm"), width = unit(1, "cm"))+
  theme(panel.background = element_rect('lightblue1'), panel.grid.major = element_line('lightblue1'),legend.position="top",legend.box.just="center",legend.box="horizontal",legend.justification = "center",legend.key.size=unit(1, "lines"),legend.margin = margin(c(0,0,0,-1),unit="lines"),legend.title=element_text(size=6),legend.text = element_text(size=5))
# ggsave(filename="~/Google Drive/SFU REM/Salmon/Nuxalk salmon/Figures/nuxalk map.jpeg",plot=nux_map,units="in",width=6,height=6)

#bcdc_get_record("https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-rivers")
rivers_in_plot_area <- bcdc_query_geodata('f7dac054-efbf-402f-ab62-6fc4b32a619e') %>%
  #filter(STREAM_ORDER %in% c(3,4,5)) %>%  #Defines as only streams order 3,4,5 (too many including 1 &2)
  filter(INTERSECTS(nux_shp_union)) %>%      # not sure about this line
  collect() %>%                           #Extracts the data
  st_intersection(nux_shp_union)             #Where it intersects with plot line

#bcdc_get_record("https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-watersheds-groups")
watersheds <- bcdc_query_geodata('51f20b1a-ab75-42de-809d-bf415a0f9c62') %>%
  filter(INTERSECTS(nux_shp_union)) %>%      # not sure about this line
  collect() %>%                           #Extracts the data
  st_intersection(nux_shp_union)             #Where it intersects with plot line

bcdc_get_record("https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-stream-network")
all_streams <- bcdc_query_geodata("92344413-8035-4c08-b996-65a9b3f62fca") %>%
  filter(INTERSECTS(nux_shp_union)) %>%
  filter(STREAM_ORDER%in% c(3,4,5,6)) %>%
  collect() %>%                           #Extracts the data
  st_intersection(nux_shp_union)             #Where it intersects with plot line

bella_coola <- rivers_in_plot_area[rivers_in_plot_area$GNIS_NAME_1%in%c("Bella Coola River","Atnarko River","Talchako River") & rivers_in_plot_area$WATERSHED_GROUP_CODE%in%c("BELA","ATNA"),]

coast_line <- bchres %>% st_intersection(plot_area_all)

#Below is to get a dataset of lakes in plot box
lakes_in_plot_area <- bcdc_query_geodata("freshwater-atlas-lakes") %>%
  filter(INTERSECTS(nux_shp_union)) %>%
  collect() %>%
  st_intersection(nux_shp_union)

# Ocean colouring - this doesn't work well  because the resolution isn't the same
ocean_colour <- bc_neighbours(ask=FALSE) %>% 
  filter(type=="Ocean") %>% 
  st_intersection(plot_area_nux)
ocean <- bc_neighbours(ask=FALSE) %>% 
  filter(type=="Ocean")

nux_points <- sf::st_centroid(nux_shp_union)
nux_points <- cbind(nux_shp_union, st_coordinates(st_centroid(nux_points$geometry))*c(1.005,1.175))
nux_points$name <- "Bella Coola & Atnarko Watersheds"

bella_coola_watershed <- watersheds[watersheds$WATERSHED_GROUP_CODE %in% c("BELA","ATNA"),]
watersheds_union <- bella_coola_watershed %>% st_union() %>% st_sf() # collapse cc_sub to one polygon

wildfires <- st_transform(wildfires, sf::st_crs(watersheds_union))  # UTM zone for Vancouver region

nuxalk_wildfires <- wildfires %>% sf::st_intersection(watersheds_union)


atnarko_fire_ts <- nuxalk_wildfires %>%
  group_by(YEAR, FIRECAUS) %>%
  summarise(hectares = sum(ADJ_HA, na.rm = TRUE),.groups = "drop") %>%
  complete(YEAR = full_seq(YEAR, 1),fill = list(hectares = 0)) %>%
  arrange(YEAR) %>%         # ensure correct time order
  # group_by(FIRECAUS) %>%
  mutate("cumulative_area" = cumsum(hectares))

saveRDS(atnarko_fire_ts,file="data/covariates/wildfire_bela_atna_timeseries.rds")

nux_all <- ggplot() +
  geom_sf(data = nux_shp_union, fill = "lightblue1") +
  geom_sf(data = bc_fish, fill = "white") +
  geom_sf(data = coast_line, fill = "grey90") +                 #Plot coastline
  # geom_sf(data=bec, aes(fill=ZONE,col=ZONE), alpha=0.8) +
  geom_sf(data = rivers_in_plot_area, colour = "#1f78b4") +  #Plot Rivers
  geom_sf(data = all_streams, colour = "#1f78b4",lwd=0.1) +  #Plot Rivers
  geom_sf(data = bella_coola, colour = "#1f78b4") +
  geom_sf(data = lakes_in_plot_area, fill = "#1f78b4",colour=NA) +     #Plot Lakes
  geom_sf(data = nux_shp_union, colour = "black",fill=NA,alpha=0.2) +
  geom_sf(data = plot_area_all, alpha = 0,colour='black') +        #Plot area box
  geom_sf(data = watersheds_union, fill = "orange4",alpha=0.4) +
  geom_sf(data = nuxalk_wildfires, alpha = 1,colour='purple4',fill="purple4") +        #Plot area box
  coord_sf(expand = FALSE) +                                    #Expands box to axes
  xlab('Longitude') + ylab('Latitude') +                        #Axis titles
  geom_text(data=nux_points,aes(x=X,y=Y,label=name)) +
  annotation_scale(location = "bl", width_hint = 0.5) +         #Rose Compass
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(1,"cm"), width = unit(1, "cm"))+
  theme(panel.background = element_rect('lightblue1'), panel.grid.major = element_line('lightblue1'),legend.position="top",legend.box.just="center",legend.box="horizontal",legend.justification = "center",legend.key.size=unit(1, "lines"),legend.margin = margin(c(0,0,0,-1),unit="lines"),legend.title=element_text(size=6),legend.text = element_text(size=5))

bc_neigh <- bc_neighbours(ask=FALSE)
bc_neigh <- bc_neigh[bc_neigh$name%in%c("Alaska"),]
bc_map <- ggplot() +
  geom_sf(data = bc_bound(ask=FALSE), fill = "grey10",colour=NA) +
  geom_sf(data=bc_neigh, fill='grey50',colour=NA) +
  geom_sf(data = nux_shp_union, fill = "white",alpha=1) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill=adjustcolor("white",0.90), colour="black")) +
  theme(legend.justification = c(0, 1),legend.position = c(0, .95)) +
  theme(text = element_text(family = "Futura-Medium"),legend.title = element_text(family = "Futura-Bold", size = 10),legend.text = element_text(family = "Futura-Medium", size = 10))
bc <- bc_map + layer_spatial(data=plot_area_all,fill = "tomato", colour = "grey70",alpha=0.5)

nux_inset <- ggdraw(nux_all) +
  draw_plot({
    bc},
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.118, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.761,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.2, 
    height = 0.2)
nux_inset
ggsave('Figures/bella_atnarko_watersheds.jpeg',plot=nux_inset,width = 6, height = 7,units='in',dpi=800)
