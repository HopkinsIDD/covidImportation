library(tidyverse)
library(magrittr)
library(maptools)
library(rgdal)
library(ggvoronoi)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()         # Seems wacky, shouldn't require this.

airport_cn_data <- read_csv("data/airport-codes.csv")


# Presumably better would be filter keoping only "international airport" 
#using the one from the CDC dataset, but I don't have access to that.
airport_cn_data %<>%  filter(iso_country == 'CN') %>%
    filter(!(type %in% c('closed', 'heliport', 'seaplane_base' ))) %>%
    filter(type %in% c('medium_airport', 'big_airport')) %>%  # Thus is a reduction
    filter(!str_detect(name, 'Air Base')) %>%
    separate(coordinates, sep = ',', c('coor_lat', 'coor_lon'), convert = TRUE)


# Province shape
china_map <- rgdal::readOGR('data/cn_admbnda_adm1/chn_admbnda_adm1_ocha.shp')

adm0_china = unionSpatialPolygons(china_map, china_map@data$ADM0_EN)
adm1_china = unionSpatialPolygons(china_map, china_map@data$ADM1_EN)

# Partitioning
voronoi_tess <- voronoi_polygon(airport_cn_data,x = "coor_lon", y = "coor_lat",
                                outline = adm0_china)

airport_cn_map <- ggplot() + 
  geom_point(data = airport_cn_data, aes(coor_lon, coor_lat)) +
  geom_polygon(data = fortify(voronoi_tess), 
               aes(long, lat, group = group),
               alpha = .4, size = .5,  colour = 'red') +
  geom_polygon(data = adm1_china, 
               aes(long, lat, group = group),
               alpha = .4,size = .2,colour = 'blue') + 
  theme(legend.position = "none")

print(airport_cn_map)

for (prov in china_map@data$ADM1_EN) {
  print(prov)
}




