library(tidyverse)
library(magrittr)
library(maptools)
library(rgdal)
library(ggvoronoi)
#if (!require(gpclib)) install.packages("gpclib", type="source")
#gpclibPermit()         # Seems wacky, shouldn't require this.

# The projection is not adjusted but the error is minor.

plot = FALSE
airport_cn_data <- read_csv("data/airport-codes.csv")

# Presumably better would be filter keeping only "international airport" 
#using the one from the CDC dataset, but I don't have access to that.
airport_cn_data %<>%  filter(iso_country == 'CN') %>%
  filter(!(type %in% c('closed', 'heliport', 'seaplane_base' ))) %>%
  filter(type %in% c('medium_airport', 'big_airport', 'large_airport')) %>%  # Thus is a reduction
  filter(!str_detect(name, 'Air Base')) %>%
  filter(!str_detect(name, 'Air Field')) %>%
  filter(!is.na(iata_code)) %>%
  separate(coordinates, sep = ',', c('coor_lat', 'coor_lon'), convert = TRUE)

# Province shape
china_map <- rgdal::readOGR('data/cn_admbnda_adm1/chn_admbnda_adm1_ocha.shp')

adm0_china = unionSpatialPolygons(china_map, china_map@data$ADM0_EN)
adm1_china = unionSpatialPolygons(china_map, china_map@data$ADM1_EN)

# Partitioning
voronoi_tess <- voronoi_polygon(airport_cn_data,x = "coor_lon", y = "coor_lat",
                                outline = adm0_china)

tri_china = unionSpatialPolygons(voronoi_tess, voronoi_tess@data$iata_code)

if (plot){
  airport_cn_map <- ggplot() + 
    geom_point(data = airport_cn_data, aes(coor_lon, coor_lat)) +
    geom_polygon(data = fortify(voronoi_tess), aes(long, lat, group = group), 
                 alpha = .4, size = .5,  colour = 'red') +
    geom_polygon(data = adm1_china, aes(long, lat, group = group),
                 alpha = .4,size = .2,colour = 'blue') + 
    geom_polygon(data = adm1_china["Zhejiang Province"], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    geom_polygon(data = tri_china['HYN'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'orange') +
    geom_polygon(data = adm0_china, aes(long, lat, group = group),
                 alpha = .1,size = 1, colour = 'orange')
  theme(legend.position = "none")
  
  print(airport_cn_map)
}

airport_attribution <- tribble(~Province, ~airport_iata, ~attribution)
for (prov in levels(china_map@data$ADM1_EN)) {
  cksum = 0      # to test if there is no error
  for (iata in voronoi_tess@data$iata_code) {
    if (!is.na(iata)){
      inter <- raster::intersect(tri_china[iata], adm1_china[prov])
      if (!is.null(inter)){
        percent_to_iata = raster::area(inter)/raster::area(adm1_china[prov])
        cksum  = cksum + percent_to_iata
        airport_attribution <-add_row(airport_attribution, Province = prov, 
                                      airport_iata = iata, attribution = percent_to_iata)
        #print(paste(prov, "intersect with",iata, "at", percent_to_iata))
      }
    }
  }
  if (cksum < 0.999) print(paste("ERROR ATTRIBUTION -", cksum, prov))
  if (cksum > 1.001) print(paste("ERROR ATTRIBUTION +", cksum, prov))
}

if (plot) {
  airport_cn_map <- ggplot() + 
    geom_point(data = airport_cn_data, aes(coor_lon, coor_lat)) +
    geom_polygon(data = fortify(voronoi_tess), 
                 aes(long, lat, group = group),
                 alpha = .4, size = .5,  colour = 'red') +
    #geom_polygon(data = adm1_china, aes(long, lat, group = group),
    #             alpha = .4,size = .2,colour = 'blue') + 
    #geom_polygon(data = adm1_china["Zhejiang Province"], aes(long, lat, group = group),
    #             alpha = .4,size = .2, fill = 'red') +
    #geom_polygon(data = adm1_china["Tianjin Municipality"], aes(long, lat, group = group),
    #             alpha = .4,size = .2, fill = 'green') +
    #geom_polygon(data = adm1_china["Liaoning Province"], aes(long, lat, group = group),
    #             alpha = .4,size = .2, fill = 'yellow') +
    #geom_polygon(data = adm1_china["Jiangxi Province"], aes(long, lat, group = group),
    #             alpha = .4,size = .2, fill = 'orange') +
    #geom_polygon(data = adm1_china["Hunan Province"], aes(long, lat, group = group),
  #             alpha = .4,size = .2, fill = 'black') +
  #geom_polygon(data = adm1_china["Hebei Province"], aes(long, lat, group = group),
  #             alpha = .4,size = .2, fill = 'violet') +
  geom_polygon(data = adm1_china["Beijing Municipality"], aes(long, lat, group = group),
               alpha = .4,size = .2, fill = 'darkred') +
    #geom_polygon(data = adm1_china["Xinjiang Uygur Autonomous Region"], aes(long, lat, group = group),
    #             alpha = .4,size = .2, fill = 'brown') +
    geom_polygon(data = tri_china['CDE'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    geom_polygon(data = tri_china['PEK'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    geom_polygon(data = tri_china['PKX'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    geom_polygon(data = tri_china['NAY'], aes(long, lat, group = group),
                 alpha = .4,size = .2, fill = 'blue') +
    theme(legend.position = "none")
  print(airport_cn_map)
}
write.csv(airport_attribution, file ='airport_attribution.csv', row.names=FALSE)
