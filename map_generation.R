#-----------you need to install the following packages. this only needs to be done once.
install.packages(c('sf', 'foreign', 'tidyverse', 'stringi', 'lwgeom'))

#-----------initialize libraries. This needs to be done for each new R session 
library(sf)
library(foreign)
library(tidyverse)
library(lwgeom)
library(stringi)

options(stringsAsFactors = FALSE)

#-----------download files
#pick a region and download/unzip the .shp.zip file: http://download.geofabrik.de/

#-----------set the working directory to wherever you unzipped the downloaded files to
setwd("C:/Users/Adam/Documents/budapest_map/hungary_map")


#-----------set some basic info about the city you're mapping
city <- "budapest"
lat <- 47.506708 #center point latitude
long <- 19.046259 #center point longitude
rad <- 7000 #radius, in meters, around the center point to map
crs <- 102013 #ESRI projection for mapping. I found mine here: https://spatialreference.org/ref/esri/europe-albers-equal-area-conic/ 102013 will give good results for Europe.


#-----------set up the road types you want to plot and what colors they should be
plottypes <-  c('Utca', 'Tér', 'Út', 'Körút', 'Rakpart', 'Lépcső‘', 'Híd', 'Sétány', 'Vizek')
plotcolors <-  c('Utca' = '#5EC3E1', 'Tér' = '#FFD035', 'Út' ='#4EB480', 'Körút' = '#2E968C', 'Rakpart' = '#EA4E66',
                'Híd' = '#0D7ABF', 'Sétány' = '#F3902C', 'Egyéb' = '#C9C9C9', 'Vizek' = '#F39CA6')

#-----------get to plotting
#import  road geography
filename <- "gis_osm_roads_free_1"
allroads_raw <- read_sf(".", filename)


#subset the roads into a circle.
pt <- data.frame(lat = lat, long = long)
pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>%  st_transform(crs) 
circle <- st_buffer(pt, dist = rad)
circle <- circle %>% st_transform(st_crs(allroads_raw))
allroads_circle <- st_intersection(circle, allroads_raw)

#waters
waters <- read_sf(".", "gis_osm_water_a_free_1")
waters_sub <- st_intersection(circle, waters) 

#remove unnamed footpaths
allroads_circle <- allroads_circle[!(allroads_circle$fclass  == "footway" & is.na(allroads_circle$name)),]

#add in length 
allroads$len <- st_length(allroads)

#-----derive road suffixes-----
allroads_circle$suffix <- substr(allroads_circle$name, stri_locate_last(allroads_circle$name, regex = " ")[, 1] + 1,  nchar(allroads_circle$name)) %>%
  tolower()

mapping <- c(
  "utca" = "utca",
  "tér" = "tér",
  "út" = "út",
  "körút" = "körút",
  "rakpart" = "rakpart",
  "lépcső" = "egyéb",
  "híd" = "híd",
  "sétány" = "sétány",
  "útja" = "út",
  "köz" = "utca",
  "aluljáró" = "egyéb",
  "udvar" = "egyéb",
  "park" = "egyéb",
  "tere" = "tér",
  "lánchíd" = "híd",
  "fasor" = "utca",
  "alagút" = "utca"
  
)

mapping <- as.data.frame(mapping)
mapping <- rownames_to_column(mapping)
colnames(mapping) <- c("suffix", "TYPE")


allroads_mapped <- merge(allroads_circle, mapping, by = "suffix")
allroads_mapped$TYPE <- str_to_title(allroads_mapped$TYPE)

allroads <- allroads_mapped


#--------uncomment and run this code to get the top roads by length.
#--------i usually run this to decide what road types to plot
#plottype <- allroads %>% select(TYPE,len)
#plottype$geometry <- NULL
#plottype <- subset(plottype, !is.na(TYPE))
#plottype <- plottype %>% group_by(TYPE) %>% summarise(Length = sum(len)) %>% arrange(-Length) 


#put other roads into their own dataframe
#allroads$TYPE[!(allroads$TYPE %in% plottypes)] <- "Egyéb"
#otherroads <- allroads[(allroads$TYPE  == "Other"),]
#allroads <- allroads[(allroads$TYPE  != "Other"),]

#plot it
blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())

ggplot() + blankbg + theme(panel.grid.major = element_line(colour = "transparent")) + 
  geom_sf(data=otherroads, size = .8, aes(color=TYPE)) + 
  geom_sf(data=allroads, size =1, aes(color=TYPE)) +
  scale_color_manual(values = plotcolors, guide = "legend") 


map_des <- ggplot() + 
  blankbg + theme(panel.grid.major = element_line(colour = "transparent")) +
  geom_sf(data=waters_sub, fill = '#F39CA6', color = '#F39CA6', alpha = .2)+
  geom_sf(data=allroads, size =1, aes(color=TYPE)) +
  scale_color_manual(values = plotcolors, guide = "legend") 

save_path <- ("C:/Users/Adam/Documents/budapest_map/exports")

ggsave(paste0(city,"3", ".png"),
       plot = map_des,
       path = save_path,
       scale = 1,
       width = 24,
       height = 36,
       units = "in",
       dpi = 500)

