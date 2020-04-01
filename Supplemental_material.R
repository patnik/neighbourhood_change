library(ggplot2)
library(plotly)
library(reshape2)
library(sf)
library(leaflet)
library(leaflet.extras) # I am using this to add full screen control
library(RColorBrewer)
library(htmlwidgets)


setwd("/Users/nikospatias/Desktop/work_np/PhD_Data/paper1/new_work/Supplemental_plots")


##### 
# temporal clustering plot
temp_cl <- read.csv("clusters_temp.csv")
temp_cl$clusters <- as.character(temp_cl$clusters)
str(temp_cl)


line_pl <- ggplot(temp_cl, aes(x = Year, y = No_grids, colour = Cluster)) +
  geom_line() + 
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("darkgreen", "midnightblue",
                                "#e70ca2", "goldenrod4", "#ed7b45",
                                "steelblue1", "#da142b", "burlywood")) +
  labs(title = "Number of grids by year and temporal cluster",
       colour = "") +
  scale_x_discrete(limits=c(temp_cl$Year)) +
  theme_minimal()


ggplotly(line_pl)



##### 
# Regional trajectories distribution
reg_traj <- read.csv("regional_frequency.csv")

cpal_2 =c("Stable affluent" = "#3b8222",
          "Ageing manual labour" = "#2c2ce6", 
          "Increasing socioeconomic diversity" = "#cb6fb9", 
          "Increasing struggling home-owners" = "#f71924", 
          "Stable multicultural urban" = "#d68c3c", 
          "Rejuvenating" = "#65d2cd", 
          "Upwarding thriving" = "#9fd65c")


# for a specific order
reg_traj$Trajectory <- factor(reg_traj$Trajectory,
                      levels=c("Stable affluent",
                               "Upwarding thriving",
                               "Increasing socioeconomic diversity",
                               "Stable multicultural urban",
                               "Rejuvenating",
                               "Ageing manual labour",
                               "Increasing struggling home-owners"))


data_wide <- dcast(reg_traj, Region ~ Trajectory, value.var="Proportion")
data_wide$Region <- as.character(data_wide$Region)
data_wide <- data_wide[order(-data_wide$`Stable affluent`, 
                             -data_wide$`Upwarding thriving`,
                             -data_wide$`Increasing socioeconomic diversity`,
                             -data_wide$`Stable multicultural urban`,
                             -data_wide$Rejuvenating,
                             -data_wide$`Ageing manual labour`,
                             -data_wide$`Increasing struggling home-owners`),]



# plot the regional graph
distribution_regions <- ggplot() + 
  geom_bar(aes(y =  Proportion, x = Region, fill = Trajectory), data = reg_traj,  stat="identity")+
  labs(title = "Distribution of neighbourhood trajectories across regions", 
       x = "", y = "", fill = "") +
  scale_x_discrete(limits=c(data_wide$Region)) +
  scale_fill_manual(values = cpal_2)+
  theme_minimal() +
  theme(
    text = element_text('Avenir Next Condensed'),
    axis.text.x=element_text(angle = 90, hjust = 1),
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_line('white', size = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.ontop = TRUE
    )



ggplotly(distribution_regions)
  

##### 
# FUA trajectories distribution
fua_traj <- read.csv("FUA_data_all.csv")

# for a specific order
fua_traj$Trajectory <- factor(fua_traj$Trajectory,
                              levels=c("Stable affluent",
                                       "Upwarding thriving",
                                       "Increasing socioeconomic diversity",
                                       "Stable multicultural urban",
                                       "Rejuvenating",
                                       "Ageing manual labour",
                                       "Increasing struggling home-owners"))


data_wide2 <- dcast(fua_traj, FUA + Region ~ Trajectory, value.var="Proportion")
data_wide2$FUA <- as.character(data_wide2$FUA)
data_wide2$Region <- as.character(data_wide2$Region)
data_wide2 <- data_wide2[order(-data_wide2$`Stable affluent`, 
                               -data_wide2$`Upwarding thriving`,
                               -data_wide2$`Increasing socioeconomic diversity`,
                               -data_wide2$`Stable multicultural urban`,
                               -data_wide2$Rejuvenating,
                               -data_wide2$`Ageing manual labour`,
                               -data_wide2$`Increasing struggling home-owners`),]

# # this is when we want to split by regions too
# data_wide2 <- data_wide2[order(data_wide2$Region,
#                               -data_wide2$`Stable affluent`,
#                              -data_wide2$`Upwarding thriving`,
#                              -data_wide2$`Increasing socioeconomic diversity`,
#                              -data_wide2$`Stable multicultural urban`,
#                              -data_wide2$Rejuvenating,
#                              -data_wide2$`Ageing manual labour`,
#                              -data_wide2$`Increasing struggling home-owners`),]


distribution_fua <- ggplot() + 
  geom_bar(aes(y =  Proportion, x = FUA, fill = Trajectory), data = fua_traj,  stat="identity")+
  labs(title = "Distribution of neighbourhood trajectories across FUAs", 
       x = "", y = "", fill = "") +
  scale_x_discrete(limits=c(data_wide2$FUA)) +
  scale_fill_manual(values = cpal_2)+
  theme_minimal() +
  theme(
    text = element_text('Avenir Next Condensed'),
    axis.text.x=element_text(angle = 90, hjust = 1),
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_line('white', size = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.ontop = TRUE
    )


ggplotly(distribution_fua) %>% 
  rangeslider()




########
#this is just for testing facets
fua_traj2 <- subset(fua_traj, Region == 'Scotland' | Region == 'North West')

data_wide3 <- dcast(fua_traj2, FUA + Region ~ Trajectory, value.var="Proportion")


library(dplyr)
data_wide4 <- subset(data_wide3, Region == 'Scotland' | Region == 'North West')
data_wide4$FUA <- as.character(data_wide4$FUA)
data_wide4$Region <- as.character(data_wide4$Region)

ggg <- data_wide4[order(data_wide4$RegionNation),]





ghhgghhggh <- data_wide4[order(data_wide4$Region,
                               -data_wide4$`Stable affluent`, 
                               -data_wide4$`Upwarding thriving`,
                               -data_wide4$`Increasing socioeconomic diversity`,
                               -data_wide4$`Stable multicultural urban`,
                               -data_wide4$Rejuvenating,
                               -data_wide4$`Ageing manual labour`,
                               -data_wide4$`Increasing struggling home-owners`),]


# this works with the facets but are not ordered within each region
ggplot() + 
  geom_bar(aes(y =  Proportion, x = FUA, fill = Trajectory), data = fua_traj2,  stat="identity")+
  labs(title = "Distribution of neighbourhood trajectories across FUAs", 
       x = "", y = "", fill = "") +
  scale_fill_manual(values = cpal_2)+
  facet_grid( ~ Region, scales = "free_x", space = "free_x") +
  theme_minimal() +
  theme(
    text = element_text('Avenir Next Condensed'),
    axis.text.x=element_text(angle = 90, hjust = 1),
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_line('white', size = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.ontop = TRUE,
    strip.placement = "outside"
    )
 




######
# create regional maps
regions_geo <- st_read("layers/UK_regions.shp")


# ### if you want to check the size of the file
# library(pryr)
# # check the size of the original shapefile
# pryr::object_size(regions_geo)


regions_geo_simplified <- st_simplify(regions_geo, dTolerance = 2000)  # 2000 m

regions_geo_simplified <- merge(x = regions_geo_simplified , y = data_wide, by.x = "geo_label",by.y = "Region", all.x = TRUE)



regions_geo_simp <- regions_geo_simplified[,c('Stable.affluent', 'Upwarding.thriving',
                        'Increasing.socioeconomic.diversity', 'Stable.multicultural.urban',
                        'Rejuvenating', 'Ageing.manual.labour',
                        'Increasing.struggling.home.owners')]

# specify the CRS needed
WGS84 = "+init=epsg:4326"
# make sure that the layer is projected on WGS84 so can be plotte with leaflet
regions_geo_simp <- st_transform(regions_geo_simp,WGS84)

# covnert sf to sp
regions_geo_simp_sp <- as(regions_geo_simp, 'Spatial')

# rename the column names
names(regions_geo_simp_sp@data)[names(regions_geo_simp_sp@data)=="Stable.affluent"] <- "Stable affluent"
names(regions_geo_simp_sp@data)[names(regions_geo_simp_sp@data)=="Upwarding.thriving"] <- "Upwarding thriving"
names(regions_geo_simp_sp@data)[names(regions_geo_simp_sp@data)=="Increasing.socioeconomic.diversity"] <- "Increasing socioeconomic diversity"
names(regions_geo_simp_sp@data)[names(regions_geo_simp_sp@data)=="Stable.multicultural.urban"] <- "Stable multicultural urban"
names(regions_geo_simp_sp@data)[names(regions_geo_simp_sp@data)=="Rejuvenating"] <- "Rejuvenating"
names(regions_geo_simp_sp@data)[names(regions_geo_simp_sp@data)=="Ageing.manual.labour"] <- "Ageing manual labour"
names(regions_geo_simp_sp@data)[names(regions_geo_simp_sp@data)=="Increasing.struggling.home.owners"] <- "Increasing struggling home owners"


my_colours <- brewer.pal(5,"YlOrRd") # rev indicates reverse order



map <- leaflet() %>% addTiles()%>% 
  addProviderTiles(providers$CartoDB.DarkMatter)

for (i in seq_along(1:7)) {
  pal <- colorQuantile(my_colours, regions_geo_simp_sp[[i]], n = 5)

  
  
  map <- map %>% 
    addPolygons(data = regions_geo_simp_sp,
                fillColor = ~pal(regions_geo_simp_sp[[i]]),
                weight = 0.4,
                opacity = 0.8,
                color = "black",
                dashArray = "3",
                fillOpacity = 0.7,
                popup = paste("Proportion of ", names(regions_geo_simp_sp@data[i]), "neighbourhoods",":" , round(regions_geo_simp_sp[[i]],2), "<br>",
                              "Region: ", regions_geo_simplified$name, "<br>"),
                group = names(regions_geo_simp_sp@data[i]),
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE))   %>%
    addLayersControl(overlayGroups = c(names(regions_geo_simp_sp@data)),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c('Upwarding thriving', 'Increasing socioeconomic diversity', 'Stable multicultural urban',
                'Rejuvenating', 'Ageing manual labour', 'Increasing struggling home owners'))
}

to_print <- map %>%
  addLegend(colors = c("#FFFFB2",  "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
            labels= c("lowest Quintile", "","","", "highest Quintile"),
            position = "bottomleft",
            title = title = paste ("Proportion of neighbourhood", "<br>", "trajectories by Region (Quintiles)")) %>% 
  addFullscreenControl()



# # this is not easily modified

# pal2 <- colorQuantile(my_colours, regions_geo_simp_sp[[1]], n = 5)
# to_print <- map %>% 
#   addLegend(pal = pal2,
#             values  = regions_geo_simp_sp[[1]],
#             labels= c("less", "","","", "more"),
#             position = "bottomleft",
#             title = "Quintiles")



#saveWidget(to_print, file="map.html", title = "map test", selfcontained=TRUE)



######
# create FUA maps
fua_geo <- st_read("layers/UK_FUA_BNG.shp")


fua_geo <- merge(x = fua_geo , y = data_wide2, by.x = "fuaname",by.y = "FUA", all.x = TRUE)



fua_geo_simp <- fua_geo[,c('Stable.affluent', 'Upwarding.thriving',
                                              'Increasing.socioeconomic.diversity', 'Stable.multicultural.urban',
                                              'Rejuvenating', 'Ageing.manual.labour',
                                              'Increasing.struggling.home.owners')]

# specify the CRS needed
WGS84 = "+init=epsg:4326"
# make sure that the layer is projected on WGS84 so can be plotte with leaflet
fua_geo_simp <- st_transform(fua_geo_simp,WGS84)

# covnert sf to sp
fua_geo_simp_sp <- as(fua_geo_simp, 'Spatial')

# rename the column names
names(fua_geo_simp_sp@data)[names(fua_geo_simp_sp@data)=="Stable.affluent"] <- "Stable affluent"
names(fua_geo_simp_sp@data)[names(fua_geo_simp_sp@data)=="Upwarding.thriving"] <- "Upwarding thriving"
names(fua_geo_simp_sp@data)[names(fua_geo_simp_sp@data)=="Increasing.socioeconomic.diversity"] <- "Increasing socioeconomic diversity"
names(fua_geo_simp_sp@data)[names(fua_geo_simp_sp@data)=="Stable.multicultural.urban"] <- "Stable multicultural urban"
names(fua_geo_simp_sp@data)[names(fua_geo_simp_sp@data)=="Rejuvenating"] <- "Rejuvenating"
names(fua_geo_simp_sp@data)[names(fua_geo_simp_sp@data)=="Ageing.manual.labour"] <- "Ageing manual labour"
names(fua_geo_simp_sp@data)[names(fua_geo_simp_sp@data)=="Increasing.struggling.home.owners"] <- "Increasing struggling home owners"


my_colours <- brewer.pal(5,"YlOrRd") # rev indicates reverse order



map2 <- leaflet() %>% addTiles()%>% 
  addProviderTiles(providers$CartoDB.DarkMatter) 

for (i in seq_along(1:7)) {
  pal <- colorQuantile(my_colours, fua_geo_simp_sp[[i]], n = 5)
  
  
  
  map2 <- map2 %>% 
    addPolygons(data = fua_geo_simp_sp,
                fillColor = ~pal(fua_geo_simp_sp[[i]]),
                weight = 0.4,
                opacity = 0.8,
                color = "black",
                dashArray = "3",
                fillOpacity = 0.7,
                popup = paste("Proportion of ", names(fua_geo_simp_sp@data[i]), "neighbourhoods",":" , round(fua_geo_simp_sp[[i]],2), "<br>",
                              "FUA: ", fua_geo$fuaname, "<br>"),
                group = names(fua_geo_simp_sp@data[i]),
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE))   %>%
    addLayersControl(overlayGroups = c(names(fua_geo_simp_sp@data)),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c('Upwarding thriving', 'Increasing socioeconomic diversity', 'Stable multicultural urban',
                'Rejuvenating', 'Ageing manual labour', 'Increasing struggling home owners'))
}

to_print2 <- map2 %>%
  addLegend(colors = c("#FFFFB2",  "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
            labels= c("lowest Quintile", "","","", "highest Quintile"),
            position = "bottomleft",
            title = paste ("Proportion of neighbourhood", "<br>", "trajectories by FUA (Quintiles)")) %>% 
  addFullscreenControl()



