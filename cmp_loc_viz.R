library(rgdal)
library(tmap)
library(ggmap)
library(ggplot2)
cmp_locations <- readOGR("C:/Users/fullerm/Documents/NPMRDS/cmp2017/congestion_locations.gpkg")
var_names <- tail(colnames(cmp_locations@data),40)
tmap_mode("view")
tmap_options(tmap.limits=c(facets.plot=64, facets.view=32))
qtm(shp = cmp_locations,lines.lwd = var_names[-c(40,35,30,25,20,15,10,5)], ncol= 4)
spplot(cmp_locations,var_names[-c(40,35,30,25,20,15,10,5)],at = brks,col.regions=pal)
grid.arrange(spplot(cmp_locations,var_names[1:4],at=brks,col.regions=pal),
             spplot(cmp_locations,var_names[6:9],at=brks,col.regions=pal),
             spplot(cmp_locations,var_names[11:14],at=brks,col.regions=pal),
             spplot(cmp_locations,var_names[16:19],at=brks,col.regions=pal),
             spplot(cmp_locations,var_names[21:24],at=brks,col.regions=pal),
             spplot(cmp_locations,var_names[26:29],at=brks,col.regions=pal),
             spplot(cmp_locations,var_names[31:34],at=brks,col.regions=pal),
             spplot(cmp_locations,var_names[36:39],at=brks,col.regions=pal)
)
#clean up data for faceted ggplot
tidied_cmp_locs <- broom::tidy(cmp_locations)
cmp_locations$id <- row.names(cmp_locations)
tidied_cmp_locs <- left_join(tidied_cmp_locs, cmp_locations@data)


gathered <- gather(tidied_cmp_locs,var_names[-c(40,35,30,25,20,15,10,5)],
                   key = "period",value = "dwdh")
separated <- separate(gathered,"period",into = c("period","year"),sep = "_(?!.*_)")

sliced <- separated[c(1:7,9,18,19,20)]
#change values to improve formatting of facet
sliced[sliced$period == "am_peak_dwdh","period"] <- "AM Peak (6-9AM)"
#sliced[sliced$period == "AM Peak","period"] <- "AM Peak (6-9AM)"
sliced[sliced$period == "pm_peak_dwdh","period"] <- "PM Peak (2-6PM)"
#sliced[sliced$period == "PM Peak","period"] <- "PM Peak (2-6PM)"
sliced[sliced$period == "off_peak_dwdh","period"] <- "Off Peak"
sliced[sliced$period == "midday_dwdh","period"] <- "Midday (9AM-2PM)"
#sliced[sliced$period == "Midday","period"] <- "Midday (9AM-2PM)"

#create factors of periods so plot order is the way I want it
sliced$period_f = factor(sliced$period, levels =c('AM Peak (6-9AM)','Midday (9AM-2PM)','PM Peak (2-6PM)','Off Peak'))


#get_map()

ph_basemap <- get_map(location = c(lon = -83.5254139, lat = 41.4748341), zoom = 10, maptype = "roadmap")
ggmap(ph_basemap) + 
  geom_line(data = sliced, aes(x= long,y=lat,group = group, col = cut(dwdh,quantile(dwdh,na.rm = TRUE))))+
  facet_grid(period_f~year, scales = "free", switch = "both")+
  labs(title = "TMACOG Area Congestion", y = "Time of Day", x = "Year",caption = "source: INRIX")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(), # change the theme options
        
        axis.ticks = element_blank()) +# remove axis ticks
  #scale_color_manual()
  scale_color_brewer(palette = "Purples",na.value = "grey50",
                     name = "Distance\nWeighted\nDelay\nHours",
                     labels = c("0-1.68","1.68-11.4","11.4-48.9","> 48.9","NA"))      
  #scale_colour_gradient(guide = "legend", name = "Distance Weighted Delay Hours") # legend options
#ggplot(data = sliced, aes(x= long,y=lat,group = group))+geom_line()+facet_grid(period~year)


  
  

'To cite ggmap in publications, please use:
  
  D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R
Journal, 5(1), 144-161. URL
http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf'
