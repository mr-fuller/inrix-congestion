library(rgdal)
library(tmap)
library(ggmap)
library(ggplot2)
library(tidyverse)

cmp_locations <- readOGR("C:/Users/fullerm/Documents/NPMRDS/cmp2017/congestion_locations.gpkg")
npmrds_dwdh <- readOGR("C:/Users/fullerm/OneDrive - TMACOG/Postgres/CMP/Data/npmrds_dwdh.gpkg")
var_names <- tail(colnames(npmrds_dwdh@data),30)


#clean up data for faceted ggplot
tidied_cmp_locs <- broom::tidy(npmrds_dwdh)

#create an id field for joining tidied geo data back to attribute data
npmrds_dwdh$id <- row.names(npmrds_dwdh)

#join tidied geo data back to attribute data
tidied_cmp_locs <- left_join(tidied_cmp_locs, npmrds_dwdh@data)

#consolidate study periods into single column
gathered <- gather(tidied_cmp_locs,var_names[-c(30,25,20,15,10,5)],
                   key = "period",value = "dwdh")

#separate year information from study periods by splitting at last underscore in string
separated <- separate(gathered,"period",into = c("period","year"),sep = "_(?!.*_)")

# remove columns that I don't need (probably optional)
sliced <- separated[c(1:11,18:20)]
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
  
  scale_color_brewer(palette = "Purples",na.value = "grey50",
                     name = "Distance\nWeighted\nDelay\nHours",
                     labels = c("0-28.37","28.37-146.29","146.29-527.65","> 527.65","NA"))



'To cite ggmap in publications, please use:

D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R
Journal, 5(1), 144-161. URL
http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf'
