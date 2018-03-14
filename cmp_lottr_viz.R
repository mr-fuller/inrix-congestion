library(rgdal)
library(tidyverse)
library(tmap)
library(ggmap)
library(ggplot2)
cmp_locations <- readOGR("C:/Users/Michael/OneDrive - Toledo Metropolitan Area Council of Governments/Postgres/CMP/Data/congestion_lottr.gpkg")
var_names <- colnames(cmp_locations@data)[4:43]


#clean up data for faceted ggplot
tidied_cmp_locs <- broom::tidy(cmp_locations)

#create an id field for joining tidied geo data back to attribute data
cmp_locations$id <- row.names(cmp_locations)

#join tidied geo data back to attribute data
tidied_cmp_locs <- left_join(tidied_cmp_locs, cmp_locations@data)

#consolidate study periods into single column
gathered <- gather(tidied_cmp_locs,var_names[-c(39,34,29,25,20,15,10,5)],
                   key = "period",value = "lottr")

#separate year information from study periods by splitting at last underscore in string
separated <- separate(gathered,"period",into = c("period","year"),sep = "_(?!.*_)")

# remove columns that I don't need (probably optional)
sliced <- separated[c(1:7,18:28)]
#change values to improve formatting of facet
sliced[sliced$period == "am_peak_lottr","period"] <- "AM Peak (6-9AM) M-F"
#sliced[sliced$period == "AM Peak","period"] <- "AM Peak (6-9AM)"
sliced[sliced$period == "pm_peak_lottr","period"] <- "PM Peak (2-6PM) M-F"
#sliced[sliced$period == "PM Peak","period"] <- "PM Peak (2-6PM)"
sliced[sliced$period == "off_peak_lottr","period"] <- "Off Peak"
sliced[sliced$period == "midday_lottr","period"] <- "Midday (9AM-2PM) M-F"
#sliced[sliced$period == "Midday","period"] <- "Midday (9AM-2PM)"

#create factors of periods so plot order is the way I want it
sliced$period_f = factor(sliced$period, levels =c('AM Peak (6-9AM) M-F','Midday (9AM-2PM) M-F','PM Peak (2-6PM) M-F','Off Peak'))


#get_map()

ph_basemap <- get_map(location = c(lon = -83.5254139, lat = 41.4748341), zoom = 10, maptype = "roadmap")

ggmap(ph_basemap) + 
  geom_line(data = sliced, aes(x= long,y=lat,group = group, col = cut(lottr,c(1,1.25,1.5,1.75,2,4))))+
  facet_grid(period_f~year, scales = "free", switch = "both")+
  labs(title = "TMACOG Area Congestion", y = "Time of Day", x = "Year",caption = "source: INRIX")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(), # change the theme options
        
        axis.ticks = element_blank()) +# remove axis ticks
  
  scale_color_brewer(palette = "RdYlGn", direction = -1,
                     na.value = "grey50",
                     #breaks =c(0,1,1.25,1.5,1.75,2),
                     name = "Travel\nTime\nReliability"
                     #labels = c("1-1.25","1.25-1.50","1.50-1.75", "1.75-2", "> 2", "NA")
                     )



'To cite ggmap in publications, please use:

D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R
Journal, 5(1), 144-161. URL
http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf'
