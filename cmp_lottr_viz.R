library(gdal)
library(tidyverse)
library(tmap)
library(ggmap)
library(ggplot2)

var_names <- c("am_peak_lottr_2017", "midday_lottr_2017",  "pm_peak_lottr_2017", "lottr_we_2017",
               "am_peak_lottr_2016", "midday_lottr_2016", "pm_peak_lottr_2016", "lottr_we_2016",
               "am_peak_lottr_2015", "midday_lottr_2015", "pm_peak_lottr_2015", "lottr_we_2015",
               "am_peak_lottr_2014", "midday_lottr_2014", "pm_peak_lottr_2014", "lottr_we_2014",
               "am_peak_lottr_2013", "midday_lottr_2013", "pm_peak_lottr_2013", "lottr_we_2013",
               "am_peak_lottr_2012", "midday_lottr_2012", "pm_peak_lottr_2012", "lottr_we_2012")

dsn <- "PG:dbname='cmpfy2018' user=postgres password=notmyactualpassword"

cmp_locations <- readOGR(dsn=dsn,"congestion_lottr")

#clean up data for faceted ggplot
tidied_cmp_locs <- broom::tidy(cmp_locations)

#create an id field for join
cmp_locations$id <- row.names(cmp_locations)

#join tidied geo data back to attribute data
tidied_cmp_locs <- left_join(tidied_cmp_locs, 
                             cmp_locations@data)

#consolidate study periods into single column
gathered <- gather(tidied_cmp_locs,var_names, 
                   key = "period",value = "lottr")

# separate year information from study periods 
# by splitting at last underscore in string
separated <- separate(gathered,"period",
                      into = c("period","year"),
                      sep = "_(?!.*_)")

#change values to improve formatting of facet
separated[separated$period == 
          "am_peak_lottr","period"] <- "AM Peak (6-10AM) M-F"
separated[separated$period == 
          "pm_peak_lottr","period"] <- "PM Peak (4-8PM) M-F"
separated[separated$period == 
          "lottr_we","period"] <- "Weekend (6AM-8PM, Sat & Sun)"
separated[separated$period == 
          "midday_lottr","period"] <- "Midday (10AM-4PM) M-F"

#create factors of periods so plot order is the way I want it
separated$period_f = factor(separated$period, 
                            levels =c('AM Peak (6-10AM) M-F',
                                      'Midday (10AM-4PM) M-F',
                                      'PM Peak (4-8PM) M-F',
                                      'Weekend (6AM-8PM, Sat & Sun)'))

#get map
ph_basemap <- get_map(location = c(lon = -83.5249231,
                                   lat = 41.5900929), 
                      zoom = 10, maptype = "terrain")

ggmap(ph_basemap) + 
  geom_line(data = separated, 
            aes(x= long,y=lat,group = group, 
            col = cut(lottr,c(1,1.25,1.5,1.75,2,4))))+
  facet_grid(period_f~year, scales = "free", switch = "both")+
  labs(title = "TMACOG Area Passenger Vehicle Reliability", 
       y = "Time of Day", x = "Year",
       caption = "source: INRIX")+
# change the theme options
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) +# remove axis ticks
  
  scale_color_brewer(palette = "RdYlGn", direction = -1, 
                     na.value = "grey50",
  name = "Difficulty\nBased on\nTravel\nTime\nReliability\n(LOTTR)"
                     )


'To cite ggmap in publications, please use:

D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R
Journal, 5(1), 144-161. URL
http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf'
