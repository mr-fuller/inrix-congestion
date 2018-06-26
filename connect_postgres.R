library(rgdal)

dsn <- "PG:dbname='cmpfy2018' user=postgres password=laskdjfhiweiofoies"

df <- readOGR(dsn=dsn,"congestion_locations")

