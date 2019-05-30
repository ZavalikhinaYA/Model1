library(tidyverse) 
library(rnoaa) 
library(lubridate) 


station_data = ghcnd_stations()
setwd("D:/R")
write.csv(station_data, file = "station_data.csv")


ai = c(32.11, 26.31, 25.64, 23.20, 18.73)
bi = c(11.30, 9.26, 9.03, 8.16, 6.59)

Kf = 300 
Qj = 1600 
Lj = 2.2
Ej = 25


station_data=read.csv('station_data.csv',header = TRUE,sep=",",dec=".")
Orenburg=data.frame(id="ORENBURG",latitude=51.77,longitude=55.09)
Orenburg_around=meteo_nearby_stations(lat_lon_df = Orenburg, station_data = station_data,
                                      limit=15, var = "all",
                                      year_min = 2007, year_max=2015)

All_Orenburg_data=tibble()
for (i  in c( 2, 3, 4)){
  temp  = meteo_tidy_ghcnd( stationid  =  Orenburg_around [["ORENBURG"]] [["id"]] [ i ], 
                            date_min = "2005-04-1", 
                            date_max = " 2015-08-31 ")
  temp  = select (temp, id , data, tavg)
  all_Orenburg_data  = rbind( all_Orenburg_data, temp)
}

all_Orenburg_data = mutate(all_Orenburg_data, month = month(date), day = day(date))
all_Orenburg_data_without0 = all_Orenburg_data

all_Orenburg_data [(all_Orenburg_data $ month  ==  4  &  all_Orenburg_data $ day <=  14), "tavg"] =  0
all_Orenburg_data [(all_Orenburg_data $ month  ==  8  &  all_Orenburg_data $ day >=  16), "tavg"] =  0

all_Orenburg_data = all_Orenburg_data %>% group_by(month)
all_Orenburg_data_without0 = all_Orenburg_data_without0 %>% group_by(month)

di  = summarize( all_Orenburg_data , di  = length( tavg [ tavg> 70 ]) / length( tavg)) [, - 1]

St  = summarize( all_Orenburg_data_without0, St  = sum (tavg [ tavg> 50 ]) / 10 / 8 )[, - 1]

Fi = ai + bi * 1.0 * St
yield = 10^6*sum(Fi*di*Kf/(Qj*Lj*(100-Ej)))
yield
