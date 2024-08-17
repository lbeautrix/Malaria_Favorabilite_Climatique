############################################################
#  Favorabilité environmentale pour la transmission de la  #
#         malaria par Anopheles gambiae au Rwanda          #
#                 Préparation des données                  #                       
############################################################

# Auteur : Louis Beautrix
# Date : 17 août 2024

### Packages ###
library(tidyverse)

### Données brutes ###
temp_min = read_csv('data/raw_data/Daily_Minimum_temperature.csv')      #1983 - 2017
temp_max = read_csv2('data/raw_data/Daily_maximum_temp.csv')            #1983 - 2017
rain = read_csv2('data/raw_data/Rainfall_daily_by_one_STNDistrict.csv') #1981 - 2017

### Localisation des stations ###
## Coordonnées
temp_min$Gihinga_Kamonyi[1] = 29.964 #Corrections des coordonnées
temp_min$Gihinga_Kamonyi[2] = -2.236
temp_min$Nyamata_Burera[1] = 30.105
temp_min$Nyamata_Burera[2] = -2.109

co_wst = temp_min[c(1,2),]
co_wst$ID[2] = 'LAT'
co_wst = co_wst %>% column_to_rownames(var='ID')
wst_co = as.data.frame(t(column_to_rownames(wst_co,'...1')))
write.csv2(co_wst,'data/input/weather_station_coordinate.csv')

## Altitude
wst_alt = read.xlsx('GIS/output/wst_altitude.xls',1)
names(wst_alt)[5] = 'Altitude'

### Préparation des données ###
## Températures
temp_min = as.data.frame(temp_min[c(-1,-2),])
temp_max$DATE = as.Date(temp_max$DATE,format = '%d-%m-%Y')
temp_min$ID = as.Date(temp_max$DATE)
temp_min = temp_min %>% rename(DATE = ID)

temp_mean = temp_min
for (i in 1:nrow(temp_min)){
  for (j in 2:ncol(temp_min))
    temp_mean[i,j] = ((temp_max[i,j]+temp_min[i,j])/2)
}

write.csv2(x = temp_max,file = 'data/input/daily_max_temp.csv',row.names = FALSE)
write.csv2(x = temp_min,file = 'data/input/daily_min_temp.csv',row.names = FALSE)
write.csv2(x = temp_mean,file = 'data/input/daily_avg_temp.csv',row.names = FALSE)

## Dataframe final
#Température

temp_min$DATE = as.Date(temp_min$DATE,format='%d-%m-%Y')
temp_max$DATE = as.Date(temp_max$DATE,format='%d-%m-%Y')
temp_mean$DATE = as.Date(temp_mean$DATE,format='%d-%m-%Y')

day_temp_min = temp_min %>% pivot_longer(!DATE, names_to = "weather_station", values_to = "temp_min")
day_temp_max = temp_max %>% pivot_longer(!DATE, names_to = "weather_station", values_to = "temp_max")
day_temp_mean = temp_mean %>% pivot_longer(!DATE, names_to = "weather_station", values_to = "temp_mean")

day_temp = full_join(day_temp_mean,day_temp_min,by = c('DATE','weather_station'))
day_temp = full_join(day_temp,day_temp_max,by = c('DATE','weather_station')) %>% 
  mutate(year = as.numeric(format(DATE, "%Y")),month = as.numeric(format(DATE,"%m")),day = as.numeric(format(DATE,"%d")),weather_station = as.factor(weather_station))
day_temp = day_temp %>% filter(year <= 2016) #Garder uniquement les années complètes

#Altitude
day_temp = full_join(day_temp,wst_alt,by = c('weather_station' = 'F1'))
day_temp$LON = NULL
day_temp$LAT = NULL
day_temp$FID = NULL

#Degrés jour de croissance :
Tbase = 19.1 #Correspond au seuil inférieur de Villena
Tsup = 30.1  #Correspond au seuil supérieur de Villena

for(i in 1:nrow(day_temp)){
  if (day_temp$temp_mean[i] > Tbase & day_temp$temp_mean[i] < Tsup){
    day_temp$GDD[i] = day_temp$temp_mean[i] - Tbase}
  else{
    day_temp$GDD[i] = 0}
}

write.csv2(day_temp,'data/input/daily_temperature.csv',row.names = FALSE)

##Précipitations
rain$ID = NULL
rain$DATE = as.Date(rain$DATE,format='%d-%m-%Y')
day_rain = rain %>% pivot_longer(!DATE, names_to = "weather_station", values_to = "rainfall") %>% 
  mutate(year = as.numeric(format(DATE, "%Y")),month = as.numeric(format(DATE,"%m")),day = as.numeric(format(DATE,"%d")),weather_station = as.factor(weather_station))
day_rain = day_rain %>% filter(year >= 1983 & year <= 2016) #Garder uniquement les années communes avec day_temp

write.csv2(day_rain,'data/input/daily_rain.csv',row.names = FALSE)










