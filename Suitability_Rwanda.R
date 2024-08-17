############################################################
#   Convenance environmentale pour la transmission de la   #
#         malaria par Anopheles gambiae au Rwanda          #
#              Modèle : Villena et al. (2022)              #                       
############################################################

# Modèle (DOI) : https://doi.org/10.1002/ecy.3685
# Auteur : Louis Beautrix
# Date : 27 Décembre 2023

### Directory :
setwd("C:/Users/louis/OneDrive - UCL/Mémoire/Workspace")
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

######### Packages ######### 
library(tidyverse)
library(sf)
library(matrixStats)
library(Hmisc)
library(xlsx)
library(scales)
library(DescTools)
library(gridExtra)

######### Data #########

##Températures
temp_mean = read_csv2('data/daily_avg_temp.csv')
colnames(temp_mean) = sapply(strsplit(colnames(temp_mean), "_"), `[`, 1)
day_temp = read_csv2('data/daily_temperature.csv')
day_temp$DATE = as.Date(day_temp$DATE,'%d-%m-%Y')
day_temp = day_temp %>% filter(year != 2017)
day_temp = day_temp %>% separate(weather_station,into=c('weather_station','region'),sep='_',remove=FALSE)
day_temp$region[day_temp$weather_station == 'Gitega'] = 'Nyaruguru'
day_temp$region[day_temp$weather_station == 'Nyanza'] = 'Nyanza'
day_temp$region[day_temp$weather_station == 'Nyagatare'] = 'Nyagatare'
day_temp$region[day_temp$weather_station == 'Kirehe'] = 'Kirehe'

##Stations météos
wst_co = read_csv2('data/weather_station_coordinate.csv')
wst_co = as.data.frame(t(column_to_rownames(wst_co,'...1')))
#write.csv2(wst_co,'data/weather_station_coordinate_transposed.csv')

#Map
rw_adm2 = st_read('data/rwanda_map_data/rwa_adm2_2006_NISR_WGS1984_20181002.shp')
ggplot(data = rw_adm2) + geom_sf() +
  geom_point(data = wst_co,aes(x=LON,y=LAT),size = 2, shape = 23, fill ='red') +
  xlab('Longitude') + ylab('Latitude') + theme_bw()

##Incidence Rwanda
incidence = read.xlsx('data/malaria_data/Incidence_Malaria_Rwanda_OMS.xlsx',sheetIndex = 1)
ggplot(data = incidence) +
  geom_line(aes(x = Period,y=Incidence)) +
  theme_bw() + xlab('Année') + ylab('Incidence estimée de paludisme \n(par 1000 personnes à risque)')

##Précipitations
day_rain = read_csv2('data/daily_rain.csv')

######### Diagramme ombrothermique #########
m_rain = day_rain %>% group_by(weather_station,year,month) %>%  summarise(rainfall = sum(rainfall))
m_rain_nat = m_rain %>% group_by(month) %>%  summarise(rainfall = mean(rainfall))
m_rain_nat = as.numeric(m_rain_nat$rainfall)
m_temp = day_temp %>% group_by(month) %>%  summarise(t_max = mean(temp_max),
                                                     t_mean = mean(temp_mean),
                                                     t_min = mean(temp_min))
m_temp_mean = as.numeric(m_temp$t_mean)
m_temp_max = as.numeric(m_temp$t_max)
m_temp_min = as.numeric(m_temp$t_min)

## Plot
month_vector <- 1:12
month_vector <- as.Date(paste0("2000-", month_vector, "-01"))
month_vector <- format(month_vector, "%b")

plot.new()
par(mar=c(4,4,3,4))
position = barplot(m_rain_nat,col="steelblue",names.arg=month_vector,axes=F,ylab="",xlab="",
                   ylim=c(0,175), las=2,space=0,cex.main=0.8)
axis(2,col="black",at=seq(0,175, by=25))
mtext("Précipitations (mm)", side=2, line=2.5, col="black")
par(new=TRUE,mar=c(4,4,3,4))
maximal = max(position)+(position[2]-position[1])
plot(position,m_temp_mean,col="orange",type="o",lwd=2,pch=16,axes=F,ylab="",xlab="",
     ylim=c(10,27.5), xlim=c(0,length(m_temp_mean)),yaxs ="i")
par(new=TRUE,mar=c(4,4,3,4))
maximal = max(position)+(position[2]-position[1])
plot(position,m_temp_max,col="red",type="o",lwd=2,pch=16,axes=F,ylab="",xlab="",
     ylim=c(10,27.5), xlim=c(0,length(m_temp_mean)),yaxs ="i")
par(new=TRUE,mar=c(4,4,3,4))
maximal = max(position)+(position[2]-position[1])
plot(position,m_temp_min,col="yellow",type="o",lwd=2,pch=16,axes=F,ylab="",xlab="",
     ylim=c(10,27.5), xlim=c(0,length(m_temp_mean)),yaxs ="i")
axis(4,col.axis="black",col="black",at=seq(10,27.5, by=2.5))
mtext("Température (°C)", side=4, line=2.5, col="black")
box()

######### Analyse descriptive de la température #########
ggplot(day_temp) +
  geom_histogram(aes(temp_mean)) +
  labs(x = 'Température moyenne (1983 - 2016)',y = 'Nombre d\'observations') +
  theme_bw()

### Températures Moyennes ###
##National :
nat_y_temp = day_temp %>% filter(year != 2017) %>% group_by(year) %>% summarise(year_mean = mean(temp_mean)) #Comparaison interannuelle
ggplot(nat_y_temp) + aes(x=year,y=year_mean) + geom_line() + xlab('Année') + ylab('Température annuelle moyenne (°C)') + theme_bw() +
  geom_smooth(method='lm', formula= y~x, color ='red')
reg = lm(year_mean~year, data=nat_y_temp)
summary(reg)

##Moyenne annuelle par station :
wst_y_temp = day_temp %>% group_by(weather_station,year) %>% summarise(year_mean = mean(temp_mean)) #Comparaison interannuelle
ggplot(wst_y_temp) + aes(x=year,y=year_mean) + geom_hline(yintercept = 19.1,color = 'red') + facet_wrap(~weather_station) + 
  geom_line(col = "#112446") + theme_bw() + xlab('Années') + ylab('Température moyenne annuelle (°C)') 

wst_signif = 0
for (i in 0:27){
  reg = lm(year_mean[(1+34*i):(34+34*i)]~year[1:34],data=wst_y_temp)
  p_val = summary(reg)[[4]][8]
  if (p_val <= 0.05){
    wst_signif = wst_signif +1
    print(paste('Station : ',wst_y_temp$weather_station[(1+i*34)]))
    print(paste('Coefficient : ',reg[[1]][[2]]))
    print(p_val)}
}
print(wst_signif)
rm(nat_y_temp,wst_signif,p_val)

### Tableau récapitulatif des stations (moy. à long terme) ###

wst_mean = day_temp %>% group_by(weather_station,Altitude) %>% summarise(t_max = mean(temp_max),
                                                                t_min = mean(temp_min),
                                                                t_mean = mean(temp_mean))
wst_mean_rain = day_rain %>% group_by(weather_station,year) %>% summarise(rainfall = sum(rainfall))
wst_mean_rain = wst_mean_rain %>% group_by(weather_station) %>% summarise(rainfall = mean(rainfall))
wst_mean$rainfall = wst_mean_rain$rainfall
rm(wst_mean_rain)
#write.csv2(wst_mean,'data/WeatherStation_LongTermMean.csv',row.names = FALSE)

### Degrés jour ###
ggplot(day_temp) +
  geom_histogram(aes(GDD)) +
  labs(x = 'Degré jour de croissance (1983 - 2016)',y = 'Nombre d\'observations') +
  theme_bw()

##Annuel :
y_gdd = day_temp %>% filter(year != 2017) %>% group_by(DATE) %>% summarise(gdd_nat = mean(GDD))
mean(y_gdd$gdd_nat) #1.19 DJ en moyenne
y_gdd$year = as.numeric(format(as.Date(y_gdd$DATE),"%Y"))
y_gdd = y_gdd %>% group_by(year) %>% summarise(gdd_sum = sum(gdd_nat))
reg_gdd = lm(gdd_sum~year, data=y_gdd)
summary(reg_gdd)

ggplot(y_gdd) +
  aes(x = year, y = gdd_sum) +
  geom_point(colour = "#112446") +
  theme_bw() +
  geom_smooth(method='lm', formula= y~x, color ='red') +
  xlab('Année') + ylab('Degré jour de croissance (DJ)')

##Par station :
wst_y_gdd = day_temp %>% group_by(weather_station,year) %>% summarise(gdd_sum = sum(GDD)) #Comparaison interannuelle
ggplot(wst_y_gdd) +
  aes(x=year,y=gdd_sum) +
  facet_wrap(~weather_station) + 
  geom_point(col = "#112446") +
  theme_bw() +
  geom_smooth(method='lm', formula= y~x, color ='red') +
  xlab('Années') + ylab('Degré jour de croissance (DJ)')

wst_signif = 0
for (i in 0:27){
  reg = lm(gdd_mean[(1+34*i):(34+34*i)]~year[1:34],data=wst_y_gdd)
  p_val = summary(reg)[[4]][8]
  if (is.na(p_val)){p_val = 1}
  if (p_val <= 0.05){
    wst_signif = wst_signif +1
    print(paste('Station : ',wst_y_gdd$weather_station[(1+i*34)]))
    print(paste('Coefficient : ',reg[[1]][[2]]))
    print(p_val)}
}
print(wst_signif)
rm(y_gdd,reg_gdd,wst_y_gdd,wst_signif)

### Normales hebdomadaires ###

lgterm_mean = day_temp %>% group_by(month,day) %>% summarise(temp_ltmean = mean(temp_mean),gdd_ltmean = mean(GDD))
lgterm_mean$date = as.Date(paste(lgterm_mean$day,lgterm_mean$month,sep='-'),'%d-%m')
lgterm_mean$week = 0
lgterm_mean$FDOW = 0
for (i in 1:53) {
  start_row = ((i - 1) * 7) + 1
  end_row = min(i * 7, nrow(lgterm_mean))
  lgterm_mean$week[start_row:end_row] = i
  lgterm_mean$FDOW[start_row:end_row] = as.character(lgterm_mean$date[start_row])
}
lgterm_mean = lgterm_mean %>% filter(week != 53)
lgterm_mean = lgterm_mean %>% group_by(week,FDOW) %>% summarise(temp_ltmean = mean(temp_ltmean),gdd_ltmean = sum(gdd_ltmean))
lgterm_mean$FDOW = as.Date(lgterm_mean$FDOW,'%Y-%m-%d')

x_limits <- range(lgterm_mean$FDOW)
y_formatter <- number_format(accuracy = 0.1)

# Graphique des températures
plot_temp <- ggplot(lgterm_mean, aes(x = FDOW, y = temp_ltmean)) +
  geom_line(color = "#112446") +
  geom_hline(yintercept = 19.1, color = 'red') +
  scale_x_date(labels = date_format('%d %b'), limits = x_limits) +
  scale_y_continuous(labels = y_formatter) +
  theme_bw() +
  xlab(NULL) +
  ylab('Normales hebdomadaires de\n température (°C)')

# Graphique GDD
plot_gdd <- ggplot(lgterm_mean, aes(x = FDOW, y = gdd_ltmean)) +
  geom_line(color = "#112446") +
  scale_x_date(labels = date_format('%d %b'), limits = x_limits) +
  scale_y_continuous(labels = y_formatter) +
  theme_bw() +
  xlab('Date') +
  ylab('Normales hebdomadaires de\n degré jour de croissance')

# Combiner les graphiques
gdd_temp_plot <- grid.arrange(plot_temp, plot_gdd, ncol = 1)

######### Régression altitude - température #########

### Fonction de validation croisée ###

mse = function(y_predicted,y_true){
  MeanSquaredError = (y_true - y_predicted)**2
  return(MeanSquaredError)
}

lm_loocv = function(data,x,y){ #lm = method (ex : y~x ; y~poly(x,2))
  for (i in 1:nrow(data)){
    reg = lm(y~x,data[-i,])
    y_t = as.numeric(y[i])
    y_p = as.numeric(reg[[1]][1]+reg[[1]][2]*x[i])  #intercept - coefficient * temperature
    data$mse[i] = mse(y_p,y_t)
  }
  RMSE = sqrt(mean(data$mse))
  output1 = paste('MSE = ',mean(data$mse))
  output2 = paste('RMSE = ',RMSE)
  print(output1)
  print(output2)
  return(RMSE)
}

### Régressions linéaires ###

##Régression globale
wst_alt_temp = day_temp %>% group_by(year,weather_station,Altitude) %>% summarise(temp_mean = mean(temp_mean))
wst_alt_temp2 = wst_alt_temp %>% group_by(weather_station,Altitude) %>% summarise(temp_mean = mean(temp_mean))

reg_alt_temp = lm(temp_mean~Altitude, data=wst_alt_temp2)
ggplot(wst_alt_temp2) +
  aes(x = Altitude, y = temp_mean) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_smooth(method='lm', formula= y~x, color ='red') +
  xlab('Altitude (m)') + ylab('Température moyenne (°C)') + #labs(title = 'Période 1983 - 2016') +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
lm_loocv(wst_alt_temp2,wst_alt_temp2$Altitude,wst_alt_temp2$temp_mean) #En moyenne RMSE de 0.67°C
summary(reg_alt_temp)
rm(wst_alt_temp2,reg_alt_temp)

##Régression pour 4 périodes entre 8 et 9 ans => 1983 - 1991 ; 1992 - 2000 ; 2001 - 2008 ; 2009 - 2016 :
#Regression 1
wst_alt_1 = wst_alt_temp %>% filter(year >= 1983 & year <= 1991) %>% group_by(weather_station,Altitude) %>% summarise(temp_mean = mean(temp_mean))
reg_alt_1 = lm(temp_mean~Altitude,data=wst_alt_1)  
summary(reg_alt_1) #Température = 30.168592 - 0.006273 * Altitude
                   
plot_reg_1 = ggplot(na.omit(wst_alt_1)) +
  aes(x = Altitude, y = temp_mean) + xlim(1350,2600) + ylim(12,24) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method='lm', formula= y~x, color ='red') +
  xlab('Altitude') + ylab('Température moyenne (°C)') + labs(title = 'Période 1983 - 1991')

plot(reg_alt_1$fitted.value,reg_alt_1$residuals)
lines(range(reg_alt_1$fitted.value),c(0,0),col="red")
lm_loocv(wst_alt_1,wst_alt_1$Altitude,wst_alt_1$temp_mean) #MSE = 0.47 => en moyenne, erreur de 0.69 °C

#Regression 2
wst_alt_2 = wst_alt_temp %>% filter(year >= 1992 & year <= 2000) %>% group_by(weather_station,Altitude) %>% summarise(temp_mean = mean(temp_mean))
reg_alt_2 = lm(temp_mean~Altitude,data=wst_alt_2) 
summary(reg_alt_2) #Température = 30.694902 - 0.006495 * Altitude

plot_reg_2 = ggplot(na.omit(wst_alt_2)) +
  aes(x = Altitude, y = temp_mean) + xlim(1350,2600) + ylim(12,24) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method='lm', formula= y~x, color ='red') +
  xlab('Altitude') + ylab(NULL) + labs(title = 'Période 1992 - 2000')

plot(reg_alt_2$fitted.value,reg_alt_2$residuals)
lines(range(reg_alt_2$fitted.value),c(0,0),col="red")
lm_loocv(wst_alt_2,wst_alt_2$Altitude,wst_alt_2$temp_mean) #MSE = 0.50 => en moyenne, erreur de 0.70°C

#Regression 3
wst_alt_3 = wst_alt_temp %>% filter(year >= 2001 & year <= 2008) %>% group_by(weather_station,Altitude) %>% summarise(temp_mean = mean(temp_mean))
reg_alt_3 = lm(temp_mean~Altitude,data=wst_alt_3) 
summary(reg_alt_3) #Température = 30.263354 - 0.006214 * Altitude

plot_reg_3 = ggplot(na.omit(wst_alt_3)) +
  aes(x = Altitude, y = temp_mean) + xlim(1350,2600) + ylim(12,24) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method='lm', formula= y~x, color ='red') +
  xlab('Altitude') + ylab('Température moyenne (°C)') + labs(title = 'Période 2001 - 2008') 

plot(reg_alt_3$fitted.value,reg_alt_3$residuals)
lines(range(reg_alt_3$fitted.value),c(0,0),col="red")
lm_loocv(wst_alt_3,wst_alt_3$Altitude,wst_alt_3$temp_mean) #MSE = 0.47 => en moyenne, erreur de 0.68°C

#Regression 4
wst_alt_4 = wst_alt_temp %>% filter(year >= 2009 & year <= 2016) %>% group_by(weather_station,Altitude) %>% summarise(temp_mean = mean(temp_mean))
reg_alt_4 = lm(temp_mean~Altitude,data=wst_alt_4) 
summary(reg_alt_4)#Température = 29.99017 - 0.006029 * Altitude

plot_reg_4 = ggplot(na.omit(wst_alt_4)) +
  aes(x = Altitude, y = temp_mean) + xlim(1350,2600) + ylim(12,24) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method='lm', formula= y~x, color ='red') +
  xlab('Altitude') + ylab(NULL) + labs(title = 'Période 2009 - 2016')

plot(reg_alt_4$fitted.value,reg_alt_4$residuals)
lines(range(reg_alt_4$fitted.value),c(0,0),col="red")
lm_loocv(wst_alt_4,wst_alt_4$Altitude,wst_alt_4$temp_mean) #MSE = 0.39 => en moyenne, erreur de 0.62°C

#Plot combiné

regs_plot = grid.arrange(plot_reg_1, plot_reg_2, plot_reg_3, plot_reg_4, ncol = 2)



rm(reg_alt_1,reg_alt2,reg_alt_3,reg_alt_4,wst_alt_1,wst_alt_2,wst_alt_3,wst_alt_4)

###############################################################
### Calcul de la favorabilité environementale (suitability) ###
###############################################################

##Données pour An. Gambiae et P. Falciparum
load("data/Angamb_Pfalc_samps.Rsave")  #Posteriors sample for R0

### Fonctions :

quad<-function(T, T0, Tm, qd){
  qd*(T-T0)*(T-Tm)*(T<=Tm)*(T>=T0)
}

quad.trunc<-function(T, T0, Tm, qd){
  1*((qd*(T-T0)*(T-Tm))>1) + (qd*(T-T0)*(T-Tm)*(T<=Tm)*(T>=T0))*((qd*(T-T0)*(T-Tm))<1)
}

quad.alt<-function(T, inter, slope, qd){
  inter+slope*T+qd*T^2
}

briere<-function(T, c, Tm, T0){
  c*T*(T-T0)*sqrt(abs(Tm-T))*(T<=Tm)*(T>=T0)
}

ST_function = function(a, PDR, MDR, efd, pea, bc, mu){
  ((a^2*bc*(efd*pea*MDR/(ec+mu^2))*exp(-mu/(PDR+ec)))/(mu+ec))^0.5
} #Fonction de suitability

############ Courbe de suitability en fonction de la température ############ 

##Températures :
temp = seq(15,35,0.05)	##temperature sequence

t<-length(temp)  ## length of temperature
n = dim(a.sampsgam)[1]    	## length of samples
thinned<-seq(1, n, by=20)  ## thinned samples
lthin<-length(thinned)  ## number of thinned samples
ec<-0.000001  ## small constant used to keep denominators from being numerically zero

##Posterior trajectories :

ST <-matrix(NA,t,lthin)
a<-PDR<-MDR<-efd<-pea<-bc<-mu<-matrix(NA,t,lthin)
for (j in 1:lthin){
  if(j%%50==0) cat("iteration", j, "\n")
  ## calculate parameter trajectories
  i<-thinned[j]
  a[,j] = briere(temp,a.sampsgam[i,3],a.sampsgam[i,2],a.sampsgam[i,1])
  PDR[,j] = briere(temp,PDR.sampsgampf[i,3],PDR.sampsgampf[i,2],PDR.sampsgampf[i,1])
  MDR[,j] = briere(temp,MDR.sampsgam[i,3],MDR.sampsgam[i,2],MDR.sampsgam[i,1])
  efd[,j] = quad(temp,efd.sampsgam[i,1],efd.sampsgam[i,2],efd.sampsgam[i,3])
  pea[,j] = quad.trunc(temp,e2a.sampsgam[i,1],e2a.sampsgam[i,2],e2a.sampsgam[i,3]) 
  bc[,j] = quad.trunc(temp,bc.sampsgampf[i,1],bc.sampsgampf[i,2],bc.sampsgampf[i,3])
  mu[,j] = quad.alt(temp,mu.sampsgam[i,1], -mu.sampsgam[i,2],mu.sampsgam[i,3])
  
  ## Calculate S(T)
  ST[,j]<-ST_function(a[,j], PDR[,j], MDR[,j], efd[,j], pea[,j], bc[,j], mu[,j])
  
}
rm(a,PDR,MDR,efd,pea,bc,mu)

##Poster mean trajectory :
ST.M<-rowMeans(ST)

#suitability quantile :
ST.q95 = apply(ST, 1, FUN=quantile, probs=0.95, na.rm=T)
ST.q5 = apply(ST, 1, FUN=quantile, probs=0.05,na.rm = T)

#Plot suitability :
st_max = max(na.omit(ST.M)) #Suitability maximale possible => Permet de comparer la suitability des différentes stations.
ST.M.rel = ST.M/st_max
ST.q95.rel = ST.q95/st_max
ST.q5.rel = ST.q5/st_max

ST.gen = as.data.frame(temp)
ST.gen$M = ST.M.rel
ST.gen$q95 = ST.q95.rel
ST.gen$q5 = ST.q5.rel
ST.gen[is.na(ST.gen)] = 0
ST.gen = round(ST.gen,digits = 3)

ST.gen_plot = ggplot(data = ST.gen, aes(x = temp)) +
  geom_line(aes(y = M), color="steelblue",linewidth = 0.8) +
  geom_line(aes(y = q95), color = "darkred",linetype = 'dashed',linewidth = 0.8) + 
  geom_line(aes(y = q5), color = "darkred",linetype = 'dashed',linewidth = 0.8) +
  labs(x = 'Température (°C)', y = 'Favorabilité climatique (F(T))') +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ST.gen_plot

#write.csv2(ST.gen,'GIS/output/Suitability_Classify.csv')
Suitability_temp = ST.gen

############ Courbe de suitability pour le Rwanda ############

##Moyenne quotidienne pour toute la période##

##Températures :
nat_d_temp = day_temp %>% filter(year != 2017) %>% group_by(month,day) %>% summarise(temp_mean = mean(temp_mean))
temp = as.vector(nat_d_temp[3])[[1]]

t<-length(temp)  ## length of temperature
n = dim(a.sampsgam)[1]    	## length of samples
thinned<-seq(1, n, by=20)  ## thinned samples
lthin<-length(thinned)  ## number of thinned samples
ec<-0.000001  ## small constant used to keep denominators from being numerically zero

##Posterior trajectories :

ST <-matrix(NA,t,lthin)
a<-PDR<-MDR<-efd<-pea<-bc<-mu<-matrix(NA,t,lthin)
for (j in 1:lthin){
  if(j%%50==0) cat("iteration", j, "\n")
  ## calculate parameter trajectories
  i<-thinned[j]
  a[,j] = briere(temp,a.sampsgam[i,3],a.sampsgam[i,2],a.sampsgam[i,1])
  PDR[,j] = briere(temp,PDR.sampsgampf[i,3],PDR.sampsgampf[i,2],PDR.sampsgampf[i,1])
  MDR[,j] = briere(temp,MDR.sampsgam[i,3],MDR.sampsgam[i,2],MDR.sampsgam[i,1])
  efd[,j] = quad(temp,efd.sampsgam[i,1],efd.sampsgam[i,2],efd.sampsgam[i,3])
  pea[,j] = quad.trunc(temp,e2a.sampsgam[i,1],e2a.sampsgam[i,2],e2a.sampsgam[i,3]) 
  bc[,j] = quad.trunc(temp,bc.sampsgampf[i,1],bc.sampsgampf[i,2],bc.sampsgampf[i,3])
  mu[,j] = quad.alt(temp,mu.sampsgam[i,1], -mu.sampsgam[i,2],mu.sampsgam[i,3])
  
  ## Calculate S(T)
  ST[,j]<-ST_function(a[,j], PDR[,j], MDR[,j], efd[,j], pea[,j], bc[,j], mu[,j])
}
rm(a,PDR,MDR,efd,pea,bc,mu)

##Poster mean trajectory :
ST.M<-rowMeans(ST)

##Suitability quantile :
ST.q95 = apply(ST, 1, FUN=quantile, probs=0.95, na.rm=T)
ST.q5 = apply(ST, 1, FUN=quantile, probs=0.05,na.rm = T)

##Plot :
ST.M.rel = ST.M/st_max #Suitability relative => 1 étant la température la plus favorable au Rwanda.
ST.q95.rel = ST.q95/st_max
ST.q5.rel = ST.q5/st_max
ST.gen = as.data.frame(temp)
ST.gen$M = ST.M.rel
ST.gen$q95 = ST.q95.rel
ST.gen$q5 = ST.q5.rel

ggplot(data = ST.gen, aes(temp)) +
  geom_line(aes(y = M*(200/3)), color="darkblue",linewidth = 0.8) +
  geom_line(aes(y = q95*(200/3)), color = "darkred",linewidth = 0.8,linetype = 'dashed') + 
  geom_line(aes(y = q5*(200/3)), color = "darkred",linewidth = 0.8,linetype = 'dashed') +
  scale_y_continuous(name = 'Nombre d\'observations',sec.axis = sec_axis(trans=~./(200/3),name='Favorabilité climatique')) +
  geom_histogram(aes(temp),alpha=0.5) + geom_vline(xintercept = 19.2,color='red',linetype = "dashed") +
  labs(x = 'Température moyenne à long terme (°C)') + theme_bw()

#Suitability frames
wst_lgt_mean = day_temp %>% group_by(weather_station,month,day) %>% summarise(lgt_mean = mean(temp_mean))
wst_lgt_mean = wst_lgt_mean %>% group_by(weather_station) %>% summarise(t_min = min(lgt_mean),t_max = max(lgt_mean),t_mean = mean(lgt_mean))

segments_df <- data.frame(t_mean = numeric(), M = numeric(), x_start = numeric(), x_end = numeric())
points_df = data.frame(t_mean = numeric(), M = numeric())
for (i in 1:nrow(wst_lgt_mean)) {
  t_mean_value = wst_lgt_mean$t_mean[i]
  if (t_mean_value < 15 | t_mean_value > 35){M_value = 0}
  else {M_value = Suitability_temp$M[Suitability_temp$temp == round(t_mean_value, 1)]}
  points_df = rbind(points_df, data.frame(t_mean = t_mean_value, M = M_value))
  x_start = wst_lgt_mean$t_min[i]
  x_end = wst_lgt_mean$t_max[i]
  segments_df <- rbind(segments_df, data.frame(t_mean = t_mean_value, M = M_value, x_start = x_start, x_end = x_end))
}

ST.gen_plot +
  geom_point(data = points_df, aes(x = t_mean, y = M),size = 3, shape = 4) +
  xlim(12.5,25) + geom_vline(xintercept = 19.2,color='red') #Position des stations sur F(T)

ST.gen_plot + 
  geom_rect(aes(xmin = 18.58, xmax = 20.43, ymin = 0, ymax = 0.6),color = "red", fill = NA, linetype = "solid", linewidth = 0.6) +
  geom_point(data = points_df[24,], aes(x = t_mean, y = M),size = 2, shape = 4) +
  #geom_segment(data = segments_df[24,], aes(x = x_start, xend = x_end, y = M, yend = M), linewidth = 0.8, color = 'gray0') +
  geom_point(data = points_df[3,], aes(x = t_mean, y = M),size = 2, shape = 3) +
  #geom_segment(data = segments_df[3,], aes(x = x_start, xend = x_end, y = M, yend = M), linewidth = 0.8, color = 'gray0') +
  geom_vline(xintercept = 19.2,color='red',linetype = "dashed")

############ Suitability pour chaque Station ############
##Températures :
for (w in 2:length(temp_mean)){
  print(paste('Suitability pour',colnames(temp_mean[w])))
  temp = as.vector(temp_mean[w])[[1]] 
  
  t<-length(temp)             ## length of temperature
  n = dim(a.sampsgam)[1]    	## length of samples
  thinned<-seq(1, n, by=20)   ## thinned samples
  lthin<-length(thinned)      ## number of thinned samples
  ec<-0.000001                ## small constant used to keep denominators from being numerically zero

  ##Posterior trajectories :

  ST <-matrix(NA,t,lthin)
  a<-PDR<-MDR<-efd<-pea<-bc<-mu<-matrix(NA,t,lthin)
  for (j in 1:lthin){
    if(j%%50==0) cat("iteration", j, "\n")
     ## calculate parameter trajectories
     i<-thinned[j]
     a[,j] = briere(temp,a.sampsgam[i,3],a.sampsgam[i,2],a.sampsgam[i,1])
     PDR[,j] = briere(temp,PDR.sampsgampf[i,3],PDR.sampsgampf[i,2],PDR.sampsgampf[i,1])
     MDR[,j] = briere(temp,MDR.sampsgam[i,3],MDR.sampsgam[i,2],MDR.sampsgam[i,1])
     efd[,j] = quad(temp,efd.sampsgam[i,1],efd.sampsgam[i,2],efd.sampsgam[i,3])
     pea[,j] = quad.trunc(temp,e2a.sampsgam[i,1],e2a.sampsgam[i,2],e2a.sampsgam[i,3]) 
     bc[,j] = quad.trunc(temp,bc.sampsgampf[i,1],bc.sampsgampf[i,2],bc.sampsgampf[i,3])
     mu[,j] = quad.alt(temp,mu.sampsgam[i,1], -mu.sampsgam[i,2],mu.sampsgam[i,3])
  
    ## Calculate S(T)
    ST[,j]<-ST_function(a[,j], PDR[,j], MDR[,j], efd[,j], pea[,j], bc[,j], mu[,j])
    }
  rm(a,PDR,MDR,efd,pea,bc,mu)

  ##Poster mean trajectory :
  ST.M<-rowMeans(ST)

  ##Suitability quantile :
  ST.q95 = apply(ST, 1, FUN=quantile, probs=0.95, na.rm=T)
  ST.q5 = apply(ST, 1, FUN=quantile, probs=0.05,na.rm = T)

  ##Plot :
  ST.M.rel = ST.M/st_max #Suitability relative => 1 étant la température la plus favorable.
  ST.q95.rel = ST.q95/st_max
  ST.q5.rel = ST.q5/st_max
  ST.gen = as.data.frame(temp)
  ST.gen$M = ST.M.rel
  ST.gen$q95 = ST.q95.rel
  ST.gen$q5 = ST.q5.rel

  file_path = paste('Station Output/St_',colnames(temp_mean[w]),'.csv',sep='')
  write.csv2(ST.gen,file = file_path)
}



############ Analyse par station de la suitability ############

wst_ST_list = list()          #Liste des outputs du modèle (point précédent)
wst_per1_list = list()        #Liste des outputs moyens pour la période 1
wst_per2_list = list()        #Liste des outputs moyens pour la période 2
wst_per3_list = list()        #Liste des outputs moyens pour la période 3
wst_per4_list = list()        #Liste des outputs moyens pour la période 4
ST_plot = list()              #Liste des plots des écarts de suitability
Season_plot = list()          #Liste des plots de la suitability moyenne par période

#Dataframe des outputs :
Season_output = as.data.frame(matrix(nrow = (length(temp_mean)-1)))  
Season_output$V1 = NULL
Season_output$Station = 0

for (w in 1:(length(temp_mean)-1)){
  ####Data : 
  file_path = paste('Station Output/St_',colnames(temp_mean[w+1]),'.csv',sep='')
  wst_ST = read.csv2(file_path,row.names = 'X')
  wst_ST$M[is.na(wst_ST$M)] = 0
  Season_output$Station[w] = colnames(temp_mean[w+1])
  
  print('--------------------')
  print(' Seasonality for  : ')
  print(colnames(temp_mean[w+1]))
  print('--------------------')
  
  ####Ecart de suitability : 
  wst_ST$date = as.Date(temp_mean$DATE,'%d-%m-%Y')
  wst_ST$year = format(wst_ST$date,'%Y')
  wst_ST$month = format(wst_ST$date,'%m')
  wst_ST$day = format(wst_ST$date,'%d')
  wst_ST_list[[w]] = wst_ST
  
  for (i in 1:nrow(wst_ST)){ 
    if(wst_ST$date[i] < as.Date('1993-01-01')){wst_ST$period[i] = 1}
    else if(wst_ST$date[i] >= as.Date('1993-01-01') & wst_ST$date[i] < as.Date('2001-01-01')){wst_ST$period[i] = 2}
    else if(wst_ST$date[i] >= as.Date('2001-01-01') & wst_ST$date[i] < as.Date('2010-01-01')){wst_ST$period[i] = 3}
    else {wst_ST$period[i] = 4}}
  
  ST_plot[[w]] = ggplot(data = wst_ST, aes(temp)) +
    geom_line(aes(y = M*1500), color="darkblue",linewidth = 1) +
    geom_line(aes(y = q95*1500), color = "darkred",linewidth = 1) + 
    geom_line(aes(y = q5*1500), color = "darkred",linewidth = 1) +
    scale_y_continuous(name = 'Nombre d\'observations',sec.axis = sec_axis(trans=~./1500,name='Favorabilité climatique')) +
    geom_histogram(aes(temp),alpha=0.5) +
    labs(x = 'Température (°C)', title = paste(colnames(temp_mean[w+1]),sep='')) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))

  Season_output$Global_mean[w] = mean(wst_ST$M)
  Season_output$Global_mean.q5[w] = mean(wst_ST$q5)
  Season_output$Global_mean.q95[w] = mean(wst_ST$q95)
  
  ####Saisonnalité pour les 4 périodes : 
  ###Période 1 : 1983 - 1991 :
  wst_per = wst_ST %>% filter(year >= 1983 & year <= 1991) %>% group_by(month,day) %>% summarise(ST_mean = mean(M),ST_mean.q5 = mean(q5),ST_mean.q95 = mean(q95))
  wst_per$date = as.Date(paste(wst_per$day,wst_per$month,sep='-'),'%d-%m')
  Season_output$Per_mean_1[w] = mean(wst_per$ST_mean)
  Season_output$Per_mean_1.q5[w] = mean(wst_per$ST_mean.q5)
  Season_output$Per_mean_1.q95[w] = mean(wst_per$ST_mean.q95)
  
  wst_per$Season_ST = wst_per$ST_mean - mean(wst_ST$M)
  wst_per$Season_ST.q5 = wst_per$ST_mean.q5 - mean(wst_ST$q5)
  wst_per$Season_ST.q95 = wst_per$ST_mean.q95 - mean(wst_ST$q95)
  wst_per[wst_per$Season_ST < 0,]$Season_ST = 0
  wst_per[wst_per$Season_ST.q5 < 0,]$Season_ST.q5 = 0
  wst_per[wst_per$Season_ST.q95 < 0,]$Season_ST.q95 = 0
  
  
  wst_per$season = 'Saison'
  wst_per$season.q5 = 'Saison'
  wst_per$season.q95 = 'Saison'
  wst_per[wst_per$Season_ST == 0,]$season = 'Hors saison'
  wst_per[wst_per$Season_ST.q5 == 0,]$season.q5 = 'Hors saison'
  wst_per[wst_per$Season_ST.q95 == 0,]$season.q95 = 'Hors saison'
  wst_per$season = as.factor(wst_per$season)
  wst_per$season.q5 = as.factor(wst_per$season.q5)
  wst_per$season.q95 = as.factor(wst_per$season.q95)
  wst_per1_list[[w]] = wst_per
  
  D_InSeason = length(wst_per$season[wst_per$season == 'Saison'])
  D_InSeason.q5 = length(wst_per$season.q5[wst_per$season.q5 == 'Saison'])
  D_InSeason.q95 = length(wst_per$season.q95[wst_per$season.q95 == 'Saison'])
  Season_output$D_InSeason_1[w] = D_InSeason
  Season_output$D_InSeason_1.q5[w] = D_InSeason.q5
  Season_output$D_InSeason_1.q95[w] = D_InSeason.q95
  
  plot_list = list()
  plot_list[[1]] = ggplot(wst_per) +
    scale_x_date(labels = date_format('%d %b')) +
    geom_line(aes(x = date, y = ST_mean,color = "Favorabilité climatique journalière")) +
    geom_hline(aes(yintercept = Season_output$Global_mean[w],color = 'Moyenne à long terme \nde favorabilité climatique')) +
    scale_color_manual(values = c("Favorabilité climatique journalière" = "#112446", 'Moyenne à long terme \nde favorabilité climatique' = "red")) +
    theme_minimal() + xlab('Date (1983 - 1991)') + ylab(paste('Favorabilité climatique (',colnames(temp_mean[w+1]),')',sep='')) +
    theme(legend.title = element_blank()) + ylim(-0.05,0.7)
  
  #Aire au dessus de la courbe rouge et sous la courbe bleue :
  Season_output$AUC_1[w] = AUC(wst_per$date,wst_per$Season_ST)
  Season_output$AUC_1.q5[w] = AUC(wst_per$date,wst_per$Season_ST.q5)
  Season_output$AUC_1.q95[w] = AUC(wst_per$date,wst_per$Season_ST.q95)
  
  ###Période 2 : 1992 - 2000 :
  wst_per = wst_ST %>% filter(year >= 1992 & year <= 2000) %>% group_by(month,day) %>% summarise(ST_mean = mean(M),ST_mean.q5 = mean(q5),ST_mean.q95 = mean(q95))
  wst_per$date = as.Date(paste(wst_per$day,wst_per$month,sep='-'),'%d-%m')
  Season_output$Per_mean_2[w] = mean(wst_per$ST_mean)
  Season_output$Per_mean_2.q5[w] = mean(wst_per$ST_mean.q5)
  Season_output$Per_mean_2.q95[w] = mean(wst_per$ST_mean.q95)
  
  wst_per$Season_ST = wst_per$ST_mean - mean(wst_ST$M)
  wst_per$Season_ST.q5 = wst_per$ST_mean.q5 - mean(wst_ST$q5)
  wst_per$Season_ST.q95 = wst_per$ST_mean.q95 - mean(wst_ST$q95)
  wst_per[wst_per$Season_ST < 0,]$Season_ST = 0
  wst_per[wst_per$Season_ST.q5 < 0,]$Season_ST.q5 = 0
  wst_per[wst_per$Season_ST.q95 < 0,]$Season_ST.q95 = 0
  
  wst_per$season = 'Saison'
  wst_per$season.q5 = 'Saison'
  wst_per$season.q95 = 'Saison'
  wst_per[wst_per$Season_ST == 0,]$season = 'Hors saison'
  wst_per[wst_per$Season_ST.q5 == 0,]$season.q5 = 'Hors saison'
  wst_per[wst_per$Season_ST.q95 == 0,]$season.q95 = 'Hors saison'
  wst_per$season = as.factor(wst_per$season)
  wst_per$season.q5 = as.factor(wst_per$season.q5)
  wst_per$season.q95 = as.factor(wst_per$season.q95)
  wst_per2_list[[w]] = wst_per
  
  D_InSeason = length(wst_per$season[wst_per$season == 'Saison'])
  D_InSeason.q5 = length(wst_per$season.q5[wst_per$season.q5 == 'Saison'])
  D_InSeason.q95 = length(wst_per$season.q95[wst_per$season.q95 == 'Saison'])
  Season_output$D_InSeason_2[w] = D_InSeason
  Season_output$D_InSeason_2.q5[w] = D_InSeason.q5
  Season_output$D_InSeason_2.q95[w] = D_InSeason.q95
  
  plot_list[[2]] = ggplot(wst_per) +
    scale_x_date(labels = date_format('%d %b')) +
    geom_line(aes(x = date, y = ST_mean,color = "Favorabilité climatique journalière")) +
    geom_hline(aes(yintercept = Season_output$Global_mean[w],color = 'Moyenne à long terme \nde favorabilité climatique')) +
    scale_color_manual(values = c("Favorabilité climatique journalière" = "#112446", 'Moyenne à long terme \nde favorabilité climatique' = "red")) +
    theme_minimal() + xlab('Date (1992 - 2000)') + ylab(paste('Favorabilité climatique (',colnames(temp_mean[w+1]),')',sep='')) +
    theme(legend.title = element_blank()) + ylim(-0.05,0.7)
  
  #Aire au dessus de la courbe rouge et sous la courbe bleue :
  Season_output$AUC_2[w] = AUC(wst_per$date,wst_per$Season_ST)
  Season_output$AUC_2.q5[w] = AUC(wst_per$date,wst_per$Season_ST.q5)
  Season_output$AUC_2.q95[w] = AUC(wst_per$date,wst_per$Season_ST.q95)
  
  ###Période 3 : 2001 - 2008 :
  wst_per = wst_ST %>% filter(year >= 2001 & year <= 2008) %>% group_by(month,day) %>% summarise(ST_mean = mean(M),ST_mean.q5 = mean(q5),ST_mean.q95 = mean(q95))
  wst_per$date = as.Date(paste(wst_per$day,wst_per$month,sep='-'),'%d-%m')
  Season_output$Per_mean_3[w] = mean(wst_per$ST_mean)
  Season_output$Per_mean_3.q5[w] = mean(wst_per$ST_mean.q5)
  Season_output$Per_mean_3.q95[w] = mean(wst_per$ST_mean.q95)
  
  wst_per$Season_ST = wst_per$ST_mean - mean(wst_ST$M)
  wst_per$Season_ST.q5 = wst_per$ST_mean.q5 - mean(wst_ST$q5)
  wst_per$Season_ST.q95 = wst_per$ST_mean.q95 - mean(wst_ST$q95)
  wst_per[wst_per$Season_ST < 0,]$Season_ST = 0
  wst_per[wst_per$Season_ST.q5 < 0,]$Season_ST.q5 = 0
  wst_per[wst_per$Season_ST.q95 < 0,]$Season_ST.q95 = 0
  
  wst_per$season = 'Saison'
  wst_per$season.q5 = 'Saison'
  wst_per$season.q95 = 'Saison'
  wst_per[wst_per$Season_ST == 0,]$season = 'Hors saison'
  wst_per[wst_per$Season_ST.q5 == 0,]$season.q5 = 'Hors saison'
  wst_per[wst_per$Season_ST.q95 == 0,]$season.q95 = 'Hors saison'
  wst_per$season = as.factor(wst_per$season)
  wst_per$season.q5 = as.factor(wst_per$season.q5)
  wst_per$season.q95 = as.factor(wst_per$season.q95)
  wst_per3_list[[w]] = wst_per
  
  D_InSeason = length(wst_per$season[wst_per$season == 'Saison'])
  D_InSeason.q5 = length(wst_per$season.q5[wst_per$season.q5 == 'Saison'])
  D_InSeason.q95 = length(wst_per$season.q95[wst_per$season.q95 == 'Saison'])
  Season_output$D_InSeason_3[w] = D_InSeason
  Season_output$D_InSeason_3.q5[w] = D_InSeason.q5
  Season_output$D_InSeason_3.q95[w] = D_InSeason.q95
  
  plot_list[[3]] = ggplot(wst_per) +
    scale_x_date(labels = date_format('%d %b')) +
    geom_line(aes(x = date, y = ST_mean,color = "Favorabilité climatique journalière")) +
    geom_hline(aes(yintercept = Season_output$Global_mean[w],color = 'Moyenne à long terme \nde favorabilité climatique')) +
    scale_color_manual(values = c("Favorabilité climatique journalière" = "#112446", 'Moyenne à long terme \nde favorabilité climatique' = "red")) +
    theme_minimal() + xlab('Date (2001 - 2008)') + ylab(paste('Favorabilité climatique (',colnames(temp_mean[w+1]),')',sep='')) +
    theme(legend.title = element_blank()) + ylim(-0.05,0.7)
  
  #Aire au dessus de la courbe rouge et sous la courbe bleue :
  Season_output$AUC_3[w] = AUC(wst_per$date,wst_per$Season_ST)
  Season_output$AUC_3.q5[w] = AUC(wst_per$date,wst_per$Season_ST.q5)
  Season_output$AUC_3.q95[w] = AUC(wst_per$date,wst_per$Season_ST.q95)
  
  ###Période 4 : 2009 - 2016 :
  wst_per = wst_ST %>% filter(year >= 2009 & year <= 2016) %>% group_by(month,day) %>% summarise(ST_mean = mean(M),ST_mean.q5 = mean(q5),ST_mean.q95 = mean(q95))
  wst_per$date = as.Date(paste(wst_per$day,wst_per$month,sep='-'),'%d-%m')
  Season_output$Per_mean_4[w] = mean(wst_per$ST_mean)
  Season_output$Per_mean_4.q5[w] = mean(wst_per$ST_mean.q5)
  Season_output$Per_mean_4.q95[w] = mean(wst_per$ST_mean.q95)
  
  wst_per$Season_ST = wst_per$ST_mean - mean(wst_ST$M)
  wst_per$Season_ST.q5 = wst_per$ST_mean.q5 - mean(wst_ST$q5)
  wst_per$Season_ST.q95 = wst_per$ST_mean.q95 - mean(wst_ST$q95)
  wst_per[wst_per$Season_ST < 0,]$Season_ST = 0
  wst_per[wst_per$Season_ST.q5 < 0,]$Season_ST.q5 = 0
  wst_per[wst_per$Season_ST.q95 < 0,]$Season_ST.q95 = 0
  
  wst_per$season = 'Saison'
  wst_per$season.q5 = 'Saison'
  wst_per$season.q95 = 'Saison'
  wst_per[wst_per$Season_ST == 0,]$season = 'Hors saison'
  wst_per[wst_per$Season_ST.q5 == 0,]$season.q5 = 'Hors saison'
  wst_per[wst_per$Season_ST.q95 == 0,]$season.q95 = 'Hors saison'
  wst_per$season = as.factor(wst_per$season)
  wst_per$season.q5 = as.factor(wst_per$season.q5)
  wst_per$season.q95 = as.factor(wst_per$season.q95)
  wst_per4_list[[w]] = wst_per
  
  D_InSeason = length(wst_per$season[wst_per$season == 'Saison'])
  D_InSeason.q5 = length(wst_per$season.q5[wst_per$season.q5 == 'Saison'])
  D_InSeason.q95 = length(wst_per$season.q95[wst_per$season.q95 == 'Saison'])
  Season_output$D_InSeason_4[w] = D_InSeason
  Season_output$D_InSeason_4.q5[w] = D_InSeason.q5
  Season_output$D_InSeason_4.q95[w] = D_InSeason.q95

  plot_list[[4]] = ggplot(wst_per) +
    scale_x_date(labels = date_format('%d %b')) +
    geom_line(aes(x = date, y = ST_mean,color = "Favorabilité climatique journalière")) +
    geom_hline(aes(yintercept = Season_output$Global_mean[w],color = 'Moyenne à long terme \nde favorabilité climatique')) +
    scale_color_manual(values = c("Favorabilité climatique journalière" = "#112446", 'Moyenne à long terme \nde favorabilité climatique' = "red")) +
    theme_minimal() + xlab('Date (2009 - 2016)') + ylab(paste('Favorabilité climatique (',colnames(temp_mean[w+1]),')',sep='')) +
    theme(legend.title = element_blank()) + ylim(-0.05,0.7)
  
  #Aire au dessus de la courbe rouge et sous la courbe bleue :
  Season_output$AUC_4[w] = AUC(wst_per$date,wst_per$Season_ST)
  Season_output$AUC_4.q5[w] = AUC(wst_per$date,wst_per$Season_ST.q5)
  Season_output$AUC_4.q95[w] = AUC(wst_per$date,wst_per$Season_ST.q95)
  
  ###Output des plots :
  Season_plot[[w]] = plot_list
  print(paste(round(100*w/(length(temp_mean)-1),digits = 0),'% completed'))
  }
rm(wst_per,wst_ST,plot_list)

#Indicateurs du changement dans la saisonnalité :
#Aire sous la courbe :
AUC_change = Season_output %>% select(AUC_1,AUC_2,AUC_3,AUC_4)
colnames(AUC_change) <- c("1983 - 1991", "1992 - 2000","2001 - 2008","2009 - 2016")
AUC_change = AUC_change %>% pivot_longer(cols = everything(), names_to = "Period", values_to = "Value")

Season_output$AUC_change = ((Season_output$AUC_4/Season_output$AUC_1)-1)*100
Season_output$AUC_change.q5 = ((Season_output$AUC_4.q5/Season_output$AUC_1.q5)-1)*100
Season_output$AUC_change.q5[is.na(Season_output$AUC_change.q5)] = 0
Season_output$AUC_change.q95 = ((Season_output$AUC_4.q95/Season_output$AUC_1.q95)-1)*100

ggplot(data = AUC_change) +
  aes(x = Period,y = Value) +
  geom_boxplot() + theme_bw() +
  xlab('Période') + ylab('Aire sous la courbe')
rm(AUC_change)
#L'Area under the curve a augmenté dans 25 dans 28 stations

AUC = Season_output %>% select(Station,AUC_1,AUC_2,AUC_3,AUC_4,AUC_change)
t.test(AUC$AUC_1,AUC$AUC_2, paired = TRUE)   #significatif => oui
((mean(AUC$AUC_2)/mean(AUC$AUC_1)) - 1) *100 #changement moyen 62.28
t.test(AUC$AUC_1,AUC$AUC_3, paired = TRUE)   #significatif => oui
((mean(AUC$AUC_3)/mean(AUC$AUC_1)) - 1) *100 #changement moyen 88.63
t.test(AUC$AUC_1,AUC$AUC_4, paired = TRUE)   #significatif => oui
((mean(AUC$AUC_4)/mean(AUC$AUC_1)) - 1) *100 #changement moyen 50.97
t.test(AUC$AUC_2,AUC$AUC_3, paired = TRUE)   #significatif => oui
((mean(AUC$AUC_3)/mean(AUC$AUC_2)) - 1) *100 #changement moyen 16.23
t.test(AUC$AUC_2,AUC$AUC_4, paired = TRUE)   #significatif => non
((mean(AUC$AUC_4)/mean(AUC$AUC_2)) - 1) *100 #changement moyen /
t.test(AUC$AUC_3,AUC$AUC_4, paired = TRUE)   #significatif => oui
((mean(AUC$AUC_4)/mean(AUC$AUC_3)) - 1) *100 #changement moyen -19.96

#Jours de saison :
DIS_change = Season_output %>% select(D_InSeason_1,D_InSeason_2,D_InSeason_3,D_InSeason_4)
colnames(DIS_change) <- c("1983 - 1991", "1992 - 2000","2001 - 2008","2009 - 2016")
DIS_change = DIS_change %>% pivot_longer(cols = everything(), names_to = "Period", values_to = "Value")

Season_output$DIS_change = ((Season_output$D_InSeason_4/Season_output$D_InSeason_1)-1)*100
ggplot(data = DIS_change) +
  aes(x = Period,y = Value) +
  geom_boxplot() + theme_bw() +
  xlab('Période') + ylab('Nombre de jours de saison')
rm(DIS_change)
#La saison de transmission s'est allongée dans 25 des 28 stations

DIS = Season_output %>% select(Station,D_InSeason_1,D_InSeason_2,D_InSeason_3,D_InSeason_4,DIS_change)
t.test(DIS$D_InSeason_1,DIS$D_InSeason_2, paired = TRUE)  #significatif => oui
((mean(DIS$D_InSeason_2)/mean(DIS$D_InSeason_1)) - 1) *100 #changement moyen 27.27
t.test(DIS$D_InSeason_1,DIS$D_InSeason_3, paired = TRUE)  #significatif => oui
((mean(DIS$D_InSeason_3)/mean(DIS$D_InSeason_1)) - 1) *100 #changement moyen 39.06
t.test(DIS$D_InSeason_1,DIS$D_InSeason_4, paired = TRUE)  #significatif => oui
((mean(DIS$D_InSeason_4)/mean(DIS$D_InSeason_1)) - 1) *100 #changement moyen 42.87
t.test(DIS$D_InSeason_2,DIS$D_InSeason_3, paired = TRUE)  #significatif => oui
((mean(DIS$D_InSeason_3)/mean(DIS$D_InSeason_2)) - 1) *100 #changement moyen 9.26
t.test(DIS$D_InSeason_2,DIS$D_InSeason_4, paired = TRUE)  #significatif => non
((mean(DIS$D_InSeason_4)/mean(DIS$D_InSeason_2)) - 1) *100 #changement moyen /
t.test(DIS$D_InSeason_3,DIS$D_InSeason_4, paired = TRUE)  #significatif => non
((mean(DIS$D_InSeason_4)/mean(DIS$D_InSeason_3)) - 1) *100 #changement moyen /

#Remarque : lorsque les plots sont extraits de la liste, w doit être modifié afin de correspondre à la station météo voulue
##Station avec la plus grosse augmentation (+437.2 %) :
w = 5
Season_plot[[w]][[1]]
Season_plot[[w]][[4]]
Season_output$Global_mean[w] #0.07

##Station avec la plus grosse diminution (-68.1 %) :
w = 11
Season_plot[[w]][[1]]
Season_plot[[w]][[4]]
Season_output$Global_mean[w] #0.38

##Suitability moyenne pour chaque période par Station :
resume_ST = Season_output %>% select(Station,Global_mean,Per_mean_1,Per_mean_2,Per_mean_3,Per_mean_4)
t.test(resume_ST$Per_mean_1,resume_ST$Per_mean_2, paired = TRUE)  #significatif => oui
((mean(resume_ST$Per_mean_2)/mean(resume_ST$Per_mean_1)) - 1) *100 #changement moyen 12.68
t.test(resume_ST$Per_mean_1,resume_ST$Per_mean_3, paired = TRUE)  #significatif => oui
((mean(resume_ST$Per_mean_3)/mean(resume_ST$Per_mean_1)) - 1) *100 #changement moyen 13.29
t.test(resume_ST$Per_mean_1,resume_ST$Per_mean_4, paired = TRUE)  #significatif => oui
((mean(resume_ST$Per_mean_4)/mean(resume_ST$Per_mean_1)) - 1) *100 #changement moyen 12.86
t.test(resume_ST$Per_mean_2,resume_ST$Per_mean_3, paired = TRUE)  #significatif => non
((mean(resume_ST$Per_mean_3)/mean(resume_ST$Per_mean_2)) - 1) *100 #changement moyen 0.55
t.test(resume_ST$Per_mean_2,resume_ST$Per_mean_4, paired = TRUE)  #significatif => non
((mean(resume_ST$Per_mean_4)/mean(resume_ST$Per_mean_2)) - 1) *100 #changement moyen 0.15
t.test(resume_ST$Per_mean_3,resume_ST$Per_mean_4, paired = TRUE)  #significatif => non
((mean(resume_ST$Per_mean_4)/mean(resume_ST$Per_mean_3)) - 1) *100 #changement moyen -0.39

resume_ST$ST_change = (resume_ST$Per_mean_4/resume_ST$Per_mean_1 -1)*100
resume_ST = resume_ST %>% select(Station,Global_mean,ST_change)
write.csv2(x = resume_ST, file = 'GIS/output/LongTerm_ST.csv')

#Diagramme de dispersion
data_disp = Season_output %>% select(Station,DIS_change,AUC_change)
data_disp$Label <- ifelse(data_disp$DIS_change > 200 | data_disp$AUC_change > 200 | data_disp$DIS_change < -50, data_disp$Station, "")
ggplot(data_disp, aes(x = DIS_change, y = AUC_change)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  geom_text(aes(label = Label), vjust = 0.4, hjust = -0.2) +
  labs(x = "Changement de NJS (%)", 
       y = "Changement d'ASC (%)") +
  theme_bw() +
  xlim(-100, 500) +
  ylim(-100, 500)

#





