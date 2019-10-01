rm(list=ls())
setwd("F:/submit2/2.raw")
source("../preprocessing-ftn.R")
library(dplyr)
library(zoo)
library(data.table)
library(lubridate)

#===================================================================
# 1. YY-MM-DD formatting
#===================================================================

#--- Data reading
dir_from = "./data1-raw/"
dir_to   = "./data2-yymmdd/"

yymmdd(dir_from, dir_to, site="A")
yymmdd(dir_from, dir_to, site="B")
yymmdd(dir_from, dir_to, site="C")


#===================================================================
# 2. 1MIN -> 15MINs (instant data)
#===================================================================

dir_from = "./data2-yymmdd/"
dir_to   = "./data3-15min/"
file_list = list.files(dir_from)

min_list = file_list[substr(file_list,10,13)=="min_"]

file<-min_list

for (i in 1:length(min_list)){
  min15csv(file[i])
}

#===================================================================
# 3. Total Weather: combining several lists 
# Bsite1: 161101-170331
# Csite1: 170401-180331
# Bsite3: 180401-180930
# Asite3: 180101-181009
# Bsite4: 181010-181130
# Asite4: 181201-181231
# new   : 190101-190228
#----------------------
# TOTAL : 161101-180228
# NROW  : 72864(=759days*24hrs*4times) 
#===================================================================

dir_from ="./data3-15min/"
file_list = list.files(dir_from) 

A_weather_list = file_list[substr(file_list,1,6)=="A_Site"&substr(file_list,10,14)=="min15"]
B_weather_list = file_list[substr(file_list,1,6)=="B_Site"&substr(file_list,10,14)=="min15"]
C_weather_list = file_list[substr(file_list,1,6)=="C_Site"&substr(file_list,10,14)=="min15"]
  
needed_weather_list = c(B_weather_list[1],C_weather_list, B_weather_list[3],
                        A_weather_list[3],B_weather_list[4],A_weather_list[4])

total_weather = data.frame()
for (file in needed_weather_list){
  print(file)
  temp = read.csv(paste0(dir_from, file),header=TRUE, stringsAsFactors=FALSE)
  file_site = substr(file,1,1)
  file_no = substr(file,8,8)
  temp_date = as.Date(temp$date, format= "%y-%m-%d")
  
  first_cond  = 
    temp_date>=as.Date("18-10-01",format="%y-%m-%d") & 
    temp_date<=as.Date("18-10-09",format="%y-%m-%d")
  
  second_cond = 
    temp_date>=as.Date("18-10-10",format="%y-%m-%d") & 
    temp_date<=as.Date("18-11-30",format="%y-%m-%d")
  
  third_cond  = 
    temp_date>= as.Date("18-12-01",format="%y-%m-%d") & 
    temp_date<= as.Date("18-12-31",format="%y-%m-%d")
  
  if(file_site=="A"& file_no=="3") {
    temp <- temp[first_cond,]
  } else if( file_site=="B"& file_no=="4") {
    temp <- temp[second_cond,]
  } else if (file_site=="A" & file_no=="4"){
    temp <- temp[third_cond,]
  } 
  
  total_weather = rbind(total_weather,temp)  
}
#--- Check
print(paste(nrow(total_weather),"rows only exist, so", 72864-nrow(total_weather),"rows are omitted."))


#============================================================
#  4. Delete Night-Time
#============================================================
library(lubridate)

total_weather$date <- as.Date(ymd(total_weather$date))
total_weather <- total_weather %>% mutate(time_index = paste(date, time) %>% ymd_hm)
total_weather <- subset(total_weather, hour(time_index) >= 5 & hour(time_index) <= 20) # daytime
total_weather <- total_weather %>% group_by(date) %>% slice(1:(n()-3))

full_time <- filter(total_weather, date == "2017-01-02")[,2] %>% pull()
date_list <- unique(total_weather$date)

full <- expand.grid(date = date_list, time = full_time, stringsAsFactors = F) %>% arrange(date)

full_weather <- left_join(full, total_weather, by = c("date","time"))


##============================================================
##  5. rain / frain
##============================================================
# [input] dataframe [output] crain --> rain 
#-------------------------------------------------------------
# 1) interpolation
# 2) diff
#-------------------------------------------------------------
rain<-function(data){
  temp<-data$crain
  temp<-matrix(temp,nrow=61)
  
  # treat initial (add 0)
  temp<-rbind(0,temp)
  
  # inter
  inter<-apply(temp,2,na.approx)
  
  # add NA
  inter %>% lapply(length) %>% unlist %>% max -> mymax
  inter %>% lapply(function(x) c(x,rep(NA,mymax-length(x)))) -> addna
  
  # diff
  addna %>% lapply(diff) %>% unlist -> rain
  
  # data<-data %>% select(-crain)
  data<-cbind(data,rain)
  return(data.frame(data))
}

frain<-function(data){
  temp<-data
  rain<-data$rain
  frain<-ifelse(rain>0,1,0)
  temp<-cbind(data,frain)
  temp <-temp %>% subset(time!="5:00")
  return(data.frame(temp))
}

total_rain <- frain(rain(full_weather))
total_rain <- select(total_rain, -time_index, -crain)

##============================================================
##  6. New data (weather)
##============================================================
Jan <- fread("data1-raw/2019_Jan.csv");Feb <- fread("data1-raw/2019_Feb.csv")
new_weather <- rbind(Jan, Feb)

# Time & Date Processing
new_weather$date <- substr(new_weather$time, 1, 10)
new_weather$time <- substr(new_weather$time, 12, 16)
new_weather <- new_weather[,c(11,1:10)]
new_weather$min <- as.numeric(substr(new_weather$time, nchar(new_weather$time)-1, nchar(new_weather$time)))
min15 <- filter(new_weather, min%%15 == 0)
min15 <- select(min15, -min)

# Delete Night-Time
min15$date <- as.Date(ymd(min15$date))
min15 <- min15 %>% mutate(time_index = paste(date, time) %>% ymd_hm)
min15 <- subset(min15, hour(time_index) >= 5 & hour(time_index) <= 20) # daytime
min15 <- min15 %>% group_by(date) %>% slice(1:(n()-3))

# rain / frain
full_time <- filter(min15, date == "2019-01-02")[,2] %>% pull()
date_list <- unique(min15$date)
full <- expand.grid(date = date_list, time = full_time, stringsAsFactors = F) %>% arrange(date)
full_min15 <- left_join(full, min15, by = c("date","time"))

min15_rain <- frain(rain(full_min15))
min15_rain <- min15_rain %>% select(-time_index,-crain)

# cloud
newcloud <-fread('data1-raw/A_Site_5_hour_0101~0224.csv')
newcloud$date <- substr(newcloud$time, 1, 10)
newcloud$time <- substr(newcloud$time, 12, 16)
newcloud <- newcloud[,c(11,1:10)]
newcloud <- newcloud[,c(3,1,2)]
newcloud$date <- substr(newcloud$date,3,10)
write.csv(newcloud,"./data2-yymmdd/A_Site_5_hour_0101~0224.csv",row.names=F)

##============================================================
##  7. Total Weather
##============================================================
final_weather <- rbind(total_rain, min15_rain)

#===================================================================
#   8. cloud data: 1HR -> 15MIN 
#    interpolation & left join to total weather data
#===================================================================

dir_from = "./data2-yymmdd/"

from = as.Date("2016-11-01")
to = as.Date("2019-2-24")

tmp2 = data.frame(date = substr(rep(as.character(as.Date(seq(from,to,by=1))), each=96), 3, 10), 
                  time = paste0(rep(c(paste0("0", 0:9), 10:23), each = 4), ":", c("00", "15", "30", "45")),
                  stringsAsFactors = FALSE)

file_list = list.files(dir_from) 

A_weather_list = file_list[substr(file_list,1,1)=="A"&substr(file_list,10,13)=="hour"]
B_weather_list = file_list[substr(file_list,1,1)=="B"&substr(file_list,10,13)=="hour"]
C_weather_list = file_list[substr(file_list,1,1)=="C"&substr(file_list,10,13)=="hour"]

for(dat in c(A_weather_list, B_weather_list, C_weather_list))
{
  print(dat)
  tmp = read.csv(paste0(dir_from, dat), stringsAsFactors = FALSE)
  tmp2 = left_join(tmp2, tmp %>% select(date, time, cloud), by = c("date", "time")) 
}

weather_cloud = data.frame(tmp2[,1:2], cloud = apply(tmp2[,-c(1:2)], 1, mean, na.rm = TRUE))

clouddat = data.frame(weather_cloud[, 1:2],
                      cloud = c(na.approx(weather_cloud[1:35041, 3], na.rm = FALSE), 
                                rep(NA, 26208),
                                na.approx(weather_cloud[61250:nrow(weather_cloud), 3], na.rm = FALSE)),
                      stringsAsFactors = FALSE)
hist(clouddat$cloud)
clouddat$cloud[clouddat$cloud > 10] = 10 
clouddat$cloud[clouddat$cloud < 0 ] = 0 

# change format of final_weather (Date -> chr)

final_weather$date <- as.character(final_weather$date)
final_weather$date <- substr(final_weather$date, 3, nchar(final_weather$date))
total_cloud = left_join(final_weather,clouddat,by=c("date","time"))

#================================================
#   9. Solar variable
#================================================

rad =pi/180                                  # radian degree
w = seq(-180,180-360/96,length=96)*rad       # time degree for 15 mins
N = 1:365                                    # Jan 1st=1, Jan 2nd =2 ,... Dec 31th = 365
delta = (23.5 * sin((N-80)*360/365*rad))*rad # axial tilt
phi = 37*rad                                 # latitude of gyorori

# altitude
alpha = function(delta) asin(cos(phi) * cos(delta) * cos(w) + sin(phi) * sin(delta)) 

altitude <- c()
for(i in 1:365){
  temp = ifelse(alpha(delta[i])>=0 & alpha(delta[i]) <=pi/2, alpha(delta[i]), NA) 
  # altitude of the sun
  altitude = c(altitude, temp)
}
altitude[is.na(altitude)] = 0

AM = 1/cos(pi/2 - altitude) # air mass
I = 1367 * (0.7)^AM         # solar irradiance
I[is.na(I)] = 0             # NA to 0

from = as.Date("2016-11-01")
to   = as.Date("2019-02-24")

y2019 = as.Date("2019-01-01")
y2017 = as.Date("2017-01-01")

I_part = tail(I,as.numeric(y2017-from)*96)
I_part2 = head(I,as.numeric(to-y2019+1)*96)

final_solar = data.frame(date  = substr(rep(as.character(as.Date(seq(from,to,by=1))),each=96),3,10), 
                         time  = rep(paste0(rep(c(paste0(0,0:9), 10:23), each = 4), ":", c("00", 15, 30, 45))),
                         solar = c(I_part,I,I,I_part2),
                         stringsAsFactors = F)

total_solar = left_join(total_cloud, final_solar, by=c("date", "time"))


#================================================
#   10-1. Combine with Power Data (A site)
#================================================
asite_1 <- fread("data2-yymmdd/Asite_1_0701~1119.csv")
asite_1 <- asite_1[,-87]
asite_2 <- fread("data2-yymmdd/Asite_2_0301~0515.csv")
asite_3 <- fread("data2-yymmdd/Asite_3_0620~1009.csv")
asite_4 <- fread("data2-yymmdd/Asite_4_1101~1231.csv")
asite_5 <- fread("data2-yymmdd/Asite_5_0101~0224.csv")


asite <- rbind(asite_1, asite_2, asite_3, asite_4, asite_5)
asite_total <- left_join(asite, total_solar)

# Delete Night-Time
asite_total$date <- as.Date(ymd(asite_total$date))
asite_total <- asite_total %>% mutate(time_index = paste(date, time) %>% ymd_hm)
asite_total <- subset(asite_total, hour(time_index) >= 5 & hour(time_index) <= 20) # daytime
asite_total <- asite_total %>% group_by(date) %>% slice(1:(n()-3))

# Remove channel data
asite_total <- asite_total[,-(9:86)]

# Remove weird date
asite_total = asite_total[complete.cases(asite_total),] # Remove NA
asite_total = asite_total[asite_total$cpac != 0,]       # Remove cpac = 0
asite_total[diff(asite_total$cpac) == 0 & diff(asite_total$mtemp) == 0,] # nothing but first 300 obs are strange

asite_total = asite_total[-c(1:300),] %>% select(-time_index) # weird trend

#================================================
#   10-2. Combine with Power Data (B site)
#================================================
bsite_1 <- fread("data2-yymmdd/Bsite_1_1101~0331.csv")
bsite_2 <- fread("data2-yymmdd/Bsite_2_0902~0228.csv")
bsite_3 <- fread("data2-yymmdd/Bsite_3_0401~0930.csv")
bsite_4 <- fread("data2-yymmdd/Bsite_4_1005~1130.csv")
bsite_4 <- bsite_4[,-c(61:110)]
bsite_5 <- fread("data2-yymmdd/Bsite_5_0101~0224.csv")

bsite <- rbind(bsite_1, bsite_2, bsite_3, bsite_4, bsite_5)
bsite_total <- left_join(bsite, total_solar)

# Delete Night-Time
bsite_total$date <- as.Date(ymd(bsite_total$date))
bsite_total <- bsite_total %>% mutate(time_index = paste(date, time) %>% ymd_hm)
bsite_total <- subset(bsite_total, hour(time_index) >= 5 & hour(time_index) <= 20) # daytime
bsite_total <- bsite_total %>% group_by(date) %>% slice(1:(n()-3))

# Remove channel data
bsite_total <- bsite_total[,-(10:60)]

# Remove NA
bsite_total = bsite_total[complete.cases(bsite_total),]
bsite_total = bsite_total[bsite_total$cpac != 0,]

# Remove weird date
error_date <- c("2016-11-08","2016-11-09","2016-12-28","2016-12-29","2017-02-16","2017-02-17",
                "2018-06-03","2018-06-04","2018-06-05","2018-06-28","2018-06-29",
                "2018-10-17","2018-10-24","2019-01-29")

bsite_total = bsite_total %>% filter(!(as.character(date) %in% error_date))
bsite_total = bsite_total %>% select(-time_index)

#================================================
#   10-3. Combine with Power Data (C site)
#================================================
csite_1 <- fread("data2-yymmdd/Csite_1_0401~0331.csv")
csite_2 <- fread("data2-yymmdd/Csite_5_0101~0224.csv")

csite <- rbind(csite_1, csite_2)
csite_total <- left_join(csite, total_solar)


# Delete Night-Time
csite_total$date <- as.Date(ymd(csite_total$date))
csite_total <- csite_total %>% mutate(time_index = paste(date, time) %>% ymd_hm)
csite_total <- subset(csite_total, hour(time_index) >= 5 & hour(time_index) <= 20) # daytime
csite_total <- csite_total %>% group_by(date) %>% slice(1:(n()-3))


# Remove channel data
csite_total = csite_total[csite_total$acp_i1 != 0 & csite_total$acp_i2 != 0,] # 인버터 둘다 돌아갈때만
csite_total <- csite_total[,-(9:46)]

# Remove NA
csite_total = csite_total[complete.cases(csite_total),]
csite_total = csite_total[csite_total$cpac != 0,]

# Remove weird date
ind = csite_total[diff(csite_total$cpac) == 0 & diff(csite_total$mtemp) == 0, 1:2]
# C : 5-24, 6-9, 6-13, 6-14, 6-15, 6-16, 6-18, 6-21, 6-23, 8-8
csite_total = csite_total[!(csite_total$date %in% unique(ind[-c(1, nrow(ind)),1])), ] 
csite_total = csite_total %>% select(-time_index)


#================================================
#   11. Write CSV
#================================================
setwd("./data4-total/")
write.csv(asite_total, "A_site_total.csv", row.names = F)
write.csv(bsite_total, "B_site_total.csv", row.names = F)
write.csv(csite_total, "C_site_total.csv", row.names = F)
