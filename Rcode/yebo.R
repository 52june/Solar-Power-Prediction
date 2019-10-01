pred1 = read.csv("F:/submit2/pred-05.csv", stringsAsFactors = FALSE)
pred2 = read.csv("F:/submit2/pred-0607.csv", stringsAsFactors = FALSE)

date1 = c(rep("2019-03-05", each = 24), "2019-03-06")
time1 = c(paste0(rep(0:23), ":", c("00")), "0:00")
tt1 = data.frame(date = date1, time = time1, stringsAsFactors = FALSE)

date2 = c(rep(c("2019-03-06", "2019-03-07"), each = 24), "2019-03-08")
time2 = c(rep(c(paste0(rep(0:23), ":", c("00"))), 2), "0:00")
tt2 = data.frame(date = date2, time = time2, stringsAsFactors = FALSE)

library(dplyr)
library(zoo)
pred11 = left_join(tt1, pred1, by = c("date", "time"))
pred21 = data.frame(pred11[, 1:2], apply(pred11[, -c(1:2)], 2, na.spline, na.rm = FALSE))

pred12 = left_join(tt2, pred2, by = c("date", "time"))
pred22 = data.frame(pred12[, 1:2], apply(pred12[, -c(1:2)], 2, na.spline, na.rm = FALSE))

rad =pi/180                                  # radian degree
w = seq(-180,180-360/24,length=24)*rad       # time degree for 15 mins
N = 1:365                                         # Jan 1st=1, Jan 2nd =2 ,... Dec 31th = 365
delta = (23.5 * sin((N-80)*360/365*rad))*rad # axial tilt
phi = 37*rad                                 # latitude of gyorori

alpha = function(delta) asin(cos(phi) * cos(delta) * cos(w) + sin(phi) * sin(delta)) 

altitude <- c()
for(i in 1:365){
  temp = ifelse(alpha(delta[i])>0 & alpha(delta[i]) <pi/2, alpha(delta[i]), NA) # altitude of the sun
  altitude = c(altitude, temp)
}
AM = 1/cos(pi/2 - altitude) # air mass
I = 1367 * (0.7)^AM         # solar irradiance
I[is.na(I)] = 0             # NA to 0

from = as.Date("2019-01-01")
to = as.Date("2019-12-31")

final_solar = data.frame(date = rep(as.character(as.Date(seq(from,to,by=1))),each=24), 
                         time = rep(paste0(rep(0:23), ":", c("00")), 365),
                         solar = I,
                         stringsAsFactors = F)

pred31 = left_join(pred21, final_solar, by = c("date", "time"))
pred32 = left_join(pred22, final_solar, by = c("date", "time"))

