#===============================================================
source('F:/submit2/SSCP.R')
library(dplyr)
library(randomForest)
#======================= read dataset ==========================

for (data in c("A", "B", "C"))
{
  tmp = read.csv(paste0("F:/submit2/2.raw/data4-total/", data, "_site_total.csv"))
  assign(data, tmp)
}

#=======================  set formula  ==========================
formula.ss = ss~ temp + swind + rhumid + cloud + solar + rain + frain
formula.cpac = cpac ~ temp + swind + rhumid + cloud + solar + rain + frain + sshat
formula<-list(formula.ss,formula.cpac)

#=========================  result  =============================
method<-c("rf")

for(i in 1:length(method)){
  print(i)
  
  assign(paste0("result_A_",method[i]),SSCP(A,formula=formula,method=method[i]))
  assign(paste0("result_B_",method[i]),SSCP(B,formula=formula,method=method[i]))
  assign(paste0("result_C_",method[i]),SSCP(C,formula=formula,method=method[i]))
}

save(list = ls(pattern = "result"), file = "F:/submit2/fit-result.RData")
load("F:/submit2/fit-result.RData")
load("F:/submit1/fit-result3.RData")

source("F:/submit2/yebo.R")

dataA_rf1 = pred31
dataB_rf1 = pred31
dataC_rf1 = pred31

dataA_rf2 = pred32
dataB_rf2 = pred32
dataC_rf2 = pred32

# full predict

dataA_rf1$sshat = predict(result_A_rf$fss, dataA_rf1, type = "response")
dataB_rf1$sshat = predict(result_B_rf$fss, dataB_rf1, type = "response")
dataC_rf1$sshat = predict(result_C_rf$fss, dataC_rf1, type = "response")
dataA_rf1$cpac = predict(result_A_rf$fcpac, dataA_rf1, type = "response")
dataB_rf1$cpac = predict(result_B_rf$fcpac, dataB_rf1, type = "response")
dataC_rf1$cpac = predict(result_C_rf$fcpac, dataC_rf1, type = "response")

dataA_rf2$sshat = predict(result_A_rf$fss, dataA_rf2, type = "response")
dataB_rf2$sshat = predict(result_B_rf$fss, dataB_rf2, type = "response")
dataC_rf2$sshat = predict(result_C_rf$fss, dataC_rf2, type = "response")
dataA_rf2$cpac = predict(result_A_rf$fcpac, dataA_rf2, type = "response")
dataB_rf2$cpac = predict(result_B_rf$fcpac, dataB_rf2, type = "response")
dataC_rf2$cpac = predict(result_C_rf$fcpac, dataC_rf2, type = "response")

time1 = c("7:00", "7:15", "7:30", "7:45",
          "8:00", "8:15", "8:30", "8:45",
          "9:00", "9:15", "9:30", "9:45",
          "10:00", "10:15", "10:30", "10:45",
          "11:00", "11:15", "11:30", "11:45",
          "12:00", "12:15", "12:30", "12:45",
          "13:00", "13:15", "13:30", "13:45",
          "14:00", "14:15", "14:30", "14:45",
          "15:00", "15:15", "15:30", "15:45",
          "16:00", "16:15", "16:30", "16:45",
          "17:00", "17:15", "17:30", "17:45",
          "18:00", "18:15", "18:30")
time2 = c("7:00", "7:15", "7:30", "7:45",
          "8:00", "8:15", "8:30", "8:45",
          "9:00", "9:15", "9:30", "9:45",
          "10:00", "10:15", "10:30", "10:45",
          "11:00", "11:15", "11:30", "11:45",
          "12:00", "12:15", "12:30", "12:45",
          "13:00", "13:15", "13:30", "13:45",
          "14:00", "14:15", "14:30", "14:45",
          "15:00", "15:15", "15:30", "15:45",
          "16:00", "16:15", "16:30", "16:45",
          "17:00", "17:15", "17:30", "17:45",
          "18:00", "18:15", "18:30",
          "7:00", "7:15", "7:30", "7:45",
          "8:00", "8:15", "8:30", "8:45",
          "9:00", "9:15", "9:30", "9:45",
          "10:00", "10:15", "10:30", "10:45",
          "11:00", "11:15", "11:30", "11:45",
          "12:00", "12:15", "12:30", "12:45",
          "13:00", "13:15", "13:30", "13:45",
          "14:00", "14:15", "14:30", "14:45",
          "15:00", "15:15", "15:30", "15:45",
          "16:00", "16:15", "16:30", "16:45",
          "17:00", "17:15", "17:30", "17:45",
          "18:00", "18:15", "18:30")

date1 = rep("2019-03-05", each = 47)
date2 = rep(c("2019-03-06", "2019-03-07"), each = 47)

# no cluster

A1f = left_join(data.frame(date = date1, time = time1, stringsAsFactors = FALSE), 
                dataA_rf1 %>% select(date, time, cpac))
A1f[c(1, 47), 3] = 0
A1f = A1f %>% mutate(CP = na.spline(cpac))

B1f = left_join(data.frame(date = date1, time = time1, stringsAsFactors = FALSE), 
                dataB_rf1 %>% select(date, time, cpac)) 
B1f[c(1, 47), 3] = 0
B1f = B1f %>% mutate(CP = na.spline(cpac))

C1f = left_join(data.frame(date = date1, time = time1, stringsAsFactors = FALSE), 
                dataC_rf1 %>% select(date, time, cpac))
C1f[c(1, 47), 3] = 0
C1f = C1f %>% mutate(CP = na.spline(cpac))


A2f = left_join(data.frame(date = date2, time = time2, stringsAsFactors = FALSE), 
                dataA_rf2 %>% select(date, time, cpac)) 
A2f[c(1, 47, 48, 94), 3] = 0
A2f = A2f %>% mutate(CP = na.spline(cpac))

B2f = left_join(data.frame(date = date2, time = time2, stringsAsFactors = FALSE), 
                dataB_rf2 %>% select(date, time, cpac)) 
B2f[c(1, 47, 48, 94), 3] = 0
B2f = B2f %>% mutate(CP = na.spline(cpac))

C2f = left_join(data.frame(date = date2, time = time2, stringsAsFactors = FALSE), 
                dataC_rf2 %>% select(date, time, cpac)) 
C2f[c(1, 47, 48, 94), 3] = 0
C2f = C2f %>% mutate(CP = na.spline(cpac))

write.csv(A1f, file = "F:/submit2/result/A_05.csv")
write.csv(A2f, file = "F:/submit2/result/A_0607.csv")
write.csv(B1f, file = "F:/submit2/result/B_05.csv")
write.csv(B2f, file = "F:/submit2/result/B_0607.csv")
write.csv(C1f, file = "F:/submit2/result/C_05.csv")
write.csv(C2f, file = "F:/submit2/result/C_0607.csv")

