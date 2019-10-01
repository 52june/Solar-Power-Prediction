"yymmdd"=function(dir_from, dir_to, site){
  
  file_list = list.files(dir_from)
  file_list = file_list[substr(file_list,1,1)==site]
  
  for (file in file_list){
    print(file)
    temp = read.csv(paste0(dir_from, file),header=TRUE, stringsAsFactors=FALSE)
    colnames(temp)[1] = "time"
    temp$time = trimws(temp$time)                               # remove whitespace
    
    temp = temp[temp$time!="",]
    file_no = ifelse(substr(file, 2,2)=="_", substr(file,8,8),substr(file,7,7)) 
    # if weather data -> file_no is located in the 8th order, else 7th order 
    
    if(site=="A"){
      
      if (file_no=="1") year <- "17"                              # 1st file: year->17
      if (file_no%in%c("2","3","4")) year <-"18"                  # 2~4th file: year->18
      if (file_no=="5") year <- "19"                              # 5th file: year ->19
      
    } else if (site =="B"){
      
      if(file_no=="1") {
        first_cond = substr(temp$time,1,2)=="11"|substr(temp$time,1,2)=="12"
        year       = ifelse(first_cond,"16","17")
      }
      if(file_no=="2"){
        second_cond = substr(temp$time,1,2)=="01"|substr(temp$time,1,2)=="02"
        year        = ifelse(second_cond,"18","17")
      }
      if(file_no%in% c("3","4")) year <-"18"
      if(file_no=="5") year <-"19"
      
    } else if (site=="C"){
      if (file_no=="1"){
        cond  = substr(temp$time,1,2)=="01"|substr(temp$time,1,2)=="02"|substr(temp$time,1,2)=="03"
        year  = ifelse(cond, "18", "17")
      }
      if (file_no=="2") year <- "19"
    } else stop("site should be A, B or C.")
    
    if (file_no!="5"){
      temp$date = paste0(year,"-",substr(temp$time, 1,5))         # date: MM-DD format
      temp$time = substr(temp$time,8,12)                          # time: Hr:Mn format
    } else {
      temp$date = substr(as.Date(substr(temp$time, 1,8),format='%d-%m-%y'),3,10)
      temp$time = substr(temp$time,9,13)
    }
    temp = temp[,c(ncol(temp),1:(ncol(temp)-1))]                # change the column order
    write.csv(temp, paste0(dir_to, file),row.names=FALSE)
  }
}

"min15csv" = function(file){
  
  print(file)
  from<-substr(file,1,12)
  to<-substr(file,13,22)
  
  temp = read.csv(paste0(dir_from, file),header=TRUE, stringsAsFactors=FALSE)
  
  min15<-as.numeric(substr(temp$time,4,5))
  temp<-cbind(min15,temp)
  
  temp15<-dplyr::filter(temp,min15==15|min15==30|min15==45|min15==0)
  temp15<-temp15[,-1]
  
  write.csv(temp15,paste0(dir_to,from,15,to,".csv"),row.names=FALSE)
}

# Weather Processing
"rain" <-function(data){
  
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



"frain" = function(data){
  temp<-data
  rain<-data$rain
  frain<-ifelse(rain>0,1,0)
  temp<-cbind(data,frain)
  temp <-temp %>% subset(time!="5:00")
  return(data.frame(temp))
}