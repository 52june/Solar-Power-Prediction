#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#                           0217_TCS                         #
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#  1 > model 1. lm
#  2 > model 2. svr
#  3 > model 3. nn
#  4 > model 4. rf
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

SSCP = function(data,formula, method=c("lm","svr","nn","rf")){

#===================== subfunction ========================
  range01 = function(x) (x-min(x))/(max(x)-min(x))
  truerange = function(y,yhat) yhat*(max(y)-min(y)) + min(y)
  mape_row = function(x)  abs((x[,1]-x[,2])/x[,1])
  mape     = function(x) (100/nrow(x)) * sum(abs((x[,1]-x[,2])/mean(x[,1])))
#======================= formula ==========================  
  if (class(formula)=="list"){
    formula.ss<-formula[[1]]
    formula.cpac<-formula[[2]] }
  else warning("Formula should be list-form")
#======================= library ==========================
  set.seed(433)
  library(e1071)
  library(dplyr)
  library(randomForest)
  library(nnet)
  
    #=====================   modeling   ========================   
    # method : "lm","svr","nn","rf"
    #--------------------------------------------------
    # 1. method = "lm"
    if(method=="lm"){
      # 1.1) sshat for training
      fit_train_sshat = lm(formula.ss, data = data)
      
      # 1.2) cpac
      data$sshat = predict(fit_train_sshat, newdata = data, type = "response")
      fit_train_cpac = lm(formula.cpac, data = data)
      
      # 1.3 ) for test
      data$cpac_hat = predict(fit_train_cpac, newdata = data, type = "response")
      data$mape = mape_row(data %>% select(cpac, cpac_hat))
      
      #== output ==#
      x = data
    }
    
    #--------------------------------------------------
    # 2. method= "svr"
    else if (method=="svr"){
        # 2.1) sshat for training
        fit_train_sshat = svm(formula.ss, data = data,gamma=1,cost=16)

        # 2.2) cpac
        data$sshat = predict(fit_train_sshat, newdata = data, type = "response")
        fit_train_cpac = svm(formula.cpac, data = data,gamma=1,cost=16)
        
        # 2.3 ) for test
        data$cpac_hat = predict(fit_train_cpac, newdata = data, type = "response")
        data$mape = mape_row(data %>% select(cpac, cpac_hat))
        
        #== output ==#
        x = data
    }

    #--------------------------------------------------
    # 3. method= "nn"
    else if (method=="nn"){
      # range: 0 - 1
      sc_data = data %>% mutate(cpac = range01(cpac),
                                ss= range01(ss),
                                temp = range01(temp),
                                swind = range01(swind),
                                rhumid = range01(rhumid),
                                ap = range01 (ap),
                                solar=range01(solar),
                                cloud=range01(cloud),
                                frain=range01(frain),
                                rain = range01(rain))
  
      # scaled data
      data    = sc_data
      raw_test = data
      
      # 3.1) sshat for training
      fit_train_sshat = nnet(formula.ss,size=2, data = data, decay=1e-4, maxit=1000)
      
      # 3.2) cpac
      data$sshat = predict(fit_train_sshat, newdata = data, type = "raw")
      fit_train_cpac = nnet(formula.cpac, size=2, data = data, decay=1e-4, maxit=1000)
      
      # 3.3 ) for test
      raw_test$cpac_hat = truerange(data$cpac, c(predict(fit_train_cpac, newdata = data, type = "raw")))
      raw_test$mape     = mape_row(raw_test %>% select(cpac, cpac_hat))
    
      #== output ==#
      raw_test$sshat = truerange(data$sshat, c(predict(fit_train_sshat, newdata = data, type = "raw")))
      x = raw_test
      
    }
    
  #--------------------------------------------------
    # 4. method= "rf"
    else if (method=="rf"){
        # 4.1) sshat for training
        fit_train_sshat = randomForest(formula.ss, 
                                       data = data, ntree = 500, 
                                       mtry = 4, importance =TRUE)
        # 4.2) cpac
        data$sshat = predict(fit_train_sshat, newdata = data, type = "response")
        fit_train_cpac = randomForest(formula.cpac,
                                      data = data, ntree = 500, 
                                      mtry = 4, importance =TRUE)
        
        # 4.3 ) for test
        data$cpac_hat = predict(fit_train_cpac, newdata = data, type = "response")
        data$mape = mape_row(data %>% select(cpac, cpac_hat))
        
        #== output ==#
        x = data
    
    }
    #--------------------------------------------------
    else stop("Methods should be one of lm, nn, svr, or rf")
  
#=======================   result   ==========================  
  total = x
  mape = mape(total %>% select(cpac, cpac_hat))
  print("done")
  return(list(test=as_tibble(total), mape=mape,
              fss=fit_train_sshat ,fcpac=fit_train_cpac))
}

