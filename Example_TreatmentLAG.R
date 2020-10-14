library(dse)
library(mgcv)
library(plyr)
library(zoo)
library(reshape2)
library(DTVEM)
library(OpenMx); library(lme4); library(mice)
#One manifest variable has a significant effect on itself at Lag-1 and the coefficient is 0.5; #One exogenous variable has a significant effect on X at Lag-7 and the coefficient is 0.8; 
AR <- array(c(1, -0.5,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0 , 0,0,0,0,0,0,0,0,0,  1,0,0,0,0,0,0,0,0) ,c(9,1,1))
C <- array(c(0,0,0,0,0,0,0,0.8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.8),c(9,1,1))
VARX <- ARMA(A=AR, B=1,C=C)
show(VARX) # The VARX structure of simulated data
test_dataoutput=matrix(nrow = 1,ncol = 3)
test_allinput=matrix(nrow = 1,ncol = 3) 
for (i in 1:100) {
  exo_condition = sample(1:2,10000,replace=TRUE)
  input_data1 = matrix(ifelse(exo_condition==1,1,0))
  allinput = input_data1
  test_data=simulate(VARX,input = allinput ,sampleT = 10000)
  dataoutput=unlist(test_data[[2]])
  dataoutput = dataoutput[(length(dataoutput)-13):length(dataoutput)]
  allinput = allinput[(nrow(allinput)-13):nrow(allinput),]
  id=rep(i,14)
  time=rep(1:14)
  data_output=cbind(time,dataoutput,id)
  data_input=cbind(time,allinput,id)
  test_dataoutput=rbind(test_dataoutput,data_output)
  test_allinput=rbind(test_allinput,data_input)
}
test_dataoutput=test_dataoutput[-1,] 
test_allinput=test_allinput[-1,] 
test_dataoutput=data.frame(test_dataoutput)
test_allinput=data.frame(test_allinput)
names(test_dataoutput)=c("Time","X","ID") 
names(test_allinput)=c("Time","U","ID") 
data = test_dataoutput
test_dataoutput = test_dataoutput[2:(ncol(test_dataoutput)-1)]
test_allinput = test_allinput[2:(ncol(test_allinput)-1)]
all_data = cbind(data["Time"],test_dataoutput,test_allinput,data["ID"]) # Simulated dataset
# Results of LMM estimation
test_onemanifest_lmmresults = TreatmentLAG("X","U",model = "LMM",exovariablesinuvector = FALSE,varnamesexo = c("U"),differentialtimevaryingpredictors=c("X","U"),outcome=c("X"),data=all_data,ID="ID",Time="Time",k=9,standardized="centerALL")
# Results of SMM estimation
test_onemanifest_ssmresults = TreatmentLAG("X","U",model = "SSM",exovariablesinuvector = FALSE,varnamesexo = c("U"),differentialtimevaryingpredictors=c("X","U"),outcome=c("X"),data=all_data,ID="ID",Time="Time",k=9,standardized="centerALL")


# Two manifest variables and one exogenous variable 
AR <- array(c(1,-0.5,0,0,0,0,0,0,   0,0.5,0,0,0,0,0,0,  0,0,0,0,0,0,0,-0.5,  1,0,0,-0.7,0,0,0,0) ,c(8,2,2))
C <- array(c(0,0,0,0,0,0,0,0.8,0,0,0,0,0,0,0,0),c(8,2,1))
VARX <- ARMA(A=AR, B=diag(1,2), C=C)
show(VARX) # The VARX structure of simulated data
test_dataoutput=matrix(nrow = 1,ncol = 4)
test_allinput=matrix(nrow = 1,ncol = 3)
for (i in 1:100) {
  exo_condition = sample(1:2,10000,replace=TRUE)
  input_data1 = matrix(ifelse(exo_condition==1,1,0))
  allinput = input_data1
  test_data=simulate(VARX,input = allinput ,sampleT = 10000)
  dataoutput=unlist(test_data[[2]])
  dataoutput = dataoutput[(nrow(dataoutput)-13):nrow(dataoutput),]
  allinput = allinput[(nrow(allinput)-13):nrow(allinput),]
  id=rep(i,14)
  time=rep(1:14)
  data_output=cbind(time,dataoutput,id)
  data_input=cbind(time,allinput,id)
  test_dataoutput=rbind(test_dataoutput,data_output)
  test_allinput=rbind(test_allinput,data_input)
}
test_dataoutput=test_dataoutput[-1,] 
test_allinput=test_allinput[-1,] 
test_dataoutput=data.frame(test_dataoutput)
test_allinput=data.frame(test_allinput)
names(test_dataoutput)=c("Time","X","Y","ID") 
names(test_allinput)=c("Time","U","ID") 
data = test_dataoutput
test_dataoutput = test_dataoutput[2:(ncol(test_dataoutput)-1)]
test_allinput = test_allinput[2:(ncol(test_allinput)-1)]
all_data = cbind(data["Time"],test_dataoutput,test_allinput,data["ID"]) # Simulated dataset

# Results of LMM estimation
test_two_manifest_lmmresults = TreatmentLAG("X","Y","U",exovariablesinuvector = FALSE,model = "LMM",varnamesexo = c("U"),differentialtimevaryingpredictors=c("X","Y","U"),outcome=c("X","Y"),data=all_data,ID="ID",Time="Time",k=9,predictionstart=1,predictionsend=10,predictionsinterval=1,standardized="centerALL")
# Results of SMM estimation
test_two_manifest_ssmresults = TreatmentLAG("X","Y","U",exovariablesinuvector = FALSE,model = "SSM",varnamesexo = c("U"),differentialtimevaryingpredictors=c("X","Y","U"),outcome=c("X","Y"),data=all_data,ID="ID",Time="Time",k=9,predictionstart=1,predictionsend=10,predictionsinterval=1,standardized="centerALL")
