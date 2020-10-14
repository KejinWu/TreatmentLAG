#' @description This is the TreatmentLAG function, this function is aimed for detecting and estimating lag structure of exogenous variables. If there are multiple manifest variables, this function will return multiple results which are based on different fixed manifest variable.
#' @description This function firstly calls the LAG function in this package, and gives one additional satge to estimate the lag structure of variables
#' @param ... A list of manifest and exogenous variable names used in the function e.g. "X","Y" (REQUIRED)
#' @param differentialtimevaryingpredictors The variables that will be a varying-coefficient of differential time (AKA the lags you want to know what times they predict the outcome). This must be specified as a vector using c("variables here"). e.g. c("X","Y") (REQUIRED)
#' @param outcome This is each of the outcome variables. Specified as outcome="outcomevariablename" for a single variable or outcome=c("outcomevariablename1","outcomevariablename2") (REQUIRED)
#' @param data Specify the data frame that contains the data e.g. data=dataframename (REQUIRED)
#' @param ID The name of the ID variable. E.G. ID = "ID" (must be specified). (REQUIRED)
#' @param Time The name of the Time variable. E.G. Time = "Time" (must be specified). (REQUIRED)
#' @param k The number of k selection points used in the model for stage 1 (see ?choose.k in mgcv package for more details) (note that this is for the raw data k2 refers to the k for the re-blocked data), default is 10. The ideal k is the maximum number of data points per person, but this slows down DTVEM and is often not required. (OPTIONAL, BUT RECOMMENDED)
#' @param k2 The number of k selection points used in the model for stage 1 of the blocked data (see ?choose.k in mgcv package for more details). Default is 10. The ideal k is the maximum number of data points per person, but this slows down DTVEM and is often not required. (OPTIONAL)
#' @param k3 The number of k selection points used in the model for the time spline (NOTE THAT THIS CONTROLS FOR TIME TRENDS OF THE POPULATION)  (see ?choose.k in mgcv package for more details). Default is 3. (OPTIONAL)
#' @param k4 The number of k selection points used in the model for the varying coefficient in the intermediate stage (see ?choose.k in mgcv package for more details). Default is 3. (OPTIONAL, BUT RECOMMENDED)
#' @param controlvariables The variables to be controlled for (not lagged). These are traditional covariates in the analysis. These are the variables that will be controlled for in a stationary fashion. To use this use controlvariables = c("list","here") (OPTIONAL)
#' @param controllag The time of the lag which coviarates should be controlled for (NOT CURRENTLY FUNCTIONAL)
#' @param predictionstart The differential time value to start with, default is NULL, and the lowest time difference in the time series will be used (use lower value if you're first value if you're interested in a smaller interval prediction) e.g. predictionstart = 1. If this is not specified and using a continuous time model, make sure to set blockdata = TRUE so that it will be automatically chosen. (OPTIONAL)
#' @param predictionsend The differential time value to end with. This means how long you want your largest time difference in the study to be (i.e. if you wanted to predict up to allow time predictions up to 24 hours and your time intervals were specified in hours, you would set predictionsend = 24). If this is not specified and using a continuous time model, make sure to set blockdata = TRUE so that it will be automatically chosen. (OPTIONAL)
#' @param predictionsinterval The intervals to predict between differential time points. If using discrete time do you want the intervals to be specified every discrete interval, if so set this to 1. If this is not specified and using a continuous time model, make sure to set blockdata = TRUE so that it will be automatically chosen. (OPTIONAL)
#' @param standardized This specifies whether all of the variables (aside from Time) should be standardized. Options are TRUE, FALSE, and "center". TRUE means within-person standardize each variable (aka get the person-centered z-scores), FALSE means use the raw data, "center" means to only within-person mean-center the variables. Default = TRUE. FALSE is not recommended unless you have done these transformations yourself. If there are exogenous variables, the "centerALL" option is recommended. "Centerall" means that every data minus the mean of all data for every variable (OPTIONAL)
#' @param software This is the software used to run the secondary analysis. State-space models are implemented by the argument "OpenMx". The option "gam" can be used to run a traditional multilevel model with a spline that controls for non-linear time trends at the population level. The option "hybrid" first runs a multilevel model then runs an state-space model. Model. Note that the state-space approach can be very slow with large amounts of lags, and consequently "gam" should be used with large amounts of lags are included. However, state-space model estimation is generally marginally superior to the multilevel modeling approach, and if using small amounts of lags or time is not an issue the state-space option is recommended. The default is "OpenMx" which implements multilevel models. (OPTIONAL)
#' @param minimumpracticalsignificance This can be used to set a minimum amount to pass on from DTVEM stage 1 to stage 2, and stage 1.5 to stage 2. This can be useful if too many variables come back as significant, but they would not meet your criteria for practical significance. Set this to a numerical value (e.g. minimumpracticalsignificance=.2). (OPTIONAL, UNCOMMONLY SPECIFIED)
#' @param gamma This can be used to change the wiggliness of the model. This can be useful if the model is too smooth (i.e flat). The lower the number the more wiggly this will be (see ?gam in MGCV for more information). The default is equal to 1. (OPTIONAL, UNCOMMONLY SPECIFIED)
#' @param independentpredictors This is whether or not the wide model comparisons should be run independently and combined via stepwise regression with backward selection. This can be useful to reduce the amount of lags included in the confirmatory model. Default is FALSE. (OPTIONAL) 
#' @param minN The smallest N that will be considered in the stage 2 model (i.e. this can be important in case you don't have observations at certain differential times, such as overnight observations). Default = 30. (OPTIONAL)
#' @param ResidualAnalysis Only applies when software = "OpenMx". Analyze the residuals of the time series with OpenMx after factoring out the non-linear effect of time (takes time trends into account). Can be run only at the group level (faster), or it can also be run with a random effect splines of time (slower) by setting ResidualAnalysis = "Individual". Default = "Group" (OPTIONAL)
#' @param blockdata This re-organizes the raw data into blocks after an exploratory first stage. Default = FALSE. TRUE = Automatic re-organization of data based on the minimum lag number and the time between two lags peaks/valleys. Including a numeric number will automatically re-block the data into chunks at those specific intervals. (OPTIONAL, UNCOMMONLY SPECIFIED)
#' @param rounddecimals The default option is TRUE which to automatically rounds the decimals to the smallest non-zero decimal place in the data. Can also specify a number to round to a specific decimal place (e.g. 1 = the tenths digit, 2 = the hundredths digit, 0 = the nearest whole number, -1 = the nearest 10th number) (OPTIONAL, UNCOMMONLY SPECIFIED)
#' @param OpenMxStartingValues Only applies when software = "OpenMx". Specify the starting values for OpenMx. Since OpenMx will 10 different runs before giving up on convergence this does not usually need to be specified. It should mostly only be specified if there is a convergence issue with OpenMx. Default is 0.3.  (OPTIONAL, UNCOMMONLY SPECIFIED)
#' @param maxintermediaterounds The maximum number of intermediate stages to perform. Default is 10 (OPTIONAL, UNCOMMONLY SPECIFIED)
#' @param debug This will print more useless information as it goes along. Only useful for troubleshooting problems. (OPTIONAL, UNCOMMONLY SPECIFIED)
#' @param differntialtimevaryingpredictors This is depricated. Only retained for backward compatibility. 
#' @param exovariablesinuvector Determine whether the exo-variables are in u vector or not, default is FALSE. 
#' @param varnamesexo The names of exo-variables, and the varnames_exo should also be in the input-list. Default is NULL
#' @param MItimes The time of doing multiple imputation, default value is 10
#' @param model Determine which model's estimation results will be returned, default value is "LMM". Choices are SSM and LMM
TreatmentLAG = function(...,exovariablesinuvector = FALSE,model = "LMM",varnamesexo = NULL,MItimes = 10 ,differentialtimevaryingpredictors=NULL,outcome=NULL,controlvariables=NULL,data=NULL,ID="ID",Time=NULL,k=10,k2=10,k3=3,k4=3,controllag=NULL,standardized=TRUE,predictionstart=NULL,predictionsend=NULL,predictionsinterval=NULL,software="OpenMx",independentpredictors=FALSE,minimumpracticalsignificance=NULL,gamma=1,minN=30,debug=FALSE,OpenMxStartingValues=.3,ResidualAnalysis="Group",blockdata=FALSE,rounddecimals=TRUE,maxintermediaterounds=10,differntialtimevaryingpredictors=NULL)
  {
  N = max(data["ID"])
  Timepoints = max(data["Time"])
  All_results = vector("list",length = length(outcome))
  for (nnn in 1:length(outcome)) { # If there are n number of manifest variables, then we analyze n times. And fix one manifest variable as outcome and take other manifest variables as exogenous part for each time
   cat(paste("This is the analysis of",outcome[nnn],", fix other variables in outcome as exogenous part", sep = " "))
   varnames_exo_eachtime = c(varnamesexo,outcome[-nnn])
   outcome_eachtime = c(outcome[nnn])
   trytest = try(LAG(...,exovariablesinuvector = exovariablesinuvector,varnamesexo = varnames_exo_eachtime,independentpredictors = independentpredictors,differentialtimevaryingpredictors=differentialtimevaryingpredictors,outcome=outcome_eachtime,data=data,ID=ID,Time=Time,k=k,standardized=standardized,predictionstart = predictionstart,predictionsend = predictionsend,predictionsinterval = predictionsinterval,ResidualAnalysis = ResidualAnalysis ),silent = FALSE)
  if(try(summary(trytest)[2],silent=TRUE) =="try-error"){
    warning("Can not discern any significant lags for this situation")
  }else{
    
    DTVEM_out = trytest
    
    #DTVEM_out$OpenMxstage2out$OpenMxout = DTVEM_out$OpenMxstage2out$OpenMxout[-which(DTVEM_out$OpenMxstage2out$OpenMxout$sig==FALSE),]
    cat(paste("Fix",outcome[nnn],"to be the only one outcome variable and perform additional stage",".\n",sep=" "))
    # Add MI lmer estimation
    numberofalldata = nrow(all_data)
    All_sig_lags = DTVEM_out$OpenMxstage2out$OpenMxout$name
    splitmat1 = strsplit(All_sig_lags,"lagon")
    splitmat1 = matrix(unlist(splitmat1),byrow = T,ncol =2,nrow = length(All_sig_lags))
    splitmat1 = cbind(splitmat1[,1], matrix(unlist(strsplit(splitmat1[,2],"lag")),byrow = T,ncol =2,nrow = length(All_sig_lags)))
    splitdataframe = data.frame(splitmat1, stringsAsFactors = F)
    all_lag = splitdataframe[,3]
    max_lag = max(as.numeric(all_lag))
    number_exosig = length(which(splitdataframe[,1]%in%varnames_exo_eachtime))
    all_input = matrix(nrow = (numberofalldata-max_lag*N),ncol = number_exosig)
    all_output = matrix(nrow = (numberofalldata-max_lag*N),ncol = (nrow(splitdataframe)-number_exosig))
    colnames_input = c()
    colnames_output = c()
    j = 1
    k =1
    # Create data colmuns which are used in lmer
    for (i in 1:nrow(splitdataframe)) {
      if(splitdataframe[i,1]%in%varnames_exo_eachtime){
        lag_value = as.numeric(splitdataframe[i,3])
        add_data = c()
        col_order = which(splitdataframe[i,1]==colnames(all_data))
        for (n in 1:N) {
          suballdata = all_data[((n-1)*Timepoints+1):(n*Timepoints),]
          numberofsubdata = nrow(suballdata)
          add_data = c(add_data,suballdata[,col_order][(max_lag+1-lag_value):(numberofsubdata-lag_value)])
        }
        all_input[,j] = add_data
        colnames_input = c(colnames_input,All_sig_lags[i])
        j = j+1
      }else{
        lag_value = as.numeric(splitdataframe[i,3])
        add_data = c()
        col_order = which(splitdataframe[i,1]==colnames(all_data))
        for (n in 1:N) {
          suballdata = all_data[((n-1)*Timepoints+1):(n*Timepoints),]
          numberofsubdata = nrow(suballdata)
          add_data = c(add_data,suballdata[,col_order][(max_lag+1-lag_value):(numberofsubdata-lag_value)])
        }
        all_output[,k] = add_data
        colnames_output = c(colnames_output,All_sig_lags[i])
        k = k+1
      }
    }
    colnames(all_input) = colnames_input
    colnames(all_output) = colnames_output
    all_input = data.frame(all_input)
    all_output = data.frame(all_output)
    formula_fit_lemr = paste(c(colnames_input,colnames_output),collapse = "+")
    formula_fit_lemr = paste("outcomelagged~",formula_fit_lemr,-1,collapse = "")
    level2_formula = paste(c(colnames_input,colnames_output),collapse = "+")
    level2_formula = paste("(",1,"+",level2_formula,"||","ID_lagged",")")
    formula_fit_lemr = paste(formula_fit_lemr,"+",level2_formula)
    formula_fit_lemr = parse(text = paste("lmer","(",formula_fit_lemr,")"))
    outcome_col_order = which(outcome[nnn]==colnames(all_data))
    all_outcome_lag = c()
    ID_lagged = c()
    for (n in 1:N) {
      suballdata = all_data[((n-1)*Timepoints+1):(n*Timepoints),]
      numberofsubdata = nrow(suballdata)
      all_outcome_lag = c(all_outcome_lag,suballdata[,outcome_col_order][(max_lag+1):numberofsubdata])
      ID_lagged = c(ID_lagged,suballdata[,length(colnames(suballdata))][(max_lag+1):numberofsubdata])
    }
    
    regression_data = cbind(matrix(ID_lagged,nrow = nrow(all_input),ncol = 1),matrix(all_outcome_lag,nrow = nrow(all_input),ncol = 1),all_output,all_input) 
    colnames(regression_data)[1:2] = c("ID_lagged","outcomelagged")
    remove_id = c()
    # Remove ID with all missing values 
    for (n in 1:N){#Univariate
      for (nn in 1:(length(colnames_output)+1)) {
        if(all(is.na(regression_data[which(regression_data[,1]==n),(nn+1)]))){
          remove_id = c(remove_id,n)
        } 
      }
    }
    if(is.null(remove_id)){
      regression_data = regression_data #do not change
    }else{
      for (nn in 1:length(unique(remove_id))) {
        regression_data = regression_data[-which(regression_data[,1]==unique(remove_id)[nn]),]
      } 
    }
    # Then do multiple imputation on regression data/ Think all XlagonX terms as level-2 predictors
    pred_test = make.predictorMatrix(regression_data)
    pred_test[c("outcomelagged",colnames_output),"ID_lagged"] = -2
    imp = vector("character",length = length(colnames(regression_data)))
    names(imp) = colnames(regression_data)
    imp[c("outcomelagged",colnames_output)] = c("2l.pan",replicate(length(colnames_output),"2lonly.mean"))
    if(N == 1){
      trymice =  try(miceoutput <- mice(regression_data[,-1], m = sMItimes ,  
                                        method = "pmm"),silent = TRUE)
    }else{
      trymice =  try(miceoutput <- mice(regression_data, m = MItimes , predictorMatrix = pred_test , method = imp),silent = TRUE)
    }
    if(is.null(summary(trymice)[2])){
      imped_data = trymice
      #Then, use lmer or lm function to do estimation
      if(N == 1){
        fit_MIlm = with(data = imped_data, expr = formula_fit_lm )
        Results_fit = summary(pool(fit_MIlm), conf.int = TRUE,conf.level = 0.95)
      }else{
        fit_MIlmer = with(data = imped_data, expr = formula_fit_lemr )
        Results_fit = summary(pool(fit_MIlmer), conf.int = TRUE,conf.level = 0.95)
      }
      rownames_Results_fit = Results_fit[,1]
      Results_fit = Results_fit[,-1]
      rownames(Results_fit) = rownames_Results_fit
    }else{
      warning("Can not perform MIimputation")
    }
  }
   names(All_results)[nnn] = paste("Analysis of fixing",outcome[nnn],"using",model,"method",sep = " ")
   matrix_results_dtvem = matrix(unlist(DTVEM_out$OpenMxstage2out$OpenMxout),nrow = nrow(DTVEM_out$OpenMxstage2out$OpenMxout), ncol = 13)
   matrix_results_dtvem = matrix_results_dtvem[,c(-2,-3,-4,-7,-8,-9,-10,-11,-12,-13)]
   lowerbound = as.numeric(matrix_results_dtvem[,2]) - as.numeric(matrix_results_dtvem[,3])*1.96
   upperbound = as.numeric(matrix_results_dtvem[,2]) + as.numeric(matrix_results_dtvem[,3])*1.96
   matrix_results = data.frame(cbind(matrix_results_dtvem[,-1],lowerbound,upperbound))
   dtvem_results = data.frame(matrix(round(as.numeric(matrix(unlist(matrix_results),nrow = nrow(DTVEM_out$OpenMxstage2out$OpenMxout),ncol = 4)),digits = 4),nrow = nrow(DTVEM_out$OpenMxstage2out$OpenMxout),ncol = 4))
   colnames(dtvem_results) = c("estimate","std.error","2.5%","97.5%")
   rownames(dtvem_results) = matrix_results_dtvem[,1]

   matrix_results_lmm =  matrix(unlist(Results_fit),nrow = nrow(Results_fit),ncol = 7)
   matrix_results_lmm = data.frame(matrix_results_lmm[,c(-3,-4,-5)])
   lmm_results = data.frame(matrix(round(as.numeric(matrix(unlist(matrix_results_lmm),nrow = ,ncol = 4)),digits = 4),nrow = nrow(Results_fit),ncol = 4))
   rownames(lmm_results) = rownames_Results_fit
   colnames(lmm_results) = c("estimate","std.error","2.5%","97.5%")
   if(model == "LMM"){
     All_results[[nnn]] = lmm_results
   }else{
     All_results[[nnn]] = dtvem_results
   }
   }
 return(All_results)
}








