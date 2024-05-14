## always sort sim dat by rep,id, time
rm(list=ls())   
# library(readr)
# for(i in 1:48){
#   control <- readr::read_file(file.path("c:/git/dual_numbers/pydarwin_nlme/run/0",sprintf("%02d", i),paste0("NLME_0_",sprintf("%02d", i),".mmdl")))
#   control[1] <- str_replace( control[1],"##DATA c:/git/dual_numbers/","##DATA c:/git/ADrepo/")
#   write(control,file.path(home_dir,"NLMEfd",i,paste0("NLME_0_",i,".mmdl")))
# 
#   control <- readr::read_file(file.path("c:/git/dual_numbers/pydarwin_nonmem/run/0",sprintf("%02d", i),paste0("NM_0_",sprintf("%02d", i),".mod")))
#   control <- str_replace(control,"c:/git/dual_numbers/","c:/git/ADrepo/")
#   write(control,file.path(home_dir,"NONMEM",i,paste0("run",i,".mod")))
# }


home_dir <- getwd()  
nreps <- 200
library(stringr)
library(dplyr)
library(Certara.RsNLME)
library(data.table)  
library(tidyvpc) 
library(lubridate)
ETANOMEGA <- c(1,2,3,4,3,6) # no simple way to get n_omega from fit object
Results <- data.frame(
  StartTime = as.character(),
  EndTime = as.character(),
  Model_num = as.integer(),
  this_comp = as.integer(),
  this_eta = as.integer(),
  this_vwt = as.integer(),
  this_gamma = as.integer(),
  n_theta = as.integer(),
  n_omega = as.integer(),
  Est_time = as.numeric(),
  Cov_time = as.numeric(),
  Success = as.logical(),
  Covar = as.logical(),
  Iterations = as.integer(),
  Algorithm = as.character(),
  Good_inits = as.logical(),
  log_path = as.character(),
  control_file = as.character(),
  data_set = as.character(),
  nparms = as.integer(),
  messages = as.integer(),
  crash= as.logical(),
  NPDECmaxMean = as.numeric(),
  NPDECmaxSD = as.numeric(),
  NPDECminMean = as.numeric(),
  NPDECminSD = as.numeric(),
  SimRunTime = as.numeric()
)
   

setwd(file.path(home_dir,"NLMEFD"))
mmdl_files <- list.files(home_dir, 
                         pattern = "^NLME_.*\\.mmdl$", 
                         full.names = TRUE, 
                         recursive = TRUE)
# Cmax from 2nd and 3rd time points
CmaxRows <- rep(c(FALSE, TRUE, TRUE,FALSE,FALSE,FALSE,FALSE), 60*nreps)
# Cmin from final time point
CminRows <- rep(c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE, TRUE), 60*nreps)
 
this_gamma <- this_vwt <- this_eta <- this_comp <- 0
Curr_model <- 0
for(this_gamma in 0:1){  
  for(this_vwt in 0:1){
    for(this_comp in 0:1){
      for(this_eta in 0:5){
        Curr_model <- Curr_model + 1
        model_meta <- create_model_from_metamodel(mmdl_files[Curr_model]) 
        model_orig <- model_meta$model 
        message("############ Running ",model_orig@modelInfo@workingDir, " at ",  strptime(Sys.time(), format = "%Y-%m-%d %H:%M")," ############")
        modelNum <- str_sub(rev(setdiff(strsplit(model_orig@modelInfo@workingDir,"/|\\\\")[[1]], ""))[1],
                            start= -2)
        
        StartTime <- Sys.time()
        fit <- fitmodel(model_orig,  
                        numIterations = 1000, 
                        numCores = 1,
                        ODE = "DVERK",  
                        sort = FALSE,
                        method = "FOCE-ELS", 
                        workingDir = model_orig@modelInfo@workingDir,
                        stdErr = "Auto-Detect",
                        maxStepsODE = 100000,
                        runInBackground = TRUE)
        if(file.exists(file.path(model_orig@modelInfo@workingDir,"err2.txt"))){
          EndTime <- Sys.time()
          file_conn <- file(model_orig@modelInfo@workingDirm,"err2.txt")
          messages <- readLines(file_conn)
          close(file_conn) 
          CurResults <- data.frame(
            StartTime = strptime(StartTime, format = "%Y-%m-%d %H:%M"),
            EndTime = EndTime,
            Model_num = Curr_model,
            this_comp = this_comp,
            this_eta = this_eta,
            this_vwt = this_vwt,
            this_gamma = this_gamma,
            n_theta = -999,
            n_omega = -999,
            Est_time = -999,
            Cov_time = -999,
            Success = FALSE,
            Covar = FALSE,
            Iterations = -999,
            Algorithm = "NLME_standard",
            Good_inits = TRUE,
            log_path = log_path,
            control_file = control_file,
            data_set = data_file,
            nparms = -999,
            messages = messages,
            crash = TRUE,
            NPDECmaxMean = -999,
            NPDECmaxSD = -999,
            NPDECminMean = -999,
            NPDECminSD = -999,
            SimRunTime = -999
          )
          }else{ 
             if(fit$Overall$RetCode < 4){
               Success <-  TRUE
             }else{
               Success <- FALSE
             }
            n_theta <- dim(fit$theta)[1] - 1
            n_omega <- ETANOMEGA[this_eta + 1] # dim(fit$omega)[1]
            logfile <- fit$nlme7engine.log
            Est_time <- logfile[grep(pattern = "engine runtime", x = logfile)]  
            Est_time <-  as.numeric(str_trim(str_replace(Est_time," engine runtime \\(secs\\) =","")))
            Cov_time <- logfile[grep(pattern = "stderr runtime", x = logfile)]  
            Cov_time <-  as.numeric(str_trim(str_replace(Cov_time, " stderr runtime \\(secs\\) =","")))
            Overall <- colnames(read.csv(file.path(model_orig@modelInfo@workingDir, "overall.csv")))
            Covar <- "Condition" %in% Overall
            Iterations <- max(fit$ConvergenceData$Iter) 
            control_file <- file.path(mmdl_files[Curr_model])
            file_conn <- file(control_file)
            control <- readLines(file_conn)
            close(file_conn)
            data_file <- str_replace(control[1],"##DATA ","")  
            log_path <- file.path(model_orig@modelInfo@workingDir, "nlme7engine.log")
            model2 <- copyModel(model_orig, 
                                modelName = "VPCModel",
                                workingDir = file.path(model_orig@modelInfo@workingDir,"VPC"),
                                acceptAllEffects = TRUE)
                                 
            message("############ Running NPDE for ", model_orig@modelInfo@workingDir," at ",strptime(Sys.time(), format = "%Y-%m-%d %H:%M") , " ############")
            finalModelVPC  <- vpcmodel(model = model2, 
                                       numReplicates = nreps, 
                                       numCores = 1,
                                       outputPRED = TRUE)
            # sim runtime in  S_001.status
            EndTime <- Sys.time()
            file_conn <- file(file.path(model_orig@modelInfo@workingDir, "VPC", "S_001.status"))
            status <- readLines(file_conn)
            close(file_conn)
            status <- str_split(status,"     ")
            SimRunTime <- as.numeric(as.POSIXct(status[[3]]) - as.POSIXct(status[[2]]))
            dt_ObsData <- finalModelVPC$predcheck0
            # Simulation output dataset predout.csv
            dt_SimData <- finalModelVPC$predout %>% 
               arrange(REPLICATE,ID5,IVAR) 
            
            NPDECmaxMean <- -999
            NPDECmaxSD <- -999 
            NPDECminMean <- -999
            NPDECminSD <- -999
            tryCatch({
             npde <- observed(dt_ObsData, x=NULL, y=DV) %>%
               simulated(dt_SimData, y=DV) %>%
               npde(id = ID5) 
               CmaxNPDEs <- npde$npdesim$npde[CmaxRows]
               NPDECmaxMean <- mean(CmaxNPDEs)
               NPDECmaxSD <- sd(CmaxNPDEs)
               CminNPDEs <- npde$npdesim$npde[CminRows]
               NPDECminMean <- mean(CminNPDEs)
               NPDECminSD <- sd(CminNPDEs) 
               message("NPDE Cmax = ", paste(round(NPDECmaxMean,4),
                                             " (",round(NPDECmaxSD,4),
                                             "), Cmin = ", round(NPDECminMean,4),
                                             " (",round(NPDECminSD,4),")"))
           },
           error = function(e){ 
             message("Error in NPDE, model ", Curr_model, " ", e) 
           }
           )
           CurResults <- data.frame(
             StartTime = strptime(StartTime,format = "%Y-%m-%d %H:%M"),  
             EndTime = strptime(EndTime,format = "%Y-%m-%d %H:%M"),
             Model_num = Curr_model,
             this_comp = this_comp,
             this_eta = this_eta,
             this_vwt = this_vwt,
             this_gamma = this_gamma,
             n_theta = n_theta,
             n_omega = n_omega,
             Est_time = Est_time,
             Cov_time = Cov_time,
             Success = Success,
             Covar = Covar,
             Iterations = Iterations,
             Algorithm = "NLME_standard",
             Good_inits = TRUE,
             log_path = log_path,
             control_file = control_file,
             data_set = data_file,
             nparms = n_theta+n_omega,
             messages = fit$Overall$RetCode,
             crash = FALSE,
             NPDECmaxMean = NPDECmaxMean,
             NPDECmaxSD = NPDECmaxSD,
             NPDECminMean = NPDECminMean,
             NPDECminSD = NPDECminSD,
             SimRunTime = SimRunTime
           ) 
          }
        Results <- rbind(Results, CurResults)
      }
    }
  }
}
write.csv(Results,file.path(home_dir,"NLMEStdResults.csv"),quote= FALSE, row.names = FALSE)
message("Done at ", strptime(Sys.time(), format = "%Y-%m-%d %H:%M"))
