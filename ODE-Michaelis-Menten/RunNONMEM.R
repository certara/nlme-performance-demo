rm(list=ls())
home_dir <-  file.path(getwd())  
library(data.table)
library(stringr)
library(xml2)
library(ggplot2)
library(dplyr)
# run all models for time, reboot computer first
setwd(file.path(home_dir,"NONMEM"))
source(file.path(home_dir,"GetNPDENONMEM.R")) 
source(file.path(home_dir,"CleanUp.R")) 
 
CompNTHETA <- c(0,2) 
VWTNTHETA <- c(0,1)
GAMMANTHETA <- c(0,1)
ETANOMEGA <- c(1,2,3,4,3,6)  
Results <- data.frame(
    StartTime = as.character(),
    EndTime = as.character(),
    ModelNum = as.integer(),
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
 
# start at last model
Curr_model <- 0
Start_model <- dim(Results)[1]
nsamp <- 200 
this_gamma <- this_vwt <- this_eta <- this_comp <- 0
for(this_gamma in 0:1){  
  for(this_vwt in 0:1){ 
    for(this_comp in 0:1){
     for(this_eta in 0:5){ 
        Curr_model <- Curr_model + 1  
        StartTime <-  Sys.time()
        if(Curr_model > Start_model){
           tryCatch(
              {
                message("############ Starting model ", Curr_model, " at ", strptime(Sys.time(), format = "%Y-%m-%d %H:%M") ," ############")
                Est_time <- Cov_time <- iterations <-  messages <- -99999
                Covar <- success <- FALSE 
                wd <- file.path(home_dir,"NONMEM", Curr_model)
                setwd(wd)
                filenameStem <- paste0("Run",Curr_model)
                command <- paste0("nmfe75 ", paste0(filenameStem,".mod "), paste0(filenameStem,".lst"))
                StartTime <-  Sys.time()
                shell(command)
                xml_file <- file.path(wd,paste0(filenameStem,".xml"))
                data <- ""
                data <- read_xml(xml_file, encoding = "ASCII")    
                control_node <- data %>% 
                  xml_find_all("//nm:control_stream") %>% 
                  xml_text()  %>% 
                  str_split("\n") 
                data_file <- control_node[[1]][grep("^\\$DATA",control_node[[1]])] %>% 
                  str_replace("\\$DATA ","") %>% 
                  str_replace("IGNORE\\=@","") %>% 
                  str_trim()
                 
                iterations_node <- xml_find_all(data, "//nm:monitor")
                iterations_children <- xml_children(iterations_node)[[2]] 
                iterations <- as.integer(xml_attr(iterations_children,"iteration"))
                messages_node <- xml_find_all(data, "//nm:termination_txtmsg")
                messages_children <- xml_children(messages_node)
                message_contents <- xml_contents(messages_node)
                messages <- as.numeric(xml_text(message_contents))
                if(length(xml_contents(messages_node)) == 0){
                  finished <- FALSE
                }else{
                  finished <- TRUE
                }
                if(37 %in% messages){
                  success <- TRUE
                }else{
                  success <- FALSE
                }
                message_char <- paste(messages, collapse = "|")
                status <- data %>% 
                  xml_find_all("//nm:termination_status") %>% 
                  xml_text() %>% 
                  as.numeric()
                Est_time <- data %>% 
                  xml_find_all("//nm:estimation_elapsed_time") %>% 
                  xml_text() %>% 
                  as.numeric() 
                
                Cov_time <- data %>% 
                  xml_find_all("//nm:covariance_elapsed_time") %>% 
                  xml_text() %>% 
                  as.numeric() 
                 
                covariance_node <- xml_find_all(data, "//nm:covariance")
                if (length(covariance_node) == 0) {
                  Covar <- FALSE
                }else{
                  Covar <- TRUE
                } 
                message("############ Starting NPDE for model ", Curr_model, " at ", strptime(Sys.time(), format = "%Y-%m-%d %H:%M"), " ############")
                NPDEResults <- GetNPDE(wd, Curr_model, nsamp) 
              },
              error = function(e){ 
                Run_time <- 9999999
                converge <- Covar <- success <- finished <- FALSE
                status <- iterations <- -99999
                message_char <- "-99999"  
                NPDEResults <-list(
                  model = -999,
                  NPDECmaxMean = -999,
                  NPDECmaxSD = -999,
                  NPDECminMean = -999,
                  NPDECminSD = -999,
                  NPDETime = -999
                )
              }
            )  
            EndTime <- as.ITime(Sys.time()) 
            NTHETA <- 4 + CompNTHETA[this_comp+1] + VWTNTHETA[this_vwt+1] + GAMMANTHETA[this_gamma+1]
            NOMEGA <- ETANOMEGA[this_eta+1]
              
            This_Result = data.frame(
              StartTime = as.character(StartTime),
              EndTime = as.character(EndTime),
              Model_num = Curr_model,
              this_comp = this_comp,
              this_eta = this_eta,
              this_vwt = this_vwt,
              this_gamma = this_gamma,
              n_theta = NTHETA,
              n_omega = NOMEGA,
              Est_time = Est_time,
              Cov_time = Cov_time,
              Success = success,
              Covar = Covar,
              Iterations = iterations,
              Algorithm = "NONMEM",
              Good_inits = TRUE,
              log_path = xml_file,
              control_file = paste0(filenameStem, ".mod "), 
              data_set = data_file, 
              nparms = NTHETA + NOMEGA ,
              messages = paste(messages, collapse = "-"),
              crash = FALSE,
              NPDECmaxMean = NPDEResults$CmaxMean,
              NPDECmaxSD = NPDEResults$CmaxSD,
              NPDECminMean = NPDEResults$CminMean,
              NPDECminSD = NPDEResults$CminSD,
              SimRunTime = NPDEResults$NPDETime
            )
            Results <- rbind(Results, This_Result) 
            save(Results, file = file.path(home_dir,"NONMEMResults.RData"))
            write.csv(Results,file.path(home_dir,"NONMEMResults.csv"), quote= FALSE, row.names = FALSE)
            CleanUp(getwd())
        }else{
          message("Skipping model, already run at ", Results$EndTime[Curr_model])
        }
     }
    }
  }
} 
message("Done at ", strptime(Sys.time(), format = "%Y-%m-%d %H:%M"))
message("Returning to home directory")
setwd(home_dir)
