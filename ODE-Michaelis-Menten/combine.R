rm(list=ls())
library(dplyr)
library(ggplot2)
home_dir <- getwd()   
# nlme has one extra THETA, for residual error
NLME_Std_all <- read.csv("NLMEStdResults.csv") %>%  
  mutate(n_theta = n_theta - 1)
NONMEM_all <- read.csv("NONMEMResults.csv") 
# sometime, if .rdata file save an certain time, model gets repeated??, so, only use first
NONMEM_all <- NONMEM_all %>% 
  dplyr::distinct(Model_num,.keep_all = TRUE)
NLME_Std_times <- NLME_Std_all %>% 
  dplyr::select(Model_num,Est_time,Cov_time) %>% 
  dplyr::filter(Est_time > 0) %>% 
  dplyr::filter(Cov_time > 0)
 
NONMEM_times <- NONMEM_all %>%  
  dplyr::select(Model_num,Est_time,Cov_time) %>% 
  dplyr::filter(Est_time > 0) %>% 
  dplyr::filter(Cov_time > 0)
summary(NONMEM_times)
geoMeanNONMENEst  <- round(exp(mean(log(NONMEM_times$Est_time))),1)
geoMeanNONMENCov  <- round(exp(mean(log(NONMEM_times$Cov_time))),1)
geoMeanNLMEStdEst  <- round(exp(mean(log(NLME_Std_times$Est_time))),1)
geoMeanNLMEStdCov  <- round(exp(mean(log(NLME_Std_times$Cov_time))),1) 
summary(NLME_Std_times) 

# set with both NLME and NONMEM
times <- inner_join(x=NONMEM_times, y=NLME_Std_times,by="Model_num")
colnames(times) <- c("Model_Num","NONMEM_Est","NONMEM_Cov","NLME_Std_Est","NLME_Std_Cov")
Sum_times <- c("GMean_NONMEM_EST" = round(exp(mean(log(times$NONMEM_Est))),0),
               "GMean_NLME_Std_EST" = round(exp(mean(log(times$NLME_Std_Est))),0), 
               "GMean_NONMEM_Cov" = round(exp(mean(log(times$NONMEM_Cov))),1),
               "GMean_NLME_Std_Cov" = round(exp(mean(log(times$NLME_Std_Cov))),1)
               )
 
Sum_times 
Sum_success <- c("FSuccess_NONMEM" = mean(NONMEM_all$Success),
                 "FSuccess_NLME_std" = mean(NLME_Std_all$Success), 
                 "FCov_NONMEM" = mean(NONMEM_all$Covar),
                 "FCov_NLME_std" = mean(NLME_Std_all$Covar) )
Sum_success
NONMEM_all <- NONMEM_all %>% 
  mutate(absNPDECmaxMean = if_else(NPDECmaxMean > -990, abs(NPDECmaxMean), NA))%>% 
  mutate(absNPDECminMean = if_else(NPDECminMean > -990, abs(NPDECminMean), NA)) 
NLME_Std_all <- NLME_Std_all %>% 
  mutate(absNPDECmaxMean = if_else(NPDECmaxMean > -990, abs(NPDECmaxMean), NA))%>% 
  mutate(absNPDECminMean = if_else(NPDECminMean > -990, abs(NPDECminMean), NA)) 
 
summary(NONMEM_all)
summary(NLME_Std_all) 
SumNPDE <- c("Cmax_NONMEM" = mean(NONMEM_all$absNPDECmaxMean,na.rm = TRUE),
             "CmaxNLME_std" = mean(NLME_Std_all$absNPDECmaxMean,na.rm = TRUE), 
             "Cmin_NONMEM" = mean(NONMEM_all$absNPDECminMean,na.rm = TRUE),
             "Cmin_NLME_std" = mean(NLME_Std_all$absNPDECminMean,na.rm = TRUE) )

SumNPDE
 
AllResults <- rbind(NLME_Std_all,NONMEM_all)
ggplot2::ggplot(AllResults,
                ggplot2::aes(x=n_theta,y=Est_time,color = Algorithm)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method='lm') +
  ggplot2::scale_y_log10() +
  ggplot2::xlab("Number of estimated THETA parameters")+
  ggplot2::ylab("Estimation time (seconds)") 
ggplot2::ggsave("Estimation Run time vs NTheta by algorithm.jpeg",device="jpeg",height = 7, width=10)
 
ggplot2::ggplot(AllResults,
                ggplot2::aes(x=n_theta,y=NPDECmaxMean, color = Algorithm)) +
  ggplot2::geom_point() +
  ggplot2::ggplot2::geom_smooth(method='lm') +
  scale_y_log10() +
  ggplot2::xlab("Number of estimated parameters (NTHETA+NOMEGA)")+
  ggplot2::ylab("Cmax NPDE Mean") +
  ggplot2::ggtitle("Cmax NPDE mean, theoretical value = 0")
ggplot2::ggsave("Cmax NPDE Mean vs NParms by algorithm.jpeg",device="jpeg",height = 7, width=10)

ggplot2::ggplot(AllResults,
                ggplot2::aes(x=n_theta,y=NPDECminMean,color = Algorithm)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method='lm') +
  vscale_y_log10() +
  ggplot2::xlab("Number of estimated parameters (NTHETA+NOMEGA)")+
  ggplot2::ylab("Cmin NPDE Mean") +
  ggplot2::ggtitle("Cmin NPDE mean, theoretical value = 0")
ggplot2::ggsave("Cmin NPDE Mean vs NParms by algorithm.jpeg",device="jpeg",height = 7, width=10)

ggplot2::ggplot(AllResults,
                ggplot2::aes(x=n_theta,y=Iterations,color = Algorithm)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method='lm') +
  ggplot2::scale_y_log10() +
  ggplot2::xlab("Number of estimated parameters (NTHETA+NOMEGA)")+
  ggplot2:: ylab("Number of iterations") 
ggplot2::ggsave("Number of iterations vs NParms by algorithm.jpeg",device="jpeg",height = 7, width=10)
 
#stats
t.test(formula = Est_time ~ Algorithm,  # Formula
       data = AllResults) # Dataframe containing the variables
t.test(formula = Cov_time ~ Algorithm,  # Formula
       data = AllResults) # Dataframe containing the variables

table_NONMEM <- table(AllResults$Success,
                      AllResults$Algorithm)
table_NONMEM
chisq.test(table_NONMEM)
table_NONMEM <- table(AllResults$Covar,
                      AllResults$Algorithm)
table_NONMEM
chisq.test(table_NONMEM)
setwd(home_dir)   
 
 