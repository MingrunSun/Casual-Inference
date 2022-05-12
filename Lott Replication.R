install.packages("tidyverse") 
library(tidyverse) 
install.packages("readstata13") 
library(readstata13) 

state <- data.frame(read.dta13("UpdatedStateLevelData-2010.dta")) 
write.csv(state, "LMReplication.csv",row.names=FALSE) 

state <- state[order(state$fipsstat,state$year),] 

state <- state[!state$yshall==1,] 


state <- state[state$year>=1977 & state$year<=2000,] 


state <- state %>% 
  
  group_by(fipsstat) %>%
  
  filter(!all(is.na(fipsstat))) 


state <- state %>% 
  
  distinct() 


state <- state %>% 
  
  group_by(fipsstat) %>% 
  
  filter(!all(is.na(ratmur))) 


write.csv(state, "C:\\Users\\ryan_shuda1\\Downloads\\state_cleaned.csv", row.names=FALSE) 



state <- data.frame(read.dta13("UpdatedStateLevelData-2010.dta")) 

state <- state[state$year>=1977 & state$year<=2000,] 




install.packages('fixest') 

library(fixest) 

mur <- feglm(lmur ~ shalll|fipsstat + year, data=state) 


rap <- feglm(lrap ~ shalll|fipsstat + year, data=state) 

aga <- feglm(laga ~ shalll|fipsstat + year, data=state) 

rob <- feglm(lrob ~ shalll|fipsstat + year, data=state) 

aut <- feglm(laut ~ shalll|fipsstat + year, data=state) 

bur <- feglm(lbur ~ shalll|fipsstat + year, data=state) 

lar <- feglm(llar ~ shalll|fipsstat + year, data=state) 


ATTmur <- round(100*mur[["coefficients"]][["shalll"]], 2) 

SEmur <- round(100*mur[["se"]][["shalll"]], 2) 



ATTrap <- round(100*rap[["coefficients"]][["shalll"]], 2) 

SErap <- round(100*rap[["se"]][["shalll"]], 2) 



ATTaga <- round(100*aga[["coefficients"]][["shalll"]], 2) 

SEaga <- round(100*aga[["se"]][["shalll"]], 2) 



ATTrob <- round(100*rob[["coefficients"]][["shalll"]], 2) 

SErob <- round(100*rob[["se"]][["shalll"]], 2) 



ATTaut <- round(100*aut[["coefficients"]][["shalll"]], 2) 

SEaut <- round(100*aut[["se"]][["shalll"]], 2) 



ATTbur <- round(100*bur[["coefficients"]][["shalll"]], 2) 

SEbur <- round(100*bur[["se"]][["shalll"]], 2) 



ATTlar <- round(100*lar[["coefficients"]][["shalll"]], 2) 

SElar <- round(100*lar[["se"]][["shalll"]], 2) 




part1 <- as.data.frame(cbind(ATTmur, ATTrap, ATTaga, ATTrob, ATTaut, ATTbur, ATTlar)) 

colnames(part1) <- c("Murder", "Rape", "Assault", "Robbery", "Auto", "Burglary", "Larceny") 

part2 <- as.data.frame(cbind(SEmur, SErap, SEaga, SErob, SEaut, SEbur, SElar)) 

colnames(part2) <- c("Murder", "Rape", "Assault", "Robbery", "Auto", "Burglary", "Larceny") 

statetable <- rbind(part1, part2) 

rownames(statetable) <- c("ATT", "Standard Error") 



statetable 



install.packages("readxl") 
library(readxl) 

lott <-read_excel("lott.xlsx") 




install.packages("bacondecomp") 

library(bacondecomp) 




f_bacon <- bacon(murder ~ shalll, 
                 
                 data = lott, 
                 
                 id_var = "state", 
                 
                 time_var = "year") 

#Callaway/Sant'Anna 

install.packages("did") 

library(did)

atts <- att_gt(yname = "murder", # LHS variable 
               
               tname = "year", # time variable 
               
               idname = "fipsstat", # id variable 
               
               gname = "effyear", # first treatment period variable 
               
               data = lott, # data 
               
               #xformla = NULL, # no covariates 
               
               xformla = ~ popstate, # with covariates 
               
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression 
               
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated"  
               
               bstrap = TRUE, # if TRUE compute bootstrapped SE 
               
               biters = 1000, # number of bootstrap iterations 
               
               print_details = FALSE, # if TRUE, print detailed results 
               
               clustervars = "state", # cluster level 
               
               panel = TRUE) # whether the data is panel or repeated cross-sectional 

agg_effects <- aggte(atts, type = "group") 


summary(agg_effects) 



#Sun and Abraham 
res_cohort = feols(murder ~ sunab(time_til, treated, att=TRUE) | fipsstat + year, lott) 
summary(res_cohort) 



