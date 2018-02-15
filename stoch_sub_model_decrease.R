setwd("M:/Documents/Case 2/Data")

rm(list=ls())
dev.off()

##Packages
library(dtplyr)
library(dplyr)
library(tidyr)
library(data.table) #necessary for as.data.table
library(psych) #necessary for describe function
library(ggplot2) #ggplot2
library(reshape2) #for ggplot1
library(plyr) #for ggplot1


##Settings
wloss_a <- 0.8
wloss_b <- 0.75
wloss_c <- 0.75


##Read data
consumption <- read.csv2("LT_Intake_7days.csv")
IP_age_sex_adults <- read.csv2("IP_age_sex_adults.csv")

food_a <- read.csv2("LT_fish_groups.csv")

food_a1 <- read.csv2("LT_fish_cold.csv")
food_a2 <- read.csv2("LT_fish_hot.csv")

food_b <- read.csv2("LT_meat.csv")
food_b1 <- read.csv2("LT_meat_proc.csv")
food_b2 <- read.csv2("LT_meat_red.csv")

food_c <- read.csv2("LT_poultry.csv")

food_d <- read.csv2("LT_stablefood.csv")


##Consumption data
consumption <- merge(IP_age_sex_adults, consumption, by = "PROJEKTID")
names(consumption)[1] <- "ID"
consumption$sex <- ifelse(consumption$sex == "Kvinde", 2, 1)

#Change NAs to zeros
consumption[is.na(consumption)] <- 0


##List of foods in food groups
food_a <- paste0("X", food_a$LT) #Make same notation as in consumption data

food_a1 <- paste0("X", food_a1$LT) #Make same notation as in consumption data
food_a2 <- paste0("X", food_a2$LT) #Make same notation as in consumption data

food_b$LT <- paste0("X", food_b$LT) #Make same notation as in consumption data

food_b1 <- paste0("X", food_b1$LT)
food_b2 <- paste0("X", food_b2$LT)

food_c <- paste0("X", food_c$LT)

food_d <- paste0("X", food_d$LT)


##Convert to cooked amounts
consumption[c(food_a2)] <- consumption[c(food_a2)] * wloss_a #fish consumed for hot meals converted from raw to cooked weights

consumption[c(food_b2)] <- consumption[c(food_b2)] * wloss_b #meat consumed for hot meals converted from raw to cooked weights





##sum foods to total food groups 
consumption <- tbl_df(consumption)

consumption <-  consumption %>%
  mutate(total.fish = X19 + X72 + X73 + X82 + X83 + X118 + X129 + X135 + X137 + X175 + X177 + X178 + X187 + X188 + X218 + X219 +X236 + X244 + X245 + X246 + 
           X268 + X312 + X314 + X317 + X318 + X319 + X320 + X353 + X354 + X882 + X886 + X908) #Sum all foods in food group a - here fish types

consumption <-  consumption %>%
  mutate(fish_lunch = X73 +  X118 + X129 + X137 + X177 + X178 + X188 + X218 + X219 + X244 + X245 + X246 + X314 + X317 + X318 + X319 + X320 + X354)

consumption <-  consumption %>%
  mutate(fish_dinner = X19 + X72 +  X82 + X83 + X135 + X175 + X187 + X236 + X268 + X312 + X353 + X882 + X886 + X908)

consumption <-  consumption %>%
  mutate(total.meat = X13 + X16 + X69 + X80 + X112 + X113 + X138 + X139 + X144 + X146 + X189 + X191 + X199 + X201 + X248 + X250 + X266 + X274 + X288 + 
           X292 + X293 + X294 + X295 + X296 + X297 + X298 + X374 + X376 + X378 + X379 + X380 + X381 + X382 + X419 + X420 + X424 + X429 + X431 + X436 + 
           X438 + X548 + X549 + X551 + X552 + X562 + X925 + X927 + X941 + X1247 + X1448 + X1453 + X1454) #Sum all meat types

consumption <-  consumption %>%
  mutate(proc_meat = X16 + X248 + X250 + X266 + X274 +  X292 +  X293 +  X294 +  X295 +  X296 +  X297 +  X298  + X548 +  X549 +  X551 +  X552  + X562  +
           X1448 + X1453 + X1454)

consumption <-  consumption %>%
  mutate(red_meat = X13 + X80 + X112 + X113 + X138 + X139 + X144 + X146 + X189 + X191 + X199 + X201 + X288 + X374 + X376 + X378 + X379 + X380 + X381 +
           X382 + X419 + X420 + X424 + X429 + X431 + X436 + X438 + X925 + X927 + X941 + X1247)

consumption <-  consumption %>%
  mutate(total.poultry = X6 + X7 + X47 + X48 + X66 + X67 + X97 + X110 + X131 + X132 + X1038 + X1039 + X1449) #Sum all types of poultry

consumption <- consumption %>%
  mutate(total.stablefood = X174 + X224 + X533 + X537 + X988 + X1016 + X1131 + X1132 + X1133 + X1311 + X1456 + X1464 + X1465 + X1469 + X1470 + X115)


##Food consumption frequencies
consumption$fish.day <- as.numeric(ifelse(consumption$total.fish > 0, c(1), c(0))) 
consumption$meat.day <- as.numeric(ifelse(consumption$total.meat > 0, c(1), c(0)))

##Total weekly consumption amounts and frequencies
consumption_weekly <- aggregate(consumption, by = list(consumption$ID, consumption$sex, consumption$age), FUN = sum)
consumption_weekly <- consumption_weekly[,c(1:3, 444:ncol(consumption_weekly))]
names(consumption_weekly)[1] <- "ID"
names(consumption_weekly)[2] <- "sex"
names(consumption_weekly)[3] <- "age"

##Here
#Food A: fish, food a1: cold fish, food a2: hot fish
#food B: Red + proc. meat, food b1: processed meat, food b2: red meat
#Food C: poultry
#Food D: staple foods

consumption_sub <- merge(consumption[,c(1:4, 441:ncol(consumption))], consumption_weekly[, c(1:4,7:13)], by = c("ID", "sex", "age"))
names(consumption_sub)[5:ncol(consumption_sub)] <- c('food_a', 'food_a1', 'food_a2', 'food_b', 'food_b1', 'food_b2', 'food_c', 'food_d','intake_a', 'intake_b', 'total_a', 'total_b', 'total_b1', 'total_b2', 'total_c', 'total_d', 'totaldays_a', 'totaldays_b')



##Settings for substitution

#consumption data for food groups a, b and c
food_abcd <- subset(consumption, select = c(ID, sex, age, lbnr, X174, X224, X533, X537, X988, X1016, X1131, X1132, X1133, X1311, X1456, X1464, X1465, X1469, X1470, X115, #staple foods 
                                            X6, X7, X47, X48, X66, X67, X97, X110, X131, X132, X1038, X1039, X1449, #poultry
                                            X13, X16,  X69,  X80,  X112, X113, X138, X139, X144, X146, X189, X191, X199, X201, X248, X250, X266, X274, X288, #meat
                                            X292, X293, X294, X295, X296, X297, X298, X374, X376, X378, X379, X380, X381, X382, X419, X420, X424, X429, X431,
                                            X436, X438, X548, X549, X551, X552, X562, X925, X927, X941, X1247,X1448,X1453,X1454,
                                            X19, X72, X73, X82, X83, X118, X129, X135, X137, X175, X177, X178, X187, X188, X218, X219,X236, X244, X245, X246,#fish
                                            X268, X312, X314, X317, X318, X319, X320, X353, X354, X882, X886, X908))


#Probability of consumption of different fish types/species
food_amount_a <- subset(consumption, select = c(X19, X72, X73, X82, X83, X118, X129, X135, X137, X175, X177, X178, X187, X188, X218, X219,X236, X244, X245, X246, 
                                                X268, X312, X314, X317, X318, X319, X320, X353, X354, X882, X886, X908)) #LT no. + intakes for food_a to sample food a type and amounts


prob_food_a <- colSums(food_amount_a[,c(1:ncol(food_amount_a))]) #probability of foods in food group A
prob_food_a <- prob_food_a/sum(prob_food_a)


food_amount_a1 <- subset(consumption, select = c(X73, X118, X129, X137, X177, X178, X188, X218, X219, X244, X245, X246, X314, X317, X318, X319, X320, X354))


prob_food_a1 <- colSums(food_amount_a1[,c(1:ncol(food_amount_a1))]) #probability of foods in food group a1
prob_food_a1 <- prob_food_a1/sum(prob_food_a1)



food_amount_a2 <- subset(consumption, select = c(X19, X72, X82, X83, X135, X175, X187, X236, X268, X312, X353, X882, X886, X908)) #LT no. + intakes for food_a to sample food a type and amounts


prob_food_a2 <- colSums(food_amount_a2[,c(1:ncol(food_amount_a2))]) #probability of foods in food group A
prob_food_a2 <- prob_food_a2/sum(prob_food_a2)


#Substitution factors
n_days <- 7 #total days of the survey
SF_b1a <- 3 #substitution factor for meat lunch --> fish
SF_b2a <- 100/93.75 #substitution factor for meat dinner --> fish
SF_b1b2 <- 9.375 #substitution factor for processed meat -> red meat
SF_b1c <- 9.375 #substitution factor for processed meat -> poultry (assume same as for red meat here)
SF_b2b1 <- 1/SF_b1b2 #substitution factor for red meat -> processed meat


consumption_sub <- cbind(food_abcd, consumption_sub) #combine new intakes with intakes of individual foods b and c


#Maximum intake amounts
stoch_amount_b1 <- rnorm(1e+04, mean = log(10), sd = log(1.25))

summary(exp(stoch_amount_b1))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.214   8.611  10.013  10.280  11.671  22.451

stoch_amount_b2 <- rnorm(1e+04, mean = log(300), sd = log(1.25)) #if median = 500g the distribution is skewet to the right compared to current intake

summary(exp(stoch_amount_b2))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 136.6   258.2   300.4   307.4   348.5   710.5

max_amount_b1 <- 10 #weekly?
sd_amount_b1 <- 1.25

max_amount_b2 <- 500
sd_amount_b2 <- 1.25


##Substitution model                       
set.seed(123)

cons_sub_new <- numeric(0)

Individual <- unique(consumption_sub$ID)

for(i in 1:length(Individual)){

  individual_cons <- subset(consumption_sub, ID == Individual[i])
  
  #All processed meat decreased to zero
  # individual_cons[,c(food_b1)] <- 0 
  
  #Decrease processed meat intake to below specified amount
  b1_draw <- exp(rnorm(1, mean = log(max_amount_b1), sd = log(sd_amount_b1)))
  
  repeat{
    
    b1 <- rowSums(individual_cons[,c(food_b1)])
    
    if(sum(b1) > b1_draw){
      
      l <- sample(1:7, 1, replace = T)
      
      type_b1 <- sample(food_b1, 1, replace = T)
      
      for(k in 1:ncol(individual_cons)){
        if(names(individual_cons[k]) == type_b1){ #Chose the column in the consumption data that fits the drawn type
          
          individual_cons[l,k] <- 0
        }
      }
    }
    
    if(sum(rowSums(individual_cons[,c(food_b1)])) < b1_draw){
      break
    }
  }
  
  
  #Decrease red meat intake to below specified amount
  b2_draw <- exp(rnorm(1, mean = log(max_amount_b2), sd = log(sd_amount_b2)))
  
  repeat{
  
  b2 <- rowSums(individual_cons[,c(food_b2)])
  # b2_new <- b2
  
    if(sum(b2) > b2_draw){
      
      l <- sample(1:7, 1, replace = T)
      
      type_b2 <- sample(food_b2, 1, replace = T)
      
      for(k in 1:ncol(individual_cons)){
        if(names(individual_cons[k]) == type_b2){ #Chose the column in the consumption data that fits the drawn type
          
          individual_cons[l,k] <- 0
        }
      }
    }
  
  if(sum(rowSums(individual_cons[,c(food_b2)])) < b2_draw){
    break
  }
  }
  
  cons_sub_new <- rbind(cons_sub_new, individual_cons)
  
}


cons_sub_new$food_b1_new <- rowSums(cons_sub_new[,c(food_b1)])
cons_sub_new$food_b1_diff <- cons_sub_new$food_b1 - cons_sub_new$food_b1_new

total_b1_new <- aggregate(cons_sub_new$food_b1_new, by = list(cons_sub_new$ID), FUN = sum)
names(total_b1_new)[1] <- "ID"
names(total_b1_new)[2] <- "total_b1_new"

cons_sub_new <- merge(cons_sub_new, total_b1_new, by = "ID")

cons_sub_new$food_b2_new <- rowSums(cons_sub_new[,c(food_b2)])
cons_sub_new$food_b2_diff <- cons_sub_new$food_b2 - cons_sub_new$food_b2_new

cons_sub_new$food_b2_diff <- ifelse(cons_sub_new$food_b2_diff < 1e-12, 0, cons_sub_new$food_b2_diff)

total_b2_new <- aggregate(cons_sub_new$food_b2_new, by = list(cons_sub_new$ID), FUN = sum)
names(total_b2_new)[1] <- "ID"
names(total_b2_new)[2] <- "total_b2_new"

cons_sub_new <- merge(cons_sub_new, total_b2_new, by = "ID")

# write.csv(cons_sub_new, file = "cons_sub_new.csv", row.names = F)
cons_sub_new <- read.csv("cons_sub_new.csv")


#processed meat before and after
quantile(cons_sub_new$food_b1, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%    10%    50%    90%  97.5% 
# 0.00   0.00  15.00 102.10 197.88 

mean(cons_sub_new$food_b1)
# [1] 37.09627

quantile(cons_sub_new$food_b1_new, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%   10%   50%   90% 97.5% 
# 0.00  0.00  0.00  1.80  6.16

mean(cons_sub_new$food_b1_new)
# [1] 0.5496425


#daily red meat before and after
quantile(cons_sub_new$food_b2, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%       10%       50%       90%     97.5% 
# 0.00000   0.00000  47.61905 180.52983 310.65871 

mean(cons_sub_new$food_b2)
# [1] 72.46428

quantile(cons_sub_new$food_b2_new, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%      10%      50%      90%    97.5% 
# 0.0000   0.0000  10.5000 101.2536 159.9147

# 2.5%       10%       50%       90%     97.5% #Probabilistic median 500g
# 0.00000   0.00000  27.82609 137.77268 213.21962 

# 2.5%       10%       50%       90%     97.5% #Deterministic 500 g limit
# 0.00000   0.00000  29.11765 137.77268 210.15427 

mean(cons_sub_new$food_b2_new)
# [1] 33.27892

# [1] 50.27726 #probabilistic median 500g

# [1] 50.6668 #deterministic 500g




#weekly processed meat before and after
quantile(cons_sub_new$total_b1, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%      10%      50%      90%    97.5% 
# 0.0000  30.0000 187.7149 567.4600 926.9521

mean(cons_sub_new$total_b1)
# [1] 259.6739

quantile(cons_sub_new$total_b1_new, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%   10%   50%   90% 97.5% 
# 0.00  0.00  2.94  9.48 11.98 

mean(cons_sub_new$total_b1_new)
# [1] 3.847497


#weekly red meat before and after
quantile(cons_sub_new$total_b2, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%        10%        50%        90%      97.5% 
# 57.31383  161.31713  451.08896  911.65463 1317.03390  

mean(cons_sub_new$total_b2)
# [1] 507.2499

quantile(cons_sub_new$total_b2_new, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%       10%       50%       90%     97.5% 
# 34.18654 113.60094 237.28285 339.39153 403.64356 

# 2.5%       10%       50%       90%     97.5% #probabilistic median 500g
# 52.93078 151.48571 361.98988 529.07437 623.75644

# 2.5%       10%       50%       90%     97.5% #Deterministic 500g
# 54.26471 157.71727 394.35288 488.76742 497.62153

mean(cons_sub_new$total_b2_new)
# [1] 232.9525

# [1] 351.9408 #probabilistic median 500g

# [1] 354.6676 #Deterministic 500g




### Substitution with food groups b and c ###

cons_sub_new$food_a2_diff <- cons_sub_new$food_b2_diff * SF_b2a

cons_sub_new$food_a1_diff <- cons_sub_new$food_b1_diff * SF_b1a


quantile(cons_sub_new$food_a2_diff, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%       10%       50%       90%     97.5% 
# 0.00000   0.00000   0.00000  80.17058 232.16775 

quantile(cons_sub_new$food_a1_diff, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%      10%      50%      90%    97.5% 
# 0.0000   0.0000  44.1454 304.1330 590.2164 


for(j in 1:nrow(cons_sub_new)){
    
    if(cons_sub_new$food_a1_diff[j] > 0){
      
      diff_a1 <- cons_sub_new$food_a1_diff[j]
      
      repeat{
        
        type_a1 <- sample(food_a1, 1, prob = prob_food_a1, replace = T) #draw a food from group a1
          
        
        for(k in 1:ncol(food_amount_a1)){
          if(names(food_amount_a1[,k]) == type_a1){ #Chose the column in the consumption data that fits the drawn food type
            amount_a1 <- food_amount_a1[,k]
            amount_a1 <- subset(amount_a1, amount_a1[1:length(amount_a1)] > 0) #chose only positive intakes
            amount_a1 <- amount_a1 %>% unlist(use.names = FALSE) #make list instead of dataframe
          }
        }
        
        for(l in 1:ncol(cons_sub_new)){
          if(names(cons_sub_new[l]) == type_a1){ #Chose the column in the consumption data that fits the drawn type
            
            a1 <- sample(amount_a1, 1, replace = T) #draw amount of food a1
            
            if(a1 <= diff_a1){ #if amount is smaller than or equal to required increase in a1
              
              cons_sub_new[j,l] <- cons_sub_new[j,l] + a1 #add food a1 amount to current consumption of food a1
              
              }
            
            if(a1 > diff_a1){ #if amount is larger than required increase in a1
              
              cons_sub_new[j,l] <- cons_sub_new[j,l] + diff_a1 #just add required amount of a1
              
              diff_a1 <- 0
              
            }
            
            if(diff_a1 > 0){
              
              diff_a1 <- diff_a1 - a1
              
            }
          }
        }
        
        if(diff_a1 == 0){
          
          break
          
        }
        
      }
    }
  
  if(cons_sub_new$food_a2_diff[j] > 0){
    
    diff_a2 <- cons_sub_new$food_a2_diff[j]
    
    repeat{
      
      type_a2 <- sample(food_a2, 1, prob = prob_food_a2, replace = T) #draw a food from group a2
      
      
      for(k in 1:ncol(food_amount_a2)){
        if(names(food_amount_a2[,k]) == type_a2){ #Chose the column in the consumption data that fits the drawn food type
          amount_a2 <- food_amount_a2[,k]
          amount_a2 <- subset(amount_a2, amount_a2[1:length(amount_a2)] > 0) #chose only positive intakes
          amount_a2 <- amount_a2 %>% unlist(use.names = FALSE) #make list instead of dataframe
        }
      }
      
      for(l in 1:ncol(cons_sub_new)){
        if(names(cons_sub_new[l]) == type_a2){ #Chose the column in the consumption data that fits the drawn type
          
          a2 <- sample(amount_a2, 1, replace = T) #draw amount of food a2
          
          if(a2 <= diff_a2){ #if amount is smaller than or equal to required increase in a2
            
            cons_sub_new[j,l] <- cons_sub_new[j,l] + a2 #add food a2 amount to current consumption of food a2
            
          }
          
          if(a2 > diff_a2){ #if amount is larger than required increase in a2
            
            cons_sub_new[j,l] <- cons_sub_new[j,l] + diff_a2 #just add required amount of a2
            
            diff_a2 <- 0
            
          }
          
          if(diff_a2 > 0){
            
            diff_a2 <- diff_a2 - a2
            
          }
        }
      }
      
      if(diff_a2 == 0){
        
        break
        
      }
      
    }
  }
  
}


cons_sub_new[c(21610:21616), food_a2]

cons_sub_new[c(21610:21616), "food_a2_diff"]

cons_sub_new[c(21610:21616), "food_a2"]

rowSums(cons_sub_new[c(21610:21616), c(food_a2)])




