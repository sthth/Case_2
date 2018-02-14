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

max_amount_b1 <- 10
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
  individual_cons[,c(food_b1)] <- 0 
  
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


cons_sub_new$food_b2_new <- rowSums(cons_sub_new[,c(food_b2)])
cons_sub_new$food_b2_diff <- cons_sub_new$food_b2 - cons_sub_new$food_b2_new

cons_sub_new$food_b2_diff <- ifelse(cons_sub_new$food_b2_diff < 1e-12, 0, cons_sub_new$food_b2_diff)

cons_sub_new$total_b2_new
total_b2_new <- aggregate(cons_sub_new$food_b2_new, by = list(cons_sub_new$ID), FUN = sum)
names(total_b2_new)[1] <- "ID"
names(total_b2_new)[2] <- "total_b2_new"

cons_sub_new <- merge(cons_sub_new, total_b2_new, by = "ID")

cons_sub_new$food_b1_new <- rowSums(cons_sub_new[,c(food_b1)])
cons_sub_new$food_b1_diff <- cons_sub_new$food_b1 - cons_sub_new$food_b1_new



#processed meat before and after
quantile(cons_sub_new$food_b1, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%    10%    50%    90%  97.5% 
# 0.00   0.00  15.00 102.10 197.88 

mean(cons_sub_new$food_b1)
# [1] 37.09627

quantile(cons_sub_new$food_b1_new, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%   10%   50%   90% 97.5% 
# 0     0     0     0     0 

mean(cons_sub_new$food_b1_new)
# [1] 0


#daily red meat before and after
quantile(cons_sub_new$food_b2, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%       10%       50%       90%     97.5% 
# 0.00000   0.00000  47.61905 180.52983 310.65871 

mean(cons_sub_new$food_b2)
# [1] 72.46428

quantile(cons_sub_new$food_b2_new, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%      10%      50%      90%    97.5% 
# 0.0000   0.0000  10.5000 101.2536 159.9147

# 2.5%      10%      50%      90%    97.5% #Probabilistic median 500g
# 0.0000   0.0000  28.1250 137.7727 209.9727 

# 2.5%       10%       50%       90%     97.5% #Deterministic 500 g limit
# 0.00000   0.00000  29.11765 137.77268 210.15427 

mean(cons_sub_new$food_b2_new)
# [1] 33.27892

# [1] 50.2127 #probabilistic median 500g

# [1] 50.6668 #deterministic 500g


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
# 52.81717 146.99765 360.53352 532.41141 621.16255 

# 2.5%       10%       50%       90%     97.5% #Deterministic 500g
# 54.26471 157.71727 394.35288 488.76742 497.62153

mean(cons_sub_new$total_b2_new)
# [1] 232.9525

# [1] 351.4889 #probabilistic median 500g

# [1] 354.6676 #Deterministic 500g




### Substitution with food groups b and c ###

cons_sub_new$food_a2_diff <- cons_sub_new$food_b2_diff * SF_b2a

cons_sub_new$food_a1_diff <- cons_sub_new$food_b1_diff * SF_b1a


quantile(cons_sub_new$food_a2_diff, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%       10%       50%       90%     97.5% 
# 0.00000   0.00000   0.00000  80.17058 226.80810 

quantile(cons_sub_new$food_a1_diff, probs = c(0.025, 0.10, 0.50, 0.90, 0.975))
# 2.5%    10%    50%    90%  97.5% 
# 0.00   0.00  45.00 306.30 593.64 




# consumption_sub$diff_b2 <- 0
# consumption_sub$diff_b1_new <- 0
# consumption_sub$diff_c <- 0
# consumption_sub$diff_d <- 0
# consumption_sub$remaining <- 0
# 
# for(i in 1:nrow(consumption_sub)){
#   
#   repeat{
#     type_b1 <- sample(food_b1, 1, replace = T) #draw a food amount food b1
#     for(k in 1:ncol(consumption_sub)){
#       if(names(consumption_sub[k]) == type_b1){ #Chose the column in the consumption data that fits the drawn type
#         if(diff_b1 <= consumption_sub[i,k]){ #if the change in food b1 is smaller than amount consumed of selected food b1
#           consumption_sub[i,k] <- consumption_sub[i,k] - diff_b1 #subtract change in food b1 from consumed amount of selected food b1
#           diff_b1 <- 0 #set change in food b1 to zero
#         }
#         if(consumption_sub[i,k] < diff_b1){ #If the change in food b1 is larger than amount consumed of selected food b1
#           diff_b1 <- diff_b1 - consumption_sub[i,k] #subtract consumed amount of selected food b1 from change in food b1
#           consumption_sub[i,k] <- 0 #set consumed amount of selected food b1 to zero
#         }
#       }
#     }
#     if(diff_b1 == 0){ #if change in food b1 is (now) zero, break
#       break
#     }
#     if(sum(consumption_sub[i,c(food_b1)]) == 0 & diff_b1 > 0){ #if the sum of foods consumed of food b1 is zero and the change in food b1 is larger than zero
#       consumption_sub$diff_b2[i] <- diff_b1 * SF_b1b2 #convert change in food b1 to change in food b2
#       break
#     }
#   }
#   
#   for(l in 1:length(food_a2)){
#     if(consumption_sub$food_a_new[i] == food_a2[l]){
#       consumption_sub$diff_b2[i] <- consumption_sub$diff_a[i]*SF_ab2 + consumption_sub$diff_b2[i]
#     }
#   }
#   
#   
#   diff_b2 <- consumption_sub$diff_b2[i]
#   
#   if(consumption_sub$food_a_new[i] == "X0"){
#     if(consumption_sub$diff_a[i] > 0){
#       sum_a2 <- sum(consumption_sub[i, food_a2])
#       sum_a <- sum(consumption_sub[i, c(food_a1,food_a2)])
#       frac_a2 <- sum_a2/sum_a
#       
#       diff_b2 <- frac_a2 * consumption_sub$diff_a[i] * SF_ab2
#     }
#   }
#   
#   repeat{
#     type_b2 <- sample(food_b2, 1, replace = T)
#     for(m in 1:ncol(consumption_sub)){
#       if(names(consumption_sub[m]) == type_b2){ #Chose the column in the consumption data that fits the drawn fish type
#         if(diff_b2 <= consumption_sub[i,m]){
#           consumption_sub[i,m] <- consumption_sub[i,m] - diff_b2
#           diff_b2 <- 0
#         }
#         if(consumption_sub[i,m] < diff_b2){
#           diff_b2 <- diff_b2 - consumption_sub[i,m]
#           consumption_sub[i,m] <- 0
#         }
#       }
#     }
#     if(diff_b2 == 0){
#       break
#     }
#     if(sum(consumption_sub[i,c(food_b2)]) == 0 & diff_b2 > 0){
#       consumption_sub$diff_b1_new[i] <- diff_b2 * SF_b2b1
#       break
#     }
#   }
#   
#   diff_b1 <- consumption_sub$diff_b1_new[i]
#   repeat{
#     type_b1 <- sample(food_b1, 1, replace = T)
#     for(n in 1:ncol(consumption_sub)){
#       if(names(consumption_sub[n]) == type_b1){ #Chose the column in the consumption data that fits the drawn fish type
#         if(diff_b1 <= consumption_sub[i,n]){
#           consumption_sub[i,n] <- consumption_sub[i,n] - diff_b1
#           diff_b1 <- 0
#         }
#         if(consumption_sub[i,n] < diff_b1){
#           diff_b1 <- diff_b1 - consumption_sub[i,n]
#           consumption_sub[i,n] <- 0
#         }
#       }
#     }
#     if(diff_b1 == 0){
#       break
#     }
#     if(sum(consumption_sub[i,c(food_b1)]) == 0 & diff_b1 > 0){
#       consumption_sub$diff_c[i] <- diff_b1 * SF_b1c
#       break
#     }
#   }
#   
#   diff_c <- consumption_sub$diff_c[i]
#   repeat{
#     type_c <- sample(food_c, 1, replace = T)
#     for(o in 1:ncol(consumption_sub)){
#       if(names(consumption_sub[o]) == type_c){ #Chose the column in the consumption data that fits the drawn fish type
#         if(diff_c <= consumption_sub[i,o]){
#           consumption_sub[i,o] <- consumption_sub[i,o] - diff_c
#           diff_c <- 0
#         }
#         if(consumption_sub[i,o] < diff_c){
#           diff_c <- diff_c - consumption_sub[i,o]
#           consumption_sub[i,o] <- 0
#         }
#       }
#     }
#     if(diff_c == 0){
#       break
#     }
#     if(sum(consumption_sub[i,c(food_c)]) == 0 & diff_c > 0){
#       consumption_sub$diff_d[i] <- diff_c
#       break
#     }
#   }
#   
#   diff_d <- consumption_sub$diff_d[i]
#   repeat{
#     type_d <- sample(food_d, 1, replace = T)
#     for(p in 1:ncol(consumption_sub)){
#       if(names(consumption_sub[p]) == type_d){ #Chose the column in the consumption data that fits the drawn fish type
#         if(diff_d <= consumption_sub[i,p]){
#           consumption_sub[i,p] <- consumption_sub[i,p] - diff_d
#           diff_d <- 0
#         }
#         if(consumption_sub[i,p] < diff_d){
#           diff_d <- diff_d - consumption_sub[i,p]
#           consumption_sub[i,p] <- 0
#         }
#       }
#     }
#     if(diff_d == 0){
#       break
#     }
#     if(sum(consumption_sub[i,c(food_d)]) == 0 & diff_d > 0){
#       consumption_sub$remaining[i] <- diff_d
#       break
#     }
#   }
#   
#   for(p in 1:ncol(consumption_sub)){
#     if(names(consumption_sub[p]) == consumption_sub$food_a_new[i]){ #Chose the column in the consumption data that fits the drawn fish type
#       consumption_sub[i,p] <- consumption_sub$diff_a[i]
#     }
#   }
#   
#   if(consumption_sub$food_a_new[i] == "X0" & consumption_sub$diff_a[i] > 0){
#     if(frac_a1 > 0){
#       consumption_sub[i, food_a1] <- consumption_sub[i, food_a1] + frac_a1 * consumption_sub$diff_a[i] * consumption_sub[i, food_a1] / sum(consumption_sub[i, food_a1])
#     }
#     if(frac_a2 > 0){
#       consumption_sub[i, food_a2] <- consumption_sub[i, food_a2] + frac_a2 * consumption_sub$diff_a[i] * consumption_sub[i, food_a2] / sum(consumption_sub[i, food_a2])
#     }
#   }
# }
# 
# 
# 
# sum(colMeans(consumption_sub[,c(food_b1, food_b2)]))
# # [1] 97.88206
# 
# 
# sum(consumption_sub$remaining > 0)
# # [1] 1012
# 
# quantile(consumption_sub$remaining, probs = c(0.025, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975, 0.99))
# # 2.5%       10%       25%       50%       75%       90%       95%     97.5%       99% 
# # 0.00000   0.00000   0.00000   0.00000   0.00000   0.00000   0.00000  88.18448 252.09627 
# 
# 
# 
# ##Creating new meat variable in substitution data set
# consumption_sub <-  consumption_sub %>%
#   mutate(total.meat = X13 + X16 + X69 + X80 + X112 + X113 + X138 + X139 + X144 + X146 + X189 + X191 + X199 + X201 + X248 + X250 + X266 + X274 + X288 + 
#            X292 + X293 + X294 + X295 + X296 + X297 + X298 + X374 + X376 + X378 + X379 + X380 + X381 + X382 + X419 + X420 + X424 + X429 + X431 + X436 + 
#            X438 + X548 + X549 + X551 + X552 + X562 + X925 + X927 + X941 + X1247 + X1448 + X1453 + X1454) #Sum all meat types
# 
# consumption_sub <-  consumption_sub %>%
#   mutate(proc_meat = X16 + X248 + X250 + X266 + X274 +  X292 +  X293 +  X294 +  X295 +  X296 +  X297 +  X298  + X548 +  X549 +  X551 +  X552  + X562  +
#            X1448 + X1453 + X1454)
# 
# consumption_sub <-  consumption_sub %>%
#   mutate(red_meat = X13 + X80 + X112 + X113 + X138 + X139 + X144 + X146 + X189 + X191 + X199 + X201 + X288 + X374 + X376 + X378 + X379 + X380 + X381 +
#            X382 + X419 + X420 + X424 + X429 + X431 + X436 + X438 + X925 + X927 + X941 + X1247)
# 
# consumption_sub <-  consumption_sub %>%
#   mutate(total.poultry = X6 + X7 + X47 + X48 + X66 + X67 + X97 + X110 + X131 + X132 + X1038 + X1039 + X1449) #Sum all types of poultry
# 
# 
# consumption_sub <- as.data.frame(consumption_sub)
# 
# ##Total weekly intakes
# consumption_sub_weekly <- aggregate(consumption_sub[,c(1:3, 122:126, 134:135, 144:147)], by = consumption_sub[c('ID', 'sex', 'age')], FUN = sum)
# 
# ##plotting daily fish consumption before and after
# 
# x <- consumption$total.fish
# z <- consumption_sub$amount_new
# 
# plot(ecdf(x), do.points = F, col = 'red', verticals = T, main = 'Daily fish consumption before and after substitution', xlab= 'g/day', ylab = 'percentile')
# plot(ecdf(z), do.points = F, col = 'blue', verticals = T, add = T)
# 
# legend('right', c('before', 'after'), fill=c('red', 'blue'), border=NA)
# 
# x_pos <- subset(consumption, total.fish > 0, select = total.fish)
# x_pos <- as.numeric(x_pos$total.fish)
# z_pos <- subset(consumption_sub, amount_new > 0, select = amount_new)
# z_pos <- as.numeric(z_pos$amount_new)
# 
# plot(ecdf(log(x_pos)), do.points = F, col = 'red', verticals = T, main = 'Daily fish consumption before and after substitution',
#      xlab= 'log consumption (consumption in g/day)', ylab = 'percentile')
# plot(ecdf(log(z_pos)), do.points = F, col = 'blue', verticals = T, add = T)
# 
# legend('right', c('before', 'after'), fill=c('red', 'blue'), border=NA)
# 
# ##Histogram of fish consumption before and after substitution
# cons_new <- consumption_sub$amount_new
# cons_new <- ifelse(cons_new == 0, exp(-3), cons_new)
# 
# hist(log(cons_new), breaks = 'fd', xlab = "log consumption (consumption in g/day)", main = "New fish consumption", ylim = c(0,12000), col = 'deepskyblue3')
# 
# cons_old <- consumption_sub$food_a
# cons_old <- ifelse(cons_old == 0, exp(-3), cons_old)
# 
# hist(log(cons_old), breaks = 'fd', xlab = "log consumption (consumption in g/day)", main = "Current fish consumption", ylim = c(0,12000), col = 'deepskyblue3')
# 
# 
# 
# 
# 
# ##Overlay histograms
# #Create the data frame for ggplot
# before <- cons_old
# after <- cons_new
# 
# ggdata <- data.frame(log(before), log(after))
# 
# # Melt the data frame
# ggdata <- melt(ggdata)
# 
# # Set the data frame, & add ecdf() data.
# ggdata <- ddply(ggdata, .(variable), transform, ecd=ecdf(value)(value))
# 
# hist <- ggplot(ggdata, aes(x=value, fill=variable)) + geom_histogram(alpha=0.4, position="identity") +
#   labs(x="log(g/day)",
#        title="Daily fish cons. before and after substitution")
# hist
# 
# # cdf <- ggplot(ggdata, aes(x=value)) + stat_ecdf(aes(colour=variable))
# # cdf
# 
# 
# ##Histogram + ecdf of increase in fish intake
# hist(log(consumption_sub$diff_a), breaks = 'fd', main = "Increase in fish intake amounts", xlab = "log(increase in fish cons. g/day)")
# 
# plot(ecdf(consumption_sub$diff_a), do.points = F, verticals = T, main = 'Increase in fish intake amounts', xlab= 'g/day')
# 
# 
# ##plotting weekly total fish consumption before and after
# x <- consumption_weekly$total.fish
# x_pos <- subset(consumption_weekly, total.fish > 0, select = total.fish)
# x_pos <- as.numeric(x_pos$total.fish)
# 
# z <- consumption_sub_weekly$amount_new
# z_pos <- subset(consumption_sub_weekly, amount_new > 0, select = amount_new)
# z_pos <- as.numeric(z_pos$amount_new)
# 
# plot(ecdf(x_pos), do.points = F, col = 'red', verticals = T, main = 'Fish consumption before and after substitution', xlab= 'g/week', ylab = 'percentile')
# plot(ecdf(z_pos), do.points = F, col = 'blue', verticals = T, add = T)
# 
# legend('right', c('before', 'after'), fill=c('red', 'blue'), border=NA)
# 
# plot(ecdf(log(x_pos)), do.points = F, col = 'red', verticals = T, main = 'Fish consumption before and after substitution',
#      xlab= 'log consumption (consumption in g/week)', ylab = 'percentile')
# plot(ecdf(log(z_pos)), do.points = F, col = 'blue', verticals = T, add = T)
# 
# legend('right', c('before', 'after'), fill=c('red', 'blue'), border=NA)
# 
# ##Fish consumption frequency
# plot(ecdf(consumption_weekly$fish.day), do.points = F, col = 'red', verticals = T, main = 'Fish cons. frequency before and after substitution',
#      xlab= 'days/week', ylab='percentile', xlim = c(0,7))
# plot(ecdf(consumption_sub_weekly$freq_new), do.points = F, col = 'blue', verticals = T, add = T)
# 
# legend('right', c('before', 'after'), fill=c('red', 'blue'), border=NA)
# 
# ##plotting total meat consumption before and after
# x <- consumption_weekly$total.meat
# z <- consumption_sub_weekly$total.meat
# 
# plot(ecdf(x), do.points = F, col = 'red', xlim = c(0,3000), verticals = T, main = 'Meat consumption before and after substitution', xlab= 'g/week', ylab = 'percentile')
# plot(ecdf(z), do.points = F, col = 'blue', add = T)
# 
# legend('right', c('before', 'after'), fill=c('red', 'blue'), border=NA)
# 
# x_pos <- subset(consumption_weekly, total.meat > 0, select = total.meat)
# x_pos <- as.numeric(x_pos$total.meat)
# z_pos <- subset(consumption_sub_weekly, total.meat > 0, select = total.meat)
# z_pos <- as.numeric(z_pos$total.meat)
# 
# plot(ecdf(x_pos), do.points = F, col = 'red', xlim = c(0,3000), verticals = T, main = 'Meat consumption before and after substitution',
#      xlab= 'log consumption (consumption in g/week)', ylab = 'percentile')
# plot(ecdf(z_pos), do.points = F, col = 'blue', add = T)
# 
# legend('right', c('before', 'after'), fill=c('red', 'blue'), border=NA)
# 
# 
# ##Fish intake before and after
# quantile(consumption_weekly$total.fish, probs = c(0.025, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975)) #weekly fish intake before substitution
# # 2.5%         5%        10%        25%        50%        75%        90%        95%      97.5% 
# # 0.000000   0.000000   2.905711  62.193481 173.414481 330.647492 524.873307 677.765985 817.776273 
# 
# quantile(consumption_sub_weekly$amount_new, probs = c(0.025, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975)) #weekly fish intake after substitution
# # 2.5%       5%      10%      25%      50%      75%      90%      95%    97.5% 
# # 193.1889 221.8782 253.9577 320.9731 403.9302 519.6799 672.1410 789.2870 924.2761 
# 
# 
# mean(consumption_sub_weekly$amount_new) #mean weekly fish intake
# # [1] 442.8289
# 
# quantile(consumption_sub$amount_new, probs = c(0.025, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975)) #daily fish intake
# # 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# # 0.00000   0.00000   0.00000   0.00000  28.78681  95.00000 177.29667 240.72410 308.07426
# 
# mean(consumption_sub$amount_new) #mean daily fish intake
# # [1] 63.26127
# 
# 
# ##Total red+proc meat intake before and after
# quantile(consumption_weekly$total.meat, probs = c(0.025, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975)) #weekly meat intake before substitution
# # 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# # 123.0288  192.9311  279.4711  445.1836  672.7634  991.0707 1356.6360 1658.3677 1944.8740 
# 
# quantile(consumption_sub_weekly$total.meat, probs = c(0.025, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975)) #weekly meat intake after substitution
# # 2.5%         5%        10%        25%        50%        75%        90%        95%      97.5% 
# # 83.90553  145.92066  213.68962  370.29075  585.71828  891.97004 1278.77121 1555.71107 1827.33648
# 
# 
# mean(consumption_sub_weekly$total.meat) #mean weekly meat intake
# # [1] 686.043
# 
# quantile(consumption_sub$total.meat, probs = c(0.025, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975)) #total meat daily cons
# # 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# # 0.00000   0.00000   0.00000  15.82622  69.98483 143.21311 232.57855 303.20990 376.10358 
# 
# mean(consumption_sub$total.meat)
# # [1] 98.00614
# 
# ##Total weekly processed meat intake before and after
# quantile(consumption_weekly$proc_meat, probs = c(0.025, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975)) #weekly meat intake before substitution
# # 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# # 0.00000  13.65000  30.00000  83.03111 187.71495 360.03576 567.43200 751.92551 926.88271 
# 
# quantile(consumption_sub_weekly$proc_meat, probs = c(0.025, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975)) #weekly meat intake after substitution
# # 2.5%         5%        10%        25%        50%        75%        90%        95%      97.5% 
# # 0.000000   5.894861  19.980264  65.558977 162.978298 327.358017 530.833321 708.187995 893.853071 
# 
# 
# ##Total weekly red meat intake before and after
# quantile(consumption_weekly$red_meat, probs = c(0.025, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975)) #weekly meat intake before substitution
# # 2.5%         5%        10%        25%        50%        75%        90%        95%      97.5% 
# # 57.40717  103.27011  161.39507  277.92426  451.08896  659.20149  911.21870 1120.45843 1316.70760
# 
# quantile(consumption_sub_weekly$red_meat, probs = c(0.025, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975)) #weekly meat intake before substitution
# # 2.5%         5%        10%        25%        50%        75%        90%        95%      97.5% 
# # 34.62042   69.15962  120.03756  227.29429  390.45495  596.87305  845.84620 1033.35316 1250.40774 
# 
# 
# # write.csv(consumption, file = "consumption.csv", row.names = F)
# write.csv(consumption_sub, file = "consumption_sub_stoch.csv", row.names = F)
# 
# sum(consumption_sub_weekly$amount_new < 350) #number of individuals below 350g fish/week
# # [1] 1007
# 
# mean(consumption_sub_weekly$amount_new < 350) #percentage individuals below 350g fish/week
# # [1] 0.326101
# 
# sum(consumption_sub_weekly$total.meat > 500) #number of individuals > 500g meat/week
# # [1] 1860
# 
# mean(consumption_sub_weekly$total.meat > 500) #percentage individuals > 500g meat/week
# # [1] 0.6023316
# 
# sum(consumption_sub_weekly$freq_new >= 3) #number of individuals with a fish consumption frequency >= 3 days per week
# # [1] 3058
# 
# mean(consumption_sub_weekly$freq_new >= 3) #percentage of individuals with a fish consumption frequency >= 3 days per week
# # [1] 0.990285
# 
# sum(consumption_sub$amount_new == 0) #Zero fish consumption days
# # [1] 6873
# 
# mean(consumption_sub$amount_new == 0) #Percentage zero fish consumption days
# # [1] 0.3179589
# 
# sum(consumption_sub$total.meat == 0) #Zero fish consumption days
# # [1] 3655
# 
# mean(consumption_sub$total.meat == 0) #Percentage zero fish consumption days
# # [1] 0.1690877
# 
# sum(consumption_sub$total.poultry == 0) #Zero fish consumption days
# # [1] 14659
# 
# mean(consumption_sub$total.poultry == 0) #Percentage zero fish consumption days
# # [1] 0.6781551
# 
# 
# sum(consumption_sub$freq_new > consumption_sub$intake_a) #extra fish consumption days
# # 4801
# 
# mean(consumption_sub$freq_new > consumption_sub$intake_a)
# # [1] 0.222104
# 
# sum(consumption_sub$amount_new > consumption_sub$food_a) # number of substitution days
# # 10875
# 
# mean(consumption_sub$amount_new > consumption_sub$food_a)
# # [1] 0.5030996