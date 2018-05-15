# Packages
library(lme4) #glmer + lmer function
library(lmerTest) #allows p-value for lmer
library(boot) #necessary for inv.logit function
library(dplyr)
library(data.table) #necessary for as.data.table
library(psych) #necessary for describe function

##Settings
iters <- 1e+04
set.seed(1)


##Model
usual_intake <-
  function(intake_prob, ID_freq, log_intake, ID_amount, covariable1_f = NULL, covariable1_a = NULL, cofactor1_f = NULL, cofactor1_a = NULL, 
           weight_table_f = NULL, weight_table_a = NULL, iters, df_f, df_a){
    
    ##frequency model
    
    if(is.null(covariable1_f) == T){
      if(is.null(cofactor1_f) == T){
        
        nvar_f <- 0
        freq_model <- glmer(intake_prob ~ (1 | ID_freq), family = "binomial", nAGQ = 10)
        
      }
      else{
        
        nvar_f <- 1
        freq_model <- glmer(intake_prob ~ cofactor1_f + (1 | ID_freq), family = "binomial", nAGQ = 10)
        
      }
    }
    
    if(is.null(covariable1_f) == F){
      if(is.null(cofactor1_f) == F){
        
        nvar_f <- 2
        freq_pol_cov1 <- poly(covariable1_f,degree = df_f, raw = F)
        freq_model <- glmer(intake_prob ~ freq_pol_cov1 + cofactor1_f + (1 | ID_freq), family = "binomial", nAGQ = 10)
        
      }
      else{
        
        nvar_f <- 1
        freq_pol_cov1 <- poly(covariable1_f,degree = df_f, raw = F)
        freq_model <- glmer(intake_prob ~ freq_pol_cov1 + (1 | ID_freq), family = "binomial", nAGQ = 10)
        
      }
    }
    
    ##amounts model
    
    
    if(is.null(covariable1_a) == T){
      if(is.null(cofactor1_a) == T){
        
        nvar_a <- 0
        amounts_model <- lmer(log_intake ~ (1|ID_amount), REML = F)
        
      }
      else{
        
        nvar_a <- 1
        amounts_model <- lmer(log_intake ~ cofactor1_a + (1|ID_amount), REML = F)
        
      }
    }
    
    if(is.null(covariable1_a) == F){
      if(is.null(cofactor1_a) == F){
        
        nvar_a <- 2
        amounts_pol_cov1 <- poly(covariable1_a, degree = df_a, raw = F)
        amounts_model <- lmer(log_intake ~ amounts_pol_cov1 + cofactor1_a + (1|ID_amount), REML = F)
      }
      else{
        
        nvar_a <- 1
        amounts_pol_cov1 <- poly(covariable1_a, degree = df_a, raw = F)
        amounts_model <- lmer(log_intake ~ amounts_pol_cov1 + (1|ID_amount), REML = F)
      }
    }
    
    frequency_coef <- numeric(df_f + nvar_f)
    for(i in 1:(df_f + nvar_f)){
      frequency_coef[i] <- coef(summary(freq_model))[i, "Estimate"]
    }
    
    dispersion <- as.numeric(sqrt(VarCorr(freq_model)$ID))    #logit
    
    
    amount_coef <- numeric(df_a + nvar_a)
    for(i in 1:(df_a + nvar_a)){
      amount_coef[i] <- coef(summary(amounts_model))[i, "Estimate"]
    }
    
    sigma_between <- as.numeric(VarCorr(amounts_model)$ID)            #log between person variance
    sigma_within <- as.numeric(attr(VarCorr(amounts_model), 'sc'))^2  #log within person variance
    
    ##frequency model
    
    if(is.null(covariable1_f) == T){ #neither covariable1 nor cofactor1
      if(is.null(cofactor1_f) == T){
        
        intake <- numeric()
        total_freq <- numeric()
        mu <- frequency_coef[1]
        f <- inv.logit(rnorm(iters, mu, dispersion))
        total_freq <- f
        
      }
      
      else{ #only cofactor1
        
        intake <- numeric()
        total_freq <- numeric()
        
        for(i in 1:(nrow(weight_table_f)))
        {
          mu <- frequency_coef[1]
          for(j in 1:(nvar_f))
          {
            mu <- mu + frequency_coef[j+nvar_f] * weight_table_f[i,nvar_f]
          }
          f <- inv.logit(rnorm(weight_table_f[i,"w"], mu, dispersion))
          total_freq <- c(total_freq,f)
        }
      }
    }
    
    if(is.null(covariable1_f) == F){ #both covariable1 and cofactor1
      if(is.null(cofactor1_f) == F){
        
        covariable1_f <- sort(covariable1_f, decreasing = F)
        freq_pol <- poly(covariable1_f, df_f, raw =F)
        freq_pol <- cbind(covariable1_f, freq_pol)
        freq_pol <- aggregate(freq_pol, by = list(freq_pol[,1]), FUN = mean)
        freq_pol[,1] <- NULL
        names(freq_pol)[1] <- "covariable1"
        weights_f <- merge(freq_pol, weight_table_f, by = "covariable1")
        
        
        intake <- numeric()
        total_freq <- numeric()
        for(i in 1:(nrow(weights_f))){
          mu <- frequency_coef[1]
          for(j in 1:df_f+1){
            mu <- mu + frequency_coef[j+1] * weights_f[i,j+1]
          }
          f <- inv.logit(rnorm(weights_f[i,"w"], mu, dispersion))
          total_freq <- c(total_freq,f)
        }
      }
      else{ #only covariable1
        
        covariable1_f <- sort(covariable1_f, decreasing = F)
        freq_pol <- poly(covariable1_f, df_f, raw =F)
        freq_pol <- cbind(covariable1_f, freq_pol)
        freq_pol <- aggregate(freq_pol, by = list(freq_pol[,1]), FUN = mean)
        freq_pol[,1] <- NULL
        names(freq_pol)[1] <- "covariable1"
        weights_f <- merge(freq_pol, weight_table_f, by = "covariable1")
        
        
        intake <- numeric()
        total_freq <- numeric()
        for(i in 1:(nrow(weights_f))){
          mu <- frequency_coef[1]
          for(j in 1:nvar_f){
            mu <- mu + frequency_coef[j+1] * weights_f[i,j+1]
          }
          f <- inv.logit(rnorm(weights_f[i,"w"], mu, dispersion))
          total_freq <- c(total_freq,f)
        }
      }
    }
    
    
    ##amounts model
    
    if(is.null(covariable1_a) == T){
      if(is.null(cofactor1_a) == T){
        
        total_amount <- numeric()
        mu <- amount_coef[1]
        a <- exp(rnorm(iters, mu, sqrt(sigma_between))+ sigma_within/2)
        total_amount <- a
        
      }
      else{
        
        total_amount <- numeric()
        for(i in 1:(nrow(weight_table_a))){
          
          mu <- amount_coef[1]
          for(j in 1:(nvar_a))
          {
            mu <- mu + amount_coef[j+nvar_a] * weight_table_a[i, nvar_a]
          }
          a <- exp(rnorm(weight_table_a[i,"w"], mu, sqrt(sigma_between))+ sigma_within/2)
          total_amount <- c(total_amount,a)
        }
      }
    }
    
    
    if(is.null(covariable1_a) == F){ #Both covariable1 and cofactor1
      if(is.null(cofactor1_a) == F){
        
        covariable1_a <- sort(covariable1_a, decreasing = F)
        amount_pol <- poly(covariable1_a, df_a, raw = F)
        amount_pol <- cbind(covariable1_a, amount_pol)
        amount_pol <- aggregate(amount_pol, by = list(amount_pol[,1]), FUN = mean)
        amount_pol[,1] <- NULL
        names(amount_pol)[1] <- "covariable1"
        weights_a <- merge(amount_pol, weight_table_a, by = c("covariable1"))
        
        total_amount <- numeric()
        for(i in 1:(nrow(weights_a))){
          mu <- amount_coef[1]
          for(j in 1:df_a){
            mu <- mu + amount_coef[j+nvar_a] * weights_a[i, j+nvar_a]
          }
          a <- exp(rnorm(weights_a[i,"w"], mu, sqrt(sigma_between))+ sigma_within/2)
          total_amount <- c(total_amount,a)
        }
      }
      
      else{
        
        covariable1_a <- sort(covariable1_a, decreasing = F)
        amount_pol <- poly(covariable1_a, df_a, raw = F)
        amount_pol <- cbind(covariable1_a, amount_pol)
        amount_pol <- aggregate(amount_pol, by = list(amount_pol[,1]), FUN = mean)
        amount_pol[,1] <- NULL
        names(amount_pol)[1] <- "covariable1"
        weights_a <- merge(amount_pol, weight_table_a, by = c("covariable1"))
        
        total_amount <- numeric()
        for(i in 1:(nrow(weights_a))){
          mu <- amount_coef[1]
          for(j in 1:df_a){
            mu <- mu + amount_coef[j+nvar_a] * weights_a[i, j+nvar_a]
          }
          a <- exp(rnorm(weights_a[i,"w"], mu, sqrt(sigma_between))+ sigma_within/2)
          total_amount <- c(total_amount,a)
          
        }
      }
    }
    
    
    intake <- total_freq * total_amount
    intake <<- intake
    
    
    ##QQ plot - are residuals normally distributed?
    qqnorm(resid(amounts_model))
    qqline(resid(amounts_model))
    
    hist <- hist(log(intake), main = "Log(usual intake)", xlab = "log(usual intake)", xlim = range(-2,0,2,4,6), col = "deepskyblue", freq = F)
    result <- list("Frequency model" = summary(freq_model), "Amounts model" = summary(amounts_model), "Mean usual intake" = mean(intake), "sd usual intake" = sd(intake),
                   "Percentiles" = quantile(intake, prob = c(.025, .05, .1, .5, .9, .95, 0.975)), "n" = length(intake))
    return(result)
    
  }
