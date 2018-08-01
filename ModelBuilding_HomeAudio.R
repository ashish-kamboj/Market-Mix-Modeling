
################################################################################################################################################################
#                                                 :::::::: Model Building [HomeAudio] ::::::::
################################################################################################################################################################
    
#********************************************************[Linear Regression Model]
  
### Preparing dataset
  ## Removing lag variables and Moving averages variables
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    LR_HA_data <- HomeAudio_final[,-c(21:22,63:77)]
    
  ## Scaling the variables
    LR_HA_data[,2:ncol(LR_HA_data)] <- scale(LR_HA_data[,2:ncol(LR_HA_data)])
    
### Stepwise Regression to remove insignificant and correlated variables
  LR_HA_base.mod <- lm(gmv ~ 1 , data= LR_HA_data)  # base intercept only model
  LR_HA_all.mod <- lm(gmv ~ . , data= LR_HA_data) # full model with all predictors
  LR_HA_stepMod <- step(LR_HA_base.mod, scope = list(lower = LR_HA_base.mod, upper = LR_HA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  LR_HA_shortlistedVars <- names(unlist(LR_HA_stepMod[[1]])) # get the shortlisted variable.
  LR_HA_shortlistedVars <- LR_HA_shortlistedVars[!LR_HA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables [using LR_HA_stepMod]
    LR_HA_model_1 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation + 
                          Digital_adstock + sla + price_tag.xMass_Product + product_analytic_vertical.xKaraokePlayer + 
                          special_sale_day.xEid...Rathayatra + week + deliverybdays + 
                          TV_adstock + product_analytic_vertical.xDJController + Content_Marketing + 
                          TV, data = LR_HA_data)
    
    
    summary(LR_HA_model_1)
    vif(LR_HA_model_1)
    
    
  ## High VIF and Insignificant p-value columns: price_tag.xMass_Product, TV
  ## Insignificant p-value columns: special_sale_day.xEid...Rathayatra 
    LR_HA_model_2 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation + 
                          Digital_adstock + sla + product_analytic_vertical.xKaraokePlayer + 
                          week + deliverybdays + TV_adstock + product_analytic_vertical.xDJController + 
                          Content_Marketing, data = LR_HA_data)
    
    summary(LR_HA_model_2)
    vif(LR_HA_model_2)
    
    
  ## Slightly High VIF and Insignificant p-value columns: Content_Marketing  
  ## Less significant p-value columns: sla
    LR_HA_model_3 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation + 
                          Digital_adstock + product_analytic_vertical.xKaraokePlayer + week + deliverybdays + 
                          TV_adstock + product_analytic_vertical.xDJController , data = LR_HA_data)
    
    
    summary(LR_HA_model_3)
    vif(LR_HA_model_3)
    
    
  ## Less significant p-value columns: TV_adstock
    LR_HA_model_4 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation + 
                          Digital_adstock + product_analytic_vertical.xKaraokePlayer + week + deliverybdays + 
                          product_analytic_vertical.xDJController , data = LR_HA_data)
    
    
    summary(LR_HA_model_4)
    vif(LR_HA_model_4)
    
    
  ## Slightly higher VIF and less significant p-value columns: deliverybdays
    LR_HA_model_5 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation + 
                          Digital_adstock + product_analytic_vertical.xKaraokePlayer + week + 
                          product_analytic_vertical.xDJController , data = LR_HA_data)
    
    
    summary(LR_HA_model_5)
    vif(LR_HA_model_5)
    
    
  ## Insignificant p-value columns: week, product_analytic_vertical.xDJController
    LR_HA_model_6 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation + 
                          Digital_adstock + product_analytic_vertical.xKaraokePlayer, data = LR_HA_data)
    
    
    summary(LR_HA_model_6)
    vif(LR_HA_model_6)
    
    
  ## Insignificant p-value columns: product_analytic_vertical.xKaraokePlayer
    LR_HA_model_7 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation + 
                          Digital_adstock, data = LR_HA_data)
    
    
    summary(LR_HA_model_7)
    vif(LR_HA_model_7) 
    
    
  ## Less significant p-value columns: Digital_adstock, product_analytic_vertical.xDockingStation
    LR_HA_model_8 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Affiliates, data = LR_HA_data)
    
    
    summary(LR_HA_model_8)
    vif(LR_HA_model_8) 
    
    
  ## Trying to remove "product_mrp" [as it is having high VIF] and then see the change in Adjusted R-squared
    LR_HA_model_9 <- lm(formula = gmv ~ units + special_sale_day.xDaussera + 
                          product_procurement_sla + Affiliates, data = LR_HA_data)
    
    
    summary(LR_HA_model_9)  # Adjusted R-squared value is changed at 3rd place of decimal, we're good to remove the variable
                            # We also tried to remove "units" variable but change in Adjusted R-squared value was more
    vif(LR_HA_model_9) 
    

  ## Insignificant p-value columns: Affiliates, product_procurement_sla
    LR_HA_model_10 <- lm(formula = gmv ~ units + special_sale_day.xDaussera, data = LR_HA_data)
    
    
    summary(LR_HA_model_10)
    vif(LR_HA_model_10) 
    
    
 
    
### Cross-validation
 cv.lm(data = LR_HA_data, form.lm = LR_HA_model_10, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
   
  
  
### Estimating the elasticity coefficients
  
  elasticity_2 <- function(var){
    LR_HA_elasticity <-as.numeric(LR_HA_model_10$coefficients[var]*mean(LR_HA_data[,var])/mean(LR_HA_data$gmv))
    return(LR_HA_elasticity)
    
  } 
  
  LR_HA_var_list <- list()
  
  for(i in 2:length(LR_HA_model_10$coefficients)){
    LR_HA_var_list[i-1] <-elasticity_2(names(LR_HA_model_10$coefficients)[i])
    
  }
  
  LR_HA_elasticity.outputs <- data.frame(names(LR_HA_model_10$coefficients[2:length(LR_HA_model_10$coefficients)]))
  LR_HA_elasticity.outputs <- cbind(LR_HA_elasticity.outputs,do.call(rbind.data.frame, LR_HA_var_list))
  colnames(LR_HA_elasticity.outputs) <- c("Variable","Elasticity")
  
  LR_HA_elasticity.outputs$Direction <- ifelse(LR_HA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

  # Plotting the elasticity
    ggplot(LR_HA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity", width = 0.9) + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y= 0.5),hjust = 1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank()) +
    ggtitle("HomeAudio - Linear Regression Model") +xlab("Variables")  
  
   
    

#********************************************************[Multiplicative Model] 
    
### Preparing dataset
  ## Removing lag variables and Moving averages variables
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    MM_HA_data <- HomeAudio_final[,-c(21:22,63:77)]
    
  ## Replacing 0 value in column with '0.00001' as log(0) is undefined
    MM_HA_data[MM_HA_data == 0] <- 0.00001
    
  ## Taking log of all the variable to buils to Multiplicative model
    MM_HA_data <- log(MM_HA_data)
    
  ## Checking the variables for linear relationship or multicollinearity
    MM_HA_model <- lm(gmv~.,MM_HA_data)
    alias(MM_HA_model)
    
  ## Removing the variables which were showing linear relationship or multicollinearity
    MM_HA_data <- MM_HA_data[, -c(51:58)]
    
### Stepwise Regression to remove insignificant and correlated variables
  MM_HA_base.mod <- lm(gmv ~ 1 , data= MM_HA_data)  # base intercept only model
  MM_HA_all.mod <- lm(gmv ~ . , data= MM_HA_data) # full model with all predictors
  MM_HA_stepMod <- step(MM_HA_base.mod, scope = list(lower = MM_HA_base.mod, upper = MM_HA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  MM_HA_shortlistedVars <- names(unlist(MM_HA_stepMod[[1]])) # get the shortlisted variable.
  MM_HA_shortlistedVars <- MM_HA_shortlistedVars[!MM_HA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables [using MM_HA_stepMod]
    MM_HA_model_1 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing + 
                          SEM_adtock + deliverybdays + wday.xTuesday + price_tag.xPremium_Product + 
                          TV_adstock + wday.xSunday + product_procurement_sla + product_analytic_vertical.xKaraokePlayer + 
                          product_analytic_vertical.xSlingBox + sla + wday.xWednesday + 
                          Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xDJController + 
                          wday.xMonday + product_analytic_vertical.xSoundMixer + is_special_sale_day + 
                          special_sale_day.xChristmas...New.Year + Online_Marketing + 
                          units + Total_Investment, data = MM_HA_data)
    
    
    summary(MM_HA_model_1)
    vif(MM_HA_model_1)
    
    
  ## High VIF and insignificant p-value columns: Total_Investment, units
    MM_HA_model_2 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing + 
                          SEM_adtock + deliverybdays + wday.xTuesday + price_tag.xPremium_Product + 
                          TV_adstock + wday.xSunday + product_procurement_sla + product_analytic_vertical.xKaraokePlayer + 
                          product_analytic_vertical.xSlingBox + sla + wday.xWednesday + 
                          Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xDJController + 
                          wday.xMonday + product_analytic_vertical.xSoundMixer + is_special_sale_day + 
                          special_sale_day.xChristmas...New.Year + Online_Marketing, data = MM_HA_data)
    
    
    summary(MM_HA_model_2)
    vif(MM_HA_model_2)
    
    
  ## Insignificant p-value columns: product_analytic_vertical.xSoundMixer
  ## Less significant p-value columns: is_special_sale_day
    MM_HA_model_3 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing + 
                          SEM_adtock + deliverybdays + wday.xTuesday + price_tag.xPremium_Product + 
                          TV_adstock + wday.xSunday + product_procurement_sla + product_analytic_vertical.xKaraokePlayer + 
                          product_analytic_vertical.xSlingBox + sla + wday.xWednesday + 
                          Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xDJController + 
                          wday.xMonday + special_sale_day.xChristmas...New.Year + Online_Marketing, data = MM_HA_data)
    
    
    summary(MM_HA_model_3)
    vif(MM_HA_model_3)
    
    
  ## Slightly high VIF and Insignificant p-value columns: special_sale_day.xChristmas...New.Year 
  ## High VIF and Insignificant p-value columns: Online_Marketing 
  ## Insignificant p-value columns: wday.xMonday
    MM_HA_model_4 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing + 
                          SEM_adtock + deliverybdays + wday.xTuesday + price_tag.xPremium_Product + 
                          TV_adstock + wday.xSunday + product_procurement_sla + product_analytic_vertical.xKaraokePlayer + 
                          product_analytic_vertical.xSlingBox + sla + wday.xWednesday + 
                          Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xDJController, data = MM_HA_data)
    
    
    summary(MM_HA_model_4)
    vif(MM_HA_model_4)
    
    
  ##High VIF and Insignificant p-value columns: product_analytic_vertical.xDJController 
    MM_HA_model_5 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing + 
                          SEM_adtock + deliverybdays + wday.xTuesday + price_tag.xPremium_Product + 
                          TV_adstock + wday.xSunday + product_procurement_sla + product_analytic_vertical.xKaraokePlayer + 
                          product_analytic_vertical.xSlingBox + sla + wday.xWednesday + 
                          Content_Marketing_adstock + Sponsorship, data = MM_HA_data)
    
    
    summary(MM_HA_model_5)
    vif(MM_HA_model_5)
    
    
  ## High VIF and Insignificant p-value columns: SEM_adtock 
  ## Less significant p-value columns: price_tag.xPremium_Product 
    MM_HA_model_6 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing + 
                          deliverybdays + wday.xTuesday + TV_adstock + wday.xSunday + 
                          product_procurement_sla + product_analytic_vertical.xKaraokePlayer + 
                          product_analytic_vertical.xSlingBox + sla + wday.xWednesday + 
                          Content_Marketing_adstock + Sponsorship, data = MM_HA_data)
    
    
    summary(MM_HA_model_6)
    vif(MM_HA_model_6)
    
    
  ## Less significant p-value columns: product_analytic_vertical.xSlingBox 
  ## High VIF and less significant p-value columns: wday.xWednesday 
    MM_HA_model_7 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing + 
                          deliverybdays + wday.xTuesday + TV_adstock + wday.xSunday + 
                          product_procurement_sla + product_analytic_vertical.xKaraokePlayer + 
                          sla + Content_Marketing_adstock + Sponsorship, data = MM_HA_data)
    
    
    summary(MM_HA_model_7)
    vif(MM_HA_model_7)
    
    
  ## High VIF and Insignificant p-value columns: Content_Marketing_adstock 
    MM_HA_model_8 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing + 
                          deliverybdays + wday.xTuesday + TV_adstock + wday.xSunday + 
                          product_procurement_sla + product_analytic_vertical.xKaraokePlayer + 
                          sla + Sponsorship, data = MM_HA_data)
    
    
    summary(MM_HA_model_8)
    vif(MM_HA_model_8)
    
    
  ## Insignificant p-value columns: Content_Marketing
  ## Less significant p-value columns: wday.xSunday, product_procurement_sla, wday.xTuesday
    MM_HA_model_9 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + deliverybdays + 
                          TV_adstock + product_analytic_vertical.xKaraokePlayer + 
                          sla + Sponsorship, data = MM_HA_data)
    
    
    summary(MM_HA_model_9)
    vif(MM_HA_model_9)
    
    
  ## Less significant p-value columns: product_analytic_vertical.xKaraokePlayer
    MM_HA_model_10 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + deliverybdays + 
                           TV_adstock + sla + Sponsorship, data = MM_HA_data)
    
    
    summary(MM_HA_model_10)
    vif(MM_HA_model_10)
    
    
  ## Removing "price_tag.xMass_Product" [as it's having high VIF]
    MM_HA_model_11 <- lm(formula = gmv ~ product_mrp + deliverybdays + 
                           TV_adstock + sla + Sponsorship, data = MM_HA_data)
    
    
    summary(MM_HA_model_11)  # Adjusted R-squared is changed at 3rd place of decimal, we're good to remove that variable
    vif(MM_HA_model_11)
    
    
  ## Less significant p-value columns: TV_adstock
    MM_HA_model_12 <- lm(formula = gmv ~ product_mrp + deliverybdays + sla + Sponsorship, data = MM_HA_data)
    
    summary(MM_HA_model_12)
    vif(MM_HA_model_12)
    
    
  ## Insignificant p-value columns: Sponsorship
    MM_HA_model_13 <- lm(formula = gmv ~ product_mrp + deliverybdays + sla, data = MM_HA_data)
    
    summary(MM_HA_model_13)
    vif(MM_HA_model_13)
    
    
    
### Cross-validation
  cv.lm(data = MM_HA_data, form.lm = MM_HA_model_13, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
    
    
    
### Estimating the elasticity coefficients
    
  elasticity_5 <- function(var){
      MM_HA_elasticity <- as.numeric(MM_HA_model_13$coefficients[var]*mean(MM_HA_data[,var])/mean(MM_HA_data$gmv))
      return(MM_HA_elasticity)
      
  } 
    
  MM_HA_var_list <- list()
    
  for(i in 2:length(MM_HA_model_13$coefficients)){
      MM_HA_var_list[i-1] <- elasticity_5(names(MM_HA_model_13$coefficients)[i])
      
  }
    
  MM_HA_elasticity.outputs <- data.frame(names(MM_HA_model_13$coefficients[2:length(MM_HA_model_13$coefficients)]))
  MM_HA_elasticity.outputs <- cbind(MM_HA_elasticity.outputs,do.call(rbind.data.frame, MM_HA_var_list))
  colnames(MM_HA_elasticity.outputs) <- c("Variable","Elasticity")
    
  MM_HA_elasticity.outputs$Direction <- ifelse(MM_HA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

    
  # Plotting the elasticity
    ggplot(MM_HA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust=0.1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    ggtitle("HomeAudio - Multiplicative Model") +xlab("Variables")  
    
    
    

      
#********************************************************[Koyck Model]
    
### Preparing dataset
  ## Removing lag variables and Moving averages variables but considering the 1 week lag value of 'gmv'
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    KM_HA_data <- HomeAudio_final[,-c(21:22,63:74,76:77)]
    
  ## Scaling the variables
    KM_HA_data[,2:ncol(KM_HA_data)] <- scale(KM_HA_data[,2:ncol(KM_HA_data)])
    
    
### Stepwise Regression to remove insignificant and correlated variables
  KM_HA_base.mod <- lm(gmv ~ 1 , data= KM_HA_data)  # base intercept only model
  KM_HA_all.mod <- lm(gmv ~ . , data= KM_HA_data) # full model with all predictors
  KM_HA_stepMod <- step(KM_HA_base.mod, scope = list(lower = KM_HA_base.mod, upper = KM_HA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  KM_HA_shortlistedVars <- names(unlist(KM_HA_stepMod[[1]])) # get the shortlisted variable.
  KM_HA_shortlistedVars <- KM_HA_shortlistedVars[!KM_HA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables [using KM_HA_stepMod]
    KM_HA_model_1 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Affiliates + Digital_adstock + 
                          sla + product_analytic_vertical.xKaraokePlayer + special_sale_day.xEid...Rathayatra + 
                          week + GMV_lag_1_per + deliverybdays + TV_adstock + Digital + 
                          wday.xSaturday + special_sale_day.xBSD + product_analytic_vertical.xHomeAudioSpeaker + 
                          wday.xSunday + product_analytic_vertical.xFMRadio + special_sale_day.xRepublic.Day + 
                          special_sale_day.xValentine.Day + Online_Marketing_adstock + 
                          Other_adstock + price_tag.xMass_Product + Sponsorship + Content_Marketing_adstock + 
                          product_analytic_vertical.xSlingBox, data = KM_HA_data)
    
    
    summary(KM_HA_model_1)
    vif(KM_HA_model_1)
    
    
  ## Insignificant p-value columns: product_analytic_vertical.xSlingBox
  ## High VIF and Insignificant p-value columns: product_analytic_vertical.xHomeAudioSpeaker, wday.xSunday
  ## Slightly High VIF and Insignificant p-value columns: sla
    KM_HA_model_2 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Affiliates + Digital_adstock + 
                          product_analytic_vertical.xKaraokePlayer + special_sale_day.xEid...Rathayatra + 
                          week + GMV_lag_1_per + deliverybdays + TV_adstock + Digital + wday.xSaturday + 
                          special_sale_day.xBSD + product_analytic_vertical.xFMRadio + special_sale_day.xRepublic.Day + 
                          special_sale_day.xValentine.Day + Online_Marketing_adstock + Other_adstock + 
                          price_tag.xMass_Product + Sponsorship + Content_Marketing_adstock, data = KM_HA_data)
    
    
    summary(KM_HA_model_2)
    vif(KM_HA_model_2)
    
    
  ## Slightly High VIF and Insignificant p-value columns: wday.xSaturday
  ## Insignificant p-value columns: special_sale_day.xBSD
  ## Slightly High VIF and less significant p-value columns: Affiliates
    KM_HA_model_3 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer + 
                          special_sale_day.xEid...Rathayatra + week + GMV_lag_1_per + deliverybdays + 
                          TV_adstock + Digital + product_analytic_vertical.xFMRadio + special_sale_day.xRepublic.Day + 
                          special_sale_day.xValentine.Day + Online_Marketing_adstock + Other_adstock + 
                          price_tag.xMass_Product + Sponsorship + Content_Marketing_adstock, data = KM_HA_data)
    
    
    summary(KM_HA_model_3)
    vif(KM_HA_model_3)
    
    
  ## High VIF and Insignificant p-value columns: Online_Marketing_adstock
    KM_HA_model_4 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer + 
                          special_sale_day.xEid...Rathayatra + week + GMV_lag_1_per + deliverybdays + 
                          TV_adstock + Digital + product_analytic_vertical.xFMRadio + special_sale_day.xRepublic.Day + 
                          special_sale_day.xValentine.Day + Other_adstock + price_tag.xMass_Product + Sponsorship + 
                          Content_Marketing_adstock, data = KM_HA_data)
    
    
    summary(KM_HA_model_4)
    vif(KM_HA_model_4)
    
    
  ## Slightly High VIF and less significant p-value columns: Sponsorship
  ## High VIF and less significant p-value columns: Content_Marketing_adstock
    KM_HA_model_5 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer + 
                          special_sale_day.xEid...Rathayatra + week + GMV_lag_1_per + deliverybdays + 
                          TV_adstock + Digital + product_analytic_vertical.xFMRadio + special_sale_day.xRepublic.Day + 
                          special_sale_day.xValentine.Day + Other_adstock + price_tag.xMass_Product, data = KM_HA_data)
    
    
    summary(KM_HA_model_5)
    vif(KM_HA_model_5)
    
    
  ## Insignificant p-value columns: Other_adstock
  ##Less significant p-value columns: deliverybdays
    KM_HA_model_6 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer + 
                          special_sale_day.xEid...Rathayatra + week + GMV_lag_1_per + TV_adstock + 
                          Digital + product_analytic_vertical.xFMRadio + special_sale_day.xRepublic.Day + 
                          special_sale_day.xValentine.Day + price_tag.xMass_Product, data = KM_HA_data)
    
    
    summary(KM_HA_model_6)
    vif(KM_HA_model_6)
    
    
  ## Insignificant p-value columns: special_sale_day.xRepublic.Day
    KM_HA_model_7 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer + 
                          special_sale_day.xEid...Rathayatra + week + GMV_lag_1_per + TV_adstock + 
                          Digital + product_analytic_vertical.xFMRadio + 
                          special_sale_day.xValentine.Day + price_tag.xMass_Product, data = KM_HA_data)
    
    
    summary(KM_HA_model_7)
    vif(KM_HA_model_7)
    
    
  ## High VIF and Insignificant p-value columns: price_tag.xMass_Product
  ## Insignificant p-value columns: special_sale_day.xValentine.Day
    KM_HA_model_8 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer + 
                          special_sale_day.xEid...Rathayatra + week + GMV_lag_1_per + TV_adstock + 
                          Digital + product_analytic_vertical.xFMRadio, data = KM_HA_data)
    
    summary(KM_HA_model_8)
    vif(KM_HA_model_8)
    
    
  ## High VIF and Insignificant p-vaue value columns: product_analytic_vertical.xFMRadio
    KM_HA_model_9 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer + 
                          special_sale_day.xEid...Rathayatra + week + GMV_lag_1_per + TV_adstock + 
                          Digital, data = KM_HA_data)
    
    
    summary(KM_HA_model_9)
    vif(KM_HA_model_9)
    
    
  ## Slightly hogh VIF and Insignificant p-value columns: Digital
  ## Less significant p-value columns: product_analytic_vertical.xKaraokePlayer
    KM_HA_model_10 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                           product_procurement_sla + Digital_adstock + special_sale_day.xEid...Rathayatra + 
                           week + GMV_lag_1_per + TV_adstock, data = KM_HA_data)
    
    
    summary(KM_HA_model_10)
    vif(KM_HA_model_10)
    
    
  ## Less significant p-va;ue columns: TV_adstock, Digital_adstock
    KM_HA_model_11 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                           product_procurement_sla + special_sale_day.xEid...Rathayatra + 
                           week + GMV_lag_1_per, data = KM_HA_data)
    
    
    summary(KM_HA_model_11)
    vif(KM_HA_model_11)
    
    
  ## Insignificant p-value columns: week
  ## Less significant p-value columns: GMV_lag_1_per, special_sale_day.xEid...Rathayatra
    KM_HA_model_12 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                           product_procurement_sla, data = KM_HA_data)
    
    
    summary(KM_HA_model_12)
    vif(KM_HA_model_12)
    
    
  ## Less significant p-value columns: product_procurement_sla
    KM_HA_model_13 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera, data = KM_HA_data)
    
    summary(KM_HA_model_13)
    vif(KM_HA_model_13)
    
    
  ## Removing "product_mrp" variable [as it' is's having high VIF] and will check the change in Adjusted R-squared value
    KM_HA_model_14 <- lm(formula = gmv ~ units  + special_sale_day.xDaussera, data = KM_HA_data)
    
    summary(KM_HA_model_14) # Slight change in Adjusted R-squared value at 3rd place of decimal
    vif(KM_HA_model_14)
    
    
    
### Cross-validation
  cv.lm(data = KM_HA_data, form.lm = KM_HA_model_14, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
    
    
    
### Estimating the elasticity coefficients
    
  elasticity_8 <- function(var){
      KM_HA_elasticity <- as.numeric(KM_HA_model_14$coefficients[var]*mean(KM_HA_data[,var])/mean(KM_HA_data$gmv))
      return(KM_HA_elasticity)
      
  } 
    
  KM_HA_var_list <- list()
    
  for(i in 2:length(KM_HA_model_14$coefficients)){
      KM_HA_var_list[i-1] <- elasticity_8(names(KM_HA_model_14$coefficients)[i])
      
  }
    
  KM_HA_elasticity.outputs <- data.frame(names(KM_HA_model_14$coefficients[2:length(KM_HA_model_14$coefficients)]))
  KM_HA_elasticity.outputs <- cbind(KM_HA_elasticity.outputs,do.call(rbind.data.frame, KM_HA_var_list))
  colnames(KM_HA_elasticity.outputs) <- c("Variable","Elasticity")
    
  KM_HA_elasticity.outputs$Direction <- ifelse(KM_HA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

    
  # Plotting the elasticity
    ggplot(KM_HA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    ggtitle("HomeAudio - Koyck Model") +xlab("Variables")  
    
    
    
    
    
#********************************************************[Distributive Lag Model] 
    
### Preparing dataset
  ## Removing lag variables and Moving averages variables but considering the 1, 2 and 3 weeks lag value of 'gmv'
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    DL_HA_data <- HomeAudio_final[,-c(21:22,63:74)]
    
  ## Scaling the variables
    DL_HA_data[,2:ncol(DL_HA_data)] <- scale(DL_HA_data[,2:ncol(DL_HA_data)])
    
    
### Stepwise Regression to remove insignificant and correlated variables
  DL_HA_base.mod <- lm(gmv ~ 1 , data= DL_HA_data)  # base intercept only model
  DL_HA_all.mod <- lm(gmv ~ . , data= DL_HA_data) # full model with all predictors
  DL_HA_stepMod <- step(DL_HA_base.mod, scope = list(lower = DL_HA_base.mod, upper = DL_HA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  DL_HA_shortlistedVars <- names(unlist(DL_HA_stepMod[[1]])) # get the shortlisted variable.
  DL_HA_shortlistedVars <- DL_HA_shortlistedVars[!DL_HA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables [using DL_HA_stepMod]
    DL_HA_model_1 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + product_analytic_vertical.xDockingStation + 
                          Digital_adstock + sla + price_tag.xMass_Product + product_analytic_vertical.xKaraokePlayer + 
                          GMV_lag_2_per + week + deliverybdays + GMV_lag_1_per + TV_adstock + 
                          Digital + wday.xSaturday + special_sale_day.xBSD + GMV_lag_3_per + 
                          product_analytic_vertical.xFMRadio + special_sale_day.xRepublic.Day + 
                          special_sale_day.xValentine.Day + Other_adstock + Radio + 
                          special_sale_day.xFHSD + product_analytic_vertical.xSlingBox + 
                          product_analytic_vertical.xHiFiSystem + Sponsorship + deliverycdays + 
                          wday.xWednesday, data = DL_HA_data)
    
    
    summary(DL_HA_model_1)
    vif(DL_HA_model_1)
    
    
  ## Slightly High VIF and Insignificant p-value columns: wday.xWednesday, product_analytic_vertical.xHiFiSystem, sla
  ## High VIF and Insignificant p-value columns: deliverybdays
  ## Insignificant p-value columns: special_sale_day.xFHSD, Sponsorship, product_analytic_vertical.xSlingBox
    DL_HA_model_2 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla + product_analytic_vertical.xDockingStation + 
                          Digital_adstock + price_tag.xMass_Product + product_analytic_vertical.xKaraokePlayer + 
                          GMV_lag_2_per + week + GMV_lag_1_per + TV_adstock + Digital + wday.xSaturday + 
                          special_sale_day.xBSD + GMV_lag_3_per + product_analytic_vertical.xFMRadio + 
                          special_sale_day.xRepublic.Day + special_sale_day.xValentine.Day + 
                          Other_adstock + Radio + deliverycdays, data = DL_HA_data)
    
    summary(DL_HA_model_2)
    vif(DL_HA_model_2)
    
    
  ## Slightly high VIF and Insignificant p-value columns: wday.xSaturday, product_analytic_vertical.xDockingStation
    DL_HA_model_3 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla +  Digital_adstock + price_tag.xMass_Product + 
                          product_analytic_vertical.xKaraokePlayer + GMV_lag_2_per + week + 
                          GMV_lag_1_per + TV_adstock + Digital + special_sale_day.xBSD + 
                          GMV_lag_3_per + product_analytic_vertical.xFMRadio + 
                          special_sale_day.xRepublic.Day + special_sale_day.xValentine.Day + 
                          Other_adstock + Radio + deliverycdays, data = DL_HA_data)
    
    
    summary(DL_HA_model_3)
    vif(DL_HA_model_3)
    
    
  ## Less significant p-value columns: GMV_lag_3_per, deliverycdays, Other_adstock
  ## Slightly high VIF and less significant p-value columns: Radio
    DL_HA_model_4 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla +  Digital_adstock + price_tag.xMass_Product + 
                          product_analytic_vertical.xKaraokePlayer + GMV_lag_2_per + week + 
                          GMV_lag_1_per + TV_adstock + Digital + special_sale_day.xBSD + 
                          product_analytic_vertical.xFMRadio + special_sale_day.xRepublic.Day + 
                          special_sale_day.xValentine.Day, data = DL_HA_data)
    
    
    summary(DL_HA_model_4)
    vif(DL_HA_model_4)
    
    
  ## Insignificant p-value columns: special_sale_day.xRepublic.Day
    DL_HA_model_5 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla +  Digital_adstock + price_tag.xMass_Product + 
                          product_analytic_vertical.xKaraokePlayer + GMV_lag_2_per + week + 
                          GMV_lag_1_per + TV_adstock + Digital + special_sale_day.xBSD + 
                          product_analytic_vertical.xFMRadio + special_sale_day.xValentine.Day, data = DL_HA_data)
    
    
    summary(DL_HA_model_5)
    vif(DL_HA_model_5)
    
    
  ## High VIF and Insignificant p-value columns: price_tag.xMass_Product
  ## Insignificant p-value columns: special_sale_day.xValentine.Day
    DL_HA_model_6 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla +  Digital_adstock + 
                          product_analytic_vertical.xKaraokePlayer + GMV_lag_2_per + week + 
                          GMV_lag_1_per + TV_adstock + Digital + special_sale_day.xBSD + 
                          product_analytic_vertical.xFMRadio, data = DL_HA_data)
    
    
    summary(DL_HA_model_6)
    vif(DL_HA_model_6)
    
    
  ## High VIF and Insignificant p-value columns: product_analytic_vertical.xFMRadio
  ## Slightly High VIF and less significant p-value columns: Digital
  ## Less significant p-value columns: special_sale_day.xBSD, product_analytic_vertical.xKaraokePlayer
    DL_HA_model_7 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla +  Digital_adstock + GMV_lag_2_per + week + 
                          GMV_lag_1_per + TV_adstock, data = DL_HA_data)
    
    
    summary(DL_HA_model_7)
    vif(DL_HA_model_7)
    
    
  ## Less significant p-value columns: TV_adstock, week
    DL_HA_model_8 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla +  Digital_adstock + GMV_lag_2_per + 
                          GMV_lag_1_per, data = DL_HA_data)
    
    
    summary(DL_HA_model_8)
    vif(DL_HA_model_8)
    
    
  ## Insignificant p-value columns: Digital_adstock
  ## Less significant p-value columns: GMV_lag_1_per, GMV_lag_2_per
    DL_HA_model_9 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera + 
                          product_procurement_sla, data = DL_HA_data)
    
    
    summary(DL_HA_model_9)
    vif(DL_HA_model_9)
    
    
  ## Removing "product_procurement_sla" value and will check the Adjusted R-squared value
    DL_HA_model_10 <- lm(formula = gmv ~ units + product_mrp + special_sale_day.xDaussera, data = DL_HA_data)
    
    
    summary(DL_HA_model_10) # Slight change in Adjusted R-squared value at 3rd place of decimal
    vif(DL_HA_model_10)
    
    
  ## Removing "product_mrp" value and will check the Adjusted R-squared value
    DL_HA_model_11 <- lm(formula = gmv ~ units +  special_sale_day.xDaussera, data = DL_HA_data)
    
    
    summary(DL_HA_model_11) # Slight change in Adjusted R-squared value at 3rd place of decimal
    vif(DL_HA_model_11)
    
    
    
### Cross-validation
  cv.lm(data = DL_HA_data, form.lm = DL_HA_model_11, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
    
    
    
### Estimating the elasticity coefficients
    
  elasticity_11 <- function(var){
      DL_HA_elasticity <- as.numeric(DL_HA_model_11$coefficients[var]*mean(DL_HA_data[,var])/mean(DL_HA_data$gmv))
      return(DL_HA_elasticity)
      
  } 
    
  DL_HA_var_list <- list()
    
  for(i in 2:length(DL_HA_model_11$coefficients)){
      DL_HA_var_list[i-1] <- elasticity_11(names(DL_HA_model_11$coefficients)[i])
      
  }
    
  DL_HA_elasticity.outputs <- data.frame(names(DL_HA_model_11$coefficients[2:length(DL_HA_model_11$coefficients)]))
  DL_HA_elasticity.outputs <- cbind(DL_HA_elasticity.outputs,do.call(rbind.data.frame, DL_HA_var_list))
  colnames(DL_HA_elasticity.outputs) <- c("Variable","Elasticity")
    
  DL_HA_elasticity.outputs$Direction <- ifelse(DL_HA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

    
  # Plotting the elasticity
    ggplot(DL_HA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    ggtitle("HomeAudio - Distributive Lag Model") +xlab("Variables")  
    

    
    
    
#********************************************************[Multiplicative + Distributive Lag Model]
    
### Preparing dataset
  ## Removing lag variables and Moving averages variables but considering the 1, 2 and 3 weeks lag value of 'gmv'
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    MD_HA_data <- HomeAudio_final[,-c(21:22,63:74)]
    
  ## Replacing 0 value in column with '0.00001' as log(0) is undefined
    MD_HA_data[MD_HA_data == 0] <- 0.00001
    
  ## Tranforming the negative values
    MD_HA_data$GMV_lag_1_per <- 1 + MD_HA_data$GMV_lag_1_per - min(MD_HA_data$GMV_lag_1_per)
    MD_HA_data$GMV_lag_2_per <- 1 + MD_HA_data$GMV_lag_2_per - min(MD_HA_data$GMV_lag_2_per)
    MD_HA_data$GMV_lag_3_per <- 1 + MD_HA_data$GMV_lag_3_per - min(MD_HA_data$GMV_lag_3_per)
    
  ## Taking log of all the variable to buils to Multiplicative model
    MD_HA_data <- log(MD_HA_data)
    
  ## Checking the variables for linear relationship or multicollinearity
    MD_HA_model <- lm(gmv~.,MD_HA_data)
    alias(MD_HA_model)
    
  ## Removing the variables which were showing linear relationship or multicollinearity
    MD_HA_data <- MD_HA_data[, -c(51:63)]
    
    
### Stepwise Regression to remove insignificant and correlated variables
  MD_HA_base.mod <- lm(gmv ~ 1 , data= MD_HA_data)  # base intercept only model
  MD_HA_all.mod <- lm(gmv ~ . , data= MD_HA_data) # full model with all predictors
  MD_HA_stepMod <- step(MD_HA_base.mod, scope = list(lower = MD_HA_base.mod, upper = MD_HA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  MD_HA_shortlistedVars <- names(unlist(MD_HA_stepMod[[1]])) # get the shortlisted variable.
  MD_HA_shortlistedVars <- MD_HA_shortlistedVars[!MD_HA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables [using MD_HA_stepMod]
    MD_HA_model_1 <- lm(formula = gmv ~ units + product_mrp + deliverybdays + Digital_adstock + 
                          product_procurement_sla + wday.xSunday + TV_adstock + Digital + 
                          product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer + 
                          product_analytic_vertical.xSlingBox + wday.xTuesday + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xDJController + wday.xWednesday + 
                          product_analytic_vertical.xHiFiSystem + Sponsorship + holiday_count + 
                          week + Affiliates + product_analytic_vertical.xVoiceRecorder + 
                          Sponsorship_adstock + Content_Marketing_adstock + special_sale_day.xChristmas...New.Year + 
                          Radio + Online_Marketing_adstock + Other + product_analytic_vertical.xSoundMixer, 
                        data = MD_HA_data)
    
    
    summary(MD_HA_model_1)
    vif(MD_HA_model_1)
    
    
  ## Insignificant p-value columns: product_analytic_vertical.xSoundMixer, product_analytic_vertical.xSlingBox
  ## High VIF and insignifiacnt p-value columns: units, product_analytic_vertical.xFMRadio, product_analytic_vertical.xHiFiSystem
    MD_HA_model_2 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock + 
                          product_procurement_sla + wday.xSunday + TV_adstock + Digital + 
                          product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer + 
                          wday.xTuesday + product_analytic_vertical.xDJController + wday.xWednesday + 
                          Sponsorship + holiday_count + week + Affiliates + product_analytic_vertical.xVoiceRecorder + 
                          Sponsorship_adstock + Content_Marketing_adstock + special_sale_day.xChristmas...New.Year + 
                          Radio + Online_Marketing_adstock + Other, data = MD_HA_data)
    
    
    summary(MD_HA_model_2)
    vif(MD_HA_model_2)
    
    
  ## High VIF and Insignificant p-value columns: wday.xTuesday
  ## Less significant p-value columns: week, product_analytic_vertical.xDJController
  ## High VIF value columns: Online_Marketing_adstock
    MD_HA_model_3 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock + 
                          product_procurement_sla + wday.xSunday + TV_adstock + Digital + 
                          product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer + 
                          wday.xWednesday + Sponsorship + holiday_count + Affiliates + 
                          product_analytic_vertical.xVoiceRecorder + Sponsorship_adstock + Content_Marketing_adstock + 
                          special_sale_day.xChristmas...New.Year + Radio + Other, data = MD_HA_data)
    
    
    summary(MD_HA_model_3)
    vif(MD_HA_model_3)
    
    
  ## High VIF and Insignificant p-value columns: product_analytic_vertical.xVoiceRecorder
    MD_HA_model_4 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock + 
                          product_procurement_sla + wday.xSunday + TV_adstock + Digital + 
                          product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer + 
                          wday.xWednesday + Sponsorship + holiday_count + Affiliates + 
                          Sponsorship_adstock + Content_Marketing_adstock + 
                          special_sale_day.xChristmas...New.Year + Radio + Other, data = MD_HA_data)
    
    
    summary(MD_HA_model_4)
    vif(MD_HA_model_4)
    
    
  ## Insignificant p-value columns: holiday_count
  ## High VIF value columns: Other
    MD_HA_model_5 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock + 
                          product_procurement_sla + wday.xSunday + TV_adstock + Digital + 
                          product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer + 
                          wday.xWednesday + Sponsorship + Affiliates + Sponsorship_adstock + Content_Marketing_adstock + 
                          special_sale_day.xChristmas...New.Year + Radio , data = MD_HA_data)
    
    
    summary(MD_HA_model_5)
    vif(MD_HA_model_5)
    
    
  ## Insignificant p-value columns: Radio
    MD_HA_model_6 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock + 
                          product_procurement_sla + wday.xSunday + TV_adstock + Digital + 
                          product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer + 
                          wday.xWednesday + Sponsorship + Affiliates + Sponsorship_adstock + Content_Marketing_adstock + 
                          special_sale_day.xChristmas...New.Year, data = MD_HA_data)
    
    
    summary(MD_HA_model_6)
    vif(MD_HA_model_6)
    
    
  ## Less significant p-value columns: product_analytic_vertical.xKaraokePlayer
  ## Slightly High VIF and Less significant p-value columns: TV_adstock
    MD_HA_model_7 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock + 
                          product_procurement_sla + wday.xSunday + Digital + 
                          product_analytic_vertical.xDockingStation + sla + wday.xWednesday + 
                          Sponsorship + Affiliates + Sponsorship_adstock + Content_Marketing_adstock + 
                          special_sale_day.xChristmas...New.Year, data = MD_HA_data)
    
    
    summary(MD_HA_model_7)
    vif(MD_HA_model_7)
    
    
  ## Less significant p-value columns: product_analytic_vertical.xDockingStation
  ## High VIF value columns: Sponsorship_adstock
    MD_HA_model_8 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock + 
                          product_procurement_sla + wday.xSunday + Digital + sla + 
                          wday.xWednesday + Sponsorship + Affiliates + Content_Marketing_adstock + 
                          special_sale_day.xChristmas...New.Year, data = MD_HA_data)
    
    summary(MD_HA_model_8)
    vif(MD_HA_model_8)
    
    
  ## Slightly High VIF and Insignificant p-value columns: Digital_adstock
  ## Less significant p-value columns: Digital, Sponsorship
    MD_HA_model_9 <- lm(formula = gmv ~ product_mrp + deliverybdays + 
                          product_procurement_sla + wday.xSunday + sla + 
                          wday.xWednesday + Affiliates + Content_Marketing_adstock + 
                          special_sale_day.xChristmas...New.Year, data = MD_HA_data)
    
    
    summary(MD_HA_model_9)
    vif(MD_HA_model_9)
    
    
  ## High VIF value columns: wday.xWednesday
    MD_HA_model_10 <- lm(formula = gmv ~ product_mrp + deliverybdays + product_procurement_sla + 
                           wday.xSunday + sla + Affiliates + Content_Marketing_adstock + 
                           special_sale_day.xChristmas...New.Year, data = MD_HA_data)
    
    
    summary(MD_HA_model_10)
    vif(MD_HA_model_10)
    
    
  ## Insignificant p-value columns: Content_Marketing_adstock
    MD_HA_model_11 <- lm(formula = gmv ~ product_mrp + deliverybdays + product_procurement_sla + 
                           wday.xSunday + sla + Affiliates + special_sale_day.xChristmas...New.Year, data = MD_HA_data)
    
    
    summary(MD_HA_model_11)
    vif(MD_HA_model_11)
    
    
  ## Insignificant p-value columns: special_sale_day.xChristmas...New.Year
    MD_HA_model_12 <- lm(formula = gmv ~ product_mrp + deliverybdays + product_procurement_sla + 
                           wday.xSunday + sla + Affiliates, data = MD_HA_data)
    
    
    summary(MD_HA_model_12)
    vif(MD_HA_model_12)
    
    
  ## Insignificant p-value columns: wday.xSunday
    MD_HA_model_13 <- lm(formula = gmv ~ product_mrp + deliverybdays + product_procurement_sla + 
                           sla + Affiliates, data = MD_HA_data)
    
    
    summary(MD_HA_model_13)
    vif(MD_HA_model_13)
    
    
  ## Insignificant p-value columns: Affiliates
    MD_HA_model_14 <- lm(formula = gmv ~ product_mrp + deliverybdays + product_procurement_sla + 
                           sla, data = MD_HA_data)
    
    
    summary(MD_HA_model_14)
    vif(MD_HA_model_14)
    
    
  ## Removing 'product_procurement_sla' variable [as it's having less significant p-value]
    MD_HA_model_15 <- lm(formula = gmv ~ product_mrp + deliverybdays + sla, data = MD_HA_data)
    
    
    summary(MD_HA_model_15)
    vif(MD_HA_model_15)
    
    
    
### Cross-validation
  cv.lm(data = MD_HA_data, form.lm = MD_HA_model_15, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
    
    
    
### Estimating the elasticity coefficients
    
  elasticity_14 <- function(var){
      MD_HA_elasticity <- as.numeric(MD_HA_model_15$coefficients[var]*mean(MD_HA_data[,var])/mean(MD_HA_data$gmv))
      return(MD_HA_elasticity)
      
  } 
    
  MD_HA_var_list <- list()
    
  for(i in 2:length(MD_HA_model_15$coefficients)){
      MD_HA_var_list[i-1] <- elasticity_14(names(MD_HA_model_15$coefficients)[i])
      
  }
    
  MD_HA_elasticity.outputs <- data.frame(names(MD_HA_model_15$coefficients[2:length(MD_HA_model_15$coefficients)]))
  MD_HA_elasticity.outputs <- cbind(MD_HA_elasticity.outputs,do.call(rbind.data.frame, MD_HA_var_list))
  colnames(MD_HA_elasticity.outputs) <- c("Variable","Elasticity")
    
  MD_HA_elasticity.outputs$Direction <- ifelse(MD_HA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

    
  # Plotting the elasticity
    ggplot(MD_HA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    ggtitle("HomeAudio - Multiplicative and Distributive Lag Model") +xlab("Variables")  
    
    