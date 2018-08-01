
################################################################################################################################################################
#                                           :::::::: Model Building [GamingAccessory] ::::::::
################################################################################################################################################################

#********************************************************[Linear Regression Model]
### Preparing dataset
  ## Removing lag variables and Moving averages variables
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    LR_GA_data <- GamingAccessory_final[,-c(21:22,68:82)]
     
  ## Scaling the variables
    LR_GA_data[,2:ncol(LR_GA_data)] <- scale(LR_GA_data[,2:ncol(LR_GA_data)])

### Stepwise Regression to remove insignificant and correlated variables
  LR_GA_base.mod <- lm(gmv ~ 1 , data= LR_GA_data)  # base intercept only model
  LR_GA_all.mod <- lm(gmv ~ . , data= LR_GA_data) # full model with all predictors
  LR_GA_stepMod <- step(LR_GA_base.mod, scope = list(lower = LR_GA_base.mod, upper = LR_GA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  LR_GA_shortlistedVars <- names(unlist(LR_GA_stepMod[[1]])) # get the shortlisted variable.
  LR_GA_shortlistedVars <- LR_GA_shortlistedVars[!LR_GA_shortlistedVars %in% "(Intercept)"]  # remove intercept

  
### Model Building::

  ## Building First model after short listing the variables [using LR_GA_stepMod]
    LR_GA_model_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + 
                          product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + Other_adstock + Affiliates + 
                          wday.xMonday + Online_Marketing + special_sale_day.xRepublic.Day + 
                          product_analytic_vertical.xGamingKeyboard + special_sale_day.xFHSD + 
                          wday.xTuesday + special_sale_day.xDiwali + special_sale_day.xPacman + 
                          Content_Marketing_adstock + wday.xWednesday + product_procurement_sla, 
                          data = LR_GA_data)
    
    
    summary(LR_GA_model_1)
    vif(LR_GA_model_1)
    
    
  ## Insignificant p-value columns: product_analytic_vertical.xMotionController, special_sale_day.xFHSD, product_procurement_sla
    LR_GA_model_2 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type + 
                       product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                       special_sale_day.xEid...Rathayatra + Other_adstock + Affiliates + 
                       wday.xMonday + Online_Marketing + special_sale_day.xRepublic.Day + 
                       product_analytic_vertical.xGamingKeyboard + 
                       wday.xTuesday + special_sale_day.xDiwali + special_sale_day.xPacman + 
                       Content_Marketing_adstock + wday.xWednesday, data = LR_GA_data)
    
    
    summary(LR_GA_model_2)
    vif(LR_GA_model_2)
    
    
  ## Insignificant p-value columns: special_sale_day.xDiwali
  ## Less Significant p-value columns: wday.xTuesday
    LR_GA_model_3 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + Other_adstock + Affiliates + 
                          wday.xMonday + Online_Marketing + special_sale_day.xRepublic.Day + 
                          product_analytic_vertical.xGamingKeyboard + special_sale_day.xPacman + 
                          Content_Marketing_adstock + wday.xWednesday, data = LR_GA_data)
    
    
    summary(LR_GA_model_3)
    vif(LR_GA_model_3)
    
    
  ## Insignificant p-value columns: Content_Marketing_adstock
  ## Less Significant p-value columns: wday.xWednesday
    LR_GA_model_4 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + Other_adstock + Affiliates + 
                          wday.xMonday + Online_Marketing + special_sale_day.xRepublic.Day + 
                          product_analytic_vertical.xGamingKeyboard + special_sale_day.xPacman, data = LR_GA_data)
    
    
    summary(LR_GA_model_4)
    vif(LR_GA_model_4)
    
    
  ## High VIF and Less Significant p-value columns: Online_Marketing
  ## Less Significant p-value columns: product_analytic_vertical.xGamingKeyboard, special_sale_day.xRepublic.Day 
    LR_GA_model_5 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + Other_adstock + Affiliates + 
                          wday.xMonday + special_sale_day.xPacman, data = LR_GA_data)
    
    
    summary(LR_GA_model_5)
    vif(LR_GA_model_5)
    
    
  ## Less Significant p-value columns: special_sale_day.xPacman, product_analytic_vertical.xGamingAccessoryKit
    LR_GA_model_6 <- lm(formula = gmv ~ units + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + Other_adstock + Affiliates + 
                          wday.xMonday, data = LR_GA_data)
    
    
    summary(LR_GA_model_6)
    vif(LR_GA_model_6)
    
    
  ## Less Significant p-value columns: Affiliates
    LR_GA_model_7 <- lm(formula = gmv ~ units + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + Other_adstock + wday.xMonday, data = LR_GA_data)
    
    
    summary(LR_GA_model_7)
    vif(LR_GA_model_7)
    
 
  ## Less Significant p-value columns: wday.xMonday
    LR_GA_model_8 <- lm(formula = gmv ~ units + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + Other_adstock, data = LR_GA_data)
    
    
    summary(LR_GA_model_8)
    vif(LR_GA_model_8)
    
  ## Trying to remove "special_sale_day.xEid...Rathayatra" variable (as it is having high p-value among the variables)
  ## and then see the variation in Adjusted R-squared
    LR_GA_model_9 <- lm(formula = gmv ~ units + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          Other_adstock, data = LR_GA_data)
    
    
    summary(LR_GA_model_9) # After removing the variable very less change in Adjusted R-squared, we're good to go with this model
    vif(LR_GA_model_9)
    
    
    
### Cross-validation
  cv.lm(data = LR_GA_data, form.lm = LR_GA_model_9, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

    
    
### Estimating the elasticity coefficients
    
  elasticity_1 <- function(var){
      LR_GA_elasticity <-as.numeric(LR_GA_model_9$coefficients[var]*mean(LR_GA_data[,var])/mean(LR_GA_data$gmv))
      return(LR_GA_elasticity)
      
    } 
    
  LR_HA_var_list <- list()
    
  for(i in 2:length(LR_GA_model_9$coefficients)){
    LR_HA_var_list[i-1] <-elasticity_1(names(LR_GA_model_9$coefficients)[i])
      
  }
    
  LR_GA_elasticity.outputs <- data.frame(names(LR_GA_model_9$coefficients[2:length(LR_GA_model_9$coefficients)]))
  LR_GA_elasticity.outputs <- cbind(LR_GA_elasticity.outputs,do.call(rbind.data.frame, LR_HA_var_list))
  colnames(LR_GA_elasticity.outputs) <- c("Variable","Elasticity")
    
  LR_GA_elasticity.outputs$Direction <- ifelse(LR_GA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")
    
  # Plotting the elasticity
    ggplot(LR_GA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-3),hjust = 1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank()) +
    ggtitle("GamingAccessory - Linear Regression Model") +xlab("Variables")
  
 
     
    
#********************************************************[Multiplicative Model]
    
### Preparing dataset
  ## Removing lag variables and Moving averages variables
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    MM_GA_data <- GamingAccessory_final[,-c(21:22,68:82)]
    
  ## Replacing 0 value in column with '0.00001' as log(0) is undefined
    MM_GA_data[MM_GA_data == 0] <- 0.00001
    
  ## Taking log of all the variable to buils to Multiplicative model
    MM_GA_data <- log(MM_GA_data)
    
    
### Stepwise Regression to remove insignificant and correlated variables
  MM_GA_base.mod <- lm(gmv ~ 1 , data= MM_GA_data)  # base intercept only model
  MM_GA_all.mod <- lm(gmv ~ . , data= MM_GA_data) # full model with all predictors
  MM_GA_stepMod <- step(MM_GA_base.mod, scope = list(lower = MM_GA_base.mod, upper = MM_GA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  MM_GA_shortlistedVars <- names(unlist(MM_GA_stepMod[[1]])) # get the shortlisted variable.
  MM_GA_shortlistedVars <- MM_GA_shortlistedVars[!MM_GA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables [using MM_GA_stepMod]
    MM_GA_model_1 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                          Other + special_sale_day.xEid...Rathayatra + product_analytic_vertical.xMotionController + 
                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset + 
                          special_sale_day.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker + 
                          special_sale_day.xChristmas...New.Year + Content_Marketing_adstock + 
                          Content_Marketing + deliverybdays + product_analytic_vertical.xGamingAdapter + 
                          sla + Digital_adstock + product_analytic_vertical.xGameControlMount + 
                          product_analytic_vertical.xGamingKeyboard + special_sale_day.xBSD + 
                          Other_adstock + product_analytic_vertical.xGamingMemoryCard + 
                          special_sale_day.xPacman + product_analytic_vertical.xTVOutCableAccessory + 
                          Digital, data = MM_GA_data)
    
    summary(MM_GA_model_1)
    vif(MM_GA_model_1)
    
    
  ## High VIF and insignificant p-value columns: Digital, product_analytic_vertical.xGamingMemoryCard, product_analytic_vertical.xTVOutCableAccessory 
  ## Insignificant p-value columns: special_sale_day.xPacman
    MM_GA_model_2 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                          Other + special_sale_day.xEid...Rathayatra + product_analytic_vertical.xMotionController + 
                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset + 
                          special_sale_day.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker + 
                          special_sale_day.xChristmas...New.Year + Content_Marketing_adstock + 
                          Content_Marketing + deliverybdays + product_analytic_vertical.xGamingAdapter + 
                          sla + Digital_adstock + product_analytic_vertical.xGameControlMount + 
                          product_analytic_vertical.xGamingKeyboard + special_sale_day.xBSD + Other_adstock, data = MM_GA_data)
    
    summary(MM_GA_model_2)
    vif(MM_GA_model_2)
    
  ## High VIF and Less significant p-value columns: Other_adstock
    MM_GA_model_3 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                          Other + special_sale_day.xEid...Rathayatra + product_analytic_vertical.xMotionController + 
                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset + 
                          special_sale_day.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker + 
                          special_sale_day.xChristmas...New.Year + Content_Marketing_adstock + 
                          Content_Marketing + deliverybdays + product_analytic_vertical.xGamingAdapter + 
                          sla + Digital_adstock + product_analytic_vertical.xGameControlMount + 
                          product_analytic_vertical.xGamingKeyboard + special_sale_day.xBSD, data = MM_GA_data)
    
    summary(MM_GA_model_3)
    vif(MM_GA_model_3)
    
    
  ## Isignificant p-value columns: special_sale_day.xBSD
  ## High VIF and Less significant p-value columns: product_analytic_vertical.xGamingKeyboard
    MM_GA_model_4 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                          Other + special_sale_day.xEid...Rathayatra + product_analytic_vertical.xMotionController + 
                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset + 
                          special_sale_day.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker + 
                          special_sale_day.xChristmas...New.Year + Content_Marketing_adstock + 
                          Content_Marketing + deliverybdays + product_analytic_vertical.xGamingAdapter + 
                          sla + Digital_adstock + product_analytic_vertical.xGameControlMount, data = MM_GA_data)
    
    summary(MM_GA_model_4)
    vif(MM_GA_model_4)
    
    
  ## Less significant p-vale columns: product_analytic_vertical.xGameControlMount, pecial_sale_day.xChristmas...New.Year
    MM_GA_model_5 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                          Other + special_sale_day.xEid...Rathayatra + product_analytic_vertical.xMotionController + 
                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset + 
                          special_sale_day.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker + 
                          Content_Marketing_adstock + Content_Marketing + deliverybdays + product_analytic_vertical.xGamingAdapter + 
                          sla + Digital_adstock, data = MM_GA_data)
    
    summary(MM_GA_model_5)
    vif(MM_GA_model_5)
    
    
  ## Slightly high VIF and Insignificant p-value columns: deliverybdays
    MM_GA_model_6 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                          Other + special_sale_day.xEid...Rathayatra + product_analytic_vertical.xMotionController + 
                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset + 
                          special_sale_day.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker + 
                          Content_Marketing_adstock + Content_Marketing + product_analytic_vertical.xGamingAdapter + 
                          sla + Digital_adstock, data = MM_GA_data)
    
    summary(MM_GA_model_6)
    vif(MM_GA_model_6)
    
    
  ## High VIF and Insignificant p-value columns: wday.xThursday, Content_Marketing_adstock
    MM_GA_model_7 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                          Other + special_sale_day.xEid...Rathayatra + product_analytic_vertical.xMotionController + 
                          wday.xMonday + product_analytic_vertical.xGamingHeadset + 
                          special_sale_day.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker + 
                          Content_Marketing + product_analytic_vertical.xGamingAdapter + 
                          sla + Digital_adstock, data = MM_GA_data)
    
    summary(MM_GA_model_7)
    vif(MM_GA_model_7)
    
    
  ## Slightly high VIF and insignificant p-value columns: wday.xSaturday 
  ## Less significant p-value columns: product_analytic_vertical.xGamingSpeaker
    MM_GA_model_8 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                          Other + special_sale_day.xEid...Rathayatra + product_analytic_vertical.xMotionController + 
                          wday.xMonday + product_analytic_vertical.xGamingHeadset + special_sale_day.xValentine.Day + 
                          Content_Marketing + product_analytic_vertical.xGamingAdapter + 
                          sla + Digital_adstock, data = MM_GA_data)
    
    summary(MM_GA_model_8)
    vif(MM_GA_model_8)
    
    
  ## Less significant p-value columns: special_sale_day.xValentine.Day, Digital_adstock
  ## High VIF and less significant p-value columns: wday.xMonday
    MM_GA_model_9 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                          Other + special_sale_day.xEid...Rathayatra + product_analytic_vertical.xMotionController + 
                          product_analytic_vertical.xGamingHeadset + 
                          Content_Marketing + product_analytic_vertical.xGamingAdapter + sla, data = MM_GA_data)
    
    summary(MM_GA_model_9)
    vif(MM_GA_model_9)
    
    
  ## Less significant p-value columns: Content_Marketing
    MM_GA_model_10 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                           product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                           Other + special_sale_day.xEid...Rathayatra + product_analytic_vertical.xMotionController + 
                           product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingAdapter + sla, data = MM_GA_data)
    
    summary(MM_GA_model_10)
    vif(MM_GA_model_10)
    
    
  ## Insignificant p-value columns: special_sale_day.xEid...Rathayatra
    MM_GA_model_11 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                           product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                           Other + product_analytic_vertical.xMotionController + 
                           product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingAdapter + sla, data = MM_GA_data)
    
    summary(MM_GA_model_11)
    vif(MM_GA_model_11)
    
    
  ## Insignificant p-value columns: product_analytic_vertical.xGamingHeadset 
    MM_GA_model_12 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                           product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                           Other + product_analytic_vertical.xMotionController + 
                           product_analytic_vertical.xGamingAdapter + sla, data = MM_GA_data)
    
    summary(MM_GA_model_12)
    vif(MM_GA_model_12)
    
    
  ## Slightly high VIF and Insignificant p-value columns: sla, Other
    MM_GA_model_13 <- lm(formula = gmv ~ units + per_order + special_sale_day.xRakshabandhan + 
                           product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xMotionController + product_analytic_vertical.xGamingAdapter, data = MM_GA_data)
    
    summary(MM_GA_model_13)
    vif(MM_GA_model_13)
    
    
  ## High VIF columns: special_sale_day.xRakshabandhan
    MM_GA_model_14 <- lm(formula = gmv ~ units + per_order + 
                           product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xMotionController + product_analytic_vertical.xGamingAdapter, data = MM_GA_data)
    
    summary(MM_GA_model_14) ## Adjusted R-squared very slightly changes (at third decimal place)
    vif(MM_GA_model_14)
    
    
  ## High VIF and insignificant p-value columns: product_analytic_vertical.xGamingAdapter, product_analytic_vertical.xMotionController
  ##                                             product_analytic_vertical.xGamingAccessoryKit
    MM_GA_model_15 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xJoystickGamingWheel + 
                           s1_fact.order_payment_type + product_analytic_vertical.xGamePad, data = MM_GA_data)
    
    summary(MM_GA_model_15)
    vif(MM_GA_model_15)
    
    
  ## High VIF columns: product_analytic_vertical.xGamePad, product_analytic_vertical.xJoystickGamingWheel, s1_fact.order_payment_type
    MM_GA_model_16 <- lm(formula = gmv ~ units + per_order, data = MM_GA_data)
    
    summary(MM_GA_model_16)
    vif(MM_GA_model_16)
    
    
    
### Cross-validation
    cv.lm(data = MM_GA_data, form.lm = MM_GA_model_16, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
    
    
    
### Estimating the elasticity coefficients
    
  elasticity_4 <- function(var){
      MM_GA_elasticity <-as.numeric(MM_GA_model_16$coefficients[var]*mean(MM_GA_data[,var])/mean(MM_GA_data$gmv))
      return(MM_GA_elasticity)
      
  } 
    
  MM_GA_var_list <- list()
    
  for(i in 2:length(MM_GA_model_16$coefficients)){
      MM_GA_var_list[i-1] <- elasticity_4(names(MM_GA_model_16$coefficients)[i])
      
  }
    
  MM_GA_elasticity.outputs <- data.frame(names(MM_GA_model_16$coefficients[2:length(MM_GA_model_16$coefficients)]))
  MM_GA_elasticity.outputs <- cbind(MM_GA_elasticity.outputs,do.call(rbind.data.frame, MM_GA_var_list))
  colnames(MM_GA_elasticity.outputs) <- c("Variable","Elasticity")
    
  MM_GA_elasticity.outputs$Direction <- ifelse(MM_GA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")
    
  # Plotting the elasticity
    ggplot(MM_GA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5), color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank()) +
    ggtitle("GamingAccessory - Multiplicative Model") +xlab("Variables")  
    
    


        
#********************************************************[Koyck Model]
    
### Preparing dataset
  ## Removing lag variables and Moving averages variables but considering the 1 week lag value of 'gmv'
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    KM_GA_data <- GamingAccessory_final[,-c(21:22,68:79, 81:82)]
    
  ## Scaling the variables
    KM_GA_data[,2:ncol(KM_GA_data)] <- scale(KM_GA_data[,2:ncol(KM_GA_data)])
    
### Stepwise Regression to remove insignificant and correlated variables
  KM_GA_base.mod <- lm(gmv ~ 1 , data= KM_GA_data)  # base intercept only model
  KM_GA_all.mod <- lm(gmv ~ . , data= KM_GA_data) # full model with all predictors
  KM_GA_stepMod <- step(KM_GA_base.mod, scope = list(lower =KM_GA_base.mod, upper = KM_GA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  KM_GA_shortlistedVars <- names(unlist(KM_GA_stepMod[[1]])) # get the shortlisted variable.
  KM_GA_shortlistedVars <- KM_GA_shortlistedVars[!KM_GA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables [using KM_GA_stepMod]
    KM_GA_model_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + 
                          product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + GMV_lag_1_per + Other_adstock + 
                          wday.xTuesday + Affiliates + special_sale_day.xDiwali + special_sale_day.xRepublic.Day + 
                          special_sale_day.xFHSD + wday.xMonday + Online_Marketing, 
                        data = KM_GA_data)
    
    
    summary(KM_GA_model_1)
    vif(KM_GA_model_1)
    
    
  ## High VIF and Insignificant p-value columns:  Online_Marketing, product_analytic_vertical.xGamingAccessoryKit 
  ## Insignificant p-value columns: wday.xMonday
    KM_GA_model_2 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + 
                          s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + GMV_lag_1_per + Other_adstock + 
                          wday.xTuesday + Affiliates + special_sale_day.xDiwali + special_sale_day.xRepublic.Day + 
                          special_sale_day.xFHSD, data = KM_GA_data)
    
    
    summary(KM_GA_model_2)
    vif(KM_GA_model_2)
    
    
  ## Insignificant p-value columns: special_sale_day.xRepublic.Day, special_sale_day.xFHSD, special_sale_day.xDiwali
  ## Less significant p-value columns: Affiliates
    KM_GA_model_3 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + 
                          s1_fact.order_payment_type + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xJoystickGamingWheel + special_sale_day.xEid...Rathayatra + 
                          GMV_lag_1_per + Other_adstock + wday.xTuesday, data = KM_GA_data)
    
    
    summary(KM_GA_model_3)
    vif(KM_GA_model_3)
    
    
  ## Insignificant p-value columns: wday.xTuesday
    KM_GA_model_4 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + 
                          s1_fact.order_payment_type + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xJoystickGamingWheel + special_sale_day.xEid...Rathayatra + 
                          GMV_lag_1_per + Other_adstock, data = KM_GA_data)
    
    
    summary(KM_GA_model_4)
    vif(KM_GA_model_4)
    
    
  ## Let's try to remove "s1_fact.order_payment_type" variable and then see if Adjusted R-squared vary much or not
    KM_GA_model_5 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + GMV_lag_1_per + Other_adstock, data = KM_GA_data)
    
    
    summary(KM_GA_model_5) # Good to remove that, Adjusted R-squared decreased at third place of decimal.
    vif(KM_GA_model_5)
    
    
  ## Again, Let's try to remove "product_analytic_vertical.xMotionController" variable and then see if Adjusted R-squared vary much or not
    KM_GA_model_6 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + GMV_lag_1_per + Other_adstock, data = KM_GA_data)
    
    
    summary(KM_GA_model_6) # Good to remove that, slight change in Adjusted R-squared
    vif(KM_GA_model_6)
    
    
  ## Again, Let's try to remove "Other_adstock" variable and then see if Adjusted R-squared vary much or not
    KM_GA_model_7 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + GMV_lag_1_per, data = KM_GA_data)
    
    
    summary(KM_GA_model_7) # Good to remove that, slight change in Adjusted R-squared
    vif(KM_GA_model_7)
    
    
  ## Removing "special_sale_day.xEid...Rathayatra" variable and will check Adjusted R-squared
    KM_GA_model_8 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          GMV_lag_1_per, data = KM_GA_data)
    
    
    summary(KM_GA_model_8) # Slight change in Adjusted R-squared value
    vif(KM_GA_model_8)
    
    
  ## Less significant p-value columns: GMV_lag_1_per
    KM_GA_model_9 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xJoystickGamingWheel, data = KM_GA_data)
    
    
    summary(KM_GA_model_9) # Slight change in Adjusted R-squared value but mean squared error also decreased
    vif(KM_GA_model_9)
    
    
    
### Cross-validation
  cv.lm(data = KM_GA_data, form.lm = KM_GA_model_9, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
    
    
    
### Estimating the elasticity coefficients
    
  elasticity_7 <- function(var){
      KM_GA_elasticity <- as.numeric(KM_GA_model_9$coefficients[var]*mean(KM_GA_data[,var])/mean(KM_GA_data$gmv))
      return(KM_GA_elasticity)
      
  } 
    
  KM_GA_var_list <- list()
    
  for(i in 2:length(KM_GA_model_9$coefficients)){
      KM_GA_var_list[i-1] <- elasticity_7(names(KM_GA_model_9$coefficients)[i])
      
  }
    
  KM_GA_elasticity.outputs <- data.frame(names(KM_GA_model_9$coefficients[2:length(KM_GA_model_9$coefficients)]))
  KM_GA_elasticity.outputs <- cbind(KM_GA_elasticity.outputs,do.call(rbind.data.frame, KM_GA_var_list))
  colnames(KM_GA_elasticity.outputs) <- c("Variable","Elasticity")
    
  KM_GA_elasticity.outputs$Direction <- ifelse(KM_GA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

    
  # Plotting the elasticity
    ggplot(KM_GA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    ggtitle("GamingAccessory - Koyck Model") +xlab("Variables")  
    
    
    
#********************************************************[Distributive Lag Model]
    
### Preparing dataset
  ## Removing lag variables and Moving averages variables but considering the 1, 2 and 3 weeks lag value of 'gmv'
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    DL_GA_data <- GamingAccessory_final[,-c(21:22,68:79)]
    
  ## Scaling the variables
    DL_GA_data[,2:ncol(DL_GA_data)] <- scale(DL_GA_data[,2:ncol(DL_GA_data)])
    
### Stepwise Regression to remove insignificant and correlated variables
  DL_GA_base.mod <- lm(gmv ~ 1 , data= DL_GA_data)  # base intercept only model
  DL_GA_all.mod <- lm(gmv ~ . , data= DL_GA_data) # full model with all predictors
  DL_GA_stepMod <- step(DL_GA_base.mod, scope = list(lower =DL_GA_base.mod, upper = DL_GA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  DL_GA_shortlistedVars <- names(unlist(DL_GA_stepMod[[1]])) # get the shortlisted variable.
  DL_GA_shortlistedVars <- DL_GA_shortlistedVars[!DL_GA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables [using DL_GA_stepMod]
    DL_GA_model_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + 
                          product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + GMV_lag_1_per + Other_adstock + 
                          wday.xTuesday + Affiliates + special_sale_day.xDiwali + special_sale_day.xRepublic.Day + 
                          special_sale_day.xFHSD + wday.xMonday + Online_Marketing, 
                        data = DL_GA_data)
    
    summary(DL_GA_model_1)
    vif(DL_GA_model_1)
    
    
  ## High VIF and Insignificant p-value columns: Online_Marketing, product_analytic_vertical.xGamingAccessoryKit 
  ## Insignificant p-value columns: wday.xMonday, special_sale_day.xRepublic.Day, special_sale_day.xFHSD, special_sale_day.xDiwali
    DL_GA_model_2 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + GMV_lag_1_per + Other_adstock + 
                          wday.xTuesday + Affiliates, data = DL_GA_data)
    
    summary(DL_GA_model_2)
    vif(DL_GA_model_2)
    
    
  ## Less significant p-value columns: Affiliates, wday.xTuesday 
    DL_GA_model_3 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + GMV_lag_1_per + Other_adstock, data = DL_GA_data)
    
    summary(DL_GA_model_3)
    vif(DL_GA_model_3)
    
    
  ## Removing 'Other_adstock' column and then check whether Adjusted R-squared value is chanf=ging or not
    DL_GA_model_4 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel + 
                          special_sale_day.xEid...Rathayatra + GMV_lag_1_per, data = DL_GA_data)
    
    summary(DL_GA_model_4) # Adjusted R-squared is changes at third place of decimal i.e. very less change
    vif(DL_GA_model_4)
    
    
  ## Removing 'product_analytic_vertical.xJoystickGamingWheel' variable
    DL_GA_model_5 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + 
                          special_sale_day.xEid...Rathayatra + GMV_lag_1_per, data = DL_GA_data)
    
    summary(DL_GA_model_5)
    vif(DL_GA_model_5)
    
    
  ## Trying to remove "special_sale_day.xEid...Rathayatra" variable and will check the Adjusted R-squared value
    DL_GA_model_6 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad + GMV_lag_1_per, data = DL_GA_data)
    
    summary(DL_GA_model_6)  # Slight change in Adjusted R-squared value
    vif(DL_GA_model_6)
    
    
  ## Slightly less significant p-value columns: GMV_lag_1_per
    DL_GA_model_7 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type + 
                          product_analytic_vertical.xGamePad, data = DL_GA_data)
    
    summary(DL_GA_model_7)
    vif(DL_GA_model_7)
    
    
    
### Cross-validation
  cv.lm(data = DL_GA_data, form.lm = DL_GA_model_7, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
    
    
    
### Estimating the elasticity coefficients
    
  elasticity_10 <- function(var){
      DL_GA_elasticity <- as.numeric(DL_GA_model_7$coefficients[var]*mean(DL_GA_data[,var])/mean(DL_GA_data$gmv))
      return(DL_GA_elasticity)
      
  } 
    
  DL_GA_var_list <- list()
    
  for(i in 2:length(DL_GA_model_7$coefficients)){
      DL_GA_var_list[i-1] <- elasticity_10(names(DL_GA_model_7$coefficients)[i])
      
  }
    
  DL_GA_elasticity.outputs <- data.frame(names(DL_GA_model_7$coefficients[2:length(DL_GA_model_7$coefficients)]))
  DL_GA_elasticity.outputs <- cbind(DL_GA_elasticity.outputs,do.call(rbind.data.frame, DL_GA_var_list))
  colnames(DL_GA_elasticity.outputs) <- c("Variable","Elasticity")
    
  DL_GA_elasticity.outputs$Direction <- ifelse(DL_GA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")
  
    
  # Plotting the elasticity
    ggplot(DL_GA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    ggtitle("GamingAccessory - Distributive Lag Model") +xlab("Variables")  
    
    
    
    
    
#********************************************************[Multiplicative + DIstributive Lag Model]
    
### Preparing dataset
  ## Removing lag variables and Moving averages variables but considering the 1, 2 and 3 weeks lag value of 'gmv'
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    MD_GA_data <- GamingAccessory_final[,-c(21:22,68:79)]
    
  ## Replacing 0 value in column with '0.00001' as log(0) is undefined
    MD_GA_data[MD_GA_data == 0] <- 0.00001
    
  ## Tranforming the negative values
    MD_GA_data$GMV_lag_1_per <- 1 + MD_GA_data$GMV_lag_1_per - min(MD_GA_data$GMV_lag_1_per)
    MD_GA_data$GMV_lag_2_per <- 1 + MD_GA_data$GMV_lag_2_per - min(MD_GA_data$GMV_lag_2_per)
    MD_GA_data$GMV_lag_3_per <- 1 + MD_GA_data$GMV_lag_3_per - min(MD_GA_data$GMV_lag_3_per)
    
  ## Taking log of all the variable to buils to Multiplicative model
    MD_GA_data <- log(MD_GA_data)
    
  ## Checking the variables for linear relationship or multicollinearity
    MD_GA_model <- lm(gmv~.,MD_GA_data)
    alias(MD_GA_model)
    
  ## Removing the variables which were showing linear relationship or multicollinearity
    MD_GA_data <- MD_GA_data[, -c(55:68)]
    
    
### Stepwise Regression to remove insignificant and correlated variables
  MD_GA_base.mod <- lm(gmv ~ 1 , data= MD_GA_data)  # base intercept only model
  MD_GA_all.mod <- lm(gmv ~ . , data= MD_GA_data) # full model with all predictors
  MD_GA_stepMod <- step(MD_GA_base.mod, scope = list(lower = MD_GA_base.mod, upper = MD_GA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  MD_GA_shortlistedVars <- names(unlist(MD_GA_stepMod[[1]])) # get the shortlisted variable.
  MD_GA_shortlistedVars <- MD_GA_shortlistedVars[!MD_GA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables [using MD_GA_stepMod]
    MD_GA_model_1 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset + 
                          product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla + 
                          wday.xSunday + product_analytic_vertical.xMotionController + 
                          special_sale_day.xBSD + product_analytic_vertical.xGamingAccessoryKit + 
                          TV_adstock + product_analytic_vertical.xGamingMousePad + 
                          product_analytic_vertical.xGamingAdapter + sla + SEM_adtock + 
                          Other + SEM + week + product_analytic_vertical.xGamingMemoryCard + 
                          Total_Investment + holiday_count + is_special_sale_day + 
                          Content_Marketing, data = MD_GA_data)
    
    
    summary(MD_GA_model_1)
    vif(MD_GA_model_1)
    
    
  ## High VIF and Insignificant p-value columns: product_analytic_vertical.xGamingMemoryCard 
  ## Insignificant p-value columns: week
    MD_GA_model_2 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset + 
                          product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla + 
                          wday.xSunday + product_analytic_vertical.xMotionController + 
                          special_sale_day.xBSD + product_analytic_vertical.xGamingAccessoryKit + 
                          TV_adstock + product_analytic_vertical.xGamingMousePad + 
                          product_analytic_vertical.xGamingAdapter + sla + SEM_adtock + 
                          Other + SEM + Total_Investment + holiday_count + is_special_sale_day + 
                          Content_Marketing, data = MD_GA_data)
    
    
    summary(MD_GA_model_2)
    vif(MD_GA_model_2)
    
    
  ## Insignificant p-value columns: special_sale_day.xBSD
    MD_GA_model_3 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset + 
                          product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla + 
                          wday.xSunday + product_analytic_vertical.xMotionController + 
                          product_analytic_vertical.xGamingAccessoryKit + 
                          TV_adstock + product_analytic_vertical.xGamingMousePad + 
                          product_analytic_vertical.xGamingAdapter + sla + SEM_adtock + 
                          Other + SEM + Total_Investment + holiday_count + is_special_sale_day + 
                          Content_Marketing, data = MD_GA_data)
    
    summary(MD_GA_model_3)
    vif(MD_GA_model_3)
    
    
  ## High VIF and Insignificant p-value columns: product_analytic_vertical.xGamingMousePad
  ## Insignificant p-value columns: holiday_count
    MD_GA_model_4 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset + 
                          product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla + 
                          wday.xSunday + product_analytic_vertical.xMotionController + 
                          product_analytic_vertical.xGamingAccessoryKit + TV_adstock + 
                          product_analytic_vertical.xGamingAdapter + sla + SEM_adtock + 
                          Other + SEM + Total_Investment + is_special_sale_day + 
                          Content_Marketing, data = MD_GA_data)
    
    
    summary(MD_GA_model_4)
    vif(MD_GA_model_4)
    
    
  ## Insignificant p-value columns: is_special_sale_day
    MD_GA_model_5 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset + 
                          product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla + 
                          wday.xSunday + product_analytic_vertical.xMotionController + 
                          product_analytic_vertical.xGamingAccessoryKit + TV_adstock + 
                          product_analytic_vertical.xGamingAdapter + sla + SEM_adtock + 
                          Other + SEM + Total_Investment + Content_Marketing, data = MD_GA_data)
    
    
    summary(MD_GA_model_5)
    vif(MD_GA_model_5)
    
    
  ## Insignificant p-value columns: sla
  ## High VIF and less significant p-value columns: Total_Investment
    MD_GA_model_6 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset + 
                          product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla + 
                          wday.xSunday + product_analytic_vertical.xMotionController + 
                          product_analytic_vertical.xGamingAccessoryKit + TV_adstock + 
                          product_analytic_vertical.xGamingAdapter + SEM_adtock + 
                          Other + SEM + Content_Marketing, data = MD_GA_data)
    
    
    summary(MD_GA_model_6)
    vif(MD_GA_model_6)
    
    
  ## Slightly High VIF and Insignificant p-value columns: SEM, TV_adstock
    MD_GA_model_7 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset + 
                          product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla + 
                          wday.xSunday + product_analytic_vertical.xMotionController + 
                          product_analytic_vertical.xGamingAccessoryKit + 
                          product_analytic_vertical.xGamingAdapter + SEM_adtock + 
                          Other + Content_Marketing, data = MD_GA_data)
    
    
    summary(MD_GA_model_7)
    vif(MD_GA_model_7)
    
    
  ## High VIF value columns: product_analytic_vertical.xGamingAdapter, product_analytic_vertical.xMotionController
    MD_GA_model_8 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset + 
                          product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla + 
                          wday.xSunday + product_analytic_vertical.xGamingAccessoryKit + 
                          SEM_adtock + Other + Content_Marketing, data = MD_GA_data)
    
    
    summary(MD_GA_model_8)
    vif(MD_GA_model_8)
    
    
  ## Insignificant p-value columns: SEM_adtock
  ## Less significant p-value columns: Other, wday.xSunday
    MD_GA_model_9 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset + 
                          product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla + 
                          product_analytic_vertical.xGamingAccessoryKit + Content_Marketing, data = MD_GA_data)
    
    
    summary(MD_GA_model_9)
    vif(MD_GA_model_9)
    
    
  ## Insignificant p-value columns: Content_Marketing
    MD_GA_model_10 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla + 
                           product_analytic_vertical.xGamingAccessoryKit, data = MD_GA_data)
    
    
    summary(MD_GA_model_10)
    vif(MD_GA_model_10)
    
    
  ## High VIF value columns: product_analytic_vertical.xGamingAccessoryKit, product_analytic_vertical.xJoystickGamingWheel
    MD_GA_model_11 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset + 
                           product_procurement_sla, data = MD_GA_data)
    
    
    summary(MD_GA_model_11) # Very slight change in Adjusted R-squared at 3rd place of decimal
    vif(MD_GA_model_11)
    
    
  ## High VIF value columns: product_analytic_vertical.xGamingHeadset
    MD_GA_model_12 <- lm(formula = gmv ~ units + per_order + product_procurement_sla, data = MD_GA_data)
    
    
    summary(MD_GA_model_12) # Slight change in Adjusted R-squared at third place of decimal, good to remove taht column
    vif(MD_GA_model_12)
    
    
  ## Less significant p-value columns: product_procurement_sla
    MD_GA_model_13 <- lm(formula = gmv ~ units + per_order, data = MD_GA_data)
    
    
      summary(MD_GA_model_13) # Slight change in Adjusted R-squared at third place of decimal, good to remove taht column
      vif(MD_GA_model_13)
    
    
    
### Cross-validation
  cv.lm(data = MD_GA_data, form.lm = MD_GA_model_13, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
    
    
    
### Estimating the elasticity coefficients
    
  elasticity_13 <- function(var){
      MD_GA_elasticity <- as.numeric(MD_GA_model_13$coefficients[var]*mean(MD_GA_data[,var])/mean(MD_GA_data$gmv))
      return(MD_GA_elasticity)
      
  } 
    
  MD_GA_var_list <- list()
    
  for(i in 2:length(MD_GA_model_13$coefficients)){
      MD_GA_var_list[i-1] <- elasticity_13(names(MD_GA_model_13$coefficients)[i])
      
  }
    
  MD_GA_elasticity.outputs <- data.frame(names(MD_GA_model_13$coefficients[2:length(MD_GA_model_13$coefficients)]))
  MD_GA_elasticity.outputs <- cbind(MD_GA_elasticity.outputs,do.call(rbind.data.frame, MD_GA_var_list))
  colnames(MD_GA_elasticity.outputs) <- c("Variable","Elasticity")
    
  MD_GA_elasticity.outputs$Direction <- ifelse(MD_GA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

    
  # Plotting the elasticity
    ggplot(MD_GA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    ggtitle("GamingAccessory - Multiplicative and Distributive Lag Model") +xlab("Variables")  
    
    