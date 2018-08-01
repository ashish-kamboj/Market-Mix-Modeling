
################################################################################################################################################################
#                                             :::::::: Model Building [CameraAccessory] ::::::::
################################################################################################################################################################
    
#******************************************************** [Linear Regression Model]

### Preparing dataset
  ## Removing lag variables and Moving averages variables
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    LR_CA_data <- CameraAccessory_final[,-c(21:22,77:91)]
  
  ## Scaling the variables
    LR_CA_data[,2:ncol(LR_CA_data)] <- scale(LR_CA_data[,2:ncol(LR_CA_data)])
    
  ## Checking the variables for linear relationship or multicollinearity
    model <- lm(gmv~.,LR_CA_data)
    alias(model)
    
  ## Removing the variables which were showing linear relationship or multicollinearity
     LR_CA_data <- LR_CA_data[, -c(54:72)]

     
### Stepwise Regression to remove insignificant and correlated variables
  LR_CA_base.mod <- lm(gmv ~ 1 , data= LR_CA_data)  # base intercept only model
  LR_CA_all.mod <- lm(gmv ~ . , data= LR_CA_data) # full model with all predictors
  LR_CA_stepMod <- step(LR_CA_base.mod, scope = list(lower = LR_CA_base.mod, upper = LR_CA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  LR_CA_shortlistedVars <- names(unlist(LR_CA_stepMod[[1]])) # get the shortlisted variable.
  LR_CA_shortlistedVars <- LR_CA_shortlistedVars[!LR_CA_shortlistedVars %in% "(Intercept)"]  # remove intercept
  

### Model Building::
  
  ## Building First model after short listing the variables[using LR_CA_stepMod]
    LR_CA_model_1 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl + 
                          Online_Marketing_adstock + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xCameraMount + week + Content_Marketing_adstock + 
                          Sponsorship + product_analytic_vertical.xTeleconverter + 
                          sla + product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xTelescope + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery + 
                          Content_Marketing + Total_Investment + product_analytic_vertical.xStrap + 
                          product_analytic_vertical.xCameraEyeCup, data = LR_CA_data)
    
  
    summary(LR_CA_model_1)
    vif(LR_CA_model_1)
    
    
  ## High VIF and Insignificant p-value columns: Total_Investment
  ## Slightly high VIF and Insignificant p-value columns: Sponsorship, product_analytic_vertical.xCameraMount
  ## Insignificant p-value columns: sla, product_analytic_vertical.xCameraEyeCup
    LR_CA_model_2 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl + 
                          Online_Marketing_adstock + product_analytic_vertical.xExtensionTube + 
                          week + Content_Marketing_adstock + product_analytic_vertical.xTeleconverter + 
                          product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xTelescope + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery + 
                          Content_Marketing + product_analytic_vertical.xStrap, data = LR_CA_data)
    
    
    summary(LR_CA_model_2)
    vif(LR_CA_model_2)
    
    
  ## Slightly high VIF and Insignificant p-value columns: week
  ## High VIF and Insignificant p-value columns: week
  ## Insignificant p-value columns: product_analytic_vertical.xCameraBatteryGrip
    LR_CA_model_3 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl + 
                          Online_Marketing_adstock + product_analytic_vertical.xExtensionTube + Content_Marketing_adstock + 
                          product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery + 
                          product_analytic_vertical.xStrap, data = LR_CA_data)
    
    
    summary(LR_CA_model_3)
    vif(LR_CA_model_3)  
    
    
  ## High VIF and Insignificant p-value columns: product_analytic_vertical.xCameraBattery
  ## Insignificant p-value columns: Content_Marketing_adstock
    LR_CA_model_4 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl + 
                          Online_Marketing_adstock + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xStrap, data = LR_CA_data)
    
    
    summary(LR_CA_model_4)
    vif(LR_CA_model_4)  
    
    
    ## Slightly high VIF and Insignificant p-value columns: product_analytic_vertical.xCameraRemoteControl
    ## Insignificant p-value columns: product_analytic_vertical.xTeleconverter
    LR_CA_model_5 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock + 
                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTelescope + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xStrap, data = LR_CA_data)
    
    
    summary(LR_CA_model_5)
    vif(LR_CA_model_5) 
    
    
  ## Slightly High VIF and Less significant p-value columns: product_analytic_vertical.xStrap, product_analytic_vertical.xCameraFilmRolls
    LR_CA_model_6 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock + 
                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTelescope, data = LR_CA_data)
    
    
    summary(LR_CA_model_6)
    vif(LR_CA_model_6) 
    
    
  ## Insignificant p-value columns: product_analytic_vertical.xTelescope
  ## less significant p-value columns: Online_Marketing_adstock, product_analytic_vertical.xExtensionTube
    LR_CA_model_7 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella, data = LR_CA_data)
    
    
    summary(LR_CA_model_7)
    vif(LR_CA_model_7) 
   
    
  ## Less significant p-value columns: product_analytic_vertical.xReflectorUmbrella
    LR_CA_model_8 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger, data = LR_CA_data)
    
    
    summary(LR_CA_model_8)
    vif(LR_CA_model_8) 
    
    
  ## Less significant p-value columns: product_analytic_vertical.xCameraBatteryCharger
    LR_CA_model_9 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score, data = LR_CA_data)
    
    
    summary(LR_CA_model_9)
    vif(LR_CA_model_9) 
    
    
  ## Removing the "units" variable and then check the Adjusted R-squared value
    LR_CA_model_10 <- lm(formula = gmv ~ product_mrp + product_analytic_vertical.xCameraTripod + 
                           deliverycdays + NPS_Score, data = LR_CA_data)
    
    summary(LR_CA_model_10)  # Adjusted R-squared value is changed at 3rd place of decimal, we'good to remove that variable
    vif(LR_CA_model_10) 
    
    


### Cross-validation
  cv.lm(data = LR_CA_data, form.lm = LR_CA_model_10, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
    
  
  
### Estimating the elasticity coefficients
  
  elasticity_3 <- function(var){
    LR_CA_elasticity <-as.numeric(LR_CA_model_10$coefficients[var]*mean(LR_CA_data[,var])/mean(LR_CA_data$gmv))
    return(LR_CA_elasticity)
    
  } 
  
  LR_CA_var_list <- list()
  
  for(i in 2:length(LR_CA_model_10$coefficients)){
    LR_CA_var_list[i-1] <-elasticity_3(names(LR_CA_model_10$coefficients)[i])
    
  }
  
  LR_CA_elasticity.outputs <- data.frame(names(LR_CA_model_10$coefficients[2:length(LR_CA_model_10$coefficients)]))
  LR_CA_elasticity.outputs <- cbind(LR_CA_elasticity.outputs,do.call(rbind.data.frame, LR_CA_var_list))
  colnames(LR_CA_elasticity.outputs) <- c("Variable","Elasticity")
  
  LR_CA_elasticity.outputs$Direction <- ifelse(LR_CA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

  
  # Plotting the elasticity
    ggplot(LR_CA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity", width = 0.9) + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank()) +
    ggtitle("CameraAccessory - Linear Regression Model") +xlab("Variables")  
  
  

    
#********************************************************[Multiplicative Model]
    
### Preparing dataset
  ## Removing lag variables and Moving averages variables
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    MM_CA_data <- CameraAccessory_final[,-c(21:22,77:91)]
    
  ## Replacing 0 value in column with '0.00001' as log(0) is undefined
    MM_CA_data[MM_CA_data == 0] <- 0.00001
    
  ## Taking log of all the variable to buils to Multiplicative model
    MM_CA_data <- log(MM_CA_data)
    
  ## Checking the variables for linear relationship or multicollinearity
    MM_CA_model <- lm(gmv~.,MM_CA_data)
    alias(MM_CA_model)
    
  ## Removing the variables which were showing linear relationship or multicollinearity
    MM_CA_data <- MM_CA_data[, -c(54:72)]
    
    
### Stepwise Regression to remove insignificant and correlated variables
  MM_CA_base.mod <- lm(gmv ~ 1 , data= MM_CA_data)  # base intercept only model
  MM_CA_all.mod <- lm(gmv ~ . , data= MM_CA_data) # full model with all predictors
  MM_CA_stepMod <- step(MM_CA_base.mod, scope = list(lower = MM_CA_base.mod, upper = MM_CA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  MM_CA_shortlistedVars <- names(unlist(MM_CA_stepMod[[1]])) # get the shortlisted variable.
  MM_CA_shortlistedVars <- MM_CA_shortlistedVars[!MM_CA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables[using MM_CA_stepMod]
    MM_CA_model_1 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount + 
                          units + product_analytic_vertical.xCameraEyeCup + deliverycdays + 
                          product_analytic_vertical.xTelescope + week + product_analytic_vertical.xFlashShoeAdapter + 
                          SEM_adtock + Sponsorship, data = MM_CA_data)
    
    
    summary(MM_CA_model_1)
    vif(MM_CA_model_1)
    
    
  ## Insignificant p-value columns: product_analytic_vertical.xFlashShoeAdapter, deliverycdays, week
    MM_CA_model_2 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount + 
                          units + product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xTelescope + 
                          SEM_adtock + Sponsorship, data = MM_CA_data)
    
    
    summary(MM_CA_model_2)
    vif(MM_CA_model_2)
    
    
  ## High VIF and insignificant p-value columns: product_analytic_vertical.xTelescope 
    MM_CA_model_3 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount + 
                          units + product_analytic_vertical.xCameraEyeCup + SEM_adtock + Sponsorship, data = MM_CA_data)
    
    
    summary(MM_CA_model_3)
    vif(MM_CA_model_3)
    
    
  ## Insignificant p-value columns: SEM_adtock, Sponsorship
  ## Less significant p-value columns: product_analytic_vertical.xCameraEyeCup
    MM_CA_model_4 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount + 
                          units, data = MM_CA_data)
    
    
    summary(MM_CA_model_4)
    vif(MM_CA_model_4)
    
    
  ## High VIF value columns: units
    MM_CA_model_5 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount, data = MM_CA_data)
    
    summary(MM_CA_model_5)
    vif(MM_CA_model_5)
    
    
  ## High VIF value columns: product_analytic_vertical.xCameraMount, product_analytic_vertical.xCameraBatteryCharger
    MM_CA_model_6 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + 
                          product_analytic_vertical.xCameraTripod , data = MM_CA_data)
    
    
    summary(MM_CA_model_6)
    vif(MM_CA_model_6)
    
    
    
### Cross-validation
    cv.lm(data = MM_CA_data, form.lm = MM_CA_model_6, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
    
    
    
### Estimating the elasticity coefficients
    
  elasticity_6 <- function(var){
      MM_CA_elasticity <- as.numeric(MM_CA_model_6$coefficients[var]*mean(MM_CA_data[,var])/mean(MM_CA_data$gmv))
      return(MM_CA_elasticity)
      
  } 
    
  MM_CA_var_list <- list()
    
  for(i in 2:length(MM_CA_model_6$coefficients)){
      MM_CA_var_list[i-1] <- elasticity_6(names(MM_CA_model_6$coefficients)[i])
      
  }
    
  MM_CA_elasticity.outputs <- data.frame(names(MM_CA_model_6$coefficients[2:length(MM_CA_model_6$coefficients)]))
  MM_CA_elasticity.outputs <- cbind(MM_CA_elasticity.outputs,do.call(rbind.data.frame, MM_CA_var_list))
  colnames(MM_CA_elasticity.outputs) <- c("Variable","Elasticity")
    
  MM_CA_elasticity.outputs$Direction <- ifelse(MM_CA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")
 
    
  # Plotting the elasticity
    ggplot(MM_CA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-1),hjust = 0.1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    ggtitle("CameraAccessory - Multiplicative Model") +xlab("Variables")  
    
    
    
    
    
#********************************************************[Koyck Model]

### Preparing dataset
  ## Removing lag variables and Moving averages variables but considering the 1 week lag value of 'gmv'
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    KM_CA_data <- CameraAccessory_final[,-c(21:22,77:88,90:91)]
    
  ## Scaling the variables
    KM_CA_data[,2:ncol(KM_CA_data)] <- scale(KM_CA_data[,2:ncol(KM_CA_data)])
    
  ## Checking the variables for linear relationship or multicollinearity
    KM_CA_model <- lm(gmv~.,KM_CA_data)
    alias(KM_CA_model)
    
  ## Removing the variables which were showing linear relationship or multicollinearity
    KM_CA_data <- KM_CA_data[, -c(54:72)]
    
    
### Stepwise Regression to remove insignificant and correlated variables
  KM_CA_base.mod <- lm(gmv ~ 1 , data= KM_CA_data)  # base intercept only model
  KM_CA_all.mod <- lm(gmv ~ . , data= KM_CA_data) # full model with all predictors
  KM_CA_stepMod <- step(KM_CA_base.mod, scope = list(lower = KM_CA_base.mod, upper = KM_CA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  KM_CA_shortlistedVars <- names(unlist(KM_CA_stepMod[[1]])) # get the shortlisted variable.
  KM_CA_shortlistedVars <- KM_CA_shortlistedVars[!KM_CA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables[using KM_CA_stepMod]
    KM_CA_model_1 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl + 
                          Online_Marketing_adstock + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xCameraMount + week + Content_Marketing_adstock + 
                          Sponsorship + product_analytic_vertical.xTeleconverter + 
                          sla + product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xTelescope + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery + 
                          Content_Marketing + Total_Investment + product_analytic_vertical.xStrap + 
                          product_analytic_vertical.xCameraEyeCup, data = KM_CA_data)
    
    
    
    summary(KM_CA_model_1)
    vif(KM_CA_model_1)
    
    
  ## Insignificant p-value columns: product_analytic_vertical.xCameraEyeCup, product_analytic_vertical.xCameraBatteryGrip, sla
  ## High VIF and Insignificant p-value columns: product_analytic_vertical.xStrap
    KM_CA_model_2 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl + 
                          Online_Marketing_adstock + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xCameraMount + week + Content_Marketing_adstock + 
                          Sponsorship + product_analytic_vertical.xTeleconverter + 
                          product_analytic_vertical.xTelescope + product_analytic_vertical.xCameraFilmRolls + 
                          product_analytic_vertical.xCameraBattery + Content_Marketing + Total_Investment, data = KM_CA_data)
    
    summary(KM_CA_model_2)
    vif(KM_CA_model_2)
    
    
  ## High VIF and Insignificant p-value columns: product_analytic_vertical.xCameraRemoteControl
  ## Slightly high VIF and Insignificant p-value columns: deliverycdays, product_analytic_vertical.xTelescope
    KM_CA_model_3 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock + 
                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount + 
                          week + Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xTeleconverter + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery + 
                          Content_Marketing + Total_Investment, data = KM_CA_data)
    
    
    summary(KM_CA_model_3)
    vif(KM_CA_model_3)
    
    
  ## Slightly High VIF and Less significant p-value columns: week
    KM_CA_model_4 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock + 
                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount + 
                          Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xTeleconverter + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery + 
                          Content_Marketing + Total_Investment, data = KM_CA_data)
    
    summary(KM_CA_model_4)
    vif(KM_CA_model_4)
    
    
  ## High VIF and insignificant p-value columns: product_analytic_vertical.xCameraBattery
  ## less significant p-value columns: product_analytic_vertical.xTeleconverter 
    KM_CA_model_5 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock + 
                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount + 
                          Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xCameraFilmRolls + 
                          Content_Marketing + Total_Investment, data = KM_CA_data)
    
    summary(KM_CA_model_5)
    vif(KM_CA_model_5)
    
    
  ## High VIF and Insignificant p-value columns: Total_Investment, Content_Marketing 
  ## Less significant p-value columns: product_analytic_vertical.xCameraFilmRolls
    KM_CA_model_6 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock + 
                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount + 
                          Content_Marketing_adstock + Sponsorship, data = KM_CA_data)
    
    summary(KM_CA_model_6)
    vif(KM_CA_model_6)
    
    
  ## Less significant p-value columns: product_analytic_vertical.xCameraMount, product_analytic_vertical.xReflectorUmbrella,
  ##                                   product_analytic_vertical.xExtensionTube
    KM_CA_model_7 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          Online_Marketing_adstock + Content_Marketing_adstock + Sponsorship, data = KM_CA_data)
    
    
    summary(KM_CA_model_7)
    vif(KM_CA_model_7)
    
    
  ## Less significant p-value columns: Online_Marketing_adstock, Content_Marketing_adstock
    KM_CA_model_8 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger + Sponsorship, data = KM_CA_data)
    
    summary(KM_CA_model_8) 
    vif(KM_CA_model_8)
    
    
  ## Removing "Sponsorship" variable and will check the change in Adjusted R-squared value
    KM_CA_model_9 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger , data = KM_CA_data)
    
    
    summary(KM_CA_model_9) # Slight change at 3rd place of decimal in Adjusted R-squared value
    vif(KM_CA_model_9)
    
    
  ## Removing "product_analytic_vertical.xCameraBatteryCharger" variable and will check the change in Adjusted R-squared value
    KM_CA_model_10 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                           NPS_Score, data = KM_CA_data)
    
    
    summary(KM_CA_model_10) # Slight change at 3rd place of decimal in Adjusted R-squared value
    vif(KM_CA_model_10)
    
    
  ## Removing "units" [because of high VIF] variable
    KM_CA_model_11 <- lm(formula = gmv ~ product_mrp + product_analytic_vertical.xCameraTripod + 
                           NPS_Score, data = KM_CA_data)
    
    
    summary(KM_CA_model_11)
    vif(KM_CA_model_11)
    ##Futher removing the variable lead to decreasing in Adjusted R-squared and increased in residual error
    
    
### Cross-validation
  cv.lm(data = KM_CA_data, form.lm = KM_CA_model_11, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
    
    
    
### Estimating the elasticity coefficients
    
  elasticity_9 <- function(var){
      KM_CA_elasticity <- as.numeric(KM_CA_model_11$coefficients[var]*mean(KM_CA_data[,var])/mean(KM_CA_data$gmv))
      return(KM_CA_elasticity)
      
  } 
    
  KM_CA_var_list <- list()
    
  for(i in 2:length(KM_CA_model_11$coefficients)){
      KM_CA_var_list[i-1] <- elasticity_9(names(KM_CA_model_11$coefficients)[i])
      
  }
    
  KM_CA_elasticity.outputs <- data.frame(names(KM_CA_model_11$coefficients[2:length(KM_CA_model_11$coefficients)]))
  KM_CA_elasticity.outputs <- cbind(KM_CA_elasticity.outputs,do.call(rbind.data.frame, KM_CA_var_list))
  colnames(KM_CA_elasticity.outputs) <- c("Variable","Elasticity")
  
  KM_CA_elasticity.outputs$Direction <- ifelse(KM_CA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

    
  # Plotting the elasticity
    ggplot(KM_CA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    ggtitle("CameraAccessory - Koyck Model") +xlab("Variables")  
    
    
    
    
    
#********************************************************[Distributive Lag Model]
    
### Preparing dataset
  ## Removing lag variables and Moving averages variables but considering the 1, 2 and 3 weeks lag value of 'gmv'
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    DL_CA_data <- CameraAccessory_final[,-c(21:22,77:88)]
    
  ## Scaling the variables
    DL_CA_data[,2:ncol(DL_CA_data)] <- scale(DL_CA_data[,2:ncol(DL_CA_data)])
    
  ## Checking the variables for linear relationship or multicollinearity
    DL_CA_model <- lm(gmv~.,DL_CA_data)
    alias(DL_CA_model)
    
  ## Removing the variables which were showing linear relationship or multicollinearity
    DL_CA_data <- DL_CA_data[, -c(54:72)]
    
    
### Stepwise Regression to remove insignificant and correlated variables
  DL_CA_base.mod <- lm(gmv ~ 1 , data= DL_CA_data)  # base intercept only model
  DL_CA_all.mod <- lm(gmv ~ . , data= DL_CA_data) # full model with all predictors
  DL_CA_stepMod <- step(DL_CA_base.mod, scope = list(lower = DL_CA_base.mod, upper = DL_CA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  DL_CA_shortlistedVars <- names(unlist(DL_CA_stepMod[[1]])) # get the shortlisted variable.
  DL_CA_shortlistedVars <- DL_CA_shortlistedVars[!DL_CA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables[using DL_CA_stepMod]
    DL_CA_model_1 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl + 
                          Online_Marketing_adstock + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xCameraMount + week + Content_Marketing_adstock + 
                          Sponsorship + product_analytic_vertical.xTeleconverter + 
                          sla + product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xTelescope + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery + 
                          Content_Marketing + Total_Investment + product_analytic_vertical.xStrap + 
                          product_analytic_vertical.xCameraEyeCup, data = DL_CA_data)
    
    
    summary(DL_CA_model_1)
    vif(DL_CA_model_1)
    
    
  ## Insignificant p-value columns: product_analytic_vertical.xCameraEyeCup
  ## High VIF and Insignificant p-value columns: product_analytic_vertical.xStrap, Total_Investment
    DL_CA_model_2 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl + 
                          Online_Marketing_adstock + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xCameraMount + week + Content_Marketing_adstock + 
                          Sponsorship + product_analytic_vertical.xTeleconverter + 
                          sla + product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xTelescope + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery + 
                          Content_Marketing, data = DL_CA_data)
    
    
    summary(DL_CA_model_2)
    vif(DL_CA_model_2)
    
    
  ## High VIF and Insignificant p-value columns: Content_Marketing, product_analytic_vertical.xCameraBattery
  ## Insignificant p-value columns: product_analytic_vertical.xCameraBatteryGrip
  ## Slightly High VIF and Insignificant p-value columns: product_analytic_vertical.xCameraRemoteControl
    DL_CA_model_3 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock + 
                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount + 
                          week + Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xTeleconverter + 
                          sla + product_analytic_vertical.xTelescope + product_analytic_vertical.xCameraFilmRolls, data = DL_CA_data)
    
    
    summary(DL_CA_model_3)
    vif(DL_CA_model_3)
    
    
  ## Insignificant p-value columns: sla
  ## Slightly high VIF and Less significant p-value columns: product_analytic_vertical.xTelescope
    DL_CA_model_4 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock + 
                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount + 
                          week + Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xTeleconverter + 
                          product_analytic_vertical.xCameraFilmRolls, data = DL_CA_data)
    
    
    summary(DL_CA_model_4)
    vif(DL_CA_model_4)
    
    
  ## Slightly high VIF and Insignificant p-value columns: week
  ## Insignificant p-value columns: product_analytic_vertical.xTeleconverter
  ## less significant p-value columns: deliverycdays
    DL_CA_model_5 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock + 
                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount + 
                          Content_Marketing_adstock + Sponsorship + 
                          product_analytic_vertical.xCameraFilmRolls, data = DL_CA_data)
    
    
    summary(DL_CA_model_5)
    vif(DL_CA_model_5)
    
    
  ## Less significant p-value columns: product_analytic_vertical.xCameraFilmRolls, product_analytic_vertical.xCameraMount,
  ##                                   product_analytic_vertical.xReflectorUmbrella
    DL_CA_model_6 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger + Online_Marketing_adstock + 
                          product_analytic_vertical.xExtensionTube + Content_Marketing_adstock + 
                          Sponsorship, data = DL_CA_data)
    
    
    summary(DL_CA_model_6)
    vif(DL_CA_model_6)
    
    
  ## Less significant p-value columns: product_analytic_vertical.xExtensionTube, Online_Marketing_adstock
    DL_CA_model_7 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          Content_Marketing_adstock + Sponsorship, data = DL_CA_data)
    
    summary(DL_CA_model_7)
    vif(DL_CA_model_7)
    
    
  ## Insignificant p-value columns: Content_Marketing_adstock
    DL_CA_model_8 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          Sponsorship, data = DL_CA_data)
    
    summary(DL_CA_model_8)
    vif(DL_CA_model_8)
    
    
  ## High VIF value columns: units
    DL_CA_model_9 <- lm(formula = gmv ~ product_mrp + product_analytic_vertical.xCameraTripod + 
                          NPS_Score + product_analytic_vertical.xCameraBatteryCharger + 
                          Sponsorship, data = DL_CA_data)
    
    summary(DL_CA_model_9)
    vif(DL_CA_model_9)
    
  
  ## Insignificant p-value columns: product_analytic_vertical.xCameraBatteryCharger, Sponsorship
    DL_CA_model_10 <- lm(formula = gmv ~ product_mrp + product_analytic_vertical.xCameraTripod + 
                          NPS_Score, data = DL_CA_data)
    
    summary(DL_CA_model_10)
    vif(DL_CA_model_10)
    ##Futher removing the variable lead to decreasing in Adjusted R-squared and increased in residual error

    
### Cross-validation
  cv.lm(data = DL_CA_data, form.lm = DL_CA_model_10, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
    
    
    
### Estimating the elasticity coefficients
    
  elasticity_12 <- function(var){
      DL_CA_elasticity <- as.numeric(DL_CA_model_10$coefficients[var]*mean(DL_CA_data[,var])/mean(DL_CA_data$gmv))
      return(DL_CA_elasticity)
      
  } 
    
  DL_CA_var_list <- list()
    
  for(i in 2:length(DL_CA_model_10$coefficients)){
      DL_CA_var_list[i-1] <- elasticity_12(names(DL_CA_model_10$coefficients)[i])
      
  }
    
  DL_CA_elasticity.outputs <- data.frame(names(DL_CA_model_10$coefficients[2:length(DL_CA_model_10$coefficients)]))
  DL_CA_elasticity.outputs <- cbind(DL_CA_elasticity.outputs,do.call(rbind.data.frame, DL_CA_var_list))
  colnames(DL_CA_elasticity.outputs) <- c("Variable","Elasticity")
    
  DL_CA_elasticity.outputs$Direction <- ifelse(DL_CA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

    
  # Plotting the elasticity
    ggplot(DL_CA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity", width = 0.8) + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    ggtitle("CameraAccessory - Distributive Lag Model") +xlab("Variables")  
    
    
    
    
    
#********************************************************[Multiplicative + Distributive Lag Model]
    
### Preparing dataset
  ## Removing lag variables and Moving averages variables but considering the 1, 2 and 3 weeks lag value of 'gmv'
  ## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
  ## Retaining those is not good idea as 'gmv' won't present in unseen data
    MD_CA_data <- CameraAccessory_final[,-c(21:22,77:88)]
    
  ## Replacing 0 value in column with '0.00001' as log(0) is undefined
    MD_CA_data[MD_CA_data == 0] <- 0.00001
    
  ## Tranforming the negative values
    MD_CA_data$GMV_lag_1_per <- 1 + MD_CA_data$GMV_lag_1_per - min(MD_CA_data$GMV_lag_1_per)
    MD_CA_data$GMV_lag_2_per <- 1 + MD_CA_data$GMV_lag_2_per - min(MD_CA_data$GMV_lag_2_per)
    MD_CA_data$GMV_lag_3_per <- 1 + MD_CA_data$GMV_lag_3_per - min(MD_CA_data$GMV_lag_3_per)
    
  ## Taking log of all the variable to buils to Multiplicative model
    MD_CA_data <- log(MD_CA_data)
    
  ## Checking the variables for linear relationship or multicollinearity
    MD_CA_model <- lm(gmv~.,MD_CA_data)
    alias(MD_CA_model)
    
  ## Removing the variables which were showing linear relationship or multicollinearity
    MD_CA_data <- MD_CA_data[, -c(54:72)]
    
    
### Stepwise Regression to remove insignificant and correlated variables
  MD_CA_base.mod <- lm(gmv ~ 1 , data= MD_CA_data)  # base intercept only model
  MD_CA_all.mod <- lm(gmv ~ . , data= MD_CA_data) # full model with all predictors
  MD_CA_stepMod <- step(MD_CA_base.mod, scope = list(lower = MD_CA_base.mod, upper = MD_CA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
  MD_CA_shortlistedVars <- names(unlist(MD_CA_stepMod[[1]])) # get the shortlisted variable.
  MD_CA_shortlistedVars <- MD_CA_shortlistedVars[!MD_CA_shortlistedVars %in% "(Intercept)"]  # remove intercept
    
    
### Model Building::
    
  ## Building First model after short listing the variables[using MD_CA_stepMod]
    MD_CA_model_1 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount + 
                          units + product_analytic_vertical.xCameraEyeCup + deliverycdays + 
                          product_analytic_vertical.xTelescope + week + product_analytic_vertical.xFlashShoeAdapter + 
                          SEM_adtock + Sponsorship, data = MD_CA_data)
    
    
    summary(MD_CA_model_1)
    vif(MD_CA_model_1)
    
    
  ## Insignificant p-value columns: product_analytic_vertical.xFlashShoeAdapter, deliverycdays, week
    MD_CA_model_2 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount + 
                          units + product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xTelescope + 
                          SEM_adtock + Sponsorship, data = MD_CA_data)
    
    
    summary(MD_CA_model_2)
    vif(MD_CA_model_2)
    
    
  ## High VIF and insignificant p-value columns: product_analytic_vertical.xTelescope
    MD_CA_model_3 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount + 
                          units + product_analytic_vertical.xCameraEyeCup + SEM_adtock + Sponsorship, data = MD_CA_data)
    
    
    summary(MD_CA_model_3)
    vif(MD_CA_model_3)
    
    
  ## Insignificant p-value columns: SEM_adtock, Sponsorship
  ## Less significant p-value columns: product_analytic_vertical.xCameraEyeCup
    MD_CA_model_4 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount + 
                          units, data = MD_CA_data)
    
    
    summary(MD_CA_model_4)
    vif(MD_CA_model_4)
    
    
  ## High VIF value columns: units, product_analytic_vertical.xCameraMount  
    MD_CA_model_5 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xCameraBatteryCharger, data = MD_CA_data)
    
    
    summary(MD_CA_model_5)
    vif(MD_CA_model_5)
    
    
  ## High VIF value column: product_analytic_vertical.xCameraBatteryCharger
    MD_CA_model_6 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + 
                          product_analytic_vertical.xCameraTripod, data = MD_CA_data)
    
    summary(MD_CA_model_6)
    vif(MD_CA_model_6)
    
    
    
### Cross-validation
  cv.lm(data = MD_CA_data, form.lm = MD_CA_model_6, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
    
  
    
### Estimating the elasticity coefficients
    
  elasticity_15 <- function(var){
      MD_CA_elasticity <- as.numeric(MD_CA_model_6$coefficients[var]*mean(MD_CA_data[,var])/mean(MD_CA_data$gmv))
      return(MD_CA_elasticity)
  } 
    
  MD_CA_var_list <- list()
    
  for(i in 2:length(MD_CA_model_6$coefficients)){
      MD_CA_var_list[i-1] <- elasticity_15(names(MD_CA_model_6$coefficients)[i])
      
  }
    
  MD_CA_elasticity.outputs <- data.frame(names(MD_CA_model_6$coefficients[2:length(MD_CA_model_6$coefficients)]))
  MD_CA_elasticity.outputs <- cbind(MD_CA_elasticity.outputs,do.call(rbind.data.frame, MD_CA_var_list))
  colnames(MD_CA_elasticity.outputs) <- c("Variable","Elasticity")
    
  MD_CA_elasticity.outputs$Direction <- ifelse(MD_CA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

    
  # Plotting the elasticity
    ggplot(MD_CA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
    geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
    scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    ggtitle("CameraAccessory - Multiplicative and Distributive Lag Model") +xlab("Variables")  
    
    
    