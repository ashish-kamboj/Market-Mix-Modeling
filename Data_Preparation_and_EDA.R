#############################################################################################################################################################
#                                                   :::::::: Market Mix Modelling ::::::::
#############################################################################################################################################################

### Business Understanding::

# ElecKart is an e-commerce firm specialising in electronic products. Over the last one year, they had spent a significant amount 
# of money in marketing. They also offered big-ticket promotions. 

# They are about to create a marketing budget for the next year which includes spending on commercials, online campaigns, 
# and pricing & promotion strategies.


### Objective::

# The aim is to develop a market mix model to observe the actual impact of different marketing variables over the last year.
# Basically needs to optimize the marketing levers to improve the revenue response.

# Below are the data or variables needs to be consider for analysis

    # Products Sales data
    # Media Investment
    # NPS Score
    # Special Sale days [Holidays]




#############################################################################################################################################################
#                                                     :::::::: Data Understanding ::::::::
#############################################################################################################################################################

### Load Libraries
  library(readxl)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(ggthemes)
  library(cowplot)
  library(dplyr)
  library(zoo)
  library(DataCombine)
  library(imputeTS)
  library(MASS)
  library(car)
  library(DAAG)


### Data Load
  eleckart <- read.csv("ConsumerElectronics.csv", stringsAsFactors = F)
  media_investment <- read_excel("Media data and other information.xlsx", sheet = "Media Investment", skip = 2)
  nps <- read_excel("Media data and other information.xlsx", sheet = "Monthly NPS Score", skip =2, col_names = FALSE)
  
  str(eleckart)   #1648824 obs. of  20 variables

  


       
################################################################################################################################################################
#                                                     :::::::: Data Preparation ::::::::
################################################################################################################################################################
  
    
### Converting the "media_investment" dataframe into weekly level spends on advertising channels (originally it hold monthly investment)

  ## computing Month, week, and no.of days per week (month, week)
    days <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')
    weekdays <- data.frame('days'=days, Month = month(days), week = week(days),nweek = rep(1,length(days)))
    weekdays <- data.frame(weekdays %>% group_by(Month,week) %>% summarise(nweeks = sum(nweek)))
    weekdays$fracDays <- weekdays$nweeks/7

  ## Replacing NA values
    media_investment[is.na(media_investment)] <- 0
      
  ## converting montly spend to weekly
    media_investment <- cbind(Month=media_investment[,c(2)], media_investment[,-c(1,2)]/4.30)
    
  ## Add weekly information
    media_investment_weekly <- merge(weekdays,media_investment, by='Month', all.x = TRUE)

      
  ## Converting media Investment at weekly granularity
  # pro-rate weekly investment as per the ratio of its days span over adjacent months
    media_investment_weekly_final <- data.frame(media_investment_weekly %>% group_by(week) %>% 
                                     summarise(Total_Investment = sum(`Total Investment`*fracDays),
                                     TV = sum(TV*fracDays), Digital=sum(Digital*fracDays),Sponsorship = sum(Sponsorship*fracDays), 
                                     Content_Marketing = sum(`Content Marketing`*fracDays),Online_Marketing = sum(`Online marketing`*fracDays), 
                                     Affiliates = sum(Affiliates*fracDays), SEM = sum(SEM*fracDays), Radio = sum(Radio*fracDays), 
                                     Other = sum(Other*fracDays)))
    
      
  ## Converting media investement into crores
    media_investment_weekly_final[,2:11] <- media_investment_weekly_final[,2:11]*10000000
 
      
            
### Converting the 'nps' dataframe into weekly level
  ## Re-naming the nps dataframe column name
      colnames(nps) <- c(0:12)
      
  ## adding two rows in nps dataframe to add 'Year' and 'Month'
      nps <- rbind(nps, c(0,2015,2015,2015,2015,2015, 2015,2016,2016,2016,2016,2016,2016), c(0,7,8,9,10,11,12,1,2,3,4,5,6))
      
  ## Setting the first first row of first column to 0
      nps[1,1] <- 0
    
  ## Taking the transpose of the nps dataframe (i.e. converting rows to column and vice-versa)
      nps = setNames(data.frame(t(nps[,-1])), nps[,1])
      
  ## Re-naming the header of nps dataframe
      colnames(nps) <- c("NPS Score","Year","Month")
      
  ## Rounding off the numerical value to one place of decimal
      nps[,'NPS Score']=round(nps[,'NPS Score'],1)
      
      

### Checking whether the data is between "July 2015 to June 2016"
    table(eleckart$Year, eleckart$Month)
    # Data is present for 2015- May & June and 2016- July
    
      
### Fiilerting out the May, JUne 2015 and July 2016 data
    eleckart_filtered <- subset(eleckart, !(Month ==5 & Year==2015 | Month ==6 & Year==2015 | Month ==7 & Year==2016))
    table(eleckart_filtered$Year, eleckart_filtered$Month)
    
### Filtering out the duplicate records based on the columns ["order_date", "order_id", "order_item_id" and "units"]
    eleckart_filtered <- eleckart_filtered[!duplicated(eleckart_filtered[c(1,5,6,8)]),]
    
      
### Filtering out the rows/order where Product_MRP is '0'
      eleckart_filtered <- subset(eleckart_filtered, product_mrp != 0)
      
      
### Replacing the "gmv" with product_mrp*units wherever gmv=0 (assuming products were sold without any discount)
    row_no <- which(eleckart_filtered$gmv==0)
      
    for(i in 1:length(row_no)){
        eleckart_filtered$gmv[row_no[i]] <- (eleckart_filtered$product_mrp[row_no[i]])*(eleckart_filtered$units[row_no[i]])
    }
      

### Filtering out the records where "gmv" is greater than 'product_mrp*units' (as we can't charge more than MRP)
    eleckart_filtered <- subset(eleckart_filtered, (product_mrp*units) >= gmv)
    
### Converting the order_date to the "Date" format
    eleckart_filtered$order_date <- as.Date(eleckart_filtered$order_date)
      
### Converted order_date in to Date format and creted as new column in order to perform analysis based on the Year and Month
    eleckart_filtered$month_date <- as.Date(cut(eleckart_filtered$order_date,breaks = "month"))

### Derived a column "wday" from the order_date to find the day of a week
    eleckart_filtered$wday <- weekdays(eleckart_filtered$order_date)
      
### Derived a column "week" from the order_date (it gives the week number i.e. 1, 2 etc.)
    eleckart_filtered$week <- as.numeric(strftime(eleckart_filtered$order_date, format = "%V"))
      
### Derived a column "week_date" from the order_date (it gives the starting date of the week)
    eleckart_filtered$Week_date <- as.Date(cut(eleckart_filtered$order_date,breaks = "week", start.on.monday = FALSE))  # changes weekly break point to Sunday

        
### Creating a vector "date" to store the dates of Special Sale Calender as provided in the 'Media data and other information.xlsx'
    date <- as.Date(c("2015-07-18","2015-07-19","2015-08-15","2015-08-16","2015-08-17","2015-08-28","2015-08-29","2015-08-30","2015-10-15","2015-10-16","2015-10-17",
                "2015-11-07","2015-11-08","2015-11-09","2015-11-10","2015-11-11","2015-11-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26","2015-12-27",
                "2015-12-28","2015-12-29","2015-12-30","2015-12-31","2016-01-01","2016-01-02","2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01",
                "2016-02-02","2016-02-14","2016-02-15","2016-02-20","2016-02-21","2016-03-07","2016-03-08","2016-03-09","2016-05-25","2016-05-26","2016-05-27"))


  ## Creating a column whether a order is placed on special sale day or not
      eleckart_filtered$is_special_sale_day <- ifelse(eleckart_filtered$order_date %in% date, "Y", "N")
    
  ## Creating one more column "special_sale_day" which stores which special day it was (like Diwali, Eid etc.)
      eleckart_filtered$special_sale_day='Regular Day'
      
      eleckart_filtered <- within(eleckart_filtered, {
        special_sale_day[order_date  %in% (date[1:2])]='Eid & Rathayatra'
        special_sale_day[order_date  %in% (date[3:5])]='Independence Day'
        special_sale_day[order_date  %in% (date[6:8])]='Rakshabandhan'
        special_sale_day[order_date  %in% (date[9:11])]='Daussera'
        special_sale_day[order_date  %in% (date[12:19])]='Diwali'
        special_sale_day[order_date  %in% (date[20:29])]='Christmas & New Year'
        special_sale_day[order_date  %in% (date[30:32])]='Republic Day'
        special_sale_day[order_date  %in% (date[33:34])]='BED'
        special_sale_day[order_date  %in% (date[35:36])]='Valentine Day'
        special_sale_day[order_date  %in% (date[37:38])]='FHSD'
        special_sale_day[order_date  %in% (date[39:41])]='BSD'
        special_sale_day[order_date  %in% (date[42:44])]='Pacman'
      })
      

  ## Creating a dataframe which holds the number of holidays per week
      
      holidays <- date   #Coverting the date vector into Date format
      week <- strftime(holidays, format = "%V")   #Extracting the weeks out of date
      Year <- format(as.POSIXct(holidays, format="%Y-%m-%d"),"%Y")  #Extracting the Year out of date
      
      holiday_details <- data.frame(cbind(Year,week))   #Creating a dataframe to hold holiday details
      holiday_details$holidays <- holidays
      holiday_details$holiday_count <- 1
      holiday_details <- aggregate(holiday_count~Year+week, holiday_details, sum)   #Aggregating the holidays couns based on week
      
      
### Checking for the "units" column in case of any outlier 
    quantile(eleckart_filtered$units, seq(0,1,.001))  #99.9% of orders/records have <=4 units
    table(eleckart_filtered$units)
      
    # Capping the maximum order to 4 and filtering out the records having units>4 as outlier
      eleckart_filtered <- subset(eleckart_filtered, units <= 4)


### Checking of the NA or any invalid valid values in "deliverybdays" column
    table(eleckart_filtered$deliverybdays)  #so many \\N values and some negative values also
      
    # Assining 0 to the values where deliverycdays are "\\N" and negative values
      eleckart_filtered$deliverybdays[eleckart_filtered$deliverybdays == "\\N" | eleckart_filtered$deliverybdays < 0] <- 0

            
### Checking of the NA or any invalid valid values in "deliverycdays" column
    table(eleckart_filtered$deliverycdays)  #so many \\N values and some negative values also
      
    # Assining 0 to the values where deliverycdays are "\\N" and negative values
      eleckart_filtered$deliverycdays[eleckart_filtered$deliverycdays == "\\N" | eleckart_filtered$deliverycdays < 0] <- 0
     

### Analyzing "product_procurement_sla" column
      table(eleckart_filtered$product_procurement_sla) #67976 negative values
      
      # Assining 0 to the values where product_procurement_sla having negative values
      eleckart_filtered$product_procurement_sla[eleckart_filtered$product_procurement_sla < 0] <- 0
      
### Checking for the range of "sla" column 
    quantile(eleckart_filtered$sla, seq(0,1,.001)) ##99.9% of data has max sla of 17, so we'll cap as 17 max
    eleckart_filtered$sla[eleckart_filtered$sla > 17] <- 17  ##Capping the max sla as 17
      
### Checking for missing('NA') value
    sapply(eleckart_filtered, function(x) sum(is.na(x)))  #No NA values

### Removing the columns, which is not important for analysis
    eleckart_filtered <- eleckart_filtered[,-c(1,2,5,6,13:16)]
    
### Merging Media investment and nps data frame with eleckart
    eleckart_filtered <- merge(eleckart_filtered, media_investment_weekly_final, by = c("week"), all.x = TRUE)
    eleckart_filtered <- merge(eleckart_filtered, nps, by = c("Year", "Month"), all.x = TRUE)
    eleckart_filtered <- merge(eleckart_filtered, holiday_details, by = c("Year", "week"), all.x = TRUE)
    
    # Setting the holiday count to where NA's are present (means no holidays for those weeks)
    eleckart_filtered$holiday_count[which(is.na(eleckart_filtered$holiday_count))] <- 0
 
         
### Further filtering the data for the product sub-categories- camera accessory, home audio and gaming accessory. 
    
    eleckart_CameraAccessory <- subset(eleckart_filtered, product_analytic_sub_category == "CameraAccessory")
    eleckart_HomeAudio <- subset(eleckart_filtered, product_analytic_sub_category == "HomeAudio")
    eleckart_GamingAccessory <- subset(eleckart_filtered, product_analytic_sub_category == "GamingAccessory")
    

    


        
################################################################################################################################################################
#                                             :::::::: Exploratory Data Analysis ::::::::
################################################################################################################################################################
    
### Aggregated Monthly orders
    monthly_units_sold <- aggregate(units~month_date, eleckart_filtered, sum, na.rm=TRUE)

    # Plotting Bar graph showing Monthly orders
      ggplot(monthly_units_sold, aes(x=month_date, y=units)) + geom_bar(stat = "identity", fill="steelblue", width = 15) + 
      geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_minimal() + scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("months")) + 
      labs(x="Months",y="Number of Units Sold") + ggtitle("Monthly Units Sold") + 
      theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
      
    # Plotting Line graph showing Monthly orders
      ggplot(monthly_units_sold, aes(x=month_date, y=units)) + geom_line(size=1, color = "steelblue") + geom_point() +
      scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("months")) + theme_bw()

### Aggregated Weekly orders
    weekly_units_sold <- aggregate(units~Week_date, eleckart_filtered, sum, na.rm=TRUE)
        
    # Plotting Bar graph showing weekly orders
      ggplot(weekly_units_sold, aes(x=Week_date, y=units)) + geom_bar(stat = "identity", fill="steelblue") + 
      geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc() + scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + 
      labs(x="Weeks",y="Number of Units Sold") + ggtitle("Weekly Units Sold") + 
      theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
        
    # Plotting Line graph showing weekly orders
      ggplot(weekly_units_sold, aes(x=Week_date, y=units)) + geom_line(size=1, color = "steelblue") + geom_point() +
      scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + 
      theme_bw() + labs(x="Weeks",y="Number of Units Sold") + ggtitle("Weekly Units Sold") + 
      theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
        
        
#*********************************************************************************************************************************************************#       
### Aggregated orders by product sub-category
    product_units_sold <- aggregate(units~product_analytic_sub_category, eleckart_filtered, sum, na.rm=TRUE)
    
    product_units_sold <- subset(product_units_sold, product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio" | 
                                   product_analytic_sub_category == "GamingAccessory")
    
    # Plotting Bar graph showing units sold for different product sub-category
      ggplot(product_units_sold, aes(x=as.factor(product_analytic_sub_category), y=units,fill=as.factor(product_analytic_sub_category))) + geom_bar(stat = "identity", width = 0.4) + 
      geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Product Sub-Category",y="Number of Units Sold") + 
      ggtitle("Products Units Sold") + scale_fill_manual("Product Sub-Category: ", values = c("CameraAccessory" = "green4", "GamingAccessory" = "yellow", "HomeAudio" = "red3"))
      theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
     
        
#*********************************************************************************************************************************************************# 
### Aggregated Monthly orders by product sub-category
    monthly_product_units_sold <- aggregate(units~month_date + product_analytic_sub_category, eleckart_filtered, sum, na.rm=TRUE)
    
    monthly_product_units_sold <- subset(monthly_product_units_sold, product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio" | 
                                   product_analytic_sub_category == "GamingAccessory")
        
    # Plotting Bar graph showing units sold for different product sub-category
      ggplot(monthly_product_units_sold, aes(x=month_date,y=units, fill=as.factor(product_analytic_sub_category))) + geom_bar(stat="identity",position = "dodge", width = 15) + 
      theme_hc(base_size = 18, base_family = "sans") + labs(x="Months",y="Number of Units Sold") + 
      ggtitle("Monthly Product Units Sold") + scale_fill_manual("Product Sub-Category: ", values = c("CameraAccessory" = "green4", "GamingAccessory" = "yellow", "HomeAudio" = "red3")) +
      theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
      scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("months"))
        
### Aggregated weekly orders by product sub-category
    weekly_product_units_sold <- aggregate(units~Week_date + product_analytic_sub_category, eleckart_filtered, sum, na.rm=TRUE)
    
    weekly_product_units_sold <- subset(weekly_product_units_sold, product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio" | 
                                           product_analytic_sub_category == "GamingAccessory")
        
    # Plotting Bar graph showing units sold for different product sub-category weekly
      ggplot(weekly_product_units_sold, aes(x=Week_date,y=units, fill=as.factor(product_analytic_sub_category))) + geom_bar(stat="identity",position = "stack", width = 4) + 
      theme_hc(base_size = 18, base_family = "sans") + labs(x="Weeks",y="Number of Units Sold") + 
      ggtitle("Weekly Units Sold by Product Sub-Categories") + scale_fill_manual("Product Sub-Category: ", values = c("CameraAccessory" = "green4", "GamingAccessory" = "yellow", "HomeAudio" = "red3")) +
      theme(legend.justification="center",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
      scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))
          
          
    # Plotting Line graph showing weekly orders
      ggplot(weekly_product_units_sold, aes(x=Week_date,y=units, group=as.factor(product_analytic_sub_category))) + geom_line(size=1.5, aes(color = product_analytic_sub_category)) + 
      scale_color_manual("Product Sub-Category: ", values = c("CameraAccessory" = "green4", "GamingAccessory" = "yellow", "HomeAudio" = "red3")) +
      scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + theme_bw() + labs(x="Weeks",y="Number of Units Sold") + ggtitle("Weekly Units Sold by Product Sub-Categories") + 
      theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
          
        
#*********************************************************************************************************************************************************# 
### Aggregated units sold by top 10 Product Vertical
    product_vertical_units_sold <- subset(eleckart_filtered, product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio" | 
                                              product_analytic_sub_category == "GamingAccessory")
      
    product_vertical_units_sold <- aggregate(units~product_analytic_vertical, product_vertical_units_sold, sum, na.rm=TRUE)
    
    top_10_product_vertical <- product_vertical_units_sold[order(product_vertical_units_sold$units, decreasing = TRUE),][1:10,]
        
    # Plotting Bar graph showing units sold for top 10 Product Vertical
      ggplot(top_10_product_vertical, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
      geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Product Vertical",y="Number of Units Sold") + 
      ggtitle("Product Vertical Units Sold") + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5),legend.position="none") 
        
     
#*********************************************************************************************************************************************************# 
### Aggregated Product units sold on weekdays/weekends
    wday_units_sold <- aggregate(units~wday, eleckart_filtered, sum, na.rm=TRUE)
    
    wday_units_sold$wday <- factor(wday_units_sold$wday,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      
    # Plotting Bar graph showing units sold for top 10 Product Vertical
      ggplot(wday_units_sold, aes(x=as.factor(wday), y=units, fill = as.factor(wday))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
      geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Weekdays",y="Number of Units Sold") + 
      ggtitle("Units Sold on Weekdays/Weekend") + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5),legend.position="none") 

     
#*********************************************************************************************************************************************************# 
### Aggregated Monthly orders by different payment types
    monthly_units_sold_by_payment_type <- aggregate(units~month_date + s1_fact.order_payment_type, eleckart_filtered, sum, na.rm=TRUE)
        
    # Plotting Bar graph showing units sold through different payment type
      ggplot(monthly_units_sold_by_payment_type, aes(x=month_date,y=units, fill=as.factor(s1_fact.order_payment_type))) + geom_bar(stat="identity",position = "dodge", width = 17) + 
      theme_hc(base_size = 18, base_family = "sans") + labs(x="Months",y="Number of Units Sold") + 
      ggtitle("Monthly Product Units Sold by Different Payment types") + scale_fill_manual("Payment Type: ", values = c("COD" = "navyblue", "Prepaid" = "goldenrod1")) +
      theme(legend.justification="center", axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
      scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("months"))
    
          
### Aggregated Weekly orders by different payment types
    weekly_units_sold_by_payment_type <- aggregate(units~Week_date + s1_fact.order_payment_type, eleckart_filtered, sum, na.rm=TRUE)
          
    # Plotting Bar graph showing units sold through different payment type
      ggplot(weekly_units_sold_by_payment_type, aes(x=Week_date,y=units, fill=as.factor(s1_fact.order_payment_type))) + geom_bar(stat="identity",position = "stack", width = 4) + 
      theme_hc(base_size = 18, base_family = "sans") + labs(x="Weeks",y="Number of Units Sold") + 
      ggtitle("Weekly Product Units Sold by Different Payment types") + scale_fill_manual("Payment Type: ", values = c("COD" = "navyblue", "Prepaid" = "goldenrod1")) +
      theme(legend.justification="center" , axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, size = 12,vjust = 0.4),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
      scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))
          
      
      
#*********************************************************************************************************************************************************#           

### Releasing memory
  gc()
      
### Weekly spends on different marketing channels [or Media Investment]
  weekly_ad_spent <- eleckart_filtered[,c(16,20:28)] %>% group_by(Week_date) %>% summarise_all(funs(sum)) %>% data.frame()
  weekly_ad_spent$week <- c(1:53)
  weekly_ad_spent <- weekly_ad_spent[,c(11,2:10)]
      
  # Plotting Line graph amount spent on different marketing channels
    plots <- list()  # new empty list
    
    for (i in 2:10) local({
        i <- i
        p0 <- ggplot(weekly_ad_spent,aes(x=weekly_ad_spent[,1],y=weekly_ad_spent[,i])) + 
              geom_line(size=1, color = "steelblue") + geom_point() + theme_bw() + 
              labs(x="Weeks",y= paste0("Spend on ", colnames(weekly_ad_spent[i])," Ads"))
        
        plots[[i-1]] <<- p0  # add each plot into plot list
        
    })
      
    # Plotting all the graphs
    # Note: It takes few seconds to load, please hold in order to come up all the graphs
      plot_grid(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],
                plots[[7]],plots[[8]],plots[[9]],align ="h")
      
      

      


################################################################################################################################################################
#                                     :::::::: Exploratory Data Analysis based on sub-categories ::::::::
################################################################################################################################################################
 
           
#***************************************************************** CameraAccessory ****************************************************************************# 

### Weekly Gross Merchandise Value
  CA_weekly_gmv <- aggregate(gmv~Week_date, eleckart_CameraAccessory, sum, na.rm=TRUE)
      
  # Plotting Line graph showing weekly Gross Merchandise Value
    ggplot(CA_weekly_gmv, aes(x=Week_date, y=gmv)) + geom_line(size=1, color = "steelblue") + geom_point() +
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + 
    theme_bw() + labs(x="Weeks",y="GMV") + ggtitle("CameraAccessory - Weekly Gross Merchandise Value") + 
    theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
      

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
### Weekly units sold
  CA_weekly_units_sold <- aggregate(units~Week_date, eleckart_CameraAccessory, sum, na.rm=TRUE)
    
  # Plotting Line graph showing weekly orders
    ggplot(CA_weekly_units_sold, aes(x=Week_date, y=units)) + geom_line(size=1, color = "steelblue") + geom_point() +
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + 
    theme_bw() + labs(x="Weeks",y="Number of Units Sold") + ggtitle("CameraAccessory - Weekly Units Sold") + 
    theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
### Aggregated Weekly orders by different payment types
  CA_weekly_units_sold_by_payment_type <- aggregate(units~Week_date + s1_fact.order_payment_type, eleckart_CameraAccessory, sum, na.rm=TRUE)
    
  # Plotting Bar graph showing units sold through different payment types
    ggplot(CA_weekly_units_sold_by_payment_type, aes(x=Week_date,y=units, fill=as.factor(s1_fact.order_payment_type))) + geom_bar(stat="identity",position = "stack", width = 4) + 
    theme_hc(base_size = 18, base_family = "sans") + labs(x="Weeks",y="Number of Units Sold") + 
    ggtitle("CameraAccessory - Weekly Product Units Sold by Different Payment types") + scale_fill_manual("Payment Type: ", values = c("COD" = "navyblue", "Prepaid" = "goldenrod1")) +
    theme(legend.justification="center", axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, size = 12,vjust = 0.4),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))
    
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
### Aggregated units sold for top 10 CameraAccessory Products
  CA_product_units_sold <- aggregate(units~product_analytic_vertical, eleckart_CameraAccessory, sum, na.rm=TRUE)
    
  CA_top_10_products <- CA_product_units_sold[order(CA_product_units_sold$units, decreasing = TRUE),][1:10,]
  
  # Ordering the top 10 products based on the number of units sold for display
  CA_top_10_products$product_analytic_vertical <- factor(CA_top_10_products$product_analytic_vertical, levels = CA_top_10_products$product_analytic_vertical[order(-CA_top_10_products$units)]) 
    
  # Plotting Bar graph showing units sold for top 10 Products
    ggplot(CA_top_10_products, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
    geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Product Vertical",y="Number of Units Sold") + 
    ggtitle("CameraAccessory - Product Units Sold") + theme(legend.position="none", axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle = 45, vjust = 0.6),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
### Weekly average mrp of the products sold
  CA_weekly_avg_product_mrp <- aggregate(product_mrp~Week_date, eleckart_CameraAccessory, mean, na.rm=TRUE)
    
  # Plotting Line graph showing weekly average mrp of the products sold
    ggplot(CA_weekly_avg_product_mrp, aes(x=Week_date, y=product_mrp)) + geom_line(size=1, color = "steelblue") + geom_point() +
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + 
    theme_bw() + labs(x="Weeks",y="Average Product MRP") + ggtitle("CameraAccessory - Weekly Average Product MRP") + 
    theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
### Aggregated Product units sold on weekdays/weekends
  CA_wday_units_sold <- aggregate(units~wday, eleckart_CameraAccessory, sum, na.rm=TRUE)
    
  CA_wday_units_sold$wday <- factor(CA_wday_units_sold$wday,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
  # Plotting Bar graph showing units sold on weekdays/weekends
    ggplot(CA_wday_units_sold, aes(x=as.factor(wday), y=units, fill = as.factor(wday))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
    geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Weekdays",y="Number of Units Sold") + 
    ggtitle("CameraAccessory - Units Sold on Weekdays/Weekend") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
### Units sold of special sale day [i.e. on different holidays]
  CA_special_sale_day_units_sold <- subset(eleckart_CameraAccessory, special_sale_day != "Regular Day")
    
  CA_special_sale_day_units_sold <- aggregate(units~special_sale_day, CA_special_sale_day_units_sold, sum, na.rm=TRUE)
  
  # Ordering the special sales day based on the number of units sold
  CA_special_sale_day_units_sold$special_sale_day <- factor(CA_special_sale_day_units_sold$special_sale_day, levels = CA_special_sale_day_units_sold$special_sale_day[order(-CA_special_sale_day_units_sold$units)]) 
  
  # Plotting Bar graph showing units sold on different holiday seaseons
    ggplot(CA_special_sale_day_units_sold, aes(x=as.factor(special_sale_day), y=units, fill = as.factor(special_sale_day))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Paired") +
    geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Special Sale Days",y="Number of Units Sold") + 
    ggtitle("CameraAccessory - Units Sold on Special Sale Day") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
### Releasing memory
  gc()
    
### Weekly spends on different marketing channels [or Media Investment]
  CA_weekly_ad_spent <- eleckart_CameraAccessory[,c(16,20:28)] %>% group_by(Week_date) %>% summarise_all(funs(sum)) %>% data.frame()
  CA_weekly_ad_spent$week <- c(1:52)
  CA_weekly_ad_spent <- CA_weekly_ad_spent[,c(11,2:10)]

  # Plotting Line graph amount spent on different marketing channels
    CA_plots <- list()  # new empty list
    for (i in 2:10) local({
        i <- i
        p1 <- ggplot(CA_weekly_ad_spent,aes(x=CA_weekly_ad_spent[,1],y=CA_weekly_ad_spent[,i])) + 
              geom_line(size=1, color = "steelblue") + geom_point() + theme_bw() + 
              labs(x="Weeks",y= paste0("Spend on ", colnames(CA_weekly_ad_spent[i])," Ads"))
        
        CA_plots[[i-1]] <<- p1  # add each plot into plot list
        
    })
      
  # Plotting all the graphs
  # Note: It takes few seconds to load, please hold in order to come up all the graphs
    plot_grid(CA_plots[[1]],CA_plots[[2]],CA_plots[[3]],CA_plots[[4]],CA_plots[[5]],CA_plots[[6]],
              CA_plots[[7]],CA_plots[[8]],CA_plots[[9]],align ="h")
      
 


#***************************************************************** HomeAudio ****************************************************************************# 
    
### Weekly Gross Merchandise Value
  HA_weekly_gmv <- aggregate(gmv~Week_date, eleckart_HomeAudio, sum, na.rm=TRUE)
    
  # Plotting Line graph showing weekly Gross Merchandise Value
    ggplot(HA_weekly_gmv, aes(x=Week_date, y=gmv)) + geom_line(size=1, color = "steelblue") + geom_point() +
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + 
    theme_bw() + labs(x="Weeks",y="GMV") + ggtitle("HomeAudio - Weekly Gross Merchandise Value") + 
    theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
### Weekly units sold
  HA_weekly_units_sold <- aggregate(units~Week_date, eleckart_HomeAudio, sum, na.rm=TRUE)
    
  # Plotting Line graph showing weekly orders
    ggplot(HA_weekly_units_sold, aes(x=Week_date, y=units)) + geom_line(size=1, color = "steelblue") + geom_point() +
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + 
    theme_bw() + labs(x="Weeks",y="Number of Units Sold") + ggtitle("HomeAudio - Weekly Units Sold") + 
    theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
### Aggregated Weekly orders by different payment types
  HA_weekly_units_sold_by_payment_type <- aggregate(units~Week_date + s1_fact.order_payment_type, eleckart_HomeAudio, sum, na.rm=TRUE)
    
  # Plotting Bar graph showing units sold through different payment types
    ggplot(HA_weekly_units_sold_by_payment_type, aes(x=Week_date,y=units, fill=as.factor(s1_fact.order_payment_type))) + geom_bar(stat="identity",position = "stack", width = 4) + 
    theme_hc(base_size = 18, base_family = "sans") + labs(x="Weeks",y="Number of Units Sold") + 
    ggtitle("HomeAudio - Weekly Product Units Sold by Different Payment types") + scale_fill_manual("Payment Type: ", values = c("COD" = "navyblue", "Prepaid" = "goldenrod1")) +
    theme(legend.justification="center",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, size = 12,vjust = 0.4),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
### Aggregated units sold for top 10 CameraAccessory Products
  HA_product_units_sold <- aggregate(units~product_analytic_vertical, eleckart_HomeAudio, sum, na.rm=TRUE)
    
  HA_top_10_products <- HA_product_units_sold[order(HA_product_units_sold$units, decreasing = TRUE),][1:10,]
  
  # Ordering the top 10 products based on the number of units sold for display
  HA_top_10_products$product_analytic_vertical <- factor(HA_top_10_products$product_analytic_vertical, levels = HA_top_10_products$product_analytic_vertical[order(-HA_top_10_products$units)]) 
    
  # Plotting Bar graph showing units sold for top 10 Products
    ggplot(HA_top_10_products, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
    geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Product Vertical",y="Number of Units Sold") + 
    ggtitle("HomeAudio - Product Units Sold") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
### Weekly average mrp of the products sold
  HA_weekly_avg_product_mrp <- aggregate(product_mrp~Week_date, eleckart_HomeAudio, mean, na.rm=TRUE)
    
  # Plotting Line graph showing weekly average mrp of the products sold
    ggplot(HA_weekly_avg_product_mrp, aes(x=Week_date, y=product_mrp)) + geom_line(size=1, color = "steelblue") + geom_point() +
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + 
    theme_bw() + labs(x="Weeks",y="Average Product MRP") + ggtitle("HomeAudio - Weekly Average Product MRP") + 
    theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
### Aggregated Product units sold on weekdays/weekends
  HA_wday_units_sold <- aggregate(units~wday, eleckart_HomeAudio, sum, na.rm=TRUE)
    
  HA_wday_units_sold$wday <- factor(HA_wday_units_sold$wday,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
  # Plotting Bar graph showing units sold on weekdays/weekends
    ggplot(HA_wday_units_sold, aes(x=as.factor(wday), y=units, fill = as.factor(wday))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
    geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Weekdays",y="Number of Units Sold") + 
    ggtitle("HomeAudio - Units Sold on Weekdays/Weekend") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
### Units sold of special sale day [i.e. on different holidays]
  HA_special_sale_day_units_sold <- subset(eleckart_HomeAudio, special_sale_day != "Regular Day")
  
  HA_special_sale_day_units_sold <- aggregate(units~special_sale_day, HA_special_sale_day_units_sold, sum, na.rm=TRUE)
  
  # Ordering the special sales day based on the number of units sold
  HA_special_sale_day_units_sold$special_sale_day <- factor(HA_special_sale_day_units_sold$special_sale_day, levels = HA_special_sale_day_units_sold$special_sale_day[order(-HA_special_sale_day_units_sold$units)]) 
    
  # Plotting Bar graph showing units sold on different holiday seaseons
    ggplot(HA_special_sale_day_units_sold, aes(x=as.factor(special_sale_day), y=units, fill = as.factor(special_sale_day))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Paired") +
    geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Special Sale Days",y="Number of Units Sold") + 
    ggtitle("HomeAudio - Units Sold on Special Sale Day") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
### Releasing memory
  gc()
    
### Weekly spends on different marketing channels [or Media Investment]
  HA_weekly_ad_spent <- eleckart_HomeAudio[,c(16,20:28)] %>% group_by(Week_date) %>% summarise_all(funs(sum)) %>% data.frame()
  HA_weekly_ad_spent$week <- c(1:49)
  HA_weekly_ad_spent <- HA_weekly_ad_spent[,c(11,2:10)]
    View
  # Plotting Line graph amount spent on different marketing channels
    HA_plots <- list()  # new empty list
    for (i in 2:10) local({
      i <- i
      p2 <- ggplot(HA_weekly_ad_spent,aes(x=HA_weekly_ad_spent[,1],y=HA_weekly_ad_spent[,i])) + 
            geom_line(size=1, color = "steelblue") + geom_point() + theme_bw() + 
            labs(x="Weeks",y= paste0("Spend on ", colnames(HA_weekly_ad_spent[i])," Ads"))
      
      HA_plots[[i-1]] <<- p2  # add each plot into plot list
      
    })
    
  # Plotting all the graphs
  # Note: It takes few seconds to load, please hold in order to come up all the graphs
    plot_grid(HA_plots[[1]],HA_plots[[2]],HA_plots[[3]],HA_plots[[4]],HA_plots[[5]],HA_plots[[6]],
              HA_plots[[7]],HA_plots[[8]],HA_plots[[9]],align ="h")
    



       
#***************************************************************** GamingAccessory ****************************************************************************# 
     
### Weekly Gross Merchandise Value
  GA_weekly_gmv <- aggregate(gmv~Week_date, eleckart_GamingAccessory, sum, na.rm=TRUE)
    
  # Plotting Line graph showing weekly Gross Merchandise Value
    ggplot(GA_weekly_gmv, aes(x=Week_date, y=gmv)) + geom_line(size=1, color = "steelblue") + geom_point() +
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + 
    theme_bw() + labs(x="Weeks",y="GMV") + ggtitle("GamingAccessory - Weekly Gross Merchandise Value") + 
    theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
### Weekly units sold
  GA_weekly_units_sold <- aggregate(units~Week_date, eleckart_GamingAccessory, sum, na.rm=TRUE)
    
  # Plotting Line graph showing weekly orders
    ggplot(GA_weekly_units_sold, aes(x=Week_date, y=units)) + geom_line(size=1, color = "steelblue") + geom_point() +
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + 
    theme_bw() + labs(x="Weeks",y="Number of Units Sold") + ggtitle("GamingAccessory - Weekly Units Sold") + 
    theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
### Aggregated Weekly orders by different payment types
  GA_weekly_units_sold_by_payment_type <- aggregate(units~Week_date + s1_fact.order_payment_type, eleckart_GamingAccessory, sum, na.rm=TRUE)
    
  # Plotting Bar graph showing units sold through different payment types
    ggplot(GA_weekly_units_sold_by_payment_type, aes(x=Week_date,y=units, fill=as.factor(s1_fact.order_payment_type))) + geom_bar(stat="identity",position = "stack", width = 4) + 
    theme_hc(base_size = 18, base_family = "sans") + labs(x="Weeks",y="Number of Units Sold") + 
    ggtitle("GamingAccessory - Weekly Product Units Sold by Different Payment types") + scale_fill_manual("Payment Type: ", values = c("COD" = "navyblue", "Prepaid" = "goldenrod1")) +
    theme(legend.justification="center",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, size = 12,vjust = 0.4),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
### Aggregated units sold for top 10 CameraAccessory Products
  GA_product_units_sold <- aggregate(units~product_analytic_vertical, eleckart_GamingAccessory, sum, na.rm=TRUE)
    
  GA_top_10_products <- GA_product_units_sold[order(GA_product_units_sold$units, decreasing = TRUE),][1:10,]
  
  # Ordering the top 10 products based on the number of units sold for display
  GA_top_10_products$product_analytic_vertical <- factor(GA_top_10_products$product_analytic_vertical, levels = GA_top_10_products$product_analytic_vertical[order(-GA_top_10_products$units)]) 
    
  # Plotting Bar graph showing units sold for top 10 Products
    ggplot(GA_top_10_products, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
    geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Product Vertical",y="Number of Units Sold") + 
    ggtitle("GamingAccessory - Product Units Sold") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle = 45, vjust = 0.6),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
### Weekly average mrp of the products sold
  GA_weekly_avg_product_mrp <- aggregate(product_mrp~Week_date, eleckart_GamingAccessory, mean, na.rm=TRUE)
    
  # Plotting Line graph showing weekly average mrp of the products sold
    ggplot(GA_weekly_avg_product_mrp, aes(x=Week_date, y=product_mrp)) + geom_line(size=1, color = "steelblue") + geom_point() +
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + 
    theme_bw() + labs(x="Weeks",y="Average Product MRP") + ggtitle("GamingAccessory - Weekly Average Product MRP") + 
    theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
### Aggregated Product units sold on weekdays/weekends
  GA_wday_units_sold <- aggregate(units~wday, eleckart_GamingAccessory, sum, na.rm=TRUE)
    
  GA_wday_units_sold$wday <- factor(GA_wday_units_sold$wday,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
  # Plotting Bar graph showing units sold on weekdays/weekends
    ggplot(GA_wday_units_sold, aes(x=as.factor(wday), y=units, fill = as.factor(wday))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
    geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Weekdays",y="Number of Units Sold") + 
    ggtitle("GamingAccessory - Units Sold on Weekdays/Weekend") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
### Units sold of special sale day [i.e. on different holidays]
  GA_special_sale_day_units_sold <- subset(eleckart_GamingAccessory, special_sale_day != "Regular Day")
    
  GA_special_sale_day_units_sold <- aggregate(units~special_sale_day, GA_special_sale_day_units_sold, sum, na.rm=TRUE)
    
  # Ordering the special sales day based on the number of units sold
  GA_special_sale_day_units_sold$special_sale_day <- factor(GA_special_sale_day_units_sold$special_sale_day, levels = GA_special_sale_day_units_sold$special_sale_day[order(-GA_special_sale_day_units_sold$units)]) 
  
  # Plotting Bar graph showing units sold on different holiday seaseons
    ggplot(GA_special_sale_day_units_sold, aes(x=as.factor(special_sale_day), y=units, fill = as.factor(special_sale_day))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Paired") +
    geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Special Sale Days",y="Number of Units Sold") + 
    ggtitle("GamingAccessory - Units Sold on Special Sale Day") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
### Releasing memory
  gc()
    
### Weekly spends on different marketing channels [or Media Investment]
  GA_weekly_ad_spent <- eleckart_GamingAccessory[,c(16,20:28)] %>% group_by(Week_date) %>% summarise_all(funs(sum)) %>% data.frame()
  GA_weekly_ad_spent$week <- c(1:53)
  GA_weekly_ad_spent <- GA_weekly_ad_spent[,c(11,2:10)]

  # Plotting Line graph amount spent on different marketing channels
    GA_plots <- list()  # new empty list
    for (i in 2:10) local({
      i <- i
      p3 <- ggplot(GA_weekly_ad_spent,aes(x=GA_weekly_ad_spent[,1],y=GA_weekly_ad_spent[,i])) + 
            geom_line(size=1, color = "steelblue") + geom_point() + theme_bw() + 
            labs(x="Weeks",y= paste0("Spend on ", colnames(GA_weekly_ad_spent[i])," Ads"))
      
      GA_plots[[i-1]] <<- p3  # add each plot into plot list
      
    })
    
  # Plotting all the graphs
  # Note: It takes few seconds to load, please hold in order to come up all the graphs
    plot_grid(GA_plots[[1]],GA_plots[[2]],GA_plots[[3]],GA_plots[[4]],GA_plots[[5]],GA_plots[[6]],
              GA_plots[[7]],GA_plots[[8]],GA_plots[[9]],align ="h")
    

 