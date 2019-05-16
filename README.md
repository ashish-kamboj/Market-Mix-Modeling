## Problem Statement
Build a  Market Mix Model for one of the e-commerce firm (specializing in electronic products). They have a detailed data available about their marketing spent for a couple of years which includes spending on commercials, online campaigns, and pricing and promotion strategies.

In order to make marketing budget for the next year need to develop a market mix model to observe the actual impact of different marketing variables over the last year and based on the understanding of the model will have to recommend the optimal budget allocation for different marketing levers.

## Data Understanding
Data is from July 2015 to June 2016. The data consists of the following types of information:

**Order level data**
 - **FSN ID:** The unique identification of each SKU
 - **Order Date:** Date on which the order was placed
 - **Order ID:** The unique identification number of each order
 - **Order item ID:** Suppose you order 2 different products under the same order, it generates 2 different order Item IDs under the same order ID; orders are tracked by the Order Item ID.
 - **GMV:** Gross Merchandise Value or Revenue
 - **Units:** Number of units of the specific product sold
 - **Order payment type:** How the order was paid – prepaid or cash on delivery
 - **SLA:** Number of days it typically takes to deliver the product
 - **Cust id:** Unique identification of a customer
 - **Product MRP:** Maximum retail price of the product
 - **Product procurement SLA:** Time typically taken to procure the product
 
 Apart from this, the following information is also available:
 - Monthly spends on various advertising channels
 - Days when there was any special sale
 - Monthly NPS score – this may work as a proxy to ‘voice of customer’

## Solution
1. **Data Exploration**
	- variable Identification
	- Univariate and Bi-Variate Analysis

2. **Data Prepartion**
	- Missing Values and Outlier Treatment
	- Variable Transformation
	- Unified date format

3. **Feature Engineering**
	- Feature Extraction and Selection
	- Variable Interaction
	- Feature Creation

4. **Model Building**
	- Basic linear Model
	- Multiplicative Model
	- Koyck Model
	- Distributed Lag Model
	- Multiplicative + Distributed Lag Model

5. **Model Evaluation**
  - Performed 5 fold cross validation
