# AutosRUs: MechaCar Statistical Analysis

## Overview of Project
AutosRUs’ newest prototype, the MechaCar, is suffering from production troubles that are blocking the manufacturing team’s progress. AutosRUs’ upper management has called on the data analytics team to review the production data for insights that may help the manufacturing team.

In this challenge, I will help the data analytics team do the following:

* Perform multiple linear regression analysis to identify which variables in the dataset predict the mpg of MechaCar prototypes
* Collect summary statistics on the pounds per square inch (PSI) of the suspension coils from the manufacturing lots
* Run t-tests to determine if the manufacturing lots are statistically different from the mean population
* Design a statistical study to compare vehicle performance of the MechaCar vehicles against vehicles from other manufacturers. For each statistical analysis, you’ll write a summary interpretation of the findings.

## Deliverables:
This challenge consists of three technical analysis deliverables and a proposal for further statistical study. I have created the following:

1. ***Deliverable 1:*** Linear Regression to Predict MPG
2. ***Deliverable 2:*** Summary Statistics on Suspension Coils
3. ***Deliverable 3:*** T-Test on Suspension Coils
4. ***Deliverable 4:*** Design a Study Comparing the MechaCar to the Competition


## What is Used?

* Data Source: `MechaCar_mpg.csv` and `Suspension_Coil.csv`
* Data Tools:  `tidyverse`, `dplyr`, `ggplot2` within the `MechaCarChallenge.RScript`.
* Software: `RStudio` and `R`

![logo](https://github.com/mpournaras/MechaCar_statistical_Analysis/blob/main/Resources/AutoRUs_Logo.png)

# Deliverable 1:  
## Linear Regression to Predict MPG
### Requirements:

The `MechaCar_mpg.csv` dataset contains mpg test results for 50 prototype MechaCars. The MechaCar prototypes were produced using multiple design specifications to identify ideal vehicle performance. Multiple metrics, such as vehicle length, vehicle weight, spoiler angle, drivetrain, and ground clearance, were collected for each vehicle. Using R, I will design a linear model that predicts the mpg of MechaCar prototypes using several variables from the `MechaCar_mpg.csv file`.  

### Deliverables:

- The `MechaCar_mpg.csv` file is imported and read into a dataframe
- An RScript is written for a linear regression model to be performed on all six variables
- An RScript is written to create the statistical summary of the linear regression model with the intended p-values
- There is a summary that addresses all three questions


#### Results:

**Linear Regression Analysis:**

![d1](https://github.com/mpournaras/MechaCar_statistical_Analysis/blob/main/Resources/mecha_mpg_LR.png)				

**Model**= mpg =  (6.267)**vehicle_length** + (0.0012)**vehicle_weight** + (0.0688)**spoiler_angle** + (3.546)**ground_clearance** + (-3.411)**AWD** + (-104.0)


**Statistical Summary:**

![d1](https://github.com/mpournaras/MechaCar_statistical_Analysis/blob/main/Resources/mecha_mpg_summary.png)


From the above output we can see that:

1. The **vehicle length**, and **vehicle ground clearance** are statistically unlikely to provide random amounts of variance to the model. In other words, the vehicle length and vehicle ground clearance have a significant impact on miles-per-gallon on the MechaCar prototype. On the other hand,
the **vehicle weight**, **spoiler angle**, and **All Wheel Drive (AWD)** have p-Values that indicate they provide a random amount of variance to the model.  

2. The p-Value for this model, 5.35e-11, is considerably smaller than the assumed significance level of 0.05. This indicates there is sufficient evidence to **reject our null hypothesis**, which further shows that the slope of this linear model is **not zero**.

3.  This linear model has a r-squared value of **0.7149**, which means that roughly 71% of all mpg predictions will be determined by this model. This multiple regression model **does effectively predict** the MPG of the MechaCar prototype. Further, with the adjusted r-squared value only being .032 less at a value of **.0683** we can infer that our model is not over-fit.


# Deliverable 2:  
## Summary Statistics on Suspension Coils
### Requirements:

The MechaCar Suspension_Coil.csv dataset contains the results from multiple production lots. In this dataset, the weight capacities of multiple suspension coils were tested to determine if the manufacturing process is consistent across production lots. Using my knowledge of R, I will create a summary statistics table to show:

- The suspension coil’s PSI continuous variable across all manufacturing lots
- The following PSI metrics for each lot: mean, median, variance, and standard deviation.

### Deliverables:

- The Suspension_Coil.csv file is imported and read into a dataframe
- An RScript is written to create a total summary dataframe that has the mean, median, variance, and standard deviation of the PSI for all manufacturing lots
- An RScript is written to create a lot summary dataframe that has the mean, median, variance, and standard deviation for each manufacturing lot
- There is a summary that addresses the design specification requirement for all the manufacturing lots and each lot individually


### Results:

First, a summary of all coil lots:

![d2](https://github.com/mpournaras/MechaCar_statistical_Analysis/blob/main/Resources/mecha_coil_totalsummary.png)

Digging a little more into each of the 3 lots:

![d2](https://github.com/mpournaras/MechaCar_statistical_Analysis/blob/main/Resources/mecha_coil_lot_summary.png)

Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually? Understanding that the design specifications for the MechaCar suspension coils mandate that **the variance of the suspension coils cannot exceed 100 pounds per square inch (PSI)**.

* The entire population of the 3 lots **is within specification** with a variance of **62.3 PSI**, well within the 100 PSI mandated variance.  
* When broken down by lot, the results change a little bit! It is important to note that while the mean and median PSI are quite similar between the lots, **lot 3 has a variance of 170 PSI**. That is 70% over the mandated range specification. Production issues can be narrowed down to that single lot. The box plot below elegantly shows the wild variation in lot 3 compared to lots 1 and 2.

![d2](https://github.com/mpournaras/MechaCar_statistical_Analysis/blob/main/Resources/mecha_coil_lot_plot.png?raw=true)


# Deliverable 3:  
## t-Tests on Suspension Coils
### Requirements:

Using my knowledge of R, I need to perform t-tests to determine if all manufacturing lots and each lot individually are statistically different from the population mean of 1,500 pounds per square inch.

### Deliverables

- An RScript is written for t-test that compares all manufacturing lots against mean PSI of the population
- An RScript is written for three t-tests that compare each manufacturing lot against mean PSI of the population
- There is a summary of the t-test results across all manufacturing lots and for each lot

### Results
The next step is to conduct a t-test on the suspension coil data to determine whether there is a statistical difference between the mean of this provided sample dataset and a hypothesized, potential population dataset. Using the presumed population mean of **1500 PSI**, we conclude the following:

* First let's look at the t-test results across all manufacturing lots:

![d3](https://github.com/mpournaras/MechaCar_statistical_Analysis/blob/main/Resources/mecha_coil_t-test_all_lots.png)

From this test we can conclude the **true mean of the sample is 1498.78 PSI**, which we also saw in the summary statistics in Deliverable 2.  With a **p-Value of 0.06**, slightly above the accepted significance level of 0.05, there is **not quite enough evidence to reject the null hypothesis**.  In other words, the mean PSI of all three of these manufacturing lots combined is statistically similar to the presumed population mean of 1500 PSI. 

* Let's take a look at the 3 individual lots:

![d3](https://github.com/mpournaras/MechaCar_statistical_Analysis/blob/main/Resources/mecha_coil_t-test_individual_lots.png)

1) Lot 1: The sample actually has the **true sample mean of 1500 PSI**. With a **p-Value of 1.0**, we absolutely cannot reject the null hypothesis with the evidence provided.
2) Lot 2 has a similar outcome with a **sample mean of 1500.02 PSI**. With a **p-Value of 0.61**, we once again do not have sufficient evidence to reject the null hypothesis, and the sample mean and the population mean of 1500 are statistically similar.
3) Lot 3, not surprisingly, is a different scenario. Here **the sample mean is 1496.14** and the **p-Value is 0.04**, which is lower than the common significance level of 0.05. This evidence indicates we **can reject the null hypothesis** that there is no statistical difference between this sample mean and the presumed population mean.

Once again, the data indicates something went awry in the production of coils in lot 3.

# Deliverable 4:  
## Study Design: MechaCar vs Competition
### Requirements:

Using my knowledge of R, design a statistical study to compare performance of the MechaCar vehicles against the performance of vehicles from other manufacturers.

### Deliverables:

The statistical study design has the following:
- A metric to be tested is mentioned
- A null hypothesis or an alternative hypothesis is described
- A statistical test is described to test the hypothesis


### The Study:

**Data:** This study would involve collecting data on MechaCar and its comparable models across several different manufacturers over the last 3 years.
* What are the competitions' comparable models, 
* Which cars will MechaCar be competing with head-to-head? which cars will be included in the study?
* Which factors will look at the study to determine the relevant to selling price?
 

**Metrics:**
Collecting data for comparable models across all major manufacturers for past 3 years for the following metrics:

*  Safety Rating: **Independent Variable**
*  MPG (Gasoline Efficiency): **Independent Variable**
*  Cargo Volume: **Independent Variable**
*  Passenger Volume: **Independent Variable**
*  MSRP: **Independent Variable**
*  Resale Value: **Independent Variable**
*  Units Sold/ Year: **Dependent Variable**


#### Hypothesis: Null and Alternative
After determining which factors are key for the MechaCar's sales:

 * Null Hypothesis (Ho): Sales of cars similar to MechaCar are based on cargo and passenger volume.
 * Alternative Hypothesis (Ha): Sales of cars similar to MechaCar are NOT based on cargo and passenger volume.
 
#### Statistical Tests
A **multiple linear regression** would be used to determine the factors that have the highest correlation/predictability with the units sold per year (dependent variable); which combination has the greatest impact on sales (it may be all of them!)



##### Michael Pournaras
