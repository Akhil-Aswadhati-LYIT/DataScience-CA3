# Import two data sets from the "FIles" menu at bottom-right corner, which are prsent in the working
# directory
library(readr)
alcohol <- read_csv("alcohol.csv")
View(alcohol)
str(alcohol)

CrimeRate <- read_csv("CrimeRate.csv")
View(CrimeRate)
str(CrimeRate)

# Load Alcohol consumption data and CrimeRate data to a dataframe
alcohol_ds <- data.frame(alcohol)
crimerate_ds <- data.frame(CrimeRate)
crimerate_ds <- crimerate_ds[-1]

View(alcohol_ds)
View(crimerate_ds)

# check if there are any NULL values in the dataframes
sum(is.na(alcohol_ds))
sum(is.na(crimerate_ds))

# All the data for the crime types is in quarters. Add them to make the data simple

crimerate_ds$'2003T' <- sum(crimerate_ds$X2003Q1, crimerate_ds$X2003Q2, crimerate_ds$X2003Q3, 
                              crimerate_ds$X2003Q4)

crimerate_ds$'2004T' <- sum(crimerate_ds$X2004Q1, crimerate_ds$X2004Q2, crimerate_ds$X2004Q3, 
                             crimerate_ds$X2004Q4)

crimerate_ds$'2005T' <- sum(crimerate_ds$X2005Q1, crimerate_ds$X2005Q2, crimerate_ds$X2005Q3, 
                             crimerate_ds$X2005Q4)

crimerate_ds$'2006T' <- sum(crimerate_ds$X2006Q1, crimerate_ds$X2006Q2, crimerate_ds$X2006Q3, 
                             crimerate_ds$X2006Q4)

crimerate_ds$'2007T' <- sum(crimerate_ds$X2007Q1, crimerate_ds$X2007Q2, crimerate_ds$X2007Q3, 
                             crimerate_ds$X2007Q4)

crimerate_ds$'2008T' <- sum(crimerate_ds$X2008Q1, crimerate_ds$X2008Q2, crimerate_ds$X2008Q3, 
                             crimerate_ds$X2008Q4)

crimerate_ds$'2009T' <- sum(crimerate_ds$X2009Q1, crimerate_ds$X2009Q2, crimerate_ds$X2009Q3, 
                             crimerate_ds$X2009Q4)

crimerate_ds$'2010T' <- sum(crimerate_ds$X2010Q1, crimerate_ds$X2010Q2, crimerate_ds$X2010Q3, 
                             crimerate_ds$X2010Q4)

crimerate_ds$'2011T' <- sum(crimerate_ds$X2011Q1, crimerate_ds$X2011Q2, crimerate_ds$X2011Q3, 
                            crimerate_ds$X2011Q4)

crimerate_ds$'2012T' <- sum(crimerate_ds$X2012Q1, crimerate_ds$X2012Q2, crimerate_ds$X2012Q3, 
                            crimerate_ds$X2012Q4)

crimerate_ds$'2013T' <- sum(crimerate_ds$X2013Q1, crimerate_ds$X2013Q2, crimerate_ds$X2013Q3, 
                            crimerate_ds$X2013Q4)

crimerate_ds$'2014T' <- sum(crimerate_ds$X2014Q1, crimerate_ds$X2014Q2, crimerate_ds$X2014Q3, 
                            crimerate_ds$X2014Q4)

crimerate_ds$'2015T' <- sum(crimerate_ds$X2015Q1, crimerate_ds$X2015Q2, crimerate_ds$X2015Q3, 
                            crimerate_ds$X2015Q4)

crimerate_ds$'2016T' <- sum(crimerate_ds$X2016Q1, crimerate_ds$X2016Q2, crimerate_ds$X2016Q3, 
                            crimerate_ds$X2016Q4)

# create a seperate dataset for the created columns

crimerate_ds_total <- data.frame(crimerate_ds[65:78])
View(crimerate_ds_total)

years <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
totals <- c(45040, 44174, 50679, 52441, 57848, 59011, 53962, 49964, 46050, 42647, 38152, 37548, 37596,
             34730)

# with the obtained values, create a new dataset so that the data will be in columns.
# as it will be easy to compare

modified_crimerate <- data.frame(years, totals)

View(modified_crimerate)
str(modified_crimerate)
str(alcohol_ds)

# plot a histogram for the data
# To use histogram, use library lattice
library(lattice)
# histogram is to visualize statistical data in different intervals
histogram(~TOTAL.CONSUMPTION.IN.LITRES, data = alcohol_ds)

# normality test carried out, as it tells the data is normally distributed or not
# normality check for independent variable
normality_test_alcohol <- shapiro.test(alcohol_ds$TOTAL.CONSUMPTION.IN.LITRES)
normality_test_alcohol

# normality check for dependent variable
normality_test_crimerate <- shapiro.test(modified_crimerate$totals)
normality_test_crimerate 

# From these both operations, p value is greater than 0.05. So we can say that 
# both are normally distributed.

# normality check using q-q plot
qqnorm(alcohol_ds$TOTAL.CONSUMPTION.IN.LITRES)
qqline(alcohol_ds$TOTAL.CONSUMPTION.IN.LITRES, col = 'red')

# carry out power analysis to determine the sample size of dataset
install.packages("pwr")
library(pwr)
# check for effective size
effective_size <- cohen.ES(test = "r", size = "large")
effective_size

power_analysis <- pwr.r.test(n= NULL, r = 0.5, sig.level = 0.05, power = 0.95, 
                             alternative = "two.sided")
power_analysis

plot(power_analysis)

# we got the n value as 46. it is the optimal sample size

# As the required operation is to determine the relation between two entities, both the entities
# are normally distributed and the data is contin ous data. So we use pearson test.

test <- cor.test(modified_crimerate$totals, alcohol_ds$TOTAL.CONSUMPTION.IN.LITRES, 
                 method = 'pearson', exact = FALSE)
test

# p value is greater than 0.05. So, the proportionality between two entities is very very low.

