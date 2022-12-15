library(tidyr)
library(ggplot2)
library(dplyr)
library(purrr)
library(stats)
library(naniar)
library(stringr)
library(tidyverse)


## DATA CLEANING AND TRANSFORMATION 
## Import data
brooklyn.2016 <- read.csv(file = 'Downloads/2016_brooklyn.csv',skip=4,header = T,strip.white=TRUE)
brooklyn.2017 <- read.csv(file = 'Downloads/2017_brooklyn.csv',skip=4,header = T,strip.white=TRUE)
brooklyn.2018 <- read.csv(file = 'Downloads/2018_brooklyn.csv',skip=4,header = T,strip.white=TRUE)
brooklyn.2019 <- read.csv(file = 'Downloads/2019_brooklyn.csv',skip=4,header = T,strip.white=TRUE)
brooklyn.2020 <- read.csv(file = 'Downloads/2020_brooklyn.csv',skip=6,header = T,strip.white=TRUE)

## Rename columns
names(brooklyn.2016) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot'
                          ,'easement','bldclasscurr','address','aptnum','zip','resunits','comunits'
                          ,'totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale'
                          ,'price','date') 
names(brooklyn.2017) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot'
                          ,'easement','bldclasscurr','address','aptnum','zip','resunits','comunits'
                          ,'totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale'
                          ,'price','date') 
names(brooklyn.2018) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot'
                          ,'easement','bldclasscurr','address','aptnum','zip','resunits','comunits'
                          ,'totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale'
                          ,'price','date') 
names(brooklyn.2019) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot'
                          ,'easement','bldclasscurr','address','aptnum','zip','resunits','comunits'
                          ,'totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale'
                          ,'price','date') 
names(brooklyn.2020) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot'
                          ,'easement','bldclasscurr','address','aptnum','zip','resunits','comunits'
                          ,'totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale'
                          ,'price','date') 

## Re-format, change data types, pay attention to white space and special characters. Take care that your data is standardized across years.

## Replace - with na
brooklyn.2016[brooklyn.2016 == "-"] <- NA
brooklyn.2017[brooklyn.2017 == "-"] <- NA
brooklyn.2018[brooklyn.2018 == "-"] <- NA
brooklyn.2019[brooklyn.2019 == "-"] <- NA
brooklyn.2020[brooklyn.2020 == "-"] <- NA

## Replace "," in numeric columns
brooklyn.2016[c(12:17,20)] <- lapply(brooklyn.2016[c(12:17,20)], gsub, pattern = ',', replacement = '')
brooklyn.2017[c(12:17,20)] <- lapply(brooklyn.2017[c(12:17,20)], gsub, pattern = ',', replacement = '')
brooklyn.2018[c(12:17,20)] <- lapply(brooklyn.2018[c(12:17,20)], gsub, pattern = ',', replacement = '')
brooklyn.2019[c(12:17,20)] <- lapply(brooklyn.2019[c(12:17,20)], gsub, pattern = ',', replacement = '')
brooklyn.2020[c(12:17,20)] <- lapply(brooklyn.2020[c(12:17,20)], gsub, pattern = ',', replacement = '')

## Strip whitespace in numeric columns
brooklyn.2016[c(12:17,20)] <- lapply(brooklyn.2016[c(12:17,20)], gsub, pattern = ' ', replacement = '')
brooklyn.2017[c(12:17,20)] <- lapply(brooklyn.2017[c(12:17,20)], gsub, pattern = ' ', replacement = '')
brooklyn.2018[c(12:17,20)] <- lapply(brooklyn.2018[c(12:17,20)], gsub, pattern = ' ', replacement = '')
brooklyn.2019[c(12:17,20)] <- lapply(brooklyn.2019[c(12:17,20)], gsub, pattern = ' ', replacement = '')
brooklyn.2020[c(12:17,20)] <- lapply(brooklyn.2020[c(12:17,20)], gsub, pattern = ' ', replacement = '')

## Strip $ in price
brooklyn.2016[c(20)] <- lapply(brooklyn.2016[c(20)], gsub, pattern = '\\$', replacement = '')
brooklyn.2017[c(20)] <- lapply(brooklyn.2017[c(20)], gsub, pattern = '\\$', replacement = '')
brooklyn.2018[c(20)] <- lapply(brooklyn.2018[c(20)], gsub, pattern = '\\$', replacement = '')
brooklyn.2019[c(20)] <- lapply(brooklyn.2019[c(20)], gsub, pattern = '\\$', replacement = '')
brooklyn.2020[c(20)] <- lapply(brooklyn.2020[c(20)], gsub, pattern = '\\$', replacement = '')

## Convert chr to numeric
brooklyn.2016[, c(12:17,20)] <- sapply(brooklyn.2016[, c(12:17,20)], as.numeric)
brooklyn.2017[, c(12:17,20)] <- sapply(brooklyn.2017[, c(12:17,20)], as.numeric)
brooklyn.2018[, c(12:17,20)] <- sapply(brooklyn.2018[, c(12:17,20)], as.numeric)
brooklyn.2019[, c(12:17,20)] <- sapply(brooklyn.2019[, c(12:17,20)], as.numeric)
brooklyn.2020[, c(12:17,20)] <- sapply(brooklyn.2020[, c(12:17,20)], as.numeric)

## Convert date into datetime
brooklyn.2016$date <- as.Date(brooklyn.2016$date , format = "%m/%d/%Y")
brooklyn.2017$date <- as.Date(brooklyn.2017$date , format = "%m/%d/%y")
brooklyn.2018$date <- as.Date(brooklyn.2018$date , format = "%m/%d/%y")
brooklyn.2019$date <- as.Date(brooklyn.2019$date , format = "%m/%d/%y")
brooklyn.2020$date <- as.Date(brooklyn.2020$date , format = "%m/%d/%y")


## Merge datasets
nyc <- rbind(brooklyn.2016,brooklyn.2017,brooklyn.2018,brooklyn.2019,brooklyn.2020[-1,])
head(nyc)

## Filter the data and make transformations specific to this analysis 
nyc.filtered <- nyc[grepl('A|R',nyc$bldclasssale),]
nyc.filtered <- nyc.filtered %>%
  filter(nyc.filtered$resunits ==1)
nyc.filtered <- nyc.filtered %>%
  filter(nyc.filtered$totunits ==1)
nyc.filtered <- nyc.filtered %>%
  filter(nyc.filtered$grosssqft >0)
nyc.filtered <- nyc.filtered[!is.na(nyc.filtered$price),]

str(nyc)
str(nyc.filtered)

## EDA AND FEATURE ENGINEERING
#Filter out price 
nyc.filtered <- nyc.filtered %>%
  filter(nyc.filtered$price > 10 & nyc.filtered$price <4060000) #Filter out the 1st and 99th percentile of data (excluding price = 0) 
quantile(nyc.filtered$price, probs = seq(0, 0.99, 0.01))
#nyc.filtered <- nyc.filtered %>%
#  filter(nyc.filtered$yrbuilt >0)

## Plot histogram
#Price
ggplot(data = nyc.filtered) +
  geom_histogram(mapping = aes(x = price),bins = 10)+
  xlim(c(0, 2000000))

#Pairplots
pairs(nyc.filtered[, c(13,15:17,20)])
summary(nyc.filtered)

plot(nyc.filtered$yrbuilt,log(nyc.filtered$price))
plot(nyc.filtered$yrbuilt,nyc.filtered$price)
#Find na - Impute
install.packages("mice")
library(mice)
sum(is.na(nyc.filtered$comunits))
nyc.filtered$comunits[is.na(nyc.filtered$comunits)] <- median(nyc.filtered$comunits,na.rm = TRUE)

##FEATURE ENGINEERING

#Find quarter-year
library(lubridate)
nyc.filtered <- nyc.filtered %>%
  mutate(qtr = quarter(date, with_year = T))
nyc.filtered$qtr <- sapply(nyc.filtered$qtr, as.character)
#Find building class number
nyc.filtered$bldclasscat.nr <- substr(nyc.filtered$bldclasscat, 1, 2)

#Find building class at sale number
nyc.filtered$bldclasssale.nr <- substr(nyc.filtered$bldclasssale, 1, 2)

#Categorize zipcode based on this list https://localistica.com/usa/ny/brooklyn/zipcodes/highest-household-income-zipcodes/
nyc.filtered$zip.wealth2[nyc.filtered$zip %in% c(11201,11215,11234,11217,11231,11228,11209,11210)] <- '5' #wealthiest
nyc.filtered$zip.wealth2[nyc.filtered$zip %in% c(11236,11238,11229,11203,11218,11214,11222,11230)] <- '4' #second wealthiest
nyc.filtered$zip.wealth2[nyc.filtered$zip %in% c(11223,11204,11235,11225,11220,11226,11232,11205)] <- '3'
nyc.filtered$zip.wealth2[nyc.filtered$zip %in% c(11208,11219,11213,11216,11207,11211,11237)] <- '2'
nyc.filtered$zip.wealth2[nyc.filtered$zip %in% c(11233,11221,11224,11212,11206,11239,11249)] <- '1'
#Categorize neighborhood wealth based on their coefficients when plotted lm(price~neighborhood)
nyc.filtered$nbh.wealth[nyc.filtered$neighborhood %in% c('BROOKLYN HEIGHTS',"COBBLE HILL",'BOERUM HILL','CARROLL GARDENS','DOWNTOWN-FULTON FERRY','PARK SLOPE','OCEAN PARKWAY-SOUTH','NAVY YARD','COBBLE HILL-WEST','RED HOOK','GOWANUS','PROSPECT HEIGHTS')] <- '5' #wealthiest
nyc.filtered$nbh.wealth[nyc.filtered$neighborhood %in% c('FORT GREENE','CLINTON HILL','PARK SLOPE SOUTH','FLATBUSH-LEFFERTS GARDEN','WILLIAMSBURG-NORTH',	'WINDSOR TERRACE','DOWNTOWN-FULTON MALL','OCEAN PARKWAY-NORTH','GREENPOINT','WILLIAMSBURG-SOUTH','MANHATTAN BEACH','KENSINGTON')] <- '4' #second wealthiest
nyc.filtered$nbh.wealth[nyc.filtered$neighborhood %in% c('FLATBUSH-CENTRAL', 'WILLIAMSBURG-EAST','DOWNTOWN-METROTECH','BAY RIDGE','MIDWOOD','MILL BASIN','SUNSET PARK','CROWN HEIGHTS','BOROUGH PARK','DYKER HEIGHTS','BEDFORD STUYVESANT','BENSONHURST')] <- '3'
nyc.filtered$nbh.wealth[nyc.filtered$neighborhood %in% c('MADISON','WILLIAMSBURG-CENTRAL','BUSHWICK','WYCKOFF HEIGHTS','GRAVESEND','SEAGATE','BERGEN BEACH','SHEEPSHEAD BAY','MARINE PARK','BRIGHTON BEACH',	'BUSH TERMINAL','BATH BEACH')] <- '2'
nyc.filtered$nbh.wealth[nyc.filtered$neighborhood %in% c('FLATBUSH-NORTH','FLATBUSH-EAST',	'CONEY ISLAND','OLD MILL BASIN','FLATLANDS','SPRING CREEK',	'GERRITSEN BEACH','CYPRESS HILLS','CANARSIE',	'EAST NEW YORK','BROWNSVILLE','OCEAN HILL')] <- '1'


## Stair step function
require(leaps)
lm_full = lm(price ~ sale.year+bldclasssale.nr+bldclasscat.nr+taxclasscurr+block+lot+zip.wealth2+landsqft+grosssqft+yrbuilt, data = nyc.filtered)
step(lm_full, direction = 'backward')

## LINEAR REGRESSION
#First version:
model.ln1 <- lm(price ~ comunits + landsqft + grosssqft + yrbuilt + taxclasssale, data = nyc.filtered)
summary(model.ln1)
RMSE_model.ln1 <- sqrt(mean((nyc.filtered$price-model.ln1$fitted.values)^2))
RMSE_model.ln1

#Second version: 
model.ln2 <- lm(price ~ bldclasscat.nr+zip.wealth2 + landsqft + grosssqft + yrbuilt + I(yrbuilt^2)+ qtr + zip.wealth2*grosssqft + bldclasscat.nr*zip.wealth2,data = nyc.filtered)
summary(model.ln2)
RMSE_model.ln2 <- sqrt(mean((nyc.filtered$price-model.ln2$fitted.values)^2))
RMSE_model.ln2

#Third version: 
model.ln3 <- lm(price ~ bldclasscat.nr+zip.wealth2 + sqrt(landsqft) + log(grosssqft) + yrbuilt + I(yrbuilt^2) +qtr+ zip.wealth2*grosssqft,data = nyc.filtered)
summary(model.ln3)
RMSE_model.ln3 <- sqrt(mean((nyc.filtered$price-model.ln3$fitted.values)^2))
RMSE_model.ln3


#Fourth version:
model.ln4 <- lm(price ~ nbh.wealth+nbh.wealth*grosssqft+lot+qtr+ landsqft + grosssqft + yrbuilt + I(yrbuilt^2) + grosssqft,data = nyc.filtered)
summary(model.ln4)
RMSE_model.ln4 <- sqrt(mean((nyc.filtered$price-model.ln4$fitted.values)^2))
RMSE_model.ln4

#Fifth version: 
model.ln <- lm(price ~ bldclasscat.nr + nbh.wealth + nbh.wealth*grosssqft+zip.wealth2 + landsqft
                + grosssqft + block +qtr ,data = nyc.filtered)
summary(model.ln)
anova(model.ln)
RMSE_model.ln <- sqrt(mean((nyc.filtered$price-model.ln$fitted.values)^2))
RMSE_model.ln

#Sixth version: 
model.ln.withdate <- lm(price ~ nbh.wealth + nbh.wealth*grosssqft+zip.wealth2 + landsqft
               + grosssqft + block +date ,data = nyc.filtered)
summary(model.ln.withdate)
anova(model.ln.withdate)
RMSE_model.ln.withdate <- sqrt(mean((nyc.filtered$price-model.ln.withdate$fitted.values)^2))
RMSE_model.ln.withdate



str(nyc.filtered)
saveRDS(list(model=model.ln, data=nyc.filtered), file = 'Downloads/linhle.RDS') 
############################################
#Multiple comparison
TukeyHSD(aov(price ~ qtr ,data = nyc.filtered))

#Filter data qtr3 & 4 only
nyc.qtr34 <- nyc.filtered %>%
  filter(nyc.filtered$qtr %in% c('2020.3','2020.4'))
nyc.excl.qtr34 <- nyc.filtered %>%
  filter(!nyc.filtered$qtr %in% c('2020.3','2020.4'))
nyc.qtr3 <- nyc.filtered %>%
  filter(!nyc.filtered$qtr %in% c('2020.3'))
nyc.qtr4 <- nyc.filtered %>%
  filter(!nyc.filtered$qtr %in% c('2020.4'))
str(nyc.qtr34)
str(nyc.excl.qtr34)
#Model on qtr 3 4 only
model.ln.34 <- lm(price ~ bldclasscat.nr + nbh.wealth + nbh.wealth*grosssqft+zip.wealth2 + landsqft
               + grosssqft + block +qtr ,data = nyc.qtr34)
summary(model.ln.34)

#Plot
#nyc.qtr34 <- na.omit(nyc.qtr34)
qtr3 <- tapply(nyc.qtr34$price[nyc.qtr34$qtr == '2020.3'],nyc.qtr34$nbh.wealth[nyc.qtr34$qtr=='2020.3'],mean)
qtr4 <- tapply(nyc.qtr34$price[nyc.qtr34$qtr == '2020.4'],nyc.qtr34$nbh.wealth[nyc.qtr34$qtr=='2020.4'],mean)
bind.df <- cbind(qtr3,qtr4)
library(ggplot2)
library(zoo)
autoplot(zoo(bind.df),facet = NULL)
  +geom_point()
  #+labs(title = "Quarterly Prices by Neighborhood Group") 
  +xlab("Neighborhood Group") 
  +ylab("Price") 
#matplot(y=cbind(qtr3,qtr4),xaxt="n")
#ggplot(y=cbind(qtr3,qtr4),xaxt="n")
#title('Quarterly Price')
#axis(side=1,at=1:5,labels=names(qtr4))


#Find duplicated rows

dup.address <- nyc.qtr34[duplicated(nyc.qtr34$address),]
dup.address[order(dup.address$address),]


#Train model on data excluding qtr34 
model.ln.excl.34 <- lm(price ~  nbh.wealth + nbh.wealth*grosssqft+zip.wealth2 + landsqft
                  + grosssqft + block +date,data = nyc.excl.qtr34)
model.qtr34.predict <- predict(model.ln.excl.34,newdata=nyc.qtr34[,-20])
mean(predict(model.ln.excl.34,newdata=nyc.qtr34[,-20]) - nyc.qtr34$price)
length(predict(model.ln.excl.34,newdata=nyc.qtr34[,-20]))
#Test for statistical significance
t.test(model.qtr34.predict, nyc.qtr34$price)

#Predict Q3 & Q4
model.qtr3.predict <- predict(model.ln.excl.34,newdata=nyc.qtr3[,-20])
model.qtr4.predict <- predict(model.ln.excl.34,newdata=nyc.qtr4[,-20])
mean(model.qtr3.predict- nyc.qtr3$price)
mean(model.qtr4.predict- nyc.qtr4$price)

#Test for statistical significance
t.test(model.qtr3.predict, model.qtr4.predict,interval = 'confidence',conf.level = 0.95)

#Actual Mean of 2 quarters
mean(nyc.qtr3$price)
mean(nyc.qtr4$price)

#Predict from model to data qtr34
model.qtr34.predict2 <- predict(model.ln,newdata=nyc.qtr34[,-20])
mean(model.qtr34.predict2 - nyc.qtr34$price)
length(predict(model.qtr34.predict2,newdata=nyc.qtr34[,-20]))

#Model with nbh and time only
model.nbh <- lm(price ~ nbh.wealth * qtr ,data = nyc.qtr34)
summary(model.nbh)
anova(model.nbh)
model.nbh <- sqrt(mean((nyc.filtered$price-model.nbh$fitted.values)^2))
model.nbh

#Model with sqftsize and time only
model.grosssqft <- lm(price ~ grosssqft * qtr ,data = nyc.qtr34)
summary(model.grosssqft)
anova(model.grosssqft)
model.grosssqft <- sqrt(mean((nyc.filtered$price-model.nbh$fitted.values)^2))
model.grosssqft

#Plot
ggplot(data = nyc.qtr34, aes(x=grosssqft, y=price)) + geom_line(aes(colour=qtr)) + geom_smooth(method = "lm")
ggplot(data=nyc.qtr34, aes(x=grosssqft, y=price, group = qtr, colour = as.factor(qtr)))    
ggplot() +
  geom_smooth(data = nyc.qtr34, aes(x = grosssqft, y = price, color = qtr), method = lm)
  labs(title = "Quarterly Prices by House Sizes") +
  xlab("Gross Square Feet") +
  ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5))
  #geom_smooth(data = nyc.qtr34, aes(x = grosssqft, y = price), method = lm) 

#Model with building class nr and time only
  model.bldclasscat.nr <- lm(price ~ bldclasscat * qtr ,data = nyc.qtr34)
  summary(model.bldclasscat.nr)
  anova(model.bldclasscat.nr)
  model.bldclasscat.nr <- sqrt(mean((nyc.filtered$price-model.bldclasscat.nr$fitted.values)^2))
  model.bldclasscat.nr

  ## Test iid of model
  res <- residuals(model.ln)
  
  ks.test(res, pnorm)
  hist(res, main = "Histogram of Residuals", xlab = "Residuals")
  
  require(lmtest)
  dwtest(model.ln)
  bptest(model.ln)
# Houses with highest residuals
  abs <- abs(nyc.qtr34$price-model.qtr34.predict)
  mostresdata <- nyc.qtr34
  mostresdata$abs_resi <- abs
  largest20 <- mostresdata[with(mostresdata,order(-abs_resi)),][1:80,]
  #Group
  library(dplyr)
  
  # Group by count using dplyr
  agg_tbl <- largest20 %>% group_by(nbh.wealth) %>% 
    summarise(total_count=n(),
              .groups = 'drop')
  agg_tbl
  
  # Convert tibble to df
  df2 <- agg_tbl %>% as.data.frame()
  df2
linhle <-readRDS('Downloads/linhle.RDS')
summary(linhle$model)
nrow(linhle$data)

