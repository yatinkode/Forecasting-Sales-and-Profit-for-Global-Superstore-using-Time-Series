# Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series
Global Mart is an online store super giant having worldwide operations. As a sales/operations manager, you want to finalise the plan for the next 6 months. So, you want to forecast the sales and the demand for the next 6 months, that would help you manage the revenue and inventory accordingly.

#### Loading Libraries and theme for ggplot
```R
load.libraries <- c('tidyr','dplyr','ggplot2','lubridate','forecast','tseries','data.table')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)

require(graphics)

#loading theme for ggplot
theme_forecast <- function () { 
  theme_bw(base_size=10, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_rect(fill="gray80", colour=NA),
      plot.background = element_rect(fill="gray96", colour=NA), 
      legend.background = element_rect(fill="white", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      legend.position="bottom"
    )
}
```

#### Loading Data
```R
gs<- read.csv("Global Superstore.csv",stringsAsFactors = F)
```

#### Data Understanding
```R
sum(is.na(gs)) # 41296 NA values present

str(gs)

#As per the business Problem We will use Order date as Timestamp column for out time-series/s
gs$Order.Date<-as.Date(gs$Order.Date,"%d-%m-%Y")
sum(is.na(gs$Order.Date)) #All rows are converted to date

#Segment the whole dataset into the 21 subsets based on the market and the customer segment level
#But First we check for dicrepancies in Market and Segment columns

sum(is.na(gs$Segment)) #No NA values in Segment
prop.table(table(gs$Segment))*100
```
| __Consumer__ | __Corporate__ | __Home Office__ |
|--------------|---------------|-----------------|
| 51.70209     |  30.08189     | 18.21603        |


```R
sum(is.na(gs$Market)) #No NA values in Market
prop.table(table(gs$Market))*100
```
| __Africa__ | __APAC__   | __Canada__ | __EMEA__ |__EU__    |__LATAM__  |__US__     |
|------------|------------|------------|----------|----------|-----------|-----------|
| 8.943264   |  21.450575 | 0.748684   | 9.805030 |19.496978 | 20.070189 | 19.485280 |


```R
prop.table(table(gs$Market,gs$Segment))*100
```

|              | __Consumer__ | __Corporate__ | __Home Office__ |
|--------------|--------------|---------------|-----------------|
| __Africa__   |  4.6422305   | 2.5580035     |  1.7430298      |
| __APAC__     |  11.1113277  | 6.4008579     |  3.9383895      |
| __Canada__   |  0.3938390   | 0.2144668     |  0.1403782      |
| __EMEA__     |  4.9483330   | 3.0688243     |  1.7878729      |
| __EU__       |  10.1111328  | 5.9992201     |  3.3866251      |
| __LATAM__    |  10.3743420  | 5.9524274     |  3.7434198      |
| __US__       |  10.1208813  | 5.8880873     |  3.4763112      |


```R
#Plotting Profit Based on Market-Segment
ggplot(gs,aes(x=Market,y=Profit,fill=Segment)) +
  theme_forecast() + 
  stat_summary(fun.y = sum, geom='bar',colour="black", position='dodge') + 
  theme(plot.title = element_text(hjust = 0.1)) +labs(x = "Market",y = "Sum of Profit",title = "Fig : Profit Based on Market-Segment" )
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/profitmarket.png)

```R
#Get Month-Year of each row
gs$Year_month<-as.character(substr(gs$Order.Date,1,7))

#Subsetting Data by Market and Segment

gs_Con_Afr<-gs[which(gs$Market=="Africa" & gs$Segment=="Consumer"),]    #Contain data where Market is Africa and Segment as Consumer

gs_Con_APAC<-gs[which(gs$Market=="APAC" & gs$Segment=="Consumer"),]     #Contain data where Market is APAC and Segment as Consumer

gs_Con_Can<-gs[which(gs$Market=="Canada" & gs$Segment=="Consumer"),]    #Contain data where Market is Canada and Segment as Consumer

gs_Con_EMEA<-gs[which(gs$Market=="EMEA" & gs$Segment=="Consumer"),]     #Contain data where Market is EMEA and Segment as Consumer

gs_Con_EU<-gs[which(gs$Market=="EU" & gs$Segment=="Consumer"),]         #Contain data where Market is EU and Segment as Consumer

gs_Con_LATAM<-gs[which(gs$Market=="LATAM" & gs$Segment=="Consumer"),]   #Contain data where Market is LATAM and Segment as Consumer

gs_Con_US<-gs[which(gs$Market=="US" & gs$Segment=="Consumer"),]         #Contain data where Market is US and Segment as Consumer

gs_Cor_Afr<-gs[which(gs$Market=="Africa" & gs$Segment=="Corporate"),]   #Contain data where Market is Africa and Segment as Corporate

gs_Cor_APAC<-gs[which(gs$Market=="APAC" & gs$Segment=="Corporate"),]    #Contain data where Market is APAC and Segment as Corporate

gs_Cor_Can<-gs[which(gs$Market=="Canada" & gs$Segment=="Corporate"),]   #Contain data where Market is Canada and Segment as Corporate

gs_Cor_EMEA<-gs[which(gs$Market=="EMEA" & gs$Segment=="Corporate"),]    #Contain data where Market is EMEA and Segment as Corporate

gs_Cor_EU<-gs[which(gs$Market=="EU" & gs$Segment=="Corporate"),]        #Contain data where Market is EU and Segment as Corporate

gs_Cor_LATAM<-gs[which(gs$Market=="LATAM" & gs$Segment=="Corporate"),]  #Contain data where Market is LATAM and Segment as Corporate

gs_Cor_US<-gs[which(gs$Market=="US" & gs$Segment=="Corporate"),]        #Contain data where Market is US and Segment as Corporate

gs_Hom_Afr<-gs[which(gs$Market=="Africa" & gs$Segment=="Home Office"),]    #Contain data where Market is Africa and Segment as Home Office

gs_Hom_APAC<-gs[which(gs$Market=="APAC" & gs$Segment=="Home Office"),]     #Contain data where Market is APAC and Segment as Corporate

gs_Hom_Can<-gs[which(gs$Market=="Canada" & gs$Segment=="Home Office"),]    #Contain data where Market is Canada and Segment as Corporate

gs_Hom_EMEA<-gs[which(gs$Market=="EMEA" & gs$Segment=="Home Office"),]     #Contain data where Market is EMEA and Segment as Corporate

gs_Hom_EU<-gs[which(gs$Market=="EU" & gs$Segment=="Home Office"),]         #Contain data where Market is EU and Segment as Corporate

gs_Hom_LATAM<-gs[which(gs$Market=="LATAM" & gs$Segment=="Home Office"),]   #Contain data where Market is LATAM and Segment as Corporate

gs_Hom_US<-gs[which(gs$Market=="US" & gs$Segment=="Home Office"),]         #Contain data where Market is US and Segment as Corporate


#We calculate the monthly profit of each of the 21 subsets we obtained earlier 
#and then compute the Coefficient of Variation to arrive at the top 2 categories

#we make use of a fuction to calculate the Coefficient of Variation
cal_coeff_var<-function(x){
  
  y<-x %>% select(Year_month,Profit)                     #collecting Year_month and Profit in a data frame
  z <-aggregate(y[-1], by=list(y$Year_month),FUN=sum)    #Aggregating sum of Profits by month
  print(sd(z$Profit)/mean(z$Profit))                     #calculating coefficient of variation as standard deviation(profit)/mean(profit)

  }

#Africa-Consumer
cal_coeff_var(gs_Con_Afr)      #Coefficient of Variation - 1.319585

#APAC-Consumer
cal_coeff_var(gs_Con_APAC)     #Coefficient of Variation - 0.6321323

#Canada-Consumer
cal_coeff_var(gs_Con_Can)      #Coefficient of Variation - 1.395312

#EMEA-Consumer
cal_coeff_var(gs_Con_EMEA)     #Coefficient of Variation - 2.188271

#EU-Consumer
cal_coeff_var(gs_Con_EU)       #Coefficient of Variation - 0.6243052

#LATAM-Consumer
cal_coeff_var(gs_Con_LATAM)    #Coefficient of Variation - 0.6614828

#US-Consumer
cal_coeff_var(gs_Con_US)       #Coefficient of Variation - 1.01239

#Africa-Corporate
cal_coeff_var(gs_Cor_Afr)      #Coefficient of Variation - 1.776105

#APAC-Corporate
cal_coeff_var(gs_Cor_APAC)     #Coefficient of Variation - 0.6980869

#Canada-Corporate
cal_coeff_var(gs_Cor_Can)      #Coefficient of Variation - 1.552775

#EMEA-Corporate
cal_coeff_var(gs_Cor_EMEA)     #Coefficient of Variation - 4.467102

#EU-Corporate
cal_coeff_var(gs_Cor_EU)       #Coefficient of Variation - 0.7638072

#LATAM-Corporate
cal_coeff_var(gs_Cor_LATAM)    #Coefficient of Variation - 0.8111217

#US-Corporate
cal_coeff_var(gs_Cor_US)       #Coefficient of Variation - 1.002409

#Africa-Home Office
cal_coeff_var(gs_Hom_Afr)      #Coefficient of Variation - 1.789996

#APAC-Home Office
cal_coeff_var(gs_Hom_APAC)     #Coefficient of Variation - 1.045978

#Canada-Home Office
cal_coeff_var(gs_Hom_Can)      #Coefficient of Variation - 2.243461

#EMEA-Home Office
cal_coeff_var(gs_Hom_EMEA)     #Coefficient of Variation - 5.880747

#EU-Home Office
cal_coeff_var(gs_Hom_EU)       #Coefficient of Variation - 1.116507

#LATAM-Home Office
cal_coeff_var(gs_Hom_LATAM)    #Coefficient of Variation - 1.175698

#US-Home Office
cal_coeff_var(gs_Hom_US)       #Coefficient of Variation - 1.096147

#The most profitable 2 categories are since variation is least in them
#EU-Consumer		          Coefficient of Variation - 0.6243052
#APAC-Consumer		        Coefficient of Variation - 0.6321323

#Finding The Sales based series for category EU-Consumer 
gs_Con_EU_Sales<-gs_Con_EU %>% select(Year_month,Sales)                                               #filtering out Year_month and Sales columns for EU-Consumer  
gs_Con_EU_Sales_Month <-aggregate(gs_Con_EU_Sales[-1], by=list(gs_Con_EU_Sales$Year_month),FUN=sum)   #aggregating sum of Sales monthwise

#Finding The Quantity based series for category EU-Consumer 
gs_Con_EU_Quantity<-gs_Con_EU %>% select(Year_month,Quantity)                                                 #filtering out Year_month and Sales columns for EU-Consumer 
gs_Con_EU_Quantity_Month <-aggregate(gs_Con_EU_Quantity[-1], by=list(gs_Con_EU_Quantity$Year_month),FUN=sum)  #aggregating sum of Quantity monthwise

#Finding The Sales based series for category APAC-Consumer
gs_Con_APAC_Sales<-gs_Con_APAC %>% select(Year_month,Sales)                                                 #filtering out Year_month and Sales columns for APAC-Consumer
gs_Con_APAC_Sales_Month <-aggregate(gs_Con_APAC_Sales[-1], by=list(gs_Con_APAC_Sales$Year_month),FUN=sum)   #aggregating sum of Quantity monthwise

#Finding The Quantity based series for category APAC-Consumer 
gs_Con_APAC_Quantity<-gs_Con_APAC %>% select(Year_month,Quantity)                                                    #filtering out Year_month and Quantity columns for APAC-Consumer
gs_Con_APAC_Quantity_Month <-aggregate(gs_Con_APAC_Quantity[-1], by=list(gs_Con_APAC_Quantity$Year_month),FUN=sum)   #aggregating sum of Quantity monthwise

#Converting The Date column in the series into Date format and arrange in chronological order of Month-Year
convert_to_date<-function(a){
 
  a$Group.1<-as.Date(paste0("01-", a$Group.1),format = "%d-%Y-%m")  #Appending '01-' at the beginning to convet it in date format
  a<-dplyr::arrange(a, Group.1)                                     #arranging month-year in chronological order
  a<-setnames(a, "Group.1", "Year.Month")                           #Setting appropriate name for month column as 'Year.month'
}

#Arranging month based Sales for Category Consumer-EU in chronological order
gs_Con_EU_Sales_Month<-convert_to_date(gs_Con_EU_Sales_Month)

#Arranging month based Quantity for Category Consumer-EU in chronological order
gs_Con_EU_Quantity_Month<-convert_to_date(gs_Con_EU_Quantity_Month)

#Arranging month based Sales for Category Consumer-APAC in chronological order
gs_Con_APAC_Sales_Month<-convert_to_date(gs_Con_APAC_Sales_Month)

#Arranging month based Quantity for Category Consumer-APAC in chronological order
gs_Con_APAC_Quantity_Month<-convert_to_date(gs_Con_APAC_Quantity_Month)
```
### Time Series Modelling for Sales of EU-Consumer Category
```R
str(gs_Con_EU_Sales_Month)

nrow(gs_Con_EU_Sales_Month)

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later
```
#### Classical Decomposition Method to Forecast Time series Consumer- EU (Sales)
```R
gs_Con_EU_Sales_Month$Year.Month <- seq(1,48)

total_timeser <- ts(gs_Con_EU_Sales_Month$Sales)
indata <- gs_Con_EU_Sales_Month[1:42,]

timeser <- ts(indata$Sales)
plot(timeser,main="Plot for Timeseries of EU-Consumer Sales" ,xlab="Month",ylab="Sales")


# Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
#plot(1, type="n", xlab="", ylab="", xlim=c(0, 50), ylim=c(1000, 50000),main="Smoothened Series for Sales of EU-Consumer")
lines(smoothedseries, col="red", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

timevals_in <- indata$Year.Month
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a Multplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#Since multplicative model fits the smoothened series more appropriately than additive model we will choose multiplicative model
#Formula obtained after multiple trial and errors
lmformula<-as.formula(Sales ~ sin(Month * 0.11) * poly(Month, 1) * cos(Month * 0.31))

lmfit <- lm(lmformula, data = smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)

#Drawing the global prediction
lines(timevals_in, global_pred, col='blue', lwd=2)

legend("bottomright", legend = c("Original","Smooth Series", "Regression Line"),
       text.width = strwidth("1,000,00000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red","blue"),
       title = "Line Types")
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/classicaltseuconsales.png)

```R
#Now, let's look at the locally predictable series. We will remove the trend and seasonality from the series and get local series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l",main="Local series of EU-Consumer-Sales",xlab="Month",ylab="Sales")  # We have found out the local series
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/localclassicaleuconsumersales.png)

```R
#Lets verify whether local series is white noise or not

#ACF test
acf(local_pred,main="ACF plot for local series of Sales of EU-Consumer")     #Lots of points are above cutoff value. It means pairwise relationships are preserved

#PACF test
pacf(local_pred,main="PACF plot for local series of Sales of EU-Consumer")    #Here also many point exceed the cutoff value
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/acfpacfclassicaleuconsales.png)

```R
print(adf.test(local_pred,alternative = "stationary"))   #p-value = 0.01 Series is stationary since p-value below 0.05 in ADF test

print(kpss.test(local_pred))                              #p-value = 0.1 Series is stationary since p-value above 0.05 in KPSS test

#Lets see if the stationary series is weak or strong
armafit <- auto.arima(local_pred)
armafit                              #ARIMA(0,0,0) with zero mean 

tsdiag(armafit)
```

```R
#Now we will get the residual
resi<-local_pred-fitted(armafit)

plot(resi,main="Residual series for Sales of EU-Consumer")

acf(resi,main="ACF plot for residual series in Consumer EU category")      #Almost all points are below cutoff value in ACF
pacf(resi,main="PACF plot for residual series in Consumer EU category")    #Almost all points are below cutoff value in PACF

#Now we check whether the residual is white noise

adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -5.3659, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
#Since p-value for Augmented Dickey-Fuller Test is less than threshold 0.05 it is stationary

kpss.test(resi)
#KPSS Level = 0.041582, Truncation lag parameter = 3, p-value = 0.1
#Since p-value for KPSS test is greater than threshold 0.05 it is stationary
```
#### Model Evaluation EU & Consumer Sales
```R
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- gs_Con_EU_Sales_Month[43:48,]
#timevals_out <- outdata$Year.Month


timevals_out <-  data.frame(Month = outdata$Year.Month)
fcast_arima <- predict(lmfit, timevals_out)
print(fcast_arima)

#MAPE (mean absolute percentage error) for finding out the error in evaluating our model
MAPE_arima <- accuracy(fcast_arima, outdata$Sales)[5]

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales)[5]
MAPE_class_dec                                       #20.16214

#The error is very less so our model is good to go

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

Futurepred <- predict(lmfit,data.frame(Month=seq(1:54)))
Futurepred[49:54]
```
| __Jan 15__ | __Feb 15__| __Mar 15__ | __Apr 15__|__May 15__|__Jun 15__|
|------------|-----------|------------|-----------|----------|----------|
| 63549.62   |  62520.53 | 59828.40   | 56285.73  |52959.78  | 50971.18 |

```R
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black" ,main="Forecasting Consumer-EU Sales by Classical Decomposition",xlab="Month",ylab="Sales")
lines(class_dec_pred, col = "red")
abline(v = 42, col="blue", lwd=2, lty=2)
rect(c(48,0), -1e6, c(54,0), 1e6, col = rgb(0.5,0.5,0.5,1/3), border=NA)

legend("topleft", legend = c("Original","Forecasted"),
       text.width = strwidth("1,000,0000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red"),
       title = "Line Types")
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/forecastclassicaleuconsales.png)

#### Auto ARIMA Method to Forecast Time series Consumer- EU (Sales)

```R
autoarima <- auto.arima(timeser)
autoarima                        #ARIMA(2,1,0)
#ARIMA method predicts that the series is of AR(2) and needed 1 level of differencing

plot(autoarima$x, col="black",main="Plotting for Consumer-EU Sales using Auto-ARIMA",xlab="Month",ylab="Sales")
lines(fitted(autoarima), col="red")
legend("bottomright", legend = c("Original","Forecasted"),
       text.width = strwidth("1,000,000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red"),
       title = "Line Types")
 ```
 ![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/autoarimaeuconsales.png)

```R
tsdiag(autoarima)               #Plotting diagnostics of time series
```
 ![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/tsdiagautoarimaeuconsales.png)
 
```R
#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01
#Hence residual is Stationary or white noise since p-value of ADF test is less than 0.05

kpss.test(resi_auto_arima)
#KPSS Level = 0.05314, Truncation lag parameter = 3, p-value = 0.1
#Hence residual is white noise since p-value of KPSS test greater tha 0.05

#Also, let's evaluate the model using MAPE and forecast the future values
fcast_auto_arima <- predict(autoarima, n.ahead = 12)
fcast_auto_arima$pred[7:12]
```
| __Jan 15__ | __Feb 15__| __Mar 15__ | __Apr 15__|__May 15__|__Jun 15__|
|------------|-----------|------------|-----------|----------|----------|
| 40288.07   |  39651.62 | 40168.29   | 40181.05  |39920.18  | 40065.12 |

```R
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Sales)[5]
MAPE_auto_arima               #28.9226
#MAPE value obtained is pretty good in Auto ARIMA test

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black",main="Forecasting Sales for Consumer-EU using Auto-ARIMA",xlab="Month",ylab="Sales")
lines(auto_arima_pred, col = "red")
abline(v = 42, col="blue", lwd=2, lty=2)
rect(c(48,0), -1e6, c(54,0), 1e6, col = rgb(0.5,0.5,0.5,1/3), border=NA)
legend("topleft", legend = c("Original","Predicted","Forecasted band"),
       text.width = strwidth("1,000,000000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red","grey"),
       title = "Line Types")
```
 ![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/autoarimaforecasteuconsales.png)

### Time Series Modelling for Demand(Quantity) of EU-Consumer Category
```R
tr(gs_Con_EU_Quantity_Month)

nrow(gs_Con_EU_Quantity_Month)

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later
```
#### Classical Decomposition Method to Forecast Time series Consumer and EU on Quantity (Demand)
```R
gs_Con_EU_Quantity_Month$Year.Month <- seq(1,48)

total_timeser <- ts(gs_Con_EU_Quantity_Month$Quantity)
indata <- gs_Con_EU_Quantity_Month[1:42,]

timeser <- ts(indata$Quantity)
plot(timeser,main="Plot for Timeseries of EU-Consumer Quantity",xlab="Month",ylab="Quantity")

#Smoothing the series
w <-1
smoothedseries <- stats::filter(timeser,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

lines(smoothedseries, col="red", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

timevals_in <- indata$Year.Month
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a Multplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#Since multplicative model fits the smoothehned series more appropriately than additive model we will choose multiplicative model
#Formula obtained after multiple trial and error
lmformula<-as.formula(Quantity ~ cos(Month * 0.19) * poly(Month, 3))

lmfit <- lm(lmformula, data = smootheddf)


global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#92.91  309.05  392.76  364.64  438.54  461.70 

#Drawing the global prediction
lines(timevals_in, global_pred, col='blue', lwd=2)

legend("topleft", legend = c("Original","Smooth Series", "Regression Line"),
       text.width = strwidth("1,000,00000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red","blue"),
       title = "Line Types")
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/classicalpredeuconquan.png)

```R
#Now, let's look at the locally predictable series. We will remove the trend and seasonality from the series and get local series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l",main="Local series of EU-Consumer-Quantity",xlab="Month",ylab="Quantity")  # We have found out the local series

#Lets verify whether local series is white noise or not

#ACF test
acf(local_pred,main="ACF for Local series of Consumer-EU Quantity")     #Lots of points are above cutoff value. It means pairwise relationships are preserved

#PACF test
pacf(local_pred,main="PACF for Local series of Consumer-EU Quantity")    #Here also many point exceed the cutoff value
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/acfpacflocaleuconquantityclassical.png)

```R
print(adf.test(local_pred,alternative = "stationary"))   #p-value = 0.01 Series is stationary since p-value below 0.05 in ADF test

print(kpss.test(local_pred))                              #p-value = 0.1 Series is stationary since p-value above 0.05 in KPSS test

#Lets see if the stationary series is weak or strong
armafit <- auto.arima(local_pred)
armafit                              #ARIMA(0,0,0) with zero mean 

tsdiag(armafit)
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/tsdiagclassicaleuconquan.png)

```R
#Now we will get the residual
resi<-local_pred-fitted(armafit)

acf(resi,main="ACF plot for residual series in Consumer EU Quantity")      #Almost all points are below cutoff value in ACF
pacf(resi,main="PACF plot for residual series in Consumer EU Quantity")    #Almost all points are below cutoff value in PACF

#Now we check whether the residual is white noise

adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -3.4464, Lag order = 3, p-value = 0.0634
#alternative hypothesis: stationary
#Since p-value for Augmented Dickey-Fuller Test is less than threshold 0.05 it is stationary

kpss.test(resi)
#KPSS Level = 0.030276, Truncation lag parameter = 1, p-value = 0.1
#Since p-value for KPSS test is greater than threshold 0.05 it is stationary


#------------------------- Model Evaluation EU & Consumer Quantity (Demand)------------------------------
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- gs_Con_EU_Quantity_Month[43:48,]
#timevals_out <- outdata$Year.Month


timevals_out <-  data.frame(Month = outdata$Year.Month)
fcast_arima <- predict(lmfit, timevals_out)
print(fcast_arima)

#MAPE (mean absolute percentage error) for finding out the error in evaluating our model
MAPE_arima <- accuracy(fcast_arima, outdata$Quantity)[5]

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Quantity)[5]
MAPE_class_dec                                       #21.21171

#The error is very less so our model is good to go

Futurepred <- predict(lmfit,data.frame(Month=seq(1:54)))
Futurepred[49:54]
```

| __Jan 15__ | __Feb 15__| __Mar 15__ | __Apr 15__|__May 15__|__Jun 15__|
|------------|-----------|------------|-----------|----------|----------|
| 1068.642   |  1257.814 | 1475.313   | 1720.022  |1990.055  | 2282.888 |

```R
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black" ,main="Forecasting Consumer-EU Quantity by Classical Decomposition",xlab="Month",ylab="Sales")
lines(class_dec_pred, col = "red")
abline(v = 42, col="blue", lwd=2, lty=2)
rect(c(48,0), -1e6, c(54,0), 1e6, col = rgb(0.5,0.5,0.5,1/3), border=NA)

legend("topleft", legend = c("Original","Forecasted"),
       text.width = strwidth("1,000,0000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red"),
       title = "Line Types")
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/classicalforecasteuquan.png)

#### Auto ARIMA for Consumer & EU for Forecasting Quantity (Demand)

```R
autoarima <- auto.arima(timeser)
autoarima                        #ARIMA(2,1,0)
#ARIMA method predicts that the series is of AR(2) and needed 1 level of differencing


plot(autoarima$x, col="black",main="Plotting for Consumer-EU Quantity using Auto-ARIMA",xlab="Month",ylab="Quantity")
lines(fitted(autoarima), col="red")
legend("topleft", legend = c("Original","Forecasted"),
       text.width = strwidth("1,000,0000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red"),
       title = "Line Types")
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/autoarimapredeuconquan.png)
```R
tsdiag(autoarima)               #Plotting diagnostics of time series
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/tsdiagclassicaleuconquan.png)
```R
#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
#Hence residual is Stationary or white noise since p-value of ADF test is less than 0.05

kpss.test(resi_auto_arima)
#KPSS Level = 0.047939, Truncation lag parameter = 1, p-value = 0.1
#Hence residual is white noise since p-value of KPSS test greater tha 0.05

#Also, let's evaluate the model using MAPE and forecast future values
fcast_auto_arima <- predict(autoarima, n.ahead = 12)
fcast_auto_arima$pred[7:12]
```
| __Jan 15__ | __Feb 15__| __Mar 15__ | __Apr 15__|__May 15__|__Jun 15__|
|------------|-----------|------------|-----------|----------|----------|
| 466.2458   |  463.7401 | 472.9520   | 467.6464  |466.1350  | 470.3663 |

```R
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Quantity)[5]
MAPE_auto_arima            #30.13319
#MAPE value obtained is pretty good in Auto ARIMA test

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black",main="Forecasting for Consumer-EU Quantity using Auto-ARIMA",xlab="Month",ylab="Quantity")
lines(auto_arima_pred, col = "red")
abline(v = 42, col="blue", lwd=2, lty=2)
rect(c(48,0), -1e6, c(54,0), 1e6, col = rgb(0.5,0.5,0.5,1/3), border=NA)
legend("topleft", legend = c("Original","Predicted","Forecasted band"),
       text.width = strwidth("1,000,000000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red","grey"),
       title = "Line Types")
```       
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/autoarimaforecasteuconquan.png)

### Time Series Modelling for Sales of APAC-Consumer Category
```R

str(gs_Con_APAC_Sales_Month)

nrow(gs_Con_APAC_Sales_Month)

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later
```
#### Classical Decomposition Method to Forecast Time series Consumer and APAC on Sales
```R
gs_Con_APAC_Sales_Month$Year.Month <- seq(1,48)

total_timeser <- ts(gs_Con_APAC_Sales_Month$Sales)
indata <- gs_Con_APAC_Sales_Month[1:42,]

timeser <- ts(indata$Sales)
plot(timeser,main="Plot for Timeseries of APAC-Consumer Sales",xlab="Month",ylab="Sales")

#--------------------------------------- Smoothing the series - Moving Average Smoothing ----------------------------------------------

w <-1
smoothedseries <- stats::filter(timeser,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

lines(smoothedseries, col="red", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

timevals_in <- indata$Year.Month
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a Multplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#Since multplicative model fits the smoothehned series more appropriately than additive model we will choose multiplicative model
#Formula obtained after multiple trial and errors
lmformula<-as.formula(Sales ~ sin(Month * 0.91) * poly(Month, 3) + cos(Month * 0.55) * poly(Month, 3)	)

lmfit <- lm(lmformula, data = smootheddf)


global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#16340   26168   35227   34503   42107   51754 

#Drawing the global prediction
lines(timevals_in, global_pred, col='blue', lwd=2)

legend("topleft", legend = c("Original","Smooth Series", "Regression Line"),
       text.width = strwidth("1,000,00000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red","blue"),
       title = "Line Types")
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/classicalconapacsalespred.png)

```R
#Now, let's look at the locally predictable series. We will remove the trend and seasonality from the series and get local series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l",main="Local series of APAC Consumer Sales",xlab="Month",ylab="Value")  # We have found out the local series
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/localclassicalconapacsales.png)

```R
#ACF test
acf(local_pred,main="ACF plot for local series in Consumer APAC Sales")     #Lots of points are above cutoff value. It means pairwise relationships are preserved

#PACF test
pacf(local_pred,main="PACF plot for local series in Consumer APAC Sales")    #Here also many point exceed the cutoff value
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/acfpacflocalconapacsales.png)

```R
print(adf.test(local_pred,alternative = "stationary"))   #p-value = 0.048 Series is stationary since p-value below 0.05 in ADF test

print(kpss.test(local_pred))                              #p-value = 0.1 Series is stationary since p-value above 0.05 in KPSS test

#Lets see if the stationary series is weak or strong
armafit <- auto.arima(local_pred)
armafit                              #ARIMA(0,0,0) with zero mean 

#Now we will get the residual
resi<-local_pred-fitted(armafit)

acf(resi,main="ACF plot for residual series in Consumer APAC Sales")      #Almost all points are below cutoff value in ACF
pacf(resi,main="PACF plot for residual series in Consumer APAC Sales")    #Almost all points are below cutoff value in PACF

#Now we check whether the residual is white noise

adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -3.5528, Lag order = 3, p-value = 0.04868
#alternative hypothesis: stationary
#Since p-value for Augmented Dickey-Fuller Test is less than threshold 0.05 it is stationary

kpss.test(resi)
#KPSS Level = 0.03881, Truncation lag parameter = 1, p-value = 0.1
#Since p-value for KPSS test is greater than threshold 0.05 it is stationary

#------------------------- Model Evaluation APAC & Consumer Sales------------------------------

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- gs_Con_APAC_Sales_Month[43:48,]

timevals_out <-  data.frame(Month = outdata$Year.Month)
fcast_arima <- predict(lmfit, timevals_out)
print(fcast_arima)

#MAPE (mean absolute percentage error) for finding out the error in evaluating our model
MAPE_arima <- accuracy(fcast_arima, outdata$Sales)[5]

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales)[5]
MAPE_class_dec                                       #14.54365

#The error is very less so our model is good to go
Futurepred <- predict(lmfit,data.frame(Month=seq(1:54)))
Futurepred[49:54]
```
| __Jan 15__ | __Feb 15__| __Mar 15__ | __Apr 15__|__May 15__|__Jun 15__|
|------------|-----------|------------|-----------|----------|----------|
| 30666.07   |  12184.69 | 20412.62   | 54671.81  |90930.44  | 97903.62 |

```R
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out),ts(Futurepred))
plot(total_timeser, col = "black" ,main="Forecasting for Consumer-APAC Sales using Classical Decomposition",xlab="Month",ylab="Sales")
lines(class_dec_pred, col = "red")
abline(v = 42, col="blue", lwd=2, lty=2)
rect(c(48,0), -1e6, c(54,0), 1e6, col = rgb(0.5,0.5,0.5,1/3), border=NA)
legend("topleft", legend = c("Original","Predicted","Forecasted band"),
       text.width = strwidth("1,000,000000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red","grey"),
       title = "Line Types")
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/classicalforecastconapacsales.png)

### Time Series Modelling for Demand of APAC-Consumer Category
```R
str(gs_Con_APAC_Quantity_Month)

nrow(gs_Con_APAC_Quantity_Month)

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later
```
#### Classical Decomposition Method to Forecast Time series Consumer and APAC on Quantity(Demand)

```R
gs_Con_APAC_Quantity_Month$Year.Month <- seq(1,48)

total_timeser <- ts(gs_Con_APAC_Quantity_Month$Quantity)
indata <- gs_Con_APAC_Quantity_Month[1:42,]

timeser <- ts(indata$Quantity)
plot(timeser,main="Plot for Timeseries of APAC-Consumer Quantity",xlab="Month",ylab="Quantity")

#--------------------------------------- Smoothing the series - Moving Average Smoothing ----------------------------------------------

w <-1
smoothedseries <- stats::filter(timeser,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

lines(smoothedseries, col="red", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

timevals_in <- indata$Year.Month
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a Multplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#Since multplicative model fits the smoothehned series more appropriately than additive model we will choose multiplicative model
#Formula obtained after multiple trial and errors
lmformula<-as.formula(Quantity ~ sin(Month * 0.38) * poly(Month, 1) * cos(Month * 0.8))

lmfit <- lm(lmformula, data = smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#173.8   320.7   419.6   406.2   486.3   590.5

lines(timevals_in, global_pred, col='blue', lwd=2)

legend("topleft", legend = c("Original","Smooth Series", "Regression Line"),
       text.width = strwidth("1,000,00000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red","blue"),
       title = "Line Types")
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/classicalapacconpredplot.png)

```R
#Now, let's look at the locally predictable series. We will remove the trend and seasonality from the series and get local series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l",main="Local Series for Quantity of APAC-Consumer",xlab="Month",ylab="Quantity")  # We have found out the local series
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/classicallocalapaccondemand.png)
```R
#Lets verify whether local series is white noise or not

#ACF test
acf(local_pred)     #Lots of points are above cutoff value. It means pairwise relationships are preserved

#PACF test
pacf(local_pred)    #Here also many point exceed the cutoff value
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/acfpacfclassicalapaccondemand.png)

```R
rint(adf.test(local_pred,alternative = "stationary"))   #p-value = 0.01 Series is stationary since p-value below 0.05 in ADF test

print(kpss.test(local_pred))                              #p-value = 0.1 Series is stationary since p-value above 0.05 in KPSS test

#Lets see if the stationary series is weak or strong
armafit <- auto.arima(local_pred)
armafit                              #ARIMA(0,0,0) with zero mean 
tsdiag(armafit)
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/tsdiagapacconclassical.png)

```R
#Now we will get the residual
resi<-local_pred-fitted(armafit)

acf(resi,main="ACF plot for residual series in Consumer APAC Sales")      #Almost all points are below cutoff value in ACF
pacf(resi,main="PACF plot for residual series in Consumer APAC Sales")    #Almost all points are below cutoff value in PACF

#Now we check whether the residual is white noise

adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -4.5424, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
#Since p-value for Augmented Dickey-Fuller Test is less than threshold 0.05 it is stationary

kpss.test(resi)
#KPSS Level = 0.033175, Truncation lag parameter = 1, p-value = 0.1
#Since p-value for KPSS test is greater than threshold 0.05 it is stationary

#It means it is white noise

#-----------------Model Evaluation APAC & Consumer Quantity----------------------------

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- gs_Con_APAC_Quantity_Month[43:48,]

timevals_out <-  data.frame(Month = outdata$Year.Month)
fcast_arima <- predict(lmfit, timevals_out)
print(fcast_arima)

#MAPE (mean absolute percentage error) for finding out the error in evaluating our model
MAPE_arima <- accuracy(fcast_arima, outdata$Quantity)[5]

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Quantity)[5]
MAPE_class_dec                                       #14.81134

Futurepred <- predict(lmfit,data.frame(Month=seq(1:54)))
Futurepred[49:54]
```
| __Jan 15__ | __Feb 15__| __Mar 15__ | __Apr 15__|__May 15__|__Jun 15__|
|------------|-----------|------------|-----------|----------|----------|
| 667.8858   |  640.6964 | 720.4176   | 738.8260  |596.8577  | 408.9849 |

```R
#The error is very less so our model is good to go

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out),ts(Futurepred))
plot(total_timeser, col = "black" ,main="Forecasting for Consumer-APAC Sales using Classical Decomposition",xlab="Month",ylab="Sales")
lines(class_dec_pred, col = "red")
abline(v = 42, col="blue", lwd=2, lty=2)
rect(c(48,0), -1e6, c(54,0), 1e6, col = rgb(0.5,0.5,0.5,1/3), border=NA)
legend("topleft", legend = c("Original","Predicted","Forecasted band"),
       text.width = strwidth("1,000,000000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red","grey"),
       title = "Line Types")
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/forecastclassicalconapacdemand.png)

#### Auto ARIMA for Consumer & APAC for Forecasting Quantity (Demand)
```R
autoarima <- auto.arima(timeser)
autoarima                        #ARIMA(0,1,0)
#ARIMA method predicts that the series needed 1 level of differencing

plot(autoarima$x, col="black",main="Plotting for Consumer-APAC Quantity using Auto-ARIMA",xlab="Month",ylab="Quantity")
lines(fitted(autoarima), col="red")
legend("topleft", legend = c("Original","Predicted"),
       text.width = strwidth("1,000,0000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red"),
       title = "Line Types")
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/autoarimaconapacdemand.png)

```R
tsdiag(autoarima)               #Plotting diagnostics of time series
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/tsdiagautoarimaconapacdemand.png)
```R
#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
#Hence residual is Stationary or white noise since p-value of ADF test is less than 0.05

kpss.test(resi_auto_arima)
#KPSS Level = 0.031535, Truncation lag parameter = 3, p-value = 0.1
#Hence residual is white noise since p-value of KPSS test greater tha 0.05

#Also, let's evaluate the model using MAPE and forecast future values
fcast_auto_arima <- predict(autoarima, n.ahead = 12)
print(fcast_auto_arima$pred[7:12])
```
| __Jan 15__ | __Feb 15__| __Mar 15__ | __Apr 15__|__May 15__|__Jun 15__|
|------------|-----------|------------|-----------|----------|----------|
| 721        |  721      | 721        | 721       |721       | 721      |

```R
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Quantity)[5]
MAPE_auto_arima
#26.24458
#MAPE value obtained is pretty good in Auto ARIMA test

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black",main="Forecasting for Consumer-APAC Quantity using Auto-ARIMA",xlab="Month",ylab="Quantity")
lines(auto_arima_pred, col = "red")
abline(v = 42, col="blue", lwd=2, lty=2)
rect(c(48,0), -1e6, c(54,0), 1e6, col = rgb(0.5,0.5,0.5,1/3), border=NA)
legend("topleft", legend = c("Original","Predicted","Forecasted band"),
       text.width = strwidth("1,000,000000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red","grey"),
       title = "Line Types")
```
![data](https://github.com/yatinkode/Forecasting-Sales-and-Profit-for-Global-Superstore-using-Time-Series/blob/master/images/forecastautoarimaconapacdemand.png) 
