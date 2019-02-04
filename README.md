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
#Consumer   Corporate Home Office 
#51.70209    30.08189    18.21603

sum(is.na(gs$Market)) #No NA values in Market
prop.table(table(gs$Market))*100
#Africa      APAC    Canada      EMEA        EU     LATAM        US 
#8.943264 21.450575  0.748684  9.805030 19.496978 20.070189 19.485280 

prop.table(table(gs$Market,gs$Segment))*100
#          Consumer  Corporate Home Office
#Africa  4.6422305  2.5580035   1.7430298
#APAC   11.1113277  6.4008579   3.9383895
#Canada  0.3938390  0.2144668   0.1403782
#EMEA    4.9483330  3.0688243   1.7878729
#EU     10.1111328  5.9992201   3.3866251
#LATAM  10.3743420  5.9524274   3.7434198
#US     10.1208813  5.8880873   3.4763112



#Plotting Profit Based on Market-Segment
ggplot(gs,aes(x=Market,y=Profit,fill=Segment)) +
  theme_forecast() + 
  stat_summary(fun.y = sum, geom='bar',colour="black", position='dodge') + 
  theme(plot.title = element_text(hjust = 0.1)) +labs(x = "Market",y = "Sum of Profit",title = "Fig : Profit Based on Market-Segment" )
```


