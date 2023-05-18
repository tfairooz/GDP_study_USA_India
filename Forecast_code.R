## To Install all the packages 
install.packages("readxl")
library("readxl")
library("fpp3")
library("ggplot2")


library(readxl)
ppp_india_usa <- read_excel("ppp_india_usa.xls")
View(ppp_india_usa) 

##### 
#Using PPP data for plotting to compare USA and India GDP 

gppp <- ppp_india_usa %>%
  mutate(gdp = log(igdp))

gppp %>%
  ggplot(aes(Year, gdp, color = Country)) + 
  geom_point(size = 2.5, alpha = 0.3) +
  geom_line(size = 1) +
  theme_minimal() +
  scale_x_discrete(breaks=c("1990","1995","2000","2005","2010","2015","2020","2025")) +
  labs(y = "GDP at $I",title = "GDP adjusted to PPP")

##### 
#Using clean data 
clean <- read_excel("clean.xls")
View(clean)

# Plotting the log real gdp of both countries; to see similar patters to previous data

clean %>%
  ggplot(aes(Year, r_gdp, color = Country)) + 
  geom_point(size = 2.5, alpha = 0.3) +
  geom_line(size = 1) +
  theme_minimal() +
  scale_x_discrete(breaks=c("1980","1985","1990","1995","2000","2005","2010","2015","2020")) +
labs(y = "Log GDP",title = "Real GDP")

## working with USA data; creating tsibble  

usa <- clean %>%
  filter(Country == "USA") %>%
  select(Year, r_gdp, Infl, d_rate, pop) %>%
  mutate(rgdp_pc =(r_gdp/pop))

ts_usa <- usa %>%
  mutate(Year = (1961:2019)) %>%
  as_tsibble(index = Year)

usa_train <- ts_usa %>%
  filter_index("1961" ~ "2014")

usa1 <- ts_usa %>%
  filter_index("1983" ~ "2007")

ts_usa %>%
  autoplot(r_gdp) +
  labs(y = "log(GDP)", x = "Year", title = "Real GDP of USA")

# testing data for unitroot

ts_usa %>%
  gg_tsdisplay(r_gdp)

ts_usa %>%
  features(r_gdp, unitroot_kpss)

ts_usa %>%
  features(Infl, unitroot_kpss)

ts_usa %>%
  autoplot(Infl)

# p - value < 0.05 (0.01), data is a unit-root process

ts_usa %>%
  mutate(diff_us = difference(r_gdp)) %>%
  autoplot(diff_us) + 
  labs(title = "Differenced GDP")

ts_usa %>%
  mutate(diff_us = difference(r_gdp)) %>%
  gg_tsdisplay(diff_us)


# unitroot after dfferencing gives a p-value of 0.0567. 
# The plot shows only one spike beyond the confidence interval, but that is not significant

####### 
#ETS USA  
## 1. Let's start with an ETS model
# Based on the AICc values of the models I choose an AAN model to forecast the data
ets_fit <- usa_train %>%
  model(AAN = ETS(r_gdp ~ error("A") + trend("A") + season("N")))
ets_fit %>%
  report()
components(ets_fit)
glance(ets_fit)

ets_fit %>%
  accuracy()

ets_fitd <- usa_train %>%
  model(AAN = ETS(r_gdp ~ error("A") + trend("Ad") + season("N")))
ets_fitd %>%
  report()

components(ets_fitd)
glance(ets_fitd)


# Forecasting using the ANN ETS model

fc_ann <- ets_fit %>%
  forecast(h = 5) 


fc_annd <- ets_fitd %>%
  forecast(h = 5) 


fc_ann <- ets_fitd %>%
  forecast(h = 5) %>%
  autoplot(ts_usa) +
  labs(y = "log GDP", title = "Real GDP Forecast W/ Damped Holt's")

# for cross-validation  

ts_usa %>%
  model(comp1 = ETS(r_gdp ~ error("A") + trend("A") + season("N"))) %>%
  accuracy()

fc_ann %>%
  accuracy(ts_usa)

ts_usa %>%
  model(comp = ETS(r_gdp ~ error("A") + trend("Ad") + season("N"))) %>%
  accuracy()
  
fc_annd %>%
  accuracy(ts_usa)

## 2. Using an ARIMA model to fit the data 
   
# therefore, differencing the data once ARIMA(p,1,q)
# choice of ARIMA from eyeballing (1,1,0) & (2,1,0)
# stepwise != eyeball model = ARIMA(0,1,1)

arima_fit <- usa_train %>%
  model(arima110 = ARIMA(r_gdp ~ pdq(1,1,0)),
        arima111 = ARIMA(r_gdp ~ pdq(1,1,1)),
        arima210 = ARIMA(r_gdp ~ pdq(2,1,0)),
        stepwise = ARIMA(r_gdp))
arima_fit %>% glance()

# confirming white noise models using ljung-box test
# test confirms white noise for all 3 models.

arima_fit %>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 1) %>%
  filter(.model != 'arima210')

arima_fit %>%
  augment()%>%
  features(.innov, ljung_box, lag = 10, dof = 2) %>%
  filter(.model == 'arima210')

# if needed to see seperately, we can pipe the code below to the above chunk
%>%
  filter(.model == 'arima110') 

# forecasting 

fc_a <- arima_fit %>%
  forecast(h = 5) %>%
  filter(.model=='arima110') 

fc_a %>%
  autoplot(ts_usa) +
  labs(title = "Real GDP Forecast w/ ARIMA(1,1,0)")


# Cross_validating 
ts_usa %>%
  model(test = ARIMA(r_gdp ~ pdq(1,1,0))) %>%
  accuracy()

fc_a %>%
  accuracy(ts_usa)

# results show that the ARIMA(0,1,1) selected by R gives us the highest P-value and the smallest AICc and BIC
# forecasting using ARIMA(1,1,1). JUST testing with this model  

ts_usa %>%
  model(arima111 = ARIMA(r_gdp ~ pdq(1,1,1))) %>%
  accuracy()

fc_arima <- arima_fit %>%
  forecast(h = 5) %>%
  filter(.model == 'arima111')

fc_arima %>%
  accuracy(ts_usa)

fc_arima %>%
  autoplot(ts_usa)
  

## Using VAR

# to find correaltion:
install.packages("GGally")
library("GGally")

ts_usa %>%
  GGally:: ggpairs(columns = 3:6)

# Based on plot, correlated variable of choice is Interest/discount Rate

fit_var <- usa_train %>%
  model(aicc = VAR(vars(r_gdp, d_rate)),
        bic = VAR(vars(r_gdp, d_rate), ic = c("bic"))
        ) 
glance(fit_var)

fit_var %>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()

# ljung test to confirm the models are infact white noise. 

fit_var %>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 2) 

# all P. values > 0.1; test confirms models are whote noise and good to forecast.

fc_var <- fit_var %>%
    forecast(h = 5) %>%
  filter(.model == 'bic')

fc_var %>%
  autoplot(ts_usa)

# Cross-validation:
ts_usa %>%
  model(VAR(vars(r_gdp, d_rate), ic = c('bic'))) %>%
  accuracy()

fc_var %>%
  accuracy(ts_usa)

## Final choice model is VAR(2)bic. 
# Forecasting next 10 years

ts_usa %>%
  model(VAR(vars(r_gdp, d_rate), ic = c('bic'))) %>%
  forecast(h = 10) %>%
  autoplot(ts_usa) +
  labs(title = "Forecast of Real GDP for next 10 years")


## Forecasting Using Regression (TSLM)
# code to plot two or more variables in one graph
1.
usa_train %>%
  pivot_longer(c(r_gdp, d_rate)) %>%
  autoplot(value)

2.
test_data <- pivot_longer(ts_usa, cols = (2:4))

ggplot(data=test_data,
       aes(x = Year, y = value, color = name)) +
  geom_line()

# this code has error
ts_usa %>%
  ggplot(aes(Year)) +
  geom_line(aes(y = rgdp_pc, color = "rgdp_pc")) +
  geom_point(size = 0.95, alpha = 0.3)
  geom_line(aes(y = d_rate, color = "d_rate")) +
  geom_point(size = .95, alpha = 0.3) +
  theme_minimal() +
  scale_x_discrete(breaks = c("1960", "1970", "1980", "1990",
                              "2000", "2010", "2020"))
##

usa2 %>%
  augment() %>%
  features(.innov, unitroot_kpss)
  
    
usa2 <- usa_train %>%
  mutate(dif = difference(r_gdp))
    
fit_tslm <- usa2 %>%
  model(lm = TSLM(dif ~ d_rate + Infl))
report(fit_tslm)

fit_tslm %>%
  gg_tsresiduals()

future_scenario <- scenarios(
  increase = new_data(ts_usa, 4) %>%
    mutate(d_rate = -0.25, Infl = -0.5),
  decrease = new_data(ts_usa, 4) %>%
    mutate(d_rate = 0.25, Infl = 0.5),
  names_to = "Scenario"
)

  
fc_tslm <- forecast(fit_tslm, new_data = future_scenario)

ts_usa %>%
  autoplot(r_gdp) +
  autolayer(fc_tslm)
  

ts_usa %>%
  model(lmt = TSLM(r_gdp ~ d_rate + Infl + Empl))

########
## working with just India Data 

india <- clean %>%
  filter(Country== "India") %>%
  select(Year, r_gdp, Infl, d_rate, pop) %>%
  mutate(rgdp_pc = r_gdp/pop)

ts_india <- india %>%
  mutate(Year = (1968:2019))%>%
  as_tsibble(index = Year)

# creating training set
india_train <- ts_india %>%
  filter_index("1968" ~ "2014")
  
ts_india %>%
  autoplot(rgdp_pc) +
  labs(y = "GDP", x = "Year", title= "Real Per Capita GDP of India")

ets_fit_india <- india_train %>%
  model(AAN = ETS(r_gdp ~ error("A") + trend("A") + season("N")))
ets_fit_india %>%
  report()
components(ets_fit_india)
glance(ets_fit)

ets_fitd_india <- india_train %>%
  model(AAN = ETS(r_gdp ~ error("A") + trend("Ad") + season("N")))
ets_fitd_india %>%
  report()

components(ets_fitd_india)

glance(ets_fitd_india)

fc_ann_india <- ets_fit_india %>%
  forecast(h = 5)

fc_ann_india %>%
  accuracy(ts_india)

fc_annd_india <- ets_fitd_india %>%
  forecast(h = 5) 

fc_annd_india %>%
  accuracy(ts_india)

# testing data for unitroot

ts_india %>%
  gg_tsdisplay(r_gdp)

ts_india %>%
  features(r_gdp, unitroot_kpss)

ts_india %>%
  features(Infl, unitroot_kpss)

ts_india %>%
  autoplot(Infl)

# p - value < 0.05 (0.01), data is a unit-root process

ts_india %>%
  mutate(diff_us = difference(r_gdp)) %>%
  autoplot(diff_us) + 
  labs(title = "Differenced GDP")

ts_india %>%
  mutate(diff_us = difference(r_gdp)) %>%
  gg_tsdisplay(diff_us)


####### 
#ETS INDIA 
## 1. Let's start with an ETS model
# Based on the AICc values of the models I choose an AAN model to forecast the data
ets_fit <- india_train %>%
  model(AAN = ETS(r_gdp ~ error("A") + trend("A") + season("N")))
ets_fit %>%
  report()
components(ets_fit)
glance(ets_fit)

ets_fit %>%
  accuracy()

ets_fitd <- india_train %>%
  model(AAN = ETS(r_gdp ~ error("A") + trend("Ad") + season("N")))
ets_fitd %>%
  report()

components(ets_fitd)
glance(ets_fitd)

ets_fitd %>%
  accuracy()


# Forecasting using the ANN ETS model

fc_ann <- ets_fit %>%
  forecast(h = 5) 

fc_ann %>%
  accuracy(ts_india)


fc_annd <- ets_fitd %>%
  forecast(h = 5) 

fc_annd %>%
  accuracy(ts_india)

fc_ann <- ets_fit %>%
  forecast(h = 5) %>%
  autoplot(ts_india) +
  labs(y = "log GDP", title = "Real GDP Forecast W/ Holt's")

# for cross-validation  

ts_india %>%
  model(comp1 = ETS(r_gdp ~ error("A") + trend("A") + season("N"))) %>%
  accuracy()

fc_ann %>%
  accuracy(ts_india)

ts_india %>%
  model(comp = ETS(r_gdp ~ error("A") + trend("Ad") + season("N"))) %>%
  accuracy()

fc_annd %>%
  accuracy(ts_india)

## 2. Using an ARIMA model to fit the data 

# therefore, differencing the data once ARIMA(p,1,q)
# choice of ARIMA from eyeballing (1,1,0) & (2,1,0)
# stepwise != eyeball model = ARIMA(0,1,1)

india_train %>%
  model(ARIMA(r_gdp))

arima_fit <- india_train %>%
  model(arima110 = ARIMA(r_gdp ~ pdq(1,1,0)),
        arima210 = ARIMA(r_gdp ~ pdq(2,1,0)),
        stepwise = ARIMA(r_gdp))
arima_fit %>% glance()

# confirming white noise models using ljung-box test
# test confirms white noise for all 3 models.

arima_fit %>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 1) %>%
  filter(.model != 'arima210')

arima_fit %>%
  augment()%>%
  features(.innov, ljung_box, lag = 10, dof = 2) %>%
  filter(.model == 'arima210')

# if needed to see seperately, we can pipe the code below to the above chunk
%>%
  filter(.model == 'arima110') 

# forecasting 

fc_a <- arima_fit %>%
  forecast(h = 5) %>%
  filter(.model=='arima110') 

fc_a %>%
  accuracy(ts_india)

fc_a %>%
  autoplot(ts_india) +
  labs(title = "Real GDP Forecast w/ ARIMA(1,1,0)")


# Cross_validating 
ts_india %>%
  model(test = ARIMA(r_gdp ~ pdq(1,1,0))) %>%
  accuracy()

fc_a %>%
  accuracy(ts_india)

# results show that the ARIMA(0,1,1) selected by R gives us the highest P-value and the smallest AICc and BIC
# forecasting using ARIMA(1,1,1). JUST testing with this model  

india_train %>%
  model(arima110 = ARIMA(r_gdp ~ pdq(1,1,0)),
        arima210 = ARIMA(r_gdp ~ pdq(2,1,0)),
        stepwise = ARIMA(r_gdp)) %>%
  accuracy()

fc_arima <- arima_fit %>%
  forecast(h = 5) %>%
  filter(.model == 'stepwise')

fc_arima %>%
  autoplot(ts_india) +
  labs(title = "RGDP forecast ARIMA(0,2,1)", y = "Real GDP Of India")

fc_arima %>%
  accuracy(ts_india)

## Using VAR

# to find correaltion:
install.packages("GGally")
library("GGally")

ts_india %>%
  GGally:: ggpairs(columns = 2:5)

# Based on plot, correlated variable of choice is Interest/discount Rate

fit_var <- india_train %>%
  model(aicc = VAR(vars(r_gdp, Infl)),
        bic = VAR(vars(r_gdp, Infl), ic = c("bic"))
  ) 
glance(fit_var)

fit_var %>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()

# ljung test to confirm the models are infact white noise. 

fit_var %>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 2) %>%
  filter(.model == "aicc")

fit_var %>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 3) %>%
  filter(.model == "bic")

# all P. values > 0.1; test confirms models are white noise and good to forecast.

fc_var <- fit_var %>%
  forecast(h = 5) %>%
  filter(.model == 'aicc')

fc_var %>%
  autoplot(ts_india)

# Cross-validation:
ts_india %>%
  model(VAR(vars(r_gdp, Infl))) %>%
  accuracy()

fc_var %>%
  accuracy(ts_india)


## Final choice model is VAR(2)bic. 
# Forecasting next 10 years

ts_india %>%
  model(AAN = ETS(r_gdp ~ error("A") + trend("A") + season("N"))) %>%
  forecast(h = 10) %>%
  autoplot(ts_india) +
  labs(title = "Forecast of Real GDP for next 10 years")

