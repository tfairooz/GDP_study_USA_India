## To Install all the packages 
install.packages("readxl")
library("readxl")
library("fpp3")
install.packages("ggplot2")
library("ggplot2")


library(readxl)
ppp_india_usa <- read_excel("ppp_india_usa.xls")
View(ppp_india_usa) 

##### 
#Using PPP for plotting

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

## working with USA data 

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

  
## 1. Let's start with an ETS model
# Based on the AICc values of the models I choose an AAN model to forecast the data
ets_fit <- usa_train %>%
  model(AAN = ETS(r_gdp ~ error("A") + trend("A") + season("N")))
ets_fit %>%
  report()
components(ets_fit)
glance(ets_fit)

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
#######
## keeping this code for a damped trend forecast at the end 
usa_train %>%
  autoplot(r_gdp) +
  autolayer(fc_ann) +
  autolayer(fc_annd) +
  guides(color = guide_legend(title = "Forecast"))
#######  

fc_ann %>%
  autoplot(ts_usa) +
  labs(y = "log GDP", title = "Real GDP Forecast W/ Damped Holt's")

# for cross-validation  
ts_usa %>%
  model(comp = ETS(r_gdp ~ error("A") + trend("Ad") + season("N"))) %>%
  accuracy()
  
fc_annd %>%
  accuracy(ts_usa)

## 2. Using an ARIMA model to fit the data 
# testing for unitroot

ts_usa %>%
  gg_tsdisplay(r_gdp)

ts_usa %>%
  features(r_gdp, unitroot_kpss)

# p - value < 0.05 (0.01), data is a unit-root process

ts_usa %>%
  mutate(diff_us = difference(r_gdp)) %>%
  autoplot(diff_us) + 
  labs(title = "Differenced GDP")

ts_usa %>%
  mutate(diff_us = difference(rgdp_pc)) %>% 
  gg_tsdisplay(diff_us)
  

# unitroot after dfferencing gives a p-value of 0.0567. 
# The plot shows only one spike beyond the confidence interval, but that is not significant   
# therefore, differencing the data once ARIMA(p,1,q)
# choice of ARIMA from eyeballing (1,1,0) & (2,1,0)
# stepwise != eyeball model = ARIMA(0,1,1)

arima_fit <- usa_train %>%
  model(arima110 = ARIMA(rgdp_pc ~ pdq(1,1,0)),
        arima111 = ARIMA(rgdp_pc ~ pdq(1,1,1)),
        arima210 = ARIMA(rgdp_pc ~ pdq(2,1,0)),
        stepwise = ARIMA(rgdp_pc))
arima_fit %>% glance()

# confirming white noise models using ljung-box test
# test confirms white noise for all 3 models.

arima_fit %>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 1) 

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
  
# auto ARIMA model gave an ARIMA(0,1,1)
fit <- usa_train %>%
  model(ARIMA(rgdp_pc))
report(fit)

fit %>%
  forecast(h = 5) %>%
  autoplot(ts_usa)

# Cross_validating 
ts_usa %>%
  model(test = ARIMA(rgdp_pc ~ pdq(1,1,0))) %>%
  accuracy()

fc_a %>%
  accuracy(ts_usa)
# results show that the ARIMA(0,1,1) selected by R gives us the highest P-value and the smallest AICc and BIC
# forecasting using ARIMA(1,1,0). JUST testing with this model  

arima_test <- ts_usa %>%
  model(arima110 = ARIMA(rgdp_pc ~ pdq(1,1,1))) %>%
  accuracy()

fc_arima <- arima_fit %>%
  forecast(h = 5) %>%
  filter(.model == 'arima111')

fc_arima %>%
  accuracy(ts_usa)

fc_arima %>%
  autoplot(ts_usa)
  

## Using VAR
# correlated variable of choice is Interest/discount Rate 

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
    forecast(h = 5)

fc_var %>%
  autoplot(ts_usa)

# Cross-validation:
ts_usa %>%
  model(VAR(vars(r_gdp, d_rate))) %>%
  accuracy()

fc_var %>%
  accuracy(ts_usa)

## Final choice model is VAR(2)aicc. 
# Forecasting next 10 years

ts_usa %>%
  model(VAR(vars(r_gdp, d_rate))) %>%
  forecast(h = 10) %>%
  autoplot(ts_usa) +
  labs(title = "Forecast of Real GDP for next 10 years")


## Forecasting Using Regression (TSLM)

fit_tslm <- usa_train %>%
  model(lm = TSLM(r_gdp ~ d_rate + Infl))
report(fit_tslm)

fit_tslm %>%
  gg_tsresiduals()
  
fc_tslm <- fit_tslm %>%
  forecast(h = 5)

ts_usa %>%
  model(lmt = TSLM(r_gdp ~ d_rate + Infl + Empl))

## 














########
## working with just India Data 

india <- clean %>%
  filter(Country== "India")

ts_india <- india %>%
  mutate(Year = (1983:2019))%>%
  as_tsibble(index = Year)
  
ts_india %>%
  autoplot(log_gdp) +
  labs(y = "GDP", x = "Year", title= "Real GDP of India")






