library(tidyverse)
library(tidymodels)
library(lubridate)
library(data.table)
library(skimr)
library(tibble)
library(tibbletime)
library(timetk)
library(imputeTS)
library(anomalize)
library(forecast)
library(GGally)
library(modeltime)
library(gt)
library(timetk)
library(lubridate)
library(janitor)
library(tidyquant)

# Visualization
library(ggthemes)
library(ggsci)
library(viridis)
library(ggExtra)

site_info <- fread('data/site_info.csv', encoding = 'UTF-8')
dangjin_fcst_data <- fread('data/dangjin_fcst_data.csv', encoding = 'UTF-8')
dangjin_obs_data <- fread('data/dangjin_obs_data.csv', encoding = 'UTF-8')
energy <- fread('data/energy.csv', encoding = 'UTF-8')
ulsan_fcst_data <- fread('data/ulsan_fcst_data.csv', encoding = 'UTF-8')
ulsan_obs_data <- fread('data/ulsan_obs_data.csv', encoding = 'UTF-8')



energy %>% is.na() %>% colSums()

energy_dg_float <- energy %>% 
    select(time, dangjin_floating)

energy_dg_house <- energy %>% 
    select(time, dangjin_warehouse)

energy_dangjin <- energy %>% 
    select(time, dangjin)

energy_dangjin <- energy %>% 
    select(time, dangjin)

energy_ulsan <- energy %>% 
    select(time, ulsan)


# ulsan 

energy_ulsan %>% 
    mutate(time = ymd_hms(time)) -> energy_ulsan

ulsan_obs_data %>% 
    select(-c('지점', '지점명')) %>% 
    rename(time = 일시) -> ulsan_obs_data

colnames(ulsan_obs_data) <- gsub("\\(.*?\\)","",colnames(ulsan_obs_data)) # .*? : () 안에 모든 것 

ulsan_obs_data %>% mutate(time = as.POSIXct(time, tz = 'gmt')) %>% 
    mutate(time = ymd_hms(time)) -> ulsan_obs_data

energy_ulsan

energy_ulsan %>% 
    left_join(ulsan_obs_data, by = 'time') -> ulsan

rdata <- fread('data/OBS_ASOS_TIM_2018.csv') %>% 
    bind_rows(fread('data/OBS_ASOS_TIM_2019.csv')) %>%
    bind_rows(fread('data/OBS_ASOS_TIM_2020.csv')) %>%
    bind_rows(fread('data/OBS_ASOS_TIM_2021.csv')) %>%
    mutate(일시 = as.POSIXct(일시, tz = 'gmt'))

colnames(rdata) <- gsub("\\(.*?\\)","",colnames(rdata)) # .*? : () 안에 모든 것

rdata %>%
    select(-c('지점', '지점명')) %>%
    rename('time' = '일시') %>% 
    mutate(time = ymd_hms(time)) %>% 
    select(time, 현지기압, 일조) -> rdata

ulsan %>% 
    left_join(rdata, by = 'time') -> ulsan

ulsan %>% is.na() %>% colSums()

ulsan %>% 
    mutate(hour = hour(time), 
           day = day(time), 
           weekday = wday(time), 
           month = month(time),
           quarter = quarter(time), 
           quarter = factor(quarter)) -> ulsan


ulsan %>% 
    group_by(time) %>% 
    summarise(value = ulsan) %>% 
    plot_time_series(time, value, .interactive = TRUE)



split <- ulsan %>% 
    initial_time_split(prop = 0.7, strata = 'ulsan')

split

split %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(time, ulsan, .interactive = TRUE)


# Model 1

model_fit_arima_no_boost <- arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(ulsan ~ time, training(split))



# Model 2 Boosted Auto ARIMA 
model_fit_arima_boosted <- arima_boost(mtry = tune(),
                                       trees = tune(),
                                       min_n = tune(),
                                       tree_depth = tune(),
                                       learn_rate = tune(),
                                       loss_reduction = tune(),
                                       sample_size = tune(),
                                       stop_iter = tune(), 
                                       seasonal_period = tune(),
                                       non_seasonal_ar = tune(),
                                       non_seasonal_differences = tune(),
                                       non_seasonal_ma = tune(),
                                       seasonal_ar = tune(),
                                       seasonal_differences = tune(),
                                       seasonal_ma = tune(),
) %>%
    set_engine(engine = "auto_arima_xgboost") %>%
    fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F), 
        data = training(split))




