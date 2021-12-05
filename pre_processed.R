library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readxl)
library(tibble)
library(gridExtra)

setwd("D:\\Emir\\Programming\\VENV\\R_files\\data_analytics\\group_project")
gas_prices <- read_excel("GRP.xls")

names(gas_prices) <- c('Date',
                       'Gas_Reference_Price')

gas_prices$Gas_Reference_Price <- gsub('.',
                                       '',
                                       gas_prices$Gas_Reference_Price,
                                       fixed = TRUE)
gas_prices$Gas_Reference_Price <- as.numeric(gsub(',',
                                                  '.',
                                                  gas_prices$Gas_Reference_Price,
                                                  fixed = TRUE))
gas_prices$Date <- dmy(gas_prices$Date)

view(gas_prices)

trade_volume <- read_excel("TTV.xls")

names(trade_volume) <- c('Date',
                         'Total_Trade_Volume')

trade_volume$Total_Trade_Volume <- gsub('.',
                                       '',
                                       trade_volume$Total_Trade_Volume,
                                       fixed = TRUE)
trade_volume$Total_Trade_Volume <- as.numeric(gsub(',',
                                                  '.',
                                                  trade_volume$Total_Trade_Volume,
                                                  fixed = TRUE))
trade_volume$Date <- dmy(trade_volume$Date)

view(trade_volume)

trade_volume$week_num <- strftime((trade_volume$Date), format = "%V")
trade_volume$week_num <- as.numeric(trade_volume$week_num)

trade_volume$year_num <- isoyear(trade_volume$Date)
trade_volume$year_num <- as.numeric(trade_volume$year_num)

trade_volume$month_num <- month(trade_volume$Date)
trade_volume$month_num <- as.numeric(trade_volume$month_num)

trade_volume$day_of_week <- wday(trade_volume$Date, label = TRUE, abbr = TRUE)

seasons <- NULL
for(row in seq_len(nrow(trade_volume["Date"]))){
  if((trade_volume["month_num"][row, 1] == 12) | (trade_volume["month_num"][row, 1] == 1) | (trade_volume["month_num"][row, 1] == 2)){
    seasons <- append(seasons,"Winter")
  }
  else if((trade_volume["month_num"][row, 1] == 3) | (trade_volume["month_num"][row, 1] == 4) | (trade_volume["month_num"][row, 1] == 5)){
    seasons <- append(seasons,"Spring")
  }
  else if((trade_volume["month_num"][row, 1] == 6) | (trade_volume["month_num"][row, 1] == 7) | (trade_volume["month_num"][row, 1] == 8)){
    seasons <- append(seasons,"Summer")
  }
  else {
    seasons <- append(seasons,"Fall")
  }
}
trade_volume["season"] <- seasons

df <- merge(x=trade_volume, y=gas_prices, by='Date')
saveRDS(df, "natural_gas_data.rds")