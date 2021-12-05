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

df <- merge(x=trade_volume, y=gas_prices, by="Date")
saveRDS(df, "natural_gas_data.rds")