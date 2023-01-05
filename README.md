Working:

This repository includes the files necessary to build an interactive interface using the R package Shiny for technical analysis of the stock market. It includes the necessary tools to select the company (from the dataset uploaded with over 1891 companies in 50 different countries around the world including NIFTY-50) and the time period to display. 

It allows to see the following values of the stock market for each company during various timelines:
- Open Values
- Close Values
- Adjusted Value
- High
- Low
- Volume

The interface presents the time series plot and an interactive plot for the user input along with different statistical values.

It provides a dashbaord page dedicated to displaying daily data of a stock with it's respectve date upto the latest 30 days.

It also predicts the future values of the stock upto next 30 days with the help of ARIMA forecasting both with seasonal and non seasonal components.

# Data Files

The dataset used for this project is:
- stock_market.csv: This file contains information of each company, including:
    - Name
    - Sector
    - Ticker
    - Country
    
# Requirements
For the interface to work it is necessary to install the next packages:  
- shiny  
- ggplot2  
- rtsdata  
- reshape  
- shinythemes  
- dygraphs 
- forecast
- tidyquant
- glue
- rvest
- stringr
- quantmod
- tseries
- TTR