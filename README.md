This repository contains the code and methodology used to forecast the unemployment rate in Colombia using a MIDAS (Mixed Data Sampling) regression model. The model utilizes both high-frequency (weekly) and low-frequency (monthly) data to predict the unemployment rate. Specifically, Google Trends search terms like "Empleo" and "Trabajo" (weekly) are used alongside the PMI (monthly) and the unemployment rate calculated by DANE.

Model Overview
The MIDAS model allows for the use of mixed-frequency data by incorporating high-frequency data as regressors while forecasting a low-frequency target variable—in this case, the monthly unemployment rate. The model also includes dummy variables to account for structural changes such as the COVID-19 pandemic.

Data
Low-frequency data (monthly):

Unemployment rate (Tasa de Desempleo) from DANE.
PMI (Purchasing Managers' Index) from Davivienda.
High-frequency data (weekly):

Google Trends searches for "Empleo" and "Trabajo."
Code Overview
The following code and libraries are used to develop, test, and evaluate the MIDAS model:

Data Preparation: Import and clean the data for both low-frequency (monthly) and high-frequency (weekly) variables. The data is read from Excel files and converted into time series (ts) format.

Model Specification: The MIDAS model is built using the midas_r function in R, which performs regression with different lags of high-frequency variables.

Forecasting: The model is used to forecast the unemployment rate one month ahead, incorporating dummy variables for months with significant structural changes (e.g., January, February, and specific months in 2020 due to the COVID-19 pandemic).

Model Evaluation: The forecast is evaluated for robustness using techniques like rolling windows and checking residuals for white noise, stationarity, and normality.

Key Files
Data.xlsx: Contains the input data for both monthly and weekly variables.
Scripts:
forecasting_model.R: Main script to build, run, and evaluate the MIDAS model. It includes the following key sections:
Data preparation and visualization.
Model construction using the midas_r function.
Forecast generation using the forecast function.
Model evaluation using residual diagnostics and tests for white noise and stationarity.
