# production_planning_group18

EMU341 Production Planning - Group 18 Shiny application.

## Modules

| Menu | File | Contents |
|---|---|---|
| Learning Curve | `R/ogrenme.R` | Learning curve model Y(u) = a u^(-b) |
| Forecasting | `R/forecasting.R` | Data upload (Excel / CSV); Moving Average, Weighted MA, Simple Exponential Smoothing, Seasonal Factors (quick and dirty); Holt's method; Seasonal Decomposition, Winters' method; AR, MA, ARMA, ARIMA (Box-Jenkins) |

Each forecasting page shows the forecasts, a plot and MAD, MSE and MAPE.
Parameters can be entered manually or searched; Box-Jenkins pages also
show ACF/PACF plots and parameter estimates.

Packages: shiny, readxl.
