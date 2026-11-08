## Modules

| Menu | File | Contents |
|---|---|---|
| Learning Curve | `R/ogrenme.R` | Learning curve model Y(u) = a u^(-b) |
| Forecasting | `R/forecasting.R` | Data upload (Excel / CSV); Moving Average, Weighted MA, Simple Exponential Smoothing, Seasonal Factors (quick and dirty); Holt's method; Seasonal Decomposition, Winters' method; AR, MA, ARMA, ARIMA (Box-Jenkins) |
| Aggregate Planning | `R/aggregate_planning.R` | Data upload (Excel / CSV) with editable period names; K entered directly or computed from past data; Heuristic plans: zero-inventory (chase), constant workforce (level) and mixed; Linear programming with options for overtime, idle time and subcontracting; Transportation method (north-west corner, cheapest cell first, optimal); Disaggregation of the aggregate plan with the square-root rule |

Each forecasting page shows the forecasts, a plot and MAD, MSE and MAPE. Parameters can be entered manually or searched; Box-Jenkins pages also show ACF/PACF plots and parameter estimates.

Each aggregate planning page shows the plan table (workers, hired, fired, production, inventory, overtime, subcontracting), a plot of cumulative production against cumulative net demand, and a cost breakdown. When the number of workers is fractional, a second plan with workers rounded up is shown next to the original, with its cost.

Packages: shiny, readxl, lpSolve.
