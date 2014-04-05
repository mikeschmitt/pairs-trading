pairs-trading (WIP)
=============
Stock pairs trading in R.


STATISTICS CONCEPTS:
  - Cointegration
  - Unit Roots
  - Augmented Dickey–Fuller Test
    - If L (lags) is too small, then remaining serial correlation in errors will bias the test.
    - If L is too large, then power of test will suffer.
    - Usually better to include too many lags than too few.

FINANCE CONCEPTS:
  - Pairs Trading

R CONCEPTS:
  - Functions
  - Default Parameters
  - Time Series
  - Plots
  - CSV File I/O

TO DO:
  - Include AIC test.
  - Set acceptance range of ADF test.
  - Generate model for accepted trading pairs.
  - Regression testing.
  - Use GS from quantmod library rather than Yahoo data.
  - Add thresholds for acceptable stocks (volume, min price, etc(.

COMPLETED:
  - Cointegration Function
  - ADF Test Function
  - Gather Stock History Function
  - Pull list of similar stocks from Yahoo Finance website to automatically generate list of trading pair possibilities.

ADDITIONAL NOTES:
  - Google's R style guide recommends UpperCamelCase for function naming and period.separated for variable naming.
