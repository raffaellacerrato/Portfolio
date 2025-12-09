This project provides a complete econometric analysis of the DAX index, focusing on return dynamics, volatility modeling, and market risk estimation.
The analysis includes data preprocessing, stationarity checks, ACF/PACF inspection, and the identification of stylized facts such as heavy tails and volatility clustering.

Multiple AR, MA, and ARMA models are estimated and compared using statistical metrics (Log-Likelihood, AIC, AICc, BIC) and residual diagnostics (Ljung–Box, Jarque–Bera).
To capture time-varying volatility, GARCH-family models are implemented, including GARCH(1,1), GJR-GARCH(1,1) and EGARCH(1,1), combined with ARMA specifications.

The project evaluates in-sample and out-of-sample forecasting performance, identifying ARMA(3,3)-EGARCH(1,1) and ARMA(2,2)-GARCH(1,1) as the best-performing models.
Market risk is then assessed using Value at Risk (VaR) and Expected Shortfall (ES), including backtesting procedures (Kupiec and Christoffersen tests) to verify model reliability.

The project demonstrates skills in time-series modeling, quantitative finance, risk measurement, and Python-based financial analysis.
