# =====================================================================
# Module: Forecasting (Nahmias, Chapter 2)  --  Assignment 2
#
# Menu "Forecasting" in the navbar:
#   Data                 upload Excel / CSV, pick the sheet and the series,
#                        set horizon, season length, error window
#   Stationary series    Moving Average, Weighted Moving Average,
#                        Simple Exponential Smoothing, Seasonal Factors
#                        (quick and dirty method)
#   Trend series         Holt's method (double exponential smoothing)
#   Seasonal series      Seasonal decomposition with moving averages,
#                        Winters' method (triple exponential smoothing)
#   Box-Jenkins          AR(p), MA(q), ARMA(p,q), ARIMA(p,d,q)
#
# Notation (slides):  F_t = forecast for period t made in t-1,
#   e_t = F_t - D_t,  MAD = mean|e_t|,  MSE = mean e_t^2,
#   MAPE = mean|e_t / D_t| x 100
#
# Packages: shiny, readxl (Excel upload) and base R "stats"
# (arima, acf, pacf). No other package is needed.
# All helper functions start with fc_ so they do not clash with
# functions of other modules.
# =====================================================================


# ---------------------------------------------------------------------
# 1) SMALL TOOLS
# ---------------------------------------------------------------------

# Integer from a numericInput (empty box -> default)
fc_int <- function(x, default) {
  if (is.null(x) || length(x) == 0 || is.na(x)) default else round(x)
}

# Number as text with k decimals ("" for NA)
fc_fmt <- function(x, k = 2) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = k, big.mark = ""))
}

# Season position (1..N) of period t
fc_season <- function(t, N) ((t - 1) %% N) + 1

# Least-squares line y = a + b x (slide formulas: b = Sxy / Sxx)
fc_line <- function(y, x) {
  b <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
  c(a = mean(y) - b * mean(x), b = b)
}

# Error measures of one-step forecasts, using periods t >= start
fc_errors <- function(d, fitted, start = 1) {
  ok <- !is.na(fitted) & seq_along(d) >= start
  if (!any(ok)) return(c(MAD = NA, MSE = NA, MAPE = NA, n = 0, from = NA))
  e  <- fitted[ok] - d[ok]
  dd <- d[ok]
  c(MAD  = mean(abs(e)),
    MSE  = mean(e^2),
    MAPE = if (any(dd != 0)) mean(abs(e / dd)[dd != 0]) * 100 else NA,
    n    = sum(ok),
    from = min(which(ok)))
}

# One number to minimise in a parameter search
fc_score <- function(d, fitted, start, crit) {
  v <- fc_errors(d, fitted, start)[[crit]]
  if (is.na(v)) Inf else v
}

# Grid of smoothing constants: coarse grid first, then a fine grid
# (step 0.01) around the best coarse point
fc_grid_search <- function(score_fun, dims, coarse = 0.05) {
  g1 <- seq(coarse, 1, by = coarse)
  grid <- as.matrix(expand.grid(rep(list(g1), dims)))
  sc <- apply(grid, 1, score_fun)
  best <- grid[which.min(sc), ]
  fine <- lapply(best, function(v) {
    s <- round(seq(v - coarse + 0.01, v + coarse - 0.01, by = 0.01), 2)
    s[s >= 0.01 & s <= 1]
  })
  grid2 <- as.matrix(expand.grid(fine))
  sc2 <- apply(grid2, 1, score_fun)
  all_p <- rbind(grid, grid2)
  all_s <- c(sc, sc2)
  keep <- !duplicated(round(all_p, 2))
  all_p <- all_p[keep, , drop = FALSE]
  all_s <- all_s[keep]
  o <- order(all_s)
  list(best = all_p[o[1], ], score = all_s[o[1]],
       top = cbind(all_p[o[1:min(5, length(o))], , drop = FALSE],
                   score = all_s[o[1:min(5, length(o))]]))
}


# ---------------------------------------------------------------------
# 2) FORECASTING METHODS
#    Every method returns a list:
#      fitted : one-step-ahead forecasts F_1..F_n (NA where none)
#      future : forecasts for periods n+1 .. n+h
#      extra  : data.frame with method details (or NULL)
#      note   : character vector shown above the plot
# ---------------------------------------------------------------------

# --- Moving average: F_t = (D_{t-1} + ... + D_{t-N}) / N --------------
fc_ma_fit <- function(d, N) {
  n <- length(d)
  f <- rep(NA_real_, n)
  if (n > N) for (t in (N + 1):n) f[t] <- mean(d[(t - N):(t - 1)])
  f
}

fc_ma <- function(d, N, h) {
  n <- length(d)
  nxt <- mean(d[(n - N + 1):n])
  list(fitted = fc_ma_fit(d, N), future = rep(nxt, h), extra = NULL,
       note = c(paste0("MA(", N, "): each forecast is the average of the last ",
                       N, " observations."),
                paste0("Forecast for period ", n + 1, " = (",
                       paste(d[n:(n - N + 1)], collapse = " + "), ") / ", N,
                       " = ", fc_fmt(nxt, 3),
                       ". A moving average gives the same value for every future period.")))
}

# --- Weighted moving average -------------------------------------------
# w[1] is the weight of the MOST RECENT period, w[N] the oldest.
fc_wma_lags <- function(d, N) {
  # rows for t = N+1..n : (D_{t-1}, D_{t-2}, ..., D_{t-N})
  embed(d[-length(d)], N)
}

fc_wma <- function(d, w, h) {
  w <- w / sum(w)
  N <- length(w)
  n <- length(d)
  f <- rep(NA_real_, n)
  f[(N + 1):n] <- as.numeric(fc_wma_lags(d, N) %*% w)
  nxt <- sum(w * d[n:(n - N + 1)])
  extra <- data.frame("Age (periods back)" = 1:N,
                      "Weight" = fc_fmt(w, 4),
                      "Data used for period n+1" = d[n:(n - N + 1)],
                      check.names = FALSE)
  list(fitted = f, future = rep(nxt, h), extra = extra,
       note = paste0("Weighted MA with N = ", N,
                     " (weights scaled to add up to 1). Forecast for period ",
                     n + 1, " = ", fc_fmt(nxt, 3), "."))
}

# All weight vectors with step 'step' that are >= 0 and add up to 1
fc_simplex <- function(N, step) {
  K <- round(1 / step)
  if (N == 1) return(matrix(1, 1, 1))
  g <- as.matrix(expand.grid(rep(list(0:K), N - 1)))
  g <- g[rowSums(g) <= K, , drop = FALSE]
  cbind(g, K - rowSums(g)) / K
}

fc_wma_search <- function(d, N, step, start, crit) {
  n <- length(d)
  W <- fc_simplex(N, step)                  # one weight set per row
  X <- fc_wma_lags(d, N)                    # lagged demand
  Fm <- X %*% t(W)                          # forecasts, one column per set
  tt <- (N + 1):n
  use <- tt >= start
  if (!any(use)) use <- rep(TRUE, length(tt))
  E <- Fm[use, , drop = FALSE] - d[tt][use]
  sc <- switch(crit,
    MAD  = colMeans(abs(E)),
    MSE  = colMeans(E^2),
    MAPE = colMeans(abs(E / d[tt][use])) * 100)
  o <- order(sc)[1:min(5, length(sc))]
  list(best = W[o[1], ],
       top = data.frame(Rank = seq_along(o),
                        Weights = apply(W[o, , drop = FALSE], 1,
                                        function(w) paste(fc_fmt(w, 2), collapse = ", ")),
                        Value = fc_fmt(sc[o], 3), check.names = FALSE))
}

# --- Simple exponential smoothing: F_{t+1} = a D_t + (1-a) F_t ----------
# S0 = initial level = F_1
fc_ses_fit <- function(d, a, S0) {
  n <- length(d)
  Fc <- numeric(n + 1)
  Fc[1] <- S0
  for (t in 1:n) Fc[t + 1] <- a * d[t] + (1 - a) * Fc[t]
  Fc                                        # F_1 .. F_{n+1}
}

fc_ses <- function(d, a, S0, h, init_txt) {
  n <- length(d)
  Fc <- fc_ses_fit(d, a, S0)
  list(fitted = Fc[1:n], future = rep(Fc[n + 1], h), extra = NULL,
       note = c(paste0("Simple exponential smoothing, alpha = ", a,
                       ".  F(t+1) = alpha D(t) + (1 - alpha) F(t)."),
                paste0("Initial level S0 = F1 = ", fc_fmt(S0, 3), " (", init_txt, ")."),
                paste0("Equivalent moving average length N = (2 - alpha) / alpha = ",
                       fc_fmt((2 - a) / a, 2), ".")))
}

# --- Seasonal factors, quick and dirty (stationary series) --------------
# 1) mean of all data  2) divide every value by the mean
# 3) average the ratios of the same season -> factors (sum = N)
fc_qd <- function(d, N, h) {
  n <- length(d)
  m <- mean(d)
  s <- fc_season(seq_len(n), N)
  ratio <- d / m
  raw <- tapply(ratio, s, mean)
  cf <- raw * N / sum(raw)
  fs <- fc_season(n + seq_len(h), N)
  extra <- data.frame("Season" = 1:N,
                      "Number of values" = as.integer(table(s)),
                      "Average ratio" = fc_fmt(as.numeric(raw), 4),
                      "Seasonal factor c" = fc_fmt(as.numeric(cf), 4),
                      "Forecast = mean x c" = fc_fmt(m * as.numeric(cf), 2),
                      check.names = FALSE)
  list(fitted = as.numeric(m * cf[s]), future = as.numeric(m * cf[fs]),
       extra = extra, fit_label = "Fitted (mean x c)",
       note = c(paste0("Quick and dirty seasonal factors, N = ", N,
                       ". Mean of all ", n, " values = ", fc_fmt(m, 3), "."),
                paste0("Factors sum to ", fc_fmt(sum(cf), 3),
                       " (= N). Forecast = mean x seasonal factor."),
                "The method assumes no trend; the 'Forecast' column shows mean x c for every period."))
}

# --- Holt's method (double exponential smoothing) ----------------------
# S_t = a D_t + (1-a)(S_{t-1} + G_{t-1})
# G_t = b (S_t - S_{t-1}) + (1-b) G_{t-1}
# F_{t,t+k} = S_t + k G_t
fc_holt_fit <- function(d, a, b, S0, G0) {
  n <- length(d)
  f <- S <- G <- numeric(n)
  Sp <- S0; Gp <- G0
  for (t in 1:n) {
    f[t] <- Sp + Gp
    S[t] <- a * d[t] + (1 - a) * (Sp + Gp)
    G[t] <- b * (S[t] - Sp) + (1 - b) * Gp
    Sp <- S[t]; Gp <- G[t]
  }
  list(f = f, S = S, G = G)
}

fc_holt <- function(d, a, b, S0, G0, h, init_txt) {
  n <- length(d)
  r <- fc_holt_fit(d, a, b, S0, G0)
  future <- r$S[n] + seq_len(h) * r$G[n]
  extra <- data.frame("Period t" = 0:n,
                      "Demand D" = c(NA, d),
                      "Forecast F = S(t-1) + G(t-1)" = fc_fmt(c(NA, r$f), 2),
                      "Level S" = fc_fmt(c(S0, r$S), 3),
                      "Slope G" = fc_fmt(c(G0, r$G), 3),
                      check.names = FALSE)
  extra[["Demand D"]] <- ifelse(is.na(extra[["Demand D"]]), "", extra[["Demand D"]])
  list(fitted = r$f, future = future, extra = extra,
       note = c(paste0("Holt's method, alpha = ", a, ", beta = ", b, "."),
                paste0("Initial values: S0 = ", fc_fmt(S0, 3), ", G0 = ",
                       fc_fmt(G0, 3), " (", init_txt, ")."),
                paste0("Forecast made in period ", n, ": F(", n, ", ", n, "+k) = ",
                       fc_fmt(r$S[n], 3), " + k x ", fc_fmt(r$G[n], 3), ".")))
}

# --- Seasonal decomposition with moving averages ------------------------
# 1) centered N-period MA  2) ratio D / CMA  3) average like seasons,
# rescale to sum N  4) deseasonalize  5) trend line, then x factor
fc_cma <- function(d, N) {
  n <- length(d)
  cma <- rep(NA_real_, n)
  if (N %% 2 == 1) {                        # odd N: MA lands on a period
    k <- (N - 1) / 2
    if (n > 2 * k) for (t in (k + 1):(n - k)) cma[t] <- mean(d[(t - k):(t + k)])
  } else {                                  # even N: average two MAs
    k <- N / 2
    if (n > 2 * k) for (t in (k + 1):(n - k)) {
      cma[t] <- (mean(d[(t - k):(t + k - 1)]) + mean(d[(t - k + 1):(t + k)])) / 2
    }
  }
  cma
}

fc_decomp <- function(d, N, h, fill_ends = TRUE, trend = "linear") {
  n <- length(d)
  tt <- seq_len(n)
  cma <- fc_cma(d, N)
  cma_used <- cma
  if (fill_ends) {                          # slides: use nearest interior CMAs
    k <- floor(N / 2)
    idx <- which(!is.na(cma))
    if (length(idx) >= k && k > 0) {
      cma_used[seq_len(idx[1] - 1)] <- mean(cma[idx[1:k]])
      last <- idx[length(idx)]
      if (last < n) cma_used[(last + 1):n] <- mean(cma[tail(idx, k)])
    }
  }
  s <- fc_season(tt, N)
  ratio <- d / cma_used
  raw <- tapply(ratio, s, mean, na.rm = TRUE)
  cf <- raw * N / sum(raw)
  deseas <- d / cf[s]
  if (trend == "linear") {
    ab <- fc_line(as.numeric(deseas), tt)
    trend_txt <- paste0("trend on deseasonalized data: ", fc_fmt(ab[["a"]], 4),
                        " + ", fc_fmt(ab[["b"]], 4), " t")
  } else {
    ab <- c(a = mean(deseas), b = 0)
    trend_txt <- paste0("flat level (mean of deseasonalized data) = ",
                        fc_fmt(ab[["a"]], 4))
  }
  ft <- n + seq_len(h)
  fitted <- (ab[["a"]] + ab[["b"]] * tt) * cf[s]
  future <- (ab[["a"]] + ab[["b"]] * ft) * cf[fc_season(ft, N)]
  extra <- data.frame("Period" = tt, "Season" = s, "Demand" = d,
                      "Centered MA" = fc_fmt(cma, 4),
                      "CMA used" = fc_fmt(cma_used, 4),
                      "Ratio D / CMA" = fc_fmt(ratio, 4),
                      "Factor c" = fc_fmt(as.numeric(cf[s]), 4),
                      "Deseasonalized" = fc_fmt(as.numeric(deseas), 3),
                      check.names = FALSE)
  fac <- paste(paste0("c", 1:N, " = ", fc_fmt(as.numeric(cf), 4)), collapse = ", ")
  list(fitted = as.numeric(fitted), future = as.numeric(future), extra = extra,
       fit_label = "Fitted (trend x c)",
       note = c(paste0("Seasonal decomposition, N = ", N,
                       if (N %% 2 == 0) " (even N: centered MA)" else " (odd N: no centering needed)",
                       if (fill_ends) "; end periods filled from the nearest centered MAs." else "."),
                paste0("Seasonal factors: ", fac, "  (sum = ", fc_fmt(sum(cf), 3), ")."),
                paste0("Forecast = (", trend_txt, ") x seasonal factor.")))
}

# --- Winters' method (triple exponential smoothing) ---------------------
# S_t = a (D_t / c_{t-N}) + (1-a)(S_{t-1} + G_{t-1})
# G_t = b (S_t - S_{t-1}) + (1-b) G_{t-1}
# c_t = g (D_t / S_t) + (1-g) c_{t-N}, then the last N factors are
#       renormed to add up to N
# F_{t,t+k} = (S_t + k G_t) c_{t+k-N}
# The first 2N periods are the initialization data D_{-2N+1}, ..., D_0.

fc_winters_init <- function(d, N, type, S0 = NA, G0 = NA) {
  x <- seq_len(2 * N) - 2 * N               # labels -2N+1 .. 0
  y <- d[1:(2 * N)]
  V1 <- mean(y[1:N]); V2 <- mean(y[(N + 1):(2 * N)])
  if (type == "nahmias") {
    G0 <- (V2 - V1) / N
    S0 <- V2 + G0 * (N - 1) / 2
  } else if (type == "regression") {
    ab <- fc_line(y, x)
    S0 <- ab[["a"]]; G0 <- ab[["b"]]
  }
  line <- S0 + G0 * x                       # point on the line for each period
  raw <- y / line
  cf <- (raw[1:N] + raw[(N + 1):(2 * N)]) / 2
  cf <- cf * N / sum(cf)                    # c_{-N+1} .. c_0
  list(S0 = S0, G0 = G0, c0 = cf, V1 = V1, V2 = V2, raw = raw)
}

fc_winters_fit <- function(d, N, a, b, g, S0, G0, c0) {
  n <- length(d)
  cc <- rep(NA_real_, n)
  cc[(N + 1):(2 * N)] <- c0
  crec <- cc                                # factor as it was when computed
  f <- S <- G <- rep(NA_real_, n)
  Sp <- S0; Gp <- G0
  for (t in (2 * N + 1):n) {
    f[t] <- (Sp + Gp) * cc[t - N]
    S[t] <- a * d[t] / cc[t - N] + (1 - a) * (Sp + Gp)
    G[t] <- b * (S[t] - Sp) + (1 - b) * Gp
    cc[t] <- g * d[t] / S[t] + (1 - g) * cc[t - N]
    last <- (t - N + 1):t
    cc[last] <- cc[last] * N / sum(cc[last])
    crec[t] <- cc[t]
    Sp <- S[t]; Gp <- G[t]
  }
  list(f = f, S = S, G = G, c = cc, crec = crec, Sn = Sp, Gn = Gp)
}

fc_winters <- function(d, N, a, b, g, ini, h, init_txt) {
  n <- length(d)
  r <- fc_winters_fit(d, N, a, b, g, ini$S0, ini$G0, ini$c0)
  k <- seq_len(h)
  last_c <- r$c[(n - N + 1):n]              # newest factor of every season
  future <- (r$Sn + k * r$Gn) * last_c[fc_season(k, N)]
  lab <- as.integer(seq_len(n) - 2 * N)
  extra <- data.frame("Period" = seq_len(n),
                      "Nahmias label" = lab,
                      "Demand" = d,
                      "Forecast" = fc_fmt(r$f, 2),
                      "Level S" = fc_fmt(r$S, 3),
                      "Slope G" = fc_fmt(r$G, 3),
                      "Factor c" = fc_fmt(r$crec, 4),
                      check.names = FALSE)
  list(fitted = r$f, future = future, extra = extra,
       note = c(paste0("Winters' method, alpha = ", a, ", beta = ", b,
                       ", gamma = ", g, ", N = ", N, "."),
                paste0("Periods 1-", 2 * N, " are the initialization data (labels ",
                       -2 * N + 1, " .. 0); forecasts start at period ", 2 * N + 1, "."),
                paste0("Initial values (", init_txt, "): S0 = ", fc_fmt(ini$S0, 3),
                       ", G0 = ", fc_fmt(ini$G0, 4),
                       if (init_txt == "Nahmias two-season method")
                         paste0("  [V1 = ", fc_fmt(ini$V1, 3), ", V2 = ", fc_fmt(ini$V2, 3), "]")
                       else ""),
                paste0("Initial factors: ",
                       paste(paste0("c", 1:N, " = ", fc_fmt(ini$c0, 4)), collapse = ", "), ".")))
}

# --- Box-Jenkins models (stats::arima) ----------------------------------
fc_arima_fit <- function(d, p, dd, q) {
  for (m in c("CSS-ML", "ML", "CSS")) {
    fit <- tryCatch(suppressWarnings(
      stats::arima(d, order = c(p, dd, q), include.mean = (dd == 0), method = m)),
      error = function(e) NULL)
    if (!is.null(fit)) return(fit)
  }
  NULL
}

fc_1 <- function(x) {                     # one finite number, otherwise NA
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 1 && is.finite(x)) x else NA_real_
}

fc_bic <- function(fit) fc_1(tryCatch(stats::BIC(fit), error = function(e) NA))

# d with the lowest standard deviation among 0, 1, 2 differences
fc_choose_d <- function(d, dmax = 2) {
  s <- sapply(0:dmax, function(k) if (k == 0) sd(d) else sd(diff(d, differences = k)))
  list(d = which.min(s) - 1, sd = s)
}

fc_bj_search <- function(d, ps, dd, qs, ic) {
  rows <- list()
  for (p in ps) for (q in qs) {
    if (p == 0 && q == 0 && dd == 0) next
    fit <- fc_arima_fit(d, p, dd, q)
    if (is.null(fit)) next
    rows[[length(rows) + 1]] <- data.frame(p = p, d = dd, q = q,
                                           AIC = fc_1(fit$aic), BIC = fc_bic(fit))
  }
  if (length(rows) == 0) return(NULL)
  tab <- do.call(rbind, rows)
  tab <- tab[is.finite(tab[[ic]]), , drop = FALSE]
  if (nrow(tab) == 0) return(NULL)
  tab[order(tab[[ic]]), , drop = FALSE]
}

fc_bj <- function(d, p, dd, q, h, label) {
  fit <- fc_arima_fit(d, p, dd, q)
  if (is.null(fit)) return(NULL)
  n <- length(d)
  fitted <- as.numeric(d - residuals(fit))
  if (dd > 0) fitted[seq_len(dd)] <- NA     # no real forecast for the first d periods
  pr <- predict(fit, n.ahead = h)
  mean_fc <- as.numeric(pr$pred)
  se <- as.numeric(pr$se)
  # parameter table
  est <- coef(fit)
  if (length(est) == 0) est <- c("(no parameters)" = NA_real_)
  V <- fit$var.coef
  se_c <- if (!is.null(V) && length(V) > 0) sqrt(pmax(diag(V), 0)) else rep(NA, length(est))
  se_c <- se_c[names(est)]
  z <- est / se_c
  coef_tab <- data.frame("Parameter" = names(est),
                         "Estimate" = fc_fmt(est, 4),
                         "Std. error" = fc_fmt(se_c, 4),
                         "z" = fc_fmt(z, 3),
                         "p-value" = fc_fmt(2 * pnorm(-abs(z)), 4),
                         check.names = FALSE)
  # residual check
  res <- as.numeric(residuals(fit))
  lb_lag <- max(p + q + 1, min(10, floor(n / 4)))
  lb <- tryCatch(Box.test(res, lag = lb_lag, type = "Ljung-Box", fitdf = p + q),
                 error = function(e) NULL)
  fit_tab <- data.frame(
    "Item" = c("Model", "Observations used", "Log-likelihood", "AIC", "BIC",
               "Residual variance (sigma^2)",
               paste0("Ljung-Box Q (lag ", lb_lag, ")"), "Ljung-Box p-value"),
    "Value" = c(paste0("ARIMA(", p, ",", dd, ",", q, ")"),
                fc_fmt(fc_1(fit$nobs), 0),
                fc_fmt(fc_1(fit$loglik), 3), fc_fmt(fc_1(fit$aic), 3), fc_fmt(fc_bic(fit), 3),
                fc_fmt(fc_1(fit$sigma2), 4),
                if (is.null(lb)) "" else fc_fmt(unname(lb$statistic), 3),
                if (is.null(lb)) "" else fc_fmt(lb$p.value, 4)),
    check.names = FALSE)
  list(fitted = fitted, future = mean_fc,
       lower = mean_fc - 1.96 * se, upper = mean_fc + 1.96 * se,
       fit = fit, coef_tab = coef_tab, fit_tab = fit_tab, resid = res,
       order = c(p, dd, q), extra = NULL,
       note = c(paste0(label, ": ARIMA(", p, ",", dd, ",", q, ") estimated with stats::arima",
                       if (dd == 0) " (with a constant mean)." else "."),
                if (!is.null(lb))
                  paste0("Ljung-Box p-value = ", fc_fmt(lb$p.value, 3),
                         if (lb$p.value > 0.05) " > 0.05: residuals look like white noise."
                         else " <= 0.05: some autocorrelation is left in the residuals."),
                "Shaded band = 95% prediction interval."))
}


# ---------------------------------------------------------------------
# 3) READING UPLOADED FILES
# ---------------------------------------------------------------------

# Text to number; accepts "12,5" as 12.5 when there is no point
fc_as_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x <- ifelse(grepl(",", x) & !grepl("\\.", x), sub(",", ".", x), x)
  suppressWarnings(as.numeric(x))
}

fc_read_csv <- function(path, sep = "auto", dec = "auto") {
  lines <- readLines(path, n = 30, warn = FALSE, encoding = "UTF-8")
  lines <- sub("^\ufeff", "", lines)
  lines <- lines[nzchar(trimws(lines))]
  if (sep == "auto") {
    cnt <- sapply(c(";", "\t", ","), function(s)
      sum(lengths(regmatches(lines, gregexpr(s, lines, fixed = TRUE)))))
    sep <- if (cnt[["\t"]] > 0 && cnt[["\t"]] >= max(cnt)) "\t"
           else if (cnt[[";"]] > 0 && cnt[[";"]] >= cnt[[","]] / 2) ";"
           else ","
  }
  if (dec == "auto") {
    dec <- if (sep != "," && any(grepl("[0-9],[0-9]", lines))) "," else "."
  }
  df <- tryCatch(read.csv(path, sep = sep, dec = dec, check.names = FALSE,
                          stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
                 error = function(e) read.csv(path, sep = sep, dec = dec,
                                              check.names = FALSE,
                                              stringsAsFactors = FALSE))
  attr(df, "csv_info") <- paste0("separator '", if (sep == "\t") "tab" else sep,
                                 "', decimal mark '", dec, "'")
  df
}

# Columns that hold at least 3 numbers
fc_numeric_cols <- function(df) {
  names(df)[vapply(df, function(col) sum(!is.na(fc_as_num(col))) >= 3, logical(1))]
}


# ---------------------------------------------------------------------
# 4) SCREEN (UI)
#    forecasting_ui() returns a navbarMenu, so in app.R it is placed
#    directly inside navbarPage (not inside a tabPanel).
# ---------------------------------------------------------------------

# Common layout of a method page: controls on the left, results right
fc_page_ui <- function(ns, key, title, controls, bj = FALSE) {
  o <- function(x) ns(paste0(key, "_", x))
  sidebarLayout(
    sidebarPanel(width = 4,
      h4(title),
      controls,
      hr(),
      helpText("Data, forecast horizon, season length, error window and search",
               "criterion are set on the Forecasting > Data page.")
    ),
    mainPanel(width = 8,
      h4(textOutput(o("title"))),
      uiOutput(o("note")),
      plotOutput(o("plot"), height = "340px"),
      fluidRow(
        column(5, h5(strong("Error measures (MAD, MSE, MAPE)")), tableOutput(o("err"))),
        column(7, h5(strong("Future forecasts")), tableOutput(o("future")))
      ),
      if (bj) tagList(
        fluidRow(
          column(5, h5(strong("Model summary")), tableOutput(o("fitstats"))),
          column(7, h5(strong("Parameter estimates")), tableOutput(o("coef")))
        ),
        h5(strong("ACF and PACF")),
        radioButtons(o("acfsrc"), NULL, inline = TRUE,
                     choices = c("Series" = "series",
                                 "Differenced series" = "diff",
                                 "Residuals" = "resid"),
                     selected = "series"),
        plotOutput(o("acf"), height = "300px")
      ),
      uiOutput(o("search_head")),
      tableOutput(o("search")),
      h5(strong("Forecasts by period")),
      tableOutput(o("period")),
      uiOutput(o("extra_head")),
      tableOutput(o("extra"))
    )
  )
}

# "Manual / Search" switch
fc_mode_ui <- function(ns, key, search_label = "Search for the best value") {
  radioButtons(ns(paste0(key, "_mode")), "Parameters",
               choices = setNames(c("manual", "search"),
                                  c("Enter manually", search_label)))
}

# Condition for conditionalPanel inside the module
fc_cond <- function(key, value) paste0("input.", key, "_mode == '", value, "'")

# Box-Jenkins page controls (type = ar, ma, arma, arima)
fc_bj_controls <- function(ns, key, type) {
  i <- function(x) ns(paste0(key, "_", x))
  has_p <- type %in% c("ar", "arma", "arima")
  has_q <- type %in% c("ma", "arma", "arima")
  p_min <- if (type == "arima") 0 else 1
  tagList(
    radioButtons(i("mode"), "Model order",
                 choices = c("Enter manually" = "manual",
                             "Search (lowest AIC or BIC)" = "search")),
    conditionalPanel(fc_cond(key, "manual"), ns = ns,
      if (has_p) numericInput(i("p"), "p (AR order)", 1, min = p_min, max = 6, step = 1),
      if (has_q) numericInput(i("q"), "q (MA order)", 1, min = p_min, max = 6, step = 1)
    ),
    conditionalPanel(fc_cond(key, "search"), ns = ns,
      if (has_p) numericInput(i("pmax"), "Largest p to try", 3, min = 1, max = 6, step = 1),
      if (has_q) numericInput(i("qmax"), "Largest q to try", 3, min = 1, max = 6, step = 1),
      selectInput(i("ic"), "Information criterion", c("AIC", "BIC"))
    ),
    if (type == "arima") tagList(
      radioButtons(i("dmode"), "Differencing d",
                   choices = c("Automatic (d = 0, 1, 2 with the lowest std. deviation)" = "auto",
                               "Enter manually" = "manual")),
      conditionalPanel(paste0("input.", key, "_dmode == 'manual'"), ns = ns,
        numericInput(i("d"), "d (number of differences)", 1, min = 0, max = 2, step = 1))
    ),
    if (type != "arima")
      helpText("AR, MA and ARMA models are fitted to the series as it is (d = 0).",
               "For a series with a trend use the ARIMA page.")
  )
}

forecasting_ui <- function(id) {
  ns <- NS(id)

  data_page <- sidebarLayout(
    sidebarPanel(width = 4,
      h4("1. Data"),
      fileInput(ns("file"), "Upload an Excel (.xlsx, .xls) or CSV (.csv, .txt) file",
                accept = c(".xlsx", ".xls", ".csv", ".txt")),
      uiOutput(ns("sheet_ui")),
      selectInput(ns("col"), "Series (column) to forecast", choices = NULL),
      selectInput(ns("csv_sep"), "CSV separator",
                  c("Auto-detect" = "auto", "Comma (,)" = ",",
                    "Semicolon (;)" = ";", "Tab" = "\t")),
      selectInput(ns("csv_dec"), "CSV decimal mark",
                  c("Auto-detect" = "auto", "Point (.)" = ".", "Comma (,)" = ",")),
      hr(),
      h4("2. Settings used by all methods"),
      numericInput(ns("h"), "Forecast horizon (number of future periods)", 4, min = 1, max = 60, step = 1),
      numericInput(ns("N"), "Season length N (seasonal methods)", 4, min = 2, max = 52, step = 1),
      numericInput(ns("start"), "Error measures use periods from", 1, min = 1, step = 1),
      selectInput(ns("crit"), "Criterion for parameter search", c("MSE", "MAD", "MAPE")),
      helpText("Set 'Error measures use periods from' to the same value on every",
               "method to compare methods on the same periods.")
    ),
    mainPanel(width = 8,
      h4(textOutput(ns("data_title"))),
      uiOutput(ns("data_info")),
      plotOutput(ns("data_plot"), height = "320px"),
      h5(strong("Summary")),
      tableOutput(ns("data_summary")),
      h5(strong("Data")),
      tableOutput(ns("data_table"))
    )
  )

  navbarMenu("Forecasting",
    tabPanel("Data", data_page),
    "----",
    "Stationary series",
    tabPanel("Moving Average", fc_page_ui(ns, "ma", "Moving Average",
      tagList(
        fc_mode_ui(ns, "ma", "Search for the best N"),
        conditionalPanel(fc_cond("ma", "manual"), ns = ns,
          numericInput(ns("ma_N"), "N (number of past periods)", 3, min = 1, step = 1)),
        conditionalPanel(fc_cond("ma", "search"), ns = ns,
          numericInput(ns("ma_Nmax"), "Try N = 1 ... ", 6, min = 2, step = 1))
      ))),
    tabPanel("Weighted Moving Average", fc_page_ui(ns, "wma", "Weighted Moving Average",
      tagList(
        fc_mode_ui(ns, "wma", "Search for the best weights"),
        conditionalPanel(fc_cond("wma", "manual"), ns = ns,
          textInput(ns("wma_w"), "Weights, most recent period first (comma separated)", "3, 2, 1"),
          helpText("Example from the slides: 3, 2, 1 (divided by 6).")),
        conditionalPanel(fc_cond("wma", "search"), ns = ns,
          numericInput(ns("wma_N"), "N (number of weights, 1-5)", 3, min = 1, max = 5, step = 1),
          selectInput(ns("wma_step"), "Weight grid step", c("0.05" = 0.05, "0.1" = 0.1)),
          helpText("All weight sets with weights >= 0 that add up to 1 are tried."))
      ))),
    tabPanel("Simple Exponential Smoothing", fc_page_ui(ns, "ses", "Simple Exponential Smoothing",
      tagList(
        fc_mode_ui(ns, "ses", "Search for the best alpha"),
        conditionalPanel(fc_cond("ses", "manual"), ns = ns,
          sliderInput(ns("ses_a"), "alpha", 0.01, 1, 0.1, step = 0.01)),
        radioButtons(ns("ses_init"), "Initial level S0 (= F1)",
                     choices = c("Regression on the first k periods" = "regression",
                                 "Enter manually" = "manual")),
        conditionalPanel("input.ses_init == 'regression'", ns = ns,
          numericInput(ns("ses_k"), "k (periods used by the regression)", 4, min = 2, step = 1)),
        conditionalPanel("input.ses_init == 'manual'", ns = ns,
          numericInput(ns("ses_S0"), "S0", NA))
      ))),
    tabPanel("Seasonal Factors (Quick and Dirty)", fc_page_ui(ns, "qd", "Seasonal Factors - Quick and Dirty Method",
      tagList(
        helpText("For a stationary series with seasonality (no trend).",
                 "The only parameter is the season length N (Data page)."),
        helpText("At least two full seasons of data are needed.")
      ))),
    "----",
    "Trend series",
    tabPanel("Holt's Method", fc_page_ui(ns, "holt", "Holt's Method (Double Exponential Smoothing)",
      tagList(
        fc_mode_ui(ns, "holt", "Search for the best alpha and beta"),
        conditionalPanel(fc_cond("holt", "manual"), ns = ns,
          sliderInput(ns("holt_a"), "alpha (level)", 0.01, 1, 0.1, step = 0.01),
          sliderInput(ns("holt_b"), "beta (trend)", 0.01, 1, 0.1, step = 0.01)),
        radioButtons(ns("holt_init"), "Initial level S0 and slope G0",
                     choices = c("Regression on the first k periods" = "regression",
                                 "Enter manually" = "manual")),
        conditionalPanel("input.holt_init == 'regression'", ns = ns,
          numericInput(ns("holt_k"), "k (periods used by the regression)", 4, min = 2, step = 1)),
        conditionalPanel("input.holt_init == 'manual'", ns = ns,
          numericInput(ns("holt_S0"), "S0 (level at period 0)", NA),
          numericInput(ns("holt_G0"), "G0 (slope at period 0)", NA))
      ))),
    "----",
    "Seasonal series",
    tabPanel("Seasonal Decomposition", fc_page_ui(ns, "dec", "Seasonal Decomposition with Moving Averages",
      tagList(
        checkboxInput(ns("dec_fill"), "Fill the end periods from the nearest centered MAs (as in the slides)", TRUE),
        radioButtons(ns("dec_trend"), "Forecast of the deseasonalized series",
                     choices = c("Linear trend (least squares)" = "linear",
                                 "Flat level (mean), if there is no trend" = "flat")),
        helpText("Season length N is set on the Data page. At least two full seasons are needed.")
      ))),
    tabPanel("Winters' Method", fc_page_ui(ns, "win", "Winters' Method (Triple Exponential Smoothing)",
      tagList(
        fc_mode_ui(ns, "win", "Search for the best alpha, beta, gamma"),
        conditionalPanel(fc_cond("win", "manual"), ns = ns,
          sliderInput(ns("win_a"), "alpha (level)", 0.01, 1, 0.2, step = 0.01),
          sliderInput(ns("win_b"), "beta (trend)", 0.01, 1, 0.1, step = 0.01),
          sliderInput(ns("win_g"), "gamma (seasonal)", 0.01, 1, 0.1, step = 0.01)),
        radioButtons(ns("win_init"), "Initial level S0 and slope G0 (from the first 2N periods)",
                     choices = c("Regression line on the first 2N periods" = "regression",
                                 "Nahmias two-season method (V1, V2)" = "nahmias",
                                 "Enter manually" = "manual")),
        conditionalPanel("input.win_init == 'manual'", ns = ns,
          numericInput(ns("win_S0"), "S0 (level at period 0)", NA),
          numericInput(ns("win_G0"), "G0 (slope per period)", NA)),
        helpText("Initial seasonal factors: each of the first 2N values is divided",
                 "by the point on the initial line, like seasons are averaged and",
                 "the factors are normed to add up to N.")
      ))),
    "----",
    "Box-Jenkins",
    tabPanel("AR(p) Model",        fc_page_ui(ns, "ar",    "Autoregressive Model AR(p)",           fc_bj_controls(ns, "ar", "ar"),       bj = TRUE)),
    tabPanel("MA(q) Model",        fc_page_ui(ns, "bjma",  "Moving Average Model MA(q)",           fc_bj_controls(ns, "bjma", "ma"),     bj = TRUE)),
    tabPanel("ARMA(p,q) Model",    fc_page_ui(ns, "arma",  "ARMA(p,q) Model",                      fc_bj_controls(ns, "arma", "arma"),   bj = TRUE)),
    tabPanel("ARIMA(p,d,q) Model", fc_page_ui(ns, "arima", "ARIMA(p,d,q) Model",                   fc_bj_controls(ns, "arima", "arima"), bj = TRUE))
  )
}


# ---------------------------------------------------------------------
# 5) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------

# Plot: demand, one-step forecasts and future forecasts
fc_plot <- function(r, title) {
  d <- r$d; n <- length(d); h <- length(r$future)
  xf <- n + seq_len(h)
  yl <- range(c(d, r$fitted, r$future, r$lower, r$upper), na.rm = TRUE)
  par(mar = c(4, 4, 2.5, 1))
  plot(seq_len(n), d, type = "o", pch = 19, col = "grey25",
       xlim = c(1, n + h), ylim = yl, xlab = "Period", ylab = "Value", main = title)
  if (!is.null(r$lower))
    polygon(c(xf, rev(xf)), c(r$lower, rev(r$upper)),
            col = adjustcolor("#B4501E", 0.15), border = NA)
  lines(seq_len(n), r$fitted, type = "o", pch = 17, lty = 2, col = "#2F6FB0")
  lines(c(n, xf), c(d[n], r$future), type = "o", pch = 17, lwd = 2, col = "#B4501E")
  abline(v = n + 0.5, lty = 3, col = "grey60")
  fl <- if (is.null(r$fit_label)) "One-step-ahead forecast" else r$fit_label
  legend("topleft", bty = "n", cex = 0.9,
         legend = c("Actual data", fl, "Future forecast"),
         col = c("grey25", "#2F6FB0", "#B4501E"), pch = c(19, 17, 17), lty = c(1, 2, 1))
}

# Register the common outputs of one method page
fc_outputs <- function(output, key, res, label) {
  o <- function(x) paste0(key, "_", x)
  # The title shows any warning message (e.g. "not enough data");
  # the other outputs stay empty instead of repeating it.
  ok <- function() {
    r <- tryCatch(res(), error = function(e) NULL)
    req(r)
    r
  }

  output[[o("title")]] <- renderText({
    r <- res()
    paste0(label, "  -  forecast for period ", length(r$d) + 1, ": ", fc_fmt(r$future[1], 2))
  })
  output[[o("note")]] <- renderUI({
    tags$div(style = "color:#444;", lapply(ok()$note, tags$p))
  })
  output[[o("plot")]] <- renderPlot(fc_plot(ok(), label))
  output[[o("err")]] <- renderTable({
    e <- ok()$errors
    validate(need(e[["n"]] > 0, "No forecasts in the selected error window."))
    data.frame("Measure" = c("MAD", "MSE", "MAPE (%)", "Periods used"),
               "Value" = c(fc_fmt(e[["MAD"]], 3), fc_fmt(e[["MSE"]], 3),
                           fc_fmt(e[["MAPE"]], 3),
                           paste0(e[["n"]], " (", e[["from"]], " to ", length(ok()$d), ")")),
               check.names = FALSE)
  })
  output[[o("future")]] <- renderTable({
    r <- ok(); n <- length(r$d)
    tab <- data.frame("Period" = as.integer(n + seq_along(r$future)),
                      "Forecast" = fc_fmt(r$future, 3), check.names = FALSE)
    if (!is.null(r$lower)) {
      tab[["95% lower"]] <- fc_fmt(r$lower, 3)
      tab[["95% upper"]] <- fc_fmt(r$upper, 3)
    }
    tab
  })
  output[[o("period")]] <- renderTable({
    r <- ok()
    fl <- if (is.null(r$fit_label)) "Forecast F" else r$fit_label
    tab <- data.frame("Period t" = seq_along(r$d), "Demand D" = r$d,
                      "F" = fc_fmt(r$fitted, 3),
                      "Error e = F - D" = fc_fmt(r$fitted - r$d, 3),
                      check.names = FALSE)
    names(tab)[3] <- fl
    tab
  })
  output[[o("search_head")]] <- renderUI({
    if (!is.null(ok()$search)) h5(strong(ok()$search_title))
  })
  output[[o("search")]] <- renderTable({
    req(ok()$search)
    ok()$search
  })
  output[[o("extra_head")]] <- renderUI({
    if (!is.null(ok()$extra)) h5(strong("Method details"))
  })
  output[[o("extra")]] <- renderTable({
    req(ok()$extra)
    ok()$extra
  })
}

forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # --- Data -------------------------------------------------------------
    file_ext <- reactive({
      req(input$file)
      tolower(tools::file_ext(input$file$name))
    })

    sheets <- reactive({
      if (file_ext() %in% c("xlsx", "xls")) readxl::excel_sheets(input$file$datapath) else NULL
    })

    output$sheet_ui <- renderUI({
      req(input$file)
      if (!is.null(sheets()))
        selectInput(session$ns("sheet"), "Excel sheet", sheets())
    })

    table_in <- reactive({
      validate(need(input$file, "Upload an Excel or CSV file on the Forecasting > Data page to start."))
      ext <- file_ext()
      df <- if (ext %in% c("xlsx", "xls")) {
        sh <- if (!is.null(input$sheet) && input$sheet %in% sheets()) input$sheet else sheets()[1]
        tryCatch(as.data.frame(readxl::read_excel(input$file$datapath, sheet = sh)),
                 error = function(e) NULL)
      } else {
        tryCatch(fc_read_csv(input$file$datapath, input$csv_sep, input$csv_dec),
                 error = function(e) NULL)
      }
      validate(need(!is.null(df) && nrow(df) > 0,
                    "The file could not be read. Use .xlsx, .xls or .csv with one column per series."))
      df
    })

    observeEvent(table_in(), {
      cols <- fc_numeric_cols(table_in())
      lc <- tolower(cols)
      pref <- cols[grepl("demand|sales|value|failure|usage|cars|order|series", lc)]
      time_like <- grepl("^(period|time|t|week|month|quarter|year|day|date|index|no|id)$", lc)
      sel <- if (length(pref) > 0) pref[1]
             else if (any(!time_like)) cols[!time_like][1]
             else tail(cols, 1)
      updateSelectInput(session, "col", choices = cols, selected = sel)
    })

    series <- reactive({
      df <- table_in()
      validate(need(length(fc_numeric_cols(df)) > 0,
                    "No numeric column with at least 3 values was found."))
      req(input$col, input$col %in% names(df))
      d <- fc_as_num(df[[input$col]])
      d <- d[!is.na(d)]
      validate(need(length(d) >= 3, "The series needs at least 3 numeric values."))
      d
    })

    set <- reactive({
      list(h = max(1, fc_int(input$h, 4)),
           N = max(2, fc_int(input$N, 4)),
           start = max(1, fc_int(input$start, 1)),
           crit = input$crit)
    })

    # Put sensible starting numbers in the "Enter manually" boxes
    observeEvent(list(series(), set()$N, input$holt_k), {
      d <- series(); n <- length(d); N <- set()$N
      updateNumericInput(session, "ses_S0", value = d[1])
      k <- min(n, max(2, fc_int(input$holt_k, 4)))
      ab <- fc_line(d[1:k], seq_len(k))
      updateNumericInput(session, "holt_S0", value = round(ab[["a"]], 3))
      updateNumericInput(session, "holt_G0", value = round(ab[["b"]], 3))
      if (n >= 2 * N) {
        ini <- fc_winters_init(d, N, "nahmias")
        updateNumericInput(session, "win_S0", value = round(ini$S0, 3))
        updateNumericInput(session, "win_G0", value = round(ini$G0, 4))
      }
    })

    output$data_title <- renderText({
      validate(need(input$file, "No data yet: upload an Excel or CSV file."))
      paste0("File: ", input$file$name,
             if (!is.null(input$col) && nzchar(input$col)) paste0("  -  series: ", input$col) else "")
    })
    output$data_info <- renderUI({
      req(input$file)
      d <- series()
      msg <- paste0(length(d), " observations will be used by every forecasting page.")
      if (!(file_ext() %in% c("xlsx", "xls")))
        msg <- c(msg, paste0("CSV read with ", attr(table_in(), "csv_info"), "."))
      tags$div(lapply(msg, tags$p))
    })
    output$data_plot <- renderPlot({
      req(input$file)
      d <- series()
      par(mar = c(4, 4, 1, 1))
      plot(seq_along(d), d, type = "o", pch = 19, col = "grey25",
           xlab = "Period", ylab = "Value")
      abline(h = mean(d), lty = 3, col = "grey60")
    })
    output$data_summary <- renderTable({
      req(input$file)
      d <- series()
      tr <- fc_line(d, seq_along(d))
      data.frame("n" = length(d), "Mean" = fc_fmt(mean(d), 3),
                 "Std. deviation" = fc_fmt(sd(d), 3),
                 "Min" = fc_fmt(min(d), 3), "Max" = fc_fmt(max(d), 3),
                 "Trend line" = paste0(fc_fmt(tr[["a"]], 3), " + ", fc_fmt(tr[["b"]], 4), " t"),
                 check.names = FALSE)
    })
    output$data_table <- renderTable({
      req(input$file)
      data.frame("Period" = seq_along(series()), "Value" = series(), check.names = FALSE)
    })

    finish <- function(r, d) {
      r$d <- d
      r$errors <- fc_errors(d, r$fitted, set()$start)
      r
    }

    # --- Moving average ------------------------------------------------------
    res_ma <- reactive({
      d <- series(); n <- length(d); s <- set()
      srch <- NULL
      if (input$ma_mode == "search") {
        Nmax <- min(fc_int(input$ma_Nmax, 6), n - 1)
        validate(need(Nmax >= 1, "Not enough data for a moving average."))
        ev <- max(s$start, Nmax + 1)
        sc <- sapply(seq_len(Nmax), function(N) fc_score(d, fc_ma_fit(d, N), ev, s$crit))
        N <- which.min(sc)
        srch <- data.frame("N" = seq_len(Nmax), "V" = fc_fmt(sc, 3), check.names = FALSE)
        names(srch)[2] <- paste0(s$crit, " (periods ", ev, "-", n, ")")
      } else {
        N <- fc_int(input$ma_N, 3)
        validate(need(N >= 1 && N < n, "N must be between 1 and (number of observations - 1)."))
      }
      r <- fc_ma(d, N, s$h)
      if (!is.null(srch)) {
        r$search <- srch
        r$search_title <- paste0("Search result: best N = ", N, " (lowest ", s$crit,
                                 "; all N compared on the same periods)")
      }
      finish(r, d)
    })
    fc_outputs(output, "ma", res_ma, "Moving Average")

    # --- Weighted moving average --------------------------------------------
    res_wma <- reactive({
      d <- series(); n <- length(d); s <- set()
      if (input$wma_mode == "search") {
        N <- fc_int(input$wma_N, 3)
        validate(need(N >= 1 && N <= 5, "N must be between 1 and 5."),
                 need(N < n, "N must be smaller than the number of observations."))
        sr <- fc_wma_search(d, N, as.numeric(input$wma_step), s$start, s$crit)
        r <- fc_wma(d, sr$best, s$h)
        names(sr$top)[3] <- s$crit
        r$search <- sr$top
        r$search_title <- paste0("Search result: best ", N, " weights by ", s$crit,
                                 " (most recent first)")
      } else {
        w <- suppressWarnings(as.numeric(strsplit(trimws(input$wma_w), "[,; ]+")[[1]]))
        w <- w[!is.na(w)]
        validate(need(length(w) >= 1 && all(w >= 0) && sum(w) > 0,
                      "Enter non-negative weights, e.g. 3, 2, 1"),
                 need(length(w) < n, "There are more weights than the data allows."))
        r <- fc_wma(d, w, s$h)
      }
      finish(r, d)
    })
    fc_outputs(output, "wma", res_wma, "Weighted Moving Average")

    # --- Simple exponential smoothing -------------------------------------
    ses_S0 <- reactive({
      d <- series()
      if (input$ses_init == "manual") {
        validate(need(!is.na(input$ses_S0), "Enter the initial level S0."))
        list(S0 = input$ses_S0, txt = "entered manually")
      } else {
        k <- min(length(d), max(2, fc_int(input$ses_k, 4)))
        ab <- fc_line(d[1:k], seq_len(k))
        list(S0 = ab[["a"]], txt = paste0("intercept of the regression line on periods 1-", k,
                                          ": ", fc_fmt(ab[["a"]], 3), " + ",
                                          fc_fmt(ab[["b"]], 3), " t"))
      }
    })
    res_ses <- reactive({
      d <- series(); n <- length(d); s <- set(); ini <- ses_S0()
      srch <- NULL
      if (input$ses_mode == "search") {
        al <- seq(0.01, 1, by = 0.01)
        sc <- sapply(al, function(a) fc_score(d, fc_ses_fit(d, a, ini$S0)[1:n], s$start, s$crit))
        o <- order(sc)[1:5]
        a <- al[o[1]]
        srch <- data.frame("alpha" = fc_fmt(al[o], 2), "V" = fc_fmt(sc[o], 3), check.names = FALSE)
        names(srch)[2] <- s$crit
      } else a <- input$ses_a
      r <- fc_ses(d, a, ini$S0, s$h, ini$txt)
      if (!is.null(srch)) {
        r$search <- srch
        r$search_title <- paste0("Search result: alpha = 0.01 ... 1 tried, best 5 by ", s$crit)
      }
      finish(r, d)
    })
    fc_outputs(output, "ses", res_ses, "Simple Exponential Smoothing")

    # --- Quick and dirty seasonal factors -----------------------------------
    res_qd <- reactive({
      d <- series(); s <- set()
      validate(need(length(d) >= 2 * s$N,
                    paste0("At least 2 full seasons (", 2 * s$N, " values) are needed for N = ", s$N, ".")))
      finish(fc_qd(d, s$N, s$h), d)
    })
    fc_outputs(output, "qd", res_qd, "Seasonal Factors (Quick and Dirty)")

    # --- Holt -------------------------------------------------------------------
    holt_ini <- reactive({
      d <- series()
      if (input$holt_init == "manual") {
        validate(need(!is.na(input$holt_S0) && !is.na(input$holt_G0), "Enter S0 and G0."))
        list(S0 = input$holt_S0, G0 = input$holt_G0, txt = "entered manually")
      } else {
        k <- min(length(d), max(2, fc_int(input$holt_k, 4)))
        ab <- fc_line(d[1:k], seq_len(k))
        list(S0 = ab[["a"]], G0 = ab[["b"]],
             txt = paste0("regression line on periods 1-", k, ": intercept = S0, slope = G0"))
      }
    })
    res_holt <- reactive({
      d <- series(); s <- set(); ini <- holt_ini()
      srch <- NULL
      if (input$holt_mode == "search") {
        sr <- fc_grid_search(function(p) fc_score(d, fc_holt_fit(d, p[1], p[2], ini$S0, ini$G0)$f,
                                                  s$start, s$crit), 2)
        a <- unname(sr$best[1]); b <- unname(sr$best[2])
        srch <- data.frame("alpha" = fc_fmt(sr$top[, 1], 2), "beta" = fc_fmt(sr$top[, 2], 2),
                           "V" = fc_fmt(sr$top[, 3], 3), check.names = FALSE)
        names(srch)[3] <- s$crit
      } else { a <- input$holt_a; b <- input$holt_b }
      r <- fc_holt(d, a, b, ini$S0, ini$G0, s$h, ini$txt)
      if (!is.null(srch)) {
        r$search <- srch
        r$search_title <- paste0("Search result: grid 0.05 then 0.01 around the best point, best 5 by ", s$crit)
      }
      finish(r, d)
    })
    fc_outputs(output, "holt", res_holt, "Holt's Method")

    # --- Seasonal decomposition ---------------------------------------------
    res_dec <- reactive({
      d <- series(); s <- set()
      validate(need(length(d) >= 2 * s$N,
                    paste0("At least 2 full seasons (", 2 * s$N, " values) are needed for N = ", s$N, ".")))
      finish(fc_decomp(d, s$N, s$h, isTRUE(input$dec_fill), input$dec_trend), d)
    })
    fc_outputs(output, "dec", res_dec, "Seasonal Decomposition")

    # --- Winters ---------------------------------------------------------------
    res_win <- reactive({
      d <- series(); s <- set(); N <- s$N
      validate(need(length(d) > 2 * N,
                    paste0("Winters' method needs more than 2 full seasons: at least ",
                           2 * N + 1, " values for N = ", N, ".")))
      if (input$win_init == "manual") {
        validate(need(!is.na(input$win_S0) && !is.na(input$win_G0), "Enter S0 and G0."))
        ini <- fc_winters_init(d, N, "manual", input$win_S0, input$win_G0)
        txt <- "entered manually"
      } else {
        ini <- fc_winters_init(d, N, input$win_init)
        txt <- if (input$win_init == "nahmias") "Nahmias two-season method"
               else paste0("regression line on periods 1-", 2 * N)
      }
      validate(need(all(is.finite(ini$c0)) && all(ini$c0 > 0),
                    "The initial line gives invalid seasonal factors; try another initialization."))
      srch <- NULL
      if (input$win_mode == "search") {
        sr <- fc_grid_search(function(p) fc_score(d, fc_winters_fit(d, N, p[1], p[2], p[3],
                                                                    ini$S0, ini$G0, ini$c0)$f,
                                                  s$start, s$crit), 3)
        a <- unname(sr$best[1]); b <- unname(sr$best[2]); g <- unname(sr$best[3])
        srch <- data.frame("alpha" = fc_fmt(sr$top[, 1], 2), "beta" = fc_fmt(sr$top[, 2], 2),
                           "gamma" = fc_fmt(sr$top[, 3], 2), "V" = fc_fmt(sr$top[, 4], 3),
                           check.names = FALSE)
        names(srch)[4] <- s$crit
      } else { a <- input$win_a; b <- input$win_b; g <- input$win_g }
      r <- fc_winters(d, N, a, b, g, ini, s$h, txt)
      if (!is.null(srch)) {
        r$search <- srch
        r$search_title <- paste0("Search result: grid 0.05 then 0.01 around the best point, best 5 by ", s$crit)
      }
      finish(r, d)
    })
    fc_outputs(output, "win", res_win, "Winters' Method")

    # --- Box-Jenkins ---------------------------------------------------------
    bj_page <- function(key, type, label) {
      i <- function(x) input[[paste0(key, "_", x)]]
      res <- reactive({
        d <- series(); n <- length(d); s <- set()
        dd <- 0; d_note <- NULL
        if (type == "arima") {
          if (i("dmode") == "auto") {
            cd <- fc_choose_d(d)
            dd <- cd$d
            d_note <- paste0("Differencing chosen automatically: std. deviation for d = 0, 1, 2 is ",
                             paste(fc_fmt(cd$sd, 3), collapse = ", "), " -> d = ", dd, ".")
          } else dd <- min(2, max(0, fc_int(i("d"), 1)))
        }
        x_used <- if (dd > 0) diff(d, differences = dd) else d
        validate(need(sd(x_used) > 0,
                      paste0("The series", if (dd > 0) paste0(" differenced ", dd, " time(s)") else "",
                             " is constant, so no Box-Jenkins model can be fitted.")))
        srch <- NULL
        if (i("mode") == "search") {
          pmax <- if (type %in% c("ar", "arma", "arima")) min(6, max(1, fc_int(i("pmax"), 3))) else 0
          qmax <- if (type %in% c("ma", "arma", "arima")) min(6, max(1, fc_int(i("qmax"), 3))) else 0
          lo <- if (type == "arima") 0 else 1
          ps <- if (type == "ma") 0 else lo:pmax
          qs <- if (type == "ar") 0 else lo:qmax
          tab <- withProgress(message = "Fitting models...", fc_bj_search(d, ps, dd, qs, i("ic")))
          validate(need(!is.null(tab), "No model could be fitted to this series."))
          p <- tab$p[1]; q <- tab$q[1]
          srch <- data.frame("Model" = paste0("ARIMA(", tab$p, ",", tab$d, ",", tab$q, ")"),
                             "AIC" = fc_fmt(tab$AIC, 3), "BIC" = fc_fmt(tab$BIC, 3),
                             check.names = FALSE)[1:min(8, nrow(tab)), ]
        } else {
          p <- if (type == "ma") 0 else max(0, fc_int(i("p"), 1))
          q <- if (type == "ar") 0 else max(0, fc_int(i("q"), 1))
          if (type != "arima") {
            validate(need(type == "ma" || p >= 1, "p must be at least 1."),
                     need(type == "ar" || q >= 1, "q must be at least 1."))
          }
        }
        validate(need(n - dd > p + q + 2,
                      "The series is too short for this model order."))
        r <- fc_bj(d, p, dd, q, s$h, label)
        validate(need(!is.null(r), "The model could not be estimated. Try a smaller order."))
        r$note <- c(r$note, d_note)
        if (!is.null(srch)) {
          r$search <- srch
          r$search_title <- paste0("Search result: models ranked by ", i("ic"), " (best first)")
        }
        finish(r, d)
      })
      fc_outputs(output, key, res, label)
      output[[paste0(key, "_fitstats")]] <- renderTable(tryCatch(res(), error = function(e) NULL)$fit_tab)
      output[[paste0(key, "_coef")]] <- renderTable(tryCatch(res(), error = function(e) NULL)$coef_tab)
      output[[paste0(key, "_acf")]] <- renderPlot({
        r <- tryCatch(res(), error = function(e) NULL)
        req(r)
        dd <- max(1, r$order[2])
        x <- switch(i("acfsrc"),
                    series = r$d,
                    diff = diff(r$d, differences = dd),
                    resid = r$resid)
        ttl <- switch(i("acfsrc"),
                      series = "series",
                      diff = paste0("series differenced ", dd, " time(s)"),
                      resid = paste0("residuals of ARIMA(", paste(r$order, collapse = ","), ")"))
        lm_ <- max(1, min(20, length(x) - 2))
        par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
        acf(x, lag.max = lm_, main = paste("ACF of", ttl), na.action = na.pass)
        pacf(x, lag.max = lm_, main = paste("PACF of", ttl), na.action = na.pass)
      })
    }
    bj_page("ar",    "ar",    "AR(p) model")
    bj_page("bjma",  "ma",    "MA(q) model")
    bj_page("arma",  "arma",  "ARMA(p,q) model")
    bj_page("arima", "arima", "ARIMA(p,d,q) model")
  })
}
