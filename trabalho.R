# Baixando bibliotecas necessárias
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("moments", quietly = TRUE)) install.packages("moments")
if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats")
if (!requireNamespace("rlang", quietly = TRUE)) install.packages("rlang")
if (!requireNamespace("rugarch", quietly = TRUE)) install.packages("rugarch")
if (!requireNamespace("forecast", quietly = TRUE)) install.packages("forecast")
library(quantmod)
library(ggplot2)
library(dplyr)
library(moments)
library(stats)
library(rlang)
library(rugarch)
library(forecast)
library(MASS)

start_date <- "1900-01-01"
end_date <- "2024-01-01"

# Importanto as bases utilizadas
BTC.USD <- getSymbols("BTC-USD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
BRL.X <- getSymbols("BRL=X", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
PETR4.SA <- getSymbols("PETR4.SA", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
SAN.MC <- getSymbols("SAN.MC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
VOW3.DE <- getSymbols("VOW3.DE", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
BVSP <- getSymbols("^BVSP", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
TSCO.L <- getSymbols("TSCO.L", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
MSFT <- getSymbols("MSFT", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
AAPL <- getSymbols("AAPL", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
GSPC <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)

df1 <- na.omit(data.frame(Date = index(BTC.USD), coredata(BTC.USD)))
df2 <- na.omit(data.frame(Date = index(BRL.X), coredata(BRL.X)))
df3 <- na.omit(data.frame(Date = index(PETR4.SA), coredata(PETR4.SA)))
df4 <- na.omit(data.frame(Date = index(SAN.MC), coredata(SAN.MC)))
df5 <- na.omit(data.frame(Date = index(VOW3.DE), coredata(VOW3.DE)))
df6 <- na.omit(data.frame(Date = index(BVSP), coredata(BVSP)))
df7 <- na.omit(data.frame(Date = index(TSCO.L), coredata(TSCO.L)))
df8 <- na.omit(data.frame(Date = index(MSFT), coredata(MSFT)))
df9 <- na.omit(data.frame(Date = index(AAPL), coredata(AAPL)))
df10 <- na.omit(data.frame(Date = index(GSPC), coredata(GSPC)))

# Verificando tamanho dos dataframes
dataframes <- list("BTC.USD", "BRL.X", "PETR4.SA", "SAN.MC", 
                   "VOW3.DE", "BVSP", "TSCO.L", "MSFT", 
                   "AAPL", "GSPC")

for (i in seq_along(dataframes)) {
  df_name <- dataframes[[i]]
  df <- get(df_name)
  cat(sprintf("Tamanho do %s: %d linhas\n", df_name, nrow(df)))
}

# Função para exploração dos dados
parte1 <- function(df, txt) {
  # Cria o nome das variáveis dinamicamente
  adjusted_col <- sym(paste0(txt, ".Adjusted"))
  
  df <- df %>%
    mutate(
      Retorno_Bruto = (!!adjusted_col / lag(!!adjusted_col)) - 1,
      Log_Retorno = log(!!adjusted_col / lag(!!adjusted_col))
    ) %>%
    slice(-1)
  
  head(df)
  
  ## Plotando os gráficos
  par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
  
  ### Gráfico 1: Preço Ajustado
  plot(df$Date, df[[as.character(adjusted_col)]], type = "l", col = "blue",
       main = "Preço Ajustado (Pt)", xlab = "Data", ylab = "Preço")
  
  ### Gráfico 2: Retorno Bruto
  plot(df$Date, df$Retorno_Bruto, type = "l", col = "orange",
       main = "Retorno Bruto (Rt)", xlab = "Data", ylab = "Retorno")
  
  ### Gráfico 3: Log-Retorno
  plot(df$Date, df$Log_Retorno, type = "l", col = "green",
       main = "Log-Retorno (rt)", xlab = "Data", ylab = "Log-Retorno")
  
  mtext(paste("Análise de Retornos para", txt), outer = TRUE, cex = 1.5)
  
  layout(1:2)
  acf(df$Log_Retorno, lag.max = 30, main ='ACF')
  pacf(df$Log_Retorno, lag.max = 30, main ='PACF')
  
  mtext(paste("Autocorrelação para", txt), outer = TRUE, cex = 1.5)
  
  ## Calcular Assimetria (A) e Curtose (K)
  A <- skewness(df$Log_Retorno, na.rm = TRUE)
  K <- kurtosis(df$Log_Retorno, na.rm = TRUE)
  
  T <- length(na.omit(df$Log_Retorno)) # Tamanho da amostra
  
  ## Testes para normalidade
  
  ### Teste para Assimetria (A = 0)
  SE_A <- sqrt(6 / T)
  z_A <- A / SE_A
  p_value_A <- 2 * (1 - pnorm(abs(z_A)))
  
  ### Teste para Curtose (K = 3)
  SE_K <- sqrt(24 / T)
  z_K <- (K - 3) / SE_K
  p_value_K <- 2 * (1 - pnorm(abs(z_K)))
  
  cat(paste(txt, "= \n"))
  ### Resultados
  cat("Assimetria:", A, ", p-valor para A = 0:", p_value_A, "\n")
  cat("Curtose:", K, ", p-valor para K = 3:", p_value_K, "\n")
  
  alpha <- 0.05
  if (p_value_A < alpha) {
    cat("Rejeitamos H0 para a assimetria (A ≠ 0).\n")
  } else {
    cat("Não rejeitamos H0 para a assimetria (A = 0).\n")
  }
  
  if (p_value_K < alpha) {
    cat("Rejeitamos H0 para a curtose (K ≠ 3).\n")
  } else {
    cat("Não rejeitamos H0 para a curtose (K = 3).\n")
  }

  return(df)

}

# Função para ajuste do modelo ARMA-GARCH
parte2 <- function(df, pv = c(1,2,3), qv = c(1,2,3), ao) {
  results <- list() 
  
  for (p in pv) {
    for (q in pv) {
      # Definir a especificação do modelo GARCH(p, q)
      spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
                         mean.model = list(armaOrder = c(ao[1], ao[2]), include.mean = TRUE),
                         distribution.model = "norm")
      
      # Ajustar o modelo GARCH
      fit <- tryCatch({
        ugarchfit(spec, df$Log_Retorno)
      }, error = function(e) {
        NULL  # Em caso de erro, retorna NULL
      })
      
      # Se o modelo for ajustado com sucesso, armazenar o AIC
      if (!is.null(fit)) {
        loglik_value <- likelihood(fit)  # Obtém a log-verossimilhança do modelo ajustado
        results[[paste0("p_", p, "_q_", q)]] <- list(fit = fit, loglik = loglik_value)
      }
    }
  }
  
  # Retornar o modelo com o menor AIC
  best_model <- results[[which.min(sapply(results, function(x) x$loglik))]]
  
  return(best_model)
}

# Função para cálculo dos Valores em Risco
parte3 <- function(df, fit) {
  
  forecast_garch <- ugarchforecast(fit$fit, n.ahead = 10)
  
  forecast_volatility <- forecast_garch@forecast$seriesFor
  
  VaR_1_percent <- quantile(forecast_volatility, 0.01)
  
  list(Log_Retorno = df$Log_Retorno, 
       forecast_volatility = forecast_volatility, 
       VaR_1_percent = VaR_1_percent)
}

for (i in 1:10) {
  df_name <- paste0("df", i)
  df <- get(df_name)
  updated_df <- parte1(df, dataframes[i])
  assign(df_name, updated_df)
}

# Calcula os melhores parâmetros de ARMA para as bases
fit.ARIMA.Log_Retorno <- auto.arima(df1$Log_Retorno,trace=TRUE, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
fit.ARIMA.Log_Retorno <- auto.arima(df1$Log_Retorno,trace=TRUE, max.p = 1, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
armaOrder1 <- c(0,0)

fit.ARIMA.Log_Retorno <- auto.arima(df2$Log_Retorno,trace=TRUE, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
armaOrder2 <- c(0,1)

fit.ARIMA.Log_Retorno <- auto.arima(df3$Log_Retorno,trace=TRUE, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
armaOrder3 <- c(0,0)

fit.ARIMA.Log_Retorno <- auto.arima(df4$Log_Retorno,trace=TRUE, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
armaOrder4 <- c(2,2)

fit.ARIMA.Log_Retorno <- auto.arima(df5$Log_Retorno,trace=TRUE, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
armaOrder5 <- c(4,0)

fit.ARIMA.Log_Retorno <- auto.arima(df6$Log_Retorno,trace=TRUE, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
armaOrder6 <- c(0,1)

fit.ARIMA.Log_Retorno <- auto.arima(df7$Log_Retorno,trace=TRUE, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
fit.ARIMA.Log_Retorno <- auto.arima(df7$Log_Retorno,trace=TRUE, max.p = 2, max.q = 2, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
armaOrder7 <- c(1,2)

fit.ARIMA.Log_Retorno <- auto.arima(df8$Log_Retorno,trace=TRUE, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
armaOrder8 <- c(4,0)

fit.ARIMA.Log_Retorno <- auto.arima(df9$Log_Retorno,trace=TRUE, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
fit.ARIMA.Log_Retorno <- auto.arima(df9$Log_Retorno,trace=TRUE, max.p = 3, max.q = 3, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
fit.ARIMA.Log_Retorno <- auto.arima(df9$Log_Retorno,trace=TRUE, max.p = 2, max.q = 1, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
armaOrder9 <- c(2,0)

fit.ARIMA.Log_Retorno <- auto.arima(df10$Log_Retorno,trace=TRUE, max.d = 0)
summary(fit.ARIMA.Log_Retorno)
armaOrder10 <- c(0,2)

# Calcula os melhores parâmetros de GARCH para as bases
fit1 <- parte2(df1, ao = armaOrder1)
fit1
fit1 <- parte2(df1, pv = c(1), ao = armaOrder1)
fit1
garchOrder1 <- c(1,1)
fit2 <- parte2(df2, ao = armaOrder2)
fit2
fit2 <- parte2(df2, pv = c(1), ao = armaOrder2)
fit2
garchOrder2 <- c(1,1)
fit3 <- parte2(df3, ao = armaOrder3)
fit3
garchOrder3 <- c(1,1)
fit4 <- parte2(df4, ao = armaOrder4)
fit4
garchOrder4 <- c(1,1)
fit5 <- parte2(df5, ao = armaOrder5)
fit5
fit5 <- parte2(df5, pv = c(1), ao = armaOrder5)
fit5
garchOrder5 <- c(1,1)
fit6 <- parte2(df6, ao = armaOrder6)
fit6
garchOrder6 <- c(1,1)
fit7 <- parte2(df7, pv = c(1,2), ao = armaOrder7)
fit7
fit7 <- parte2(df7, pv = c(1), qv = c(1), ao = armaOrder7)
fit7
garchOrder7 <- c(1,1)
fit8 <- parte2(df8, ao = armaOrder8)
fit8
fit8 <- parte2(df8, pv = c(1), ao = armaOrder8)
fit8
garchOrder8 <- c(1,1)
fit9 <- parte2(df9, , ao = armaOrder9)
fit9
fit9 <- parte2(df9, pv = c(1,2), ao = armaOrder9)
fit9
garchOrder9 <- c(1,1)
fit10 <- parte2(df10, pv = c(1), qv = c(1,2), ao = armaOrder10)
fit10
garchOrder10 <- c(1,1)

for (i in 1:10) {
  df_name <- paste0("df", i)
  df <- get(df_name)
  fit_name <- paste0("fit", i)
  fit <- get(fit_name)
  
  result <- parte3(df, fit)
  print(paste("VaR a 1% para os próximos 10 dias (df", i, "):"))
  print(paste(round(result$VaR_1_percent * 100, 3), "%", sep = ""))
}