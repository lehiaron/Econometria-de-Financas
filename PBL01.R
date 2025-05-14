# Pacotes necessários
#install.packages(c("quantmod", "readxl", "moments", "ggplot2", "xts", "tseries"))
library(quantmod)
library(readxl)
library(moments)
library(ggplot2)
library(xts) 
library(tseries)

# 1. Leitura do arquivo Excel (pulando as linhas iniciais com cabeçalhos desnecessários)
dados_raw <- read_excel("C:\\Users\\lhaon\\Downloads\\PBL01.xlsx", skip = 3)

# 2. Renomear a coluna que contém os preços para "MGLU3"
colnames(dados_raw)[grepl("MGLU3", colnames(dados_raw))] <- "MGLU3"

# 3. Garantir que os dados estejam no tipo correto
dados_raw$MGLU3 <- as.numeric(dados_raw$MGLU3)
dados_raw$Data <- as.Date(dados_raw$Data)

# 4. Criar objeto xts
dados_xts <- xts(dados_raw$MGLU3, order.by = dados_raw$Data)
colnames(dados_xts) <- "Preco"
dados_xts <- na.omit(dados_xts)

# 5. Calcular log-retornos com quantmod
retornos <- dailyReturn(dados_xts, type = "log")
colnames(retornos) <- "LogRetorno"

# 6. Histograma com curva normal
media <- mean(retornos$LogRetorno, na.rm = TRUE)
desvio <- sd(retornos$LogRetorno, na.rm = TRUE)

hist(retornos$LogRetorno,
     breaks = 30,
     probability = TRUE,
     col = "lightblue",
     main = "Histograma dos Log-Retornos de MGLU3",
     xlab = "Log-Retorno")

curve(dnorm(x, mean = media, sd = desvio), 
      col = "red", lwd = 2, add = TRUE)

# 7. Calcular os 4 primeiros momentos
momento1 <- mean(retornos$LogRetorno, na.rm = TRUE)            # Média
momento2 <- var(retornos$LogRetorno, na.rm = TRUE)             # Variância
momento3 <- skewness(retornos$LogRetorno, na.rm = TRUE)        # Assimetria
momento4 <- kurtosis(retornos$LogRetorno, na.rm = TRUE)        # Curtose

cat("Momentos dos log-retornos:\n")
cat("1º Momento (Média):", momento1, "\n")
cat("2º Momento (Variância):", momento2, "\n")
cat("3º Momento (Assimetria):", momento3, "\n")
cat("4º Momento (Curtose):", momento4, "\n")

# Teste de Normalidade
jarque.bera.test(retornos$LogRetorno)

# 8. Plot da evolução do preço
chartSeries(dados_xts,
            name = "Evolução do Preço - MGLU3",
            theme = chartTheme("black"))

# 9. Plot da evolução dos retornos
chartSeries(retornos,
            name = "Evolução dos Log-Retornos - MGLU3",
            theme = chartTheme("black"),
            type = "line")
