# Pacotes necessários
# install.packages(c("quantmod", "dplyr", "lubridate", "tidyr", "readxl", "ggplot2"))

library(quantmod)
library(xts)
library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
library(ggplot2)

# Diretório
setwd("C:\\Users\\lhaon\\Desktop\\FGV\\Econo Fin")

# Ler dados das planilhas
precos_ajust      <- read_excel("dados_acoes_formatado.xlsx", sheet = "Fechamento_Ajustado")
precos_nao_ajust  <- read_excel("dados_acoes_formatado.xlsx", sheet = "Fechamento_Nao_Ajustado")
qtds              <- read_excel("dados_acoes_formatado.xlsx", sheet = "Quantidade_Acoes")
volumes           <- read_excel("dados_acoes_formatado.xlsx", sheet = "Volume_Transacoes")

# Converter datas
precos_ajust$Data      <- as.Date(precos_ajust$Data)
precos_nao_ajust$Data  <- as.Date(precos_nao_ajust$Data)
qtds$Data              <- as.Date(qtds$Data)
volumes$Data           <- as.Date(volumes$Data)

# Garantir que todas as colunas (exceto Data) sejam numéricas
precos_ajust <- precos_ajust %>%
  mutate(across(-Data, ~ as.numeric(.)))

precos_nao_ajust <- precos_nao_ajust %>%
  mutate(across(-Data, ~ as.numeric(.)))

qtds <- qtds %>%
  mutate(across(-Data, ~ as.numeric(.)))

volumes <- volumes %>%
  mutate(across(-Data, ~ as.numeric(.)))

# FILTRAR LIQUIDEZ: ações com volume positivo em pelo menos 90% dos dias
ativos_liquidos <- volumes %>%
  select(-Data) %>%
  summarise_all(~ mean(!is.na(.) & . > 0)) %>%
  pivot_longer(cols = everything(), names_to = "ticker", values_to = "proporcao_negociada") %>%
  filter(proporcao_negociada >= 0.9) %>%
  pull(ticker)

# Filtrar apenas ativos líquidos
precos_ajust     <- precos_ajust     %>% select(Data, all_of(ativos_liquidos))
precos_nao_ajust <- precos_nao_ajust %>% select(Data, all_of(ativos_liquidos))
qtds             <- qtds             %>% select(Data, all_of(ativos_liquidos))
volumes          <- volumes          %>% select(Data, all_of(ativos_liquidos))

# VALOR DE MERCADO com preço NÃO AJUSTADO
valor_mercado <- precos_nao_ajust
for (ticker in ativos_liquidos) {
  valor_mercado[[ticker]] <- precos_nao_ajust[[ticker]] * qtds[[ticker]]
}

# Valor de mercado no primeiro dia de cada mês
valor_mercado_mensal <- valor_mercado %>%
  mutate(Month = floor_date(Data, "month")) %>%
  group_by(Month) %>%
  slice(1) %>%
  ungroup()

# Retornos com preço AJUSTADO
retornos <- precos_ajust %>%
  arrange(Data) %>%
  mutate(across(-Data, ~ ROC(.x, type = "discrete"))) %>%
  drop_na()

# Sequência de meses
datas_mensais <- valor_mercado_mensal$Month

# Transformar retornos em xts
retornos_xts <- xts(retornos[,-1], order.by = retornos$Data)

# Inicializar lista para guardar retornos mensais
lista_retornos <- list()

# Loop mensal
for (i in 1:(length(datas_mensais) - 1)) {
  data_inicio <- datas_mensais[i]
  data_fim    <- datas_mensais[i + 1] - 1
  
  # Selecionar valor de mercado no início do mês
  vm_row <- valor_mercado_mensal %>%
    filter(Month == data_inicio) %>%
    select(-Data, -Month)
  
  size_df <- data.frame(
    Ticker = names(vm_row),
    ValorMercado = as.numeric(vm_row[1, ])
  ) %>% arrange(ValorMercado)
  
  # Selecionar os extremos (quartil inferior e superior)
  n <- nrow(size_df)
  long  <- size_df$Ticker[1:floor(n/4)]
  short <- size_df$Ticker[(n - floor(n/4) + 1):n]
  
  # Selecionar retornos no período
  retornos_mes <- retornos_xts[paste0(data_inicio + 1, "/", data_fim)]
  
  # Calcular retorno médio long, short e long-short
  retorno_long  <- rowMeans(retornos_mes[, long], na.rm = TRUE)
  retorno_short <- rowMeans(retornos_mes[, short], na.rm = TRUE)
  retorno_ls    <- retorno_long - retorno_short
  
  # Armazenar na lista com as datas corretas
  lista_retornos[[i]] <- xts(retorno_ls, order.by = time(retornos_mes))
}


# Empilhar todos os retornos em uma série única
retorno_ls_xts <- do.call(rbind, lista_retornos)

# Calcular retorno acumulado
retorno_acumulado <- cumprod(1 + na.omit(retorno_ls_xts)) - 1

# Preparar para o ggplot
retorno_df <- data.frame(
  Data = index(retorno_acumulado),
  retorno_acumulado = as.numeric(retorno_acumulado)
)

# Plotar
ggplot(retorno_df, aes(x = Data, y = retorno_acumulado)) +
  geom_line(color = "blue") +
  labs(title = "Retorno acumulado - Portfólio Long-Short (Size Factor)",
       subtitle = "Long: empresas pequenas | Short: empresas grandes",
       x = "Data", y = "Retorno acumulado") +
  theme_minimal()
