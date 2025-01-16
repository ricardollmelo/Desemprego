# Instalar pacotes necessários 

library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(caret)  

# Carregar os dados do arquivo Excel

data <- df_final

# Converter a coluna 'Date' para o formato de data
data$Date <- as.Date(paste0(data$Date, "-01"), format = "%Y-%m-%d")

# Calcular as diferenças de primeira ordem 
data <- data %>%
  mutate(
    delta_seguro_desemprego = c(NA, diff(seguro_desemprego)),
    delta_decimo_terceiro = c(NA, diff(decimo_terceiro)),
    delta_fgts = c(NA, diff(fgts)),
    delta_inss = c(NA, diff(inss)),
    delta_empregos = c(NA, diff(empregos)),
    delta_seguridade_social = c(NA, diff(seguridade_social)),
    delta_vagas_de_emprego = c(NA, diff(vagas_de_emprego)),
    delta_taxa_desemprego = c(NA, diff(taxa_desemprego))
  )

# Remover a primeira linha com valores NA nas diferenças
data <- na.omit(data)

# Dividir os dados em treinamento (80%) e teste (20%)
set.seed(123) 
trainIndex <- createDataPartition(data$delta_taxa_desemprego, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Ajustando o modelo 
modelo_mixed <- lm(delta_taxa_desemprego ~ seguro_desemprego + delta_seguro_desemprego + 
                     decimo_terceiro + delta_decimo_terceiro + 
                     fgts + delta_fgts + 
                     inss + delta_inss + 
                     empregos + delta_empregos + 
                     seguridade_social + delta_seguridade_social + 
                     vagas_de_emprego + delta_vagas_de_emprego, data = train_data)

# Resumo do modelo
summary(modelo_mixed)

# Fazer previsões para o conjunto de teste
test_data$predicted_delta_unemployment <- predict(modelo_mixed, newdata = test_data)

# Calcular a taxa de desemprego prevista (somando a mudança prevista à taxa anterior)
test_data$predicted_unemployment <- test_data$taxa_desemprego[1] + cumsum(test_data$predicted_delta_unemployment)

# Comparar as previsões com as diferenças reais
ggplot(test_data, aes(x = Date)) +
  geom_line(aes(y = delta_taxa_desemprego, color = "Diferença Real de Desemprego")) +
  geom_line(aes(y = predicted_delta_unemployment, color = "Diferença Prevista de Desemprego")) +
  labs(title = "Comparação entre Diferença Real e Prevista da Taxa de Desemprego (Fora da Amostra)",
       x = "Data", y = "Diferença da Taxa de Desemprego") +
  theme_minimal()

# Avaliação da performance fora da amostra
rmse <- sqrt(mean((test_data$delta_taxa_desemprego - test_data$predicted_delta_unemployment)^2))
print(paste("RMSE (Fora da amostra):", rmse))

# Previsão do próximo mês fora da amostra (baseada no último ponto do conjunto de teste)
ultimo_mes_teste <- tail(test_data, 1)

# Criar um novo data frame para o próximo mês, usando as mesmas variáveis do último mês do conjunto de teste
proximo_mes_teste <- data.frame(
  seguro_desemprego = ultimo_mes_teste$seguro_desemprego,
  delta_seguro_desemprego = 0,  
  decimo_terceiro = ultimo_mes_teste$decimo_terceiro,
  delta_decimo_terceiro = 0,
  fgts = ultimo_mes_teste$fgts,
  delta_fgts = 0,
  inss = ultimo_mes_teste$inss,
  delta_inss = 0,
  empregos = ultimo_mes_teste$empregos,
  delta_empregos = 0,
  seguridade_social = ultimo_mes_teste$seguridade_social,
  delta_seguridade_social = 0,
  vagas_de_emprego = ultimo_mes_teste$vagas_de_emprego,
  delta_vagas_de_emprego = 0
)

# Fazer a previsão para o próximo mês fora da amostra
previsao_proximo_mes_teste <- predict(modelo_mixed, newdata = proximo_mes_teste)

previsao_taxa_desemprego_proximo_mes_teste <- (ultimo_mes_teste$taxa_desemprego + previsao_proximo_mes_teste) * 100

print(paste("Previsão da taxa de desemprego para o próximo mês (Fora da amostra):", previsao_taxa_desemprego_proximo_mes_teste))

