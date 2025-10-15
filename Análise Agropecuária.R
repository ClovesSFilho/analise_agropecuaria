# Fase 2 - Cap 7 - Cloves Silva - RM567250
# Guilherme Barbosa Mendes da Silva - RM567995
# José Luiz de Oliveira Junior - RM567432
# Decolando com ciências de dados

# ANÁLISE ESTÁTISTICA DE DADOS DO AGRONEGÓCIO
# Fonte: IBGE - Pesquisa Trimestral do Leite

# ️Instalar e carregar pacotes
if (!require(readr)) install.packages("readr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
library(readr)
library(ggplot2)
library(dplyr)

# Ler o arquivo CSV (ajuste o caminho conforme seu arquivo)
dados <- read_csv2("C:/Users/clove/Downloads/Dados Agro.csv",
                   locale = locale(encoding = "UTF-8"))

# Limpeza e formatação
dados <- dados[, 1:5]

# Renomear colunas para evitar erros com acentuação e espaços
colnames(dados) <- c("Regiao", "UF", "Periodo", "Leite_mil_litros", "Preco_medio")

# Garantir que os textos não tenham espaços extras
dados$Regiao <- trimws(dados$Regiao)
dados$UF <- trimws(dados$UF)



# ANÁLISE EXPLORATÓRIA DA VARIÁVEL
# Preço Médio (R$/litro)

preco <- dados$Preco_medio

# --- Medidas de Tendência Central ---
media <- mean(preco, na.rm = TRUE)
mediana <- median(preco, na.rm = TRUE)
moda <- as.numeric(names(sort(table(preco), decreasing = TRUE)[1]))

cat("Média:", media, "\n")
cat("Mediana:", mediana, "\n")
cat("Moda:", moda, "\n")

# --- Medidas de Dispersão ---
variancia <- var(preco, na.rm = TRUE)
desvio_padrao <- sd(preco, na.rm = TRUE)
amplitude <- max(preco, na.rm = TRUE) - min(preco, na.rm = TRUE)
coef_var <- (desvio_padrao / media) * 100

cat("Variância:", variancia, "\n")
cat("Desvio padrão:", desvio_padrao, "\n")
cat("Amplitude total:", amplitude, "\n")
cat("Coeficiente de variação (%):", coef_var, "\n")

# --- Medidas Separatrizes ---
quartis <- quantile(preco, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
decis <- quantile(preco, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
cat("\nQuartis:\n"); print(quartis)
cat("\nDecis:\n"); print(decis)



# ANÁLISE GRÁFICA DA VARIÁVEL
# Região

# Gráfico de barras - Preço médio do leite cru por Região

# Tirar valores em branco ou NA
dados <- dados %>% filter(!is.na(Regiao) & Regiao != "")

# Calcular média do preço por região
preco_regiao <- dados %>%
  group_by(Regiao) %>%
  summarise(Media_Preco = mean(Preco_medio, na.rm = TRUE))

# Fazer o gráfico
ggplot(preco_regiao, aes(x = Regiao, y = Media_Preco, fill = Regiao)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Media_Preco, 2)), vjust = -0.3) +
  ggtitle("Preço Médio do Leite Cru por Região (R$/litro)") +
  xlab("Região") +
  ylab("Preço Médio (R$/litro)") +
  theme_minimal()

# Gráfico de pizza - Participação das Regiões na Quantidade Total de Leite Cru

# Calcular o total de leite por região
leite_regiao <- dados %>%
  group_by(Regiao) %>%
  summarise(Total_Leite = sum(Leite_mil_litros, na.rm = TRUE))

# Calcular o percentual
leite_regiao <- leite_regiao %>%
  mutate(Percentual = round((Total_Leite / sum(Total_Leite)) * 100, 1))

# Fazer o gráfico de pizza
ggplot(leite_regiao, aes(x = "", y = Percentual, fill = Regiao)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Percentual, "%")), 
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  ggtitle("Participação das Regiões na Produção Total de Leite Cru (%)") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )