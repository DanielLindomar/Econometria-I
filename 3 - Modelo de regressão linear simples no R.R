# Modelo de regressão linear simples no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# Tópicos:
#   Regressão simples
#   Previsão após a regressão
#   Medidas de qualidade do ajuste (R-quadrado)
#   Formas funcionais: modelo log-log e outros modelos log-lineares

# Arquivos de dados:
#   Salário por hora2.csv

# Preparando
rm(list = ls()) 
diretório <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Regressão simples ------------------------------------------------------------

# Exemplo da relação entre salário por hora e educação 
salário <- read.csv2(paste0(diretório, "Salário por hora2.csv"))
View(salário)

salário %<>% select(salhora, esc)
str(salário)
stargazer(salário, type = "text")
head(salário, 10)

# Explorando os dados
cor(salário)
salário %<>% mutate(salhora_médio = mean(salhora))

# Regressão simples: salário/hora = b1 + b2*escolaridade + e
modelo_salhora1 <- lm(formula = salhora ~ esc, data = salário)
summary(modelo_salhora1)
modelo_salhora1$coefficients['esc']
# Podemos ainda obter os coeficientes b1 e b2 aplicando diretamente a fórmula de MQO
(b2 <- cov(salário$salhora, salário$esc)/var(salário$esc))
(b1 <- mean(salário$salhora)-b2*mean(salário$esc))

# Gráfico de dispersão com a reta de regressão estimada
plot(x = salário$esc, y = salário$salhora)
abline(a = modelo_salhora1$coefficients['(Intercept)'], 
       b = modelo_salhora1$coefficients['esc'],
       col = 'red')
# ou use  pacote ggplot
ggplot(data = salário, mapping = aes(x = esc, y = salhora)) +
  theme_bw() + # personaliza o fundo do gráfico, bw refere-se ao tema preto e branco
  geom_point() + # refere-se ao gráfico de dispersão
  geom_smooth(method = "lm", se = FALSE) # faz o ajuste da linha de regressão


# Previsão após a regressão ----------------------------------------------------


# Valores previstos (estimados) para a variável dependente salário/hora (salhora_est)
salário %<>% mutate(salhora_est = fitted(modelo_salhora1))
stargazer(salário, type = "text")
ggplot(data = salário, mapping = aes(x = esc)) +
  geom_point(mapping = aes(y = salhora, color = 'Salário por hora - valor observado')) +
  geom_point(mapping = aes(y = salhora_est, color = 'Salário por hora - valor estimado')) + 
  xlab('Escolaridade')

# Resíduos
salário %<>% mutate(e_est = residuals(modelo_salhora1))
stargazer(salário, type = "text")
ggplot(salário, aes(x = esc)) +
  geom_point(aes(y = salhora, col = 'Salário por hora - valor observado')) +
  geom_point(aes(y = e_est, col = 'Resíduo estimado')) +
  xlab('Escolaridade')

head(salário, 10)

# Gráfico dos valores observados e previstos e dos resíduos
ggplot(salário, aes(x = esc)) +
  geom_point(aes(y = salhora, color = 'Salário por hora - valor observado')) +
  geom_point(aes(y = salhora_est, color = 'Salário por hora - valor estimado')) +
  geom_point(aes(y = e_est, color = 'Resíduo estimado')) +
  geom_smooth(aes(y = salhora, color = 'Ajuste de linha'), 
              method = "lm", se = FALSE) +
  xlab('Escolaridade')


# Medidas de qualidade do ajuste (R-quadrado) ----------------------------------

# Use a função 'str' para mostrar o que está incluído nos resultados da regressão
str(modelo_salhora1)
str(summary(modelo_salhora1))

# O R-quadrado é armazenado como 'r.squared'
summary(modelo_salhora1)$r.squared

# Use a função 'nobs' para ver o número de observações
nobs(modelo_salhora1)


# Formas funcionais: modelo log-log e outros modelos log-lineares --------------

# Vamos criar as variável log_salhora e log_esc
salário %<>% mutate(log_salhora=log(salhora), log_esc=log(esc))

# Forma funcional linear
modelo_salhora1 <- lm(salhora ~ esc, salário)
summary(modelo_salhora1)
# Gráfico
ggplot(salário, aes(x = esc, y = salhora)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# Forma funcional log-log ou log-linear
modelo_salhora2 <- lm(log_salhora ~ log_esc, salário)
summary(modelo_salhora2)
# Gráfico
ggplot(salário, aes(x = log_esc, y = log_salhora)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Forma funcional lin-log
modelo_salhora3 <- lm(salhora ~ log_esc, salário)
summary(modelo_salhora3)
# Gráfico
ggplot(salário, aes(x = log_esc, y = salhora)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Forma funcional log-lin
modelo_salhora4 <- lm(log_salhora ~ esc, salário)
summary(modelo_salhora4)
# Gráfico
ggplot(salário, aes(x = esc, y = log_salhora)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Regressão que passa pela origem, modelo de intercepto zero
modelo_salhora5 <- lm(salhora ~ 0 + esc, salário)
summary(modelo_salhora5)
# Gráfico
ggplot(salário, aes(x = esc, y = salhora)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ 0 + x, se = F)

# Regressão sobre uma constante
modelo_salhora6 <- lm(salhora ~ 1, salário)
summary(modelo_salhora6)
# Gráfico
ggplot(salário, aes(x = esc, y = salhora)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ 1, se = F)

# Fim do Script ----------------------------------------------------------------