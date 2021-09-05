# Autocorrelação no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# Tópicos:
#   Análise gráfica da autocorrelação 
#   Testes de autocorrelação:
#     Teste de Breusch-Godfrey
#     Teste de Dunbin-Watson
#   Medidas corretivas:
#     Erros padrão robustos para heterocedasticidade e autocorrelação
#     Transformação de primeira diferença
#     Mínimos Quadrados Generalizados Factíveis (MQGF)

# Arquivos de dados: 
#   Consumo.csv

# Preparando
rm(list = ls()) 
diretório <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", "car", "orcutt", "quantmod", "prais")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Análise gráfica da autocorrelação -------------------------------------------- 

# Dados de consumo
consumo <- read.csv2(paste0(diretório, "Consumo.csv"))

consumo %>%
  str

consumo %>% 
  stargazer(type = "text")

consumo %>% 
  head(10)

# Primeiro faremos o ajuste do modelo de regressão para explicar o consumo agregado nos EUA
modelo_0 <- lm(consumo ~ renda + riqueza + juros, consumo)
summary(modelo_0)
consumo %<>% mutate(resid = resid(modelo_0))

# Análise do gráfico do resíduo no tempo
ggplot(data = consumo, mapping = aes(x = year, y = resid)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Resíduos', x = 'Ano')
#(Qualquer padrão observado pode indicar a autocorrelação)

# Análise do gráfico de resíduos atuais com os resíduos defasados
consumo %<>% mutate(resid_def = Lag(resid, k=1))
ggplot(data = consumo, mapping = aes(x = resid_def, y = resid)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Resíduos', x = 'Resíduos defasados')
#(Qualquer padrão observado pode indicar a autocorrelação)


# Testes de autocorrelação -----------------------------------------------------

# Os testes de autocorrelação consistem na estimação de regressões dos resíduos   
# do modelo de interesse em relação aos resíduos defasados como variáveis independentes 
# (Teste de Breush-Godfrey). Podemos, ainda, calcular uma espécie de coeficiente de 
# correlação entre o resíduo e o resíduo defasado (estatística d de Durbin-Watson)


# Teste de autocorrelação para a regressão do consumo agregado -----------------

# O mesmo do modelo_0
# modelo_0 <- lm(consumo ~ renda + riqueza + juros, consumo)
summary(modelo_0)

# Teste de autocorrelação de Breush-Godfrey
bgtest(modelo_0, order = 1)
bgtest(modelo_0, order = 2)
bgtest(modelo_0, order = 3)
# H0: Resíduos do modelo não são autocorrelacionados
# H1: Resíduos do modelo são autocorrelacionados
# Se o p-value calculado for menor que 0,05 podemos rejeitar H0 e, portanto, o modelo apresenta autocorrelação

# Teste de autocorrelação de Durbin-Watson (estatística d)
dwtest(modelo_0)
# Valores da estatística de Durbin-Watson próximos de 2 são indícios de ausência de autocorrelação, ou seja, valores distantes de 2 indicam presença de autocorrelação
# H0: Resíduos do modelo não são autocorrelacionados
# H1: Resíduos do modelo são autocorrelacionados


# Medidas corretivas -----------------------------------------------------------

# Erros padrão robustos para heterocedasticidade e autocorrelação --------------

# Regressão para o consumo agregado
# modelo_0 <- lm(consumo ~ renda + riqueza + juros, consumo)
summary(modelo_0)

# Regressão para o consumo agregado com erros padrão robustos
coeftest(modelo_0, vcovHAC)
# Os coeficientes são os mesmos, mas os erros padrão agora estão corrigidos para a heterocedasticidade e autocorrelação


# Transformação de primeira diferença ------------------------------------------

# A tranformação de primeira diferença retira componentes que causam a autocorrelação
modelo_1 <- lm(diff(consumo) ~ diff(renda) + diff(riqueza) + diff(juros), consumo)
summary(modelo_1)
bgtest(modelo_1)


# Mínimos Quadrados Generalizados Factíveis (MQGF) -----------------------------

# Os estimadores MQGF calculam o grau de autocorrelação e corrigem a regressão

# Regressão para o consumo agregado
# modelo_0 <- lm(consumo ~ renda + riqueza + juros, consumo)
summary(modelo_0)

# Regressão de Prais-Winstein
modelo_prais <- prais_winsten(modelo_0, data=consumo)
summary(modelo_prais)

# Regressão de Cochrane-Orcutt
modelo_cochrane <- cochrane.orcutt(modelo_0)
summary(modelo_cochrane)

# Fim do Script ----------------------------------------------------------------
