# Heterocedasticidade no R
  
# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# Tópicos:
#   Análise gráfica da heterocedasticidade 
#   Testes de heterocedasticidade:
#     Teste de Breusch-Pagan
#     Teste de White
#   Medidas corretivas:
#     Erros padrão robustos para heterocedasticidade
#     Mínimos Quadrados Ponderado (MQP)
#     Mínimos Quadrados Generalizados Factíveis (MQGF)

# Arquivos de dados: 
#   Salário por hora2.csv

# Preparando
rm(list = ls()) 
diretório <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", "car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Análise gráfica da heterocedasticidade --------------------------------------- 

# Dados de salário por hora
salário <- read.csv2(paste0(diretório, "Salário por hora2.csv"))

salário %>% 
  select(salhora, esc, exp, gênero, região) %>% 
  str

salário %>% 
  select(salhora, esc, exp, gênero, região) %>% 
  stargazer(type = "text")

salário %>% 
  select(salhora, esc, exp, gênero, região) %>%
  head(10)

# Regressão do salário por hora
modelo_0 <- lm(salhora ~ esc + exp + gênero, salário)
summary(modelo_0)
salário %<>% mutate(resid2 = resid(modelo_0)^2)

# Análise do gráfico de resíduos quadráticos contra o Y previsto
salário %<>% mutate(y_est = fitted(modelo_0))
ggplot(data = salário, mapping = aes(x = y_est, y = resid2)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Resíduos quadráticos', x = 'Salário por hora previsto')
#(Caso indentifique-se uma relação positiva ou negativa no gráfico existe evidência de presença de heterocedasticidade)

# A transformação logarítmica comprime a amplitude da variância ajudando com o problema da heterocedasticidade
# Regressão para o logarítmo do salário por hora
modelo_1 <- lm(log(salhora) ~ esc + exp + gênero, salário)
summary(modelo_1)
salário %<>% mutate(resid21 = resid(modelo_1)^2)

# Análise do gráfico de resíduos quadráticos contra o Y previsto
salário %<>% mutate(y_est1 = fitted(modelo_1))
ggplot(data = salário, mapping = aes(x = y_est1, y = resid21)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Resíduos quadráticos', x = 'Salário por hora previsto')
#(Caso indentifique-se uma relação positiva ou negativa no gráfico existe evidência de presença de heterocedasticidade)


# Testes de heterocedasticidade ------------------------------------------------
  
# Os testes de heterocedasticidade consistem na estimação de regressões dos resíduos   
# do modelo de interesse em relação às variáveis independentes (Teste de Breush-Pagan),
# em relação às variáveis independentes seus termos quadráticos e interações (Teste de White),
# ou ainda, em relação aos valores previstos da variável dependente e seu termo quadrático (Teste alternativo de White).


# Teste de heterocedasticidade para a regressão do salário por hora ------------

# O mesmo do modelo_0
# modelo_0 <- lm(salhora ~ esc + exp + gênero, salário)
summary(modelo_0)

# Teste de Breush-Pagan
bptest(modelo_0)
# H0: Modelo é homocedástico (variância do resíduo é constante)
# H1: Modelo é heterocedástico (variância do resíduo não é constante)
# Se o p-value calculado for menor que 0,05 podemos rejeitar H0 e, portanto, o modelo apresenta heterocedasticidade

#Teste alternativo de White
bptest(modelo_0, ~ fitted(modelo_0) + I(fitted(modelo_0)^2))
#H0: Modelo é homocedástico (variância do resíduo é constante)
#H1: Modelo é heterocedástico (variância do resíduo não é constante)
#Se o p-value calculado for menor que 0,05 podemos rejeitar H0 e, portanto, o modelo apresenta heterocedasticidade


# Teste de heterocedasticidade para a regressão do logaritmo do salário por hora

# O mesmo do modelo_1
# modelo_1 <- lm(log(salhora) ~ esc + exp + gênero, salário)
summary(modelo_1)

# Teste de Breush-Pagan
bptest(modelo_1)
# H0: Modelo é homocedástico (variância do resíduo é constante)
# H1: Modelo é heterocedástico (variância do resíduo não é constante)
# Se o p-value calculado for menor que 0,05 podemos rejeitar H0 e, portanto, o modelo apresenta heterocedasticidade

#Teste alternativo de White
bptest(modelo_1, ~ fitted(modelo_0) + I(fitted(modelo_0)^2))
#H0: Modelo é homocedástico (variância do resíduo é constante)
#H1: Modelo é heterocedástico (variância do resíduo não é constante)
#Se o p-value calculado for menor que 0,05 podemos rejeitar H0 e, portanto, o modelo apresenta heterocedasticidade


# Medidas corretivas -----------------------------------------------------------


# Erros padrão robustos para heterocedasticidade -------------------------------

# Regressão para salário por hora
# modelo_0 <- lm(salhora ~ esc + exp + gênero, salário)
summary(modelo_0)

# Regressão para salário por hora com erros padrão robustos
coeftest(modelo_0, vcov. = vcovHC(modelo_0, type = "HC1"))
# Os coeficientes são os mesmos, mas os erros padrão agora estão corrigidos para a heterocedasticidade

# Regressão para o logaritmo do salário por hora
# modelo_1 <- lm(log(salhora) ~ esc + exp + gênero, salário)
summary(modelo_1)

# Regressão para o logaritmo do salário por hora com erros padrão robustos
coeftest(modelo_1, vcov. = vcovHC(modelo_1, type = "HC1"))
# Nesse caso, como os testes não mostram a presença do problema, os erros padrão robustos são desnecessários 


# Mínimos Quadrados Ponderados (MQP) -------------------------------------------
  
# Usamos o MQP quando conhecemos o padrão de heterocedasticidade, no entanto, essa 
# situação é rara. Como  exemplo, vamos supor que o padrão de heterocedasticidade será
# var(e|x)=(sigma^2)*(esc), assim o peso usado no MQP será peso=1/esc.

# MQP: estime o modelo com peso=1/esc 
modelo_MQP <- lm(formula = salhora ~ esc + exp + gênero, 
                data = salário, weights = 1/esc)
summary(modelo_MQP)


# Mínimos Quadrados Generalizados Factíveis (MQGF) -----------------------------

# Quando a forma da heterocedasticidade não é conhecida, o caso mais comum,
# var(e|x) = sigma^2*(salhora = b1 + b2*esc + b3*exp + b4*gênero)
# estime h_est e use o MQP com peso=1/h_est.

# Forma da heterocedasticidade, estime h_est
# modelo_0 <- lm(salhora ~ esc + exp + gênero, salário)
summary(modelo_0)
salário %<>% mutate(e = resid(modelo_0), 
                    g = log(e^2))
modelo_g <- lm(g ~ esc + exp + gênero, salário)
salário %<>% mutate(g_est = fitted(modelo_g),
                    h_est = exp(g_est))

# MQGF: estime o modelo usando MQP com peso=1/h_est
modelo_MQGF <- lm(formula = salhora ~ esc + exp + gênero, 
                 data = salário, 
                 weights = 1/h_est)
summary(modelo_MQGF)

# Fim do Script ----------------------------------------------------------------

