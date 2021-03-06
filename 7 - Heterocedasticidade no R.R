# Heterocedasticidade no R
  
# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# T�picos:
#   An�lise gr�fica da heterocedasticidade 
#   Testes de heterocedasticidade:
#     Teste de Breusch-Pagan
#     Teste de White
#   Medidas corretivas:
#     Erros padr�o robustos para heterocedasticidade
#     M�nimos Quadrados Ponderado (MQP)
#     M�nimos Quadrados Generalizados Fact�veis (MQGF)

# Arquivos de dados: 
#   Sal�rio por hora2.csv

# Preparando
rm(list = ls()) 
diret�rio <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", "car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# An�lise gr�fica da heterocedasticidade --------------------------------------- 

# Dados de sal�rio por hora
sal�rio <- read.csv2(paste0(diret�rio, "Sal�rio por hora2.csv"))

sal�rio %>% 
  select(salhora, esc, exp, g�nero, regi�o) %>% 
  str

sal�rio %>% 
  select(salhora, esc, exp, g�nero, regi�o) %>% 
  stargazer(type = "text")

sal�rio %>% 
  select(salhora, esc, exp, g�nero, regi�o) %>%
  head(10)

# Regress�o do sal�rio por hora
modelo_0 <- lm(salhora ~ esc + exp + g�nero, sal�rio)
summary(modelo_0)
sal�rio %<>% mutate(resid2 = resid(modelo_0)^2)

# An�lise do gr�fico de res�duos quadr�ticos contra o Y previsto
sal�rio %<>% mutate(y_est = fitted(modelo_0))
ggplot(data = sal�rio, mapping = aes(x = y_est, y = resid2)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Res�duos quadr�ticos', x = 'Sal�rio por hora previsto')
#(Caso indentifique-se uma rela��o positiva ou negativa no gr�fico existe evid�ncia de presen�a de heterocedasticidade)

# A transforma��o logar�tmica comprime a amplitude da vari�ncia ajudando com o problema da heterocedasticidade
# Regress�o para o logar�tmo do sal�rio por hora
modelo_1 <- lm(log(salhora) ~ esc + exp + g�nero, sal�rio)
summary(modelo_1)
sal�rio %<>% mutate(resid21 = resid(modelo_1)^2)

# An�lise do gr�fico de res�duos quadr�ticos contra o Y previsto
sal�rio %<>% mutate(y_est1 = fitted(modelo_1))
ggplot(data = sal�rio, mapping = aes(x = y_est1, y = resid21)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Res�duos quadr�ticos', x = 'Sal�rio por hora previsto')
#(Caso indentifique-se uma rela��o positiva ou negativa no gr�fico existe evid�ncia de presen�a de heterocedasticidade)


# Testes de heterocedasticidade ------------------------------------------------
  
# Os testes de heterocedasticidade consistem na estima��o de regress�es dos res�duos   
# do modelo de interesse em rela��o �s vari�veis independentes (Teste de Breush-Pagan),
# em rela��o �s vari�veis independentes seus termos quadr�ticos e intera��es (Teste de White),
# ou ainda, em rela��o aos valores previstos da vari�vel dependente e seu termo quadr�tico (Teste alternativo de White).


# Teste de heterocedasticidade para a regress�o do sal�rio por hora ------------

# O mesmo do modelo_0
# modelo_0 <- lm(salhora ~ esc + exp + g�nero, sal�rio)
summary(modelo_0)

# Teste de Breush-Pagan
bptest(modelo_0)
# H0: Modelo � homoced�stico (vari�ncia do res�duo � constante)
# H1: Modelo � heteroced�stico (vari�ncia do res�duo n�o � constante)
# Se o p-value calculado for menor que 0,05 podemos rejeitar H0 e, portanto, o modelo apresenta heterocedasticidade

#Teste alternativo de White
bptest(modelo_0, ~ fitted(modelo_0) + I(fitted(modelo_0)^2))
#H0: Modelo � homoced�stico (vari�ncia do res�duo � constante)
#H1: Modelo � heteroced�stico (vari�ncia do res�duo n�o � constante)
#Se o p-value calculado for menor que 0,05 podemos rejeitar H0 e, portanto, o modelo apresenta heterocedasticidade


# Teste de heterocedasticidade para a regress�o do logaritmo do sal�rio por hora

# O mesmo do modelo_1
# modelo_1 <- lm(log(salhora) ~ esc + exp + g�nero, sal�rio)
summary(modelo_1)

# Teste de Breush-Pagan
bptest(modelo_1)
# H0: Modelo � homoced�stico (vari�ncia do res�duo � constante)
# H1: Modelo � heteroced�stico (vari�ncia do res�duo n�o � constante)
# Se o p-value calculado for menor que 0,05 podemos rejeitar H0 e, portanto, o modelo apresenta heterocedasticidade

#Teste alternativo de White
bptest(modelo_1, ~ fitted(modelo_0) + I(fitted(modelo_0)^2))
#H0: Modelo � homoced�stico (vari�ncia do res�duo � constante)
#H1: Modelo � heteroced�stico (vari�ncia do res�duo n�o � constante)
#Se o p-value calculado for menor que 0,05 podemos rejeitar H0 e, portanto, o modelo apresenta heterocedasticidade


# Medidas corretivas -----------------------------------------------------------


# Erros padr�o robustos para heterocedasticidade -------------------------------

# Regress�o para sal�rio por hora
# modelo_0 <- lm(salhora ~ esc + exp + g�nero, sal�rio)
summary(modelo_0)

# Regress�o para sal�rio por hora com erros padr�o robustos
coeftest(modelo_0, vcov. = vcovHC(modelo_0, type = "HC1"))
# Os coeficientes s�o os mesmos, mas os erros padr�o agora est�o corrigidos para a heterocedasticidade

# Regress�o para o logaritmo do sal�rio por hora
# modelo_1 <- lm(log(salhora) ~ esc + exp + g�nero, sal�rio)
summary(modelo_1)

# Regress�o para o logaritmo do sal�rio por hora com erros padr�o robustos
coeftest(modelo_1, vcov. = vcovHC(modelo_1, type = "HC1"))
# Nesse caso, como os testes n�o mostram a presen�a do problema, os erros padr�o robustos s�o desnecess�rios 


# M�nimos Quadrados Ponderados (MQP) -------------------------------------------
  
# Usamos o MQP quando conhecemos o padr�o de heterocedasticidade, no entanto, essa 
# situa��o � rara. Como  exemplo, vamos supor que o padr�o de heterocedasticidade ser�
# var(e|x)=(sigma^2)*(esc), assim o peso usado no MQP ser� peso=1/esc.

# MQP: estime o modelo com peso=1/esc 
modelo_MQP <- lm(formula = salhora ~ esc + exp + g�nero, 
                data = sal�rio, weights = 1/esc)
summary(modelo_MQP)


# M�nimos Quadrados Generalizados Fact�veis (MQGF) -----------------------------

# Quando a forma da heterocedasticidade n�o � conhecida, o caso mais comum,
# var(e|x) = sigma^2*(salhora = b1 + b2*esc + b3*exp + b4*g�nero)
# estime h_est e use o MQP com peso=1/h_est.

# Forma da heterocedasticidade, estime h_est
# modelo_0 <- lm(salhora ~ esc + exp + g�nero, sal�rio)
summary(modelo_0)
sal�rio %<>% mutate(e = resid(modelo_0), 
                    g = log(e^2))
modelo_g <- lm(g ~ esc + exp + g�nero, sal�rio)
sal�rio %<>% mutate(g_est = fitted(modelo_g),
                    h_est = exp(g_est))

# MQGF: estime o modelo usando MQP com peso=1/h_est
modelo_MQGF <- lm(formula = salhora ~ esc + exp + g�nero, 
                 data = sal�rio, 
                 weights = 1/h_est)
summary(modelo_MQGF)

# Fim do Script ----------------------------------------------------------------

