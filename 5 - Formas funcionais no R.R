# Formas funcionais no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# Tópicos:
#   Regressão com operações ou mudanças de escala nas variáveis
#   Regressão com variáveis padronizadas
#   Regressão com logaritmos
#   Regressão com termos quadráticos ou polinomiais
#   Regressão com termos de interação

# Arquivos de dados:
#   PIB real.csv
#   Salário por hora2.csv

# Preparando
rm(list = ls()) 
diretório <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr", "effects", "dynlm")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


# Regressão com operações ou mudanças de escala nas variáveis ------------------

# Dados do PIB real dos EUA entre 1960 e 2007 em US$ bilhões
PIB <- read.csv2(paste0(diretório, "PIB real.csv"))
PIBts <- ts(PIB, start = 1960)
str(PIB)
str(PIBts)

# Podemos estimar a regressão do PIB em relação a uma tendência
modelo_PIB <- dynlm(PIB.real ~ trend(PIBts), PIBts)
summary(modelo_PIB)
PIB %<>% mutate(PIB_est = fitted(modelo_PIB))
ggplot(data = PIB, mapping = aes(x = ano)) + 
  theme_bw() +
  geom_point(mapping = aes(y = PIB.real, col = 'PIB')) +
  geom_line(mapping = aes(y = PIB_est, col = 'linha de regressão'))

# O PIB é medido em US$ bilhões, mas podemos querer usar em US$ milhões
# Para isso, e para outras operações podemos usar a função I(...)
(dynlm(I(PIB.real*1000) ~ trend(PIBts), PIBts)) %>% summary()

# Podemos estar interessados no PIB em R$ bilhões de reais
# Para isso, multiplicamos pela taxa de câmbio R$/US$, vamos supor 5,50
(dynlm(I(PIB.real*5.5) ~ trend(PIBts), PIBts)) %>% summary()

# Para R$ trilhões
(dynlm(I(PIB.real*5.5/1000) ~ trend(PIBts), PIBts)) %>% summary()

# Para tendência quadrática
modelo_PIB2 <- dynlm(PIB.real ~ trend(PIBts) + I(trend(PIBts)^2), PIBts)
summary(modelo_PIB2)
PIB %<>% mutate(PIB_est2 = fitted(modelo_PIB2))
ggplot(data = PIB, mapping = aes(x = ano)) + 
  theme_bw() +
  geom_point(mapping = aes(y = PIB.real, col = 'PIB')) +
  geom_line(mapping = aes(y = PIB_est2, col = 'linha de regressão'))

#Se quisermos calcular a taxa média de crescimento
(dynlm(log(PIB.real) ~ trend(PIBts), PIBts)) %>% summary()


# Regressão com variáveis padronizadas ----------------------------------------

# Vamos voltar para nosso exemplo do salário por hora
salário <- read.csv2(paste0(diretório, "Salário por hora2.csv"))

# Modelo com escolaridade e experiência
(lm(salhora ~ esc + exp, salário)) %>% summary()

# Uma variável padronizada é construída pela subtração da média e divisão pelo desvio padrão
# A unidade de medida passa a ser o desvio padrão e, assim, é possível verificar qual variável 
# tem maior influência sobre a variável dependente, a função scale(...) padroniza as variáveis
(lm(scale(salhora) ~ scale(esc) + scale(exp), salário)) %>% summary()


# Regressão com logaritmos -----------------------------------------------------

# Já vimos as várias formas funcionais com logaritmos, a função log(...) facilita a utilização desses modelos
# Regressão em nível
lm(salhora ~ esc + exp, salário) %>% summary
# Regressão lin-log
lm(salhora ~ log(esc) + exp, salário) %>% summary
# Regressão log-lin
lm(log(salhora) ~ esc + exp, salário) %>% summary
# Regressão log-log
lm(log(salhora) ~ log(esc) + exp, salário) %>% summary


# Regressão com termos quadráticos ou polinomiais ------------------------------

# Regressão do salário hora em função da escolaridade
modelo_1 <- lm(salhora ~ esc, salário)
summary(modelo_1)
salário %<>% mutate(salhora_est = fitted(modelo_1))
ggplot(data = salário, mapping = aes(x = esc)) + 
  theme_bw() +
  geom_point(mapping = aes(y = salhora, col = 'Salário por hora')) +
  geom_line(mapping = aes(y = salhora_est, col = 'Linha de regressão'))

# Regressão com termo quadrático
# salhora = b1 + b2*esc + b3*(esc^2) + e
modelo_2 <- lm(salhora ~ esc + I(esc^2), salário)
summary(modelo_2)

# Podemos usar também a função poly(...)
modelo_2 <- lm(salhora ~ poly(esc, 2, raw=TRUE), salário)
summary(modelo_2)

salário %<>% mutate(salhora_est2 = fitted(modelo_2))
ggplot(data = salário, mapping = aes(x = esc)) + 
  theme_bw() +
  geom_point(mapping = aes(y = salhora, col = 'Salário por hora')) +
  geom_line(mapping = aes(y = salhora_est2, col = 'Linha de regressão'))

# Quando inserimos efeitos não lineares nas variáveis, seu efeito marginal dependerá
# do ponto em que é avaliado.

# Calculando o ponto de máximo ou mínimo para o efeito marginal, esc* = -b2/2*b3
coef_2  <- coef(modelo_2)
b_esc   <- coef_2["poly(esc, 2, raw = TRUE)1"]
b_esc2  <- coef_2["poly(esc, 2, raw = TRUE)2"]
-b_esc / (2*b_esc2)

# Efeito marginal da escolaridade sobre o salário por hora = b2 + 2*b3*esc
b_esc + 2*b_esc2*3
b_esc + 2*b_esc2*4.3
b_esc + 2*b_esc2*5
b_esc + 2*b_esc2*10

# Efeito marginal no ponto médio
(média_esc <- mean(salário$esc))
(emm_esc <- b_esc + 2*b_esc2*média_esc)

# Efeito marginal médio
em_esc  <- b_esc + 2*b_esc2*salário$esc
(emmédio_esc <- mean(em_esc))

# Gráfico dos efeitos marginais da escolaridade sobre o salário por hora
plot(effect("esc", modelo_2))

# Exemplo com experiência no lugar da escolaridade

# Regressão do salário hora em função da experiência
modelo_3 <- lm(salhora ~ exp, salário)
summary(modelo_3)
salário %<>% mutate(salhora_est3 = fitted(modelo_3))
ggplot(data = salário, mapping = aes(x = exp)) + 
  theme_bw() +
  geom_point(mapping = aes(y = salhora, col = 'Salário por hora')) +
  geom_line(mapping = aes(y = salhora_est3, col = 'Linha de regressão'))

# Regressão com termo quadrático
# salhora = b1 + b2*exp + b3*(exp^2) + e
modelo_4 <- lm(salhora ~ poly(exp, 2, raw=TRUE), salário)
summary(modelo_4)
salário %<>% mutate(salhora_est4 = fitted(modelo_4))
ggplot(data = salário, mapping = aes(x = exp)) + 
  theme_bw() +
  geom_point(mapping = aes(y = salhora, col = 'Salário por hora')) +
  geom_line(mapping = aes(y = salhora_est4, col = 'Linha de regressão'))

# Calculando o ponto de máximo ou mínimo para o efeito marginal, exp* = -b2/2*b3
coef_4 <- coef(modelo_4)
b_exp  <- coef_4["poly(exp, 2, raw = TRUE)1"]
b_exp2 <- coef_4["poly(exp, 2, raw = TRUE)2"]
-b_exp / (2*b_exp2)

# Efeito marginal da experiência sobre o salário por hora = b2 + 2*b3*esc
b_exp + 2*b_exp2*10
b_exp + 2*b_exp2*20
b_exp + 2*b_exp2*24.6
b_exp + 2*b_exp2*30

# Efeito marginal no ponto médio
(média_exp <- mean(salário$exp))
(emm_exp <- b_exp + 2*b_exp2*média_exp)

# Efeito marginal médio
em_exp  <- b_exp + 2*b_exp2*salário$exp
(emmédio_exp <- mean(em_exp))

# Gráfico dos efeitos marginais da experiência sobre o salário por hora
plot(effect("exp", modelo_4))


# Regressão com termos de interação --------------------------------------------

# Regressão do salário por hora
modelo_5 <- lm(salhora ~ esc + exp + gênero, salário)
summary(modelo_5)
salário %<>% mutate(salhora_est5 = fitted(modelo_5))
ggplot(salário, aes(x = esc)) +
  theme_bw() +
  geom_point(aes(y = salhora, col = 'Salário por hora')) +
  geom_point(aes(y = salhora_est5, col = 'Valores previstos'))

# Regressão com termo de interação
# salhora = b1 + b2*esc + b3*exp + b4*gênero + b5*esc*exp
modelo_6 <- lm(salhora ~ esc + exp + gênero + esc*exp, salário)
summary(modelo_6)
salário %<>% mutate(salhora_est6 = fitted(modelo_6))
ggplot(salário, aes(x = esc)) +
  theme_bw() +
  geom_point(aes(y = salhora, col = 'Salário por hora')) +
  geom_point(aes(y = salhora_est6, col = 'Valores previstos'))

# Calculando o efeito marginal da escolaridade sobre o salário por hora em vários níveis de experiência
# = b2 + b5*exper
coef_6    <- coef(modelo_6)
b_esc     <- coef_6["esc"]
b_escXexp <- coef_6["esc:exp"]

b_esc + b_escXexp*10
b_esc + b_escXexp*20
b_esc + b_escXexp*30

# Fim do Script ----------------------------------------------------------------

