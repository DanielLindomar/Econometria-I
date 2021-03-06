# Formas funcionais no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# T�picos:
#   Regress�o com opera��es ou mudan�as de escala nas vari�veis
#   Regress�o com vari�veis padronizadas
#   Regress�o com logaritmos
#   Regress�o com termos quadr�ticos ou polinomiais
#   Regress�o com termos de intera��o

# Arquivos de dados:
#   PIB real.csv
#   Sal�rio por hora2.csv

# Preparando
rm(list = ls()) 
diret�rio <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr", "effects", "dynlm")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


# Regress�o com opera��es ou mudan�as de escala nas vari�veis ------------------

# Dados do PIB real dos EUA entre 1960 e 2007 em US$ bilh�es
PIB <- read.csv2(paste0(diret�rio, "PIB real.csv"))
PIBts <- ts(PIB, start = 1960)
str(PIB)
str(PIBts)

# Podemos estimar a regress�o do PIB em rela��o a uma tend�ncia
modelo_PIB <- dynlm(PIB.real ~ trend(PIBts), PIBts)
summary(modelo_PIB)
PIB %<>% mutate(PIB_est = fitted(modelo_PIB))
ggplot(data = PIB, mapping = aes(x = ano)) + 
  theme_bw() +
  geom_point(mapping = aes(y = PIB.real, col = 'PIB')) +
  geom_line(mapping = aes(y = PIB_est, col = 'linha de regress�o'))

# O PIB � medido em US$ bilh�es, mas podemos querer usar em US$ milh�es
# Para isso, e para outras opera��es podemos usar a fun��o I(...)
(dynlm(I(PIB.real*1000) ~ trend(PIBts), PIBts)) %>% summary()

# Podemos estar interessados no PIB em R$ bilh�es de reais
# Para isso, multiplicamos pela taxa de c�mbio R$/US$, vamos supor 5,50
(dynlm(I(PIB.real*5.5) ~ trend(PIBts), PIBts)) %>% summary()

# Para R$ trilh�es
(dynlm(I(PIB.real*5.5/1000) ~ trend(PIBts), PIBts)) %>% summary()

# Para tend�ncia quadr�tica
modelo_PIB2 <- dynlm(PIB.real ~ trend(PIBts) + I(trend(PIBts)^2), PIBts)
summary(modelo_PIB2)
PIB %<>% mutate(PIB_est2 = fitted(modelo_PIB2))
ggplot(data = PIB, mapping = aes(x = ano)) + 
  theme_bw() +
  geom_point(mapping = aes(y = PIB.real, col = 'PIB')) +
  geom_line(mapping = aes(y = PIB_est2, col = 'linha de regress�o'))

#Se quisermos calcular a taxa m�dia de crescimento
(dynlm(log(PIB.real) ~ trend(PIBts), PIBts)) %>% summary()


# Regress�o com vari�veis padronizadas ----------------------------------------

# Vamos voltar para nosso exemplo do sal�rio por hora
sal�rio <- read.csv2(paste0(diret�rio, "Sal�rio por hora2.csv"))

# Modelo com escolaridade e experi�ncia
(lm(salhora ~ esc + exp, sal�rio)) %>% summary()

# Uma vari�vel padronizada � constru�da pela subtra��o da m�dia e divis�o pelo desvio padr�o
# A unidade de medida passa a ser o desvio padr�o e, assim, � poss�vel verificar qual vari�vel 
# tem maior influ�ncia sobre a vari�vel dependente, a fun��o scale(...) padroniza as vari�veis
(lm(scale(salhora) ~ scale(esc) + scale(exp), sal�rio)) %>% summary()


# Regress�o com logaritmos -----------------------------------------------------

# J� vimos as v�rias formas funcionais com logaritmos, a fun��o log(...) facilita a utiliza��o desses modelos
# Regress�o em n�vel
lm(salhora ~ esc + exp, sal�rio) %>% summary
# Regress�o lin-log
lm(salhora ~ log(esc) + exp, sal�rio) %>% summary
# Regress�o log-lin
lm(log(salhora) ~ esc + exp, sal�rio) %>% summary
# Regress�o log-log
lm(log(salhora) ~ log(esc) + exp, sal�rio) %>% summary


# Regress�o com termos quadr�ticos ou polinomiais ------------------------------

# Regress�o do sal�rio hora em fun��o da escolaridade
modelo_1 <- lm(salhora ~ esc, sal�rio)
summary(modelo_1)
sal�rio %<>% mutate(salhora_est = fitted(modelo_1))
ggplot(data = sal�rio, mapping = aes(x = esc)) + 
  theme_bw() +
  geom_point(mapping = aes(y = salhora, col = 'Sal�rio por hora')) +
  geom_line(mapping = aes(y = salhora_est, col = 'Linha de regress�o'))

# Regress�o com termo quadr�tico
# salhora = b1 + b2*esc + b3*(esc^2) + e
modelo_2 <- lm(salhora ~ esc + I(esc^2), sal�rio)
summary(modelo_2)

# Podemos usar tamb�m a fun��o poly(...)
modelo_2 <- lm(salhora ~ poly(esc, 2, raw=TRUE), sal�rio)
summary(modelo_2)

sal�rio %<>% mutate(salhora_est2 = fitted(modelo_2))
ggplot(data = sal�rio, mapping = aes(x = esc)) + 
  theme_bw() +
  geom_point(mapping = aes(y = salhora, col = 'Sal�rio por hora')) +
  geom_line(mapping = aes(y = salhora_est2, col = 'Linha de regress�o'))

# Quando inserimos efeitos n�o lineares nas vari�veis, seu efeito marginal depender�
# do ponto em que � avaliado.

# Calculando o ponto de m�ximo ou m�nimo para o efeito marginal, esc* = -b2/2*b3
coef_2  <- coef(modelo_2)
b_esc   <- coef_2["poly(esc, 2, raw = TRUE)1"]
b_esc2  <- coef_2["poly(esc, 2, raw = TRUE)2"]
-b_esc / (2*b_esc2)

# Efeito marginal da escolaridade sobre o sal�rio por hora = b2 + 2*b3*esc
b_esc + 2*b_esc2*3
b_esc + 2*b_esc2*4.3
b_esc + 2*b_esc2*5
b_esc + 2*b_esc2*10

# Efeito marginal no ponto m�dio
(m�dia_esc <- mean(sal�rio$esc))
(emm_esc <- b_esc + 2*b_esc2*m�dia_esc)

# Efeito marginal m�dio
em_esc  <- b_esc + 2*b_esc2*sal�rio$esc
(emm�dio_esc <- mean(em_esc))

# Gr�fico dos efeitos marginais da escolaridade sobre o sal�rio por hora
plot(effect("esc", modelo_2))

# Exemplo com experi�ncia no lugar da escolaridade

# Regress�o do sal�rio hora em fun��o da experi�ncia
modelo_3 <- lm(salhora ~ exp, sal�rio)
summary(modelo_3)
sal�rio %<>% mutate(salhora_est3 = fitted(modelo_3))
ggplot(data = sal�rio, mapping = aes(x = exp)) + 
  theme_bw() +
  geom_point(mapping = aes(y = salhora, col = 'Sal�rio por hora')) +
  geom_line(mapping = aes(y = salhora_est3, col = 'Linha de regress�o'))

# Regress�o com termo quadr�tico
# salhora = b1 + b2*exp + b3*(exp^2) + e
modelo_4 <- lm(salhora ~ poly(exp, 2, raw=TRUE), sal�rio)
summary(modelo_4)
sal�rio %<>% mutate(salhora_est4 = fitted(modelo_4))
ggplot(data = sal�rio, mapping = aes(x = exp)) + 
  theme_bw() +
  geom_point(mapping = aes(y = salhora, col = 'Sal�rio por hora')) +
  geom_line(mapping = aes(y = salhora_est4, col = 'Linha de regress�o'))

# Calculando o ponto de m�ximo ou m�nimo para o efeito marginal, exp* = -b2/2*b3
coef_4 <- coef(modelo_4)
b_exp  <- coef_4["poly(exp, 2, raw = TRUE)1"]
b_exp2 <- coef_4["poly(exp, 2, raw = TRUE)2"]
-b_exp / (2*b_exp2)

# Efeito marginal da experi�ncia sobre o sal�rio por hora = b2 + 2*b3*esc
b_exp + 2*b_exp2*10
b_exp + 2*b_exp2*20
b_exp + 2*b_exp2*24.6
b_exp + 2*b_exp2*30

# Efeito marginal no ponto m�dio
(m�dia_exp <- mean(sal�rio$exp))
(emm_exp <- b_exp + 2*b_exp2*m�dia_exp)

# Efeito marginal m�dio
em_exp  <- b_exp + 2*b_exp2*sal�rio$exp
(emm�dio_exp <- mean(em_exp))

# Gr�fico dos efeitos marginais da experi�ncia sobre o sal�rio por hora
plot(effect("exp", modelo_4))


# Regress�o com termos de intera��o --------------------------------------------

# Regress�o do sal�rio por hora
modelo_5 <- lm(salhora ~ esc + exp + g�nero, sal�rio)
summary(modelo_5)
sal�rio %<>% mutate(salhora_est5 = fitted(modelo_5))
ggplot(sal�rio, aes(x = esc)) +
  theme_bw() +
  geom_point(aes(y = salhora, col = 'Sal�rio por hora')) +
  geom_point(aes(y = salhora_est5, col = 'Valores previstos'))

# Regress�o com termo de intera��o
# salhora = b1 + b2*esc + b3*exp + b4*g�nero + b5*esc*exp
modelo_6 <- lm(salhora ~ esc + exp + g�nero + esc*exp, sal�rio)
summary(modelo_6)
sal�rio %<>% mutate(salhora_est6 = fitted(modelo_6))
ggplot(sal�rio, aes(x = esc)) +
  theme_bw() +
  geom_point(aes(y = salhora, col = 'Sal�rio por hora')) +
  geom_point(aes(y = salhora_est6, col = 'Valores previstos'))

# Calculando o efeito marginal da escolaridade sobre o sal�rio por hora em v�rios n�veis de experi�ncia
# = b2 + b5*exper
coef_6    <- coef(modelo_6)
b_esc     <- coef_6["esc"]
b_escXexp <- coef_6["esc:exp"]

b_esc + b_escXexp*10
b_esc + b_escXexp*20
b_esc + b_escXexp*30

# Fim do Script ----------------------------------------------------------------

