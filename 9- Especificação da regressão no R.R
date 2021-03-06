# Especifica��o da regress�o no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# T�picos:
#   Teste RESET
#   Vari�veis proxy
#   Erros de medi��o nas vari�veis dependente e independente


# Arquivos de dados: 
#   wage1.csv
#   wage2.csv

# Preparando
rm(list = ls()) 
diret�rio <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr", "car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


# Teste RESET ------------------------------------------------------------------
# O teste RESET inclui os termos quadr�tico e c�bico dos valores previstos no
# modelo de regress�o e testa a signific�ncia conjunta dos coeficientes.

# Dados de sal�rio por hora
sal�rio1 <- read.csv(paste0(diret�rio, "wage1.csv"))

# Modelo de regress�o do sal�rio por hora
modelo_1 <- lm(wage ~ educ + exper + tenure, sal�rio1)
summary(modelo_1)
sal�rio1 %<>% mutate(yprev = fitted(modelo_1),
                  yprev2 = yprev^2,
                  yprev3 = yprev^3)

# Teste RESET, testando a signific�ncia conjunta dos termos quadr�tico e c�bico
modelo_1_RESET <- update(modelo_1, ~ . + yprev2 + yprev3)
summary(modelo_1_RESET)
# A hip�tese nula � de que o modelo est� bem specificado
linearHypothesis(modelo_1_RESET, c("yprev2 = 0", "yprev3 = 0"))
# Como o p-value<0,05 o modelo est� subespecificado ou mal especificado.

# Modelo de regress�o do logar�tmo do sal�rio por hora
modelo_2 <- lm(lwage ~ educ + exper + tenure, sal�rio1)
summary(modelo_2)
sal�rio1 %<>% mutate(lyprev = fitted(modelo_2),
                     lyprev2 = lyprev^2,
                     lyprev3 = lyprev^3)
modelo_2_RESET <- update(modelo_2, ~ . + lyprev2 + lyprev3, sal�rio1)
summary(modelo_2_RESET)
linearHypothesis(modelo_2_RESET, c("lyprev2 = 0", "lyprev3 = 0"))
# Como o p-value<0,05 o modelo est� subespecificado ou mal especificado.

# Modelo de regress�o do sal�rio por hora incluindo termos quadr�ticos
modelo_3 <- update(modelo_1, ~ . + I(educ^2) + I(exper^2) + I(tenure^2))
summary(modelo_3)
sal�rio1 %<>% mutate(yprev1 = fitted(modelo_3),
                     yprev12 = yprev1^2,
                     yprev13 = yprev1^3)
modelo_3_RESET <- update(modelo_3, ~ . + yprev12 + yprev13)
summary(modelo_3_RESET)
linearHypothesis(modelo_3_RESET, c("yprev12 = 0", "yprev13 = 0"))
# Como o p-value<0,05 o modelo est� subespecificado ou mal especificado.

# Modelo de regress�o do logar�tmo do sal�rio por hora incluindo termos quadr�ticos
modelo_4 <- update(modelo_2, ~ . + I(educ^2) + I(exper^2) + I(tenure^2))
summary(modelo_4)
sal�rio1 %<>% mutate(lyprev1 = fitted(modelo_4),
                     lyprev12 = lyprev1^2,
                     lyprev13 = lyprev1^3)
modelo_4_RESET <- update(modelo_4, ~ . + lyprev12 + lyprev13, sal�rio1)
summary(modelo_4_RESET)
linearHypothesis(modelo_4_RESET, c("lyprev12 = 0", "lyprev13 = 0"))
# Esse modelo est� corretamente especificado pois p-value>0,05.


# Vari�vel proxy ---------------------------------------------------------------
  
# Exemplo do sal�rio com QI
sal�rio2 <- read.csv(paste0(diret�rio, "wage2.csv"))

# Gerando uma vari�vel normal padronizada
set.seed(0)
# set.seed (0) gera a mesma semente aleat�ria (valor inicial) para fins de reprodu��o do comando
# n�meros aleat�rios podem n�o ser replic�veis em softwares diferentes devido � diferen�a de algoritmos
sal�rio2 %<>% mutate(r = rnorm(n = nrow(sal�rio2)))
head(sal�rio2$r, 10)

# Gerando a vari�vel fake habilidade "habil"
sal�rio2 %<>% mutate(habil1 = 5 + 10*IQ + r,
                  habil = round(habil1, 0))

# Nova base de dados
sal�rio2 %>% select(wage, educ, habil, IQ) %>%
  stargazer(type = "text", digits = 1)

sal�rio2 %>% select(wage, educ, habil, IQ) %>%
  head(5)

# Modelo verdadeiro com educa��o e habilidade 
# wage = b0 + b1*educ + b2*habil + u
modelo_5 <- lm(wage ~ educ + habil, sal�rio2)
summary(modelo_5)
(b2 <- coef(modelo_5)["habil"])

# Modelo com a vari�vel habilidade omitida, o coeficiente da educa��o ser� viesado
modelo_6 <- update(modelo_5, ~ . - habil)
summary(modelo_6)

# IQ � uma vari�vel proxy para a vari�vel habilidade
# Modelo para a habilidade sendo explicada por IQ
# habil = d0 + d2*IQ + v
modelo_7 <- lm(habil ~ IQ, sal�rio2)
summary(modelo_7)
(d2 <- coef(modelo_7)["IQ"])

# Modelo com IQ como uma proxy para habilidade
modelo_8 <- lm(wage ~ educ + IQ, sal�rio2)
summary(modelo_8)
coef(modelo_7)["IQ"]
b2*d2
# O coeficiente da educa��o n�o � viesado no modelo com a vari�vel proxy.
# O coeficiente da vari�vel proxy � um m�ltiplo de dois coeficientes
# b2 (coef. da habilidade na regress�o do sal�rio (wage)) e d2 (coef. do IQ na regress�o da habilidade).


# Erros de mensura��o -------------------------------------------------------

# Gerando a vari�vel sal�rio com erro de mensura��o wage_m, arredondada para o m�ltiplo de 5 superior
sal�rio1 %<>% mutate(wage_m = case_when(
  wage > 20 ~ 25,
  wage > 15 ~ 20,
  wage > 10 ~ 15,
  wage > 5 ~ 10,
  wage > 0 ~ 5
))

# Gerando a vari�vel sal�rio com erro de mensura��o exper_m, arredondada para o m�ltiplo de 10 superior
sal�rio1 %<>% mutate(exper_m = case_when(
  exper > 40 ~ 50,
  exper > 30 ~ 40,
  exper > 20 ~ 30,
  exper > 10 ~ 20,
  exper > 0 ~ 10
))

select(sal�rio1, wage, wage_m, exper, exper_m) %>% head(10)
select(sal�rio1, wage, wage_m, exper, exper_m) %>% stargazer(type = "text")

# Modelo sem erro de mensura��o
# modelo_1 <- lm(wage ~ educ + exper + tenure, wage1)
summary(modelo_1)

# Modelo com erro de mensura��o na vari�vel dependente wage_m
modelo_9 <- update(modelo_1, wage_m ~ .)
summary(modelo_9)
# Os coeficientes n�o s�o viesados, mas o coeficiente de exper torna-se n�o significativo.

# Modelo com erro de mensura��o na vari�vel independente exper_m
modelo_10 <- update(modelo_1, ~ . - exper + exper_m)
summary(modelo_10)
# Coeficiente da vari�vel exper_m � viesado.

# Fim do script
