# Especificação da regressão no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# Tópicos:
#   Teste RESET
#   Variáveis proxy
#   Erros de medição nas variáveis dependente e independente


# Arquivos de dados: 
#   wage1.csv
#   wage2.csv

# Preparando
rm(list = ls()) 
diretório <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr", "car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


# Teste RESET ------------------------------------------------------------------
# O teste RESET inclui os termos quadrático e cúbico dos valores previstos no
# modelo de regressão e testa a significância conjunta dos coeficientes.

# Dados de salário por hora
salário1 <- read.csv(paste0(diretório, "wage1.csv"))

# Modelo de regressão do salário por hora
modelo_1 <- lm(wage ~ educ + exper + tenure, salário1)
summary(modelo_1)
salário1 %<>% mutate(yprev = fitted(modelo_1),
                  yprev2 = yprev^2,
                  yprev3 = yprev^3)

# Teste RESET, testando a significância conjunta dos termos quadrático e cúbico
modelo_1_RESET <- update(modelo_1, ~ . + yprev2 + yprev3)
summary(modelo_1_RESET)
# A hipótese nula é de que o modelo está bem specificado
linearHypothesis(modelo_1_RESET, c("yprev2 = 0", "yprev3 = 0"))
# Como o p-value<0,05 o modelo está subespecificado ou mal especificado.

# Modelo de regressão do logarítmo do salário por hora
modelo_2 <- lm(lwage ~ educ + exper + tenure, salário1)
summary(modelo_2)
salário1 %<>% mutate(lyprev = fitted(modelo_2),
                     lyprev2 = lyprev^2,
                     lyprev3 = lyprev^3)
modelo_2_RESET <- update(modelo_2, ~ . + lyprev2 + lyprev3, salário1)
summary(modelo_2_RESET)
linearHypothesis(modelo_2_RESET, c("lyprev2 = 0", "lyprev3 = 0"))
# Como o p-value<0,05 o modelo está subespecificado ou mal especificado.

# Modelo de regressão do salário por hora incluindo termos quadráticos
modelo_3 <- update(modelo_1, ~ . + I(educ^2) + I(exper^2) + I(tenure^2))
summary(modelo_3)
salário1 %<>% mutate(yprev1 = fitted(modelo_3),
                     yprev12 = yprev1^2,
                     yprev13 = yprev1^3)
modelo_3_RESET <- update(modelo_3, ~ . + yprev12 + yprev13)
summary(modelo_3_RESET)
linearHypothesis(modelo_3_RESET, c("yprev12 = 0", "yprev13 = 0"))
# Como o p-value<0,05 o modelo está subespecificado ou mal especificado.

# Modelo de regressão do logarítmo do salário por hora incluindo termos quadráticos
modelo_4 <- update(modelo_2, ~ . + I(educ^2) + I(exper^2) + I(tenure^2))
summary(modelo_4)
salário1 %<>% mutate(lyprev1 = fitted(modelo_4),
                     lyprev12 = lyprev1^2,
                     lyprev13 = lyprev1^3)
modelo_4_RESET <- update(modelo_4, ~ . + lyprev12 + lyprev13, salário1)
summary(modelo_4_RESET)
linearHypothesis(modelo_4_RESET, c("lyprev12 = 0", "lyprev13 = 0"))
# Esse modelo está corretamente especificado pois p-value>0,05.


# Variável proxy ---------------------------------------------------------------
  
# Exemplo do salário com QI
salário2 <- read.csv(paste0(diretório, "wage2.csv"))

# Gerando uma variável normal padronizada
set.seed(0)
# set.seed (0) gera a mesma semente aleatória (valor inicial) para fins de reprodução do comando
# números aleatórios podem não ser replicáveis em softwares diferentes devido à diferença de algoritmos
salário2 %<>% mutate(r = rnorm(n = nrow(salário2)))
head(salário2$r, 10)

# Gerando a variável fake habilidade "habil"
salário2 %<>% mutate(habil1 = 5 + 10*IQ + r,
                  habil = round(habil1, 0))

# Nova base de dados
salário2 %>% select(wage, educ, habil, IQ) %>%
  stargazer(type = "text", digits = 1)

salário2 %>% select(wage, educ, habil, IQ) %>%
  head(5)

# Modelo verdadeiro com educação e habilidade 
# wage = b0 + b1*educ + b2*habil + u
modelo_5 <- lm(wage ~ educ + habil, salário2)
summary(modelo_5)
(b2 <- coef(modelo_5)["habil"])

# Modelo com a variável habilidade omitida, o coeficiente da educação será viesado
modelo_6 <- update(modelo_5, ~ . - habil)
summary(modelo_6)

# IQ é uma variável proxy para a variável habilidade
# Modelo para a habilidade sendo explicada por IQ
# habil = d0 + d2*IQ + v
modelo_7 <- lm(habil ~ IQ, salário2)
summary(modelo_7)
(d2 <- coef(modelo_7)["IQ"])

# Modelo com IQ como uma proxy para habilidade
modelo_8 <- lm(wage ~ educ + IQ, salário2)
summary(modelo_8)
coef(modelo_7)["IQ"]
b2*d2
# O coeficiente da educação não é viesado no modelo com a variável proxy.
# O coeficiente da variável proxy é um múltiplo de dois coeficientes
# b2 (coef. da habilidade na regressão do salário (wage)) e d2 (coef. do IQ na regressão da habilidade).


# Erros de mensuração -------------------------------------------------------

# Gerando a variável salário com erro de mensuração wage_m, arredondada para o múltiplo de 5 superior
salário1 %<>% mutate(wage_m = case_when(
  wage > 20 ~ 25,
  wage > 15 ~ 20,
  wage > 10 ~ 15,
  wage > 5 ~ 10,
  wage > 0 ~ 5
))

# Gerando a variável salário com erro de mensuração exper_m, arredondada para o múltiplo de 10 superior
salário1 %<>% mutate(exper_m = case_when(
  exper > 40 ~ 50,
  exper > 30 ~ 40,
  exper > 20 ~ 30,
  exper > 10 ~ 20,
  exper > 0 ~ 10
))

select(salário1, wage, wage_m, exper, exper_m) %>% head(10)
select(salário1, wage, wage_m, exper, exper_m) %>% stargazer(type = "text")

# Modelo sem erro de mensuração
# modelo_1 <- lm(wage ~ educ + exper + tenure, wage1)
summary(modelo_1)

# Modelo com erro de mensuração na variável dependente wage_m
modelo_9 <- update(modelo_1, wage_m ~ .)
summary(modelo_9)
# Os coeficientes não são viesados, mas o coeficiente de exper torna-se não significativo.

# Modelo com erro de mensuração na variável independente exper_m
modelo_10 <- update(modelo_1, ~ . - exper + exper_m)
summary(modelo_10)
# Coeficiente da variável exper_m é viesado.

# Fim do script
