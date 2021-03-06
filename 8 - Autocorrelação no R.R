# Autocorrela��o no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# T�picos:
#   An�lise gr�fica da autocorrela��o 
#   Testes de autocorrela��o:
#     Teste de Breusch-Godfrey
#     Teste de Dunbin-Watson
#   Medidas corretivas:
#     Erros padr�o robustos para heterocedasticidade e autocorrela��o
#     Transforma��o de primeira diferen�a
#     M�nimos Quadrados Generalizados Fact�veis (MQGF)

# Arquivos de dados: 
#   Consumo.csv

# Preparando
rm(list = ls()) 
diret�rio <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", "car", "orcutt", "quantmod", "prais")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# An�lise gr�fica da autocorrela��o -------------------------------------------- 

# Dados de consumo
consumo <- read.csv2(paste0(diret�rio, "Consumo.csv"))

consumo %>%
  str

consumo %>% 
  stargazer(type = "text")

consumo %>% 
  head(10)

# Primeiro faremos o ajuste do modelo de regress�o para explicar o consumo agregado nos EUA
modelo_0 <- lm(consumo ~ renda + riqueza + juros, consumo)
summary(modelo_0)
consumo %<>% mutate(resid = resid(modelo_0))

# An�lise do gr�fico do res�duo no tempo
ggplot(data = consumo, mapping = aes(x = year, y = resid)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Res�duos', x = 'Ano')
#(Qualquer padr�o observado pode indicar a autocorrela��o)

# An�lise do gr�fico de res�duos atuais com os res�duos defasados
consumo %<>% mutate(resid_def = Lag(resid, k=1))
ggplot(data = consumo, mapping = aes(x = resid_def, y = resid)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Res�duos', x = 'Res�duos defasados')
#(Qualquer padr�o observado pode indicar a autocorrela��o)


# Testes de autocorrela��o -----------------------------------------------------

# Os testes de autocorrela��o consistem na estima��o de regress�es dos res�duos   
# do modelo de interesse em rela��o aos res�duos defasados como vari�veis independentes 
# (Teste de Breush-Godfrey). Podemos, ainda, calcular uma esp�cie de coeficiente de 
# correla��o entre o res�duo e o res�duo defasado (estat�stica d de Durbin-Watson)


# Teste de autocorrela��o para a regress�o do consumo agregado -----------------

# O mesmo do modelo_0
# modelo_0 <- lm(consumo ~ renda + riqueza + juros, consumo)
summary(modelo_0)

# Teste de autocorrela��o de Breush-Godfrey
bgtest(modelo_0, order = 1)
bgtest(modelo_0, order = 2)
bgtest(modelo_0, order = 3)
# H0: Res�duos do modelo n�o s�o autocorrelacionados
# H1: Res�duos do modelo s�o autocorrelacionados
# Se o p-value calculado for menor que 0,05 podemos rejeitar H0 e, portanto, o modelo apresenta autocorrela��o

# Teste de autocorrela��o de Durbin-Watson (estat�stica d)
dwtest(modelo_0)
# Valores da estat�stica de Durbin-Watson pr�ximos de 2 s�o ind�cios de aus�ncia de autocorrela��o, ou seja, valores distantes de 2 indicam presen�a de autocorrela��o
# H0: Res�duos do modelo n�o s�o autocorrelacionados
# H1: Res�duos do modelo s�o autocorrelacionados


# Medidas corretivas -----------------------------------------------------------

# Erros padr�o robustos para heterocedasticidade e autocorrela��o --------------

# Regress�o para o consumo agregado
# modelo_0 <- lm(consumo ~ renda + riqueza + juros, consumo)
summary(modelo_0)

# Regress�o para o consumo agregado com erros padr�o robustos
coeftest(modelo_0, vcovHAC)
# Os coeficientes s�o os mesmos, mas os erros padr�o agora est�o corrigidos para a heterocedasticidade e autocorrela��o


# Transforma��o de primeira diferen�a ------------------------------------------

# A tranforma��o de primeira diferen�a retira componentes que causam a autocorrela��o
modelo_1 <- lm(diff(consumo) ~ diff(renda) + diff(riqueza) + diff(juros), consumo)
summary(modelo_1)
bgtest(modelo_1)


# M�nimos Quadrados Generalizados Fact�veis (MQGF) -----------------------------

# Os estimadores MQGF calculam o grau de autocorrela��o e corrigem a regress�o

# Regress�o para o consumo agregado
# modelo_0 <- lm(consumo ~ renda + riqueza + juros, consumo)
summary(modelo_0)

# Regress�o de Prais-Winstein
modelo_prais <- prais_winsten(modelo_0, data=consumo)
summary(modelo_prais)

# Regress�o de Cochrane-Orcutt
modelo_cochrane <- cochrane.orcutt(modelo_0)
summary(modelo_cochrane)

# Fim do Script ----------------------------------------------------------------
