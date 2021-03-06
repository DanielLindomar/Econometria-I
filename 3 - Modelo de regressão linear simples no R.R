# Modelo de regress�o linear simples no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# T�picos:
#   Regress�o simples
#   Previs�o ap�s a regress�o
#   Medidas de qualidade do ajuste (R-quadrado)
#   Formas funcionais: modelo log-log e outros modelos log-lineares

# Arquivos de dados:
#   Sal�rio por hora2.csv

# Preparando
rm(list = ls()) 
diret�rio <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Regress�o simples ------------------------------------------------------------

# Exemplo da rela��o entre sal�rio por hora e educa��o 
sal�rio <- read.csv2(paste0(diret�rio, "Sal�rio por hora2.csv"))
View(sal�rio)

sal�rio %<>% select(salhora, esc)
str(sal�rio)
stargazer(sal�rio, type = "text")
head(sal�rio, 10)

# Explorando os dados
cor(sal�rio)
sal�rio %<>% mutate(salhora_m�dio = mean(salhora))

# Regress�o simples: sal�rio/hora = b1 + b2*escolaridade + e
modelo_salhora1 <- lm(formula = salhora ~ esc, data = sal�rio)
summary(modelo_salhora1)
modelo_salhora1$coefficients['esc']
# Podemos ainda obter os coeficientes b1 e b2 aplicando diretamente a f�rmula de MQO
(b2 <- cov(sal�rio$salhora, sal�rio$esc)/var(sal�rio$esc))
(b1 <- mean(sal�rio$salhora)-b2*mean(sal�rio$esc))

# Gr�fico de dispers�o com a reta de regress�o estimada
plot(x = sal�rio$esc, y = sal�rio$salhora)
abline(a = modelo_salhora1$coefficients['(Intercept)'], 
       b = modelo_salhora1$coefficients['esc'],
       col = 'red')
# ou use  pacote ggplot
ggplot(data = sal�rio, mapping = aes(x = esc, y = salhora)) +
  theme_bw() + # personaliza o fundo do gr�fico, bw refere-se ao tema preto e branco
  geom_point() + # refere-se ao gr�fico de dispers�o
  geom_smooth(method = "lm", se = FALSE) # faz o ajuste da linha de regress�o


# Previs�o ap�s a regress�o ----------------------------------------------------


# Valores previstos (estimados) para a vari�vel dependente sal�rio/hora (salhora_est)
sal�rio %<>% mutate(salhora_est = fitted(modelo_salhora1))
stargazer(sal�rio, type = "text")
ggplot(data = sal�rio, mapping = aes(x = esc)) +
  geom_point(mapping = aes(y = salhora, color = 'Sal�rio por hora - valor observado')) +
  geom_point(mapping = aes(y = salhora_est, color = 'Sal�rio por hora - valor estimado')) + 
  xlab('Escolaridade')

# Res�duos
sal�rio %<>% mutate(e_est = residuals(modelo_salhora1))
stargazer(sal�rio, type = "text")
ggplot(sal�rio, aes(x = esc)) +
  geom_point(aes(y = salhora, col = 'Sal�rio por hora - valor observado')) +
  geom_point(aes(y = e_est, col = 'Res�duo estimado')) +
  xlab('Escolaridade')

head(sal�rio, 10)

# Gr�fico dos valores observados e previstos e dos res�duos
ggplot(sal�rio, aes(x = esc)) +
  geom_point(aes(y = salhora, color = 'Sal�rio por hora - valor observado')) +
  geom_point(aes(y = salhora_est, color = 'Sal�rio por hora - valor estimado')) +
  geom_point(aes(y = e_est, color = 'Res�duo estimado')) +
  geom_smooth(aes(y = salhora, color = 'Ajuste de linha'), 
              method = "lm", se = FALSE) +
  xlab('Escolaridade')


# Medidas de qualidade do ajuste (R-quadrado) ----------------------------------

# Use a fun��o 'str' para mostrar o que est� inclu�do nos resultados da regress�o
str(modelo_salhora1)
str(summary(modelo_salhora1))

# O R-quadrado � armazenado como 'r.squared'
summary(modelo_salhora1)$r.squared

# Use a fun��o 'nobs' para ver o n�mero de observa��es
nobs(modelo_salhora1)


# Formas funcionais: modelo log-log e outros modelos log-lineares --------------

# Vamos criar as vari�vel log_salhora e log_esc
sal�rio %<>% mutate(log_salhora=log(salhora), log_esc=log(esc))

# Forma funcional linear
modelo_salhora1 <- lm(salhora ~ esc, sal�rio)
summary(modelo_salhora1)
# Gr�fico
ggplot(sal�rio, aes(x = esc, y = salhora)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# Forma funcional log-log ou log-linear
modelo_salhora2 <- lm(log_salhora ~ log_esc, sal�rio)
summary(modelo_salhora2)
# Gr�fico
ggplot(sal�rio, aes(x = log_esc, y = log_salhora)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Forma funcional lin-log
modelo_salhora3 <- lm(salhora ~ log_esc, sal�rio)
summary(modelo_salhora3)
# Gr�fico
ggplot(sal�rio, aes(x = log_esc, y = salhora)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Forma funcional log-lin
modelo_salhora4 <- lm(log_salhora ~ esc, sal�rio)
summary(modelo_salhora4)
# Gr�fico
ggplot(sal�rio, aes(x = esc, y = log_salhora)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Regress�o que passa pela origem, modelo de intercepto zero
modelo_salhora5 <- lm(salhora ~ 0 + esc, sal�rio)
summary(modelo_salhora5)
# Gr�fico
ggplot(sal�rio, aes(x = esc, y = salhora)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ 0 + x, se = F)

# Regress�o sobre uma constante
modelo_salhora6 <- lm(salhora ~ 1, sal�rio)
summary(modelo_salhora6)
# Gr�fico
ggplot(sal�rio, aes(x = esc, y = salhora)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ 1, se = F)

# Fim do Script ----------------------------------------------------------------