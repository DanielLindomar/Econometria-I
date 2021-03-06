# Regress�o com vari�veis qualitativas no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# T�picos:
#   Regress�o simples com vari�vel qualitativa
#   Intera��es com outra vari�vel qualitativa
#   Intera��o com outras vari�veis
#   Teste F para diferen�as entre grupos
# 
# Arquivos de dados:
#   Sal�rio por hora2.csv

# Preparando
rm(list = ls()) 
diret�rio <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "broom", "magrittr", "car", "dplyr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Regress�o simples com vari�vel qualitativa -----------------------------------

# Exemplo do sal�rio por hora
sal�rio <- read.csv2(paste0(diret�rio, "Sal�rio por hora2.csv"))
sal�rio %<>%
  rename(feminino = g�nero, casado = civil, sul = regi�o) 
sal�rio %>% 
  select(salhora, esc, exp, feminino, casado, sul) %>% 
  head(10)
sal�rio %>% 
  select(salhora, esc, exp, feminino, casado, sul) %>%
  str
sal�rio %>% 
  select(salhora, esc, exp, feminino, casado, sul) %>%
  stargazer(type = "text")

# Sal�rio por hora m�dio
lm(salhora ~ 1, sal�rio) %>% summary
sal�rio %>% select(salhora) %>% stargazer(type = "text")

# Regress�o do sal�rio por hora em rela��o a feminino
lm(salhora ~ feminino, sal�rio) %>% summary

# Gr�fico do sal�rio em rela��o a feminino
ggplot(sal�rio, aes(x = feminino, y = salhora)) +
  theme_bw() +
  geom_point() +
  geom_smooth(aes(col = 'linha de regress�o'), method = "lm", se = F)

# Estat�sticas descritivas para os grupos feminino e masculino
sal�rio %>% 
  select(salhora) %>% 
  split(sal�rio$feminino) %>%  # divide os dados com base na vari�vel feminino
  walk(~ stargazer(., type = "text")) # calcula estat�sticas descritivas para cada grupo

# Teste t para diferen�a de m�dias de sal�rio entre homens e mulheres
t.test(formula = salhora ~ feminino, data = sal�rio)
# A m�dia, a estat�stica t, e o valor-p s�o os mesmos do coeficiente de feminino 
# na regress�o e no teste t que compara os sal�rio de homens e mulheres.

# Criando uma vari�vel dummy para masculino
sal�rio %<>% mutate(masculino = 1 - feminino)

# Regress�o do sal�rio/hora em rela��o a feminino e em rela��o a masculino
lm(salhora ~ feminino, sal�rio) %>% summary
lm(salhora ~ masculino, sal�rio) %>% summary
# O coeficiente de masculino tem a mesma magnitude e signific�ncia 
# mas tem o sinal oposto da vari�vel feminino.

# Regress�o com masculino e feminino n�o pode ser estimada devido a colinearidade
lm(salhora ~ feminino + masculino, sal�rio) %>% summary

# Regress�o com masculino e feminino pode ser estimada sem a constante
lm(salhora ~ 0 + feminino + masculino, sal�rio) %>% summary


# Intera��es com outra vari�vel qualitativa ------------------------------------

# Regress�o com termos de intera��o
modelo_1 <- lm(salhora ~ feminino*casado, sal�rio)
summary(modelo_1)

# Efeito marginal de solteiro (casado=0) e feminino=1 sobre o sal�rio m�dio
coef(modelo_1)['feminino']

# Efeito marginal de casado=1 e feminino=1 sobre o sal�rio m�dio
coef(modelo_1)['feminino'] + coef(modelo_1)['casado'] + 
  coef(modelo_1)['feminino:casado']


# Intera��o com outras vari�veis

# Podem ser criadas intera��es entre vari�veis qualitativas e vari�veis cont�nuas

# Regress�o de salhora sobre esc 
# Modelo com o mesmo intercepto e inclina��o para feminino e masculino
modelo_2 <- lm(salhora ~ esc, sal�rio) 
summary(modelo_2)

ggplot(sal�rio, aes(x = esc)) + 
  theme_bw() + 
  geom_point(aes(y = salhora)) +
  geom_smooth(aes(y = salhora), method = lm, se = F)

# Regress�o de salhora sobre esc e feminino
# Modelo com a mesma inclina��o para masculino e feminino mas com inteceptos diferentes
modelo_3 <- lm(salhora ~ esc + feminino, sal�rio)
summary(modelo_3)
sal�rio %<>% mutate(salhora_est = fitted(modelo_3))

# Gr�fico do sal�rio por hora em rela��o � escolaridade, mesma inclina��o
# para masculino e feminino mas com inteceptos diferentes

# Converta "feminino" em um fator(categ�rico) para fazer o gr�fico
sal�rio %<>%
  mutate(g�nero = factor(feminino, levels = 1:0, labels = c('feminino', 'masculino')))

ggplot(data = sal�rio, mapping = aes(x = esc, col = g�nero)) + 
  theme_bw() + 
  geom_point(aes(y = salhora)) + 
  geom_line(aes(y = salhora_est)) +
  guides(color = guide_legend(title = 'g�nero'))

# Regress�o de salhora sobre esc, feminino e uma intera��o feminino*esc
# Modelo com inclina��o e interceptos diferentes para masculino e feminino
modelo_4 <- lm(salhora ~ esc*feminino, sal�rio)
summary(modelo_4)

# Interceptos e inclina��es para feminino e masculino
b <- coef(modelo_4)
print(paste0("o intercepto para masculino � ", b['(Intercept)']))
print(paste0("o intercepto para feminino � ", b['(Intercept)'] + b['feminino']))
print(paste0("o efeito marginal da educa��o sobre o sal�rio por hora para homens � ", b['esc']))
print(paste0("o efeito marginal da educa��o sobre o sal�rio por hora para mulheres � ", 
             b['esc'] + b['esc:feminino']))


# Teste F para diferen�as entre grupos -----------------------------------------

# Podemos usar o teste F de signific�ncia geral para testar v�rias hip�teses sobre  
# a signific�ncia conjunta dos coeficientes.

# Como exemplo, podemos estimar o seguinte modelo para detemrinar o sal�rio por hora:
# salhora = b1 + b2esc + b3exp + b4feminino + b5feminino*esc + b6feminino*exp +e
modelo_5 <- lm(salhora ~ (esc + exp)*feminino, sal�rio)
summary(modelo_5)

# Podemos testar, por exemplo, se os coeficientes da v�ri�veis feminino e das intera��es com escolaridade e experi�ncia
# s�o significativas em conjunto
linearHypothesis(modelo_5, c('feminino        = 0', 
                             'esc:feminino    = 0',
                             'exp:feminino    = 0'))

# Podemos testar se apenas os coeficientes das intera��es com escolaridade e experi�ncia s�o significativos em conjunto
linearHypothesis(modelo_5, c('esc:feminino    = 0',
                             'exp:feminino    = 0'))

# Fim do Script ----------------------------------------------------------------

