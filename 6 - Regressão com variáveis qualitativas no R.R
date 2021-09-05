# Regressão com variáveis qualitativas no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# Tópicos:
#   Regressão simples com variável qualitativa
#   Interações com outra variável qualitativa
#   Interação com outras variáveis
#   Teste F para diferenças entre grupos
# 
# Arquivos de dados:
#   Salário por hora2.csv

# Preparando
rm(list = ls()) 
diretório <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "broom", "magrittr", "car", "dplyr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Regressão simples com variável qualitativa -----------------------------------

# Exemplo do salário por hora
salário <- read.csv2(paste0(diretório, "Salário por hora2.csv"))
salário %<>%
  rename(feminino = gênero, casado = civil, sul = região) 
salário %>% 
  select(salhora, esc, exp, feminino, casado, sul) %>% 
  head(10)
salário %>% 
  select(salhora, esc, exp, feminino, casado, sul) %>%
  str
salário %>% 
  select(salhora, esc, exp, feminino, casado, sul) %>%
  stargazer(type = "text")

# Salário por hora médio
lm(salhora ~ 1, salário) %>% summary
salário %>% select(salhora) %>% stargazer(type = "text")

# Regressão do salário por hora em relação a feminino
lm(salhora ~ feminino, salário) %>% summary

# Gráfico do salário em relação a feminino
ggplot(salário, aes(x = feminino, y = salhora)) +
  theme_bw() +
  geom_point() +
  geom_smooth(aes(col = 'linha de regressão'), method = "lm", se = F)

# Estatísticas descritivas para os grupos feminino e masculino
salário %>% 
  select(salhora) %>% 
  split(salário$feminino) %>%  # divide os dados com base na variável feminino
  walk(~ stargazer(., type = "text")) # calcula estatísticas descritivas para cada grupo

# Teste t para diferença de médias de salário entre homens e mulheres
t.test(formula = salhora ~ feminino, data = salário)
# A média, a estatística t, e o valor-p são os mesmos do coeficiente de feminino 
# na regressão e no teste t que compara os salário de homens e mulheres.

# Criando uma variável dummy para masculino
salário %<>% mutate(masculino = 1 - feminino)

# Regressão do salário/hora em relação a feminino e em relação a masculino
lm(salhora ~ feminino, salário) %>% summary
lm(salhora ~ masculino, salário) %>% summary
# O coeficiente de masculino tem a mesma magnitude e significância 
# mas tem o sinal oposto da variável feminino.

# Regressão com masculino e feminino não pode ser estimada devido a colinearidade
lm(salhora ~ feminino + masculino, salário) %>% summary

# Regressão com masculino e feminino pode ser estimada sem a constante
lm(salhora ~ 0 + feminino + masculino, salário) %>% summary


# Interações com outra variável qualitativa ------------------------------------

# Regressão com termos de interação
modelo_1 <- lm(salhora ~ feminino*casado, salário)
summary(modelo_1)

# Efeito marginal de solteiro (casado=0) e feminino=1 sobre o salário médio
coef(modelo_1)['feminino']

# Efeito marginal de casado=1 e feminino=1 sobre o salário médio
coef(modelo_1)['feminino'] + coef(modelo_1)['casado'] + 
  coef(modelo_1)['feminino:casado']


# Interação com outras variáveis

# Podem ser criadas interações entre variáveis qualitativas e variáveis contínuas

# Regressão de salhora sobre esc 
# Modelo com o mesmo intercepto e inclinação para feminino e masculino
modelo_2 <- lm(salhora ~ esc, salário) 
summary(modelo_2)

ggplot(salário, aes(x = esc)) + 
  theme_bw() + 
  geom_point(aes(y = salhora)) +
  geom_smooth(aes(y = salhora), method = lm, se = F)

# Regressão de salhora sobre esc e feminino
# Modelo com a mesma inclinação para masculino e feminino mas com inteceptos diferentes
modelo_3 <- lm(salhora ~ esc + feminino, salário)
summary(modelo_3)
salário %<>% mutate(salhora_est = fitted(modelo_3))

# Gráfico do salário por hora em relação à escolaridade, mesma inclinação
# para masculino e feminino mas com inteceptos diferentes

# Converta "feminino" em um fator(categórico) para fazer o gráfico
salário %<>%
  mutate(gênero = factor(feminino, levels = 1:0, labels = c('feminino', 'masculino')))

ggplot(data = salário, mapping = aes(x = esc, col = gênero)) + 
  theme_bw() + 
  geom_point(aes(y = salhora)) + 
  geom_line(aes(y = salhora_est)) +
  guides(color = guide_legend(title = 'gênero'))

# Regressão de salhora sobre esc, feminino e uma interação feminino*esc
# Modelo com inclinação e interceptos diferentes para masculino e feminino
modelo_4 <- lm(salhora ~ esc*feminino, salário)
summary(modelo_4)

# Interceptos e inclinações para feminino e masculino
b <- coef(modelo_4)
print(paste0("o intercepto para masculino é ", b['(Intercept)']))
print(paste0("o intercepto para feminino é ", b['(Intercept)'] + b['feminino']))
print(paste0("o efeito marginal da educação sobre o salário por hora para homens é ", b['esc']))
print(paste0("o efeito marginal da educação sobre o salário por hora para mulheres é ", 
             b['esc'] + b['esc:feminino']))


# Teste F para diferenças entre grupos -----------------------------------------

# Podemos usar o teste F de significância geral para testar várias hipóteses sobre  
# a significância conjunta dos coeficientes.

# Como exemplo, podemos estimar o seguinte modelo para detemrinar o salário por hora:
# salhora = b1 + b2esc + b3exp + b4feminino + b5feminino*esc + b6feminino*exp +e
modelo_5 <- lm(salhora ~ (esc + exp)*feminino, salário)
summary(modelo_5)

# Podemos testar, por exemplo, se os coeficientes da váriáveis feminino e das interações com escolaridade e experiência
# são significativas em conjunto
linearHypothesis(modelo_5, c('feminino        = 0', 
                             'esc:feminino    = 0',
                             'exp:feminino    = 0'))

# Podemos testar se apenas os coeficientes das interações com escolaridade e experiência são significativos em conjunto
linearHypothesis(modelo_5, c('esc:feminino    = 0',
                             'exp:feminino    = 0'))

# Fim do Script ----------------------------------------------------------------

