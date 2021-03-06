# Modelo de regress�o m�ltipla no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# T�picos:
#   Regress�o m�ltipla
#   Significado dos coeficientes parciais de regress�o
#   Qualidade do ajuste (R-quadrado e R-quadrado ajustado)
#   Colinearidade perfeita 
#   Multicolinearidade usando FIV	
#   Vi�s de vari�vel omitida
#   Vari�ncia em modelos subespecificados	
#   Homocedasticidade e heterocedasticidade

# Arquivos de dados:
#   Sal�rio por hora2.csv

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


# Regress�o m�ltipla -----------------------------------------------------------

# Estime uma regress�o m�ltipla e interprete os coeficientes
# Observe como os coeficientes s�o diferentes para a regress�o simples e m�ltipla

# Exemplo do sal�rio por hora
sal�rio <- read.csv2(paste0(diret�rio, "Sal�rio por hora2.csv"))
sal�rio %>% 
  select(salhora, esc, exp, g�nero) %>% 
  head(10)
sal�rio %>% 
  select(salhora, esc, exp, g�nero) %>%
  str
sal�rio %>% 
  select(salhora, esc, exp, g�nero) %>%
  stargazer(type = "text")

# Compare os resultados de uma regress�o simples e de uma regress�o m�ltipla:
# Se a educa��o aumenta em 1 ano, em quantos d�lares aumentar� o sal�rio por hora? 

# Regress�o simples
modelo_simples <- lm(salhora ~ esc, sal�rio)
summary(modelo_simples)

# Regress�o m�ltipla
modelo_m�ltiplo1 <- lm(salhora ~ esc + exp, sal�rio)
modelo_m�ltiplo2 <- lm(salhora ~ esc + exp + g�nero, sal�rio)
summary(modelo_m�ltiplo1)
summary(modelo_m�ltiplo2)

# Mostrar apenas os coeficientes
coef(modelo_m�ltiplo2)
modelo_m�ltiplo2$coefficients

# Valores estimados e res�duos (assim como na regress�o simples)
sal�rio %<>% mutate(salhora_est = fitted(modelo_m�ltiplo2),
                    e_est = residuals(modelo_m�ltiplo2))

sal�rio %>% 
  select(salhora, salhora_est, e_est) %>% 
  head(10)

sal�rio %>% 
  select(salhora, salhora_est, e_est) %>%
  stargazer(type = "text")
# salhora = salhora_est + e_est
# mean(e_est) = 0 e mean(salhora) = mean(salhora_est)


# Significado dos coeficientes parciais de regress�o ---------------------------

# O coeficiente parcial de regress�o mostra o efeito l�quido da vari�vel a ele associada
# sobre a vari�vel dependente filtrada (descontada) a influ�ncia das demais vari�veis.

# salhora = b1 + b2*esc + b3*exp + b4*g�nero + e
# O mesmo modelo do 'modelo_m�ltiplo2'
summary(modelo_m�ltiplo2)

# esc = a1 + a2*exp + a3*g�nero + u
modelo_parcial <- lm(esc ~ exp + g�nero, sal�rio)
summary(modelo_parcial)

# Estimando os res�duos u_est
sal�rio %<>% mutate(u_est = resid(modelo_parcial))

# salhora = c0 + b1*u_est + v
lm(salhora ~ u_est, sal�rio) %>% summary


# Qualidade do ajuste (R-quadrado e R-quadrado ajustado) -----------------------

# A qualidade do ajuste � medida pelo R-quadrado e R-quadrado ajustado que mostram
# a propor��o da vari�ncia da vari�vel dependente explicada pela regress�o

# Estime uma regress�o simples
# a mesma do 'modelo_simples'
summary(modelo_simples)

# Mostre apenas o R-quadrado
summary(modelo_simples)$r.squared

# Mostre apenas o R-quadrado ajustado
summary(modelo_simples)$adj.r.squared

# Estime uma regress�o m�ltipla com 2 regressores
# a mesma do 'modelo_m�ltiplo1'
summary(modelo_m�ltiplo1)
summary(modelo_m�ltiplo1)$r.squared
summary(modelo_m�ltiplo1)$adj.r.squared

# Estime uma regress�o m�ltipla com 3 regressores
# a mesma do 'modelo_m�ltiplo2'
summary(modelo_m�ltiplo2)
summary(modelo_m�ltiplo2)$r.squared
summary(modelo_m�ltiplo2)$adj.r.squared

# Modelo lin-log
modelo_lin_log <- lm(salhora ~ log(esc) + exp + g�nero, sal�rio)
summary(modelo_lin_log)
summary(modelo_lin_log)$r.squared
summary(modelo_lin_log)$adj.r.squared

# Modelo log-linear ou log-log
modelo_log_log <- lm(log(salhora) ~ log(esc) + exp + g�nero, sal�rio) 
# n�o calculamos o logaritmo de vari�veis dummies e, nesse caso, da vari�vel exp, pois apresentam valores '0' zero
summary(modelo_log_log)
summary(modelo_log_log)$r.squared
summary(modelo_log_log)$adj.r.squared

# Modelo log-lin
modelo_log_lin <- lm(log(salhora) ~ esc + exp + g�nero, sal�rio)
summary(modelo_log_lin)
summary(modelo_log_lin)$r.squared
summary(modelo_log_lin)$adj.r.squared

# Compare os R-quadrado e os R-quadrado ajustados

# Podemos criar uma tabela para comparar os resultados
stargazer(list(modelo_simples, modelo_m�ltiplo1, modelo_m�ltiplo2, modelo_lin_log, modelo_log_lin, modelo_log_log), type = "text", keep.stat = c("rsq", "adj.rsq"))


# Colinearidade perfeita -------------------------------------------------------

# Colinearidade perfeita � uma rela��o linear perfeita entre vari�veis
# Um exemplo de colineraridade perfeita seria masculino = 1-g�nero (uma vez que a vari�vel g�nero � igual a 1 para feminino)

# Modelo de regress�o para sal�rio por hora com a vari�vel g�nero
modelo_sem_colinearidade1 <- lm(salhora ~ esc + exp + g�nero, sal�rio)
summary(modelo_sem_colinearidade1)

# A vari�vel masculino ser� uma combina��o linear perfeita de g�nero (colinearidade perfeita)
sal�rio %<>% mutate(masculino = 1 - g�nero)

# Modelo de regress�o para sal�rio por hora com a vari�vel masculino
modelo_sem_colinearidade2 <- lm(salhora ~ esc + exp + masculino, sal�rio)
summary(modelo_sem_colinearidade2)

# Tente estimar um modelo que tenha ambas as vari�veis g�nero e masculino  
modelo_colinear <- lm(salhora ~ esc + exp + masculino + g�nero, sal�rio)
summary(modelo_colinear)
# Esse modelo n�o pode ser estimado por causa da colinearidade perfeita
# O R n�o considera uma das vari�veis

# Uma solu��o para incluir ambas as vari�veis � estimar um modelo sem intercepto
model_sem_constante <- lm(salhora ~ 0 + esc + exp + masculino + g�nero, sal�rio) 
summary(model_sem_constante)


# Multicolinearidade usando FIV ------------------------------------------------

# A Multicolineraridade � caracterizada pela alta correla��o linear entre os regressores.

# No exemplo de sal�rio/hora, normalmente podemos esperar alta correla��o linear entre  
# escolaridade e experi�ncia

# Matriz de correla��o
sal�rio %>% 
  select(esc, exp, g�nero) %>% 
  cor

# Estime a regress�o e calcule o FIV. Se o FIV>10 ent�o temos um problema grave de multicolinearidade
(modelo_fiv <- lm(salhora ~ esc + exp + g�nero, sal�rio))
vif(modelo_fiv)


# Vi�s de vari�vel omitida -----------------------------------------------------

# O vi�s de vari�vel omitida ocorre quando deixamos de incluir uma vari�vel importante no modelo
# isso far� com os coeficientes calculados sejam viesados

# Modelo correto (verdadeiro) com escolaridade e experi�ncia
# salhora = b1 + b2*esc + b3*exp + e
modelo_correto <- lm(salhora ~ esc + exp, sal�rio)
summary(modelo_correto)
b2 <- coef(modelo_correto)["esc"]
b2
b3 <- coef(modelo_correto)["exp"]
b3

# Modelo entre experi�ncia e educa��o
# exp = d1 + d2*esc + v
modelo_exp <- lm(exp ~ esc, sal�rio)
summary(modelo_exp)
d2 <- coef(modelo_exp)["esc"]
d2

# Modelo onde omitimos a vari�vel experi�ncia, logo o coeficiente da escolaridade ser� viesado
# salhora = (b1+b3*d1) + (b2+b3*d2)*esc + (b3*v+e)
modelo_omitido <- lm(salhora ~ esc, sal�rio)
summary(modelo_omitido)
b2_viesado <- coef(modelo_omitido)["esc"]
b2_viesado

# C�lculo do vi�s e do coeficiente viesado
vi�s <- b3*d2
vi�s

b2_viesado_calculado <- b2 + b3*d2
b2_viesado_calculado


# Vari�ncia em modelos subespecificados ----------------------------------------

# Usaremos os mesmos modelos do exemplo do vi�s de vari�vel omitida 

# Modelo correto (verdadeiro) com escolaridade e experi�ncia
# salhora = b1 + b2*esc + b3*exp + e
# modelo_correto <- lm(salhora ~ esc + exp, sal�rio)
summary(modelo_correto)

# Modelo onde omitimos a vari�vel experi�ncia, logo o coeficiente da escolaridade ser� viesado
# modelo_omitido <- lm(salhora ~ esc, sal�rio)
summary(modelo_omitido)
# O coeficiente de esc � viesado mas possui um erro padr�o menor


# Homocedasticidade e heterocedasticidade --------------------------------------

# Homocedasticidade � a situa��o em que a vari�ncia do erro (res�duo) � constante em rela��o ao x
# Heterocedasticidade � a situa��o em que a vari�ncia do erro (res�duo) n�o � constante em rela��o ao x

# modelo_m�ltiplo2 <- lm(salhora ~ esc + exp + g�nero, sal�rio)
summary(modelo_m�ltiplo2)

# Gr�fico dos res�duos com "geom_point"
ggplot(data = sal�rio, mapping = aes(x = esc)) +
  theme_bw() +
  geom_point(mapping = aes(y = e_est)) +
  geom_hline(yintercept = 0, col = 'red') + # adiciona uma linha horizontal
  ylab(label = "Res�duos") # muda o t�tulo do eixo y

ggplot(data = sal�rio, mapping = aes(x = exp)) +
  theme_bw() +
  geom_point(mapping = aes(y = e_est)) +
  geom_hline(yintercept = 0, col = 'red') +
  ylab(label = "Res�duos")

# Os gr�ficos indicam heterocedasticidade para esc e homocedasticidade para exp

# Fim do Script ----------------------------------------------------------------
