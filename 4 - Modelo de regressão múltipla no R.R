# Modelo de regressão múltipla no R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# Tópicos:
#   Regressão múltipla
#   Significado dos coeficientes parciais de regressão
#   Qualidade do ajuste (R-quadrado e R-quadrado ajustado)
#   Colinearidade perfeita 
#   Multicolinearidade usando FIV	
#   Viés de variável omitida
#   Variância em modelos subespecificados	
#   Homocedasticidade e heterocedasticidade

# Arquivos de dados:
#   Salário por hora2.csv

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


# Regressão múltipla -----------------------------------------------------------

# Estime uma regressão múltipla e interprete os coeficientes
# Observe como os coeficientes são diferentes para a regressão simples e múltipla

# Exemplo do salário por hora
salário <- read.csv2(paste0(diretório, "Salário por hora2.csv"))
salário %>% 
  select(salhora, esc, exp, gênero) %>% 
  head(10)
salário %>% 
  select(salhora, esc, exp, gênero) %>%
  str
salário %>% 
  select(salhora, esc, exp, gênero) %>%
  stargazer(type = "text")

# Compare os resultados de uma regressão simples e de uma regressão múltipla:
# Se a educação aumenta em 1 ano, em quantos dólares aumentará o salário por hora? 

# Regressão simples
modelo_simples <- lm(salhora ~ esc, salário)
summary(modelo_simples)

# Regressão múltipla
modelo_múltiplo1 <- lm(salhora ~ esc + exp, salário)
modelo_múltiplo2 <- lm(salhora ~ esc + exp + gênero, salário)
summary(modelo_múltiplo1)
summary(modelo_múltiplo2)

# Mostrar apenas os coeficientes
coef(modelo_múltiplo2)
modelo_múltiplo2$coefficients

# Valores estimados e resíduos (assim como na regressão simples)
salário %<>% mutate(salhora_est = fitted(modelo_múltiplo2),
                    e_est = residuals(modelo_múltiplo2))

salário %>% 
  select(salhora, salhora_est, e_est) %>% 
  head(10)

salário %>% 
  select(salhora, salhora_est, e_est) %>%
  stargazer(type = "text")
# salhora = salhora_est + e_est
# mean(e_est) = 0 e mean(salhora) = mean(salhora_est)


# Significado dos coeficientes parciais de regressão ---------------------------

# O coeficiente parcial de regressão mostra o efeito líquido da variável a ele associada
# sobre a variável dependente filtrada (descontada) a influência das demais variáveis.

# salhora = b1 + b2*esc + b3*exp + b4*gênero + e
# O mesmo modelo do 'modelo_múltiplo2'
summary(modelo_múltiplo2)

# esc = a1 + a2*exp + a3*gênero + u
modelo_parcial <- lm(esc ~ exp + gênero, salário)
summary(modelo_parcial)

# Estimando os resíduos u_est
salário %<>% mutate(u_est = resid(modelo_parcial))

# salhora = c0 + b1*u_est + v
lm(salhora ~ u_est, salário) %>% summary


# Qualidade do ajuste (R-quadrado e R-quadrado ajustado) -----------------------

# A qualidade do ajuste é medida pelo R-quadrado e R-quadrado ajustado que mostram
# a proporção da variância da variável dependente explicada pela regressão

# Estime uma regressão simples
# a mesma do 'modelo_simples'
summary(modelo_simples)

# Mostre apenas o R-quadrado
summary(modelo_simples)$r.squared

# Mostre apenas o R-quadrado ajustado
summary(modelo_simples)$adj.r.squared

# Estime uma regressão múltipla com 2 regressores
# a mesma do 'modelo_múltiplo1'
summary(modelo_múltiplo1)
summary(modelo_múltiplo1)$r.squared
summary(modelo_múltiplo1)$adj.r.squared

# Estime uma regressão múltipla com 3 regressores
# a mesma do 'modelo_múltiplo2'
summary(modelo_múltiplo2)
summary(modelo_múltiplo2)$r.squared
summary(modelo_múltiplo2)$adj.r.squared

# Modelo lin-log
modelo_lin_log <- lm(salhora ~ log(esc) + exp + gênero, salário)
summary(modelo_lin_log)
summary(modelo_lin_log)$r.squared
summary(modelo_lin_log)$adj.r.squared

# Modelo log-linear ou log-log
modelo_log_log <- lm(log(salhora) ~ log(esc) + exp + gênero, salário) 
# não calculamos o logaritmo de variáveis dummies e, nesse caso, da variável exp, pois apresentam valores '0' zero
summary(modelo_log_log)
summary(modelo_log_log)$r.squared
summary(modelo_log_log)$adj.r.squared

# Modelo log-lin
modelo_log_lin <- lm(log(salhora) ~ esc + exp + gênero, salário)
summary(modelo_log_lin)
summary(modelo_log_lin)$r.squared
summary(modelo_log_lin)$adj.r.squared

# Compare os R-quadrado e os R-quadrado ajustados

# Podemos criar uma tabela para comparar os resultados
stargazer(list(modelo_simples, modelo_múltiplo1, modelo_múltiplo2, modelo_lin_log, modelo_log_lin, modelo_log_log), type = "text", keep.stat = c("rsq", "adj.rsq"))


# Colinearidade perfeita -------------------------------------------------------

# Colinearidade perfeita é uma relação linear perfeita entre variáveis
# Um exemplo de colineraridade perfeita seria masculino = 1-gênero (uma vez que a variável gênero é igual a 1 para feminino)

# Modelo de regressão para salário por hora com a variável gênero
modelo_sem_colinearidade1 <- lm(salhora ~ esc + exp + gênero, salário)
summary(modelo_sem_colinearidade1)

# A variável masculino será uma combinação linear perfeita de gênero (colinearidade perfeita)
salário %<>% mutate(masculino = 1 - gênero)

# Modelo de regressão para salário por hora com a variável masculino
modelo_sem_colinearidade2 <- lm(salhora ~ esc + exp + masculino, salário)
summary(modelo_sem_colinearidade2)

# Tente estimar um modelo que tenha ambas as variáveis gênero e masculino  
modelo_colinear <- lm(salhora ~ esc + exp + masculino + gênero, salário)
summary(modelo_colinear)
# Esse modelo não pode ser estimado por causa da colinearidade perfeita
# O R não considera uma das variáveis

# Uma solução para incluir ambas as variáveis é estimar um modelo sem intercepto
model_sem_constante <- lm(salhora ~ 0 + esc + exp + masculino + gênero, salário) 
summary(model_sem_constante)


# Multicolinearidade usando FIV ------------------------------------------------

# A Multicolineraridade é caracterizada pela alta correlação linear entre os regressores.

# No exemplo de salário/hora, normalmente podemos esperar alta correlação linear entre  
# escolaridade e experiência

# Matriz de correlação
salário %>% 
  select(esc, exp, gênero) %>% 
  cor

# Estime a regressão e calcule o FIV. Se o FIV>10 então temos um problema grave de multicolinearidade
(modelo_fiv <- lm(salhora ~ esc + exp + gênero, salário))
vif(modelo_fiv)


# Viés de variável omitida -----------------------------------------------------

# O viés de variável omitida ocorre quando deixamos de incluir uma variável importante no modelo
# isso fará com os coeficientes calculados sejam viesados

# Modelo correto (verdadeiro) com escolaridade e experiência
# salhora = b1 + b2*esc + b3*exp + e
modelo_correto <- lm(salhora ~ esc + exp, salário)
summary(modelo_correto)
b2 <- coef(modelo_correto)["esc"]
b2
b3 <- coef(modelo_correto)["exp"]
b3

# Modelo entre experiência e educação
# exp = d1 + d2*esc + v
modelo_exp <- lm(exp ~ esc, salário)
summary(modelo_exp)
d2 <- coef(modelo_exp)["esc"]
d2

# Modelo onde omitimos a variável experiência, logo o coeficiente da escolaridade será viesado
# salhora = (b1+b3*d1) + (b2+b3*d2)*esc + (b3*v+e)
modelo_omitido <- lm(salhora ~ esc, salário)
summary(modelo_omitido)
b2_viesado <- coef(modelo_omitido)["esc"]
b2_viesado

# Cálculo do viés e do coeficiente viesado
viés <- b3*d2
viés

b2_viesado_calculado <- b2 + b3*d2
b2_viesado_calculado


# Variância em modelos subespecificados ----------------------------------------

# Usaremos os mesmos modelos do exemplo do viés de variável omitida 

# Modelo correto (verdadeiro) com escolaridade e experiência
# salhora = b1 + b2*esc + b3*exp + e
# modelo_correto <- lm(salhora ~ esc + exp, salário)
summary(modelo_correto)

# Modelo onde omitimos a variável experiência, logo o coeficiente da escolaridade será viesado
# modelo_omitido <- lm(salhora ~ esc, salário)
summary(modelo_omitido)
# O coeficiente de esc é viesado mas possui um erro padrão menor


# Homocedasticidade e heterocedasticidade --------------------------------------

# Homocedasticidade é a situação em que a variância do erro (resíduo) é constante em relação ao x
# Heterocedasticidade é a situação em que a variância do erro (resíduo) não é constante em relação ao x

# modelo_múltiplo2 <- lm(salhora ~ esc + exp + gênero, salário)
summary(modelo_múltiplo2)

# Gráfico dos resíduos com "geom_point"
ggplot(data = salário, mapping = aes(x = esc)) +
  theme_bw() +
  geom_point(mapping = aes(y = e_est)) +
  geom_hline(yintercept = 0, col = 'red') + # adiciona uma linha horizontal
  ylab(label = "Resíduos") # muda o título do eixo y

ggplot(data = salário, mapping = aes(x = exp)) +
  theme_bw() +
  geom_point(mapping = aes(y = e_est)) +
  geom_hline(yintercept = 0, col = 'red') +
  ylab(label = "Resíduos")

# Os gráficos indicam heterocedasticidade para esc e homocedasticidade para exp

# Fim do Script ----------------------------------------------------------------
