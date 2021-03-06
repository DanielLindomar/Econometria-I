# Estrutura dos dados econ�micos

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# T�picos:
#   Dados de corte transversal
#   Dados de s�ries temporais
#   Dados em painel

# Arquivos de dados:
#   Sal�rio po hora.csv
#   PIB real.csv
#   Investimento.csv

# Preparando
rm(list = ls()) # limpar a �rea de trabalho, remove todos os dados
diret�rio <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"


# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr", "moments")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Dados de corte transversal ---------------------------------------------------

# Base de dados em corte transversal sobre o sal�rio por hora e outras caracter�sticas
# A unidade de observa��o s�o os indiv�duos

# Importar base de dados
sal�rio <- read.csv2(paste0(diret�rio, "Sal�rio por hora.csv"))
View(sal�rio)

# Estat�sticas descritivas
stargazer(sal�rio, type = "text")

# Criando novas vari�veis
sal�rio %<>% 
  mutate(lnSal�rio=log(Sal�rio), Escolaridade2=Escolaridade^2, Experi�ncia2=Experi�ncia^2)
  View(sal�rio)
# Lembre-se de que o operador '%<>%' � uma variante da fun��o "pipe" que atribui o resultado do c�digo ao objeto do lado esquerdo. 

# Mantenha apenas vari�veis selecionadas
sal�rio %<>% select(Sal�rio, Feminino, N�o.branco, Sindicato, Escolaridade, Experi�ncia, lnSal�rio, Escolaridade2, Experi�ncia2)
View(sal�rio)

# Estrutura dos dados
str(sal�rio)
head(sal�rio, 10)

# Tabelas de frequ�ncia
table(sal�rio$Feminino)
prop.table(table(sal�rio$Feminino))

# Gr�ficos
plot(sal�rio$Sal�rio ~ sal�rio$Escolaridade)
modelo.linear.1 <- lm(sal�rio$Sal�rio ~ sal�rio$Escolaridade)
abline(modelo.linear.1)


# Dados de s�ries temporais ----------------------------------------------------

# Base de dados do PIB real dos EUA (ajustado pela infla��o) entre 1960 e 2007
# A unidade de observa��o � o tempo, nesse caso os anos

# Importar base de dados
PIB <- read.csv2(paste0(diret�rio, "PIB real.csv"))
View(PIB)

# Estrutura dos dados e estat�stivas descritivas
str(PIB)
stargazer(PIB, type = "text")
table(PIB$ano)

# Gr�ficos
plot(PIB$PIB.real ~ PIB$ano)


# Dados em Painel --------------------------------------------------------------

# Base de dados de vendas de cerveja nos estados americanos entre 1985 e 2000
# A unidade de observa��o � o tempo, nesse caso os anos, e tamb�m o indiv�duo, nesse caso, os estados

# Importar a base de dados
cerveja <- read.csv2(paste0(diret�rio, "Cerveja.csv"))
View(cerveja)

# Estrutura dos dados e estat�sitcas descritivas
str(cerveja)
stargazer(cerveja, type = "text")
head(cerveja, 10)
table(cerveja$ano)

# Estat�sticas descritivas para vendas e ano
cerveja %>% 
  select(vendas_cerveja, ano) %>%
  stargazer(type = "text")

# Gr�ficos
plot(cerveja$vendas_cerveja ~ cerveja$imposto_cerveja)
modelo.linear.2 <- lm(cerveja$vendas_cerveja ~ cerveja$imposto_cerveja)
abline(modelo.linear.2)

# Para um estado espec�fico
cerveja %>% 
  filter(id_estado == 1) %>%
  select(vendas_cerveja, imposto_cerveja) %>% 
  plot(cerveja$vendas_cerveja ~ cerveja$imposto_cerveja)

# Fim do Script ----------------------------------------------------------------