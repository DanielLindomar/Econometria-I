# Estrutura dos dados econômicos

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# Tópicos:
#   Dados de corte transversal
#   Dados de séries temporais
#   Dados em painel

# Arquivos de dados:
#   Salário po hora.csv
#   PIB real.csv
#   Investimento.csv

# Preparando
rm(list = ls()) # limpar a área de trabalho, remove todos os dados
diretório <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"


# Instalar e abrir pacotes
PackageNames <- c("tidyverse", "stargazer", "magrittr", "moments")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Dados de corte transversal ---------------------------------------------------

# Base de dados em corte transversal sobre o salário por hora e outras características
# A unidade de observação são os indivíduos

# Importar base de dados
salário <- read.csv2(paste0(diretório, "Salário por hora.csv"))
View(salário)

# Estatísticas descritivas
stargazer(salário, type = "text")

# Criando novas variáveis
salário %<>% 
  mutate(lnSalário=log(Salário), Escolaridade2=Escolaridade^2, Experiência2=Experiência^2)
  View(salário)
# Lembre-se de que o operador '%<>%' é uma variante da função "pipe" que atribui o resultado do código ao objeto do lado esquerdo. 

# Mantenha apenas variáveis selecionadas
salário %<>% select(Salário, Feminino, Não.branco, Sindicato, Escolaridade, Experiência, lnSalário, Escolaridade2, Experiência2)
View(salário)

# Estrutura dos dados
str(salário)
head(salário, 10)

# Tabelas de frequência
table(salário$Feminino)
prop.table(table(salário$Feminino))

# Gráficos
plot(salário$Salário ~ salário$Escolaridade)
modelo.linear.1 <- lm(salário$Salário ~ salário$Escolaridade)
abline(modelo.linear.1)


# Dados de séries temporais ----------------------------------------------------

# Base de dados do PIB real dos EUA (ajustado pela inflação) entre 1960 e 2007
# A unidade de observação é o tempo, nesse caso os anos

# Importar base de dados
PIB <- read.csv2(paste0(diretório, "PIB real.csv"))
View(PIB)

# Estrutura dos dados e estatístivas descritivas
str(PIB)
stargazer(PIB, type = "text")
table(PIB$ano)

# Gráficos
plot(PIB$PIB.real ~ PIB$ano)


# Dados em Painel --------------------------------------------------------------

# Base de dados de vendas de cerveja nos estados americanos entre 1985 e 2000
# A unidade de observação é o tempo, nesse caso os anos, e também o indivíduo, nesse caso, os estados

# Importar a base de dados
cerveja <- read.csv2(paste0(diretório, "Cerveja.csv"))
View(cerveja)

# Estrutura dos dados e estatísitcas descritivas
str(cerveja)
stargazer(cerveja, type = "text")
head(cerveja, 10)
table(cerveja$ano)

# Estatísticas descritivas para vendas e ano
cerveja %>% 
  select(vendas_cerveja, ano) %>%
  stargazer(type = "text")

# Gráficos
plot(cerveja$vendas_cerveja ~ cerveja$imposto_cerveja)
modelo.linear.2 <- lm(cerveja$vendas_cerveja ~ cerveja$imposto_cerveja)
abline(modelo.linear.2)

# Para um estado específico
cerveja %>% 
  filter(id_estado == 1) %>%
  select(vendas_cerveja, imposto_cerveja) %>% 
  plot(cerveja$vendas_cerveja ~ cerveja$imposto_cerveja)

# Fim do Script ----------------------------------------------------------------