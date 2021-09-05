# Introdução ao Pacote estatístico R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# Tópicos:
#   Ajustes gerais
#   Explorando dados
#   Editando dados
#   Estimando regressões
#   Exportando os resultados das regressões
#   Simulação de Monte Carlo

# Arquivos de dados: 
#   Limonada.csv

# Ajustes gerais ---------------------------------------------------------------
rm(list = ls()) # limpar a área de trabalho, remove todos os dados

# O software R utiliza pacotes com comandos para realizar as análises desejadas

# Para atualizar pacotes já instalados:
update.packages()

# Para instalar os pacotes utilizados nessa aula:
install.packages("tidyverse") # reúne vários pacotes para manipulação e visualização de dados
install.packages("magrittr") # pacote que inclui os operadores "pipe" que são atalhos para os comandos
install.packages("stargazer") # pacote que inclui comandos para exportar tabelas formatadas dos resultados

# Carregar os pacotes instalados
library(tidyverse)
library(magrittr)
library(stargazer)

# Configurar uma pasta de trabalho
# Podemos criar um objeto com o endereço onde estão os arquivos de dados, aqui chamaremos de "diretório":
diretório <- "C:/Users/lindo/OneDrive/Documentos/Arquivos R/"

# Ver e configurar a pasta de trabalho
# Podemos ver o endereço da pasta onde o R está salvando os arquivos:
getwd()
# De forma alternativa, podemos configurar outro endereço
setwd("C:/Users/lindo/OneDrive/Documentos/Arquivos R")
getwd()

# Leia o arquivo de dados de limonada ou outro arquivo de dados qualquer
# Caso não tenha configurado o endereço onde se encontra o arquivo, utilize um dos seguintes comandos:
Limonada <- read.csv2("C:/Users/lindo/OneDrive/Documentos/Arquivos R/Limonada.csv")
Limonada <- read.csv2(paste0(diretório, "Limonada.csv"))

# Como configuramos o endereço do arquivo usaremos o seguinte comando
Limonada <- read.csv2("Limonada.csv")
View(Limonada) # ver a base de dados
attach(Limonada) # indica ao R que usaremos essa base de dados quando nos referirmos a uma base de dados

# É possível ainda utilizar outros formatos de arquivo e usar o atalho "Import Dataset" na aba "Environment"


# Explorando os dados ----------------------------------------------------------
  
# Descrevendo a base de dados
str(Limonada) # ver a estrutura dos dados
str(select(Limonada, Temperatura, Chuva, Vendas))
Limonada %>% select(Temperatura, Chuva, Vendas) %>% str
# O operador "pipe" `%>%` (cano ou duto) é uma função que atribui o objeto do lado esquerdo
# como o primeiro argumento da função à direita. Para maiores detalhes, digite ?`%>%`.
# O comando "Limonada %>% select(Temperatura, Chuva, Vendas)" é equivalente a "select(Limonada, Temperatura, Chuva, Vendas)".

# Listando variáveis na base de dados
names(Limonada)

# Mostre as primeiras linhas da base de dados
head(Limonada)
tail(Limonada)
Limonada[1:10,]
Limonada %>% select(Temperatura, Chuva, Vendas) %>% head(10)

# Estatísticas descritivas

# Média:
mean(Vendas)
# Mediana:
median(Vendas)
# Desvio padrão:
sd(Vendas)
# Várias estatísticas:
summary(Vendas)
Limonada %>% stargazer(type = "text")
Limonada %>% select(Temperatura, Chuva, Vendas) %>% stargazer(type = "text")

# Estatísticas descritivas por grupo
table(Preço)
prop.table(table(Dia))
table(Dia, Preço)
Limonada %>% select(Dia) %>% table
Limonada %>% filter(Dia == "Domingo") %>% stargazer(type = "text")
split(Limonada, Limonada$Dia) %>% walk( ~ stargazer(., type = "text"))
# 'split' divide a base de dados de acordo com uma variável categórica, nesse caso "Dia".
# 'walk' aplica o comando 'stargazer' para cada divisão.
# '.' no comando stargazer aplica a função para cada divisão.

# Histograma e boxplot
hist(Vendas)
hist(Chuva)
boxplot(Vendas)
boxplot(Chuva)

# Correlação entre variáveis
cor(Vendas, Panfletos)
Limonada %>% select(Vendas, Chuva, Temperatura) %>% pairs

# Teste-t para teste de igualdade de média
t.test(Vendas, mu=29.9) #No Laboratório 3 testamos a hipótese da média de vendas diárias ser igual a 29,9 

# Teste  de igualdade de média entre grupos
anova(lm(Vendas ~ factor(Preço)))
anova(lm(Vendas ~ factor(Dia)))


# Editando dados ---------------------------------------------------------------
# Apagar e manter variáveis
Limonada %<>% select(Temperatura, Chuva, Panfletos, Preço, Vendas) # mantém na base apenas as variáveis selecionadas.
View(Limonada)
Limonada %<>% select(-Chuva) # o sinal de menos (-) indica que se quer apagar essa variável.
Limonada %<>% filter(Preço < 2) # mantém apenas as observações (dias) onde o preço é menor que 2.
# '%<>%' é uma variante da função "pipe" que atribui o resultado do código ao objeto do lado esquerdo. 
# "Limonada %<>% filter(Preço < 2)" é equivalente ao código "Limonada <- filter(Limonada, Preço < 2)"

# Gerando novas variáveis
Limonada %<>% mutate(logVendas = log(Vendas), Receita = Vendas*Preço)


# Estimando regressões e exportando os resultados ------------------------------
# Usaremos o pacote 'stargazer' que já foi carregado.

#Vamos carregar a base de dados novamente, uma vez que fizemos algumas alterações
Limonada <- read.csv2("Limonada.csv")

# Estimando um modelo de regressão linear simples
modelo.linear.1 <- lm(Vendas ~ Temperatura, Limonada) # as Vendas são regredidas em relação a uma constante e à variável Temperatura.
summary(modelo.linear.1)

# Exportar os resultados para uma tabela
stargazer(modelo.linear.1, type = "text", out = "tabreg.doc")
# 'stargazer' produz arquivos html ou Word.
# O arquivo é salvo na pasta de trabalho do R, indicada anteriormente.

# Estimando um modelo de regressão linear múltipla
modelo.linear.2 <- lm(Vendas ~ Temperatura + Panfletos + Preço, Limonada)
summary(modelo.linear.2)

# Exportar os resultados para uma tabela
stargazer(modelo.linear.1, modelo.linear.2,
          type = "text",
          out = "tabreg2.doc")

# Gráficos
plot (Vendas ~ Temperatura)
modelo.linear.1 <- lm(Vendas ~ Temperatura)
abline(modelo.linear.1)

# Redefinindo variáveis 
Y <- cbind(Vendas)
X <- cbind(Temperatura, Panfletos, Preço)
summary(Y)
summary(X)
reg_vendas <- lm(Y ~ X)
summary(reg_vendas) 
stargazer(reg_vendas, type = "text")


# Simulação de Monte Carlo -----------------------------------------------------
# Uma abordagem muito utilazada para estudar as propriedades dos estimadores são as simulações de Monte Carlo

# Verificando a distribuição do estimador da média
# No nosso exemplo de Limonada observamos que o histograma da variável Vendas parece ser normal
hist(Vendas)
mean(Vendas)
sd(Vendas)

# Vamos verificar agora se o estimador da média também segue a distribuição normal
# Pelo exercício de distribuição amostral que fizemos no excel é de se esperar que a resposta seja "sim"

# Escolhemos uma semente aleatória
set.seed(123456) # os números gerados pelo R não são puramente aleatórios, a semente serve para conseguirmos reproduzir os resultados

# Faremos 10000 amostras aleatórias, portanto, vamos criar um vetor para armazenar os resultados

média_Vendas <- numeric(10000)

# Agora, como fizemos no excel, vamos extrair 10 mil amostras aleatórias de uma distribuição normal, com 100 observações cada 
for (j in 1:10000) {
  # retiramos uma amostra aleatória, calculamos a média e armazenamos na posição j=1,2... no vetor média_Vendas
  amostra <- rnorm(100, 25.3,6.9)
  média_Vendas[j] <- mean(amostra)
  }

# Vamos ver os resultados

# Vejamos as primeiras 20 médias estimadas das 10 mil
média_Vendas[1:20]

# Vejamos a média e o desvio padrão simulados das 10 mil estimativas
mean(média_Vendas)
sd(média_Vendas) # o desvio padrão do estimador deve ser próximo à: desvpad/(raiz(n))

# Vejamos se a distribuição do estimador da média é parecido com a distribuição normal
plot(density(média_Vendas))
curve(dnorm(x,25.3,0.69), add=TRUE, lty=2)

# Fim do Script ----------------------------------------------------------------