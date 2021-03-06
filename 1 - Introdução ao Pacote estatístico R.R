# Introdu��o ao Pacote estat�stico R

# Lindomar Pegorini Daniel - Universidade do Estado de Mato Grosso

# T�picos:
#   Ajustes gerais
#   Explorando dados
#   Editando dados
#   Estimando regress�es
#   Exportando os resultados das regress�es
#   Simula��o de Monte Carlo

# Arquivos de dados: 
#   Limonada.csv

# Ajustes gerais ---------------------------------------------------------------
rm(list = ls()) # limpar a �rea de trabalho, remove todos os dados

# O software R utiliza pacotes com comandos para realizar as an�lises desejadas

# Para atualizar pacotes j� instalados:
update.packages()

# Para instalar os pacotes utilizados nessa aula:
install.packages("tidyverse") # re�ne v�rios pacotes para manipula��o e visualiza��o de dados
install.packages("magrittr") # pacote que inclui os operadores "pipe" que s�o atalhos para os comandos
install.packages("stargazer") # pacote que inclui comandos para exportar tabelas formatadas dos resultados

# Carregar os pacotes instalados
library(tidyverse)
library(magrittr)
library(stargazer)

# Configurar uma pasta de trabalho
# Podemos criar um objeto com o endere�o onde est�o os arquivos de dados, aqui chamaremos de "diret�rio":
diret�rio <- "C:/Users/lindo/Google Drive/Projetos R/Econometria-I/"

# Ver e configurar a pasta de trabalho
# Podemos ver o endere�o da pasta onde o R est� salvando os arquivos:
getwd()
# De forma alternativa, podemos configurar outro endere�o
setwd("C:/Users/lindo/Google Drive/Projetos R/Econometria-I")
getwd()

# Leia o arquivo de dados de limonada ou outro arquivo de dados qualquer
# Caso n�o tenha configurado o endere�o onde se encontra o arquivo, utilize um dos seguintes comandos:
Limonada <- read.csv2("C:/Users/lindo/Google Drive/Projetos R/Econometria-I/Limonada.csv")
Limonada <- read.csv2(paste0(diret�rio, "Limonada.csv"))

# Como configuramos o endere�o do arquivo usaremos o seguinte comando
Limonada <- read.csv2("Limonada.csv")
View(Limonada) # ver a base de dados
attach(Limonada) # indica ao R que usaremos essa base de dados quando nos referirmos a uma base de dados

# � poss�vel ainda utilizar outros formatos de arquivo e usar o atalho "Import Dataset" na aba "Environment"


# Explorando os dados ----------------------------------------------------------
  
# Descrevendo a base de dados
str(Limonada) # ver a estrutura dos dados
str(select(Limonada, Temperatura, Chuva, Vendas))
Limonada %>% select(Temperatura, Chuva, Vendas) %>% str
# O operador "pipe" `%>%` (cano ou duto) � uma fun��o que atribui o objeto do lado esquerdo
# como o primeiro argumento da fun��o � direita. Para maiores detalhes, digite ?`%>%`.
# O comando "Limonada %>% select(Temperatura, Chuva, Vendas)" � equivalente a "select(Limonada, Temperatura, Chuva, Vendas)".

# Listando vari�veis na base de dados
names(Limonada)

# Mostre as primeiras linhas da base de dados
head(Limonada)
tail(Limonada)
Limonada[1:10,]
Limonada %>% select(Temperatura, Chuva, Vendas) %>% head(10)

# Estat�sticas descritivas

# M�dia:
mean(Vendas)
# Mediana:
median(Vendas)
# Desvio padr�o:
sd(Vendas)
# V�rias estat�sticas:
summary(Vendas)
Limonada %>% stargazer(type = "text")
Limonada %>% select(Temperatura, Chuva, Vendas) %>% stargazer(type = "text")

# Estat�sticas descritivas por grupo
table(Pre�o)
prop.table(table(Dia))
table(Dia, Pre�o)
Limonada %>% select(Dia) %>% table
Limonada %>% filter(Dia == "Domingo") %>% stargazer(type = "text")
split(Limonada, Limonada$Dia) %>% walk( ~ stargazer(., type = "text"))
# 'split' divide a base de dados de acordo com uma vari�vel categ�rica, nesse caso "Dia".
# 'walk' aplica o comando 'stargazer' para cada divis�o.
# '.' no comando stargazer aplica a fun��o para cada divis�o.

# Histograma e boxplot
hist(Vendas)
hist(Chuva)
boxplot(Vendas)
boxplot(Chuva)

# Correla��o entre vari�veis
cor(Vendas, Panfletos)
Limonada %>% select(Vendas, Chuva, Temperatura) %>% pairs

# Teste-t para teste de igualdade de m�dia
t.test(Vendas, mu=29.9) #No Laborat�rio 3 testamos a hip�tese da m�dia de vendas di�rias ser igual a 29,9 

# Teste  de igualdade de m�dia entre grupos
anova(lm(Vendas ~ factor(Pre�o)))
anova(lm(Vendas ~ factor(Dia)))


# Editando dados ---------------------------------------------------------------
# Apagar e manter vari�veis
Limonada %<>% select(Temperatura, Chuva, Panfletos, Pre�o, Vendas) # mant�m na base apenas as vari�veis selecionadas.
View(Limonada)
Limonada %<>% select(-Chuva) # o sinal de menos (-) indica que se quer apagar essa vari�vel.
Limonada %<>% filter(Pre�o < 2) # mant�m apenas as observa��es (dias) onde o pre�o � menor que 2.
# '%<>%' � uma variante da fun��o "pipe" que atribui o resultado do c�digo ao objeto do lado esquerdo. 
# "Limonada %<>% filter(Pre�o < 2)" � equivalente ao c�digo "Limonada <- filter(Limonada, Pre�o < 2)"

# Gerando novas vari�veis
Limonada %<>% mutate(logVendas = log(Vendas), Receita = Vendas*Pre�o)


# Estimando regress�es e exportando os resultados ------------------------------
# Usaremos o pacote 'stargazer' que j� foi carregado.

#Vamos carregar a base de dados novamente, uma vez que fizemos algumas altera��es
Limonada <- read.csv2("Limonada.csv")

# Estimando um modelo de regress�o linear simples
modelo.linear.1 <- lm(Vendas ~ Temperatura, Limonada) # as Vendas s�o regredidas em rela��o a uma constante e � vari�vel Temperatura.
summary(modelo.linear.1)

# Exportar os resultados para uma tabela
stargazer(modelo.linear.1, type = "text", out = "tabreg.doc")
# 'stargazer' produz arquivos html ou Word.
# O arquivo � salvo na pasta de trabalho do R, indicada anteriormente.

# Estimando um modelo de regress�o linear m�ltipla
modelo.linear.2 <- lm(Vendas ~ Temperatura + Panfletos + Pre�o, Limonada)
summary(modelo.linear.2)

# Exportar os resultados para uma tabela
stargazer(modelo.linear.1, modelo.linear.2,
          type = "text",
          out = "tabreg2.doc")

# Gr�ficos
plot (Vendas ~ Temperatura)
modelo.linear.1 <- lm(Vendas ~ Temperatura)
abline(modelo.linear.1)

# Redefinindo vari�veis 
Y <- cbind(Vendas)
X <- cbind(Temperatura, Panfletos, Pre�o)
summary(Y)
summary(X)
reg_vendas <- lm(Y ~ X)
summary(reg_vendas) 
stargazer(reg_vendas, type = "text")


# Simula��o de Monte Carlo -----------------------------------------------------
# Uma abordagem muito utilazada para estudar as propriedades dos estimadores s�o as simula��es de Monte Carlo

# Verificando a distribui��o do estimador da m�dia
# No nosso exemplo de Limonada observamos que o histograma da vari�vel Vendas parece ser normal
hist(Vendas)
mean(Vendas)
sd(Vendas)

# Vamos verificar agora se o estimador da m�dia tamb�m segue a distribui��o normal
# Pelo exerc�cio de distribui��o amostral que fizemos no excel � de se esperar que a resposta seja "sim"

# Escolhemos uma semente aleat�ria
set.seed(123456) # os n�meros gerados pelo R n�o s�o puramente aleat�rios, a semente serve para conseguirmos reproduzir os resultados

# Faremos 10000 amostras aleat�rias, portanto, vamos criar um vetor para armazenar os resultados

m�dia_Vendas <- numeric(10000)

# Agora, como fizemos no excel, vamos extrair 10 mil amostras aleat�rias de uma distribui��o normal, com 100 observa��es cada 
for (j in 1:10000) {
  # retiramos uma amostra aleat�ria, calculamos a m�dia e armazenamos na posi��o j=1,2... no vetor m�dia_Vendas
  amostra <- rnorm(100, 25.3,6.9)
  m�dia_Vendas[j] <- mean(amostra)
  }

# Vamos ver os resultados

# Vejamos as primeiras 20 m�dias estimadas das 10 mil
m�dia_Vendas[1:20]

# Vejamos a m�dia e o desvio padr�o simulados das 10 mil estimativas
mean(m�dia_Vendas)
sd(m�dia_Vendas) # o desvio padr�o do estimador deve ser pr�ximo �: desvpad/(raiz(n))

# Vejamos se a distribui��o do estimador da m�dia � parecido com a distribui��o normal
plot(density(m�dia_Vendas))
curve(dnorm(x,25.3,0.69), add=TRUE, lty=2)

# Fim do Script ----------------------------------------------------------------