#NOME ESMAEL RODRIGUES DA SILVA
#MATRICULA: 476203

#N 1�
#a) O percentual de pessoas ocupadas por sexo;
#b) O rendimento m�dio mensal por sexo;
#c) A distribui��o de pessoas desocupadas por faixa et�ria (14 a 17 anos, 18 a 24 anos, 25 a 39
#anos, 40 a 59 anos, 60 anos ou mais);
#d) Rendimento m�dio mensal por faixa et�ria.
#e)Compare os valores obtidos nos itens (a) - (d) com os dados referentes ao mesmo trimestre
#de 2020. Monte quadros comparativos e analise a evolu��o dos valores.

#Instalando os pacotes
install.packages("PNADcIBGE")
library(PNADcIBGE)
library(survey)
library(dplyr)
library(haven)
library(lmtest)
library(rgl)
library(faraway)

gc()
memory.limit (9999999999)
fit<-lm(Y ~ X)
gc()

#Fazendo a importa��o dos dados PNAD no site do IBGE
variaveis_selecionadasPNAD <- c("UF", "V2007", "V2009", "VD4001", "VD4002", "VD4020","VD4009","VD4016","VD4020")
dadosPNADc2019.4 <- get_pnadc(year = 2019, quarter = 4, vars = variaveis_selecionadasPNAD)
dados2019_4 <- dadosPNADc2019.4$variables

#ANO 2019 (4�TRIMESTRE)
#a)Percentual de pessoas ocupadas por sexo.
oc_sexo2019.4 <- svyby(~V2007, ~VD4002, dadosPNADc2019.4, svymean, na.rm = T)
oc_sexo2019.4

#b)Rendimento m�dio mensal por sexo.
rend_med_mensal_sexo2019.4 <- svyby(~VD4020, ~V2007, dadosPNADc2019.4, svymean, na.rm = T)
rend_med_mensal_sexo2019.4

#c) Distribui��o de pessoas desocupadas por faixa et�ria.
dadosPNADc2019.4 <-update(dadosPNADc2019.4, Idade = case_when(
  V2009 %in% 14:17 ~ "14-17",
  V2009 %in% 18:24 ~ "18-24", 
  V2009 %in% 25:39 ~ "25-39", 
  V2009 %in% 40:59 ~ "40-59", 
  V2009 > 59 ~ "60+")
)
desoc_idade2019.4 <- svyby(~Idade, ~VD4002, dadosPNADc2019.4, svymean, na.rm = T)
desoc_idade2019.4

#d) Rendimento m�dio mensal por faixa et�ria.
rend_med_mensal_idade2019.4 <- svyby(~VD4020, ~Idade, dadosPNADc2019.4, svymean, na.rm = T)
rend_med_mensal_idade2019.4


#2020 (4�TRIMESTRE)
variaveis_selecionadasPNADc2020 <- c("UF", "V2007", "V2009", "VD4001", "VD4002", "VD4020","VD4009","VD4016","VD4020")
dadosPNADc2020.4 <- get_pnadc(year = 2020, quarter = 4, vars = variaveis_selecionadasPNAD)
dados2020.4 <- dadosPNADc2020.4$variables

#a)Percentual de pessoas ocupadas por sexo.
oc_sexo2020.4 <- svyby(~V2007, ~VD4002, dadosPNADc2020.4, svymean, na.rm = T)
oc_sexo2020.4

#b)Rendimento m�dio mensal por sexo.
rend_med_mensal_sexo2020.4 <- svyby(~VD4020, ~V2007, dadosPNADc2020.4, svymean, na.rm = T)
rend_med_mensal_sexo2020.4

#c) Distribui��o de pessoas desocupadas por faixa et�ria.
dadosPNADc2020.4 <-update(dadosPNADc2020.4, Idade = case_when(
  V2009 %in% 14:17 ~ "14-17",
  V2009 %in% 18:24 ~ "18-24", 
  V2009 %in% 25:39 ~ "25-39", 
  V2009 %in% 40:59 ~ "40-59", 
  V2009 > 59 ~ "60+")
)
desoc_idade2020.4 <- svyby(~Idade, ~VD4002, dadosPNADc2020.4, svymean, na.rm = T)
desoc_idade2020.4

#d) Rendimento m�dio mensal por faixa et�ria.
rend_med_mensal_idade2020.4 <- svyby(~VD4020, ~Idade, dadosPNADc2020.4, svymean, na.rm = T)
rend_med_mensal_idade2020.4

#e)Compare os valores obtidos nos itens (a) - (d) do 4�trimestre 2019 e 4�trimestre 2020. 
#Monte quadros comparativos e analise a evolu��o dos valores.

quadro1<-dplyr::right_join(oc_sexo2019.4,oc_sexo2020.4, by = "VD4002")
quadro1

#a)Pode-se perceber que que a ocupa��o do sexo do 4�trimestre-2019, a mulher: 44,06% e o homem: 55,94%. J� no mesmo 
#trimestre de 2020, tem-se que a mulher possui uma ocupa��o de 43,50% e o homem: 56,50%. Comparando os dois anos, 
#percebe-se que 2020 teve maior ocupa��o para o homem e menor ocupa��o para mulher em rela��o a 2019.

quadro2<-dplyr::right_join(rend_med_mensal_sexo2019.4,rend_med_mensal_sexo2020.4, by = "V2007")
quadro2

#b)Em 2019,o rendimento m�dio mensal para o homem foi de R$2654,90 e para mulher R$2107,33. Em 2020, o rendimento
#m�dio mensal para homem foi R$2682,62 e para mulher R$2217,60, logo em 2020 possui maior rendimento medio mensal 
#para os dois sexos.

quadro3<-dplyr::right_join(desoc_idade2019.4,desoc_idade2020.4, by = "VD4002")
quadro3

#c)Em rela��o a desocupa��o por idade em 2019, a menor desocupa��o foi na faixa et�ria foi de igual ou maior de 60 anos: 
#2,90% e a maior foi na faixa et�ria de 25 � 39 anos: 34,78%. J� em 2020, tamb�m as mesmas faixas et�rias apresentaram
#menor e maior desocupa��o (igual ou > 60anos:2,82% e 25-39 anos: 35,26%)

quadro4<-dplyr::right_join(rend_med_mensal_idade2019.4,rend_med_mensal_idade2020.4, by = "Idade")   
quadro4

#d)Em 2019, a faixa et�ria que apresentou o menor rendimento m�dio mensal foi de 14-17 anos: R$: 554,84 e maior rendimento
#na faixa et�ria de igual ou >60 anos: 2929,50. J� em 2020, as mesmas faixas et�rias tamb�m mostraram o menor e o maior 
#rendimento m�dio mensal (14-17anos: R$626,20 e igual ou >60anos: R$2989,69). Vale ressaltar que os rendimentos de 2020 em
#valores reais s�o maiores que o de 2019.

#N 2�

#Obs: No caso das estat�sticas descritivas utilize as vari�veis: V2009, VD4020 e VD4035.
#Vari�veis:

#V2007  - Sexo
#V2009  - Idade do morador na data de refer�ncia
#V2010  - Cor ou ra�a
#VD3004 - N�vel de instru��o mais elevado alcan�ado (pessoas de 5 anos ou mais de idade)
#VD4020 - Rendimento mensal efetivo de todos os trabalhos para pessoas de 14 anos ou mais de idade
#(apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho)
#VD4035 - Horas efetivamente trabalhadas na semana de referência em todos os trabalhos para
#pessoas de 14 anos ou mais de idade

#Importando a base da PNADcCont�nua (4�trimestre-2019)
variaveis_selecionadasPNAD <- c("UF", "V2007", "V2009", "V2010", "VD3004", "VD4020", "VD4035")
dadosPNADc <- get_pnadc(year = 2019, quarter = 4, vars = variaveis_selecionadasPNAD2)
dados2019 <- dadosPNADc$variables

#Histograma
gc()
memory.limit(9999999999)
svyhist(~ as.numeric(V2007), dadosPNADc, main = "Histograma", xlab = "Sexo")
svyhist(~ as.numeric(V2009), dadosPNADc, main = "Histograma", xlab = "Idade do morador")
svyhist(~ as.numeric(V2010), dadosPNADc, main = "Histograma", xlab = "Cor ou Ra�a")
svyhist(~ as.numeric(VD3004), dadosPNADc, main = "Histograma", xlab = "N�vel de instru��o")
svyhist(~ as.numeric(VD4020), dadosPNADc, main = "Histograma", xlab = "Rendimento mensal efetivo - VD4020")
svyhist(~ as.numeric(VD4035), dadosPNADc, main = "Histograma", xlab = "N�mero de Horas trabalhadas")
gc()
#Gr�fico de Caixa

par(mfrow=c(2,2))
layout(matrix(c(1, 1,
                2, 3), nrow=2, byrow=TRUE))
svyboxplot(V2009 ~ V2007, dadosPNADc, main = "Idade do morador por sexo")
svyboxplot(V2009 ~ V2010, dadosPNADc, main = "Idade do morador por Cor ou Ra�a")
svyboxplot(V2009 ~ VD3004, dadosPNADc, main = "Idade do morador por N�vel de instru��o")

par(mfrow=c(2,2))
layout(matrix(c(1, 1,
                2, 3), nrow=2, byrow=TRUE))
svyboxplot(VD4020 ~ V2007, dadosPNADc, main = "Rendimento mensal efetivo por sexo")
svyboxplot(VD4020 ~ V2010, dadosPNADc, main = "Rendimento mensal efetivo por Cor ou Ra�a")
svyboxplot(VD4020 ~ VD3004, dadosPNADc, main = "Rendimento mensal efetivo por n�vel de instru��o")

par(mfrow=c(2,2))
layout(matrix(c(1, 1,
                2, 3), nrow=2, byrow=TRUE))
svyboxplot(VD4035 ~ V2007, dadosPNADc, main = "N�mero de Horas trabalhadas por sexo")
svyboxplot(VD4035 ~ V2010, dadosPNADc, main = "N�mero de Horas trabalhadas por Cor ou Ra�a")
svyboxplot(VD4035 ~ VD3004, dadosPNADc, main = "N�mero de Horas trabalhadas por n�vel de instru��o")


#Gr�fico de dispers�o

par(mfrow=c(2,2))
layout(matrix(c(1, 1,
                2, 3), nrow=2, byrow=TRUE))
svyplot(V2009 ~ V2007, dadosPNADc, style = "bubble", xlab = "Sexo", ylab = "Idade do morador")
svyplot(VD4020 ~ V2007, dadosPNADc, style = "bubble", xlab = "Sexo", ylab = "Rendimento mensal efetivo")
svyplot(VD4035 ~ V2007, dadosPNADc, style = "bubble", xlab = "Sexo", ylab = "N�mero de Horas trabalhadas")

par(mfrow=c(2,2))
layout(matrix(c(1, 1,
                2, 3), nrow=2, byrow=TRUE))
svyplot(VD4020 ~ V2010, dadosPNADc, style = "bubble", xlab = "Cor ou Ra�a", ylab = "Rendimento mensal efetivo")
svyplot(V2009 ~ V2010, dadosPNADc, style = "bubble", xlab = "Cor ou Ra�a", ylab = "Idade do morador")
svyplot(VD4035 ~ V2010, dadosPNADc, style = "bubble", xlab = "Cor ou Ra�a", ylab = "N�mero de Horas trabalhadas")

par(mfrow=c(2,2))
layout(matrix(c(1, 1,
                2, 3), nrow=2, byrow=TRUE))
svyplot(V2009 ~ VD3004, dadosPNADc, style = "bubble", xlab = "N�vel de Instru��o", ylab = "Idade do Morador")
svyplot(VD4035 ~ VD3004, dadosPNADc, style = "bubble", xlab = "N�vel de Instru��o", ylab = "N�mero de Horas trabalhadas")
svyplot(VD4020 ~ VD3004, dadosPNADc, style = "bubble", xlab = "N�vel de Instru��o", ylab = "Rendimento mensal efetivo")


# Estat�sticas descritivas
nomes<-c("M�dia","Mediana","M�ximo", "M�nimo")

#Idade do morador
media_idade_morador <- svymean(~V2009, dadosPNADc, na.rm = T)
mediana_idade_morador <- svyquantile(~V2009, dadosPNADc, quantiles = .5, na.rm = T)
max_idade <- max(dadosPNADc$variables$V2009, na.rm = TRUE)
min_idade <- min(dadosPNADc$variables$V2009, na.rm = TRUE)
estat_desc_idade_mor <- data.frame(media_idade_morador[1],mediana_idade_morador[1],max_idade,min_idade)
names(estat_desc_idade_mor) <- nomes
estat_desc_idade_mor

#Rendimento mensal
media_rend_mens <- svymean(~VD4020, dadosPNADc, na.rm = T)
mediana_rend_mens <- svyquantile(~VD4020, dadosPNADc, quantiles = .5, na.rm = T)
max_rend <- max(dadosPNADc$variables$VD4020, na.rm = TRUE)
min_rend <- min(dadosPNADc$variables$VD4020, na.rm = TRUE)
estat_desc_rend <- data.frame(media_rend_mens[1],mediana_rend_mens[1],max_rend,min_rend)
names(estat_desc_rend) <- nomes
estat_desc_rend

#Horas trabalhadas
media_horas <- svymean(~VD4035, dadosPNADc, na.rm = T)
mediana_horas <- svyquantile(~VD4035, dadosPNADc, quantiles = .5, na.rm = T)
max_horas <- max(dadosPNADc$variables$VD4035, na.rm = TRUE)
min_horas <- min(dadosPNADc$variables$VD4035, na.rm = TRUE)
estat_desc_horas <- data.frame(media_horas[1],mediana_horas[1],max_horas,min_horas)
names(estat_desc_horas) <- nomes
estat_desc_horas


#N 3� 
#(a) Uma vari�vel distribu�da como normal padr�o ser maior que 3.
1-pnorm(3)

#(b) Uma vari�vel normalmente distribu�da com m�dia 35 e desvio padr�o 6 ser maior que 42.
1-pnorm(42,mean=35,sd=6)

#(c) Obtendo 10 de 10 sucessos em uma distribui��o binomial com probabilidade de 0,8.
dbinom(x=10, size = 10,prob = 0.8)

#(d) X < 0,9 quando X tem a distribui��o uniforme padr�o.
punif(0.9)

#(e) X > 6,5 em uma distribui��o X^2 com 2 graus de liberdade.
pchisq(6.5, df = 2,lower.tail = FALSE)


#N 4� Para estimar uma regress�o linear m�ltipla no R devemos utilizar a fun��o lm
#Sendo assim, importe para o RStudio o arquivo nomeado cidade.dta (Stata) e responda os itens a
#seguir:

#Fixar Diret�rio, importar os dados e fixar a base
setwd("D:/Jessica/Ci�ncias Econ�micas UFC/4� SEMESTRE/M�TODOS COMPUTACIONAIS/Lista de Exerc�cios II_JessicaMariaSilvaVasconcelos")
cidades <- read_dta("base.cidades.dta")
attach(cidades)

#a) Plotar um gr�fico de dispers�o entre entre a taxa de div�rcio (div) e a taxa de matrim�mio (mat).
plot(div,mat,main = "Gr�fico de Dispers�o", xlab = "Taxa de Div�rcio", ylab = "Taxa de Matrin�mio")

#b)Verificar a correla��o entre a taxa de div�rcio (div) e a taxa de matrim�mio (mat).
cor(div,mat)

#c)Fa�a uma regress�o linear simples da taxa de div�rcio contra a taxa de matrim�nio e verifique
#a signific�ncia global, individual e o grau de ajustamento do modelo (R�).
reg_div_mat <- lm(div~mat)
summary(reg_div_mat)

#Conforme a regress�o linear simples, pode-se perceber que o modelo � globalmente significante, pois no teste F,o p-valor 
#foi de < 2.2e-16.
#Vale ressaltar que a vari�vel matrim�nio � significante, pois conforme o teste T, o p-valor foi de < 2.2e-16.
#O R� corresponde � 86,88%, ou seja, quer dizer que as varia��es da taxa do matrim�nio s�o explicadas em 86,88% nas varia��es
#da taxa de div�rcio. Percebe-se tamb�m que uma varia��o de uma unidade na taxa do matrim�nio promove 0,303 de varia��o na 
#taxa do div�rcio.

#d)Adicione ao modelo de regress�o linear simples acima as var�veis medpop (mediana da idade da popula��o) e a dummy indicativa 
#de regi�o e, em seguida, reinterprete os resultados.

cidades2 <- cidades %>%
  mutate(region = case_when(regiao == 1 ~ "A",regiao == 2 ~ "B", regiao == 3 ~ "C"))
attach(cidades2)
reg_multipla <- lm(div~mat+medpop+region)
summary(reg_multipla)

##e) Testar a presen�a de heterocedasticidade e multicolinearidade no modelo. Interprete o
#resultado dos testes.

#Teste de multicolinariedade e heterocedasticidade.
vif(reg_multipla)
bptest(reg_multipla)

#Todas as estat�sticas VIF foram menor que 5, logo n�o tem multicolinearidade e n�o apresenta heterocedasticidade.


#N 5� 

#ln(TC)=B1 + B2*ln(Q) + B3*ln(PL) + B4*ln(PF) + B5*ln(PK) + e

#Observa��o: Use o Rstudio para importar a base de dados. Em seguida criar as vari�veis em logaritmo
#e realizar a estima��o.
 
#Importando os dados e fixar base
nerlove <-read.csv2("D:/Jessica/Ci�ncias Econ�micas UFC/4� SEMESTRE/M�TODOS COMPUTACIONAIS/base de dados nerlove.csv")
nerlove <- nerlove[,1:5]
nerlove
attach(nerlove)

#Responda:  
#a) Interprete os resultados obtidos.
regm_nerlove <- lm(log(TC)~log(Q)+log(PL)+log(PF)+log(PK))
resultados<-c(summary(regm_nerlove))

#Analisando a regress�o m�ltipla percebe-se que somente as vari�veis Produto (Q) e Pre�o do Combust�vel (PF) foram 
#estatisticamente significantes. 
#Vale ressaltar que todas as outras vari�veis em ceteris paribus, uma varia��o de 1% na vari�vel Produto (Q) gera uma
#varia��o de 0,72091% na vari�vel Custo (TC).
#Vale ressaltar que todas as outras vari�veis em ceteris paribus, uma varia��o de 1% na vari�vel Pre�o do Combust�vel (PF) 
#gera uma varia��o de 0,42581% na vari�vel Custo (TC)

#b) O modelo � globalmente ajustado? Justifique.
#Analisando a regress�o m�ltipla, percebe-se que pelo test T, o p-valor foi < 2.2e-16, logo o modelo global � 
#estatisticamente  significante.
#Analisando o R�,as vari�veis independetes s�o explicadas em 92,6% nas varia��es da vari�vel Custo (CT).

#c) Use o pacote rgl para esbo�ar um Gr�fico 3D com as vari�veis PRE�O DO CAPITAL (PK),PRE�O DO TRABALHO (PL) e 
#PRE�O DO COMBUST�VEL (PF).
plot3d(PK,PL,PF)

#d) Analise a coer�ncia dos sinais dos estimadores.
#Analisando a regress�o, os sinais apresentados dos estimadores pelas vari�veis Produto (Q) e Pre�o do Combust�vel (PF) s�o
#adequados e esperados, visto que, que quanto maior o volume produzido ou unidades produzidas, maior ser� o custo da produ��o.
#Logo quanto maior o pre�o do combust�vel, maior ser� o custo da produ��o.