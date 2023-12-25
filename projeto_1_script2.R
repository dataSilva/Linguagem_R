####exploração e análise de dados
#carregar pacotes caso já não  o estejam

library(dplyr)
if(!require(rstatix)) install.packages('rstatix')
library(rstatix)

#abrir arquivo
covid_sp_tratado = read.csv2('covid_sp_tratado.csv', sep=";", encoding='utf-8')
View(covid_sp_tratado)

#tabela configurada, conforme foi salvo
covid_sp_tratado = read.csv2('covid_sp_tratado.csv', sep=",", encoding='utf-8')
View(covid_sp_tratado)

glimpse(covid_sp_tratado)


#arrumando a data
covid_sp_tratado$data = as.Date(covid_sp_tratado$data, '%Y-%m-%d')
glimpse(covid_sp_tratado)

#o erre não reconhece o () do nome idoso(%)
covid_sp_tratado$idoso... = as.numeric(covid_sp_tratado$idoso...)
glimpse(covid_sp_tratado)

#renomeando para um nome aceitável
#não colocar () pois o R entende como parâmetro
covid_sp_tratado =  rename(covid_sp_tratado, porcentagem_idoso = idoso...)

glimpse(covid_sp_tratado)
covid_sp_tratado$porcentagem_idoso = as.numeric(covid_sp_tratado$porcentagem_idoso)
glimpse(covid_sp_tratado)



#filtrar por linha (cidade)
#campinas
covid_campinas = covid_sp_tratado %>% filter(municipio =='Campinas')
View(covid_campinas)

#criando coluna
#não aceitou
#covid_campinas["dens_demografica"] = covid_campinas%pop/covid_campinas$area
covid_campinas["dens_demografica"] = 1175501/794.57

View(covid_campinas)
glimpse(covid_campinas)

#ajustando dado da coluna
covid_campinas["area"] = covid_campinas$area/100
#outro tipo de filtro; , e espaço == quer dizer que está pegando utdp
covid_guarulhos = covid_sp_tratado[which(covid_sp_tratado$municipio=="Guarulhos"), ]
View(covid_guarulhos)                                 
                     
#serve para criar e atribuir valores ao mesmo tempo
covid_guarulhos["area"] =  covid_guarulhos$area/100
covid_guarulhos["dens_demografica"] = covid_guarulhos$pop/covid_guarulhos$area
View(covid_guarulhos)



##análises estatísticas

#medidas de centralidade
#média
#começar a usar o rstatix
mean(covid_campinas$casos_novos)
mean(covid_guarulhos$casos_novos)
#resume a subtabela
summarise_at(covid_campinas, vars(obitos_novos, casos_novos),mean)

#média movel
#casos com relação à data
plot(covid_campinas$data, covid_campinas$casos_mm7d, title('média móvel'),col='red')
plot(covid_campinas$data, covid_campinas$obitos_mm7d, title('média móvel'),col='purple')

#mediana
median(covid_campinas$obitos_novos)
median(covid_campinas$casos_novos)
#faz uma tabelinha resumo
summarise_at(covid_campinas, vars(obitos_novos, casos_novos),median)


#moda
#criando uma função, pq n tm pronta
moda = function(m){
  valor_unico = unique(m)#busca valores únicos na coluna
  #pega apenas o valor máximo
  valor_unico[which.max(tabulate(match(m,valor_unico)))]#tabular, contabiliza quantas vezes o valor único aparece
}
moda

#Obtenção da moda
#os valores que mais repetem são zero
moda(covid_campinas$obitos_novos)
moda(covid_campinas$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), moda)



#Abordando outra parte dos dados do subdataframe covid_campinas
#filtra os dados onde a coluna mês=7
covid_julho_campinas = covid_campinas%>%filter(mes==7)
#ver como a moda já mudou para esse mês
moda(covid_julho_campinas$obitos_novos)
moda(covid_julho_campinas$casos_novos)
summarise_at(covid_julho_campinas, vars(obitos_novos,casos_novos),moda)

mean(covid_julho_campinas$obitos_novos)
mean(covid_julho_campinas$casos_novos)


#histograma
#acho que o border padrão é o black
hist(covid_julho_campinas$obitos_novos,border='white',col='blue')
hist(covid_julho_campinas$obitos_novos,col='orange')
hist(covid_julho_campinas$casos_novos,col='red')

hist(covid_campinas$obitos_novos, col ='blue')
hist(covid_campinas$casos_novos, col ='red')

hist(covid_guarulhos$obitos_novos,border='white', col ='green')
hist(covid_guarulhos$casos_novos,border='white',col ='yellow')


#medidas de posição
View(covid_guarulhos)
min(covid_campinas$obitos_novos)
min(covid_campinas$casos_novos)
max(covid_campinas$obitos_novos)
summarise_at(covid_campinas, vars(obitos_novos,casos_novos),min)
summarise_at(covid_campinas, vars(obitos_novos,casos_novos),max)

#amplitude total
#retorna o min e max
range(covid_campinas$obitos_novos)
summarise_at(covid_campinas, vars(obitos_novos,casos_novos),range)


#Quartis
quantile(covid_campinas$obitos_novos)
summarize_at(covid_campinas, vars(obitos_novos,casos_novos), quantile)


#Amplitude interquantil
#distância entre Q1 ao q3
IQR(covid_campinas$obitos_novos)
IQR(covid_campinas$casos_novos)
summarize_at(covid_campinas, vars(obitos_novos,casos_novos), IQR)


IQR(covid_guarulhos$obitos_novos)
IQR(covid_guarulhos$casos_novos)


summary(covid_campinas$obitos_novos)
summary(covid_campinas$casos_novos)

summary(covid_guarulhos$obitos_novos)
summary(covid_guarulhos$casos_novos)

#bloxplot
summary(covid_julho_campinas$obitos_novos)
boxplot(covid_julho_campinas$obitos_novos)

boxplot(covid_julho_campinas$casos_novos)
summary(covid_campinas$casos_novos)
boxplot(covid_campinas$casos_novos)

#tratando outliers

#identificando e excluindo todos os outliers
#%>% relaciona a função ao dataframe
covid_guarulhos %>% identify_outliers(casos_novos)
#retirando o outlier
outliers = c(boxplot.stats(covid_guarulhos$casos_novos)$out)
outliers #410 em casos novos
#excluir todas as linhas com outliers
covid_guarulhos_sem_outliers = covid_guarulhos[-c(which(covid_guarulhos$casos_novos %in% outliers )), ]
boxplot(covid_guarulhos_sem_outliers$casos_novos)


#outliers em campinas
#há 3
covid_campinas%>% identify_outliers(casos_novos)
#tira apenas 1 dos outliers
covid_campinas_sem_outliers = covid_campinas %>% filter(data != '2020-06-19')
boxplot(covid_campinas_sem_outliers$casos_novos)


#Medidas de dispersão
summary(covid_campinas)
summary(covid_campinas$letalidade)

#variância
#distância com relação à média
var(covid_campinas$obitos_novos)#24849.39
var(covid_campinas$casos_novos)

var(covid_julho_campinas$obitos_novos)#24432.18
var(covid_julho_campinas$casos_novos)


#guarulhos está mais disperso
var(covid_guarulhos$obitos_novos)#49.10336
var(covid_guarulhos$casos_novos)

#desvio padrão
sd(covid_campinas$obitos_novos)
sd(covid_campinas$casos_novos)
#qto mais dispersos,menor a confiança


#Testes de normalidade
#Existem 4 testes de normalidade principais(numericos)e dois testes gráficos
#shapiro-wilk(limite de 5000 amostras)
#anderson-darling
#Kolmogorov-smirnov
#cramer-von mises
#histograma
#QQplot


#testes estatísticos de normalidade
#nível de significância de 0,05(5%) ou nível de confiança de 95%(Mais utilizado)
#Quando o parâmetro p >0.05 (distribuição normal)
if(!require(nortest)) install.packages(('nortest'))
library(nortest)


#Histograma
#resultado é assimétrico à direita(positiva), não é uma normal
hist(covid_campinas$casos_novos, probability = T,col='blue')
#coloca uma linha no histograma
lines(density(covid_campinas$casos_novos), col='red')

#QQplot
#Para ser uma normal, dados (bolinhas) deveriam acompanhar a linha
qqnorm(covid_campinas$casos_novos)
qqline(covid_campinas$casos_novos)

#Shapiro-Wilk
#da para aplicar nos subdataframes
shapiro.test(covid_campinas$casos_novos)#p-value < 2.2e-16
#dá um numero muito menor que 0,05, não é uma normal

#Anderson-Darling
#dá o mesmo p-value do teste anterior
ad.test(covid_campinas$casos_novos)

#Kolmogorov-Smirnov

ks.test(covid_campinas$casos_novos,pnorm)#esse não é mais usado,não funciona
lillie.test(covid_campinas$casos_novos)#esse é o novo

#Cramer-Von Mises
cvm.test(covid_campinas$casos_novos)
#avisa que o p value é muito peqno=distr.não normal



#CORRELAÇÃO LINEAR
#Method:"pearson" para dados paramétricos(normalidade e homocedasticidade)
#'spearman' (volume grande de dados não paramétricos--dados não normais)
#'kendall' (volume pequeno de dados não paramétricos)
#'
#'#sabendo conforme testes anteriores que covid_campinas não é normal/paramétrico
plot(covid_campinas$casos, covid_campinas$obitos)
#há uma correlação linear positiva entre casos e obitos que já é exibida pelo plot
cor(covid_campinas$casos, covid_campinas$obitos, method='spearm')
#valor 0.9996747= correlação bem próxima a 1

#para conseguir montar a equação da reta
#lm=linear model
regressao = lm(formula = obitos ~ casos, data = covid_campinas)#modelo de regressão
regressao$coefficients
summary(regressao)

#(Intercept)       casos 
#51.66957500  0.03371981 
#são o y e o x
#Equação: óbitos = 51.67 + 0,037 x casos
#coeficiente ajustado/coeficiente de determinação:0,9832==fala da precisão,da acurácia do modelo
#aparecee no summary(regressao)
##Gráfico de linha com ajuste de reta com GGPlot2


#GRÁFICO DE LINHA COM AJUSTE DE RETA COM GGPLOT2
if(!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
#para mostrar a equação no gráfico
if(!require(ggpubr)) install.packages('ggpubr')#equação da reta no gráfico
library(ggpubr)

#colocar os parametros x e y e seus correspondentes
#lm=linear model
ggplot(data= covid_campinas,mapping =aes(x=casos, y=obitos)) +
  geom_point() +
  geom_smooth(method = 'lm', col='red') +
  stat_regline_equation(aes(label=paste(..eq.label..,..adj.rr.label..,
                                        #configs de impressão da equação
                                        sep ='*plain(\',           \')~~')), label.x =15000,
                        label.y=1800)+
  theme_gray()#fundo

#MATRIZ DE CORRELAÇÃO
if(!require(corrplot)) install.packages('corrplot')
library(corrplot) #grafico de correlação

#Verificar se as colunas/dados estão relacionados entre si
#entre colunas 5 a 13
matriz_corr = cor(covid_campinas[5:13], method ='spearman')
View(matriz_corr) #tabela


#Vai apresentar a correlação de modo mais visual:
#Quanto mais escuro o azul,maior correlação
corrplot(matriz_corr, method='color')
corrplot(matriz_corr,method ='color',
         type='full', order='original',
         addCoef.col ='black',#adiciona o coeficiente à matriz
         tl.col='black', tls.srt=45,#cor e rotação do nome às variáveis
         )


#GRÁFICOS LINEARES POR CIDADES
covid_cidades = covid_sp_tratado %>% filter (municipio %in% c ("Campinas", "Guarulhos"))
View(covid_cidades)

#cria duas linhas que servem de comparação entre as cidades

ggplot(covid_cidades, aes(x =casos, y=obitos, color =municipio)) +
  geom_line()+
  labs(title ="Evolução dos óbitos em função dos casos de Covid",
       x='Casos', y='Óbitos', color='meses') +
  theme_classic()





#Exportação de arquivos
write.table(covid_sp_alterado2, file ="covid_sp_tratado.txt", sep = ",")


# Opção de exportação de arquivos
install.packages("readr", dependencies = TRUE)
library("readr")
write_delim(covid_sp_alterado2, "covid_sp_tratado.csv", delim = ",")












