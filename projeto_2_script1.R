####Projeto 2#####

# CARREGAR PACOTES
library(dplyr)
library(rstatix)
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(plotly)) install.packages("plotly")
library(plotly)
getwd()




# ABRIR ARQUIVO
srag_sp <- read.csv('SRAG_2020.csv', sep = ";")
View(srag_sp)
summary(srag_sp)
unique(srag_sp$CS_SEXO)



# EXCLUIR COLUNAS
srag_sp_mod <- select(srag_sp, -c(51:133))
#aqui já filtra estado e país, indiretamente
srag_sp_mod_filtrado = srag_sp_mod %>% filter (SG_UF == 'SP')
srag_sp_mod_filtrado <- select(srag_sp_mod_filtrado, -c(5:8))
srag_sp_mod_filtrado <- select(srag_sp_mod_filtrado, -c(6,8))
View(srag_sp_mod_filtrado)
unique(srag_sp_mod$SG_UF)
unique(srag_sp_mod_filtrado$ID_PAIS)#aparece null porque país foi retirado

#como o r reconhece as variáveis
glimpse(srag_sp_mod_filtrado)

srag_sp_mod_filtrado$DT_NOTIFIC <- as.Date(srag_sp_mod_filtrado$DT_NOTIFIC, '%m/%d/%Y')


# Renomeando variáveis (colunas)
srag_sp_mod_filtrado <- rename(srag_sp_mod_filtrado, sexo = CS_SEXO, idade = NU_IDADE_N)
View(srag_sp_mod_filtrado)


# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(srag_sp_mod_filtrado, function(x) sum(is.na(x)))
sapply(srag_sp_mod_filtrado, function(x) sum(is.nan(x)))


#nativo do R
?graphics
library(help = "graphics")


# GRÁFICO DE BARRAS

# Contagem COLUNA sexo
#fazendo tabela
table(srag_sp_mod_filtrado$sexo)


#barplot(srag_sp_mod_filtrado$sexo,col="blue") #dá erro
#é preciso transformar em uma tabela para que o comando reconheça como argumento numérico
#jeito correto de fazer
grafico_barras=table(srag_sp_mod_filtrado$sexo)
barplot(grafico_barras, col="yellow", main="QUANTIDADE POR SEXO")

?barplot


# COM O GGPLOT2 (gráfico mais estruturado, mais 'bonitinho')
#aes=eixo
ggplot(srag_sp_mod_filtrado, aes(x = sexo)) +
  geom_bar(fill ='red')+ labs(title="Quantidade por sexo",
                              subtitle = "SRAG",
                              x = "Sexo", y = "Contagem")
?geom_bar()






# GRÁFICO POR RAÇA
sapply(srag_sp_mod_filtrado, function(x) sum(is.na(x)))
sapply(srag_sp_mod_filtrado, function(x) sum(is.nan(x)))
#recebe o valor 9 que corresponde a 'ignorado', conforme dicionário/documentação do BD
srag_sp_mod_filtrado$CS_RACA[which(is.na(srag_sp_mod_filtrado$CS_RACA))] <- 9
View(srag_sp_mod_filtrado)

#modificando os códigos por números
srag_sp_mod_filtrado$CS_RACA[srag_sp_mod_filtrado$CS_RACA == 1] <- "Branca"
srag_sp_mod_filtrado$CS_RACA[srag_sp_mod_filtrado$CS_RACA == 2] <- "Preta"
srag_sp_mod_filtrado$CS_RACA[srag_sp_mod_filtrado$CS_RACA == 3] <- "Amarela"
srag_sp_mod_filtrado$CS_RACA[srag_sp_mod_filtrado$CS_RACA == 4] <- "Parda"
srag_sp_mod_filtrado$CS_RACA[srag_sp_mod_filtrado$CS_RACA == 5] <- "Indígena"
srag_sp_mod_filtrado$CS_RACA[srag_sp_mod_filtrado$CS_RACA == 9] <- "Ignorado"

unique(srag_sp_mod_filtrado$CS_RACA)



# Contagem em uma tabelinha pelas alternativas da coluna
table(srag_sp_mod_filtrado$CS_RACA)

grafico_barras=table(srag_sp_mod_filtrado$CS_RACA)
barplot(grafico_barras, col="yellow", main="QUANTIDADE POR RAÇA")


# COM O GGPLOT2
ggplot(srag_sp_mod_filtrado, aes(x = CS_RACA)) +
  #geom_bar=gráfico de barras
  geom_bar(fill ='blue')+ labs(title="Quantidade por raça",
                               subtitle = "SRAG",
                               x = "Raça", y = "Contagem")


# GRÁFICO POR RAÇA, SEXO e REGIÃO

#verificar onde tem o valor 'na'
sapply(srag_sp_mod_filtrado, function(x) sum(is.na(x)))
unique(srag_sp_mod_filtrado$CS_ZONA)
srag_sp_mod_filtrado$CS_ZONA[which(is.na(srag_sp_mod_filtrado$CS_ZONA))] <- 9
unique(srag_sp_mod_filtrado$CS_ZONA)


srag_sp_mod_filtrado$CS_ZONA[srag_sp_mod_filtrado$CS_ZONA == 1] <- "Urbana"
srag_sp_mod_filtrado$CS_ZONA[srag_sp_mod_filtrado$CS_ZONA == 2] <- "Rural"
srag_sp_mod_filtrado$CS_ZONA[srag_sp_mod_filtrado$CS_ZONA == 3] <- "Periurbana"
srag_sp_mod_filtrado$CS_ZONA[srag_sp_mod_filtrado$CS_ZONA == 9] <- "Ignorado"
unique(srag_sp_mod_filtrado$CS_ZONA)


table(srag_sp_mod_filtrado$CS_ZONA)

ggplot(srag_sp_mod_filtrado, aes(x = CS_RACA, y = sexo, fill = factor(CS_ZONA))) +
  geom_col(position = "dodge") +
  labs(title = "Região por sexo e raça",
       x = "Raça",
       y = "Sexo",
       fill = "Região")


# Gráfico de barras na horizontal
ggplot(srag_sp_mod_filtrado, aes(x = CS_RACA, y = sexo, fill = factor(CS_ZONA))) +
  geom_col(position = "dodge") +
  labs(title = "Região por sexo e raça",
       x = "Raça",
       y = "Sexo",
       fill = "Região") +
  coord_flip()


# GRÁFICO DE BARRAS EMPILHAD0

grafico <- aggregate(idade ~ sexo + CS_ZONA, data=srag_sp_mod_filtrado, FUN=mean)

ggplot(grafico, aes(x = CS_ZONA, y = idade, fill = factor(sexo))) +
  geom_col(position = "stack")


?ggplot()

# GRÁFICO COM O PLOTLY
#é interativo, pois quando passamos o mouse no gráfico ele exibe os valores
srag_sp_mod_filtrado%>% plot_ly(x = ~ CS_RACA) %>%
  layout(xaxis = list(title = "Raça"),
         yaxis = list(title = "Quantidade")) 




# BOXPLOT PARA IDADE

# IDADE
#relacionado duas colunas, coluna 1 é onde será mudada, col 2 é a referência
#onde tp_idade for 2 ou 1 , idade receberá 0
srag_sp_mod_filtrado$idade[srag_sp_mod_filtrado$TP_IDADE == 2] <- 0
srag_sp_mod_filtrado$idade[srag_sp_mod_filtrado$TP_IDADE == 1] <- 0


summary(srag_sp_mod_filtrado$idade)
boxplot(srag_sp_mod_filtrado$idade)



srag_sp_mod_filtrado %>% identify_outliers(idade)
#retirando os outliers
outliers <- c(boxplot.stats(srag_sp_mod_filtrado$idade)$out)
#srag_atual=sem outliers
srag_atual <- srag_sp_mod_filtrado[-c(which(srag_sp_mod_filtrado$idade %in% outliers)),]
srag_atual %>% identify_outliers(idade)


summary(srag_atual$idade)
boxplot(srag_atual$idade)




# COM O GGPLOT2
srag_sp_mod_filtrado %>% filter(!is.na(idade)) %>% 
  ggplot(aes(x = " ", y = idade)) + 
  #width=largura da caixa
  geom_boxplot(width = .3, outlier.colour = "purple")


srag_atual %>% filter(!is.na(idade)) %>% 
  ggplot(aes(x = " ", y = idade)) + 
  geom_boxplot(width = .10, outlier.colour = "red")

?geom_boxplot()



# COM PLOTLY
#também é interativo ao passar o mouse
plot_ly(srag_sp_mod_filtrado, y = srag_sp_mod_filtrado$idade, 
        type = "box") %>%
  layout(title = "BOXPLOT POR IDADE",
         yaxis = list(title = "Idade"))



### BOXPLOT coletivo (mais de 1 ao mesmo tempo)
par(mfrow=c(1,2)) # Gráficos na mesma linha, tb serve para outros gráficos
boxplot(srag_atual$idade, ylab="idade sem outliers")
boxplot(srag_sp_mod_filtrado$idade, ylab="idade com outliers")


par(mfrow=c(1,1)) # Gráficos uma linha e duas colunas
boxplot(idade ~ sexo, srag_atual, ylab="Idade", xlab="Sexo")
boxplot(idade ~ CS_RACA, srag_atual, ylab="Idade", xlab="Raça")


par(mfrow=c(2,2)) # Gráficos duas linhas e duas colunas
#PODE DAR ERRO POR PRECISAR DE ESPAÇO PARA GERAR O GRÁFICO NO R
boxplot(idade ~ sexo, srag_atual, ylab="Idade", xlab="Sexo")
boxplot(idade ~ CS_RACA, srag_atual, ylab="Idade", xlab="Raça")
boxplot(srag_atual$idade, ylab="idade sem outliers")
boxplot(idade ~ CS_ZONA, srag_atual, ylab="Idade", xlab="Região")

par(mfrow=c(1,1)) # Único Gráfico

# COM GGPLOT2
ggplot(srag_atual, aes(x = factor(sexo), y = idade)) +
  geom_boxplot(fill = "dodgerblue") +
  labs(y = "Idade",
       x = "Sexo",
       title = "Distribuição das idades por sexo") 


# COM PLOTLY (se diz 'plot ly')
plot_ly(srag_atual, y = srag_atual$idade, color = srag_atual$sexo, 
        type = "box") %>%
  layout(title = "BOXPLOT POR IDADE",
         xaxis = list(title = "Sexo"), yaxis = list(title = "Idade"))


# HISTOGRAMA PARA IDADE

hist(srag_atual$idade, col="blue", main = "SRAG POR IDADE",
     xlab = "Distribuição das idades", ylab = "Frequência" )
#help nos comandos!!
?hist
?ggplot

hist(srag_atual$idade, probability=T, col="brown")
lines(density(srag_atual$idade) , col="orange")

summary(srag_atual$idade)


# Criando a função moda
moda <- function(m) {
  valor_unico <- unique(m) #Busca o valor único para a coluna valor
  valor_unico[which.max(tabulate(match(m, valor_unico)))] #tabular (contabilizar quantas vezes o valor único aparece) e buscar o maior valor
}
moda(srag_atual$idade)#0 ano


# QQPLOT (GRÁFICO DE DISTRIBUIÇÃO NORMAL)
qqnorm(srag_atual$idade, col = "gray")
qqline(srag_atual$idade, col= "red")



library(nortest)
# Teste de normalidade Shapiro-Wilk
#não rola esse teste,devido ao tamanho da amostra
shapiro.test(srag_atual$idade)

# Anderson-Darling
#A = 589.1, p-value < 2.2e-16  --> não é normal
ad.test(srag_atual$idade)


?geom_histogram
# COM O GGPLOT2
ggplot(data = srag_atual, aes(x=idade)) +
  #bin=intervalo das barras
  geom_histogram(fill ='red', bins = 25)+ labs(title="Histograma da idade",
                                               subtitle = "SRAG",
                                               x = "Idade", y = "Contagem")

# COM O PLOTY
plot_ly(x = srag_atual$idade, type = "histogram")%>%
  layout(title = "HISTOGRAMA POR IDADE",
         xaxis = list(title = "Idade"), yaxis = list(title = "Quantidade"))



# COM O GGPLOT2
ggplot(data = srag_atual, aes(x=idade)) +
  geom_histogram(fill ='red', bins = 25)+ labs(title="Histograma da idade",
                                               subtitle = "SRAG",
                                               x = "Idade", y = "Contagem")

# COM O PLOTY
plot_ly(x = srag_atual$idade, type = "histogram")%>%
  layout(title = "HISTOGRAMA POR IDADE",
         xaxis = list(title = "Idade"), yaxis = list(title = "Quantidade"))


# GRÁFICO DE DISPERSÃO


#rodar juntos
plot(srag_atual$DT_NOTIFIC, srag_atual$idade,
     title("Casos de SRAG por mês e por idade"), col = "purple")

scatter.smooth(srag_atual$DT_NOTIFIC, srag_atual$idade)


# COM O GGPLOT2 (2 variáveis)
#QUANTO MAIS VARIAVEIS, MAIS DEMORADO PARA GERAR O GRÁFICO
ggplot(srag_atual, aes(x = DT_NOTIFIC, y = idade)) +
  geom_point() +
  labs(title = "Relação data de notificação e idade",
       x = "Data de notificação",
       y = "Idade")


# COM O GGPLOT2 (4 variáveis)
#consulta mais restrita
srag_atual_camp <- srag_atual %>% filter(ID_MN_RESI=="CAMPINAS")
View(srag_atual_camp)



ggplot(srag_atual_camp, aes(x = DT_NOTIFIC, y = idade, 
                            color = CS_RACA, shape = sexo)) +
  geom_point() + 
  labs(title = "Relação entre data de notificação, idade e sexo",
       x = "Data de Notificação",
       y = "Idade")


# COM O PLOTLY
plot_ly(x=srag_atual_camp$DT_NOTIFIC,y=srag_atual_camp$idade,type='scatter',
        mode='markers', color = srag_atual_camp$sexo)




# GRÁFICO DE BOLHAS
srag_atual_tupa <- srag_atual %>% filter(ID_MN_RESI=="TUPA")
View(srag_atual_tupa)


ggplot(srag_atual_tupa, aes(x = DT_NOTIFIC, y = CS_ZONA, 
                            size = idade)) +
  geom_point() + 
  labs(title = "Relação entre data e região por idade",
       x = "Data de Notificação",
       y = "Região")


# COM O PLOTLY
plot_ly(x=srag_atual_camp$DT_NOTIFIC,y=srag_atual_camp$CS_ZONA,type='scatter',
        mode='markers', size = srag_atual_camp$idade)


# Gráfico de setores (pizza)

table(srag_atual_camp$sexo)
pie(table(srag_atual_camp$sexo),col=c("red","blue"), radius=2)



glimpse(srag_atual_camp)
srag_atual_camp$sexo <- as.factor(srag_atual_camp$sexo)


contagem <- table(srag_atual_camp$sexo)
nomes = levels(srag_atual_camp$sexo)
porcentagem = round(contagem/sum(contagem)*100, 2)
rotulo = paste(nomes," (",porcentagem,"%",")", sep=" ")
pie(table(srag_atual_camp$sexo),labels=rotulo, main="SEXO",
    col=c("red","blue"), radius = 2)



# COM O GGPLOT2
library(scales)
grafico_pizza <- ggplot(srag_atual_camp, aes(x=" ", fill=sexo))+
  geom_bar(width = 1)+
  coord_polar("y")
grafico_pizza+ theme(plot.background = element_rect(fill="gray", colour="red"))


#fazendo a tabelinha
table(srag_atual_camp$sexo)
grafico <- data.frame(
  grupo = c("Masculino", "Feminino"),
  valores = c(1311, 1041))
soma = sum(table(srag_atual_camp$sexo))

#fazendo o gráfico
grafico %>%
  ggplot(aes(x="", y=valores, fill=grupo)) +
  geom_col() +
  geom_text(aes(label = percent(valores/soma, accuracy = 0.1)), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds") +
  coord_polar("y") +
  theme_void() +
  labs(title = "QUANTIDADE POR SEXO",
       fill = "LEGENDA")


# COM PLOTLY
plot_ly(srag_atual_camp, labels = ~sexo, type = 'pie')
plot_ly(srag_atual_camp, labels = ~CS_RACA, type = 'pie')
plot_ly(srag_atual_camp, labels = ~CS_ZONA, type = 'pie')


### FREQUÊNCIAS ####

if(!require(sampling)) install.packages("sampling")
if(!require(TeachingSampling)) install.packages("TeachingSampling")
library(sampling)
library(TeachingSampling)

# Tabela de Frequências Absolutas
freq_abs <- table(srag_atual$idade) 
View(freq_abs)

# Tabela de Frequências Relativas
freq_rel <- prop.table(freq_abs) 
View(freq_rel)

# Porcentagem da frequência relativa
p_freq_rel <- 100 * prop.table(freq_rel) 
View(p_freq_rel)

# Criar uma linha com o total, no fim da tabela=totalizadores
freq_abs <- c(freq_abs, sum(freq_abs)) 
View(freq_abs)
names(freq_abs)[112] <- "Total"
View(freq_abs)

# Juntando a frequência relativa e a frequência percentual com suas respectivas somas.
freq_rel <- c(freq_rel, sum(freq_rel))
p_freq_rel <- c(p_freq_rel, sum(p_freq_rel))

# Tabela final com todos os valores
#cbind =junta os dados
tabela_final <- cbind(freq_abs, 
                      freq_rel = round(freq_rel, digits = 5), 
                      p_freq_rel = round(p_freq_rel, digits = 2))
View(tabela_final)


#CONSTRUINDO CLASSES DE FREQUÊNCIAS
intervalo_classes <- seq(0,120,10)
View(intervalo_classes)
#right=FALSE-> não considera o numero à direita
tabela_classes <- table(cut(srag_atual$idade, breaks=intervalo_classes, right=FALSE))
View(tabela_classes)





# GRÁFICOS DE FREQUÊNCIA

# HISTOGRAMA
hist(srag_atual$idade, col = "red")

#df=dataframe
df1 <- as.data.frame(tabela_classes)

df1 %>% plot_ly(x = ~Var1, y = ~Freq) %>%
  layout(xaxis = list(title = "Intervalo de idades"),
         yaxis = list(title = "Quantidade")) 

# Polígono de frequência
plot(tabela_classes,type='o')
?plot

# GRÁFICO DE OGIVA

# Frequência Acumulada
freq_rel_classes <- prop.table(table(cut(srag_atual$idade,
                                         breaks = c(intervalo_classes))))
View(freq_rel_classes)
freq_acum <- cumsum(tabela_classes)[seq_along(intervalo_classes)]
View(freq_acum)

# GRÁFICO
plot(intervalo_classes, freq_acum, type='o')


# GRÁFICO OGIVA NO GGPLOT
df <- as.data.frame(freq_acum)

ggplot(df, aes(x = intervalo_classes, y = freq_acum)) +
  geom_line() +
  geom_point() +
  labs(title = "GRÁFICO OGIVA: FREQUÊNCIA ACUMULADA POR CLASSES DE IDADE",
       x = "Idade",
       y = "Frequência Acumulada de SRAG",
       color = "Meses") +
  theme_classic()


