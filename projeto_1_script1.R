#####tratamento dos dados ####


#qdo instala, a mensagem aparece em vermelho!
if(!require(dplyr)) install.packages('dplyr')


#carregar
library(dplyr)

#buscar a pasta onde está o arquivo
setwd('C:/Users/adrianas/Desktop/Erre/curso R/dados-covid-sp-master/data')
getwd()

#abrir arquivo
#utf 8 no meu R não está obrigatório
covid_sp_ad = read.csv('dados_covid_sp.csv', sep =';', encoding = 'utf-8')
View(covid_sp_ad)
#mostra as 6 primeiras linhas
head(covid_sp_ad)
tail(covid_sp_ad)


#renomear colunas=variaveis=atributos(s sinonimos)
#evitar espaços e acentos
#municipio é o novo nome ,nome_munic , o antigo
covid_sp_ad = rename(covid_sp_ad, municipio =nome_munic)
#alterar mais de uma coluna 

covid_sp_ad <- rename(covid_sp_ad, data = datahora,
                            rotulo_mapa = map_leg,codigo_mapa = map_leg_s)
View(covid_sp_ad)

#excluir coluna por nome
covid_sp_ad$cod_ra = NULL
#excluir coluna por número
#o comando diz: covid_sp_ad recebe covid_sp_ad, exceto a coluna 21
covid_sp_ad = select(covid_sp_ad, -c(21))

#excluir várias colunas por nome
covid_sp_ad = subset(covid_sp_ad, select = -c(codigo_ibge,cod_drs))
View(covid_sp_ad)

# Excluir várias colunas por número 
covid_sp_alterado <- select(covid_sp_ad, -c(14,15))

covid_sp_alterado <- select(covid_sp_alterado, -c(17:19))

#excluir linha por numero
#ou colocar um intervalo de linhas x:y
covid_sp_alterado = slice(covid_sp_alterado, -c(239660))
covid_sp_alterado = slice(covid_sp_alterado, -c(239661:239666))

#excluir linhas por nome
#excluiu registros que tinha o municipio como ignorado
covid_sp_alterado = covid_sp_alterado %>% filter(municipio!='Ignorado')
View(covid_sp_alterado)
unique(covid_sp_alterado$municipio)


#VERIFICAR DADOS FALTANTES
#NA = valores ausentes
#NAN -NOT A NUMBER(Vvalores indefinidos)
#Percorre as colunas, verificando se há NA OU NAN
#soma quantas vezes na ou nan aparecem nas colunas do dataframe
sapply(covid_sp_alterado, function(x) sum(is.na(x)))
sapply(covid_sp_alterado, function(x) sum(is.nan(x)))

#Substituir valores missing

########### Função mutate all temporariamente desabilitada ######
#esse pacote não tem mais
# if(!require(tidyr)) install.packages("tidyr")
# library(tidyr)

# covid_sp_alterado2 <- covid_sp_alterado %>% mutate_all(replace_na, 54)
# View(covid_sp_alterado2)
###################################################################

### OPÇÃO:
covid_sp_alterado2 <- replace(x = covid_sp_alterado,list = is.na(covid_sp_alterado),
                              values = 54)

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$semana_epidem == 54] <- 2021

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '01/01/2021' &
                                   covid_sp_alterado2$data <= '07/01/2021'  ] <- 54

#AJUSTANDO DATAS,as referidas semanas
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '08/01/2021' &
                                   covid_sp_alterado2$data <= '14/01/2021'  ] <- 55


covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '15/01/2021' &
                                   covid_sp_alterado2$data <= '21/01/2021'  ] <- 56

#covid_sp_alterado2  tabela com valors NA alterados
#conferindo
sapply(covid_sp_alterado2, function(x) sum(is.na(x)))




#VERIFICAÇÃO DA TIPAGEM DOS ATRIBUTOS (Variáveis)
# EXISTEM 7 TIPOS BÁSICOS:
# character (caracteres)
# integer (números inteiros)
# numeric (números reais)
# logical (falso ou verdadeiro)
# complex (números complexos)
# factor (fator: Sequência de valores definidos por níveis)
# date (data)


#exibe como os dados estão estruturados na tabela, sua classificação
str(covid_sp_alterado2)
# OU
glimpse(covid_sp_alterado2)

#Transformação da tipagem de atributos
covid_sp_alterado2$semana_epidem <- as.integer(covid_sp_alterado2$semana_epidem)
glimpse(covid_sp_alterado2)

covid_sp_alterado2$data <- as.Date(covid_sp_alterado2$dates, '%Y/%m/%d/')
glimpse(covid_sp_alterado2)


# Alterar várias variáveis de uma única vez
#covid_sp_alterado2[1:17] <- lapply(covid_sp_alterado2[1:17], as.character)
#glimpse(covid_sp_alterado2)


# Criação de colunas
#nome da coluna idoso(%),sa coluna recebe 100xcoluna pop_60 % pela coluna pop
covid_sp_alterado2["idoso(%)"]<-100*covid_sp_alterado2$pop_60/covid_sp_alterado2$pop
View(covid_sp_alterado2)

#Exportação de arquivos
#salvar para não precisar rodar tudo de novo no R
write.table(covid_sp_alterado2, file ="covid_sp_tratado.txt", sep = ",")


