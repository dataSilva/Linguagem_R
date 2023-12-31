#########################################
###   ESTRUTURA DOS DADOS - VETORES   ###
#########################################

# Sequ�ncia de valores num�ricos ou caracteres

vetor <- c(1,2,3,4,5,6,7)
class(vetor)

dias <- c("segunda", "ter�a", "quarta", "quinta", "sexta", "s�bado", "domingo")
class(dias)

juntando <- c(vetor, dias)
juntando
class(juntando)

gastos_dia <- c(400, 300, 100, 500, 150, 430, 70)
gastos_dia
class(gastos_dia)
length(gastos_dia)

ordem_crescente <- sort(gastos_dia)
ordem_crescente

total <- sum(gastos_dia) # soma dos valores do vetor
total

minimo <- min(gastos_dia)
min(gastos_dia)

max(gastos_dia)
maximo <- max(gastos_dia)

media <- mean(gastos_dia)
mean(gastos_dia)

limite <- (gastos_dia <= 300)
limite

intervalo <- (3:8)
intervalo

passo <- seq(2,48,by=5)
passo

repeticao <- rep(2,8)
repeticao

repeticao_multipla <- rep(c(3,5),c(4,6))
repeticao_multipla

repeticao_programada <- rep(3:5, each = 3)
repeticao_programada

repeticao_programada_2 <- rep(3:6,3)
repeticao_programada_2

vetor2 <- c(2,4,6,8,10,12)
vetor3 <- c(vetor2,14) # incluindo registro num vetor
vetor3
class(vetor3)
vetor3 <- as.integer(vetor3)

vetor4 <- c(vetor3,"pares")
vetor4
class(vetor4)

posicao <- vetor3[5]
posicao
vetor3[4]

posicao_inexistente <- vetor3[8]
posicao_inexistente

posicao_excluida <- vetor3[-3]
posicao_excluida

posicao_excluida <- vetor3[-5]
posicao_excluida









