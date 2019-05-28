library(MASS)

#Siano date n variabili aleatorie di distribuzione esponenziale indipendenti stocasticamente e identicamente distribuite
campione1 <- c(0.67, 0.09, 0.09, 0.78, 0.72, 1.89, 0.98, 0.02)
campione2 <- c(0.00, 0.17, 0.34, 0.07, 0.06, 0.07, 0.10, 0.21)

#Semplice stimatore media aritmetica
stimatore_media_aritmetica <- function(campione){
    media <- 0
    for(i in 1:length(campione)){
        media <- media + campione[i]
    }
    media <- media/length(campione)
    return(media)
}

#Si presume X sia una variabile aleatoria di tipo distribuzione esponenziale ed inoltre Z e Y siano variabili aleatorie di tipo combinazione lineare di X
#Tali per cui:
#Y = costante * X
#Z = X + costante

#Costante moltiplicativa di Y
costanteMoltiplicativaY <- 3
#Costante additiva di Z
costanteAdditivaZ <- -6

#Si ha grazie al metodo della verosimiglianza
Y <- function(campione){
    return((costanteMoltiplicativaY)/stimatore_media_aritmetica(campione))
}

Z <- function(campione){
    return(1/(stimatore_media_aritmetica(campione) + abs(costanteAdditivaZ))) 
}

#Dopo i calcoli della verosimiglianza
stimaY <- Y(campione1)
cat("La stima di λ con lo stimatore Y = ", costanteMoltiplicativaY, "X vale: ", stimaY, "\n")
fractions(stimaY)

#Per quanto riguarda il secondo esercizio della verosimiglianza si ha
stimaZ <- Z(campione2)
cat("La stima di λ con lo stimatore Z = X + (", costanteAdditivaZ, ") vale: ", stimaZ, "\n")
fractions(stimaZ)

#RISPOSTE:
#1: 4.580153
#2: 0.1631987