library(MASS);

#dati
omegaR <- c(0.06, 0.06, 0.21, 0.29, 0.14, 0.24);
test <- sum(omegaR);
test == 1;

#numeri per il successo
s1 <- 1;
s2 <- 6;

#Quesiti

#1 Probabilità di successo
p <- omegaR[s1] + omegaR[s2];
q <- 1-p;

print("Successo: ");
p;
fractions(p);

#2 Probabilità di ottenere il primo successo esattamente all'iesimo lancio
i <- 5;
#R considera la funzione geometrica come p(1-p)^(x)
quesito2 <- sum(dgeom(x=(i-1), prob=p));
print("Successo all'iesimo lancio");
quesito2;
fractions(quesito2);

#3 Probabilità che siano necessari più di n lanci per ottenere il primo successo
n <- 5;
complementare <- 0;
index <- 1;
while(index <= n){
    complementare <- complementare + sum(dgeom(x=(index-1), prob=p));
    index <- index +1;
}
quesito3 <- 1 - complementare;
print("Necessari più di n lanci");
quesito3;
fractions(quesito3);

#4 Probabilità di X <= x
x <- 8.76;
quesito4 <- 0;
index <- 1;
while(index <= trunc(x)){
    quesito4 <- quesito4 + sum(dgeom(x=(index-1), prob=p));
    index <- index + 1;
}

print("Due metodi per calcolare la probabilità di X<=x");

quesito4;

#Secondo metodo:
quesito4_metodo2 <- 1 - (q)^trunc(x);
quesito4_metodo2;

quesito4 == quesito4_metodo2;
fractions(quesito4);


#1: 0.3 == 0.06 + 0.24
#2: 0.07203 == 0.30 * (0.70)^4
#3: 0.16807 == (0.70)^5
#4: 0.942352 == 1 - (0.70)^8