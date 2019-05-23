library(MASS);

#Esponente della potenza della funzione f
exp <- 5;
#non toccare, giusto così, dopo viene ricalcolata e tutto torna
#R usa dynamic scoping, serve solo per non avere problemi
c <- 1;
#Funzione di densità
f <- function(x){
    return(c*x^exp);
}

#1 Si determini c casuale X>0 in modo che f(x) si la funzione di densità di una variabile

#intervalli dove è definito il limite
inf <- 0;
max <- 1;
#calcolo di c
value <- integrate(f, inf, max, stop.on.error = FALSE)$value;
c <- 1/value;
c;

#Funzione per il calcolo del valore atteso, ossia integrale di x*f(x)!
xf <- function(x){
    return(x * f(x));
}

#2 Valor Medio della funzione di densità una volta determinata la c
speranza <- integrate(xf, inf, max, stop.on.error = FALSE)$value;
speranza
fractions(speranza);

#Funzione per la varianza
vf <- function(x){
    return((x - speranza)^2 * c * x^exp);
}

#3 Determinare la varianza di X
var <- integrate(vf, inf, max, stop.on.error = FALSE)$value;
var
fractions(var);

#4 Determinare la probabilità P (X ≤ 0.29)
sup <- 0.67; #estremo superiore dell'intervallo di probabilità
prob <- integrate(f, inf, sup, stop.on.error = FALSE)$value;
prob;

#1: c = 4
#2: 4/5 == 0.8
#3: 2/75 == 0.02666667
#4: (0.29)^4 ==  0.00707281
