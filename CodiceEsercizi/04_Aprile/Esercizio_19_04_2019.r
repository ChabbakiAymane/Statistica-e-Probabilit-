options(digits=8)

#Dati
mu <- 26;
o_2 <- 43.1649
o <-  sqrt(o_2);

#Definiamo la corrispettiva funzione di densità:
normale <- function(x, mu, o) {
    return((1/sqrt(2 * pi * o^2)) * exp(-1/2 * ((x-mu)^2)/(o^2)));
}

X <- function(x){
    return(normale(x, mu, o));
}

#1: Qual è il valore standardizzato corrispondente a 31.5845
	x <- 31.5845;
	stdx <- (x - mu)/o;
	stdx;

#2: Calcolare P(X ≤ 31.5845) attraverso standardizzazione.
#Pr({X ≤ c}) = Pr((X - mu)/o ≤ (c - mu)/o)

#Modi diversi di calcolarlo
#1 Mediante integrale:
v1 <- integrate(X, -Inf, x)$value;
v1;

#2 Mediante la funzione di distribuzione
#2.1 Non standard
v2 <- pnorm(x, mean = mu, sd = o);
v2;

#2.2 Standard
v3 <- pnorm(stdx, mean = 0, sd = 1);
v3;

#3: Calcolare P(X ≥ 44.7245) attraverso standardizzazione e usando le tavole.
#Calcolare P(X <= y) attraverso la standardizzazione
y <- 44.7245;
#1 Mediante integrale
p <- integrate(X, y, Inf)$value;
p;

#2 Mediante la funzione di distribuzione
#2.1 Non standard
#Nota: lower.tail = FALSE non fa altro che calcolare la probabilità al contrario, ossia invece di Pr(X <= c) calcola Pr(X >= c)
p2 <- pnorm(y, mean = mu, sd = o, lower.tail = FALSE);
p2;

#Calcolo stdy
stdy <- (y - mu)/o;

#2.2 Standard
p3 <- pnorm(stdy, mean = 0, sd = 1, lower.tail = FALSE);
p3;