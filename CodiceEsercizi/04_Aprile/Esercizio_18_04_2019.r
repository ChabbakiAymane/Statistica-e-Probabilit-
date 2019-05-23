library(MASS);

#f(x) = k(9.87 - 2x)	0 < x < 4.7
#		0				altrimenti

#non toccare, giusto così, dopo viene ricalcolata e tutto torna
#R usa dynamic scoping, serve solo per non avere problemi
k <- 1;

#Funzione di densità
	f <- function(x){
		return(k*(9.87 - 2*x));
	}

#Funzione per il calcolo del valore atteso di ordine 1, ossia integrale di x*f(x)!
	xf <- function(x){
		return(x * f(x));
	}
	
#Funzione per il calcolo del valore atteso di ordine 2, ossia integrale di x^2*f(x)!
	x2f <- function(x){
		#return(x * xf(x));
		return(x^2 * f(x));
	}

#1: Determinare la costante  affinchè f(x) sia una funzione di densità
	#intervalli dove è definito il limite
	inf <- 0;
	max <- 4.7;

	#calcolo di c
	value <- integrate(f, inf, max, stop.on.error = FALSE)$value;
	k <- 1/value;
	k;

#2: Calcolare il valore atteso di X
	#E(X) = integrale(x*f(x))
	speranza <- integrate(xf, inf, max, stop.on.error = FALSE)$value;
	speranza

#3: Calcolare il valore atteso di 8.6 + 1.9X
	#E(a+bx) = a + b*E(x)
	a <- 8.6
	b <- 1.9
	#var <- a + (b * integrate(xf, inf, max, stop.on.error = FALSE)$value);
	var <- a + (b * speranza)
	var

#4: Dato il momento non centrato di ordine 2, calcolare la varianza di X
	#Var(X) = E(x^2) - (E(x))^2
	#"Dato" un caz... che è sbagliato. Tocca calcolarlo
	
	momento_nonCentrato_2 <- integrate(x2f, inf, max, stop.on.error = FALSE)$value;
	momento_nonCentrato_2
	var <- momento_nonCentrato_2 - (speranza)^2;
	var
	
#1: 0.04115396
#2: 1.637879
#3: 11.71197 == 8.6 + (1.9 * 1.637879)
#4: 1.333717 == 4.016364 - (1.637879)^2 == 4.016364 - 2.682648