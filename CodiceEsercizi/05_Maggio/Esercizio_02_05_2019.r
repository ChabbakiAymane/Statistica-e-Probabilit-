library(MASS);
#Distribuzione esponenziale di media 4 minuti

#Funzioni:
F <- function(x){ return(lamda * exp(-lamda * x));}
F_prova <- function(x){ return (exp(-lamda * x));}

#Dati
media = 4;
#E(X) = 1/lamda --> lamda = 1/E(X)
lamda = 1/4;

#1: Determinare la probabilità che il tempo trascorso tra il passaggio 
#	di due veicoli successivi sia minore di 4.9
	inf = 0;
	max = 4.9;
	
	print("Calcolata usando integrale");
	pr_1 <- integrate(F,inf,max)$value;
	pr_1;
	
	print("Calcolata usando formula Pr(T < x) = 1 - exp(-lamda * x)");
	prova1 <- 1 - F_prova(max);
	prova1;

#2: Determinare l'intervallo di tempo t tale per cui siamo certi al 85%,
#	che il tempo trascorso tra il passaggio di due veicoli successivi sia maggiore di t.
	#P(T > x) = 0.85
	val = 85/100;
	t <- (-media) * log(val);
	t;


#3: Sapendo che sono già trascorsi 2.8 minuti dal passaggio di un veicolo, qual'è
#	la probabilità che si debba attendere altri 4 minuti per il passaggio del veicolo successivo?
	inf = 0;
	max = 4;
	
	print("Calcolata usando integrale");
	pr_2 <- integrate(F,inf,max)$value;
	pr_2;
	
	print("Calcolata usando formula Pr(T < x) = 1 - exp(-lamda * x)");
	prova2 <- 1 - F_prova(max);
	prova2;

#4: Trasformazione di variabile casuale: si consideri ora la variabile casuale U = sqrt(T). 
#	Scrivere la funzione di densità U.
	#U = sqrt(T)
	#g^(-1) = y^2
	#(g^(-1))' = 2y
	#Quindi la trasformazione della variabile casuale nel supporto (0, +Inf):
	#f_x(g^(-1)) * (g^(-1))' = λ * e^{-λ * g^(-1)} * |(g^(-1))'| = λ * e^{-λ * y^2} * |2y| = λ * e^{-λ * y^2} * 2y
	B <- function (u) { ifelse (u<0, 0, lamda * 2 * u * exp(-(u^2) * lamda)) }	

#RISULTATI:
#1: 0.7062423 == integrate(function(x){ return((1/4) * exp(-(1/4) * x));}, 0, 4.9)$value;
#2: 0.6500757 == -4*log(0.85)
#3: 0.6321206 == integrate(function(x){ return((1/4) * exp(-(1/4) * x));}, 0, 4)$value;
#4: function (u) { ifelse (u>=0, 2*(1/4)*u * exp(-(1/4) * u^2), 0) }