library(MASS);

#Funzione definita su tutto R:
#	              1
#	 F(x) = ---------------
#	        1 + e^(1.4 * x)

#1: Scrivere un'implementazione in R di F
#La funzione deve essere della forma: function(x){...}, ovvero deve accettare
#in input un solo parametro.
P <- function(x){
	tmp <- 1/(1 + exp(1.4 * x));
	return (tmp);
}

#2: F(x) è una funzione di Probabilità per gli eventi di B(R)?
	#Funzione di distribuzione se:
		#1- Funzione non decrescente
		#2- Funzione continua a destra: per ogni x_0 lim(x -> x_0^+) F(x) = F(x_0)
		#3- Ammette limite a sinistra: per ogni x_0 Esiste lim(x -> x_0^-) F(x)
		#4- lim(x -> + inf) F(x) = 1
		#5- lim(x -> - ìnf) F(x) = 0
	
		#1- La funzione è decrescente (dal grafico).
		#2- Ammette limite sinistro.
		#3- E' continua a destra.
		#4- lim(x -> + inf) F(x) = 0
		#5- lim(x -> - inf) F(x) = 1

	#Visto che 1,4,5 non sono rispettate, F(x) non è una funzione di probabilità.
		FALSE;

######################################################################################
		
#Se F non è una funzione di probabilità, di seguito si consideri:
#	              1
#	 F(x) = ----------------
#	        1 + e^(-1.4 * x)

F <- function(x){
	tmp <- 1/(1 + exp(-1.4 * x));
	return (tmp);
}

#3: Qual'è la probabilità dell'intervallo [0.49, 1.08]:
	Pr_Intervallo <- F(1.08) - F(0.49);
	Pr_Intervallo;

#4: Qual'è la probabilità di {0}?
	#La probabilità del singoletto {0} è 0. Perchè F è una funzione di distribuzione
	#continua e non discreta e quindi la probabilità dei singoletti vale 0.
	#F(x) = F(x) - F(x) per ogni x
	Pr_Singoletto <- F(0) - F(0);
	Pr_Singoletto;