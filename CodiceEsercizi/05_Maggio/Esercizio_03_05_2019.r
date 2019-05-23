library(MASS);
#Distributore contenente palline numerate da 1 a 5.
#Probabilità palline:

#	| 1.00 | 2.00 | 3.00 | 4.00 | 5.00 |
# p | 0.20 | 0.26 | 0.16 | 0.21 | 0.17 |

#Indichiamo con X la variabile aleatoria discreta che descrive
#il risultato "otteniamo la pallina con numero..." X(w) in {1,2,3,4,5}

probabilità <- c(0.20, 0.26, 0.16, 0.21, 0.17);

#1: Calcolare il valore atteso di X
	valore_atteso <- 0;
	i <- 0;
	for(i in 1:5){
		valore_atteso <- valore_atteso + (i * probabilità[i]);
	}
	valore_atteso;

#2: Calcolare il terzo momento centrato di X
	terzo_momento_centrato <- 0;
	for(i in 1:5){
		terzo_momento_centrato <- terzo_momento_centrato + ((i-valore_atteso)^3 * probabilità[i]);
	}
	terzo_momento_centrato;

#3: Calcolare il terzo momento non centrato di Y = 2.75*X
	terzo_momento_non_centrato <- 0;
	val <- 2.75
	for(i in 1:5){
		terzo_momento_non_centrato <- terzo_momento_non_centrato + ((i * val)^3 * probabilità[i]);
	}
	terzo_momento_non_centrato;
	
	prova <- 0;
	for(i in 1:5){
		prova <- prova + val^3 * ((i)^3 * probabilità[i]);
	}
	prova;

#4: Probabilità condizionata:
	#Supponiamo ora che siamo due amici, entrambi vorremmo tentare la fortuna 
	#con il distributore ma, siccome abbiamo una sola moneta, tiriamo a sorte a chi tocca. 
	
	#La moneta è equilibrata. Io punto su testa. 
	#Qual è la probabilità che io ottenga la pallina con il numero 1?
	
	Pr_pallina <- probabilità[1] * (1/2);
	Pr_pallina;
	

#SOLUZIONI:
#1: 2.89
#2: 0.350838
#3: 858.703
#4: 0.1 == 0.2 * 0.5