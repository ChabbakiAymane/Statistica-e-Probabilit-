#X ~ Pois(λ = 6)

#dati
	lambda <- 6; #macchine all'ora (in media)

#1: Qual è la probabilità che non arrivino macchine nell'ora indicata?
#1: dpois(0, 6) == 0.002478752
	macchine_aspettate <- 0
	quesito1 <- dpois(macchine_aspettate, lambda);
	quesito1;

#2: Qual è la probabilità che arrivino esattamente 1 macchine nell'ora indicata?
#2: dpois(1, 6) == 0.01487251
	macchine_aspettate <- 1;
	quesito2 <- dpois(macchine_aspettate, lambda);
	quesito2;

#3: Qual è la probabilità che arrivino almeno 8 e non più di 11 macchine?
#3: sum(dpois(8:11, 6)) == 0.2359283
	minMacchine <- 8;
	maxMacchine <- 11;

	quesito3 <- 0;

	#Con Poisson
	quesito3 <- sum(dpois(minMacchine:maxMacchine, lambda));
	quesito3;

	#Con proprietà ricorsiva
	myPoisson <- function(index, starting_index, lambda) {
		if(index < starting_index) return(0);
		base <- dpois(starting_index, lambda);
		rec <- 1;
		while(index > starting_index){
			rec <- rec * lambda/index;
			index <- index - 1;
		}
		return(rec * base);
	}

	probRic <- 0;
	for (index in minMacchine:maxMacchine){
		probRic <- probRic + myPoisson(index, minMacchine, lambda);
	}
	probRic;