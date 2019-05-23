library(MASS)

#1: Lanciando 20 volte una moneta equilibrata, l'evento ottenere n/2 teste è più (strettamente più) probabile del suo complementare?
	n_lanci <- 20
	n_teste <- n_lanci/2

	Pr_meta_teste <- choose(n_lanci, n_teste)*(1/2)^(n_teste)*(1/2)^(n_lanci-n_teste)
	Pr_meta_teste
	Pr_complementare <- 1-Pr_meta_teste
	Pr_complementare
	Pr_meta_teste > Pr_complementare

#2: Qual è la probabilità di ottenere almeno 17 teste?
	n_teste_2 <- 17

	i<-n_teste_2
	Pr_almeno_n_teste_2 <- 0
	while(i<=n_lanci){
		Pr_almeno_n_teste_2 <- Pr_almeno_n_teste_2 + choose(n_lanci, i)*(1/2)^(i)*(1/2)^(n_lanci-i)
		i <- i+1
	}
	Pr_almeno_n_teste_2

#3: In quanti possibili modi si possono distribuire 10 successi in una successione di 14 prove
	#uso le combinazioni
	choose(n_lanci, n_teste_2)

#4: Supponendo che ora la probabilità di ottenere testa sia 0.38, qual è il numero medio di teste in 14 lanci
	pr_testa <- 0.38

	E_Bin <- pr_testa*n_lanci
	E_Bin
	
#RISULTATI:
	#1: FALSE
	#2: 0.001288414 
	#3: 1140 == choose(20,17)
	#4: 7.6 == 20 * (0.38)