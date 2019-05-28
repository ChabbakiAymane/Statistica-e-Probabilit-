library(MASS)

#Sia {X1, .. Xn} un campione di variabili aleatorie di distribuzione esponenziale stocasticamente indipendenti e identicamente distribuite
campione <- c(10.50,1.00,19.30,2.70,1.20,0.80,2.60,11.50,1.70,4.30)

#Serve solo per il codice, non per altri scopi, modificarlo o meno non da alcun problema
lamdaCampione <- 1

mediaAritmetica <- function(campione){
    media <- 0
    for(i in 1:length(campione)){
        media <- media + campione[i]
    }
    return(media/length(campione))
}

stimatoreMaximumLikehoodExp <- function(campione){
    return(1/mediaAritmetica(campione))
}

#T_1 = X_3
#T_2 = (X_1 + 2*X_2) / 3
#T_3 = (X_1 + X_2 + X_3)/6 - (X_3 + X_4)/4

#1: Quale tra i precedenti stimatori sono non-distorti?
	#Si presume: 
	#T1 = cX3
	costanteMoltiplicativaT1 <- 1
	
	#T2 = (X1 + cX2)/d
	costanteMoltiplicativaX1T2 <- 1
	costanteMoltiplicativaX2T2 <- 2
	costanteDenominatoreT2 <- 3
	
	#T3 = (X1 + X2 + X3)/6 + (X3 + X4)/4
	costanteMoltiplicativaX1T3 <- 1
	costanteMoltiplicativaX2T3 <- 1
	costanteMoltiplicativaX3T3_1 <- 1
	costanteDenominatoreT3_1 <- 6
	costanteMoltiplicativaX3T3_2 <- 1
	costanteMoltiplicativaX4T3_2 <- 1
	costanteDenominatoreT3_2 <- 4

	#Si assume Tbar dato da stimare
		Tbar <- 1/lamdaCampione
		Eexp <- 1/lamdaCampione #dato che sono identicamente distribuite
		VarExp <- 1/(lamdaCampione^2)
	#Tornano i valori attesi

ET1 <- function(){
	costanteMoltiplicativaT1 * Eexp
}

ET2 <- function(){
	(1/costanteDenominatoreT2)*(costanteMoltiplicativaX1T2 * Eexp + costanteMoltiplicativaX2T2 * Eexp)
}

ET3 <- function(){
	(1/costanteDenominatoreT3_1) * (costanteMoltiplicativaX1T3 * Eexp + costanteMoltiplicativaX2T3 * Eexp + costanteMoltiplicativaX3T3_1 * Eexp) + (1/costanteDenominatoreT3_2)*(costanteMoltiplicativaX3T3_2 * Eexp + costanteMoltiplicativaX4T3_2 * Eexp)
}

#Varianze
VT1 <- function(){
    (costanteMoltiplicativaT1^2) * VarExp;
}

VT2 <- function(){
    (costanteMoltiplicativaX1T2/costanteDenominatoreT2)^2 * VarExp + (costanteMoltiplicativaX2T2/costanteDenominatoreT2)^2 * VarExp;
}

VT3 <- function(){
    VarExp * ((costanteMoltiplicativaX1T3/costanteDenominatoreT3_1)^2 + (costanteMoltiplicativaX2T3/costanteDenominatoreT3_1)^2 + (costanteMoltiplicativaX3T3_1/costanteDenominatoreT3_1)^2 + (costanteMoltiplicativaX3T3_2/costanteDenominatoreT3_2)^2 +(costanteMoltiplicativaX3T3_2/costanteDenominatoreT3_2)^2 + (2*VarExp/(costanteDenominatoreT3_1 * costanteDenominatoreT3_2))) 
}

BT1 <- ET1() - Tbar
BT2 <- ET2() - Tbar
BT3 <- ET3() - Tbar

if(BT1 != 0 && BT2 != 0 && BT3 != 0){
    0
}

if(BT1 == 0 && BT2 != 0 && BT3 != 0){
    1
}

if(BT1 != 0 && BT2 == 0 && BT3 != 0){
    2
}

if(BT1 != 0 && BT2 != 0 && BT3 == 0){
    3
}

if(BT1 == 0 && BT2 == 0 && BT3 != 0){
    4
}

if(BT1 == 0 && BT2 != 0 && BT3 == 0){
    5
}

if(BT1 != 0 && BT2 == 0 && BT3 == 0){
    6
}

if(BT1 == 0 && BT2 == 0 && BT3 == 0){
    7
}

#2: Calcolare la varianza dello stimatore T_1, T_2, T_3
	#Calcolo della varianza
	#Scrivere la varianza che si vuole
	Var <- VT1()
	function(lambda){
		return(ifelse(lambda > 0, Var * 1/(lambda^2), 0))
	}

#3: Quale tra gli stimatori T_1, T_2, T_3 è preferibile (in termini di errore quadratico medio)?
	min <- VT1()

	if(VT2() < min){
		min <- VT2()
	}

	if(VT3() < min){
		min <- VT3()
	}

	fractions(min)

#4: Dopo aver determinato lo stimatore di massima verosomiglianza lambda' per lambda, inserirne la stima per il campione
	stima <- stimatoreMaximumLikehoodExp(campione);
	stima;
	
#RISPOSTE:
#1: 7
#2: function(lambda){return(ifelse(lambda <= 0,0,1/lambda^2))}
#3: 3
#4: 0.1798561