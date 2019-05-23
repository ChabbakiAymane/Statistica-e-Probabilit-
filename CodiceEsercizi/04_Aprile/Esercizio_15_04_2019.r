#Probabilità che nasca un maschio è 0.39
#Numero di figli è 6

#La probabilità che nasca un maschio è p, il numero di figli è n
p <- 0.39
n <- 6

#1: Calcolare che almeno uno sia maschio
x <- 1
Pr_minore_x <- 0
i <- 0
while(i<x){
    Pr_minore_x <- Pr_minore_x + choose(n, i)*((p)^i)*((1-p)^(n-i))
    i <- i+1
}
Pr_almeno_x <- 1 - Pr_minore_x
Pr_almeno_x

#2: Ci siano esattamente 3 femmine
m <- 3
q <- 1-p
Pr_m_femmine <- choose(n,m)*(q^m)*((1-q)^(n-m))
Pr_m_femmine

#3: Non ci siano più di 3 maschi
y <- 3
i <- 0 
Pr_massimo_y <- 0
while(i<=y){
    Pr_massimo_y <- Pr_massimo_y + choose(n,i)*(p^i)*((1-p)^(n-i))
    i <- i+1
}
Pr_massimo_y

# 0.9484796 == 1 - (choose(6,0) * (0.39)^0 * (0.61)^6)
# 0.2692857 == choose(6,3) * (0.39)^3 * (0.61)^3
# 0.8343345 == sum(dbinom(0:3, 6, 0.39))