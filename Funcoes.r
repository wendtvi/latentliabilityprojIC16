library(ape)
library(MASS)

sigma2=2 #variancia do mov brown 
n=6     #Numero de especies

F=rcoal(10)   #simula arvore
plot(F)



F$edge
F$tip.label


###############################  Esta é uma função que simula movimento 
#Browniano univariado em uma árvore filogenética F


movBrown=function(F, sigma2=1){

X=vector() #valor da variável contínua em cada no
X[F$edge[1,1]]=0

for(i in 1:length(F$edge.length)){
  X[F$edge[i,2]]=rnorm(1, mean=X[F$edge[i,1]],sd=sqrt(sigma2*F$edge.length[i]))
}



return(X[1:length(F$tip.label)])}





###############################  Esta é uma função que simula movimento 
#Browniano univariado em uma árvore filogenética F

#sigma é a matriz de correlação

Sigma=matrix(c(1,0.3,0,0.3,1,0,0,0,1), ncol=3)  #deve ser QUADRADA e simétrica e positiva definda




#função
MVmovBrown=function(F, Sigma, root=c(rep(0, times=ncol(Sigma)))){
  
  d=ncol(Sigma)
  n=length(F$tip.label)
  
  X=matrix(ncol=d, nrow=2*n-1) #valor da variável contínua em cada no
  X[F$edge[1,1],]=root
  
  for(i in 1:length(F$edge.length)){
    X[F$edge[i,2],]=mvrnorm(1, mu=X[F$edge[i,1],],Sigma=Sigma*sqrt(F$edge.length[i]))
  }

  
  xx=X[1:length(F$tip.label),]
  row.names(xx)=F$tip.label
  
  
  
    return(xx)}


#########################


MVmovBrown(F, Sigma)













