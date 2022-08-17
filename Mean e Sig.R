library(LaplacesDemon)

setwd("C:/Users/LCPM/Desktop/SIMULACAO1")
arquivos = list.files("C:/Users/LCPM/Desktop/SIMULACAO1", pattern="Aquileguia_Matrix.*log") 
rep=1000
contMean=1
i=0
MATRIXMean=matrix(nrow=rep,ncol=1)
MATRIXSig=matrix(nrow=rep,ncol=1)

for (p in 1:rep){
tab=read.table(arquivos[p], skip=100)  #ler o log da matriz de precisÃ£o
ess=ESS(tab[-c(1:3),2])

if(ess<70){
  return
}

else{
m=dim(tab)[1]  #numero de amostras no log
d=sqrt(dim(tab)[2]-4)

mat=matrix(ncol=d^2, nrow=m)     #log matriz de correlação

for (i in 1:m){
  temp=solve(matrix(as.numeric(tab[i,-c(1:4)]),ncol=d))
  temp2=matrix(temp,ncol=d)
  
  CORR=matrix(ncol=d,nrow=d)
  for(k in 1:d)
  {for(j in 1:d)
    
    CORR[k,j]=temp2[k,j]/sqrt(temp2[k,k]*temp2[j,j])	}
  
  mat[i,]=as.vector(CORR)
  
  
}

mean=vector()
sig=vector()
for (i in 1:d^2){
  mean[i]=mean(mat[,i])    #media de cada covariancia
  
  sig[i]=mean(mat[,i]>0)    # fraçao de cov maiores que 0
  
}

stringMeanFile=toString(mean)
stringMeanFile_s_virgula=gsub(",","",stringMeanFile)
MATRIXMean[p,]=stringMeanFile_s_virgula
  
stringSigFile=toString(sig)
stringSigFile_s_virgula=gsub(",","",stringSigFile)
MATRIXSig[p,]=stringSigFile_s_virgula
}
}


MATRIXMean_S_NA <- na.omit(MATRIXMean)
MATRIXSig_S_NA <- na.omit(MATRIXSig)

write(MATRIXMean_S_NA, file="MeanFile.txt", sep=" ")
write(MATRIXSig_S_NA, file="SigFile.txt", sep=" ")

