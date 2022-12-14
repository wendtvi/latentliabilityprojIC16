#pacotes necessarios
library(ape)
library(MASS)
library(XML)

Re= 1000   #numero de replica??es
n = 10  #tamanho da amostra
d = 4   #dimens?es da vari?vel latente
Raiz= rnorm(d,0,1) #raiz a priori
num_discreta=2
Sigma=matrix(c(1,0, 0, 0, 0, 1, 0 ,0, 0, 0, 1, 0 , 0, 0, 0, 1  ), nrow=d)   #Matriz de covari?ncia do mov brown
#deve ser QUADRADA e sim?trica e positiva definda
F=rcoal(n)   #simulamos a ?rvore
arq=xmlInternalTreeParse("Aquilegia_simulacoes.xml", FALSE)  #le xml
arquivo=file("Aquilegia_simulacoes.xml")


## Fun??o necess?ria
# Simula mov browniano multivariado na ?rvore

MVmovBrown=function(F, Sigma, root=Raiz){
  
  d=ncol(Sigma)
  n=length(F$tip.label)
   
  X=matrix(ncol=d, nrow=2*n-1) #valor da vari?vel cont?nua em cada no
  X[F$edge[1,1],]=root
  
  for(i in 1:length(F$edge.length)){
    X[F$edge[i,2],]=mvrnorm(1, mu=X[F$edge[i,1],],Sigma=Sigma*sqrt(F$edge.length[i]))
  }
  
  
  xx=X[1:length(F$tip.label),]
  row.names(xx)=F$tip.label
  
  
  
  return(xx)}


#### Aqui temos a simula??o

#nesta simula??o consideramos Y1 e Y2 continuos e Y3 e Y4 bin?rios

#Para cada repeti??o 
X=MVmovBrown(F, Sigma)  #simulamos a vari?vel latente
Y=matrix(ncol=d, nrow=n)  #Montamos a matriz de dados observados
row.names(Y)=row.names(X)  #coloca os nomes das tips em Y
Y[,1:2]=X[,1:2]            # Para os tra?os cont?nuos, Y=X
Y[,3:4]=1*(X[,3:4]>0)   #
Y
nomes=row.names(Y)

#Falta simular valor da raiz usando distribui??o a priori
#automatizar montagem do xml

write.tree(F)


#EDITA XML
raiz=xmlRoot(arq)
TaxaNode= getNodeSet(arq, "//taxa")
TreeNode=getNodeSet(arq, "//newick")
MatrixNode = getNodeSet(arq, "//matrixParameter")
MultivariateNode= getNodeSet(arq, "//meanParameter")
PriorMultivariate= getNodeSet(arq, "//multivariateWishartPrior")
MaskNode= getNodeSet(arq, "//mask")
AlinhamentoNode= getNodeSet(arq, "//alignment")
#numClassesNode=getNodeSet(arq, "//numClasses")


####adiciona elemento taxa
for (i in 1:n){ 
  string=toString(Y[i,])
  string_s_virgula=gsub(",","",string)
  taxonNode=newXMLNode("taxon", attrs=c("id"=nomes[i]))
  attrNode=newXMLNode("attr", attrs=c("name"="latent"),paste(string_s_virgula, sep=" "))
  TaxonNode=addChildren(taxonNode,attrNode)
  TaxaNode[[1]]=addChildren(TaxaNode[[1]], taxonNode)
  
  }


#adiciona startingTree ao xml
TreeNode[[1]]=addChildren(TreeNode[[1]], write.tree(F))


###adiciona matrizes
matriz1=matrix(nrow=d,ncol=d)
matriz2=matrix(nrow=d,ncol=d)
for (i in 1:d){
  for (j in 1:d){
  if (i==j) matriz1[i,j]=0.001  else matriz1[i,j]=0 
  if (i==j) matriz2[i,j]=1  else matriz2[i,j]=0
}
string_matriz1=toString(matriz1[i,])
string_s_virgula_matriz1=gsub(",","",string_matriz1)
string_matriz2=toString(matriz2[i,])
string_s_virgula_matriz2=gsub(",","",string_matriz2)
matrix1=newXMLNode("parameter", attrs=c("value"= paste(string_s_virgula_matriz1,sep=" ")))
matrix2=newXMLNode("parameter", attrs=c("value"= paste(string_s_virgula_matriz2,sep=" ")))
MatrixNode[[1]]=addChildren(MatrixNode[[1]],matrix1) 
MatrixNode[[2]]=addChildren(MatrixNode[[2]],matrix2)
}

###adiciona mean parameter na likelihood
meanParameter_matrix=matrix(nrow=1,ncol=d)
for(i in 1:d){
  meanParameter_matrix[,i]="0.0"
}
stringMeanParameter=toString(meanParameter_matrix[1,])
stringMeanParameter_s_virgula=gsub(",","",stringMeanParameter)
meanParameterNode=newXMLNode("parameter", attrs=c("value"= paste(stringMeanParameter_s_virgula,sep=" ")))
MultivariateNode[[1]]=addChildren(MultivariateNode[[1]],meanParameterNode)


###adiciona masked
Matrix_Masked=Y
Matrix_Masked[,1:2]=0
Matrix_Masked[,3:4]=1
mask_matrix=matrix(nrow=1,ncol=n)
for(i in 1:n){
  string=toString(Matrix_Masked[i,])
  string_s_virgula=gsub(",","",string)
  mask_matrix[1,i]=string_s_virgula
  string2=toString(mask_matrix[1,])
  string2_s_virgula=gsub(",","",string2)
}
mask_parameter=newXMLNode("parameter", attrs=c("id"="mask", "value"=string2_s_virgula))
MaskNode[[1]]=addChildren(MaskNode[[1]],mask_parameter)

### alinhamentos
Matrix_Alinhamento=Y
Matrix_Alinhamento[,1:2]=1
for (i in 1:n){ 
  string=toString(Matrix_Alinhamento[i,])
  string_s_virgula=gsub(", ","",string)
  sequenceNode=newXMLNode("sequence")
  taxonNode=newXMLNode("taxon", attrs=c("idref"=nomes[i]))
  Alinhamento=paste(string_s_virgula)
  addTaxonNode=addChildren(sequenceNode,taxonNode)
  addTaxonNode_alinhamento=addChildren(sequenceNode,Alinhamento)
  AlinhamentoNode[[1]]=addChildren(AlinhamentoNode[[1]], sequenceNode)
  
}

###classes
#GUARDA_NUM_DISCRETA=matrix(nrow=1,ncol=1)
#cont=0
#MATRIX_CLASSES=matrix(nrow=1,ncol=d)
#for (i in 1:num_discreta){
 # MATRIX_CLASSES[1,i]="1.0"
#}
#for (i in d-num_discreta+1:d){
 # GUARDA_NUM_DISCRETA[1,1]=Y[1,i]
#  for (j in 2:n){  
    
#}
 # MATRIX_CLASSES[1,i]=cont
#}

#salvando xml
saveXML(raiz,arquivo)