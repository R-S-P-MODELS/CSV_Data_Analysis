NewPCA<-function(data,k){
	suppressMessages(require(RSpectra) )
	data1=as.data.frame(scale(data[sapply(data,class) %in% c('integer','numeric')],scale=TRUE))
	data2=abs(data1)
	aux=which(apply(abs(data2),2,sum)==0)
	if(length(aux)>0)
		data1=data1[,-aux]
	faltantes=which(cleanerR::NA_VALUES(data1)>0 )
	if(length(faltantes)>0)
		data1=data1[,-faltantes]
	univalorados=which(sapply(sapply(data1,unique),length)==1)
	if(length(univalorados)>0)
		data1=data1[,-univalorados]

	if(ncol(data1)==0)
		return(0)
	#data[names(data1)]=data1 # numericos
	covariancia=cov(data1)
	autovalores=RSpectra::eigs(covariancia,k)

	pca=t(t(autovalores$vectors) %*% t(data1))

	return(pca)
}

PCAHOT<-function(data,k=2,target){
PrimaryKey=0.9
require(RSpectra)
Remover=which(names(data)==target)
if(length(Remover)==0)
	return(0)
data=data[,-Remover]
faltantes=which(cleanerR::NA_VALUES(data)>0 )
if(length(faltantes)>0)
data=data[,-faltantes]

data1=as.data.frame(scale(data[sapply(data,class) %in% c('integer','numeric')],scale=TRUE))
#data[names(data1)]=data1 # numericos
fatores=as.numeric(which(sapply(data,class) %in% c('factor','character')))
data[,fatores]=sapply(data[,fatores],as.character)
 data=data[,as.numeric(fatores[which( (sapply(sapply(data[,fatores],unique),length)/nrow(data) ) < PrimaryKey) ])]
caracteres=as.numeric(which(sapply(data,class) %in% c('character')))
for(i in caracteres){
onehot=one_hot_encoding(data[,i],names(data)[i])
data1=cbind(data1,onehot)
}
print(dim(data1))
data1=scale(data1,scale=TRUE)
data2=abs(data1)
aux=which(apply(data2,2,sum)==0)
if(length(aux)>0)
	data1=data1[,-aux]
#faltantes=which(cleanerR::NA_VALUES(data1)>0 )
#data1=data1[,-faltantes]
if(ncol(data1)==0)
	return(0)
covariancia=cov(data1)
 autovalores=RSpectra::eigs(covariancia,k)

pca=t(t(autovalores$vectors) %*% t(data1))

return(pca)
}

one_hot_encoding<-function(variable,nome){
tamanho=length(unique(variable))
m=matrix(0,nrow=length(variable),ncol=tamanho)
for(i in 1:tamanho){
indices=which(variable==unique(variable)[i])
m[indices,i]=1
}
m=as.data.frame(m)
names(m)=paste(nome,".",1:tamanho,sep="")
return(m)
}
