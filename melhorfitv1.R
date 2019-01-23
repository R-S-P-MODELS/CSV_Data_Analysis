options(warn=-1)
require(ggplot2)
modelos=function(eixox,eixoy,limitepol){ # pede o eixo x, y pro fit e caso espandiamos em uma serie de taylor qual o limite da ordem
	x=list()
	x[[1]]=lm(eixoy~eixox)
	x[[2]]=lm(eixoy~exp(eixox))
	if(sum(eixox==0)==0){
	x[[3]]=lm(eixoy~log(eixox))
	}
	#ao inserir novos modelos coloque os aqui e apos modifique o valor de tamanho
	tamanho=3 
	for(i in 1:limitepol){
		x[[i+tamanho]]=lm(eixoy~poly(eixox,i+1))
	}
	if(sum(eixox==0)==0){
	w=1/eixox
	k=length(x)
	x[[k+1]]=lm(eixoy~w)
        x[[k+2]]=lm(eixoy~exp(w))
        x[[k+3]]=lm(eixoy~log(w))
	tamanho=length(x)
	for(i in 1:limitepol){
                x[[i+tamanho]]=lm(eixoy~poly(w,i+1))
	}
	tamanho=length(x)
	for(i in 1:limitepol){
	  
	 x[[i+tamanho]]=lm(eixoy~exp(poly(w,i+1)))
	}
	tamanho=length(x)
	if(sum(eixox==0)==0){
	for(i in 1:limitepol){
	  
	  x[[i+tamanho]]=lm(eixoy~log(poly(w,i+1)))
	}
	}
	}
	tamanho=length(x)
	for(i in 1:limitepol){
	  
	  x[[i+tamanho]]=lm(eixoy~exp(poly(eixox,i+1)))
	}
	
	tamanho=length(x)
	if(sum(eixox==0)==0){
	for(i in 1:limitepol){
	  
	  x[[i+tamanho]]=lm(eixoy~log(poly(eixox,i+1)))
	}
}
	
	
	
	return(x)
}


seleciona_melhor=function(x,y,limitepol,numero)
{
	candidatos=modelos(x,y,limitepol)
	vec=c()
	for(i in 1:length(candidatos)){
		vec[i]=anova(candidatos[[i]])$F[1]
	}
	#vec=vec[sort(vec,decreasing=TRUE)]
	#melhor=which(max(vec)==vec)[1]
#	print(candidatos)
	print(c(length(vec),length(candidatos))  )
#	print(order(vec,decreasing=TRUE))
	candidatos_ordenados=candidatos[order(vec,decreasing=TRUE)]
	#print(candidatos_ordenados)
	#print(candidatos[1:numero])
	return(candidatos_ordenados[1:numero])
	
}

seleciona_melhor_rs=function(x,y,limitepol,numero)
{
  candidatos=modelos(x,y,limitepol)
  vec=c()
  for(i in 1:length(candidatos)){
    vec[i]=summary(candidatos[[i]])$r.squared
  }
  #vec=vec[sort(vec,decreasing=TRUE)]
  #melhor=which(max(vec)==vec)[1]
  #print(candidatos)
  #print(vec)
  #print(order(vec,decreasing=TRUE))
  #print(c(length(vec),length(candidatos))  )
  #	print(order(vec,decreasing=TRUE))
  candidatos_ordenados=candidatos[order(vec,decreasing=TRUE)]
  #print(candidatos_ordenados)
  print(candidatos_ordenados[1:numero])
  return(candidatos_ordenados[1:numero])
  
}


fit_melhor_caso=function(x,y,limitepol,numero){
melhor_modelo=seleciona_melhor(x,y,limitepol,numero)
melhor_modelo2=seleciona_melhor_rs(x,y,limitepol,numero)
v=data.frame(x,y)
#print(melhor_modelo)
plot(x,y)
#print(melhor_modelo)
u=data.frame("0","0","0")
names(u)=c("x","y","modelo")
for(i in 1:length(melhor_modelo2)){
lines(v[,1], predict(melhor_modelo2[[i]], data.frame(x=v[,1])), col="red")
#  aux=data.frame(v[,1],predict(melhor_modelo2[[i]], data.frame(x=v[,1])),toString(melhor_modelo2$call$formula) )
#  aux=data.frame(v[,1],predict(melhor_modelo2[[i]], data.frame(x=v[,1])),i )
#  names(aux)=names(u)
#  print(aux)
#  u=rbind(u,aux)
}
#print(u)
#ggplot(data=u) +geom_point(aes(x=x,y=y,col="black")) +geom_line(aes(x=u[,1],y=u[,2],colours=u[,3] )  )
}
