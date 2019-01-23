options(warn=-1)
require(ggplot2)
modelos=function(eixox,eixoy,limitepol){ # pede o eixo x, y pro fit e caso espandiamos em uma serie de taylor qual o limite da ordem
	x=list()
	#eixoy=eixoy[which(eixox!=0)]
	#eixox=eixox[which(eixox!=0)]
	#eixox=eixox[which(eixoy!=0)]
	#eixoy=eixoy[which(eixoy!=0)]
	x[[1]]=lm(eixoy~eixox)
	index=1
	if(sum(exp(eixox)==Inf)==0){
	  index=index+1
	x[[index]]=lm(eixoy~exp(eixox))
	}
	if(sum(eixox==0)==0){
	  index=index+1
	x[[index]]=lm(eixoy~log(eixox))
	}
	#ao inserir novos modelos coloque os aqui e apos modifique o valor de tamanho
	tamanho=length(x) 
	for(i in 1:limitepol){
		x[[i+tamanho]]=lm(eixoy~poly(eixox,i+1,raw = TRUE))
	}
	if(sum(eixox==0)==0){
	w=1/eixox
	k=length(x)
	x[[k+1]]=lm(eixoy~w)
        x[[k+2]]=lm(eixoy~exp(w))
        x[[k+3]]=lm(eixoy~log(w))
	tamanho=length(x)
	for(i in 1:limitepol){
                x[[i+tamanho]]=lm(eixoy~poly(w,i+1,raw=TRUE ) )
	}
	tamanho=length(x)
	for(i in 1:limitepol){
	  
	 x[[i+tamanho]]=lm(eixoy~exp(poly(w,i+1,raw=TRUE)))
	}
	tamanho=length(x)
	if(sum(eixox==0)==0){
	for(i in 1:limitepol){
	  
	  x[[i+tamanho]]=lm(eixoy~log(poly(w,i+1,raw=TRUE)))
	}
	}
	}
	tamanho=length(x)
	for(i in 1:limitepol){
	  if(sum(exp(poly(eixox,i+1,raw=TRUE))==Inf)==0){
	    
	  x[[i+tamanho]]=lm(eixoy~exp(poly(eixox,i+1,raw = TRUE)))
	  }
	}
	
	tamanho=length(x)
	if(sum(eixox==0)==0){
	for(i in 1:limitepol){
	  
	  x[[i+tamanho]]=lm(eixoy~log(poly(eixox,i+1,raw=TRUE)))
	}
}
	
	
	
	return(x)
}

elimina_Fit_falho=function(modelos){
  vec=list()
  cont=1
  for(i in 1:length(modelos)){
    ll=sum(is.na(modelos[[i]]$coefficients))
    if(ll==0){
      vec[[cont]]=modelos[[i]]
    cont=cont+1
    }
  }
  return(vec)
  
}

elimina_erros=function(modelos,x){
  vec=list()
  cont=1
  v=data.frame(x)
  for(i in 1:length(modelos)){
    tryCatch({
    ll=predict(modelos[[i]], data.frame(x=v[,1]))
      if( sum(is.na(ll))==0 & sum(ll==Inf)==0  )
        vec[[cont]]=modelos[[i]]
        cont=cont+1
    },error=function(e){})
  }
  return(vec)
  
}

seleciona_melhor=function(x,y,limitepol,numero)
{
	candidatos=modelos(x,y,limitepol)
	candidatos=elimina_Fit_falho(candidatos)
	candidatos=elimina_erros(candidatos,x)
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
  candidatos=elimina_Fit_falho(candidatos)
  candidatos=elimina_erros(candidatos,x)
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
#  print(candidatos_ordenados[1:numero])
  return(candidatos_ordenados[1:numero])
  
}


fit_melhor_caso=function(x,y,limitepol,numero){
melhor_modelo=seleciona_melhor(x,y,limitepol,numero)
melhor_modelo2=seleciona_melhor_rs(x,y,limitepol,numero)
v=data.frame(x,y)
#print(melhor_modelo)
#plot(x,y)
#print(melhor_modelo)
sem0=v#v[which(v[,1]!=0),]
ll=predict(melhor_modelo[[1]], data.frame(x=sem0[,1]))
u=data.frame(sem0[,1],ll,toString(melhor_modelo[[1]]$call$formula))
names(u)=c("x","y","modelo")
if(length(melhor_modelo)>1){
for(i in 2:length(melhor_modelo)){
#lines(v[,1], predict(melhor_modelo2[[i]], data.frame(x=v[,1])), col="red")
#  aux=data.frame(v[,1],predict(melhor_modelo2[[i]], data.frame(x=v[,1])),toString(melhor_modelo2$call$formula) )
  ll=predict(melhor_modelo[[i]], data.frame(x=sem0[,1]))
  print(ll)
  aux=data.frame(sem0[,1],ll,toString(melhor_modelo[[i]]$call$formula) )
  names(aux)=names(u)
#  print(aux)
  u=rbind(u,aux)
  print(nrow(u))
  
}
}
print(dim(u))
#write.table(file="meta.dat",u,row.names = FALSE)
#print(u[1:201,])
ggplot(data=u) +geom_point(aes(x=x,y=y)) +geom_line(aes(x=u[,1],y=u[,2],col=u[,3] )  )
}
