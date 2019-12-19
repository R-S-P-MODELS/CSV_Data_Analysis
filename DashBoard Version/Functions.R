GrafoCorrelacao<-function(w,tresh=0.8){
  m=Matriz_Correlacao_retorno(w)
  require(igraph)
  require(reshape2)
  require(ggraph)
  m=melt(m)
  names(m)[3]='cooc'
  m$cooc=abs(m$cooc)
  m=m[m$cooc>tresh,]
  m=m[m[,1]!=m[,2],]
  wordnetwork=graph_from_data_frame(m)
  
  p<-ggraph(wordnetwork, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
    geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    theme(legend.position = "none")  + labs(title = "Grafo onde variaveis com correlação maior que 0.9 em modulo são conectadas")
  return(p)
  
}




recuperar_categorica=function(c,d,delta){ # c sera a coluna com maior correlacao d a de valores faltando, delta o fator de aceitacao de valores iguais para valores reais
  #vec2=c()
  #b=d
  #na pratica todo b é d
  
  vec1=c()
  cont=1
  g=which(is.na(d)) #d e a coluna com valores faltantes
  e=c[g] # c será a coluna que tem maior correlação com a coluna que falta valores
  for(i in e){
    if(class(c)!="numeric" & class(c)!="integer")
      tabela=table(d[which(c==i)] )
    else
      tabela=table(d[which(c-delta<i & c+delta > i)] )
    if(length(tabela)>0){
    probabilidades=as.numeric(tabela)
    probabilidades=probabilidades/sum(probabilidades)
    probabilidades=cumsum(probabilidades)
    indice=min(which(runif(1)<probabilidades))
    if(class(c)!="numeric" & class(c)!="integer")
      d[ g[cont] ]=names(tabela)[indice]
    else
      d[g[cont]]=as.numeric(tabela)[indice]
    #print(b[g[cont]])
    }
    cont=cont+1
  }
  #vec2[j]=sum(vec1==b[g])/length(b[g])
  return(d)
}


#segunda opcao featurizer





predicao_importancia_v2=function(w,meta){ #essa ta incompleta
  a=matriz_correlacao_completa(w)
  nome1=names(w)[meta]
  indice=which(nome1==names(a[,1]))
  valores=as.numeric(a[,indice])
  print(a[order(abs(a[,indice])),indice])
  
}

predicao_importancia_v2_table=function(w,meta){ #espera um data.table
  a=matriz_correlacao_completa_table(w)
   nome1=names(w)[meta]
  indice=which(nome1==names(a[,1]))
  valores=as.numeric(a[,indice])
  print(a[order(abs(a[,indice])),indice])
  
}

Matriz_Correlacao_retorno=function(w){
  w=as.data.frame(w)
  categoricos=which(sapply(w,class)=="character" )
  if(length(categoricos)>0){
    for(i in categoricos)
      w[,i]=as.factor(w[,i])
  }
  #print(dim(w))
  fac=sapply(w,as.numeric)  
  
   return( cor(fac,use = "pairwise.complete.obs"))

    
  }


MatrixggplotShiny=function(ly){
require(reshape2)
require(ggplot2)
require(plotly)
  print(dim(ly))
h1=melt(ly)
p1=ggplot(data = h1, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + labs(x="",y=""  ) + theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
        axis.ticks.x=element_blank() ,axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank() )    
p1=p1+scale_fill_gradientn(colours = rainbow(20))
p1=p1+labs(title="Matriz de correlação do dataset")
#return(ggplotly(p1))
return(p1)
#ggplotly(ggplot(data = h1, aes(x=Var1, y=Var2, fill=value)) + geom_tile() +scale_color_gradientn(colours = rainbow(20)) + labs(x="",y=""  ) + theme(axis.title.x=element_blank(),
 #       axis.text.x=element_blank(),
 #       axis.ticks.x=element_blank() ,axis.title.y=element_blank(),
 #       axis.text.y=element_blank(),
 #       axis.ticks.y=element_blank() )  )  



}

matriz_correlacao_completa=function(w){
  categoricos=which(sapply(w,class)=="character" )
  if(length(categoricos)>0){
    for(i in categoricos)
      w[,i]=as.factor(w[,i])
  }
  #print(dim(w))
  fac=sapply(w,as.numeric)  
  return( cor(fac,use = "pairwise.complete.obs") )
  
  
  
}

matriz_correlacao_completa_old=function(w){
  numericos=which(sapply(w,class)=="numeric" | (sapply(w,class)=="integer") )
  #print(dim(w))
  fac=w[,-numericos]
  #       print(dim(fac))
  numer=w[,numericos]
  #       print(dim(numer))
  if(ncol(fac)>0){
    for(i in 1:ncol(fac)){
      fac[,i]=convert_fac_num(fac[,i])
    }
    fac=data.frame(fac,numer)
    cor(fac,use = "pairwise.complete.obs")
    
    
  }
}
matriz_correlacao_completa_table_old=function(w){
  numericos=which(sapply(w,class)=="numeric" | (sapply(w,class)=="integer") )
  #print(dim(w))
  fac=w[,-..numericos]
  #       print(dim(fac))
  numer=w[,..numericos]
  #       print(dim(numer))
  if(ncol(fac)>0){
    for(i in 1:ncol(fac)){
      fac[,names(fac)[i] :=convert_fac_num(unlist(fac[,..i]))]
    }
    fac=data.table(fac,numer)
    cor(fac,use = "pairwise.complete.obs")
    
    
  }
}

matriz_correlacao_completa_table=function(w){
  categoricos=which(sapply(w,class)=="character" )
  w=as.data.frame(w)
  if(length(categoricos)>0){
    for(i in categoricos)
      w[,i]=as.factor(w[,i])
  }
  #print(dim(w))
  fac=sapply(w,as.numeric)  
  return( cor(fac,use = "pairwise.complete.obs") )
  
  
  
}

convert_fac_num=function(y){
  c=c()
  cont=1
  #print(unique(y))
  for(i in unique(y))
  {
    c[which(y==i)]=cont
    cont=cont+1
  }
  return(c)
}



#funcoes para calculo do melhor ajuste

#emsemble de modelos
modelos=function(eixox,eixoy,limitepol){ # pede o eixo x, y pro fit e caso espandiamos em uma serie de taylor qual o limite da ordem
  x=list()
  x2=list()
  #eixoy=eixoy[which(eixox!=0)]
  #eixox=eixox[which(eixox!=0)]
  #eixox=eixox[which(eixoy!=0)]
  #eixoy=eixoy[which(eixoy!=0)]
  x[[1]]=lm(eixoy~eixox)
  x2[[1]]="y=ax+b"
  index=1
  if(sum(exp(eixox)==Inf)==0){
    index=index+1
    x[[index]]=lm(eixoy~exp(eixox))
    x2[[index]]="y=a*exp(bx)"
  }
  if(sum(eixox==0)==0){
    index=index+1
    x[[index]]=lm(eixoy~log(eixox))
    x2[[index]]="y=a*log(bx)"
  }
  #ao inserir novos modelos coloque os aqui e apos modifique o valor de tamanho
  tamanho=length(x) 
  for(i in 1:limitepol){
    x[[i+tamanho]]=lm(eixoy~poly(eixox,i+1,raw = TRUE))
    str=paste("y=poly(x^",i+1,")",sep="")
    x2[[i+tamanho]]=str
  }
  if(sum(eixox==0)==0){
    w=1/eixox
    k=length(x)
    x[[k+1]]=lm(eixoy~w)
    x2[[k+1]]="y=a/x+b"
    x[[k+2]]=lm(eixoy~exp(w))
    x2[[k+2]]="y=a/exp(x)+b"
    x[[k+3]]=lm(eixoy~log(w))
    x2[[k+3]]="y=a/log(x)+b"
    tamanho=length(x)
    for(i in 1:limitepol){
      x[[i+tamanho]]=lm(eixoy~poly(w,i+1,raw=TRUE ) )
      str=paste("y=poly(1/x^",i+1,")",sep="")
      x2[[i+tamanho]]=str
    }
    tamanho=length(x)
    for(i in 1:limitepol){
      
      x[[i+tamanho]]=lm(eixoy~exp(poly(w,i+1,raw=TRUE)))
      str=paste("y=exp(poly(1/x^",i+1,"))",sep="")
      x2[[i+tamanho]]=str
    }
    tamanho=length(x)
    if(sum(eixox==0)==0){
      for(i in 1:limitepol){
        
        x[[i+tamanho]]=lm(eixoy~log(poly(w,i+1,raw=TRUE)))
        str=paste("y=log(poly(1/x^",i+1,"))",sep="")
        x2[[i+tamanho]]=str
      }
    }
  }
  tamanho=length(x)
  for(i in 1:limitepol){
    if(sum(exp(poly(eixox,i+1,raw=TRUE))==Inf)==0){
      
      x[[i+tamanho]]=lm(eixoy~exp(poly(eixox,i+1,raw = TRUE)))
      str=paste("y=exp(poly(x^",i+1,")",sep="")
      x2[[i+tamanho]]=str
    }
  }
  
  tamanho=length(x)
  if(sum(eixox==0)==0){
    for(i in 1:limitepol){
      
      x[[i+tamanho]]=lm(eixoy~log(poly(eixox,i+1,raw=TRUE)))
      str=paste("y=log(poly(x^",i+1,"))",sep="")
      x2[[i+tamanho]]=str
    }
  }
  tamanho=length(x)
  #combinacoes exp log
    for(i in 1:limitepol){
      for(j in 1:limitepol)
      tryCatch({
        x[[(i-1)*limitepol+j+tamanho]]=lm(eixoy~exp(poly(eixox,i,raw=TRUE)):log(poly(eixox,j,raw=TRUE))  )
        str=paste("y=exp(poly(x^",i,")","*","log(poly(x^",j,")",sep="")
        x2[[(i-1)*limitepol+j+tamanho]]=str
    },error=function(e){})
    }
  
# combinacoes exp x  
  tamanho=length(x)
  
  for(i in 1:limitepol){
    for(j in 1:limitepol)
      tryCatch({
      x[[(i-1)*limitepol+j+tamanho]]=lm(eixoy~exp(poly(eixox,i,raw=TRUE)):poly(eixox,j,raw=TRUE)  )
      str=paste("y=exp(poly(x^",i,")","*","poly(x^",j,")",sep="")
      x2[[(i-1)*limitepol+j+tamanho]]=str
      },error=function(e){})
  }
  
  # combinacoes log x  
  tamanho=length(x)
  
  for(i in 1:limitepol){
    for(j in 1:limitepol)
      tryCatch({
      x[[(i-1)*limitepol+j+tamanho]]=lm(eixoy~log(poly(eixox,i,raw=TRUE)):poly(eixox,j,raw=TRUE)  )
      str=paste("y=log(poly(x^",i,")","*","poly(x^",j,")",sep="")
      x2[[(i-1)*limitepol+j+tamanho]]=str
  },error=function(e){})
  }
  
  # combinacoes exp 1/x  
  tamanho=length(x)
  
  for(i in 1:limitepol){
    for(j in 1:limitepol)
      tryCatch({
      x[[(i-1)*limitepol+j+tamanho]]=lm(eixoy~exp(poly(eixox,i,raw=TRUE)):poly(w,j,raw=TRUE)  )
      str=paste("y=exp(poly(x^",i,")","*","poly(1/x^",j,")",sep="")
      x2[[(i-1)*limitepol+j+tamanho]]=str
  },error=function(e){})
  }
  
  # combinacoes log 1/x  
  tamanho=length(x)
  
  for(i in 1:limitepol){
    for(j in 1:limitepol)
      tryCatch({
      x[[(i-1)*limitepol+j+tamanho]]=lm(eixoy~log(poly(eixox,i,raw=TRUE)):poly(w,j,raw=TRUE)  )
      str=paste("y=log(poly(x^",i,")","*","poly(1/x^",j,")",sep="")
      x2[[(i-1)*limitepol+j+tamanho]]=str
      },error=function(e){})
  }
  
  # combinacoes exp1/x x  
  tamanho=length(x)
  
  for(i in 1:limitepol){
    for(j in 1:limitepol)
      tryCatch({
      x[[(i-1)*limitepol+j+tamanho]]=lm(eixoy~exp(poly(w,i,raw=TRUE)):poly(eixox,j,raw=TRUE)  )
      str=paste("y=exp(poly(1/x^",i,")","*","poly(x^",j,")",sep="")
      x2[[(i-1)*limitepol+j+tamanho]]=str
      },error=function(e){})
  }
  
  # combinacoes log1/x x  
  tamanho=length(x)
  
  for(i in 1:limitepol){
    for(j in 1:limitepol)
      tryCatch({
      x[[(i-1)*limitepol+j+tamanho]]=lm(eixoy~log(poly(w,i,raw=TRUE)):poly(eixox,j,raw=TRUE)  )
      str=paste("y=log(poly(1/x^",i,")","*","poly(x^",j,")",sep="")
      x2[[(i-1)*limitepol+j+tamanho]]=str
      },error=function(e){})
  }
  
  # combinacoes exp1/x 1/x  
  tamanho=length(x)
  
  for(i in 1:limitepol){
    for(j in 1:limitepol)
      tryCatch({
      x[[(i-1)*limitepol+j+tamanho]]=lm(eixoy~exp(poly(w,i,raw=TRUE)):poly(w,j,raw=TRUE)  )
      str=paste("y=exp(poly(1/x^",i,")","*","poly(1/x^",j,")",sep="")
      x2[[(i-1)*limitepol+j+tamanho]]=str
      },error=function(e){})
  }
  
  # combinacoes log1/x 1/x  
  tamanho=length(x)
  
  for(i in 1:limitepol){
    for(j in 1:limitepol)
      tryCatch({
      x[[(i-1)*limitepol+j+tamanho]]=lm(eixoy~log(poly(w,i,raw=TRUE)):poly(w,j,raw=TRUE)  )
      str=paste("y=log(poly(1/x^",i,")","*","poly(1/x^",j,")",sep="")
      x2[[(i-1)*limitepol+j+tamanho]]=str
  },error=function(e){})
  }
  cont=1
  x3=list()
  for(i in 1:length(x)){
    if(!is.null(x[[i]])){
    x3[[cont]]=x[[i]]
    cont=cont+1
    }
  }
  x=x3
  w=list()
  w[[1]]=x # modelos
  w[[2]]=x2 #nomes
  return(w)
}
#corta fit que deu problema
elimina_Fit_falho=function(modelos){
  vec=c()
  cont=1
  for(i in 1:length(modelos)){
    ll=sum(is.na(modelos[[i]]$coefficients))
    if(ll==0){
      vec[cont]=i
      cont=cont+1
    }
  }
  return(vec)
  
}
#corta fit com erros no predict
elimina_erros=function(modelos,x){
  vec=c()
  cont=1
  v=data.frame(x)
  for(i in 1:length(modelos)){
    tryCatch({
      ll=predict(modelos[[i]], data=x)
      if( sum(is.na(ll))==0 & sum(ll==Inf)==0  ){
        vec[cont]=i
      cont=cont+1}
    },error=function(e){})
  }
  return(vec)
  
}
#seleciona o que maximiza f value
seleciona_melhor=function(x,y,limitepol,numero)
{
  conjunto=modelos(x,y,limitepol)
  #print("mensagem")
  #print(conjunto)
  #print(conjunto[[1]])
  candidatos=conjunto[[1]]
  nomes_modelos=conjunto[[2]]
  print(candidatos)
  print("fit falho")
  indices=elimina_Fit_falho(candidatos)
  candidatos=candidatos[indices]
  nomes_modelos=nomes_modelos[indices]
  print(candidatos)
  indices=elimina_erros(candidatos,x)
  candidatos=candidatos[indices]
  nomes_modelos=nomes_modelos[indices]
  print("erros")
  print(candidatos)
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
  nomes_ordenados=nomes_modelos[order(vec,decreasing=TRUE)]
  #print(candidatos_ordenados)
  #print(candidatos[1:numero])
  print("terminou F Value")
  
  u=list()
  if(numero<length(candidatos_ordenados))
    u=list(candidatos_ordenados[1:numero],nomes_ordenados[1:numero])
  else
    u=list(candidatos_ordenados,nomes_ordenados)
  
  return(u)
}
#seleciona o que maximiza R squared
seleciona_melhor_rs=function(x,y,limitepol,numero)
{
  conjunto=modelos(x,y,limitepol)
  #print("mensagem")
  #print(conjunto)
  #print(conjunto[[1]])
  candidatos=conjunto[[1]]
  nomes_modelos=conjunto[[2]]
  print(candidatos)
  print("fit falho")
  indices=elimina_Fit_falho(candidatos)
  candidatos=candidatos[indices]
  nomes_modelos=nomes_modelos[indices]
  print(candidatos)
  indices=elimina_erros(candidatos,x)
  candidatos=candidatos[indices]
  nomes_modelos=nomes_modelos[indices]
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
  nomes_ordenados=nomes_modelos[order(vec,decreasing=TRUE)]
  #print(candidatos_ordenados)
  #  print(candidatos_ordenados[1:numero])
  print("terminou R squared")
  u=list()
  if(numero<length(candidatos_ordenados))
    u=list(candidatos_ordenados[1:numero],nomes_ordenados[1:numero])
  else
    u=list(candidatos_ordenados,nomes_ordenados)
  return(u)
#    return(candidatos_ordenados[1:numero])
#  else
#    return(candidatos_ordenados)
}


fit_melhor_caso=function(x,y,cores,limitepol,numero,metrica,nomeX,nomeY){
 # melhor_modelo1=seleciona_melhor(x,y,limitepol,numero)
  if(metrica=="R squared")
    melhor_modelo=seleciona_melhor_rs(x,y,limitepol,numero)
  else if(metrica=="F Value")
    melhor_modelo=seleciona_melhor(x,y,limitepol,numero)
  print(melhor_modelo)
  v=data.frame(x,y)
  nomes_melhor=melhor_modelo[[2]]
  melhor_modelo=melhor_modelo[[1]]
  #print(melhor_modelo)
  #plot(x,y)
  #print(melhor_modelo)
  sem0=v#v[which(v[,1]!=0),]
  ll=predict(melhor_modelo[[1]], data=x)
  u=data.frame(sem0[,1],ll,nomes_melhor[[1]])
  names(u)=c("x","y","modelo")
  if(length(melhor_modelo)>1){
    for(i in 2:length(melhor_modelo)){
      #lines(v[,1], predict(melhor_modelo2[[i]], data.frame(x=v[,1])), col="red")
      #  aux=data.frame(v[,1],predict(melhor_modelo2[[i]], data.frame(x=v[,1])),toString(melhor_modelo2$call$formula) )
      ll=predict(melhor_modelo[[i]], data=x)
      #print(ll)v
      aux=data.frame(sem0[,1],ll,nomes_melhor[[i]] )
      names(aux)=names(u)
      #  print(aux)
      u=rbind(u,aux)
     print(nrow(u))
      print(nrow(v))
    }
  }
  print(dim(u))
  #print(u)
  
  #write.table(file="meta.dat",u,row.names = FALSE)
  #print(u[1:201,])
  auxiliar=nrow(u)/nrow(v)
  print(auxiliar)
  auxiliar=auxiliar-1
  v0=v
  if(auxiliar>0)
  for(i in 1:auxiliar){
    v=rbind(v,v0)
  }
  print(nrow(v))
  print(nrow(u))
  print(c(nomeX,nomeY))
  ggplot(data=u) +geom_count(aes(x=v[,1],y=v[,2])) +geom_line(aes(x=u[,1],y=u[,2],col=u[,3] )  ) + labs(x=nomeX,y=nomeY,col="Modelos")
}


fit_melhor_caso_table=function(x,y,cores,limitepol,numero,metrica,nomeX,nomeY){
 # melhor_modelo1=seleciona_melhor(x,y,limitepol,numero)
  if(metrica=="R squared")
    melhor_modelo=seleciona_melhor_rs(x,y,limitepol,numero)
  else if(metrica=="F Value")
    melhor_modelo=seleciona_melhor(x,y,limitepol,numero)
  print(melhor_modelo)
  v=data.table(x,y)
  nomes_melhor=melhor_modelo[[2]]
  melhor_modelo=melhor_modelo[[1]]
  #print(melhor_modelo)
  #plot(x,y)
  #print(melhor_modelo)
  sem0=v#v[which(v[,1]!=0),]
  ll=predict(melhor_modelo[[1]], data=x)
  u=data.table(sem0[,1],ll,nomes_melhor[[1]])
  names(u)=c("x","y","modelo")
  if(length(melhor_modelo)>1){
    for(i in 2:length(melhor_modelo)){
      #lines(v[,1], predict(melhor_modelo2[[i]], data.frame(x=v[,1])), col="red")
      #  aux=data.frame(v[,1],predict(melhor_modelo2[[i]], data.frame(x=v[,1])),toString(melhor_modelo2$call$formula) )
      ll=predict(melhor_modelo[[i]], data=x)
      #print(ll)v
      aux=data.table(sem0[,1],ll,nomes_melhor[[i]] )
      names(aux)=names(u)
      #  print(aux)
      u=rbind(u,aux)
     print(nrow(u))
      print(nrow(v))
    }
  }
  print(dim(u))
  #print(u)
  
  #write.table(file="meta.dat",u,row.names = FALSE)
  #print(u[1:201,])
  auxiliar=nrow(u)/nrow(v)
  print(auxiliar)
  auxiliar=auxiliar-1
  v0=v
  if(auxiliar>0)
  for(i in 1:auxiliar){
    v=rbind(v,v0)
  }
  print(nrow(v))
  print(nrow(u))
  print(c(nomeX,nomeY))
  ggplot(data=u) +geom_count(aes(x=v[,1],y=v[,2])) +geom_line(aes(x=u[,1],y=u[,2],col=u[,3] )  ) + labs(x=nomeX,y=nomeY,col="Modelos")
}
#fim destas

min.f1f2 <- function(x, mu1, mu2, sd1, sd2) {
  f1 <- dnorm(x, mean=mu1, sd=sd1)
  f2 <- dnorm(x, mean=mu2, sd=sd2)
  pmin(f1, f2)
}
# function to calculate the coefficient of intersection(how much of the data is intersected)
#to know it use the code integrate(min.f1f2, -Inf, Inf, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2)


featurizer=function(df,meta,contra,separacao){
  p1=1.0
  p2=1.0
  b=df[1:(nrow(df)-nrow(df)%%separacao),]
  b=b[order(b[,contra]),]
  analise=matrix(b[,meta],ncol=separacao)
  if(class(df[1,meta])=='numeric'){
    medias=apply(analise,2,mean,na.rm=TRUE)
    desvio=apply(analise,2,sd,na.rm=TRUE)
    desvio[which(is.na(desvio))]=mean(desvio,na.rm = TRUE)
    medias[which(is.na(medias))]=mean(medias,na.rm = TRUE)
    #	print(c(medias,desvio))
    contador=0
    ntermos=0
    for(i in 1:(length(medias)-1))
    {
      for(j in (i+1):length(medias))
      {
        # print(c(medias[i],"mediai"))
        # print(c(medias[i],"mediaj"))
        # print(c(desvio[i],"desvio"))
        # print(c(desvio[j],"desvioj"))
        hu=integrate(min.f1f2, -Inf, Inf, mu1=medias[i], mu2=medias[j], sd1=desvio[i], sd2=desvio[j],stop.on.error = FALSE )
        if(!is.na(hu$value)){
          contador=contador+ (1-hu$value )
          ntermos=ntermos+1 }
      }
      
    }
    return (contador/ntermos)
    #return (p1*sd(medias,na.rm = TRUE) -p2*mean(desvio,na.rm = TRUE)) # maximizar a distancia entre as diferentes medias mas minimizar o desvio padrão da relacao
  }
  else{
    tabular=apply(analise,2,table)
    desvio=c()
    medias=c()
    for(i in 1:length(tabular)){
      desvio[i]=sd(as.numeric(unlist(tabular[i]) ),na.rm=TRUE )
      medias[i]=mean(as.numeric(unlist(tabular[i]) ),na.rm=TRUE )
    }
    desvio[which(is.na(desvio))]=mean(desvio,na.rm = TRUE)
    medias[which(is.na(medias))]=mean(medias,na.rm = TRUE)
    contador=0
    ntermos=0
    for(i in 1:(length(medias)-1))
    {
      for(j in (i+1):length(medias))
      {
        
        hu=integrate(min.f1f2, -Inf, Inf, mu1=medias[i], mu2=medias[j], sd1=desvio[i], sd2=desvio[j],stop.on.error = FALSE )
        if(!is.na(hu$value)){
          contador=contador+ (1-hu$value )
          ntermos=ntermos+1 }
      }
      
    }
    return (contador/ntermos)
    #return(sd(desvio))
  }}

featurizer_table=function(df,meta,contra,separacao){ #requires a data.table
  p1=1.0
  p2=1.0
  b=df[1:(nrow(df)-nrow(df)%%separacao),]
  b=b[order(b[,..contra]),]
  analise=matrix(b[,..meta],ncol=separacao)
  if(class(df[1,..meta])=='numeric'){
    medias=apply(analise,2,mean,na.rm=TRUE)
    desvio=apply(analise,2,sd,na.rm=TRUE)
    desvio[which(is.na(desvio))]=mean(desvio,na.rm = TRUE)
    medias[which(is.na(medias))]=mean(medias,na.rm = TRUE)
    #	print(c(medias,desvio))
    contador=0
    ntermos=0
    for(i in 1:(length(medias)-1))
    {
      for(j in (i+1):length(medias))
      {
        # print(c(medias[i],"mediai"))
        # print(c(medias[i],"mediaj"))
        # print(c(desvio[i],"desvio"))
        # print(c(desvio[j],"desvioj"))
        hu=integrate(min.f1f2, -Inf, Inf, mu1=medias[i], mu2=medias[j], sd1=desvio[i], sd2=desvio[j],stop.on.error = FALSE )
        if(!is.na(hu$value)){
          contador=contador+ (1-hu$value )
          ntermos=ntermos+1 }
      }
      
    }
    return (contador/ntermos)
    #return (p1*sd(medias,na.rm = TRUE) -p2*mean(desvio,na.rm = TRUE)) # maximizar a distancia entre as diferentes medias mas minimizar o desvio padrão da relacao
  }
  else{
    tabular=apply(analise,2,table)
    desvio=c()
    medias=c()
    for(i in 1:length(tabular)){
      desvio[i]=sd(as.numeric(unlist(tabular[i]) ),na.rm=TRUE )
      medias[i]=mean(as.numeric(unlist(tabular[i]) ),na.rm=TRUE )
    }
    desvio[which(is.na(desvio))]=mean(desvio,na.rm = TRUE)
    medias[which(is.na(medias))]=mean(medias,na.rm = TRUE)
    contador=0
    ntermos=0
    for(i in 1:(length(medias)-1))
    {
      for(j in (i+1):length(medias))
      {
        
        hu=integrate(min.f1f2, -Inf, Inf, mu1=medias[i], mu2=medias[j], sd1=desvio[i], sd2=desvio[j],stop.on.error = FALSE )
        if(!is.na(hu$value)){
          contador=contador+ (1-hu$value )
          ntermos=ntermos+1 }
      }
      
    }
    return (contador/ntermos)
    #return(sd(desvio))
  }}

#b=a[,which( sapply(a,class)=="numeric"|sapply(a,class)=="integer")]
grupo=function(a,meta,sep){
  b=a
  frame=data.frame(1,2,3)
  opcao=meta
  for(i in 1:ncol(b)){
    frame[i,]=c(names(b)[i],featurizer(b,opcao,i,sep),class(b[,i]))
    
    
  }
  
  aux=as.numeric(frame[opcao,2])
  frame[,2]=as.numeric(frame[,2])/aux
  frame=frame[order(frame[,2],frame[,3]),]
  return(frame)
}

grupo_table=function(a,meta,sep){
  b=a
  frame=data.table(1,2,3)
  opcao=meta
  for(i in 1:ncol(b)){
    frame[..i,]=c(names(b)[i],featurizer_table(b,opcao,i,sep),class(b[,..i]))
    
    
  }
  
  aux=as.numeric(frame[..opcao,2])
  frame[,2]=as.numeric(..frame[,2])/aux
  frame=frame[order(..frame[,2],..frame[,3]),]
  return(frame)
}

