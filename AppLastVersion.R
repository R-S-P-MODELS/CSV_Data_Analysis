#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

### bootstrap para mensagems de loading


# Now to support data tables

require(data.table)


#
###inicio de definicao de funcao


#funcao para completar variaveis categoricas
source("Encontrar_candidatos_dataset_v1.R")

#require(gg3D)

GrafoCorrelacao<-function(w,tresh=0.8){
  m=Matriz_Correlacao_retorno(w)
  require(igraph)
  require(reshape2)
  require(ggraph)
  m=melt(m)
  names(m)[3]='cooc'
  m$cooc=abs(m$cooc)
  m=m[m$cooc>tresh,]
  wordnetwork=graph_from_data_frame(m)
  
  p<-ggraph(wordnetwork, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
    geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    theme(legend.position = "none") 
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
###fim da definicao de funcoes

options(warn=-1)
library(shiny)
library(plotly)
#require(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
   # Application title
   titlePanel("Exploratory Data Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      #  actionButton("botaoleitura",label = "Iniciar analise"),
        fileInput("file1", "Insira o arquivo a ser analisado.csv",
                  accept = c(
                    "text/csv",".csv") ),
        checkboxInput("showsummary", "Show Summary", FALSE),
        checkboxInput("showcorrelation", "Show correlation matrix", FALSE),
        checkboxInput("showna","Show the relative amount of missing data",FALSE),
       # checkboxInput("showfeature","Show the featurizer(uses x as variable to predict)",FALSE),
        checkboxInput("showfunctional","Show the functional dependency(uses x as variable to predict)",FALSE),
	checkboxInput("showbestvector","Show The minimum set of aproximate functional dependency(uses x as variable to predict)",FALSE),
	 conditionalPanel(
         condition = "input.showbestvector == true",
         sliderInput("veclength", "Maximum number of collumns", min=1, max=50, value=1),
	sliderInput("percent","Percentage of unique values needed to consider a collumn a primary key and reject it on the set",min=1,max=100,value=100),
	 checkboxInput("numtrigger","Set TRUE to reject elements that show only once",TRUE),
	 checkboxInput("runbest","Find best set, check to Run",FALSE)
      ),
        checkboxInput("generate","Generate best Graph",FALSE),
        conditionalPanel(
          condition = "input.generate == true",
       sliderInput("poly",label = "Termos maximos da expansão",min = 1,max=20,value=2)),
        

       conditionalPanel(
         condition = "input.generate == true",
	  uiOutput("NTermos"),
         selectInput("metrica",label = "Metricas",choices = c("R squared","F Value"))
		
),
        
        conditionalPanel(
          condition = "is.null(input.file1) == true",
          checkboxInput("histogram","Show histogram of x")),
        uiOutput("Eixox"),
        uiOutput("Eixoy"),
        uiOutput("Eixoz"),
        uiOutput("cores"),
        uiOutput("tamanhos"),
        actionButton("preencherdados","Completar Dataset"),
        downloadButton("downloadData", "Download do dataset completado")
        ),
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id="Referenciador",
                    tabPanel("Data Exploration",plotlyOutput("distPlot")),
                    tabPanel("PCA Visualization",plotlyOutput("PCAPlot"),verbatimTextOutput("Outliers")  ),
		    tabPanel("Correlation Matrix Image",
		             #selectInput("OpcaoMatriz",label = "Forma de vizualizar",choices=c('Matriz','Grafo')),
		             plotlyOutput("MatrixPlot"),
		             plotOutput('GrafoCorrelacaov1') ),
		    tabPanel("3D plots",plotlyOutput("threeDplots"))
        ),
         conditionalPanel(
           condition = "input.showsummary == true",
         verbatimTextOutput("sum")),
         conditionalPanel(
           condition = "input.showcorrelation == true",
         verbatimTextOutput("corre")),
         verbatimTextOutput("nas"),
         verbatimTextOutput("grupos"),
         verbatimTextOutput("grupos_alt"),
	 verbatimTextOutput("bestvec"),
         h3("Aplicativo desenvolvido por Rafael Silva Pereira!\n\n"),
         h4("Em caso de duvidas ou problemas favor entrar em contato\n\n"),
         h4("Para gerar o grafico a primeira vez clique para completar o dataset, não será nescessario para futuras explorações\n\n"),
         h4("r.s.p.models@gmail.com")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=1000000*1024^2)
  output$NTermos=renderUI({
    sliderInput("opcoes",label = "Numero de modelos a ser plotado",min = 1,max=3*input$poly,value=1)
    
  })
  
  LeituraArquivo<-reactive(
    {
      if(!is.null(input$file1)){
        #funcoes_reativas()
	print("feito leitura")
        w=fread(input$file1$datapath,header=TRUE)
        return(unique(w) )

      }
      
      
    }
    
  )

  LeituraArquivoCsv<-reactive(
    {
      if(!is.null(input$file1)){
        #funcoes_reativas()
	print("feito leitura")
     #   w=fread(input$file1$datapath,header=TRUE)
        w=LeituraArquivo()
	w1=as.data.frame(w)
        return(unique(w1) )

      }
      
      
    }
    
  )

output$bestvec <- renderPrint({
     if(!is.null(input$file1)){
       if(input$showbestvector==TRUE){
	if(input$runbest==TRUE){
	#cat("tentarei ler\n")
	percentage=input$percent/100.0
	showModal(modalDialog("The code is currently running please wait", footer=NULL))
	w=LeituraArquivoCsv()
#	print("lido")
	Goal=which(names(w)==input$X)
	if(length(unique(w[,Goal]))==nrow(w)){
		cat("the variable",names(w)[Goal],"is a primary key of the dataset\n")
		#return(0)
		removeModal()
	}
	else{
	w=w[,as.numeric(which(sapply(sapply(w,unique),length) <percentage*nrow(w)))] # eliminating primary keys
	Goal=which(names(w)==input$X)
#	print("pre best")
	z=best_vector(w,Goal,as.numeric(input$veclength),nrow(w),as.numeric(input$numtrigger))
	if(z==0)
		z=best_vector(w,Goal,as.numeric(input$veclength),nrow(w),0)
#	print("foi best")
  	x=names(w)[z]
#	print("vai no mean")
	y=MeanAccuracy(w,z,Goal)
       cat("The best set is",x,"it predicts",names(w)[Goal],"with",y*100,"% Accuracy\n")}
       removeModal()}
       }}}
   )

  output$separacoes=renderUI({
    if(!is.null(input$file1)){
   if(input$showfeature == TRUE){
     w=LeituraArquivoCsv()
     
     sliderInput(inputId = "separacoes",min =2,max =0.5*nrow(w),value = 2,label="numero de subconjuntos do dataset"  ) 
   }
  }}
  )
  output$tamanhos = renderUI({
    if(!is.null(input$file1)){
      w=LeituraArquivo()
      a=c()
      for(i in 1:length(w[,1]))
        a[i]=1
      aux=data.table(w,a)
      names(aux)[length(aux)]='None'
 #   selectInput("X", label = "X",choices = names(aux))
    
  #  selectInput("Y", label = "Y",choices = names(aux))
    
   # selectInput("color", label = "Color",choices = names(aux))
      if(input$histogram==FALSE){
        if(input$Referenciador=="Data Exploration")
          selectInput("size", label = "Size",
                      choices = names(aux))
        else if(input$Referenciador=="3D plots"){
          Indices=sapply(aux,class) %in% c('numeric','integer')
          selectInput("size", label = "Size",
                      choices = names(aux[,..Indices]))
          
        }
        else{
          logic=lapply(w,class) %in% c("numeric","integer")
          logic=!logic
          steps=which(logic)
          selectInput("size", label = "Label",
                      choices = c('None',names(aux)[steps]) )
        }
      }
    }
     })
  output$cores = renderUI({
    if(!is.null(input$file1)){
      #w=LeituraArquivoCsv()
      w=LeituraArquivo()
      a=c()
      for(i in 1:length(w[,1]))
        a[i]=1
      aux=data.table(a,w)
      names(aux)[1]='None'
      # selectInput("X", label = "X",choices = names(aux))
      
      # selectInput("Y", label = "Y",choices = names(aux))
      if(input$Referenciador=="PCA Visualization"){
        #Classes=which(as.numeric(sapply(w,class) %in% c('character','factor')))
        #print(c("Classe,",Classes) )
        #Classes=c(Classes,ncol(aux))
        #print(names(aux)[Classes])
        #selectInput("color", label = "Color",choices = names(aux)[Classes])
        numericInput("color",label="Number of components",value = 2,min=2,max=ncol(w))
      }
      else{
        selectInput("color", label = "Color",choices = names(aux))
      }
      
      
      
    }
  })
  
  output$Eixoy = renderUI({
    if(!is.null(input$file1)){
      w=LeituraArquivo()
      #w=LeituraArquivoCsv()
      # selectInput("X", label = "X",choices = names(w))
      if(input$histogram==FALSE){
        if(input$Referenciador=="PCA Visualization"){
          Results=GerarPCA()
          step=Results$Variables
          w=as=data.frame(w)
          w=w[,step]
          names(w)=paste('Component',1:ncol(w),sep="")
          #step=which(lapply(w,class) %in% c("numeric","integer"))
          selectInput("Y", label = "Y",choices = names(w))
        }
        else{
          selectInput("Y", label = "Y",choices = names(w))
        }
        
        #selectInput("Y", label = "Y",choices = names(w))
      }
      
      
      
    }
  })
  
  output$Eixoz = renderUI({
    if(!is.null(input$file1)){
      if(input$Referenciador=="3D plots"){
      w=LeituraArquivo()
      #w=LeituraArquivoCsv()
      # selectInput("X", label = "X",choices = names(w))
      if(input$histogram==FALSE){
        if(input$Referenciador=="PCA Visualization"){
          step=which(lapply(w,class) %in% c("numeric","integer"))
          selectInput("Z", label = "Z",choices = names(w[,..step]))
        }
        else{
          selectInput("Z", label = "Z",choices = names(w))
        }
        
        #selectInput("Y", label = "Y",choices = names(w))
      }
      
      }
      
    }
  })
  
  output$Eixox = renderUI({
    if(!is.null(input$file1)){
      w=LeituraArquivo()
      #w=LeituraArquivoCsv()
      if(input$Referenciador=="PCA Visualization"){
        Results=GerarPCA()
        step=Results$Variables
        w=as=data.frame(w)
        w=w[,step]
        names(w)=paste('Component',1:ncol(w),sep="")
        #step=which(lapply(w,class) %in% c("numeric","integer"))
        selectInput("X", label = "X",choices = names(w))
        #selectInput("X", label = "X",choices = names(w[,which(lapply(w,class) %in% c("numeric","integer"))]))
      }
      else{
        selectInput("X", label = "X",choices = names(w))
      }
      
      
      
    }
  })
  
  
  
  
  
  
  

   
   
   output$sum <- renderPrint({
     if(!is.null(input$file1)){
       if(input$showsummary==TRUE){
       
     w=LeituraArquivo()
    # showModal(modalDialog("The code is currently running please wait", footer=NULL))
     summary(w)
     #removeModal()
     }}
   })
   
   output$grupos <- renderPrint({
     if(!is.null(input$file1)){
       if(input$showfeature==TRUE){
         
         w=LeituraArquivo()
         eixoX=which(names(w)==input$X)
         print(grupo(w,eixoX,input$separacoes))
       }}
   })
   
   output$grupos_alt <- renderPrint({
     if(!is.null(input$file1)){
       if(input$showfunctional==TRUE){
         
         w=LeituraArquivo()
         eixoX=which(names(w)==input$X)
         print(predicao_importancia_v2_table(w,eixoX))
       }}
   })
   output$corre <- renderPrint({
     if(!is.null(input$file1)){
       if(input$showcorrelation==TRUE){
        # 	showModal(modalDialog("The code is currently running please wait", footer=NULL))
       w=LeituraArquivo()
       #e=w[,..as.numeric(which(sapply(w,class)=="integer" | sapply(w,class)=="numeric") )]
#	w=data.frame(w)
       matriz_correlacao_completa_table(w)}
	#removeModal()
      # cor(e, use = "pairwise.complete.obs")     }
   }})
   
   
   
   output$nas <- renderPrint({
     if(!is.null(input$file1)){
       if(input$showna==TRUE){
         
         w=LeituraArquivo()
	showModal(modalDialog("The code is currently running please wait", footer=NULL))
         for(i in 1:ncol(w))
         cat("O numero percentual de elementos NA na coluna", names(w)[i], "é", sum(is.na(w[,..i]))/length(w[,..i]),"\n" )
	removeModal()
       }}
   })

   
     completar_distribuicao<- function(arquivo_ori){ # arquivo_ori e a distribuicao original
     w=arquivo_ori
     for(i in 1:ncol(w)){
       c=w[,i]
       b=which(is.na(c)) #indices
       distribuicao_original=hist(c,breaks=20,plot=FALSE)
       aux=distribuicao_original$counts/sum(distribuicao_original$counts)
       vetor_de_probabilidades=cumsum(aux) # probabilidade acumulada do histograma
       #print("probabilidades")
       #print(vetor_de_probabilidades)
       chutes=runif(length(b),min =vetor_de_probabilidades[1],max=1 ) #numeros aleatorios entre 0 e 1
       #print("chutes")
       #print(chutes)
       if(length(b)>0){
       for(j in 1:length(b)){
         cat("estou na linha",j/length(b),"da coluna",i/ncol(w),"\n")
         #preenchimento de novos valores
         localizacao=min(which(chutes[j]<vetor_de_probabilidades))
        # print("localizacao")
         #print(localizacao)
         #print(b[j])
         #print(distribuicao_original$breaks[localizacao-1])
         #print(distribuicao_original$breaks[localizacao])
         #entre localizacao-1 e localizacao esta nosso alvo
         c[b[j]]=runif(1,min=distribuicao_original$breaks[localizacao-1],max=distribuicao_original$breaks[localizacao])
       } # fim da atualizacao de uma coluna
      #   mn=!is.na(w[,i])
      #if(sum(w[mn,i]==round(w[mn,i]))==length(w[mn,i])){
      #  c=round(c)}
      w[,i]=c 
       }
     }
     return(w) # distribuicao sem NA
   }
   
completar_distribuicao_table<- function(arquivo_ori){ # arquivo_ori e a distribuicao original
     w=arquivo_ori
     withProgress(message = 'Inputing Values', value = 0, {
     for(i in 1:ncol(w)){
       print(i)
       c=unlist(w[,..i])
       b=which(is.na(c)) #indices
       distribuicao_original=hist(as.numeric(c),breaks=20,plot=FALSE)
       aux=distribuicao_original$counts/sum(distribuicao_original$counts)
       vetor_de_probabilidades=cumsum(aux) # probabilidade acumulada do histograma
       #print("probabilidades")
       #print(vetor_de_probabilidades)
       chutes=runif(length(b),min =vetor_de_probabilidades[1],max=1 ) #numeros aleatorios entre 0 e 1
       #print("chutes")
       #print(chutes)
       if(length(b)>0){
       for(j in 1:length(b)){
         cat("estou na linha",j/length(b),"da coluna",i/ncol(w),"\n")
         #preenchimento de novos valores
         localizacao=min(which(chutes[j]<vetor_de_probabilidades))
        # print("localizacao")
         #print(localizacao)
         #print(b[j])
         #print(distribuicao_original$breaks[localizacao-1])
         #print(distribuicao_original$breaks[localizacao])
         #entre localizacao-1 e localizacao esta nosso alvo
         c[b[j]]=runif(1,min=distribuicao_original$breaks[localizacao-1],max=distribuicao_original$breaks[localizacao])
       } # fim da atualizacao de uma coluna
      #   mn=!is.na(w[,i])
      #if(sum(w[mn,i]==round(w[mn,i]))==length(w[mn,i])){
      #  c=round(c)}
         w[,names(w)[i] :=c] 
       }
       incProgress(1/ncol(w), detail = paste("Inputting variable", names(w)[i] ) )
     }
       
     })
     return(w) # distribuicao sem NA
   }
   
   # anotacoes vc deve implementar o completar_distribuicao com a medida do data.table
   completar_reativo<-eventReactive(input$preencherdados,
    {
          showModal(modalDialog("The code is currently running please wait", footer=NULL))
       
         epislon=0.05
         w=LeituraArquivo()
       #  lixo_w=w
       #for(i in 1:ncol(w)){
      #     if(length(unique(w[,i]) )/length(w[,i]) < epislon & (class(w[,i])=="numeric"|class(w[,i])=="integer") & (sum(!is.na(w[,i]) )>0 )   ) # conversao para fatores caso existam elementos mas seja pouca variedade
      #       lixo_w[,i]=as.factor(w[,i])
           
      # }
       #  w=lixo_w
	 cordenadas_auxiliares=as.numeric(which(sapply(w,class)=="integer" | sapply(w,class)=="numeric") )
	 nomes=names(w)
	 if(length(cordenadas_auxiliares)>0){
         e=w[,..cordenadas_auxiliares]
         
         nomes=names(w)
         cont=0
         dados=completar_distribuicao_table(e)
         while(sum(is.na(dados))>0 & cont<6   ){
           dados=completar_distribuicao_table(dados)
           cont=cont+1
         }
         #atualizacao do dataframe original
         a=c()
         for(i in 1:length(names(e)) ){
           a[i]=which(names(e)[i]==names(w))
	   lixo=a[i]
	w[,names(w)[lixo] := dados[,..i] ]

         #  w[,..lixo]=dados[,..i]
         }
         print(dim(w))
         #factors
	 } 
         f=as.numeric(which(sapply(w,class)!="integer" & sapply(w,class)!="numeric") )
         print(f)
         print(names(w)[f])
         for(i in f){
           print(c(i,names(w)[i],class(i) ) )
           
           #encontrar qual coluna é dependencia funcional da minha meta se esta tem elemtnso nulos
           if(sum(is.na(w[,..i]))>0 &sum(is.na(w[,..i]))<length(w[,..i])  ){
            # print(c(i,names(w)[i],class(i) ) )
             l=predicao_importancia_v2_table(w,i)
            # print(l)
            # print(c(i,names(w)[i],class(i) ) )
             li=names(l)[which(max(as.numeric(l),na.rm = TRUE)==as.numeric(l))-1] # nome do  mais importante
             index=which(names(w)==li)
            # print(c(index,names(w)[index]))
             w[,names(w)[i] :=recuperar_categorica(w[,..index],w[,..i],0.1)]
           #  w[,names(w)[i] :=c] 
             
           }
           
         }
       
      
    #   a=which(names(w)==names(dados))
        
      
         names(w)=nomes
         print(dim(w)) 
       #w[,a]=dados[,a]
       write.csv(file="dataset_prenchido.csv",w,col.names = TRUE,row.names = FALSE)
	removeModal()
   })
   
   
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("dataset_prenchido", ".csv", sep = "")
     },
     content = function(file) {
       a=read.csv("dataset_prenchido.csv")
       write.csv(a, file, row.names = FALSE,col.names = TRUE)
     }
   )
   
   funcoes_reativas<- reactive({

     completar_reativo()
    # findBestSet()
     lixo=1

   })
   
   
   output$distPlot <- renderPlotly({
     #leitura()
     if(!is.null(input$file1)){
      #funcoes_reativas()
       completar_reativo()
       w=LeituraArquivo()
       a=c()
       for(i in 1:length(w[,1]))
         a[i]=1
       aux=data.table(w,a)
       print(dim(aux))
       names(aux)[length(aux)]='None'
       eixoX=which(names(w)==input$X)
       eixoY=which(names(w)==input$Y)
       colorido=which(names(aux)==input$color)
       tamanho=which(names(aux)==input$size)
	Auxiliar=unique(aux[,c(..eixoX,..eixoY,..colorido,..tamanho)])
	names(Auxiliar)=c("n1","n2","n3","n4")
	print(names(Auxiliar))
       if(input$histogram==FALSE){
         if(input$generate==FALSE){
         Graph=ggplot(data=Auxiliar) +geom_count(aes(x=unlist(Auxiliar[,1]),y=unlist(Auxiliar[,2]),color=unlist(Auxiliar[,3]),size=unlist(Auxiliar[,4])) )+labs(x=input$X,y=input$Y,colour=input$color,size=input$size )
         ggplotly(Graph,dragmode=pan)
         } 
          else
           fit_melhor_caso(unlist(Auxiliar[,1]),unlist(Auxiliar[,2]),cores =unlist(Auxiliar[,3]) ,limitepol =input$poly,numero = input$opcoes,metrica = input$metrica,nomeX=input$X,nomeY=input$Y )}
       else if(input$histogram==TRUE){
         Graph=ggplot(data=Auxiliar) +geom_histogram(aes(x=unlist(Auxiliar[,1]),fill=unlist(Auxiliar[,3])),stat = 'count' )+labs(x=input$X,fill=input$color )
         ggplotly(Graph,dragmode=pan)
       }
       
       
     }
     
     #plot(mtcars[,eixoX],mtcars[,eixoY],xlab=input$X,ylab=input$Y)
   })
   
   
   output$threeDplots <- renderPlotly({
     #leitura()
     if(!is.null(input$file1)){
       #funcoes_reativas()
       completar_reativo()
       w=LeituraArquivo()
       a=c()
       for(i in 1:length(w[,1]))
         a[i]=1
       aux=data.table(w,a)
       print(dim(aux))
       names(aux)[length(aux)]='None'
       eixoX=which(names(w)==input$X)
       eixoY=which(names(w)==input$Y)
       eixoZ=which(names(w)==input$Z)
       colorido=which(names(aux)==input$color)
       tamanho=which(names(aux)==input$size)
       Auxiliar=unique(aux[,c(..eixoX,..eixoY,..eixoZ,..colorido,..tamanho)])
       names(Auxiliar)=c("n1","n2","n3","n4","n5")
       print(names(Auxiliar))
       if(input$histogram==FALSE){
         if(input$generate==FALSE)
        #  ggplotly( ggplot(Auxiliar, aes(x=unlist(Auxiliar[,1]), y=unlist(Auxiliar[,2]), z=unlist(Auxiliar[,3]), color=unlist(Auxiliar[,4]),size=unlist(Auxiliar[,5]) ) )+ 
         #  theme_void() +
          # axes_3D() +
           #stat_3D() +
          #labs(x=input$X,y=input$Y,z=input$Z,colour=input$color,size=input$size ) )
           plot_ly(x=unlist(Auxiliar[,1]),y=unlist(Auxiliar[,2]),z=unlist(Auxiliar[,3]),color = unlist(Auxiliar[,4]),size = unlist(Auxiliar[,5]),type="scatter3d",mode="markers" ) %>% layout(
             title = "Data Visualization",
             scene = list(
               xaxis = list(title = input$X),
               yaxis = list(title = input$Y),
               zaxis = list(title = input$Z)
             ))
           #ggplot(data=Auxiliar) +geom_count(aes(x=unlist(Auxiliar[,1]),y=unlist(Auxiliar[,2]),color=unlist(Auxiliar[,3]),size=unlist(Auxiliar[,4])) )+labs(x=input$X,y=input$Y,colour=input$color,size=input$size )
         else
           fit_melhor_caso(unlist(Auxiliar[,1]),unlist(Auxiliar[,2]),cores =unlist(Auxiliar[,3]) ,limitepol =input$poly,numero = input$opcoes,metrica = input$metrica,nomeX=input$X,nomeY=input$Y )}
       else if(input$histogram==TRUE)
         ggplot(data=Auxiliar) +geom_histogram(aes(x=unlist(Auxiliar[,1]),fill=unlist(Auxiliar[,3])),stat = 'count' )+labs(x=input$X,fill=input$color )
       
       
       
     }
     
     #plot(mtcars[,eixoX],mtcars[,eixoY],xlab=input$X,ylab=input$Y)
   })
   
   GerarPCA<-reactive({
     source("PCAOtimizado.R")
     #funcoes_reativas()
     print(c("Grupos",input$color))
     #w=read.csv(input$file1$datapath,header=TRUE)
     w=LeituraArquivo()
     step=which(lapply(w,class) %in% c("numeric","integer"))
     # logic=lapply(w,class) %in% c("numeric","integer")
     # logic=!logic
     univariate=which(sapply(sapply(w[,..step],unique),length)==1)
     if(length(univariate)>0)
       step=step[-univariate]
     # steps=which(logic)
     PC = tryCatch({
       data=w[,..step]
       data=as.data.frame(scale(data,center=TRUE))
       PCA=NewPCA(data,input$color)
       print('Deu certo')
       step=step[1:input$color]
     }, error = function(e) {
       w1=prcomp(w[,..step],scale=TRUE)
       PCA=w1$x
     })
     #w1=prcomp(w[,..step],scale=TRUE)
     #PCA=w1$x
     Results=list(PCA=PCA,Variables=step)
     return(Results)
   })
   
   output$PCAPlot <- renderPlotly({
     #leitura()
     if(!is.null(input$file1)){
       w=LeituraArquivo()
       Results=GerarPCA()
       step=Results$Variables
       PCA=Results$PCA
       w2=w[,..step]
       
       names(w2)=paste('Component',1:ncol(w2),sep="")
      # carsHC <- hclust(dist(w1$scores), method = "ward.D2")
       #v=as.numeric(input$color)
      # carsClusters <- cutree(carsHC, k = v)
      # carsDf <- data.frame(w1$scores, "cluster" = factor(carsClusters))
      # carsDf <- transform(carsDf, cluster_name = paste("Cluster",carsClusters))
      # print(input$size)
      # print(names(w))
       LabelChoice=which(names(w)==input$size)
       print(LabelChoice)
       a=c()
       for(i in 1:length(w[,1]))
         a[i]=1
       aux=data.frame(w,a)
       names(aux)[length(aux)]='None'
       eixoX=which(names(w2)==input$X)
       eixoY=which(names(w2)==input$Y)
       colorido=which(names(aux)==input$size)
       print(input$size)
       print(names(aux))
       #names(w1$x)=names(w)
       #tamanho=which(names(aux)==input$size)
       PCA=as.data.frame(PCA)
       color=aux[,colorido ]
       names(PCA)=paste('Component',1:ncol(PCA),sep="")
       #print(c(length(w1$x[,eixoX]),length(w1$x[,eixoY]),length(aux[,colorido])  )   )
       p1 <-ggplot(PCA) + 
         theme_classic() +
         geom_hline(yintercept = 0, color = "gray70") +
         geom_vline(xintercept = 0, color = "gray70") +
         geom_count(aes_string(x=input$X, y=input$Y) , alpha = 0.55, size = 3) +
         aes(color = color) +
         xlab(paste("Component",eixoX) ) +
         ylab(paste("Component",eixoY)) +
         labs(col=names(aux)[colorido]) +
         ggtitle("PCA Clusters") 
       
       
       
       #p1 <- ggplot(carsDf,aes(x=carsDf[,eixoX], y=carsDf[,eixoY])) +
      #   theme_classic() +
      #   geom_hline(yintercept = 0, color = "gray70") +
      #   geom_vline(xintercept = 0, color = "gray70") +
      #   geom_count(aes(color = cluster), alpha = 0.55, size = 3) +
      #   xlab(eixoX) +
      #   ylab(eixoY) + 
      #   xlim(-5, 6) + 
      #   ggtitle("PCA Clusters") 
       #print(unlist(w[,..LabelChoice]))
       #print(w[[LabelChoice]])
       #print(nrow(carsDf))
       #if(length(LabelChoice)>0)
      #  p1= p1 + geom_text(aes(y = w1$x[,eixoY], label =  w[[LabelChoice]]  ))
       ggplotly(p1,dragmode=pan)
       
       
       #plot(mtcars[,eixoX],mtcars[,eixoY],xlab=input$X,ylab=input$Y)
     } } ) 
   
   output$Outliers<-renderPrint({
     if(!is.null(input$file1) & input$Referenciador=="PCA Visualization"   ){
       
      PCA=GerarPCA()
      w=LeituraArquivo()
      w=as.data.frame(w)
      #w=w[,PCA$Variables]
      PCA=PCA$PCA
      PCA=as.data.frame(PCA)
      names(PCA)=paste("Component",1:ncol(PCA),sep="")
      #print(names(PCA))
      #Ponto=nearPoints(PCA, input$plot_click, threshold = 10, maxpoints = 1,
            #     addDist = FALSE)
      Ponto<- event_data("plotly_click")
      if (!is.null(Ponto)){
        Ponto=Ponto[,3:4] # Esta tupla deve ser modificada de forma a identificar o x y associado
        PCA=data.frame(PCA[input$X],PCA[input$Y])
        names(PCA)=c(input$X,input$Y)
       # print(head(PCA))
       # print(Ponto)
      Encontrar=FALSE
      i=1
      while(!Encontrar){
        if(sum(abs(PCA[i,]-Ponto))<3e-6 ){
          Encontrar=TRUE
          Linha=i
          
        }
        i=i+1
      }
      #print(Linha)
      print(w[Linha,])
      #return(w[linha,])
      } 
      }
   } )



	output$MatrixPlot <- renderPlotly({
     if(!is.null(input$file1)){
      # caminho=input$file1$datapath
      # reject=input$Palavras
      # a=leitura(caminho)
      # a=Palavras(a)
      # b=ConversorMatriz(unlist(a),reject)
      # b=MatrizDistanciasPalavras(b,method)
     #source("Analise_texto.R")
     w=LeituraArquivo()
     #if(input$OpcaoMatriz=="Matriz"){
       
      m=Matriz_Correlacao_retorno(w)
      print(dim(m))
      grafico=MatrixggplotShiny(m)	  
    # }
    # if(input$escolhas=="Media")
    #   grafico= ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="media",linguagem=input$linguagens)
    # else if(input$escolhas=="Desvio Padrao")
    #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="desvio",input$linguagens)
    # else if(input$escolhas=="Ambos")
    #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="ambos",input$linguagens)
     ggplotly(grafico,dragmode=pan)
     }
   })
	
	output$GrafoCorrelacaov1<-renderPlot({
	  if(!is.null(input$file1)){
	    
	    w=LeituraArquivo()
	    #m=Matriz_Correlacao_retorno(w)
	    #if(input$OpcaoMatriz=="Grafo")
	      GrafoCorrelacao(w,0.8)
	  }
	})


#      findBestSet<-eventReactive(input$preencherdados,
#	{
#		if(!is.null(input$file1)){
 #      		w=read.csv(input$file1$datapath,header=TRUE)
#		Goal=which(names(w)==input$X)
#		z=best_vector(w,Goal,input$veclength,nrow(w),as.numeric(input$numtrigger))
#		if(length(z)==0)
#			z=best_vector(w,Goal,input$veclength,nrow(w),1)
#		
#		return(z)
		#cat("The best set is",x,"it predicts",names(w)[Goal],"with",y*100,"% Accuracy\n")
#	}
#	})
}

# Run the application 
shinyApp(ui = ui, server = server)
