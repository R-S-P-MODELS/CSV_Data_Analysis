#loadNamespace("plyr")
require(plyr)
#'\code{candidates} Asks for a dataframe and some parameters and returns how close the collums chosen can predict the goal collum
#' Should be used mostly with generate_candidates or preferably best_vector in case you only want the best combination possible for prediction
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param vec a vector of collums you wish to test if can be used to predict the values
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger If its 1 it wont use candidates that only show once, recomend to run always on 1  and on numeric values compare with 1 to 0
#'@export
candidates=function(df,goal,vec,repetitions,trigger=1){
y=list()
### nova linha
df=df[which(!is.na(df[,goal]) ),]
### fim da nova
if(length(vec)==1)
	x=length(unique(df[,vec] )   )
else
	x=nrow(unique(df[,vec] )   )
#vec[length(vec)+1]=goal
vec=append(vec,goal)
m=plyr::count(df[,vec])
z=nrow(m)
#y[[1]]=z-x
valores_m=as.numeric(names(table(m[,ncol(m)])))
#print(valores_m)
if(trigger==1)
  aceitacao=sum(valores_m<=1) # aceitação =0 eu aceito senao existe chaves unicas em vec+goal
else
  aceitacao=0
y[[2]]=aceitacao
# como z-x deve ser 0 e aceitacao retornar 0 para aceitarmos
#print(c(z,x,aceitacao))
#cat("o valor de z é:",z,"x vale",x,"aceitacao",aceitacao,"o vetor já sera",vec,"\n")
w=abs(z-x)
#print(c(w,vec))
y[[1]]=w
w=as.numeric(w>repetitions)
#return(w)
return(y)
#return( w +aceitacao) # apenas valor 0 aceita vec para esta goal
}

#'\code{generate_candidates} Asks for a dataframe and some parameters and returns all possible combinations of collums for prediction that satisfy a given error in input
#'in a list the first element of the list are the combinations while the second is its measure of error,to get the best parameters call best_vector
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger If its 1 it wont use candidates that only show once, recomend to run always on 1  and on numeric values compare with 1 to 0
#'@export
generate_candidates=function(df,goal,maxi,repetitions,trigger=1){
groups=1:ncol(df)
groups=groups[-goal]
if(length(groups)>maxi)
  loop=maxi
else
  loop=length(groups)
###versao otimiziada mas que gera todas as combinacoes
	#z= lapply(1:length(groups), function(x) combn(groups, x))
  z=lapply(1:loop, function(x) combn(groups, x))
###traducao do lapply
#if(length(groups)>maxi ){
#		loop=maxi
#}
#else
#	loop=length(groups)
#z=list()
#or(i in 1:loop){
#	z[[i]]=combn(groups, i)


#}
vetores_candidatos=list()
erros=c()
cont=1
if(length(z)>maxi)
	maximo=length(z)
else
	maximo=maxi
for(i in 1:maximo){
	#print(i)
	data=data.frame(z[[i]])
	for(j in 1:ncol(data)){
		vetor=data[,j]
		#print(candidatos(df,goal,vetor))
		lista_auxiliar=candidates(df,goal,vetor,repetitions,trigger)
		if(lista_auxiliar[[1]]<=repetitions & lista_auxiliar[[2]]==0){
			vetores_candidatos[[cont]]=vetor
			erros[cont]=lista_auxiliar[[1]]
			cont=cont+1

		}

	}


}
retorno=list(vetores_candidatos,erros)
#return(vetores_candidatos)
return(retorno)
}



#'\code{best_vector} Asks for a dataframe and some parameters and returns the best combination of collums to predict the missing value
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger If its 1 it wont use candidates that only show once, recomend to run always on 1  and on numeric values compare with 1 to 0
#'@export
best_vector=function(df,goal,maxi,repetitions,trigger=1){
z=generate_candidates(df,goal,maxi,repetitions,trigger)
if(length(z[[1]])>0)
  return(unlist(z[[1]][min(which(z[[2]]==min(z[[2]])))]) )
else{
  cat("Could not find any candidate, please use trigger=0\n")
  return(0)

}

}



#'\code{NA_VALUES} Asks for a dataframe and returns a table of how many missing values are in each collum
#' @param df A dataframe with the missing values you wish to fill
NA_VALUES=function(df){
return(apply(apply(df,2,is.na),2,sum) )

}



#'\code{Complete_dataset} Asks for a dataframe, a vector of collumn indices and the goal collumn and  returns the data frame with the values filled
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param rows The collumns you wish to use to predict the missing values
#'@export
Complete_dataset=function(df,rows,goal){
  #lets suppose df has missing values on only goal collum
  test=list()
  df2=df
  vectors=which(is.na(df[,goal]))
  df1=df[-vectors,] # for plyr::count
  aux=c(rows,goal)
  frequency_table=plyr::count(df1[,aux])
  for(i in vectors){
    test=list()
    for(j in 1:length(rows)  )
      {
         test[[j]]=which(df[i,rows[j]]==frequency_table[,j]   )
        # print(c(df [i,rows[j]],frequency_table[,j] )  )
    }
 #   print(test)
    prob_table=frequency_table[Reduce(intersect,test),c(ncol(frequency_table)-1,ncol(frequency_table))]
    prob=cumsum(prob_table[,2])
    prob=prob/max(prob)
    print(c(prob,prob_table))
    df2[i,goal]=prob_table[min(which(runif(1)<prob)),1]
  }
  return(df2)
}

#'\code{autoComplete} Asks for a dataframe, a vector of collumn indices and the goal collumn and  returns the data frame with the values filled
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger If its 1 it wont use candidates that only show once, recomend to run always on 1  and on numeric values compare with 1 to 0
#'@export
autoComplete=function(df,goal,maxi,repetitions,trigger=1){
  z1=best_vector(df,goal = goal,maxi = maxi,repetitions = repetitions,trigger = trigger)
  df2=Complete_dataset(df = df,rows = z1,goal = goal)
  return(df2)
}


#'\code{MeanAccuracy} Asks for a dataframe, a vector of collumn indices and the goal collumn the expected value of accuracy of filling missing values if the dataset is representative
#' @param df A dataframe that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#'@export
MeanAccuracy=function(df,VECTORS,goal){
 lixo=plyr::count(df[,c(VECTORS,goal)  ])
  lixo1=unique(df[,VECTORS])
  lista=list()
  if(length(VECTORS)>1)
    tamanho=nrow(lixo1)
  else
    tamanho=length(lixo1)
   for(j in 1:tamanho){
     vec=c()
     for(i in 1:nrow(lixo)){
    if(length(VECTORS)>1   )
     vec[i]=sum(lixo[i,1:ncol(lixo1)]==lixo1[j,1:ncol(lixo1)])==ncol(lixo1)
    else
      vec[i]=lixo[i,1]==lixo1[j]
     }
   lista[[j]]=vec
   }
  lista=lapply(lista,which)
  maximo=sum(lixo$freq)
  prob=0
  #print()
for(i in 1:length(lista)){
  if(length(lista[[i]])==1 )
    prob=prob+lixo$freq[lista[[i]]]/maximo # probabilidade de escolhelo 100%
  else{
    prob_auxiliar=lixo$freq[unlist(lista[[i]])]
    acumulado=sum(prob_auxiliar)
    Pe=prob_auxiliar/acumulado #probabilidade de ser escolhido
    Pa=prob_auxiliar/maximo # probabilidade global
    prob=prob+sum(Pe*Pa)
  }
}
  return(prob)
}

#'\code{BestAccuracy} Asks for a dataframe, a vector of collumn indices and the goal collumn and returns the maximum possible value of accuracy of filling missing values
#' @param df A dataframe that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#'@export
BestAccuracy=function(df,VECTORS,goal){
  auxiliar_vector=c()
  lixo=plyr::count(df[,c(VECTORS,goal)  ])
  lixo1=unique(df[,VECTORS])
  lista=list()
  if(length(VECTORS)>1)
    tamanho=nrow(lixo1)
  else
    tamanho=length(lixo1)
  for(j in 1:tamanho){
    vec=c()
    for(i in 1:nrow(lixo)){
      if(length(VECTORS)>1   )
        vec[i]=sum(lixo[i,1:ncol(lixo1)]==lixo1[j,1:ncol(lixo1)])==ncol(lixo1)
      else
        vec[i]=lixo[i,1]==lixo1[j]
    }
    lista[[j]]=vec
  }
  lista=lapply(lista,which)
  maximo=sum(lixo$freq)
  prob=0
  #print()
  for(i in 1:length(lista)){
    if(length(lista[[i]])==1 )
      #prob=prob+lixo$freq[lista[[i]]]/maximo # probabilidade de escolhelo 100%
      auxiliar_vector[i]=1
    else{
      prob_auxiliar=lixo$freq[unlist(lista[[i]])]
      acumulado=sum(prob_auxiliar)
      Pe=prob_auxiliar/acumulado #probabilidade de ser escolhido
      Pa=prob_auxiliar/maximo # probabilidade global
      prob=prob+sum(Pe*Pa)
      auxiliar_vector[i]=sum(Pe*Pe)
    }
  }
  return(max(auxiliar_vector))
}


#'\code{WorstAccuracy} Asks for a dataframe, a vector of collumn indices and the goal collumn and returns the minimum possible value of accuracy of filling missing values
#' @param df A dataframe that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#'@export
WorstAccuracy=function(df,VECTORS,goal){
  auxiliar_vector=c()
  lixo=plyr::count(df[,c(VECTORS,goal)  ])
  lixo1=unique(df[,VECTORS])
  lista=list()
  if(length(VECTORS)>1)
    tamanho=nrow(lixo1)
  else
    tamanho=length(lixo1)
  for(j in 1:tamanho){
    vec=c()
    for(i in 1:nrow(lixo)){
      if(length(VECTORS)>1   )
        vec[i]=sum(lixo[i,1:ncol(lixo1)]==lixo1[j,1:ncol(lixo1)])==ncol(lixo1)
      else
        vec[i]=lixo[i,1]==lixo1[j]
    }
    lista[[j]]=vec
  }
  lista=lapply(lista,which)
  maximo=sum(lixo$freq)
  prob=0
  #print()
  for(i in 1:length(lista)){
    if(length(lista[[i]])==1 )
      #prob=prob+lixo$freq[lista[[i]]]/maximo # probabilidade de escolhelo 100%
      auxiliar_vector[i]=1
    else{
      prob_auxiliar=lixo$freq[unlist(lista[[i]])]
      acumulado=sum(prob_auxiliar)
      Pe=prob_auxiliar/acumulado #probabilidade de ser escolhido
      Pa=prob_auxiliar/maximo # probabilidade global
      prob=prob+sum(Pe*Pa)
      auxiliar_vector[i]=sum(Pe*Pe)
    }
  }
  return(min(auxiliar_vector))
}
