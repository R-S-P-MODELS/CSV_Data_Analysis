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
source("Functions.R")
###fim da definicao de funcoes

options(warn=-1)
library(shiny)
library(plotly)
#require(ggplot2)
# Define UI for application that draws a histogram
#source("GraphicalInterface.R")
source("UiDashboardPlus.R")
# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=1000000*1024^2)


  output$res <- renderText({
      paste("You've selected:", input$Referenciador)
    })

  output$NTermos=renderUI({
    sliderInput("opcoes",label = "Number of models on graph",min = 1,max=3*input$poly,value=1)
    
  })
  
  LeituraArquivo<-reactive(
    {
      if(!is.null(input$file1)){
        #funcoes_reativas()
	#print("feito leitura")
        w=fread(input$file1$datapath,header=TRUE)
        return(unique(w) )

      }
      
      
    }
    
  )

  LeituraArquivoCsv<-reactive(
    {
      if(!is.null(input$file1)){
        #funcoes_reativas()
	#print("feito leitura")
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
     
     sliderInput(inputId = "separacoes",min =2,max =0.5*nrow(w),value = 2,label="Number of subsets in dataset"  ) 
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
      if(input$TipoGrafico!="Histogram"){
        if(input$Referenciador=="DataExploration")
          selectInput("size", label = "Size",
                      choices = names(aux))
        else if(input$Referenciador=="3Dplots"){
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
      if(input$Referenciador=="PCAVisualization"){
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
      if(input$TipoGrafico!="Histogram"){
        if(input$Referenciador=="PCAVisualization"){
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
      if(input$Referenciador=="3Dplots"){
      w=LeituraArquivo()
      #w=LeituraArquivoCsv()
      # selectInput("X", label = "X",choices = names(w))
      if(input$TipoGrafico!="Histogram"){
        if(input$Referenciador=="PCAVisualization"){
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
      if(input$Referenciador=="PCAVisualization"){
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
  
  

  
  output$EixoyErro = renderUI({
    if(!is.null(input$file1)){
      if(input$TipoGrafico %in% c('ErrorBar')){
        w=LeituraArquivo()
        w=as.data.frame(w)
        w=data.frame(w,0)
        names(w)[ncol(w)]='NoError'
        classes=sapply(w,class) %in% c('integer','numeric')
        w=w[,classes]
        selectInput("Yerror", label = "Y error",choices = names(w))
      }
    }
  })

  output$EixoxErro = renderUI({
    if(!is.null(input$file1)){
      if(input$TipoGrafico %in% c('ErrorBar')){
        w=LeituraArquivo()
        w=as.data.frame(w)
        w=data.frame(w,0)
        names(w)[ncol(w)]='NoError'
        classes=sapply(w,class) %in% c('integer','numeric')
        w=w[,classes]
        selectInput("Xerror", label = "X error",choices = names(w))
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
      # print(i)
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
         #print(dim(w))
         #factors
	 } 
         f=as.numeric(which(sapply(w,class)!="integer" & sapply(w,class)!="numeric") )
         print(f)
         print(names(w)[f])
         for(i in f){
           #print(c(i,names(w)[i],class(i) ) )
           
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
         #print(dim(w)) 
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
       ErroY=which(names(w)==input$Yerror)
       ErroX=which(names(w)==input$Xerror)
       
       colorido=which(names(aux)==input$color)
       tamanho=which(names(aux)==input$size)
	     Auxiliar=unique(aux[,c(..eixoX,..eixoY,..colorido,..tamanho)])
	     names(Auxiliar)=c("n1","n2","n3","n4")
	     print(names(Auxiliar))
       if(input$TipoGrafico!="Histogram"){
         if(input$generate==FALSE){
         if(input$TipoGrafico=="Points"){
          Graph=ggplot(data=Auxiliar) +geom_count(aes(x=unlist(Auxiliar[,1]),y=unlist(Auxiliar[,2]),color=unlist(Auxiliar[,3]),size=unlist(Auxiliar[,4])) )+labs(x=input$X,y=input$Y,colour="",size="" )
         } else if(input$TipoGrafico=="Lines"){
           Graph=ggplot(data=Auxiliar) +geom_line(aes(x=unlist(Auxiliar[,1]),y=unlist(Auxiliar[,2]),color=unlist(Auxiliar[,3]) )  )+labs(x=input$X,y=input$Y,colour=input$color )
           
         } else if(input$TipoGrafico=="BoxPlot"){
           Graph=ggplot(data=Auxiliar) +geom_boxplot(aes(x=unlist(Auxiliar[,1]),y=unlist(Auxiliar[,2]),color=unlist(Auxiliar[,3])) )+labs(x=input$X,y=input$Y,colour=input$color )
           
         }
           else if(input$TipoGrafico=="Density2d"){
             Graph=ggplot(data=Auxiliar) +geom_density2d(aes(x=unlist(Auxiliar[,1]),y=unlist(Auxiliar[,2]),color=unlist(Auxiliar[,3])) )+labs(x=input$X,y=input$Y,colour=input$color )
             
           }
           else if(input$TipoGrafico=="Quantile"){
             Graph=ggplot(data=Auxiliar) +geom_quantile(quantiles = c(0.25,0.5,0.75,1),aes(x=unlist(Auxiliar[,1]),y=unlist(Auxiliar[,2]),color=unlist(Auxiliar[,3]),group=unlist(Auxiliar[,3]), ) )+labs(x=input$X,y=input$Y,colour=input$color )
             
           }
           else if(input$TipoGrafico=="HeatMap"){
             Graph=ggplot(data=Auxiliar) +geom_bin2d(aes(x=unlist(Auxiliar[,1]),y=unlist(Auxiliar[,2]),fill=unlist(Auxiliar[,3]),size=unlist(Auxiliar[,4]), ) )+labs(x=input$X,y=input$Y,colour=input$color,size=input$size )
             
           }
           else if(input$TipoGrafico=="ErrorBar"){
             if(length(ErroY)>0 & length(ErroX)>0 ){
             Auxiliar=unique(aux[,c(..eixoX,..eixoY,..colorido,..tamanho,..ErroY,..ErroX)])
             
             ymin=unlist(Auxiliar[,2])-unlist(Auxiliar[,5])
             ymax=unlist(Auxiliar[,2])+unlist(Auxiliar[,5])
             xmin=unlist(Auxiliar[,1])-unlist(Auxiliar[,6])
             xmax=unlist(Auxiliar[,1])+unlist(Auxiliar[,6])
             Xaxis=unlist(Auxiliar[,1])
             Yaxis=unlist(Auxiliar[,2])
             }else if(length(ErroX)>0){
               Auxiliar=unique(aux[,c(..eixoX,..eixoY,..colorido,..tamanho,..ErroX)])
               
               ymin=unlist(Auxiliar[,2])
               ymax=unlist(Auxiliar[,2])
               xmin=unlist(Auxiliar[,1])-unlist(Auxiliar[,5])
               xmax=unlist(Auxiliar[,1])+unlist(Auxiliar[,5])
               Xaxis=unlist(Auxiliar[,1])
               Yaxis=unlist(Auxiliar[,2])
             }else if(length(ErroY)>0){
               Auxiliar=unique(aux[,c(..eixoX,..eixoY,..colorido,..tamanho,..ErroY)])
               
               ymin=unlist(Auxiliar[,2])-unlist(Auxiliar[,5])
               ymax=unlist(Auxiliar[,2])+unlist(Auxiliar[,5])
               xmin=unlist(Auxiliar[,1])
               xmax=unlist(Auxiliar[,1])
               Xaxis=unlist(Auxiliar[,1])
               Yaxis=unlist(Auxiliar[,2])
             }else{
               Auxiliar=unique(aux[,c(..eixoX,..eixoY,..colorido,..tamanho)])
               ymin=unlist(Auxiliar[,2])
               ymax=unlist(Auxiliar[,2])
               xmin=unlist(Auxiliar[,1])
               xmax=unlist(Auxiliar[,1])
               Xaxis=unlist(Auxiliar[,1])
               Yaxis=unlist(Auxiliar[,2])
             }
             
             Graph=ggplot() + geom_errorbar(aes(x=Xaxis,y=Yaxis,ymin=ymin,ymax=ymax,color=unlist(Auxiliar[,3])))+geom_errorbarh(aes(color=unlist(Auxiliar[,3]),x=Xaxis,y=Yaxis,xmin = xmin,xmax = xmax))+labs(x=input$X,y=input$Y,colour=input$color)
           }
         ggplotly(Graph,dragmode=pan)
         } 
          else
           fit_melhor_caso(unlist(Auxiliar[,1]),unlist(Auxiliar[,2]),cores =unlist(Auxiliar[,3]) ,limitepol =input$poly,numero = input$opcoes,metrica = input$metrica,nomeX=input$X,nomeY=input$Y )}
       else if(input$TipoGrafico=="Histogram"){
          #if(input$TipoGrafico=="Histogram"){
            Graph=ggplot(data=Auxiliar) +geom_histogram(aes(x=unlist(Auxiliar[,1]),fill=unlist(Auxiliar[,3])),stat = 'count' )+labs(x=input$X,fill=input$color )
          #} 
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
       #print(names(Auxiliar))
       if(input$TipoGrafico!="Histogram"){
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
       else if(input$TipoGrafico=="Histogram")
         ggplot(data=Auxiliar) +geom_histogram(aes(x=unlist(Auxiliar[,1]),fill=unlist(Auxiliar[,3])),stat = 'count' )+labs(x=input$X,fill=input$color )
       
       
       
     }
     
     #plot(mtcars[,eixoX],mtcars[,eixoY],xlab=input$X,ylab=input$Y)
   })
   
   GerarPCA<-reactive({
     source("PCAOtimizado.R")
     #funcoes_reativas()
     #print(c("Grupos",input$color))
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
       #print(PCA)
       #print('Deu certo')
       step=step[1:input$color]
     }, error = function(e) {
       w1=prcomp(w[,..step],scale=TRUE)
       PCA=w1$x
     })
     #w1=prcomp(w[,..step],scale=TRUE)
     #PCA=w1$x
     Results=list(PCA=PCA,Variables=step)

    # return(Results)
   })
   
   output$PCAPlot <- renderPlotly({
     #leitura()
     if(!is.null(input$file1)){
       w=LeituraArquivo()
	print(w)
	
	    if(input$ReductDim=="PCA"){
       Results=GerarPCA()
	    }else if(input$ReductDim=="TSNE"){
	      Results=GerarTSNE()
	    }
     
       print(Results)
       step=Results$Variables
       PCA=Results$PCA
       w2=w[,..step]
      #print('ate aqui vai')
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
     if(!is.null(input$file1) & input$Referenciador=="PCAVisualization"   ){
       if(input$ReductDim=="PCA"){
         PCA=GerarPCA()
       }else if(input$ReductDim=="TSNE"){
         PCA=GerarTSNE()
       }
       
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
	      GrafoCorrelacao(w,0.9)
	  }
	})
	
	
	
	OptimumClustering<-function(matrix,kmin,kmax){
	  vec=c()
	  require(cluster)
	  withProgress(message = 'Building Som', value = 0, {
	    for(i in kmin:kmax){
	      set.seed(100)
	      clustercriado=kmeans(matrix,i)
	      silhueta=silhouette(clustercriado$cluster,matrix)[,3]
	      silhueta=silhueta[abs(silhueta)<Inf]
	      vec[i-kmin+1]=mean(silhueta,na.rm=TRUE)
	      incProgress(1/(kmax-kmin +1), detail = "Finding Optimum Cluster" )
	    }
	    Otimo=min(which(vec==max(vec)) ) + kmin
	    set.seed(100)
	  })
	  # return(Otimo)
	  return(kmeans(matrix,Otimo))
	}
	

  
	heatmap.som <- function(model){
	  for (i in 1:10) {
	    plot(model, type = "property", property = getCodes(model)[,i], 
	         main = colnames(getCodes(model))[i]) 
	  }
	}
	

	output$OpcoesSom<-renderUI({
	  if(!is.null(input$file1)){
	   if(input$Referenciador=="SOM") 
	     selectInput("SomGraphs",label="Som Vizualization Graph",choices=c('Node Count','HeatMap','Cluster'))
	  }
	})
	output$XdimSom<-renderUI({
	  if(!is.null(input$file1)){
	    if(input$Referenciador=="SOM") 
	      numericInput("XSom",label="Nos no eixo x do mapa",value=10)
	  }
	})
	
	output$YdimSom<-renderUI({
	  if(!is.null(input$file1)){
	    if(input$Referenciador=="SOM") 
	      numericInput("YSom",label="Number of nodes on y axis on map",value=10)
	  }
	})

	CalcularSom<-reactive({
	  w=LeituraArquivo()
	  w=as.data.frame(w)
	  require(kohonen)
	  w=w[,sapply(w,class) %in% c('numeric','integer')]
	  ads.train <- as.matrix(scale(w))
	  ads.grid <- somgrid(xdim = input$XSom, ydim = input$YSom, topo = "hexagonal")
	  ads.model <- som(ads.train, ads.grid, rlen = 500, radius = 2.5, keep.data = TRUE,
	                   dist.fcts = "euclidean")
	  return(list(w=w,model=ads.model))
	})
	
	output$SOMPlot<-renderPlot({
	  if(!is.null(input$file1)){
	    
	    ads.model=CalcularSom()
	    w=ads.model$w
	    ads.model=ads.model$model
	    #heatmap.som(ads.model)
	    if(input$SomGraphs=="Node Count"){
	      distri=as.numeric(table(ads.model$unit.classif))
	      amostra=round(runif(100,min(distri),max(distri)))
	      if(ks.test(amostra,distri)$p <0.05){
	        text="The Map node distribution is not uniform"
	      } else if(sum(ads.model$unit.classif==0)>0){
	        text="There are empty nodes on this map"
	      }else{
	        text="Node Counts"
	      }
	      plot(ads.model, type="count", main=text)
	    }
	    
	    if(input$SomGraphs=="Cluster"){
	   # Clusters<-OptimumClustering(ads.model$codes[[1]],2,10)
	    wss <- sapply(1:15, function(k){kmeans(ads.model$codes[[1]], k, nstart=50,iter.max = 15 )$tot.withinss})
	    Clusters<-which(abs(diff(wss)/wss[-length(wss)]) < 0.1)[1]
	    clust <- kmeans(ads.model$codes[[1]], Clusters)
	    plot(ads.model, type = "codes", bgcol = rainbow(9)[clust$cluster], main = "Cluster Map")
	    #colorido=which(names(w)==input$color)
	    
	    #cor=w[,colorido]
	    #plot(ads.model, type = "codes", bgcol = rainbow(9)[cor], main = "Cluster Map")
	    add.cluster.boundaries(ads.model, clust$cluster)
	    }else if(input$SomGraphs=="HeatMap"){
	      EixoHeat=which(input$X==names(w))
	      if(length(EixoHeat)>0){
	        plot(ads.model, type = "property", property = getCodes(ads.model)[,EixoHeat], 
	           main = colnames(getCodes(ads.model))[EixoHeat])
	      }
	    }
	    #plot(ads.model, type = "counts")
	    
	  }
	})
	output$OpcoesReduction<-renderUI({
	  if(!is.null(input$file1)){
	    if(input$Referenciador=="PCAVisualization")
	      selectInput("ReductDim",label="Dimension Reduction Method",choices=c('PCA','TSNE'))
	  }
	})
	
	GerarTSNE<-reactive({
	  if(!is.null(input$file1)){
	    
  	  require(tsne)
  	  w=LeituraArquivo()
  	  step=which(lapply(w,class) %in% c("numeric","integer"))
  	  # logic=lapply(w,class) %in% c("numeric","integer")
  	  # logic=!logic
  	  univariate=which(sapply(sapply(w[,..step],unique),length)==1)
  	  if(length(univariate)>0)
  	    step=step[-univariate]
  	  w=w[,..step]
  	  Calculado=tsne(w,k=input$color,max_iter=300)
  	  Results=list(PCA=Calculado,Variables=step)
  	  return(Results)
  	  #return(list(PCA=Calculado,Variables=step ) )
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
