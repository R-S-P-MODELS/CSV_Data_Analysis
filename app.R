#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(warn=-1)
library(shiny)
require(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
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
        conditionalPanel(
          condition = "is.null(input.file1) == TRUE",
          checkboxInput("histogram","Show histogram of x")),
        uiOutput("Eixox"),
        uiOutput("Eixoy"),
        uiOutput("cores"),
        uiOutput("tamanhos"),
        actionButton("preencherdados","Completar Dataset"),
        downloadButton("downloadData", "Download do dataset completado")
        ),
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         conditionalPanel(
           condition = "input.showsummary == TRUE",
         verbatimTextOutput("sum")),
         conditionalPanel(
           condition = "input.showcorrelation == TRUE",
         verbatimTextOutput("corre")),
         verbatimTextOutput("nas"),
         h3("Aplicativo desenvolvido por Rafael Silva Pereira!\n\n"),
         h4("Em caso de duvidas ou problemas favor entrar em contato\n\n"),
         h4("Para gerar o grafico a primeira vez clique para completar o dataset, não será nescessario para futuras explorações\n\n"),
         h4("r.s.p.models@gmail.com")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=100*1024^2)
  
  output$tamanhos = renderUI({
    if(!is.null(input$file1)){
      w=read.csv(input$file1$datapath,header=TRUE)
      a=c()
      for(i in 1:length(w[,1]))
        a[i]=1
      aux=data.frame(w,a)
      names(aux)[length(aux)]='None'
 #   selectInput("X", label = "X",choices = names(aux))
    
  #  selectInput("Y", label = "Y",choices = names(aux))
    
   # selectInput("color", label = "Color",choices = names(aux))
    if(input$histogram==FALSE)
      selectInput("size", label = "Size",
                choices = names(aux))
    
    }
     })
  output$cores = renderUI({
    if(!is.null(input$file1)){
      w=read.csv(input$file1$datapath,header=TRUE)
      a=c()
      for(i in 1:length(w[,1]))
        a[i]=1
      aux=data.frame(w,a)
      names(aux)[length(aux)]='None'
      selectInput("X", label = "X",choices = names(aux))
      
      selectInput("Y", label = "Y",choices = names(aux))
      
      selectInput("color", label = "Color",choices = names(aux))
     
      
    }
  })
  
  output$Eixoy = renderUI({
    if(!is.null(input$file1)){
      
      w=read.csv(input$file1$datapath,header=TRUE)
     # selectInput("X", label = "X",choices = names(w))
      if(input$histogram==FALSE)
      selectInput("Y", label = "Y",choices = names(w))
      
    
      
      
    }
  })
  
  output$Eixox = renderUI({
    if(!is.null(input$file1)){
      w=read.csv(input$file1$datapath,header=TRUE)
      selectInput("X", label = "X",choices = names(w))
      
      
      
    }
  })
  
  
  
  
  
  

   
   
   output$sum <- renderPrint({
     if(!is.null(input$file1)){
       if(input$showsummary==TRUE){
       
     w=read.csv(input$file1$datapath,header=TRUE)
     
     summary(w)
     }}
   })
   output$corre <- renderPrint({
     if(!is.null(input$file1)){
       if(input$showcorrelation==TRUE){
         
       w=read.csv(input$file1$datapath,header=TRUE)
       e=w[,as.numeric(which(sapply(w,class)=="integer" | sapply(w,class)=="numeric") )]
       
       cor(e, use = "pairwise.complete.obs")     }
   }})
   
   
   
   output$nas <- renderPrint({
     if(!is.null(input$file1)){
       if(input$showna==TRUE){
         
         w=read.csv(input$file1$datapath,header=TRUE)
         for(i in 1:ncol(w))
         cat("O numero percentual de elementos NA na coluna", names(w)[i], "é", sum(is.na(w[,i]))/length(w[,i]),"\n" )
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
      w[,i]=c 
       }
     }
     return(w) # distribuicao sem NA
   }
   
   
   completar_reativo<-eventReactive(input$preencherdados,
    {
     
       
         
         w=read.csv(input$file1$datapath,header=TRUE)
         e=w[,as.numeric(which(sapply(w,class)=="integer" | sapply(w,class)=="numeric") )]
         nomes=names(w)
         cont=0
         dados=completar_distribuicao(e)
         while(sum(is.na(dados))>0 & cont<6   ){
           dados=completar_distribuicao(dados)
           cont=cont+1
         }
           
       
       #atualizacao do dataframe original
    #   a=which(names(w)==names(dados))
         a=c()
        for(i in 1:length(names(e)) ){
            a[i]=which(names(e[i])==names(w))
            w[,a[i]]=dados[,i]
        } 
         names(w)=nomes
       #w[,a]=dados[,a]
       write.csv(file="dataset_prenchido.csv",w,col.names = FALSE,row.names = FALSE)
   })
   
   
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("dataset_prenchido", ".csv", sep = "")
     },
     content = function(file) {
       a=read.csv("dataset_prenchido.csv")
       write.table(a, file, row.names = FALSE,col.names = FALSE)
     }
   )
   
   funcoes_reativas<- reactive({
     completar_reativo()
     lixo=1
   })
   
   
   output$distPlot <- renderPlot({
     #leitura()
     if(!is.null(input$file1)){
       funcoes_reativas()
       w=read.csv(input$file1$datapath,header=TRUE)
       a=c()
       for(i in 1:length(w[,1]))
         a[i]=1
       aux=data.frame(w,a)
       names(aux)[length(aux)]='None'
       eixoX=which(names(w)==input$X)
       eixoY=which(names(w)==input$Y)
       colorido=which(names(aux)==input$color)
       tamanho=which(names(aux)==input$size)
       if(input$histogram==FALSE)
         ggplot(data=aux) +geom_point(aes(x=aux[,eixoX],y=aux[,eixoY],color=aux[,colorido],size=aux[,tamanho]) )+labs(x=input$X,y=input$Y,colour=input$color,size=input$size )
       else
         ggplot(data=aux) +geom_histogram(aes(x=aux[,eixoX],fill=aux[,colorido]),stat = 'count' )+labs(x=input$X,fill=input$color )
       
       
     }
     
     #plot(mtcars[,eixoX],mtcars[,eixoY],xlab=input$X,ylab=input$Y)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

