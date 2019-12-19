library(shiny)
 library(shinydashboard)
library(shinydashboardPlus)

require(plotly)

titulo<-dashboardHeaderPlus(title = "Exploratory Data Analysis",titleWidth = 450,enable_rightsidebar = TRUE, rightSidebarIcon = "gears",disable = FALSE)

menudireito<-rightSidebar(width=250,
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
                            sliderInput("poly",label = "Maximum polynomial complexity",min = 1,max=20,value=2)),
                          
                          
                          conditionalPanel(
                            condition = "input.generate == true",
                            uiOutput("NTermos"),
                            selectInput("metrica",label = "Metrics",choices = c("R squared","F Value"))
                            
                            
                            
                            
                          ) ) 
                           

menu<- dashboardSidebar(width = 250,
 tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
    sidebarMenu(id="Referenciador",
      menuItem("Data Exploration", tabName = "DataExploration", icon = icon("dashboard")),
      menuItem("Dimensionality Reduction", tabName = "PCAVisualization", icon = icon("dashboard")),
      menuItem("Self Organizing Maps",tabName="SOM",icon=icon("dashboard")),
menuItem("Correlation Matrix Image", tabName = "CorrelationMatrixImage", icon = icon("th")),
menuItem("3D plots", tabName = "3Dplots", icon = icon("th"))
    ),

       
        selectInput("TipoGrafico","Select your Graph Type",choices=c('Points','Lines','BoxPlot',"Density2d","HeatMap","Quantile","Histogram","ErrorBar")),
       # conditionalPanel(
        #  condition = "is.null(input.file1) == true",
         # checkboxInput("histogram","Show histogram of x")),
        uiOutput("OpcoesSom"),
        uiOutput("OpcoesReduction"),
        uiOutput("XdimSom"),
        uiOutput("YdimSom"),
        uiOutput("Eixox"),
        uiOutput("EixoxErro"),
        uiOutput("Eixoy"),
        uiOutput("EixoyErro"),
        uiOutput("Eixoz"),
        uiOutput("cores"),
        uiOutput("tamanhos")
        
  )



## Body content
 corpo<- dashboardBody(
   
   tags$head(tags$script("$(function(){
                          $('.sidebar-toggle').on('click',function(){Shiny.onInputChange('sidebarStatus',$('aside.main-sidebar.shiny-bound-input').attr('data-collapsed'))});
                          });
                          ")),
fileInput("file1", "Insert CSV file to be analized",
                  accept = c(
                    "text/csv",".csv") ),
      #textOutput("res"),

    tabItems(
      # First tab content
      tabItem(tabName = "DataExploration",
        fluidRow(
          plotlyOutput("distPlot")

          
        )
      ),

tabItem(tabName = "PCAVisualization",
        fluidRow(
          plotlyOutput("PCAPlot"),
	  verbatimTextOutput("Outliers") 
          
        )
      ),

tabItem(tabName = "SOM",
        fluidRow(
	plotOutput("SOMPlot")

          
        )
      ),



tabItem(tabName = "CorrelationMatrixImage",
        fluidRow(
          plotlyOutput("MatrixPlot"),
	  plotOutput('GrafoCorrelacaov1')

          
        )
      ),

tabItem(tabName = "3Dplots",
        fluidRow(
          plotlyOutput("threeDplots")
	  

          
        )
      )

      
    ),

conditionalPanel(
           condition = "input.showsummary == true",
         verbatimTextOutput("sum")),
         conditionalPanel(
           condition = "input.showcorrelation == true",
         verbatimTextOutput("corre")) ,
         verbatimTextOutput("nas"),
         verbatimTextOutput("grupos"),
         verbatimTextOutput("grupos_alt"),
	 verbatimTextOutput("bestvec"),
actionButton("preencherdados","Complete Dataset"),
downloadButton("downloadData", "Download Completed dataset"),
         h3("Application developed by Rafael Silva Pereira!\n\n"),
         h4("If you have any questions or problems, please contact us.\n\n"),
         h4("To create your first vizualization please click on complete dataset,This will not be nescessary for future explorations\n\n"),
         h4("Contact: r.s.p.models@gmail.com")


  )


ui <- dashboardPagePlus(titulo,menu,corpo,menudireito,skin="green",title = "Exploratory Data Analysis" )

