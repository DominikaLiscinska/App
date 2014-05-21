moje=read.table(file="forex_data.txt", header=TRUE, sep=",") 

library(shiny)
library(xtable)

shinyUI(
  pageWithSidebar(
    headerPanel('Time series preprocessing'),
  

    sidebarPanel(
		wellPanel(
		fileInput('file1', h4(strong('Choose file'))),
		checkboxInput('header', 'Header', TRUE),
		gsub("label class=\"radio\"", "label class=\"radio inline\"", radioButtons('sep', 'Separator:', c(Coma=',', Semicolon=';', Tab='\t'), selected='Coma')),
		br(),
		selectInput("y", "Select attribute:",colnames(moje)[2:ncol(moje)], names(moje)[[2]])
		),
				      
      wellPanel(
        h4(strong('Moving average')), 
        numericInput(inputId='Krok',label=('Input the length of moving part:*'), value=3),
        br(),
        downloadButton('downloadPlot','Download plot'),
        downloadButton('downloadData', 'Download data')
      ),
      
      wellPanel(
        h4(strong('Mean average')), 
        checkboxInput(inputId = 'Prepojenie', label =" Use data from moving average", value = FALSE),
        numericInput(inputId='Frekvencia',label='Input frequency of average: *', value=5),
        br(),
        downloadButton('downloadPlot1','Download plot'),
        downloadButton('downloadData1','Download data')
      ),

      wellPanel(
        h4(strong('Turning points')), 
        numericInput(inputId='Rcislo', label='Input parameter R: *', value=1.0009),
        br(),
        downloadButton('downloadPlot2','Download plot'),
        downloadButton('downloadData2', 'Download data')
      ),
     
    
      
      h6(helpText(strong("* Input required parameters"), style = "color:grey"))
    ),  

    mainPanel(
      tabsetPanel(
        tabPanel("Table with data",tableOutput('tabulka')),
        tabPanel("Graph",plotOutput('graf'),verbatimTextOutput('summary')),
        tabPanel("Moving average", plotOutput('klzavy'),verbatimTextOutput('summary1')),
        tabPanel("Mean average",plotOutput('aritmet'),verbatimTextOutput('summary2')),
        tabPanel("Turning points",plotOutput('body'),tableOutput('table')),
        tabPanel("Info",textOutput('info'), h4('General information', style='color:slategrey'),
          
          ('The application serves for some time series preprocessing steps. For proper function you need to set the required parameters for each feature. As a default example is chosen dataset about ..., but also own dataset can be used.  '),
          br(),
          br(),
          p(h5('1. Moving average', style="color:slategrey"), ('The length of moving part determines from how many 
            consecutive values algorithm has to calculate the average')),
          br(),
          p(h5('2. Mean average', style="color:slategrey"), ('The frequency of mean average determines from how many 
            values the mean value is calculated.')),
          br(),
          p(h5('3. Turning points', style="color:slategrey"), ('The turning points function finds local extremes in choosed time serie.  
            The R needs to be choosed right depending on the size of browsing area. The R has to be always bigger than 1.'))
        ) 
      )
    )
  )
)