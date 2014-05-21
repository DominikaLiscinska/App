moje=read.table(file="forex_data.txt", header=TRUE, sep=",") 

shinyServer(function(input, output, session){

functionTable <- function(x) {

		x[,1] <- as.POSIXct(x[,1])
		x2<-x
		x2[,1]<-as.character(x2[,1])

		for (i in 2:ncol(x2))
        {x2[,i] <- formatC(x2[,i],digits=5,format="f")}

		tabulka<-xtable(x2) }

functionGraph <- function(x) {	
		x[,1] <- as.POSIXct(x[,1])
	
	    y=input$y
		plot(x[,1],x[,y], type="b", lty=3, pch=19, xlab="Date and time", ylab="Values", col="blue") }

functionSummary <- function(x) {
x[,1] <- as.POSIXct(x[,1])

      summary(x) }

functionMoving <- function(x) {
	  krok <- input$Krok
      if (is.na(krok)) krok = 0
	  y=input$y
      x[,1] <- as.POSIXct(x[,1])
      n = krok
      hodnoty<- x[,y]
    
      mav <- function(hodnoty,n){filter(hodnoty,rep(1/n,n),sides=2)}
    
      plot(mav(hodnoty,n), xlab="Index", ylab="Values", col="blue") }

functionSummary1 <- function(x) {
      krok <- input$Krok
      if (is.na(krok)) krok = 0
      
      y=input$y
      x[,1] <- as.POSIXct(x[,1])
      n = krok
      hodnoty<- x[,y]
    
      mav <- function(hodnoty,n){filter(hodnoty,rep(1/n,n),sides=2)}
      x3=mav(hodnoty,n)

      summary(x3) }
	  
functionAM <- function(x){
      if (input$Prepojenie) {
        krok <- input$Krok
        if (is.na(krok)) krok = 0
        f <- input$Frekvencia
        if (is.na(f)) f=0 

        y=input$y
        x[,1] <- as.POSIXct(x[,1])
        n = krok
        hodnoty<- x[,y]
    
        mav <- function(hodnoty,n){filter(hodnoty,rep(1/n,n),sides=2)}

        x3 <- mav(hodnoty,n)
        l=length(x3)
        zaznamy=c()
        k=0
    
        for(i in(1:l)){
          if (is.na(x3[i]))
            k=k+1
          else
            zaznamy[i-k]=x3[i]
        } 
    
        N=length(zaznamy)                     ## find out the count of values
        z=N%%f                       ## calculate the remainder of the division
        opak=floor(N/f)                  ## find out the count of repetitions
        P=c()                             ## create empty vector P
      
        for (i in (0:(opak-1))) {
          P[i+1]=sum(zaznamy[(i*f+1):(f*(i+1))])/f}
        if (z>0) 
          P[opak+1]=sum(zaznamy[(opak*f+1):N])/z

        plot(P, type="b", lty=3, pch=19, xlab="Index", ylab="Values", col="blue")
      }

      else {
        f <- input$Frekvencia
        if (is.na(f)) f=0 
	    y=input$y
	  
        zaznamy=x[,y]                    ## save values to new variable
        N=length(zaznamy)                     ## find out the count of values
        z=N%%f                       ## calculate the remainder of the division
        opak=floor(N/f)                  ## find out the count of repetitions
        P=c()                             ## create empty vector P
      
        for (i in (0:(opak-1))) {
          P[i+1]=sum(zaznamy[(i*f+1):(f*(i+1))])/f}
        if (z>0) 
          P[opak+1]=sum(zaznamy[(opak*f+1):N])/z
      
        plot(P, type="b", lty=3, pch=19, xlab="Index", ylab="Values", col="blue")
      }
    }

functionSummary2 <- function(x) {
      if (input$Prepojenie) {
        krok <- input$Krok
        if (is.na(krok)) krok = 0
        f <- input$Frekvencia
        if (is.na(f)) f=0 

        y=input$y
        x[,1] <- as.POSIXct(x[,1])
        n = krok
        hodnoty<- x[,y]
    
        mav <- function(hodnoty,n){filter(hodnoty,rep(1/n,n),sides=2)}

        x3 <- mav(hodnoty,n)
        l=length(x3)
        zaznamy=c()
        k=0
      
        for(i in(1:l)){
          if (is.na(x3[i]))
            k=k+1
          else
            zaznamy[i-k]=x3[i]
        } 
    
        N=length(zaznamy)                     ## find out the count of values
        z=N%%f                       ## calculate the remainder of the division
        opak=floor(N/f)                  ## find out the count of repetitions
        P=c()                             ## create empty vector P
      
        for (i in (0:(opak-1))) {
          P[i+1]=sum(zaznamy[(i*f+1):(f*(i+1))])/f}
        if (z>0) 
          P[opak+1]=sum(zaznamy[(opak*f+1):N])/z

        summary(P)
      }

      else {
        f <- input$Frekvencia
        if (is.na(f)) f=0 
     
        y=input$y	  ## load the table
        zaznamy=x[,y]                    ## save values to new variable
        N=length(zaznamy)                     ## find out the count of values
        z=N%%f                       ## calculate the remainder of the division
        opak=floor(N/f)                  ## find out the count of repetitions
        P=c()                             ## create empty vector P
      
        for (i in (0:(opak-1))) {
          P[i+1]=sum(zaznamy[(i*f+1):(f*(i+1))])/f}
        if (z>0) 
          P[opak+1]=sum(zaznamy[(opak*f+1):N])/z

        summary(P)
      }
    }
	
functionPoints <- function(x) {
      R <- input$Rcislo
      if (is.null(R)) return(NULL)

	  y=input$y
      X<-x[,y]
      n=length(X)

      FIND_FIRST<- function(a,R){
      iMin=1
      iMax=1
      i=2
      n=length(a)
  
      while(i<n && a[i]/a[iMin]<R && a[iMax]/a[i]<R){
        if (a[i]<a[iMin])
          iMin=i
        if(a[iMax]<a[i])
          iMax=i
        i=i+1
      }
  
      if (iMin<iMax){
        retx=iMin
        rety=a[iMin]
      }
      
      else {
        retx=iMax
        rety= a[iMax]
      }
      
      return(c(retx,rety))
      }
 
      FIND_MAXIMUM<-function(a,R,i){
      iMax=i
      n=length(a)
      i=i+1
  
      while(i<n && (a[iMax]<R*a[i])){
        if (a[i]>a[iMax])
          iMax=i
        i=i+1
      }
  
      if (i<n){
        retx=iMax
        rety=a[iMax]
      }
  
      else{
        retx=n
        rety=a[n]
      }
    
      return(c(retx,rety))
      }

      FIND_MINIMUM<-function(a,R,i){
      iMin=i
      n=length(a)
      i=i+1
  
      while(i<n && (a[i]<R*a[iMin])){
        if (a[i]<a[iMin])
          iMin=i
        i=i+1
      }
          
      if (i<n){
        retx<-iMin
        rety<-a[iMin]
      }
      
      else{
        retx<-n
        rety<-a[n]
      }

      return(c(retx,rety))
      }

      v <- FIND_FIRST(X,R)

      x = c(v[1])
      y = c(v[2])
      i = x[length(x)]

      if (x[1] < n && y[1] < X[1]){
      v <- FIND_MAXIMUM(X, R, i)
      x <- c(x, c(v[1]))
      y <- c(y, c(v[2]))
      i = x[length(x)]
      }

    
      while (i<n){
      v<- FIND_MINIMUM(X,R,i)
      x <- as.vector(c(x, v[1])) 
      y <- c(y, v[2])
      i = x[length(x)]
  
      if(i<n){
        v<- FIND_MAXIMUM(X,R,i)
        x <- c(x, v[1])
        y <- c(y, v[2])
        i <- x[length(x)]
      }
      }

      plot(x,y, xlab="Index", ylab="Values", col='blue')
      lines(X, xlab="Index", ylab="Values", col='green')
    }
functionPointsTable<- function(x) {
      R <- input$Rcislo
      if (is.null(R)) return(NULL)

      y=input$y
      X<-x[,y]
      x[,1] <- as.POSIXct(x[,1])
      x2<-x
      Z<-as.character(x2[,1])
      n=length(X)

      FIND_FIRST<- function(a,R){
        iMin=1
        iMax=1
        i=2
        n=length(a)
  
        while(i<n && a[i]/a[iMin]<R && a[iMax]/a[i]<R){
          if (a[i]<a[iMin])
            iMin=i
          if(a[iMax]<a[i])
            iMax=i
          i=i+1
        }
  
        if (iMin<iMax){
          retx=iMin
          rety=a[iMin]
        }
      
        else {
          retx=iMax
          rety= a[iMax]
        }
      
        return(c(retx,rety))
      }
 
      FIND_MAXIMUM<-function(a,R,i){
        iMax=i
        n=length(a)
        i=i+1
  
        while(i<n && (a[iMax]<R*a[i])){
          if (a[i]>a[iMax])
            iMax=i
          i=i+1
        }
  
        if (i<n){
          retx=iMax
          rety=a[iMax]
        }
  
        else{
          retx=n
          rety=a[n]
        }
    
        return(c(retx,rety))
      }

      FIND_MINIMUM<-function(a,R,i){
        iMin=i
        n=length(a)
        i=i+1
  
        while(i<n && (a[i]<R*a[iMin])){
          if (a[i]<a[iMin])
            iMin=i
          i=i+1
        }
          
        if (i<n){
          retx<-iMin
          rety<-a[iMin]
        }
      
        else{
          retx<-n
          rety<-a[n]
        }

        return(c(retx,rety))
      }

      v <- FIND_FIRST(X,R)

      x = c(v[1])
      y = c(v[2])
      i = x[length(x)]

      if (x[1] < n && y[1] < X[1]){
        v <- FIND_MAXIMUM(X, R, i)
        x <- c(x, c(v[1]))
        y <- c(y, c(v[2]))
        i = x[length(x)]
      }

    
      while (i<n){
        v<- FIND_MINIMUM(X,R,i)
        x <- c(x, v[1]) 
        y <- c(y, v[2])
        i = x[length(x)]

        if(i<n){
          v<- FIND_MAXIMUM(X,R,i)
          x <- c(x, v[1])
          y <- c(y, v[2])
          i <- x[length(x)]
        }
      }

      a = length(y)
      Extrem = c()
  
      for(i in(1:(a-1))){
        if (y[i]<y[i+1]){
          Extrem[i+1]<-c("loc. max")
          Extrem[1]<-c("loc. min")}
        else{
          Extrem[i+1]<-c("loc. min")
          Extrem[1]<-c("loc. max")}
      }

      Date=c()
  
      for(i in(1:a)){
        Date[i]=Z[x[i]]
      }

      Status=c()
      Status[1]=c('-')
  
      for (i in (2:length(Extrem))){
        if (Extrem[i]=='loc. max')
          Status[i]='rises'
        else
          Status[i]='falls'
      }

      Time<-as.POSIXct(Date)
      During_in_min=c()
      During_in_min[1]=c('-')
      for(i in(2:length(Time))){
        During_in_min[i]=as.numeric(difftime(Time[i],Time[i-1], units = c('mins')))
      }

      y<-formatC(y,digits=5,format="f")

      table<-data.frame(Date, y, Extrem, Status, During_in_min)
    }
	
output$tabulka <- renderTable({  functionTable(moje) })	

output$graf <- renderPlot({ functionGraph(moje) })	

output$summary <- renderPrint({ functionSummary(moje) })

output$klzavy <- renderPlot({ functionMoving(moje) }) 

output$downloadPlot <- downloadHandler( 
      filename = function() {paste('graf_KP', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.png', sep='')},
      content = function(file) {
          
        png(file)
        functionMoving(moje)
        dev.off()
    })
	
output$downloadData <- downloadHandler(
      filename = function() {paste('data_KP', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.csv', sep='')},
      content = function(file) {
           
        krok <- input$Krok
        if (is.na(krok)) krok = 0
        
	    y=input$y
        moje[,1] <- as.POSIXct(moje[,1])
        n = krok
        hodnoty<- moje[,y]
        
        mav <- function(hodnoty,n){filter(hodnoty,rep(1/n,n),sides=2)}
        write.csv(mav(hodnoty,n), file)   
    })
	
output$summary1 <- renderPrint({ functionSummary1(moje) })

output$aritmet <- renderPlot({ functionAM(moje) })	

output$downloadPlot1 <- downloadHandler( 
      filename = function() {paste('graf_AP', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.png', sep='')},
      content = function(file) {

        png(file)
        functionAM(moje)
        dev.off()
    })	
	
output$downloadData1 <- downloadHandler(  
      filename = function() {paste('data_AP', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.csv', sep='')},
      content = function(file) {
        if (input$Prepojenie) {
          krok <- input$Krok
          if (is.na(krok)) krok = 0
          f <- input$Frekvencia
          if (is.na(f)) f=0 

          y=input$y
          moje[,1] <- as.POSIXct(moje[,1])
          n = krok
          hodnoty<- moje[,y]
    
          mav <- function(hodnoty,n){filter(hodnoty,rep(1/n,n),sides=2)}

          moje3 <- mav(hodnoty,n)
          l=length(moje3)
          zaznamy=c()
          k=0
      
          for(i in(1:l)){
            if (is.na(moje3[i]))
              k=k+1
            else
              zaznamy[i-k]=moje3[i]
          } 

          N=length(zaznamy)                     ## find out the count of values
          z=N%%f                       ## calculate the remainder of the division
          opak=floor(N/f)                  ##  find out the count of repetitions
          P=c()                             ## create empty vector P
      
          for (i in (0:(opak-1))) {
            P[i+1]=sum(zaznamy[(i*f+1):(f*(i+1))])/f}
          if (z>0) 
            P[opak+1]=sum(zaznamy[(opak*f+1):N])/z
    
          write.csv(P, file)
        }
     
        else {  
          f <- input$Frekvencia
          if (is.na(f)) f=0 
     
          y=input$y	  ##  load the table
          zaznamy=moje[,y]                    ## save values to new variable
          N=length(zaznamy)                     ##  find out the count of values
          z=N%%f                       ## calculate the remainder of the division
          opak=floor(N/f)                  ## find out the count of repetitions
          P=c()                             ## create empty vector P
      
          for (i in (0:(opak-1))) {
            P[i+1]=sum(zaznamy[(i*f+1):(f*(i+1))])/f}
          if (z>0) 
            P[opak+1]=sum(zaznamy[(opak*f+1):N])/z

          df<-data.frame(P)
          write.csv(df,file)
        }
    })
	
output$summary2 <- renderPrint({ functionSummary2(moje) })	

output$body <- renderPlot({ functionPoints(moje) })

output$downloadPlot2 <- downloadHandler(
      filename = function() {paste('graf_DB', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.png', sep='')},
      content = function(file) {

        png(file)
        functionPoints(moje)
        dev.off()
    })
output$downloadData2<- downloadHandler(
      filename = function() {paste('data_DB', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.csv', sep='')},
      content = function(file) {

        R <- input$Rcislo
        if (is.null(R)) return(NULL)

        y=input$y
        X<-moje[,y]
        moje[,1] <- as.POSIXct(moje[,1])
        moje2<-moje
        Z<-as.character(moje2[,1])
        n=length(X)

        FIND_FIRST<- function(a,R){
          iMin=1
          iMax=1
          i=2
          n=length(a)
  
          while(i<n && a[i]/a[iMin]<R && a[iMax]/a[i]<R){
            if (a[i]<a[iMin])
              iMin=i
            if(a[iMax]<a[i])
              iMax=i
            i=i+1
          }
  
          if (iMin<iMax){
            retx=iMin
            rety=a[iMin]
          }
      
          else {
            retx=iMax
            rety= a[iMax]
          }
      
          return(c(retx,rety))
        }

        FIND_MAXIMUM<-function(a,R,i){
          iMax=i
          n=length(a)
          i=i+1
  
          while(i<n && (a[iMax]<R*a[i])){
            if (a[i]>a[iMax])
              iMax=i
            i=i+1
          }
  
          if (i<n){
            retx=iMax
            rety=a[iMax]
          }
  
          else{
            retx=n
            rety=a[n]
          }
    
          return(c(retx,rety))
          }

        FIND_MINIMUM<-function(a,R,i){
          iMin=i
          n=length(a)
          i=i+1
  
          while(i<n && (a[i]<R*a[iMin])){
            if (a[i]<a[iMin])
              iMin=i
            i=i+1
          }
          
          if (i<n){
            retx<-iMin
            rety<-a[iMin]
          }
      
          else{
            retx<-n
            rety<-a[n]
          }

          return(as.vector(c(retx,rety)))
        }

        v <- FIND_FIRST(X,R)

        x = c(v[1])
        y = c(v[2])
        i = x[length(x)]

        if (x[1] < n && y[1] < X[1]){
          v <- FIND_MAXIMUM(X, R, i)
          x <- c(x, c(v[1]))
          y <- c(y, c(v[2]))
        }

        i = x[length(x)]
        while (i<n){
          v<- FIND_MINIMUM(X,R,i)
          x <- as.vector(c(x, v[1])) 
          y <- c(y, v[2])
          i = x[length(x)]
  
          if(i<n){
          v<- FIND_MAXIMUM(X,R,i)
          x <- c(x, v[1])
          y <- c(y, v[2])
          i <- x[length(x)]
          }
        } 

        a = length(y)
        Extrem = c()
  
        for(i in(1:(a-1))){
          if (y[i]<y[i+1]){
            Extrem[i+1]<-c("loc. max")
            Extrem[1]<-c("loc. min")}
          else{
            Extrem[i+1]<-c("loc. min")
            Extrem[1]<-c("loc. max")}
        }

        Date=c()
  
        for(i in(1:a)){
          Date[i]=Z[x[i]]
        }

        Status=c()
        Status[1]=c('-')
  
        for (i in (2:length(Extrem))){
          if (Extrem[i]=='loc. max')
            Status[i]='rises'
          else
            Status[i]='falls'
        }

        Time<-as.POSIXct(Date)
        During_in_min=c()
        During_in_min[1]=c('-')
        for(i in(2:length(Time))){
          During_in_min[i]=as.numeric(difftime(Time[i],Time[i-1], units = c('mins')))
        }

        y<-formatC(y,digits=5,format="f")
        db<- data.frame(x, Date, y, Extrem, Status, During_in_min)

        write.csv(db, file)
      })
	  
output$table <- renderTable({ functionPointsTable(moje) })

observe({
	
    inFile <- input$file1
	if (is.null(inFile)) return(NULL)
	
	mojedata=read.delim(inFile$datapath, header=input$header, sep=input$sep)
    updateSelectInput(session, "y", " Select attribute:", colnames(mojedata)[2:ncol(mojedata)], names(mojedata)[[2]])
		
####################################################Table#################################################  
    output$tabulka <- renderTable({  functionTable(mojedata) })
######################################################### Graph #########################################################
    output$graf <- renderPlot({ functionGraph(mojedata) })
##################################################################################################################################
    output$summary <- renderPrint({ functionSummary(mojedata) })
########################################################### Graph MA ################################################################
    output$klzavy <- renderPlot({ functionMoving(mojedata) }) 
######################################################################################################################
    output$summary1 <- renderPrint({ functionSummary1(mojedata) })
################################################################ Download Graph MA ########################################################
    output$downloadPlot <- downloadHandler( 
      filename = function() {paste('graf_KP', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.png', sep='')},
      content = function(file) {
          
        png(file)
        functionMoving(mojedata)
        dev.off()
    })
############################################################### Download Data MA ################################################################
    output$downloadData <- downloadHandler(
      filename = function() {paste('data_KP', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.csv', sep='')},
      content = function(file) {
           
        inFile <- input$file1
        krok <- input$Krok
        if (is.na(krok)) krok = 0
        if (is.null(inFile)) return(NULL)

        mojedata=read.delim(inFile$datapath, header=input$header, sep=input$sep)
	      y=input$y
        mojedata[,1] <- as.POSIXct(mojedata[,1])
        n = krok
        hodnoty<- mojedata[,y]
        
        mav <- function(hodnoty,n){filter(hodnoty,rep(1/n,n),sides=2)}
        write.csv(mav(hodnoty,n), file)   
    })
############################################################### Graph AM #############################################################
    output$aritmet <- renderPlot({ functionAM(mojedata) })
##########################################################################################################################
    output$summary2 <- renderPrint({ functionSummary2(mojedata) })
############################################################### Download Graph AM  ############################################  
    output$downloadPlot1 <- downloadHandler( 
      filename = function() {paste('graf_AP', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.png', sep='')},
      content = function(file) {

        png(file)
        functionAM(mojedata)
        dev.off()
    })
####################################################### Download Data AM ################################################## 
    output$downloadData1 <- downloadHandler(  
      filename = function() {paste('data_AP', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.csv', sep='')},
      content = function(file) {
        if (input$Prepojenie) {
          inFile <- input$file1
          krok <- input$Krok
          if (is.na(krok)) krok = 0
          if (is.null(inFile)) return(NULL)
          f <- input$Frekvencia
          if (is.na(f)) f=0 

          mojedata=read.delim(inFile$datapath, header=input$header, sep=input$sep)
		      y=input$y
          mojedata[,1] <- as.POSIXct(mojedata[,1])
          n = krok
          hodnoty<- mojedata[,y]
    
          mav <- function(hodnoty,n){filter(hodnoty,rep(1/n,n),sides=2)}

          mojedata3 <- mav(hodnoty,n)
          l=length(mojedata3)
          zaznamy=c()
          k=0
      
          for(i in(1:l)){
            if (is.na(mojedata3[i]))
              k=k+1
            else
              zaznamy[i-k]=mojedata3[i]
          } 

          N=length(zaznamy)                     ## find out the count of values
          z=N%%f                       ## calculate the remainder of the division
          opak=floor(N/f)                  ## find out the count of repetitions
          P=c()                             ## create empty vector P
      
          for (i in (0:(opak-1))) {
            P[i+1]=sum(zaznamy[(i*f+1):(f*(i+1))])/f}
          if (z>0) 
            P[opak+1]=sum(zaznamy[(opak*f+1):N])/z
    
          write.csv(P, file)
        }
     
        else {  
          inFile <- input$file1
          f <- input$Frekvencia
          if (is.null(inFile)) return(NULL)
          if (is.na(f)) f=0 
     
          mojedata=read.delim(inFile$datapath, header=input$header, sep=input$sep)
          y=input$y	  ## load the table
          zaznamy=mojedata[,y]                    ## save values to new variable
          N=length(zaznamy)                     ##  find out the count of values
          z=N%%f                       ## calculate the remainder of the division
          opak=floor(N/f)                  ## find out the count of repetitions
          P=c()                             ## create empty vector P
      
          for (i in (0:(opak-1))) {
            P[i+1]=sum(zaznamy[(i*f+1):(f*(i+1))])/f}
          if (z>0) 
            P[opak+1]=sum(zaznamy[(opak*f+1):N])/z

          df<-data.frame(P)
          write.csv(df,file)
        }
    })
########################################################################### Important Points #######################################  
    output$body <- renderPlot({ functionPoints(mojedata) })

########################################### Table #############################################################################################
    output$table <- renderTable({ functionPointsTable(mojedata) })

##############################################################################################################################
    output$downloadPlot2 <- downloadHandler(
      filename = function() {paste('graf_DB', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.png', sep='')},
      content = function(file) {

        png(file)
        functionPoints(mojedata)
        dev.off()
    })
############################################ Turning Points- download ######################################  
    output$downloadData2<- downloadHandler(
      filename = function() {paste('data_DB', format(Sys.time(),"%Y_%b_%d_%H_%M_%S"), '.csv', sep='')},
      content = function(file) {

        inFile <- input$file1 
        if (is.null(inFile)) return(NULL)
        R <- input$Rcislo
        if (is.null(R)) return(NULL)

        mojedata=read.delim(inFile$datapath, header=input$header, sep=input$sep)
        y=input$y
        X<-mojedata[,y]
        mojedata[,1] <- as.POSIXct(mojedata[,1])
        mojedata2<-mojedata
        Z<-as.character(mojedata2[,1])
        n=length(X)

        FIND_FIRST<- function(a,R){
          iMin=1
          iMax=1
          i=2
          n=length(a)
  
          while(i<n && a[i]/a[iMin]<R && a[iMax]/a[i]<R){
            if (a[i]<a[iMin])
              iMin=i
            if(a[iMax]<a[i])
              iMax=i
            i=i+1
          }
  
          if (iMin<iMax){
            retx=iMin
            rety=a[iMin]
          }
      
          else {
            retx=iMax
            rety= a[iMax]
          }
      
          return(c(retx,rety))
        }

        FIND_MAXIMUM<-function(a,R,i){
          iMax=i
          n=length(a)
          i=i+1
  
          while(i<n && (a[iMax]<R*a[i])){
            if (a[i]>a[iMax])
              iMax=i
            i=i+1
          }
  
          if (i<n){
            retx=iMax
            rety=a[iMax]
          }
  
          else{
            retx=n
            rety=a[n]
          }
    
          return(c(retx,rety))
          }

        FIND_MINIMUM<-function(a,R,i){
          iMin=i
          n=length(a)
          i=i+1
  
          while(i<n && (a[i]<R*a[iMin])){
            if (a[i]<a[iMin])
              iMin=i
            i=i+1
          }
          
          if (i<n){
            retx<-iMin
            rety<-a[iMin]
          }
      
          else{
            retx<-n
            rety<-a[n]
          }

          return(as.vector(c(retx,rety)))
        }

        v <- FIND_FIRST(X,R)

        x = c(v[1])
        y = c(v[2])
        i = x[length(x)]

        if (x[1] < n && y[1] < X[1]){
          v <- FIND_MAXIMUM(X, R, i)
          x <- c(x, c(v[1]))
          y <- c(y, c(v[2]))
        }

        i = x[length(x)]
        while (i<n){
          v<- FIND_MINIMUM(X,R,i)
          x <- as.vector(c(x, v[1])) 
          y <- c(y, v[2])
          i = x[length(x)]
  
          if(i<n){
          v<- FIND_MAXIMUM(X,R,i)
          x <- c(x, v[1])
          y <- c(y, v[2])
          i <- x[length(x)]
          }
        } 

        a = length(y)
        Extrem = c()
  
        for(i in(1:(a-1))){
          if (y[i]<y[i+1]){
            Extrem[i+1]<-c("loc. max")
            Extrem[1]<-c("loc. min")}
          else{
            Extrem[i+1]<-c("loc. min")
            Extrem[1]<-c("loc. max")}
        }

        Date=c()
  
        for(i in(1:a)){
          Date[i]=Z[x[i]]
        }

        Status=c()
        Status[1]=c('-')
  
        for (i in (2:length(Extrem))){
          if (Extrem[i]=='loc. max')
            Status[i]='rises'
          else
            Status[i]='falls'
        }

        Time<-as.POSIXct(Date)
        During_in_min=c()
        During_in_min[1]=c('-')
        for(i in(2:length(Time))){
          During_in_min[i]=as.numeric(difftime(Time[i],Time[i-1], units = c('mins')))
        }

        y<-formatC(y,digits=5,format="f")
        db<- data.frame(x, Date, y, Extrem, Status, During_in_min)

        write.csv(db, file)
      })
    })
})
