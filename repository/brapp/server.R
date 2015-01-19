##function for finding common earliest date between 2 vectors or a matrix of a number of vectors. if 1
## argument is given before lengthdata, then method for matrix is used, else method for 2 vectors is used.
##  $first will give adjusted first data, $second will give adjusted second data, and $earliest will give index of where the first valid point is.

library("ggplot2")
library("PerformanceAnalytics")
library("lubridate")
library("shiny")
library(reshape2)
library(e1071)
library(functional)

##these are some general functions I have created for use within the server.
earliestcommon = function(first=NULL, second=NULL, lengthdata)
{
  if(missing(lengthdata))
  {
    stop("give a length of all your data. (argument must be like lenghdata=)")
  }
  
  if(class(first) == "numeric" && class(second) == "numeric")
  {
    first = first[!is.na(first)]
    second = second[!is.na(second)]
    
    if(length(first)> length(second))
    {
      first = first[ ((length(first)-length(second))+1): length(first)  ]  
    }
    if (length(second) >length(first))
    {
      second = second [((length(second)-length(first))+1):length(second)]
    }
    returnstuff = new.env()
    returnstuff$first = first
    returnstuff$second = second
    returnstuff$earliest = (lengthdata+1)-length(first)
    returnstuff
    
  }
  
  else if(class(first) == "matrix" && is.null(second) && nrow(first)>1)
  {
    first = first[,apply(!is.na(first), 2, all)]
    returnstuff = new.env()
    returnstuff$first = first
    returnstuff$earliest = (lengthdata+1)-ncol(first)
    returnstuff
  }
  
  else if(class(first)=="matrix" && is.null(second) && nrow(first)==1)
  {
    first = first[,apply(!is.na(first), 2, all)]
    returnstuff = new.env()
    returnstuff$first = first
    returnstuff$earliest = (lengthdata+1)-NROW(first)
    returnstuff
  }
  else
  {stop("vector or matrix por favor. and enter them correctly you dunce.")}
  
}

inforat = function(fund, riskfree, use = NULL)
{
  if( use == "pairwise")
  {
    fundtemp = fund[complete.cases(fund,riskfree)]
    riskfree = riskfree[complete.cases(fund,riskfree)]
    fund = fundtemp
  }
  
  excessret = fund - riskfree
  if(!identical(fund,riskfree)){
    theratio = mean(excessret, na.rm=T) / sd(excessret, na.rm=T)
  }
  else  ##case where fund and riskfree are the same
  {theratio = 0}
  theratio*sqrt(12)  ##annualized
  
}

dancumreturn = function(fund)
{
  fund = fund[complete.cases(fund)]
  prod(1 + fund) - 1
}

#runs when app is launched

shinyServer(
  function(input, output, clientData, session) {
    
    reholderret = reactive({
      ##read the file input
      inFile <- input$monthrets
      if (is.null(inFile))
        return(NULL)
      
      ##is file type ok? 
      potentialinputerror= try(read.csv(inFile$datapath,sep = ","), silent = T )  
      if(class(potentialinputerror) =="try-error")
      {   ##our environment for data is bad! this is a read.csv error in R.
        thedata = new.env()
        thedata$oldholder = "badfile"
        
        return(thedata)
        ##environment is returned
      }
      
      # adjust the inputted data:  
      holderret = read.csv(inFile$datapath,sep = ",")
      trueOld = holderret
      
      holderret = as.matrix(holderret)
      
      rownames(holderret) = holderret[,1]
      
      holderret = (holderret[,-c(1)])
      
      ##test if the rows are dates, if so transpose the matrix
      isrowdate = rownames(holderret)
      isrowdate = gsub("X", "", isrowdate)
      isrowdate = gsub("\\.","-",isrowdate)
      
      isit = try(as.Date(isrowdate), silent = T)
      
      if(class(isit)!="try-error") ##if this is the case, the rows of the file can be converted to dates
      {
        holderret = t(holderret) ##transpose
      }
      
      ##check if any of the funds have insufficient data; if so, they will be deleted from the data used by the matrix generators.
      newholderret=holderret
      
      if(NROW(holderret)>1&&NCOL(holderret)>1) ##our data could be bad here(ie. it was read correctly but there is not enough of it)
      {
        newholderret = newholderret[rowSums(!is.na(holderret))>12, ]
      }
      else
      {   ##our environment for data is bad! there is only 1 row and/or 1 column in the data.(or less)
        thedata = new.env()
        thedata$oldholder = "notenough"
        
        return(thedata)
        ##environment is returned
      }
      ##error checking if data is bad here
      if(length(newholderret)==0 || ncol(holderret)<=60)
      {   ##our environment for data is bad! the rows have not enough or invalid data.
        thedata = new.env()
        thedata$oldholder = "notenough"
        
        return(thedata)
        ##environment is returned
      }
      
      ##trying to get dates from excel file (making format yyyy-mm-dd)
      dates = colnames(holderret)
      dates = gsub("X", "", dates)
      dates = gsub("\\.","-",dates)
      
      isit = try(as.Date(dates), silent = T)  #are the dates convertible to date class?
      
      if(class(isit)=="try-error")
      {
        thedata = new.env()
        thedata$oldholder = "baddate"
        thedata$dates = ""
        return(thedata)
      }
      ##dimensions of compscores and holderret may differ, bad if it does
      if(!identical(dim(recompscores())[2],dim(trueOld)[2]))
      {thedata = new.env()
       thedata$oldholder = "compandretdiffer"
       thedata$holderret = newholderret
       thedata$dates = dates
       return(thedata)}
      
      
      ##make environment for data
      thedata = new.env()
      thedata$holderret = newholderret
      thedata$oldholder = "holder is ok"
   
      thedata$dates = dates
 
      
      thedata  ##environment is returned
    })
    
    rematrixscores = reactive({
      ##read the file input
      inFile <- input$inmatscores
      if (is.null(inFile))
        return(NULL)
      
      ##is file type ok? 
      potentialinputerror= try(read.csv(inFile$datapath,sep = ","), silent = T )  
      if(class(potentialinputerror) =="try-error")
      {   
        ##read.csv error in matrix scores
        return("badscores")
        
      }
      
      matscores = read.csv(inFile$datapath,sep = ",")
      
      if(NROW(matscores)<=1&&NCOL(matscores)<=1) ##our data could be bad here
      { return("notenough")}
      
      if(length(matscores)==0 || ncol(matscores)<=60)
      {return("notenough")}
      
      matscores = as.matrix(matscores)
      
      rownames(matscores) = matscores[,1]
      
      matscores = matscores[,-c(1)]
      
      if(!identical(dim(matscores)[2],dim(reholderret()$holderret)[2]))
      {return("notsame")}
        
      matscores
      
      
    })
    
    recompscores = reactive({
      ##read the file input
      inFile <- input$incompscores
      if (is.null(inFile))
        return(NULL)
      
      ##is file type ok? 
      potentialinputerror= try(read.csv(inFile$datapath,sep = ","), silent = T )  
      if(class(potentialinputerror) =="try-error")
      {   
        ##read.csv error in matrix scores
        return("badscores")
        
      }
      
      compscores = read.csv(inFile$datapath,sep = ",")
      
      if(NROW(compscores)<=1&&NCOL(compscores)<=1) ##our data could be bad here
      { return("notenough")}
      
      if(length(compscores)==0 || ncol(compscores)<=60)
      {return("notenough")}
      
      compscores
      
      
    })
    

    
    output$graphdates1 = renderUI({
      
      ##check both comp scores and returns areinputted
      validate(need(!is.null(reholderret()), ""),
               need(!is.null(recompscores()),"")
      )
      dates=reholderret()$dates
      dates = try(as.Date(dates), silent = T)  #are the dates convertible to date class?
      
      dateRangeInput("ldaterange1", label= "Graph Dates", start = dates[1], end = dates[length(dates)], min = dates[1], max = dates[length(dates)], startview = "year",weekstart =1 )
     
      
    })
    
 
    
    output$tabledate1 = renderUI({
      
      ##check both comp scores and returns areinputted
      validate(need(!is.null(reholderret()), ""),
               need(!is.null(recompscores()),"")
      )
      dates=reholderret()$dates
      
      dates = try(as.Date(dates), silent = T)  #are the dates convertible to date class?
      
      dateRangeInput( 'date1',
                label = "Table Dates", start = dates[1], end = dates[length(dates)], min = dates[1], max = dates[length(dates)], startview = "year",weekstart =1 )
      
    })
    
 
    
    
    output$warnings = renderText({
      
      validate( need(!is.null(reholderret()$dates), ""))
      validate( need(reholderret()$oldholder!="baddate", "Dates of return file are probably in incorrect format (must be yyyy-mm-dd, and must each be on the 1st of the month)"),
                need(reholderret()$oldholder!="badfile", "I can't read the returns file."  ),
                need(reholderret()$oldholder!="notenough", "Not enough data in returns file."  ),
                need(reholderret()$oldholder!="compandretdiffer" || is.null(recompscores()), "The Composite and Return files differ in dimensions."),
                need(reholderret()$dates!="dontworry","")
                ,errorClass = "redalert"  )
      
      dates=reholderret()$dates
      ##give an error message 
     
      
      validate(
        need(mday(dates[1]) ==1, "Dates of return file are probably in incorrect format (must be yyyy-mm-dd, and must each be on the 1st of the month)") 
        ,errorClass = "redalert"  )
      
      validate(
        need(mday(dates[length(dates)]) ==1, "Dates of return file are probably in incorrect format (must be yyyy-mm-dd, and must each be on the 1st of the month)") 
        ,errorClass = "redalert" )
      
      validate(
        need(year(dates[1])!=1, "Dates of return file are probably in incorrect format (must be yyyy-mm-dd, and must each be on the 1st of the month)")
        ,errorClass = "redalert")
      
    
      ##
      
    })
    
    
    output$selectbox1 = renderUI({
      
      
      
      validate(need(!is.null(recompscores()), ""))
      
      compscores = recompscores()
      
      
        selectInput("text1", label="Select Funds (options are from composite scores file)" , choices = as.character(compscores[,1]),  multiple = T
                     , width = "1000%" 
        )
      
      
    })
    
  
    
    output$benchmark1 = renderUI({
      
      
      validate(need(!is.null(reholderret()), "") )
      
      holderret = reholderret()$holderret
    
        selectInput("bench1", label="Benchmark for Correlation(options are from returns file)", choices = as.character(rownames(holderret)),
                  multiple = T
        )
      
    })
    
    output$riskfreeui = renderUI({
      
      
      validate(need(!is.null(reholderret()), "") )
      
      holderret = reholderret()$holderret
      
      ##we must check if the funds have NAs, if so they cannot be used for benchmarks
      holderret = holderret[rowSums(is.na(holderret))<1, ]
      
      selectInput("riskfree1", label="Risk free rate(options are from returns file)", choices = as.character(rownames(holderret)),
             selected = rownames(holderret)[length(rownames(holderret))]     #,width = "60%" 
      )
      
    })
    
    
    output$graph1 = renderPlot ({
      
      validate(
        need(!is.null(reholderret()$dates), "No returns file."),
        need(!is.null(recompscores()), "No composite score file.")
        
      )
    
      ##pick a fund
      
      validate(need(input$text1 != "", "Go ahead and pick a fund to compare."))
      
      ##check if dates are right
      validate(
        need(as.numeric(input$ldaterange1[1]) <= as.numeric(input$ldaterange1[2]), "Check your dates. :)") )
  
      
      
      validate(
        need(mday(input$ldaterange1[1]) ==1, "The dates chosen must be on the 1st of the month.") )
      
      validate(
        need(mday(input$ldaterange1[2]) ==1, "The dates chosen must be on the 1st of the month.") )
     
 
      ##get composite scores
      holder1 = recompscores()
      
      holder1 = as.matrix(holder1)
      
      rownames(holder1) = holder1[,1]
      
      holder1 = holder1[,-c(1)]
      
            
      #daterange of data
      gdaterange = reholderret()$dates
   gdaterange = try(as.Date(gdaterange), silent = T)  #are the dates convertible to date class?
      
      
      
   dataholder = matrix(nrow = ncol(holder1), ncol = length(input$text1))
   
   for (i in 1:length(input$text1))
   {
     dataholder[,i] = as.numeric(holder1[match(as.character(input$text1[i]), rownames(holder1) ),] )
     
   }
   
   
   ##adjust data so it is only the dates we want to look at
   
   ##make a new matrix the length of the graph daterange
   adjdataholder = matrix(nrow = length(c(match(input$ldaterange1[1],gdaterange):match(input$ldaterange1[2],gdaterange))), 
                          ncol = length(input$text1))
   
   for (i in 1:length(input$text1))
   {
     
     adjdataholder[,i] = as.numeric(dataholder[match(input$ldaterange1[1],gdaterange):match(input$ldaterange1[2],gdaterange),i])
   }
   
   
      
      adjdaterange = seq(as.Date(input$ldaterange1[1]), as.Date (input$ldaterange1[2]), by = "month")
      
      
      #at positions on axis
      axposit = rep(0, length(adjdaterange))
      
      for( i in 1:length(adjdaterange))
      {
        axposit[i] = i
        
      }
      
      ##plot the graph for matrix composite score 
      
   
        ##make a rainbow vector based on number of funds selected
   rbow = rainbow(length(input$text1))
    
     
      
   ##plot the first selected data vector
   
        plot(adjdataholder[,1], ylim=c(0,100), main = "Composite Scores",xlab= "Date", ylab = "Composite Score", axes = F,type= "l",col=rbow[1])
         
         axis(side = 1, at = axposit , labels = adjdaterange
         )
         axis(side = 2)
        
        #      , at=seq(0,70,10))
   
        ##plot all the rest in the multi-select (only if there is more than 1)
   
   if( ncol(adjdataholder)>1)
   {
      for(i in 1:(length(input$text1)-1))
      {
       lines(adjdataholder[,i+1], type="l", col = rbow[i+1])
        
      }
   
   }
        
        if(input$hidelegend == F){ legend( 
          input$legendmove*10,100, input$text1, fill = rbow  )
        
        } 
      
    })
   
   
   ##graph for returns
output$retgraph = renderPlot({
     validate(
       need(!is.null(reholderret()$dates), ""),
       need(!is.null(recompscores()), "")
       
     )

     ##pick a fund
     
     validate(need(input$text1 != "", ""))
     
     ##check if dates are right
     validate(
       need(as.numeric(input$ldaterange1[1]) <= as.numeric(input$ldaterange1[2]), "") )
     
     validate(
       need(mday(input$ldaterange1[1]) ==1, "") )
     
     validate(
       need(mday(input$ldaterange1[2]) ==1, "") )
     
     #daterange of data
     gdaterange = reholderret()$dates
     gdaterange = try(as.Date(gdaterange), silent = T)  #are the dates convertible to date class?
     
     ##get holderret
     holderret = reholderret()$holderret
     
     dataholder = matrix(nrow = ncol(holderret), ncol = length(input$text1))
     
     for (i in 1:length(input$text1))
     {
       dataholder[,i] = as.numeric(holderret[match(as.character(input$text1[i]), rownames(holderret) ),] )
       
     }
     ##get index of NAs
     nalholder = c(1:ncol(dataholder))
     
     for( i in 1:length(nalholder))
     {
       nalholder[i] = length(dataholder[is.na(dataholder[,i]),i])
     }
     

     
     ##fill NAs where data is not
     
     for(i in 1:length(input$text1))
     {
       for(j in 1:nalholder[i])
       {
         dataholder[j,i] = NA
       }
     }
     
     ##adjust data so it is only the dates we want to look at
     
     ##make a new matrix the length of the graph daterange
     adjdataholder = matrix(nrow = length(c(match(input$ldaterange1[1],gdaterange):match(input$ldaterange1[2],gdaterange))), 
                            ncol = length(input$text1))
     
     for (i in 1:length(input$text1))
     {
       
       adjdataholder[,i] = as.numeric(dataholder[match(input$ldaterange1[1],gdaterange):match(input$ldaterange1[2],gdaterange),i])
     }

     adjdaterange = seq(as.Date(input$ldaterange1[1]), as.Date (input$ldaterange1[2]), by = "month")
     
     cumdataholder = adjdataholder
     ##adjust data to get Return cumulative for each point
     for(i in 1:length(input$text1))
     {
       for(j in 1:nrow(adjdataholder))
       {
         cumdataholder[j,i] = dancumreturn(adjdataholder[1:j,i])
       }
     }
     adjdataholder = cumdataholder*100
     
     #at positions on axis
     axposit = rep(0, length(adjdaterange))
     
     for( i in 1:length(adjdaterange))
     {
       axposit[i] = i
       
     }
     
     ##plot the graph for returns

     ##make a rainbow vector based on number of funds selected
     rbow = rainbow(length(input$text1))
     
     ##plot the first selected data vector
     
     plot(adjdataholder[,1],  ylim=c(min(na.omit(adjdataholder)),max(na.omit(adjdataholder))), main = "Returns",xlab= "Date", ylab = "Return(%)", axes = F,type= "l",col=rbow[1])
     
     axis(side = 1, at = axposit , labels = adjdaterange
     )

     ##plot all the rest in the multi-select (only if there is more than 1)
     
     if( ncol(adjdataholder)>1)
     {
       for(i in 1:(length(input$text1)-1))
       {
         lines(adjdataholder[,i+1], type="l", col = rbow[i+1])
         
       }
       
     }
     axis(side=2)
 
})
    ##reactive for holding table data
tables = reactive({
  ##first table only if there is a matrix score file
  if(class(rematrixscores())=="matrix")
  {
    dates=reholderret()$dates
  datesasdates =as.Date("0001-01-01")
  datesasdates = try(as.Date(dates), silent = T)  #are the dates convertible to date class?
  
  ##get composite scores
  holder1 = recompscores()
  
  holder1 = as.matrix(holder1)
  
  rownames(holder1) = holder1[,1]
  
  holder1 = holder1[,-c(1)]
  
  
  #daterange of data
  gdaterange = reholderret()$dates
  
  ##this is matrix scores

  holder2 = rematrixscores()
  
  ##tihs is returns
  holderret = reholderret()$holderret
  
  ##cum return
  retdataholder  = matrix(nrow = ncol(holderret), ncol = length(input$text1))
  
  for (i in 1:length(input$text1))
  {
    retdataholder[,i] = as.numeric(holderret[match(as.character(input$text1[i]), rownames(holderret) ),] )
    
  }
  newdata = retdataholder
  #common date.
  
  newdata = newdata[apply(newdata, 1, Compose(is.finite, all)),]
  
  shortest = min(length(dates),(length(newdata)/length(input$text1)) )
  
  
  newdaterange = dates[((length(dates)+1)-shortest):length(dates)]
  
  validate(need(newdaterange[1]<input$date1[2],"Your date range is out of range of your picked funds." ))
  
  ##function to find a position in a matrix where the second value equals the first
  
  compare = function(first,second){
    temp = 0
    
    
    for(i in 1:(length(first)) )
    {
      if(first[i] == second)
      {
        temp = i
      }
      
    }   
    return(temp)  
  }
  
  
  ## function to get average matrix score for a date range and a set of data
  avmxscore = function(date1,date2,thedata,daterange){
    
    
    begin = compare(daterange,date1)
    end = compare(daterange,date2)
    avmx = mean(thedata[c(begin:end)], na.rm=T)
    
    ##   if(avmx == NULL)
    ##   {
    ##   avmx = c("No Data.")
    ##  }
    
    return(round(avmx,3))
  }
  
  
  
  finholder = matrix(nrow = 6, ncol = length(input$text1))
  dimnames(finholder) = list(c("Av. Matrix Score","Av. Composite Score","Total Return (%)", "Worst month return (%)", "Compound Return (%)", "Month of Worst Return"),input$text1)
  
  ##the date range for data
  daterange = dates
  
  ##make a matrix based on selected funds (no checkbox selection yet)
  #no.of funds : multi select now
  
  ##average comp score
  
  compdataholder = matrix(nrow = ncol(holder1), ncol = length(input$text1))
  
  for (i in 1:length(input$text1))
  {
    compdataholder[,i] = as.numeric(holder1[match(as.character(input$text1[i]), rownames(holder1) ),] )
    
  }
  
  ##average matrix score
  
  msdataholder  = matrix(nrow = ncol(holder2), ncol = length(input$text1))
  
  for (i in 1:length(input$text1))
  {
    msdataholder[,i] = as.numeric(holder2[match(as.character(input$text1[i]), rownames(holder2) ),] )
    
  }
  
  ##cum return
  retdataholder  = matrix(nrow = ncol(holderret), ncol = length(input$text1))
  
  for (i in 1:length(input$text1))
  {
    retdataholder[,i] = as.numeric(holderret[match(as.character(input$text1[i]), rownames(holderret) ),] )
    
  }
  
  ##function for cum ret
  cumretperiod = function(date1,date2,thedata,daterange){
    
    
    begin = compare(daterange,date1)
    end = compare(daterange,date2)
    cumr = Return.cumulative(thedata[c(begin:end)])
    cumr = cumr*100
    
    return(round(cumr,3))
  }
  comretperiod = function(date1,date2,thedata,daterange){
    
    
    begin = compare(daterange,date1)
    end = compare(daterange,date2)
    comr = Return.annualized(thedata[c(begin:end)], scale = 12)
    comr = comr*100
    
    return(round(comr,3))
  }
  minretperiod = function(date1,date2,thedata,daterange){
    
    
    begin = compare(daterange,date1)
    end = compare(daterange,date2)
    cumr = min(thedata[c(begin:end)], na.rm = T)
    cumr = cumr*100
    
    return(round(cumr,3))
  }
  
  ##finding month of lowest return in period
  
  #  library("lubridate")
  minmonth = function(date1,date2,thedata,daterange){
    
    
    begin = compare(daterange,date1)
    end = compare(daterange,date2)
    cumr = min(thedata[c(begin:end)], na.rm = T)
    
    indexRow = which(cumr == thedata[c(begin:end)])
    numb = seq(as.Date(date1), as.Date (date2), by = "month")
    
    datereq = as.Date(numb[indexRow])
    datereq = as.character(datereq)
    
    if(length(datereq>0) && length(datereq)==1){
      return(datereq)
    }
    else if(length(datereq)>1)
    {
      return(datereq[1])
    }
  }
  
  
  ##filling up the data
  
  for(j in 1:ncol(finholder))
  {
    finholder[1,j] = avmxscore(as.Date(input$date1[1]), as.Date(input$date1[2]), msdataholder[,j], daterange)
    finholder[2,j] = avmxscore(as.Date(input$date1[1]), as.Date(input$date1[2]), compdataholder[,j], daterange)
    finholder[3,j] = cumretperiod(as.Date(input$date1[1]), as.Date(input$date1[2]), retdataholder[,j], daterange)
    finholder[4,j] = minretperiod(as.Date(input$date1[1]), as.Date(input$date1[2]), retdataholder[,j], daterange)
    finholder[5,j] = comretperiod(as.Date(input$date1[1]), as.Date(input$date1[2]), retdataholder[,j], daterange)
    finholder[6,j] = minmonth(as.Date(input$date1[1]), as.Date(input$date1[2]), retdataholder[,j], daterange)
  }
  
  ##matrix is filled, show it
  
  table1=finholder
  }
  else
  {table1 = NULL}
  
  dates=reholderret()$dates
  
  dates = try(as.Date(dates), silent = T)  #are the dates convertible to date class?
  
  ##tihs is returns
  holderret = reholderret()$holderret
  ##daterange
  daterange = dates
  
  ##cum return
  retdataholder  = matrix(nrow = ncol(holderret), ncol = length(input$text1))
  
  for (i in 1:length(input$text1))
  {
    retdataholder[,i] = as.numeric(holderret[match(as.character(input$text1[i]), rownames(holderret) ),] )
    
  }
  
  newdata = retdataholder*100
  
  
  ##trash row
  benchmark = as.numeric(holderret[1,] )
  newdata = cbind(benchmark,newdata)
  
  retdataholder = cbind(benchmark, retdataholder)
  
  #common date.
  if( input$commondate == T)
  {
    newdata = newdata[apply(newdata, 1, Compose(is.finite, all)),]
    
    shortest = min(length(daterange),(length(newdata)/length(input$text1)) )
    
    
    newdaterange = daterange[((length(daterange)+1)-shortest):length(daterange)]
    
  }
  
  
  
  ##create holder matrix for summary stats
  finholder = matrix(nrow = 8, ncol = length(input$text1))
  dimnames(finholder) = list(c("Mean","Skew","Median", "75% Quantile", "25% Quantile", "Kurtosis", "Standard Deviation",paste("Revised Sharpe Ratio:",input$riskfree1) ),input$text1)
  
  ##get riskfree for inforat
  riskfree =as.numeric(holderret[match(as.character(input$riskfree1), rownames(holderret) ),] )
  
  
  for(i in 1:length(input$text1))
    
  {
    finholder[1,i] = mean(newdata[,i+1], na.rm=T)
    finholder[2,i] = skewness(newdata[,i+1], na.rm=T)
    finholder[3,i] = median(newdata[,i+1],na.rm = T)
    finholder[4,i] = quantile(newdata[,i+1], c(.75), na.rm =T)
    finholder[5,i] = quantile(newdata[,i+1], c(.25), na.rm =T)
    finholder[6,i] = kurtosis(newdata[,i+1], na.rm=T)
    
    
    finholder[7,i] = sd(newdata[,i+1],na.rm = T)*sqrt(12)  ##annualized
    finholder[8,i] = inforat(retdataholder[,i+1],riskfree, use = "pairwise")
    
  }
  
  
  ##print the matrix with sum stats
  distable = finholder
  ##if at least one benchmark is entered
  if(!is.null(input$bench1))
 { 
   dates=reholderret()$dates
  
  dates = try(as.Date(dates), silent = T)  #are the dates convertible to date class?
  
  ##tihs is returns
  holderret = reholderret()$holderret
  
  
  
  ##cum return
  retdataholder  = matrix(nrow = ncol(holderret), ncol = length(input$text1))
  
  for (i in 1:length(input$text1))
  {
    retdataholder[,i] = as.numeric(holderret[match(as.character(input$text1[i]), rownames(holderret) ),] )
    
  }
  
  ##also get a matrix of benchmarks for correlation
  benchdataholder = matrix(nrow = ncol(holderret), ncol = length(input$bench1))
  for (i in 1:length(input$bench1))
  {
    benchdataholder[,i] = as.numeric(holderret[match(as.character(input$bench1[i]), rownames(holderret) ),] )
    
  }
  ##trash row
  trashrow = as.numeric(holderret[1,] )
  newdata = retdataholder
  newbench=benchdataholder
  newdata = cbind(trashrow,newdata)
  newbench = cbind(trashrow,newbench)
  
  
  cornames = paste("Correlation ","(",input$bench1,")", sep = "")
  
  ##create holder matrix for summary stats
  finholder = matrix(nrow = length(input$bench1), ncol = length(input$text1))
  dimnames(finholder) = list(cornames,input$text1)
  
  for(i in 1:length(input$text1))
    
  {
    for(j in 1:length(input$bench1))
    { 
      finholder[j,i] = cor(newdata[,i+1], newbench[,j+1], use="pairwise.complete.obs")[1]  ##+1 to avoid trash row
      
      ##pairwise is this amazing feature which only takes matching pairs of the two vectors (it uses complete.cases, which I need to use more)
      ## so data used will be only data where there is not an NA in either vector
    }
    
  }
  ##print correlation matrix
  cortable = finholder
 }
 else
 {cortable = NULL}
  
  
  tableEnv = new.env()
  tableEnv$table1 = table1
  tableEnv$distable = distable
  tableEnv$cortable = cortable
 tableEnv$fundexcel = switch(input$tabletype, "Table 1"=table1, "Table 2"=distable, "Correlation Table"=cortable)
  tableEnv
  
})
    ##making the table
    
    output$table1 = renderTable ({
      
      validate( need(!is.null(reholderret()$dates), ""),
                need(!is.null(rematrixscores()), "No matrix score file."))
      
      
      validate(need(class(rematrixscores())!="character" || rematrixscores()!="notsame", "Dimensions for matrix scores and returns differ."),
               need(rematrixscores()!="notenough" || class(rematrixscores())!="character" , "Matrix scores don't have enough data."),
               need(rematrixscores()!="badscores" || class(rematrixscores())!="character" , "Bad Matrix Score file."),
               
               need(!is.null(recompscores()), "")
      )
      
     
      
      ##pick a fund
      validate(
        need(input$text1 != "", "")
      )
      
      ##check if dates are right
      validate(
        need(as.numeric(input$date1[1]) <= as.numeric(input$date1[2]), "Check your dates. :)") )

      
      
      validate(
        need(mday(input$date1[1]) ==1, "The dates chosen must be on the 1st of the month.") )
      
      validate(
        need(mday(input$date1[2]) ==1, "The dates chosen must be on the 1st of the month.") )
      ##

      tables()$table1
    })
    
    output$histgraph1 = renderPlot ({
      
      
      ##pick a fund
      validate(
        need(input$text1 != "", "")
      )
      #is there a return file
      validate(need(!is.null(reholderret()), "No returns file."))
      
      ##tihs is returns
     holderret = reholderret()$holderret
      
      ##daterange
      daterange = reholderret()$dates
      
      
      ##cum return
      retdataholder  = matrix(nrow = ncol(holderret), ncol = length(input$text1))
      
      for (i in 1:length(input$text1))
      {
        retdataholder[,i] = as.numeric(holderret[match(as.character(input$text1[i]), rownames(holderret) ),] )
        
      }
      
    
      newdata = retdataholder
      
   
      
      newdata = newdata*100
      
      
      #common date.
   if( input$commondate == T)
   {
      newdata = newdata[apply(newdata, 1, Compose(is.finite, all)),]
      
      shortest = min(length(daterange),(length(newdata)/length(input$text1)) )
      
      
      newdaterange = daterange[((length(daterange)+1)-shortest):length(daterange)]
            
   }   
        x <- data.frame(newdata)
   
          
        ## plot the graph with smooth returns
         if (input$smooth == T)
        {
           library(reshape2)
          data<- melt(x)
          ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.2) + xlab("Monthly Returns(%)") +
            ylab("Density") +
            ggtitle("Distribution of Returns for Funds") + scale_fill_discrete(name="Funds",
                                                                               
                                                                               labels=input$text1)+
            geom_vline(data=NULL, aes(xintercept=0),
                       linetype="dashed", size=1, colour="black")+scale_x_continuous(breaks = seq(-100,100, by = 2))
        }
        
        ## plot without smoothness
        else if (input$smooth == F)
          
        {
          data<- melt(x)
          ggplot(data,aes(x=value, fill=variable)) + geom_histogram(alpha=0.2, colour = "black", binwidth = 1, position="dodge") + xlab("Monthly Returns(%)") +
            ylab("Count") +scale_x_continuous(breaks = seq(-100,100, by = 2))+
            ggtitle("Distribution of Returns for Funds") + scale_fill_discrete(name="Funds",
                                                                               
                                                                               labels=input$text1)+
            geom_vline(data=NULL, aes(xintercept=0),
                       linetype="dashed", size=1, colour="black")
       
        }
    
    })
  
    output$commonestdate = renderText({
      ##pick a fund
      validate(
        need(input$text1 != "", "")
      )
      
      #is there a return file
      validate(need(!is.null(reholderret()), ""))
      ##tihs is returns
     holderret = reholderret()$holderret
      
      ##daterange
      daterange =reholderret()$dates 
      
      
      ##cum return
      retdataholder  = matrix(nrow = ncol(holderret), ncol = length(input$text1))
      
      for (i in 1:length(input$text1))
      {
        retdataholder[,i] = as.numeric(holderret[match(as.character(input$text1[i]), rownames(holderret) ),] )
        
      }
      
      ##benchmark
      benchmark = as.numeric(holderret[match(as.character(input$bench1), rownames(holderret) ),] )
      
      newdata = retdataholder
      
      newdata = newdata*100
    
      #common date.
        
        newdata = newdata[apply(newdata, 1, Compose(is.finite, all)),]
    
      shortest = min(length(daterange),(length(newdata)/length(input$text1)) )
    
      
      newdaterange = daterange[((length(daterange)+1)-shortest):length(daterange)]
      
          # print the highest common date

    paste("Earliest Common Date (returns):", as.character(newdaterange[1]))
   
    })
    
    output$distable = renderTable({
      ##pick a fund
      validate(
        need(input$text1 != "", "")
      )
     
      tables()$distable
      
    })
   
   output$cortable = renderTable({
     
     ##pick a fund
     validate(
       need(input$text1 != "", "")
     )
     ##pick a benchmark
     validate(
       need(input$bench1 != "", "Pick benchmarks for correlation.")
     )
    tables()$cortable
   })
   
   output$downloadfundstats = renderUI({
     validate(need(!is.null(input$text1), ""))
     
     downloadButton("downloadstats","Download Fund Statistics")
   })
   
   output$downloadstats <- downloadHandler(
     
     # This function returns a string which tells the client
     # browser what name to use when saving the file.
     filename = function() {
       paste("Fund Statistics", "(", Sys.Date(),")",".csv", sep = "")
     },
     
     # This function should write data to a file given to it by
     # the argument 'file'.
     content = function(filespec) {
       
       # Write to a file specified by the 'file' argument
       write.csv(tables()$fundexcel, filespec)
     }
   )
   
   
   
   #*************************************************tab 2(portfolio making) *********************************************************#
    
    output$mixselectbox1 = renderUI({
      
      validate(need(!is.null(reholderret()), "")
               )
      
      holderret = reholderret()$holderret
      
      selectInput("mixfundsa", label="Port. A Funds" , choices = as.character(rownames(holderret)), multiple= T)
      
    })
    
  
    
    output$totalporta = renderText({
      ##calculate total weighting of port a
      weighporta = unlist(strsplit(input$weighporta,","))
     weighporta = gsub("%", "", weighporta)
      
      atotal = sum(as.numeric(weighporta))
      
      totalpa = "Total weighting of Portfolio A (should equal about 100): "
      paste(totalpa, atotal,"%")
      
    })
    
    
    output$mixselectbox1b = renderUI({
      
      validate(need(!is.null(reholderret()), "")
              )
      
      holderret = reholderret()$holderret
      
      
        selectInput("mixfundsb", label="Port. B Funds" , choices = as.character(rownames(holderret)), multiple= T )
      
      
    })
  
    

    
    output$totalportb = renderText({
      ##calculate total weighting of port b
      
      weighportb = unlist(strsplit(input$weighportb,","))
      
      weighportb = gsub("%", "", weighportb)
      
      btotal = sum(as.numeric(weighportb))
      
      totalpb = "Total weighting of Portfolio B (should equal about 100): "
      paste(totalpb, btotal,"%")
      
    })
   
   output$portdaterange = renderUI({ ##daterange input to adjust dates for portfolios
   
     
     dates=reholderret()$dates
     dates = try(as.Date(dates), silent = T)  #are the dates convertible to date class?
     
     dateRangeInput("portdaterange1", label= "Portfolio Dates", start = dates[1], end = dates[length(dates)], min = dates[1], max = dates[length(dates)], startview = "year",weekstart =1 )
   })
   
   userweightings = reactive({
     
     ##get numeric vectors of weightings from user input
     weighportb = unlist(strsplit(input$weighportb,","))
     
     weighportb = gsub("%", "", weighportb)
     
     weighporta = unlist(strsplit(input$weighporta,","))
     weighporta = gsub("%", "", weighporta)
     
     weighs = new.env()
     weighs$weighporta = weighporta
     weighs$weighportb = weighportb
     weighs
      
   })
   
   portfoliovectors = reactive({  ##this is a reactive where the portfolios, after the user has created them, will be stored for use by the graph output and the table output.
     if(is.null(reholderret()$holderret))
       return(NULL)
     
     ##tihs is returns
     holderret = reholderret()$holderret
     
     ##daterange
     daterange = reholderret()$dates
     daterange = try(as.Date(daterange), silent = T)
     
     if(!is.null(input$bench1))
     {
     benchmark =as.numeric(holderret[match(as.character(input$bench1[1]), rownames(holderret) ),] )
     }
     else
     {
       benchmark = as.numeric(holderret[1,])
     }
     
     riskfree =as.numeric(holderret[match(as.character(input$riskfree1), rownames(holderret) ),] )
     
     ##cum return a
     retdataholder  = matrix(nrow = length(userweightings()$weighporta), ncol = ncol(holderret))
     
     for (i in 1:length(userweightings()$weighporta))
     {
       retdataholder[i,] = as.numeric(holderret[match(as.character(input$mixfundsa[i]), rownames(holderret) ),] )
       
     }
     
     
     newdata = retdataholder*100
     
     #cum return port b
     
     retdataholder2  = matrix(nrow = length(userweightings()$weighportb), ncol = ncol(holderret))
     
     for (i in 1:length(userweightings()$weighportb))
     {
       retdataholder2[i,] = as.numeric(holderret[match(as.character(input$mixfundsb[i]), rownames(holderret) ),] )
       
     }
     
     
     newdata2 = retdataholder2*100
     
     
     
     
     ##make data according to dates inputted 
     
     newdata = newdata[,match(input$portdaterange1[1], daterange):match(input$portdaterange1[2],daterange)]
     
     newdata2 = newdata2[,match(input$portdaterange1[1], daterange):match(input$portdaterange1[2],daterange)]
     
     if(class(newdata) == "matrix") ##checks if we only have 1 fund (same as NCOL used below, just different method)
     
    { newdata = newdata[,apply(!is.na(newdata), 2, all)]}
    else
    {newdata = newdata[!is.na(newdata)]}
    
    if(class(newdata2) == "matrix")
     
   {  newdata2 = newdata2[,apply(!is.na(newdata2), 2, all)]}
   else
   {newdata2 = newdata2[!is.na(newdata2)]}
     
     
     
     #make new data set with weightings from numeric inputs  
     #done by multiplying vector of weightings from inputs with columns of the newdatas
     
     ##port a
     
     if(NROW(newdata)>1 && NCOL(newdata)>1) #this checks if the inputted funds is more than 1; 1 is treated like a vector, more than one like a matrix.
     {
       for(i in 1:length(input$mixfundsa))
       {
         newdata[i,] = ((as.numeric(userweightings()$weighporta[i]))/100)*newdata[i,]
       }
     }
     else
       
     {
       newdata = ((as.numeric(userweightings()$weighporta[1]))/100)*newdata    
     }
     #      
     ##port b  
     
     if(NROW(newdata2)>1 && NCOL(newdata2)>1)
     {
       for(i in 1:length(input$mixfundsb))
       {
         newdata2[i,] = ((as.numeric(userweightings()$weighportb[i]))/100)*newdata2[i,]
       }   
     }    
     
     else
       
     {
       newdata2 = ((as.numeric(userweightings()$weighportb[1]))/100)*newdata2     
     }
     
     ##get the sum of the rows of the data to get final return for each date for the portfolio
     
     v1 = rep(0,(length(newdata)/length(userweightings()$weighporta)))
     
     if(NROW(newdata)>1 && NCOL(newdata)>1)
     { 
       for(i in 1:(length(newdata)/length(userweightings()$weighporta)))
       {
         v1[i] = sum(newdata[,i])
         
       }
     }
     else
     {
       v1 = newdata
     }
     ##now for port b 
     v2 = rep(0,(length(newdata2)/length(userweightings()$weighportb)) )
     
     if(NROW(newdata2)>1 && NCOL(newdata2)>1)
     { 
       for(i in 1:(length(newdata2)/length(userweightings()$weighportb)))
       {
         v2[i] = sum(newdata2[,i])
         
       }
     }
     else
     {
       
       v2 = newdata2
     }
     
     newv1 = v1
     newv2 = v2
     ##get earlies common date for two portfolios
     if(length(v1)> length(v2))
     {
       newv1 = v1[ ((length(v1)-length(v2))+1): length(v1)  ]  
     }
     if (length(v2) >length(v1))
     {
       newv2 = v2 [((length(v2)-length(v1))+1):length(v2)]
     }
     
   ##use old returns matrices to find earliest common of all the funds in the portfolios
 
     ECD =  max(daterange[earliestcommon(retdataholder,lengthdata=length(daterange))$earliest],daterange[earliestcommon(retdataholder2,lengthdata=length(daterange))$earliest])


     
   ##also adjust for benchmark
   corv1 =earliestcommon(newv1,benchmark,length(daterange))$first
   corv2 = earliestcommon(newv2,benchmark,length(daterange))$first
   benchmark = earliestcommon(newv1,benchmark,length(daterange))$second
   
   finholder = matrix(nrow = 10, ncol =2)
   
   if(!is.null(input$bench1))
   {
   thing = paste("Correlation ","(",input$bench1[1],")", sep ="")
   }
   else
   {
     thing = "Correlation (defaulting to first row of data)"
   }
   
   dimnames(finholder) = list(c("Mean","Skew","Median", "75% Quantile", "25% Quantile", "Compound Return (%)", "Kurtosis", thing , "Standard Deviation",paste("Revised Sharpe Ratio:",input$riskfree1)),c("Portfolio A","Portfolio B"))
   
   ##fill first column with stats for port a
   finholder[1,1] = mean(newv1)
   finholder[2,1] = skewness(newv1)
   finholder[3,1] = median(newv1)
   finholder[4,1] = quantile(newv1, c(.75), na.rm =T)
   finholder[5,1] = quantile(newv1, c(.25), na.rm =T)
   finholder[6,1] = Return.annualized(newv1/100, scale=12)*100 ##vector must be divided by 100 to get returns in decimal form, then multiplied by 100 again to get %compound return
   finholder[7,1] = kurtosis(newv1, na.rm=T)
   finholder[8,1] = cor(corv1, benchmark*100)
   finholder[9,1] = sd(newv1,na.rm = T)*sqrt(12) ##annualized
   ##adjust riskfree
   riskfree = earliestcommon(newv1,riskfree, length(daterange))$second
   finholder[10,1] = inforat(newv1,riskfree, use = "pairwise")
   
   ##then port b
   
   finholder[1,2] = mean(newv2)
   finholder[2,2] = skewness(newv2)
   finholder[3,2] = median(newv2)
   finholder[4,2] = quantile(newv2, c(.75), na.rm =T)
   finholder[5,2] = quantile(newv2, c(.25), na.rm =T)
   finholder[6,2] = Return.annualized(newv2/100, scale=12)*100
   finholder[7,2] = kurtosis(newv2, na.rm=T)
   finholder[8,2] = cor(corv2, benchmark*100)
   finholder[9,2] = sd(newv2,na.rm = T)*sqrt(12) ##annualized
   finholder[10,2] = inforat(newv2,riskfree, use = "pairwise")
   ##make a table for excel output with 2 columns
   if(length(newv1)>nrow(finholder))
  { 
     adjfinholder = cbind(rep("", nrow(finholder)),rownames(finholder),finholder)
   extrafinholder = rep("", length(newv1)-nrow(finholder))
   extrafinholder = cbind(extrafinholder,extrafinholder,extrafinholder,extrafinholder)
   
   extrafinholder=rbind(adjfinholder,extrafinholder)
   
   ##make part where portfolio A user inputs are shown
   portfolioA = cbind(input$mixfundsa,paste(userweightings()$weighporta, "%", sep=""))
  extraA = cbind(rep("", length(newv1)-length(input$mixfundsa)),rep("", length(newv1)-length(input$mixfundsa)))
   portfolioA = rbind(portfolioA,extraA) ##extra stuff to match up rows with return stream rows
   
  #user inputs for portfolio B
   portfolioB = cbind(input$mixfundsb,paste(userweightings()$weighportb, "%", sep=""))
  extraB = cbind(rep("", length(newv1)-length(input$mixfundsb)),rep("", length(newv1)-length(input$mixfundsb)))
   portfolioB = rbind(portfolioB,extraB)

   finalexcel = cbind(paste(newv1, "%", sep=""),paste(newv2, "%", sep=""),extrafinholder,rep("",length(newv1)),portfolioA,portfolioB)
   colnames(finalexcel) = c("Portfolio A returns","Portfolio B returns","","","Portfolio A stats","Portfolio B stats","","Input for Portfolio A","","Input for Portfolio B","")
   rownames(finalexcel)=as.character(daterange[earliestcommon(newv1,benchmark,length(daterange))$earliest:length(daterange)])
   }
  else
  {
    finalexcel=paste("Warning: You should have at least",nrow(finholder)+1 ,"points of data in your portfolio!")
  }
     
     portenv = new.env()
     portenv$portA = newv1  ##portfolio A after adjusted for dates and all weightings and funds applied
     portenv$portB = newv2  ##port B ^^^
   portenv$corportA = corv1
   portenv$corportB = corv2
   portenv$ECD = ECD #earliest common date of the data
   portenv$finholder = finholder ##table with all the stats
   portenv$benchmark = benchmark
   portenv$finalexcel = finalexcel
     portenv
     
     
   })
    
    output$portsgraph = renderPlot({
      
      validate( need(!is.null(reholderret()),"No returns file.")
        )
      
      ##check we haven't stuffed up portfolio entries
      validate(
        need(length(userweightings()$weighportb)==length(input$mixfundsb), "Your funds and weightings for B don't match."),
        need(length(userweightings()$weighporta)==length(input$mixfundsa), "Your funds and weightings for A don't match."),
        need(length(input$weighportb)!=0 && length(input$weighporta)!=0 && length(input$mixfundsa)!=0 && length(input$mixfundsb)!=0, "Enter values for the portfolios."  )
      )
      

     
      #plot the graph
      
      x <- data.frame(portfoliovectors()$portA, portfoliovectors()$portB)



      data<- melt(x)
      ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.2) + xlab("Monthly Returns(%)") +
        ylab("Density") +
        ggtitle("Distribution of Portfolios") + scale_fill_discrete(name="Portfolios",
                                                                    
                                                                    labels=c("Portfolio A", "Portfolio B"))+
        geom_vline(data=NULL, aes(xintercept=0),
                   linetype="dashed", size=1, colour="black")+scale_x_continuous(breaks = seq(-100,100, by = 2))
    })
    
    output$porttable = renderTable({
      

      
      ##check we haven't stuffed up portfolio entries
      validate(
        need(length(userweightings()$weighportb)==length(input$mixfundsb), ""),
        need(length(userweightings()$weighporta)==length(input$mixfundsa), ""),
        need(length(input$weighportb)!=0 && length(input$weighporta)!=0 && length(input$mixfundsa)!=0 && length(input$mixfundsb)!=0, ""  )
      )
      


      portfoliovectors()$finholder
    })
  
    output$commondateport = renderText ({
      
     
      
      ##check we haven't stuffed up portfolio entries
      validate(
        need(length(userweightings()$weighportb)==length(input$mixfundsb), ""),
        need(length(userweightings()$weighporta)==length(input$mixfundsa), ""),
        need(length(input$weighportb)!=0 && length(input$weighporta)!=0 && length(input$mixfundsa)!=0 && length(input$mixfundsb)!=0, ""  )
      )
  
      # print the highest common date
   
      paste("Earliest Common Date:", portfoliovectors()$ECD)
      
    })
  observe({ ##update the date range for portfolio depending on earliest common date of funds
    ##only works if all these conditions are satisfied(ie. portfolio entry is sound)
    if(!is.null(input$monthrets) && length(userweightings()$weighportb)==length(input$mixfundsb)
       && length(userweightings()$weighporta)==length(input$mixfundsa) && length(input$weighportb)!=0 && length(input$weighporta)!=0 && length(input$mixfundsa)!=0 && length(input$mixfundsb)!=0
       && !is.null( portfoliovectors() )
       )
      
    {  updateDateRangeInput(session, "portdaterange1",
                          start = as.Date(portfoliovectors()$ECD),
                           min = as.Date(portfoliovectors()$ECD) )}
                          
  })
  
  output$downloadportui = renderUI({
    

    ##check we haven't stuffed up portfolio entries
    validate(
      need(length(userweightings()$weighportb)==length(input$mixfundsb), ""),
      need(length(userweightings()$weighporta)==length(input$mixfundsa), ""),
      need(length(input$weighportb)!=0 && length(input$weighporta)!=0 && length(input$mixfundsa)!=0 && length(input$mixfundsb)!=0, ""  )
    )
    
    downloadButton('downloadport', 'Download Portfolio Stats')
  })
 
 output$downloadport <- downloadHandler(
   
   # This function returns a string which tells the client
   # browser what name to use when saving the file.
   filename = function() {
     paste("Portfolio Statistics", "(", Sys.Date(),")",".csv", sep = "")
   },
   
   # This function should write data to a file given to it by
   # the argument 'file'.
   content = function(filespec) {
     
     # Write to a file specified by the 'file' argument
     write.csv(portfoliovectors()$finalexcel, filespec)
   }
 )
  
  output$testings = renderText ({
    
    #*****************SPACE FOR TESTING.************************
  
    
    #************************************************************************
    
  })
    
  }
  
)