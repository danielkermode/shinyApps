##
require("shiny")
require("PerformanceAnalytics")
require("gtools")
require("utils")
require("tools")

##this function makes sure data is numeric and then returns TRUE if it is and FALSE if it isnt(ie. no characters like %). it doesnt take into account already existing NAs.
checknonnum <- function(x) {
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  !any(badNum & !is.na(x))
}
#replacement for Return.annualized which makes the code much faster(copied code of their function, but took out parts to make it more specific
#to our data type)
danreturnAn = function(fund)
{
  result = prod(1 + fund)^(12/length(fund)) - 1
  result
}


shinyServer(
  function(input, output, session) {
    
    output$testes = renderText({
      ##test things here
  
    })
    
    output$filein = renderUI ({
      
   validate(need(input$goButton==0 || is.null(input$goButton) , "Rerun the program to pick another file. You may generate again for the current file."))
      
      fileInput("monthrets", strong("CSV file of Returns"), accept = c(
        
        '.csv'
        
      ))
      
    })
    
    ##action button will only appear if a file has been chosen
    output$button = renderUI ({
      ##has a file been picked?
      validate(need(input$monthrets !="", "Select a file to use.")
      )
      ##also we need to weed out potential problems with the data.
      
      validate(need((reholderret()$oldholder!="badfile" || !is.null(reholderret()$holderret)), "Is your file type correct? Values must be in decimal form in a csv file, eg. 0.1 for 10% (At least 60 points are needed for at least 2 return streams)."))
      
                    
      validate(need(all(!is.infinite(reholderret()$cumret)), "Your data has values that are too big! Values must be in decimal form, eg. 0.1 for 10%") )
      
      validate(need((reholderret()$oldholder!="notenough" || !is.null(reholderret()$holderret)), "I don't understand your data! Values must be in decimal form in a csv file, eg. 0.1 for 10% (At least 60 points are needed for at least 2 return streams)."))
      validate(need(checknonnum(reholderret()$holderret), "There are non-numeric characters in your data! Values must be in decimal form, eg. 0.1 for 10%"))

      ##check for dates!! checked yyyy-mm-dd       
      ##this is not needed anymore, pretty much all date formats from excel are correctly changed into just a string.
      #HOWEVER, dates should probably be in yyyy-mm-dd if user plans to use data in other apps (eg. brapp)
      ##also this check sucks, it only checks if there are 5 characters before the first . in the dates, so something
      #like X1994. will get picked up, but something like XJuly will also(incorrectly).
      
#       validate(need(
#         nchar(unlist(strsplit(colnames(reholderret()$holderret)[1],"[.]"))[1])==5
#         ,"Dates are probably not in yyyy-mm-dd format, please check them."))
      
      ##do any of the funds have insufficient data?
      validate(need(nrow(reholderret()$holderret)==nrow(reholderret()$oldholder), "One or more of your return streams has 12 or less data points, or you have 'phantom data'."))
      
      validate(need(length(matrixnames())>1, "You need at least 2 funds with full data for the benchmarks.")
               
               )

      actionButton("goButton", strong("Get Scores!"), 
                   icon = icon("cog", class = "fa-spin")
      )
      
      
    })

##render our select inputs from the matrixnames, here the benchmark for correlation and inforat will be chosen 
output$pickcorbenchmark = renderUI({
  
  ##has a file been picked?
  validate(need(input$monthrets !="", "")
  ) 
  
  validate(need(ncol(reholderret()$holderret)>60, ""))
  ##take dependency on go button
  input$goButton
  
  selectInput("corbenchmark", label = "Select Benchmark for Correlation", choices = matrixnames())
  
  
})

output$pickinfobenchmark = renderUI({
  
  ##has a file been picked?
  validate(need(input$monthrets !="", "")
  )
  validate(need(ncol(reholderret()$holderret)>60, ""))
  ##take dependency on go button
  input$goButton
  
  selectInput("infobenchmark", label = "Select risk free rate", choices = matrixnames(), selected = matrixnames()[length(matrixnames())])
  
})

##does the user wish to exclude any funds from the rankings?
output$excludefunds = renderUI({
  ##has a file been picked?
  validate(need(input$monthrets !="", "")
  ) 
  ##take dependency on go button
  input$goButton
  
  selectInput("exclude", label = strong("Exclude funds from the ranking metrics (they can still be used as benchmarks)"), choices = as.character(rownames(reholderret()$oldholder)), multiple =T)
})


output$downloadspecUI = renderUI({
  ##has a file been picked?
  validate(need(input$monthrets !="", "")
  )
  ##take dependency on go button
  input$goButton
  
  downloadButton('downloadspec', 'Download Specific Matrix')
  
})

output$downloadpartUI = renderUI({
  ##has a file been picked?
  validate(need(input$monthrets !="", "")
  )
  ##take dependency on go button
  input$goButton
  
  downloadButton('downloadpart', 'Download Specific Composite Part')
  
  
})

output$downloadmscoreUI = renderUI({
  ##has a file been picked?
  validate(need(input$monthrets !="", "")
  )
  ##take dependency on go button
  input$goButton
  
  downloadButton('downloadmscore', 'Download Matrix Score Data')
  
})

output$downloadcompscoreUI = renderUI({
  ##has a file been picked?
  validate(need(input$monthrets !="", "")
  )
  ##take dependency on go button
  input$goButton
  
  downloadButton('downloadcompscore', 'Download Composite Score Data')
  
})

reholderret = reactive({  ##this is the reactive which reads user's "returns" data. it does a number of checks to make sure data is ok.
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
    
  holderret = as.matrix(holderret)
  
  rownames(holderret) = holderret[,1]
  
  holderret = (holderret[,-c(1)])
 
 ##test if the rows are dates, if so transpose the matrix
 isrowdate = rownames(holderret)
 isrowdate = gsub("X", "", isrowdate)
 isrowdate = gsub("\\.","-",isrowdate)
 
 isit = try(as.Date(isrowdate), silent = T)

 if(class(isit)!="try-error") ##the rows can be converted to dates
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
  
  ##trying to get dates from excel file (these dates aren't used anywhere at the moment, dates from next reactive are used.)
  dates = colnames(holderret)
  dates = gsub("X", "", dates)
  dates = gsub("\\.","-",dates)

  ##we don't need a date class any longer, so this conversion is NOT NEEDED (a char string is used only for column names of output matrices)
 
  #      if(nchar(unlist(strsplit(colnames(holderret)[1],"[.]"))[1])==5)
  #      {
  #       dates = as.Date(dates)
  #      }
  
  ## we need to get the index of when the data starts for each row
  nalholder = c(1:nrow(newholderret))
  
  for( i in 1:length(nalholder))
  {
    nalholder[i] = length(newholderret[i,is.na(newholderret[i,])])
  }
  
  ##we also want a matrix of cum returns for some parts of the code
  
  holdercumret =matrix(0, nrow = nrow(newholderret), ncol = ncol(newholderret))
  
  for(j in 1:nrow(newholderret))
  {
    for( i in (nalholder[j]+1):ncol(holdercumret))
    {
      
      ##fill the first value with 1000*starting return
      if(i == nalholder[j] +1)
      {
        holdercumret[j,i] = (1+as.numeric(newholderret[j,i]))*1000
      }
      else ##fill with return*previous value
      {
        holdercumret[j,i] = (1+as.numeric(newholderret[j,i]))*holdercumret[j,i-1]
      }
    }
  }
  
  ##make environment for data
  thedata = new.env()
  thedata$oldholder = holderret
  thedata$holderret = newholderret
  thedata$dates = dates
  thedata$cumret = holdercumret
  thedata$nalholder = nalholder
  
  thedata  ##environment is returned
  
  
})

excludeindices = reactive({ ##this excludes any funds the user doesnt want in the rankings, and this holder is used by matrix and composite reactives.
   
  ##take dependency on goButton, first has the button been clicked?
  if(!is.null(input$goButton)){
  if (input$goButton == 0)
    return()
  input$goButton
  
  ##get original data from first reactive
  
  newholderret = reholderret()$holderret
  
  ##also exclude funds the user wants excluded
  excludedindices = match(as.character(isolate(input$exclude)), rownames(newholderret) )
  if(!is.null(excludedindices))
  { 
    if(length(excludedindices !=0))
    {
      newholderret = newholderret[-excludedindices,]
    }
   
  }
  ##trying to get dates from excel file
  dates = colnames(newholderret)
  dates = gsub("X", "", dates)
  dates = gsub("\\.","-",dates)
  
  ##we don't need a date class any longer, so this conversion is NOT NEEDED (a char string is used only for column names of output matrices)
  #      if(nchar(unlist(strsplit(colnames(holderret)[1],"[.]"))[1])==5)
  #      {
  #       dates = as.Date(dates)
  #      }
  
  ## we need to get the index of when the data starts for each row
  nalholder = c(1:nrow(newholderret))
  
  for( i in 1:length(nalholder))
  {
    nalholder[i] = length(newholderret[i,is.na(newholderret[i,])])
  }
  
  ##we also want a matrix of cum returns for some parts of the code
  
  holdercumret =matrix(0, nrow = nrow(newholderret), ncol = ncol(newholderret))
  
  for(j in 1:nrow(newholderret))
  {
    for( i in (nalholder[j]+1):ncol(holdercumret))
    {
      
      ##fill the first value with 1000*starting return
      if(i == nalholder[j] +1)
      {
        holdercumret[j,i] = (1+as.numeric(newholderret[j,i]))*1000
      }
      else ##fill with return*previous value
      {
        holdercumret[j,i] = (1+as.numeric(newholderret[j,i]))*holdercumret[j,i-1]
      }
    }
  }
  
  excluded = isolate(input$exclude)
  
  goodata = new.env()
  goodata$inds = excludedindices
  goodata$dates = dates
  goodata$cumret = holdercumret
  goodata$nalholder = nalholder
  goodata$holderret = newholderret
  goodata$excluded = excluded
  
  goodata
  
  }
  
})
    

benchdata = reactive({ ##this reactive stores benchmark data (without taking into account excluded funds, so that all funds can be used for this)
  ##take dependency on goButton, first has the button been clicked?
  if (input$goButton == 0)
    return()
  input$goButton

  ##get data from first reactive(in this case we use first reactive data, as user may exclude funds they want as benchmark)
  
  holderret = reholderret()$holderret 
  
  ##riskfree
  riskfree = as.numeric(holderret[match(as.character(isolate(input$infobenchmark)), rownames(holderret) ),])
  ##index of riskfree in rows
  indexriskfree = match(as.character(isolate(input$infobenchmark)), rownames(holderret) )
  
  
  ##corbenchmark
  benchmark = as.numeric(holderret[match(as.character(isolate(input$corbenchmark)), rownames(holderret) ),])
  
  benches = new.env()
  benches$riskfree = riskfree
  benches$indexriskfree = indexriskfree
  benches$benchmark = benchmark
  benches$riskname = as.character(isolate(input$infobenchmark))
  benches
  
})
    progressenv = reactive({  ##this is a new reactive environment created for the progress bar (as it must be accessed across 2 different reactives)
      
      ##take dependency on goButton, first has the button been clicked?
      if (input$goButton == 0)
        return()
      
input$goButton   
      holderret = excludeindices()$holderret
      
      progressbar = new.env()  ##this will hold our required variables, to be accessed from progressenv()


      progressbar$total = nrow(holderret)*100
      progressbar$pb <- shiny::Progress$new(session, min = 1, max = nrow(holderret)*100)
      progressbar ##environment is returned
      
    })
    
   ##NOTE: this is only benchmark usable fund names, so don't use as "excluded funds" choices
    matrixnames = reactive({
     
      holderret = reholderret()$holderret
      
      ##we must check if the funds have NAs, if so they cannot be used for benchmarks
      holderret = holderret[rowSums(is.na(holderret))<1, ]
      #return rownames (funds) of csv
      as.character(rownames(holderret))
      
    })


##three ui outputs here for information for user, however these could be combined into one(all are generated as helpText)
output$printcorbench = renderUI({
  
  ##has a file been picked?
  validate(need(input$monthrets !="", ""),
           need(length(as.character(rownames(reholderret()$oldholder)))-length(isolate(input$exclude)) >1 || is.null(input$goButton), "")
  )
  if (is.null(input$goButton))
    return()
  
  if (input$goButton == 0)
    return()
  
  input$goButton
  
 info = isolate(input$infobenchmark)
 cor = isolate(input$corbenchmark)
 helpText( HTML('<strong style = "color:black" >Correlation Benchmark: </strong> '), cor, p(), 
   HTML('<strong style = "color:black" >Infomation Ratio RFR: </strong>' ), info) 
  
})

output$printexcluded = renderUI({
  ##has a file been picked?
  validate(need(input$monthrets !="", ""),
           need(length(as.character(rownames(reholderret()$oldholder)))-length(isolate(input$exclude)) >1 || is.null(input$goButton) || is.null(reholderret()$holderret), "Don't be such a meanie.")
  )
  
  if (is.null(input$goButton))
    return()
  
  if (input$goButton == 0)
    return()
  
  input$goButton
  
  numbers = c(1:length(excludeindices()$excluded))
  
  if(is.null(excludeindices()$excluded))
  {
    helpText(HTML(' <p style ="color:green"> Nothing has been excluded. </p>') )
  }
  else{
  helpText(HTML('<strong style ="color:#FF9912"> Excluded: </strong>'),
  paste("[",numbers,"]",excludeindices()$excluded))
  }
  
})

output$timetaken = renderUI({ ##print time taken (renderUI is better and should probably be also used instead of renderText above, because better customisation of text through helpText and other html crossovers)
  
  ##has a file been picked?
  validate(need(input$monthrets !="", ""),
           need(length(as.character(rownames(reholderret()$oldholder)))-length(isolate(input$exclude)) >1 || is.null(input$goButton), "")
  )
  if (is.null(input$goButton))
    return()
  
  if (input$goButton == 0)
    return()
  input$goButton
  ##time taken for the reactives generating composite and matrix data, taken from matrix and composite reactives
  matrixtime = round(matrixdata()$matrixtime,1)
  compositetime = round(compositedata()$compositetime,1)
  totaltime = round(matrixdata()$matrixtime+compositedata()$compositetime,1)
  
  helpText(HTML('<strong style="color: #6D9BF1">Time taken for matrix scores:</strong>'), HTML(paste('<strong style ="color:#3232CD">',  matrixtime, "</strong>")), HTML('secs'),  p(),
        HTML('<strong style="color: #6D9BF1">Time taken for composite scores:</strong>'), HTML(paste('<strong style ="color:#3232CD">',  compositetime, "</strong>")),HTML('secs'),  p(), 
        HTML('<strong style ="color:#6D9BF1"> Total time:</strong>'), HTML(paste('<strong style ="color:#3232CD">',  totaltime, "</strong>")), HTML('secs')) 

})
   
  ################this is where calculations are done

 matrixdata = reactive({
   
   ##take dependency on goButton, first has the button been clicked?
   if (input$goButton == 0)
     return()
   input$goButton
   
   ##get initial time, we will use this to get time taken for the entire reactive
   timestart = proc.time()[3]
     
 ##get data from SECOND reactive, after funds have been excluded
      
     holderret = excludeindices()$holderret    
      dates = excludeindices()$dates
      nalholder = excludeindices()$nalholder
     holdercumret = excludeindices()$cumret
      
      
      ##this is the holder
      
      matrixholder = matrix(nrow = nrow(holderret), ncol = ncol(holderret))
      
      
      ###################################### FILL MATRIX ###############################################################
      
 ##note: many of the calculations here and in compositedata use 'for' loops. These are very slow in R. There
 ## are a few ways to speed up the code: 2 ideas are replace certain recursive loops with versions of apply,
 ## or maybe rewrite functions in c++ code. can't get Rcpp package working in R at the moment, though.
      
      ##now we need to fill up all the positions of the matrix with either 1 or 0 depending on whether the first condition (1.) is breached
      ##formula: current point - mean(previous data) > 2*std(previous data)
      ##significant month   
      
      ## now we can fill the matrixholder
      
      for( j in 1:nrow(matrixholder))
      {
        
        for( i in (nalholder[j]+2):ncol(matrixholder))
        {
          if(  (as.numeric(holderret[j,i]) - mean(as.numeric(holderret[j,1:i]), na.rm = T)) >= 2*sd(as.numeric(holderret[j,1:i]), na.rm = T) )  
            
          {
            matrixholder[j,i] = 0.5
            
          }
          
          else if (   (as.numeric(holderret[j,i]) - mean(as.numeric(holderret[j,1:i]), na.rm = T)) <= -2*sd(as.numeric(holderret[j,1:i]), na.rm = T) )  
          {
            matrixholder[j,i] = 1
          }
          else
          {
            matrixholder[j,i] = 0
            
          }
          
        }
        ##set 1st block (blocks are of the progress bar)
  
        (progressenv()$pb)$set(value = j*12.5, message="Calculating sig. month...", 
                     detail =paste( round((j*12.5/progressenv()$total)*(100),1),"% Done") )
 
      }
        
      ##we have a matrix filled with scores for sig. months.  now we are going to make 7 more matrices, each with the 
      ## matrix score for the other variables we want to test. we will add these together at the end to get 1 big matrix score matrix.
           
      ##next we will do "information ration of less than 0.5" 
      
      ## citi 3 month treasury bill is the riskfree rate normally used, but this is chosen by user from the file
      
      riskfree = benchdata()$riskfree
      
      ##we make a new formula to find info ratio: this is the average of excess return over the sd of excess return.
      
      inforat = function(fund, riskfree)
      {
        excessret = fund - riskfree
        if(!identical(fund,riskfree)){
          theratio = mean(excessret, na.rm=T) / sd(excessret, na.rm=T)
        }
        else  ##case where fund and riskfree are the same
          {theratio = 0}
        theratio*sqrt(12)  ##annualized
        
      }
           
      matrixholder2 = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      for( j in 1:nrow(matrixholder2))
      {
        if(benchdata()$indexriskfree != j ) ##need to make sure we dont calculate this for riskfree with itself
        {
          for( i in (nalholder[j]+4):ncol(matrixholder2))
          {
            if( inforat(as.numeric(holderret[j,(nalholder[j]+1):i]), riskfree[(nalholder[j]+1):i]) <0.5)
            {
              matrixholder2[j,i] = 1
            }
          }
         
        }
        ##set 2nd block
       
        (progressenv()$pb)$set(value = j*12.5+nrow(holderret)*12.5, message="Calculating info ratio...", 
                               detail =paste( round((j*12.5/progressenv()$total)*(100) +12.5,1),"% Done") )
      
        
      }
      
      ##note: this process has been improved by the removal of PerformanceAnalytics' function and info rat replaced.
        
      ## next we will check if the fund has had a 12-month rolling loss.
      ##code to give 12 month rolling is : Return.cumulative(fund[(k-11):k]) for each data point k
      
      matrixholder3 = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      
      for( j in 1:nrow(matrixholder3))
      {  
        
        for( i in (nalholder[j]+2+11):ncol(matrixholder3))
        {
          if(Return.cumulative(as.numeric(holderret[j,(i-11):i]), geometric = T)
             <0)
          {
            matrixholder3[j,i] = 1
          }
          
          else
          {
            matrixholder3[j,i] = 0
          }
          
        }
      
        ##set 3rd block

        
        (progressenv()$pb)$set(value = j*12.5+nrow(holderret)*25, message="Calculating 12-month loss...", 
                     detail =paste( round((j*12.5/progressenv()$total)*(100) +25,1),"% Done") )
        
       
      }
 
      ##next we will check if the compound return is less than 8% annualized (over all the data)
      
      matrixholder4 = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      
      for( j in 1:nrow(matrixholder4))
      {
        for( i in (nalholder[j]+2):ncol(matrixholder4))
        {
          if(danreturnAn(as.numeric(holderret[j,(nalholder[j]+2):i]))
             <0.08)
          {
            matrixholder4[j,i] = 1
          }
          
          else
          {
            matrixholder4[j,i] = 0
          }
        }
        ##set 4th block

        
        (progressenv()$pb)$set(value = j*12.5+nrow(holderret)*37.5, message="Calculating compound return...", 
                     detail =paste( round((j*12.5/progressenv()$total)*(100) +37.5,1),"% Done") )
        
       
      }
      
      ##next we will check if the volatility is in excess of 15%
      
      
      matrixholder5 = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      
      for( j in 1:nrow(matrixholder5))
      {
        for( i in (nalholder[j]+2):ncol(matrixholder5))
        {
          if((sd(as.numeric(holderret[j,(nalholder[j]+1):i]), na.rm=T))*sqrt(12)
             >0.15)
          {
            matrixholder5[j,i] = 1
          }
          
          else
          {
            matrixholder5[j,i] = 0
          }
        }
        ##set 5th block

        
        (progressenv()$pb)$set(value = j*12.5+nrow(holderret)*50, message="Calculating volatility...", 
                     detail =paste( round((j*12.5/progressenv()$total)*(100) +50,1),"% Done") )
        
        
      }
  
      ##next we check if the fund has had 4 or more losing months in the past 6
      ##we create a "losingmonths" fn which calculates this, which will be applied to EACH POINT of the matrix, and returns a 1 or 0 so does the work for us.
      
      losingmonths = function(fund, point)
      { 
        hitit = 0
         if (length(!is.na(fund[1:point]))>6)  ##are there more than 6 points in the fund up to the current point
         {
        vector6 = c(1:6)
        ##gets the past 6 data points from the current point in the fund
        for(i in 1:6)
        {
          if(fund[point+1-i]>=0)
          {
            vector6[i] = 1
            
          }
          else
          {
            
            vector6[i] = NA
          }
          
        }
        if (length(vector6[!is.na(vector6)])>4)
        {
          ## hit the point with a 1 or 0
          hitit = 0
        }
        if (length(vector6[!is.na(vector6)])<=4)
        {
          hitit = 1
        }
        hitit
        }
        else
           hitit
     }

      ##now we fill up matrix6 using losingmonths
      matrixholder6 = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      for( j in 1:nrow(matrixholder6))
      {
        for( i in (nalholder[j]+7):ncol(matrixholder6))
        {
          matrixholder6[j,i] = losingmonths(as.numeric(holderret[j,]), i )
        }
        ##set 6th block

        
        (progressenv()$pb)$set(value = j*12.5+nrow(holderret)*62.5, message="Calculating losing months...", 
                     detail =paste( round((j*12.5/progressenv()$total)*(100) +62.5,1),"% Done") )

      }
  
      ##next up we must determine if correlation with benchmark(normally MSCI) is greater than 0.5
      
      matrixholder7 = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      ##benchmark taken from benchdata() reactive
      
     benchmark = benchdata()$benchmark
      
      
      for( j in 1:nrow(matrixholder7))
      {
        for( i in (nalholder[j]+12):ncol(matrixholder7))
        {
          if(cor(as.numeric(holderret[j,(nalholder[j]+10):i]), benchmark[(nalholder[j]+10):i], use = "na.or.complete")
             >0.5)
          {
            matrixholder7[j,i] = 1
          }
          
          else
          {
            matrixholder7[j,i] = 0
          }
        }
        ##set 7th block

        
        (progressenv()$pb)$set(value = j*12.5+nrow(holderret)*75, message="Calculating correlation...", 
                     detail =paste( round((j*12.5/progressenv()$total)*(100) +75,1),"% Done") )

      }
        
      
 ##we create a function "checkdds" which returns a 0 or a 1 depending on whether the current month has drawn down at least 10% from the max value (high water mark).
 # this function works for the matrix of asset values(cumret) created at the start (each with a starting value of 1000)
 
 checkdds = function(cumretfund, start, end)
 {
   ##get differences 
   if((cumretfund[end]/max(cumretfund[start:end])-1 )<(-0.1) )
   {1}
   else 
   {0}
   
 }
 
 matrixholder8 = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
 
 
 for(j in 1:nrow(matrixholder8))
 {
   for( i in (nalholder[j]+2):ncol(matrixholder8))
   {
     matrixholder8[j,i] = checkdds(holdercumret[j,],nalholder[j]+1,i)
   }
   ##set 8th block

   
   (progressenv()$pb)$set(value = j*12.5+nrow(holderret)*87.5, message="Calculating drawdowns...", 
                detail =paste( round((j*12.5/progressenv()$total)*(100) +86.5,1),"% Done") )
 
 }               
      
      ##add all matrices together for the big one
      finalmatrixholder = matrixholder + matrixholder2 +matrixholder3 +matrixholder4+matrixholder5+matrixholder6+matrixholder7 +matrixholder8
      
      ## we now need to fill up nas again in the new matrix holder to signal where data actually starts (done at end)
      
      
      for( j in 1:nrow(matrixholder))
      {
        for( i in 1:(nalholder[j]+1))
        {
          matrixholder[j,i] = NA
          matrixholder2[j,i] = NA
          matrixholder3[j,i] = NA
          matrixholder4[j,i] = NA
          matrixholder5[j,i] = NA
          matrixholder6[j,i] = NA
          matrixholder7[j,i] = NA
          matrixholder8[j,i] = NA  
          finalmatrixholder[j,i] = NA
        }
        
      }
      
      
      ##we need to remember that 12 months in, even though there is monthly return data, is simply not relevant for the matrix score. therefore this data will be filled with NAs.
      ##even a bit longer than this (say a 3 year period) will not be super helpful as there is just not enough data, but the score is still included for illustrative purposes.
      for( j in 1:nrow(matrixholder))
      {
        for( i in 1:min((nalholder[j]+13),ncol(matrixholder)))
        {
          
          finalmatrixholder[j,i] = NA
        }
        
      }
        
      ##first put row names and colnames in
      
      rownames(finalmatrixholder) = rownames(holderret)
      colnames(finalmatrixholder) = as.character(dates)
 
 rownames(matrixholder) = rownames(holderret)
 colnames(matrixholder) = as.character(dates)
 
 rownames(matrixholder2) = rownames(holderret)
 colnames(matrixholder2) = as.character(dates)

 rownames(matrixholder3) = rownames(holderret)
 colnames(matrixholder3) = as.character(dates)
 
 rownames(matrixholder4) = rownames(holderret)
 colnames(matrixholder4) = as.character(dates)
 
 rownames(matrixholder5) = rownames(holderret)
 colnames(matrixholder5) = as.character(dates)
 
 rownames(matrixholder6) = rownames(holderret)
 colnames(matrixholder6) = as.character(dates)
 
 rownames(matrixholder7) = rownames(holderret)
 colnames(matrixholder7) = as.character(dates)
 
 rownames(matrixholder8) = rownames(holderret)
 colnames(matrixholder8) = as.character(dates)
 
 #get time for entire reactive by taking difference of timestart and current proc.time() now (3rd element of proc.time is elapsed)
 timetaken = proc.time()[3] - timestart
 
 mscores = new.env() ##hold out matrices we are interested in looking at (parts of the matrix score with the 0s and 1s)
 mscores$m1 = matrixholder
 mscores$m2 = matrixholder2
 mscores$m3 = matrixholder3
 mscores$m4 = matrixholder4    ##the matrices can now be accessed from the environment
 mscores$m5 = matrixholder5
 mscores$m6 = matrixholder6
 mscores$m7 = matrixholder7
 mscores$m8 = matrixholder8
 mscores$fm = finalmatrixholder
 mscores$matrixtime = timetaken

 ######## return environment in this reactive expression ############
      mscores
   
    })

    #***********************************NOW COMPOSITE DATA*******************************************************

    compositedata = reactive({
      
      ##take dependency on goButton, first has the button been clicked?
      if (input$goButton == 0)
        return()
      input$goButton 
        
    
      ##quickly we get in the matrix scores we need from matrix reactive and the reactive with "data after exclusions"
      finalmatrixholder = matrixdata()$fm
      
      ##get initial time, we will use this to get time taken for the entire reactive
      timestart = proc.time()[3]
   
    holderret = excludeindices()$holderret
    
    dates = excludeindices()$dates
    
    colnames(holderret) = dates
    
      ## we need to get the index of when the data starts for each row
     nalholder = excludeindices()$nalholder
      #------------------------------------------------------------------------------------------
    
    ## progress bar again 
    ## progress bar not incremented in this part of the code; it could be, but it would be complicated since there are heaps of loops to set it for
   
    (progressenv()$pb)$set(value = nrow(holderret)*99, message="Calculating composite scores...", 
                 detail ="Nearly there..." )
  
      ##we also want a matrix of the average matrix score for each point. #1
      
      avmatrixscore = finalmatrixholder
      
      for(j in 1:nrow(finalmatrixholder) )
      {
        for( i in (nalholder[j]+12):ncol(avmatrixscore))
        {
          
          avmatrixscore[j,i] = mean(finalmatrixholder[j,(nalholder[j]+12):i], na.rm=T)
        }
  
      }
      
      #------------------------------------------------------------------------------------------
      ##this is the holder for 12 month rolling returns #2
      
      matrix12returns = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      for( j in 1:nrow(matrix12returns))
        
      {
        for( i in (nalholder[j]+2+11):ncol(matrix12returns))
        {
          matrix12returns[j,i] =   Return.cumulative(as.numeric(holderret[j,(i-11):i]), geometric = T)
          
        }
      }
        
      ## we now need to fill up nas again in the new matrix holder to signal where data actually starts (done at end)
       
      for( j in 1:nrow(holderret))
      {
        for( i in 1:(nalholder[j]+12))   #note that NAs are being filled 12 places from where the month rets start, this is because 12 month return can only be calculated with this data anway
        {
          matrix12returns[j,i] = NA
          
        }
        
      }
      ##make a matrix to hold ranks #3
      
      matrix12rranks = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      rownames(matrix12rranks) = rownames(holderret)
      
      for(i in 1:ncol(matrix12returns))
      {
        matrix12rranks[,i] =  rank(matrix12returns[,i]*-1, na.last = "keep", ties.method ="max")
        

      }
      
      #------------------------------------------------------------------------------------------
      ##next ranks we need are 36 month rolling ret ranks. pretty much same code, just need data for 36 month returns. #4
      
      matrix36returns = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      for( j in 1:nrow(matrix36returns))
        
      {
        if(ncol(holderret) - nalholder[j] >36)  ##if the fund has a long enough track record for 36 months
        {
          for( i in (nalholder[j]+2+35):ncol(matrix36returns))
          {
            matrix36returns[j,i] =   Return.cumulative(as.numeric(holderret[j,(i-35):i]), geometric = T)
            
          }
        }

      }
    
      ## we now need to fill up nas again in the new matrix holder to signal where data actually starts (done at end)
    
      for( j in 1:nrow(holderret))
      {
        
        if(ncol(holderret) - nalholder[j] >36)  ##if the fund has a long enough track record for 36 months
        {
          for( i in 1:(nalholder[j]+36))   #note that NAs are being filled 36 places from where the month rets start, this is because 36 month return can only be calculated with this data anway
          {
            matrix36returns[j,i] = NA
            
          }
        }
        else
        {
          matrix36returns[j,] = NA
        }
        
      }
      ##make a matrix to hold ranks #5
      
      matrix36rranks = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      rownames(matrix36rranks) = rownames(holderret)
      
      for(i in 1:ncol(matrix36returns))
      {
        matrix36rranks[,i] =  rank(matrix36returns[,i]*-1, na.last = "keep", ties.method ="max")
        

      }
      
      #------------------------------------------------------------------------------------------
      ##60 month (very similar code again) #6
      
      matrix60returns = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      for( j in 1:nrow(matrix60returns))
        
      {
        if(ncol(holderret) - nalholder[j] >60)  ##if the fund has a long enough track record for 60 months
        {
          for( i in (nalholder[j]+2+59):ncol(matrix60returns))
          {
            matrix60returns[j,i] =   Return.cumulative(as.numeric(holderret[j,(i-59):i]), geometric = T)
            
          }
        }

      }

      ## we now need to fill up nas again in the new matrix holder to signal where data actually starts (done at end)
   
      for( j in 1:nrow(holderret))
      {
        
        if(ncol(holderret) - nalholder[j] >60)  ##if the fund has a long enough track record for 60 months
        {
          for( i in 1:(nalholder[j]+60))   #note that NAs are being filled 60 places from where the month rets start, this is because 36 month return can only be calculated with this data anway
          {
            matrix60returns[j,i] = NA
            
          }
        }
        else    ##the fund doesnt have a long enough track record, 60 months is NA
        {
          matrix60returns[j,] = NA
        }
        
      }
      ##make a matrix to hold ranks #7
      
      matrix60rranks = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      rownames(matrix60rranks) = rownames(holderret)
      
      for(i in 1:ncol(matrix60returns))
      {
        matrix60rranks[,i] =  rank(matrix60returns[,i]*-1, na.last = "keep", ties.method ="max")
   
      }
      
      #------------------------------------------------------------------------------------------
      #total returns (you know the drill) #8
   
      matrixtreturns = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      for( j in 1:nrow(matrixtreturns))
        
      {
        for( i in (nalholder[j]+2):ncol(matrixtreturns))
        {
          matrixtreturns[j,i] =   Return.cumulative(as.numeric(holderret[j,(nalholder[j]+1):i]), geometric = T)
          
        }
        

      }
     
      ## we now need to fill up nas again in the new matrix holder to signal where data actually starts (done at end)
   
      for( j in 1:nrow(holderret))
      {
        for( i in 1:(nalholder[j]+2))   #NAs only filled up where we dont have data
        {
          matrixtreturns[j,i] = NA
          
        }
        
      }
      ##make a matrix to hold ranks #9
      
      matrixtrranks = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      rownames(matrixtrranks) = rownames(holderret)
      
      for(i in 1:ncol(matrixtreturns))
      {
        matrixtrranks[,i] =  rank(matrixtreturns[,i]*-1, na.last = "keep", ties.method ="max")
        

      }
      
      #------------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------------
      
      ##now we must find rankings matrices for the info ratio
      
      ##we make a new formula to find info ratio: this is the average of excess return over the sd of excess return.
      
    inforat = function(fund, riskfree)
    {
      excessret = fund - riskfree
      if(!identical(fund,riskfree)){
        theratio = mean(excessret, na.rm=T) / sd(excessret, na.rm=T)
      }
      else  ##case where fund and riskfree are the same
      {theratio = 0}
      theratio*sqrt(12)  ##annualized
      
    }
      
      ##this is the riskfree rate (can be adjusted)
      riskfree = benchdata()$riskfree
    
      #total info ratio ranks #10
    
      matrixtinfo = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      for( j in 1:nrow(matrixtinfo))
        
      {
        for( i in (nalholder[j]+2):ncol(matrixtinfo))
        {
          matrixtinfo[j,i] =   inforat(as.numeric(holderret[j,(nalholder[j]+1):i]), riskfree[(nalholder[j]+1):i])
          
        }

      }
    
      ## we now need to fill up nas again in the new matrix holder to signal where data actually starts (done at end)
      
      
      for( j in 1:nrow(holderret))
      {
        for( i in 1:(nalholder[j]+2))   #NAs only filled up where we dont have data
        {
          matrixtinfo[j,i] = NA
          
        }
        
      }
      ##make a matrix to hold ranks #11
      
      matrixtiranks = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      rownames(matrixtiranks) = rownames(holderret)
      
      for(i in 1:ncol(matrixtinfo))
      {
        matrixtiranks[,i] =  rank(matrixtinfo[,i]*-1, na.last = "keep", ties.method ="max")
        

        
      }
      
      #------------------------------------------------------------------------------------------
      ##now we want 36 months for info ratio #12
      
      matrix36info = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      for( j in 1:nrow(matrix36info))
        
      {
        if(ncol(holderret) - nalholder[j] >36)  ##if the fund has a long enough track record for 36 months
        {
          for( i in (nalholder[j]+37):ncol(matrix36info))
          {
            matrix36info[j,i] =   inforat(as.numeric(holderret[j,i-35:i]), riskfree[i-35:i])
            
          }
        }

        
      }
  
      ## we now need to fill up nas again in the new matrix holder to signal where data actually starts (done at end)
      
      
      for( j in 1:nrow(holderret))
      {
        
        if(ncol(holderret) - nalholder[j] >36)  ##if the fund has a long enough track record for 36 months
        {
          for( i in 1:(nalholder[j]+36))   #NAs only filled up where we dont have data (note that it is 36 months from actual start)
          {
            matrix36info[j,i] = NA
            
          }
        }
        
        else    ##the fund doesnt have a long enough track record, 36 months is NA
        {
          matrix36info[j,] = NA
        }
        
      }
      
      ##we need to fill up benchmark with NAs (important, or this will screw up rankings as benchmark undeservedly gets a rank)
      ##not needed any more because user can exclude what they want from rankings
#     if(length(match(benchdata()$riskname, rownames(holderret)))==0)
#       {matrix36info[benchdata()$indexriskfree,] = NA}

      ##make a matrix to hold ranks #13
      
      matrix36iranks = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      rownames(matrix36iranks) = rownames(holderret)
      
      for(i in 1:ncol(matrix36info))
      {
        matrix36iranks[,i] =  rank(matrix36info[,i]*-1, na.last = "keep", ties.method ="max")
        

      }
      
      #------------------------------------------------------------------------------------------
      ##now we want 60 months for info ratio #14
      
      matrix60info = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      
      for( j in 1:nrow(matrix60info))
        
      {
        if(ncol(holderret) - nalholder[j] >60)  ##if the fund has a long enough track record for 36 months
        {
          for( i in (nalholder[j]+61):ncol(matrix60info))
          {
            matrix60info[j,i] =   inforat(as.numeric(holderret[j,i-59:i]), riskfree[i-59:i])
            
          }
        }
  
      }
      
      ## we now need to fill up nas again in the new matrix holder to signal where data actually starts (done at end)
      
      
      for( j in 1:nrow(holderret))
      {
        
        if(ncol(holderret) - nalholder[j] >60)  ##if the fund has a long enough track record for 60 months
        {
          for( i in 1:(nalholder[j]+60))   #NAs only filled up where we dont have data (note that it is 60 months from actual start)
          {
            matrix60info[j,i] = NA
            
          }
        }
        
        else    ##the fund doesnt have a long enough track record, 36 months is NA
        {
          matrix60info[j,] = NA
        }
        
      }
      
      ##we need to fill up benchmark with NAs (important, or this will screw up rankings as benchmark undeservedly gets a rank)
    ##not needed because user can simply exclude whatever they want from rankings
#     if(length(match(benchdata()$riskname, rownames(holderret)))==0)
#       {matrix60info[benchdata()$indexriskfree,] = NA}
 
      ##make a matrix to hold ranks #15
      
      matrix60iranks = matrix(0,nrow = nrow(holderret), ncol = ncol(holderret))
      rownames(matrix60iranks) = rownames(holderret)
      
      for(i in 1:ncol(matrix60info))
      {
        matrix60iranks[,i] =  rank(matrix60info[,i]*-1, na.last = "keep", ties.method ="max")
        

        
      }
      #------------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------------
      ##now we will weight the matrices. this is done by first adding all the matrices together to get 1
      ## big "returns" matrix.
      
      ##must be done to stop NAs appearing in areas there should be just a total return rank(for example)
      matrix12rranks[is.na(matrix12rranks)] = 0
      matrix36rranks[is.na(matrix36rranks)] = 0
       matrix60rranks[is.na(matrix60rranks)] = 0
       matrixtrranks[is.na(matrixtrranks)] = 0
      
      bigrranks = matrix12rranks+matrix36rranks+matrix60rranks+matrixtrranks
      
      ## next we weight each point depending on how long it has been in the track record of the fund.
      ##if it has been less than 12 months, only total return data is used.
      ##if it has been <12 months, only 12month data is used(and the total, which is a given), so 1/2 the matrices applies as 2 matrices are being used.
      ## if it has been <36 months, 1/3 applies as 3 matrices are being used.
      ## if it has been <60 months, 1/4 applies as 4 different matrices are being used for rankings.
      
      for(j in 1:nrow(holderret))  #16
      {
        for(i in nalholder[j]:12)  ## 1st 12 months
        {
          bigrranks[j,i] = bigrranks[j,i]
        }
        if(ncol(holderret) - nalholder[j] >12)  ##if the fund has a long enough track record 
        {
          for(i in (nalholder[j]+13):min((nalholder[j]+36),ncol(holderret) ) )  ## 12 to 36 months or whenever the fund ends (ie. it could only have say a 30 month track record.)
          {
            bigrranks[j,i] = bigrranks[j,i]*1/2
          }
        }
        if(ncol(holderret) - nalholder[j] >36)  ##if the fund has a long enough track record 
        {
          for(i in (nalholder[j]+37):min((nalholder[j]+60),ncol(holderret) ) ) ## 36 to 60 months
          {
            bigrranks[j,i] = bigrranks[j,i]*1/3
          }
        }
        if(ncol(holderret) - nalholder[j] >60)  ##if the fund has a long enough track record 
        {
          for(i in (nalholder[j]+61): ncol(holderret) )  ##rest of data
          {
            bigrranks[j,i] = bigrranks[j,i]*1/4
          }
        }

        
      }
      #------------------------------------------------------------------------------------------
      ## we now have weighted ranks for returns. we can do a similar process to get sharpe(info) rankings.
      
      
       matrix36iranks[is.na(matrix36iranks)] = 0
       matrix60iranks[is.na(matrix60iranks)] = 0
      matrixtiranks[is.na(matrixtiranks)] = 0
       
      bigiranks = matrix36iranks+matrix60iranks+matrixtiranks
      
      for(j in 1:nrow(holderret))  #17
      {
        for(i in nalholder[j]:36)  ## 1st 36 months
        {
          bigiranks[j,i] = bigiranks[j,i]
        }
        
        if(ncol(holderret) - nalholder[j] >36)  ##if the fund has a long enough track record 
        {
          for(i in (nalholder[j]+37):min((nalholder[j]+60),ncol(holderret) ) ) ## 36 to 60 months
          {
            bigiranks[j,i] = bigiranks[j,i]*1/2
          }
        }
        if(ncol(holderret) - nalholder[j] >60)  ##if the fund has a long enough track record 
        {
          for(i in (nalholder[j]+61): ncol(holderret) )  ##rest of data
          {
            bigiranks[j,i] = bigiranks[j,i]*1/3
          }
        }

      }
      
      ##we need to fill these matrices with NAs instead of 0s.
      for( j in 1:nrow(bigrranks))
      {
        for( i in 1:(nalholder[j]+2))
        {
          
          bigrranks[j,i] = NA
          bigiranks[j,i] = NA
        }
        
      }
      
      
      ## we now need to normalize the data
      ## I'm going to use a formula I copied
      
      #           (b-a)(x - min)
      # f(x) =    --------------   + a
      #            max - min
      #
      #Where [a,b] is the range you want,
      #and [min, max] is your current range of the vector.
      
      ##trust me, this works. test it if you like. Unfortunately I don't know how.
      
      linscale = function(vector, oldmin, oldmax, a, b)
      {
        #a is the min value you want and b is the max value you want
        
        newvector = vector
        
        for( i  in 1: length(vector))
        {
          newvector[i] = ( ((b-a)*(vector[i] - oldmin))/(oldmax - oldmin) ) +a
          
        }
        
        newvector
      }
      
      
      normalizedrank = bigiranks + bigrranks
      
      ##what is the min value our rank (this is return rank added to sharpe rank) can take?
      oldmin = 2
      ##what is the max value of our rank? it is number of funds times 2.(will differ for each column, NAs will be excluded)
      
      
      for(i in 1:ncol(normalizedrank))  ##we scale all the columns of the ranks depending on what range we want. #18
        
      {
        ## the new range we want for rank is between 2 and 10.
        normalizedrank[,i] = linscale(normalizedrank[,i], oldmin, length(normalizedrank[!is.na(normalizedrank[,i]),i])*2, 2, 10 )
        
       
      }
      
      ##for the grand finale, we must use 2 matrices we have already created(in the last reactive expression);
      ## finalmatrixholder, which is the matrix scores,
      ##and avmatrixscore,which is the average matrix scores.
      
      ##we create a new holder, compscores, which will hold our composite scores.
      
      compscores = normalizedrank
      
      ## each point in this compscores matrix will use a formula that depends on the points of the other 3 matrices:
      ##compscore = 100*log10((normalizedrank)^((1+matrix+avmatrix)/17))

##current matrix score is taken from last 3 months average to smooth out matrix scores  #19

current3monthmatrix = finalmatrixholder

for(j in 1:nrow(finalmatrixholder))
{
  for(i in (nalholder[j]+1):ncol(finalmatrixholder))
  {
    current3monthmatrix[j,i] = mean(finalmatrixholder[j,(nalholder[j]+1):i], na.rm=T)
  }

}

  ##final composite scores
      
      for( j in 1:nrow(compscores))   #20
      {
        for( i in (nalholder[j]+2):ncol(compscores))
        {
          
          compscores[j,i] = 100*log10((normalizedrank[j,i])^((1+current3monthmatrix[j,i]+avmatrixscore[j,i])/17))
        }
        

        
      }
      
      rownames(compscores) = rownames(holderret)
      colnames(compscores) = as.character(dates)

##close progress
(progressenv()$pb)$close()


##this is a "report" of composite scores of the 12 most recent months of all the funds.
reportcomp = compscores[,(ncol(compscores)-11):(ncol(compscores))]

#get time for entire reactive by taking difference of timestart and current proc.time() now (3rd element of proc.time is elapsed)
timetaken = proc.time()[3] - timestart

##all the different parts of the composite scores function that may be wanted. names are hopefully self-explanatory.
      
      compparts = new.env()
     compparts$avm = avmatrixscore
    compparts$r12= matrix12rranks
  compparts$r36= matrix36rranks
  compparts$r60= matrix60rranks
  compparts$rt= matrixtrranks
compparts$i36= matrix36iranks
compparts$i60= matrix60iranks
compparts$it= matrixtiranks
compparts$wrr= bigrranks
compparts$wir= bigiranks
compparts$tNr= normalizedrank
compparts$compscores = compscores
compparts$reportcomp = reportcomp
compparts$threemmatrix = current3monthmatrix 
compparts$compositetime = timetaken
      compparts
    
    })

      
    ##output this table of composite scores on the page just to give a quick indication of results to user.
    output$finaltable = renderTable({
     
    ##check for infinite values in original data(as in go button)
      validate(need(all(!is.infinite(reholderret()$cumret)), "") )
      ##has a file been picked?
      validate(need(input$monthrets !="", ""),
    need(length(as.character(rownames(reholderret()$oldholder)))-length(isolate(input$exclude)) >1 || is.null(input$goButton) || is.null(reholderret()$holderret), "You can't exclude that many funds!")
      )
            
      if (is.null(input$goButton))
        return(NULL)
      
      ##take dependency on goButton, first has the button been clicked?
      if (input$goButton == 0)
        return()
      input$goButton 
       
      
     compositedata()$compscores
        
    })
##after results have been generated, the download buttons will be shown and these downloadhandlers determine what is in them.
    
    output$downloadmscore <- downloadHandler(
  
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        paste("Matrix Scores (", reholderret()$dates[length(reholderret()$dates)],")",".csv", sep = "")
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
            
        # Write to a file specified by the 'file' argument
        write.csv(matrixdata()$fm, file
                    )
      }
    )
    
    output$downloadcompscore <- downloadHandler(
          
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        paste("Composite Scores (", reholderret()$dates[length(reholderret()$dates)],")",".csv", sep = "")
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(filetwo) {
        
        # Write to a file specified by the 'file' argument
        write.csv(compositedata()$compscores, filetwo)
      }
    )
##picking specific matrices to download (from generator reactives) in a reactive expression based on the select input.
pickspec = reactive({
    
    switch(input$pickmatrix,"Sig. Month"=matrixdata()$m1,"Information Ratio<0.5"=matrixdata()$m2,"12-Month Rolling Loss"=matrixdata()$m3, 
           "Compound Return<8%"=matrixdata()$m4,"Volatility>15%"=matrixdata()$m5,">=4/6 Losing Months"=matrixdata()$m6,"Benchmark Correlation>0.5"=matrixdata()$m7,"In a Drawdown>10%"=matrixdata()$m8,"Average Matrix Score"=compositedata()$avm
           )
})
##same for composite score parts
pickpart = reactive({
  
  switch(input$pickpart, "12 most recent months" =compositedata()$reportcomp ,"12 Month Return Ranks"=compositedata()$r12,"36 Month Return Ranks"=compositedata()$r36, "60 Month Return Ranks"=compositedata()$r60, "Total Return Ranks"=compositedata()$rt, "36 Month Info Ratio Ranks"=compositedata()$i36, "60 Month Info Ratio Ranks"=compositedata()$i60, "Total Info Ratio Ranks"=compositedata()$it, "Weighted Return Ranks"=compositedata()$wrr, "Weighted Info Ratio Ranks"=compositedata()$wir,"Total (Normalised) Rank"=compositedata()$tNr
         ,"3 Month Matrix Score Average" = compositedata()$threemmatrix) 
            
})

##download the specific matrices
output$downloadspec <- downloadHandler(
  
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste(input$pickmatrix, "(", reholderret()$dates[length(reholderret()$dates)],")",".csv", sep = "")
  },
  
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(filespec) {
    
    # Write to a file specified by the 'file' argument
    write.csv(pickspec(), filespec)
  }
)

output$downloadpart <- downloadHandler(
  
  
  
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste(input$pickpart, "(", reholderret()$dates[length(reholderret()$dates)],")",".csv", sep = "")
  },
  
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(filespec) {
    
    # Write to a file specified by the 'file' argument
    write.csv(pickpart(), filespec)
  }
)


  })
