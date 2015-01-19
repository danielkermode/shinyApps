

shinyUI(
  fluidPage(
 
   h1("Matrix/Composite Score Calculator", align = "center"),
    hr(),
    br(),
    fluidRow(
      uiOutput("filein"), 
      textOutput("testes"),
     fluidRow(
      column(width= 4, uiOutput("pickcorbenchmark")),
     column(width= 4,uiOutput("pickinfobenchmark"))
     ),
     br(),
     uiOutput("excludefunds"),
     
    uiOutput("button"),
   
     hr(),
    br(),
fluidRow(
  column(width = 3,  selectInput("pickmatrix", label=NULL, choices = c("Average Matrix Score","Sig. Month", "Revised Sharpe Ratio<0.5","12-Month Rolling Loss","Compound Return<8%","Volatility>15%", ">=4/6 Losing Months", "Benchmark Correlation>0.5","In a Drawdown>10%")) ),
  column(width = 3, selectInput("pickpart", label=NULL, choices = c("12 most recent months","12 Month Return Ranks","36 Month Return Ranks", "60 Month Return Ranks", "Total Return Ranks", "36 Month Revised Sharpe Ratio Ranks", "60 Month Revised Sharpe Ratio Ranks", "Total Revised Sharpe Ratio Ranks", "Weighted Return Ranks", "Weighted Revised Sharpe Ratio Ranks","Total (Normalised) Rank","3 Month Matrix Score Average" )) )
    
                     ),
br(),

fluidRow(
  column( width= 3, uiOutput("downloadspecUI")),
  column( width= 3,uiOutput("downloadpartUI")),
     column(width= 3,uiOutput("downloadmscoreUI") ),
     
     column( width= 3,uiOutput("downloadcompscoreUI"))
            
 ),

    br(),

    uiOutput("printcorbench"),
    uiOutput("printexcluded"),
uiOutput("timetaken"),
    hr(),
br(),

     uiOutput("finaltable")
     
      ),
   
    br(),
    ##style with css
    tags$head(tags$style(HTML(
      
" 
#goButton { /*basic styles*/ width: 250px; height: 70px; color: white; background-color: #d46d45; text-align: center; font-size: 30px; line-height: 50px; /*gradient styles*/ background: -webkit-gradient(linear, 0% 0%, 0% 100%, from(#d46d45), to(#b1432a)); background: -moz-linear-gradient(19% 75% 90deg,#b1432a, #d46d45); /*border styles*/ border-top: solid 2px #e28d79; border-bottom: solid 1px #51281f; border-right: solid 1px #d46d45; border-left: solid 1px #d46d45; -moz-border-radius: 30px; -webkit-border-radius: 30px; border-radius: 30px; }
select#corbenchmark{
background-image:-webkit-gradient(linear, 0 0, 0 100%, from(white), to(red))
}

"
)))
 
    )
)
