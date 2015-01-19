library(shiny)

shinyUI(
  
  
  
  navbarPage( NULL, 
              collapsable = TRUE, responsive = TRUE, inverse = TRUE, windowTitle = "NZAM",
              
            #  theme = "orangeone.css",
              
              
              tabPanel(
                "Comparisons", 
                h2("Comparing Funds", align = "center"),
                hr(),
                
                # selectInput("wstyle", "Style:", c("Orange"="orangeone.css")),
                br(),
                fluidRow(
           column(  width=4,   fileInput("monthrets", strong("CSV file of Returns"),  accept = c(   '.csv'   )) ),
             
                
           column(  width=4, fileInput("incompscores", strong("CSV file of Composite Scores"),  accept = c(  '.csv'      ))),
           column( width=4,  fileInput("inmatscores", strong("CSV file of Matrix Scores"),  accept = c(   '.csv'  )))
             ),
                
                fluidRow( 
                  
                  column(
                    width = 8, textOutput("warnings")
                    
                  )
#                   column(
#                    # width = 4, img(src = "rgoodlogo.png", height = 150, width = 150)
#                   )
                ),
             
                
              
                fluidRow( class = "row1",
                  column(width = 12,uiOutput("selectbox1"))
                ),
                
       
                fluidRow( class = "row1",
               column( width=6, uiOutput("benchmark1") ), column( width=6, uiOutput("riskfreeui") )
                ),
                hr(),
               
                fluidRow(
                  
                  
                  column( width = 8,
                          
                          uiOutput("graphdates1")
                          
                  ),
                  
                  
                  column( width = 4, 
                  checkboxInput("hidelegend", "Hide Legend", value = F),
                  conditionalPanel(
                    condition = "input.hidelegend == false",
                    ##legendmove
                    numericInput("legendmove", "Move Legend Position", 1, min = -20, max = 300) )
                  )
                  
                ),
              
                hr(),
                
                fluidRow(
                  column( width = 12,  
                          plotOutput('graph1') )
                  
                ),
                hr(),
   checkboxInput("returnsgraph", "Show returns graph", value = F),
   conditionalPanel(
     condition = "input.returnsgraph == true",
     ##graph for cum returns
     plotOutput("retgraph")
     
     ),
                br(),
                hr(),
                br(),
                fluidRow(
                  column( width = 4, 
                          uiOutput("tabledate1")
                            
                          
                  ),
                  
                  column( width = 8, uiOutput('table1') 
                  )
                  
                )
                ,
                br(),
                hr(),
                br(),
                fluidRow(
                  
                  checkboxInput("smooth", "Smooth Returns", value = T),
                  checkboxInput("commondate", "Earliest Common Date", value = T),
                  hr(),
                  textOutput("commonestdate"),
                  plotOutput("histgraph1"),
                  br(),
                  br(),
                  uiOutput('distable')
                  
                ),
                br(),
uiOutput('cortable'),
                hr(),
selectInput("tabletype", "Select Table", c("Table 1","Table 2", "Correlation Table")),
uiOutput("downloadfundstats"),
                
               # img(src = "botlinegood.png", height = 125, width = 3000),
                # img(src = "rgoodlogo.png", height = 150, width = 150),
                br(),
                br()
                
              ),
              
              
              ########################second part of app#################################
              
              tabPanel( "Portfolio Tester", 
                        
                        h2("Portfolio Tester", align = "center"),
                        hr(),
                        
                        fluidRow(
                          column( width = 4,
                                  uiOutput("portdaterange")
                          ),
                          
                          column( width = 4,
                                  br(),
                                  br(),
                                  textOutput("commondateport")
                          )
#                           column( width = 4,
#                           #        img(src = "rgoodlogo.png", height = 150, width = 150)
#                                   
#                           )
                          
                          
                        ),
                        br(),
                        h5("Portfolio A"),
                        fluidRow(
                          column(width = 12,uiOutput("mixselectbox1"))
                    
                          
                          
                        ),
                        fluidRow(
                          
                          textInput("weighporta", strong("Enter port. A weightings, seperated by a comma."))
                        ),
                        hr(),
                        fluidRow(
                          textOutput("totalporta", container = span)
                        ),
                        hr(),
                        br(),
                        
                        ##graph showing dists of both portfolios
                        fluidRow(
                          plotOutput("portsgraph")
                          
                        ),
                        
                        h5("Portfolio B"),
                        fluidRow(
                          column(width = 12,uiOutput("mixselectbox1b"))
                          
                          
                          
                        ),
                        fluidRow(
                          
                          textInput("weighportb", strong("Enter port. B weightings, seperated by a comma."))
                          
                        ),
                        hr(),
                        fluidRow(
                          textOutput("totalportb", container = span)
                        ),
                        hr(),
                        br(),
                        fluidRow(
                          uiOutput('porttable')
                        ),
                        hr(),
                        uiOutput("downloadportui"),
                        hr(),
                        br(),
                       # img(src = "botlinegood.png", height = 125, width = 3000),

                        br(),
                        br(),
                        fluidRow( textOutput('testings')
                          
                          )
                        ##end of tabpanel
              ),
            ##style with css
            tags$head(tags$style(HTML(
              "
              .shiny-output-error-redalert{
          color: red;
              }
              
         
            .row1{
           font-weight: bold;

            }
        

              "
            )))
          ##end of css    
              
              
  )
  
)