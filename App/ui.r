shinyUI(
  tabsetPanel(
  
tabPanel('Historic Combinations',
    titlePanel(h1("Super Bowl Squares")),              
    pageWithSidebar(
    titlePanel(h2("Historic Combinations")),
                    
    sidebarPanel(
    numericInput("hor",label="Horizontal Team Last Digit",value=0,min=0, max=9, step=1),
    numericInput("ver",label="Vertical Team Last Digit",value = 0,min=0, max=9, step=1),
    submitButton(text="Submit"),
    
    h5(tags$b("Total Occurances:\n")),
    tableOutput("total"),
    h5(tags$b("\n\nOccurances Per Game:\n")),
    tableOutput("pergame")
                ),
                    
    mainPanel(
      h5(tags$b("\n\nOccurances Per Game:\n")),
      plotOutput("hist"),
      
      tags$hr(),
      h5(tags$b("\n\nProbabilities Combinations Will Occur At Least Once:\n")),
      plotOutput("table.hist")
              )
              )
         
),
tabPanel('Next Square Probability',
         titlePanel(h1("Super Bowl Squares")),                       
         pageWithSidebar(
           titlePanel(h2("Next Square Probability")),
           
           sidebarPanel(
             selectInput("tm1", label = "Horizontal Team", choices=list('Bears','Bengals','Bills','Broncos','Browns','Buccaneers','Cardinals','Chargers','Chiefs','Colts','Cowboys','Dolphins','Eagles','Falcons','Giants','Jaguars','Jets','Lions','Packers','Panthers','Patriots','Raiders','Rams','Ravens','Redskins','Saints','Seahawks','Steelers','Titans','Vikings'), selected='Cowboys'),
             numericInput("num",label="Horizontal Team Score",value=0,min=0, max=100, step=1),
             selectInput("tm2", label = "Vertical Team", choices=list('Bears','Bengals','Bills','Broncos','Browns','Buccaneers','Cardinals','Chargers','Chiefs','Colts','Cowboys','Dolphins','Eagles','Falcons','Giants','Jaguars','Jets','Lions','Packers','Panthers','Patriots','Raiders','Rams','Ravens','Redskins','Saints','Seahawks','Steelers','Titans','Vikings'), selected='Patriots'),
             numericInput("num2",label="Vertical Team Score",value = 0,min=0, max=100, step=1),
             numericInput("qtr",label="Quarter",value = 1,min=1, max=5, step=1),
             selectInput("ep", label = "Can Next Score Be An Extra Point?", choices=list("No"=0, "Yes: Horizontal Team Scored Touchdown" = 1, "Yes: Vertical Team Scored Touchdown" = 2)),
             submitButton(text="Submit")
           ),
           
           mainPanel(
             #h5("Probabilty of Final Score"),
             #tableOutput("table3"),    
             h5(tags$b("Next Winning Square Probability:\n")),
             plotOutput("table")
           )
         )
)
)
)