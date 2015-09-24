library(BH)

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
),
tabPanel('App Documentation',
         titlePanel(h1("Super Bowl Squares")),                       
         pageWithSidebar(
         titlePanel(h2("App Documentation")),
         sidebarPanel(
           strong("Coursera Data Science Specialization"),
           br(),
           br(),
           p("Created By: crhenderson"),
           p("Created On: 2015-09-23"),
           br(),
           a(href="https://github.com/crhenderson/Super-Bowl-Squares-App","Github Repository")
         ),
         mainPanel(
           tabsetPanel(
             tabPanel("What is 'Super Bowl Squares'?",
                      p(
                        em("Super Bowl Squares"),
                        " is a betting game related to the NFL Super Bowl.  Each square in the 10x10 grid represents a combination of the last digit of each teams score.  Before the game starts, players buy squars and then the numbers 0:9 are randomly placed accross the top and side of the grid, ie, players blindly select squares."),
                      plotOutput("example"),
                      p("For example, suppose the Dallas Cowboys and New England Patriots are playing in the Super Bowl and that the score is Cowboys 24 - Patriots 17.  Then the last digits of the scores would be 4 & 7 (as shown by the red dot above).  With the group that I play with each year, every time a team scores, who ever owns the new combination square wins a set amount of money.  Every score change pays.")
                      ),
             tabPanel("Historic Combinations",
                      strong("About This Page:"),
                      p(
                        "This tab gives historic distributions and empirical probabilities for each last digit combination based on every regular season NFL football game from 2002-2014.  The data comes from:",
                        a(href="http://www.pro-football-reference.com", "Pro Football Reference")
                      ),
                      br(),
                      p(
                        "On the sidebar, input the last digit for each team and hit ",
                        em("Submit"),
                        ".  You will see four displays.  On the sidebar, you will see the number of times between 2002-2014 that the inputed score combination occurred.  You will also see a table showing frequencies for the number of games in which last digit combination occurred N times."
                      ),
                      p("In the main panel, you see a bar plot showing the number of games each number of occurrences happened.  You will also the empirical probabilities each last digit combination will occur at least once in a game.  The inputed combination will be highlighted with a red dot.")
                      ),
             tabPanel("Next Score Probability",
                      strong("About This Page:"),
                      p(
                        "This tab gives the predicted probabilities of what the next 'winning' square will be based on provided inputs.  Select the horizonal and vertical team from drop downs and input the current score in a game.  Also select the quarter and if the next score can be an extra point or not.  Once all inputs are entered, hit ",
                        em("Submit"),
                        ".  Displayed you will see a red dot at the current last digit score combination based on the inputed scores.  You will also see the probabilities of the next likely squares."
                        ),
                      br(),
                      br(),
                      strong("How Probabilities Are Generated:"),
                      p(
                        "Probabilities are generated using multinomial regression.  The model predicts the probabilities that the next score will be a certain type with model inputs of quarter, score difference (Horizonal - Vertical), and total score.  If the next score cannot be an extra point the model predicts the probabilities that the next score will be either a Team A touchdown, Team A field goal, Team A safety, Team B touchdown, Team B field goal or Team B safety.  If an extra point can occur, the probability that the touchdown scoring team can get an extra point is also a possible output of the model as well as the possibility of a two point conversion."
                        ),
                      p("Next Score Type ~ Quarter + Score Difference + Total.Score")
             )
           )
        
           
         )
           
)
)
)
)