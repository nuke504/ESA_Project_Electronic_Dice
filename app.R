# Electronic Dice R Shiny App
library(shiny)
library(ggplot2)
library(stats)
library(bnlearn)
library(expm)

gen.pmf <- function(){
  # Create a list of PMFs
  pmf <- list()
  
  # Generate Standard Dice PMF
  pmf$standard <- data.frame(X=1:6,Y=rep(1/6,6))
  
  # Generate Normal-Binom Approx PMF
  sd1 <- (6-3.5)/3-0.5+1.25*runif(1)
  pmf$binomial <- data.frame(X=1:6,Y=(pnorm(seq(1.5,6.5,by = 1), mean = 3.5, sd = sd1)-pnorm(seq(0.5,5.5,by = 1), mean = 3.5, sd = sd1))/(pnorm(6.5, mean = 3.5, sd = sd1)-pnorm(0.5, mean = 3.5, sd = sd1)))
  
  # Generate Bimodal PMF
  sd2 <- (6-3.5)/3-0.5+1.25*runif(1)
  pmf$bimodal <- data.frame(X=c(3,2,1,6,5,4),Y=(pnorm(seq(1.5,6.5,by = 1), mean = 3.5, sd = sd2)-pnorm(seq(0.5,5.5,by = 1), mean = 3.5, sd = sd2))/(pnorm(6.5, mean = 3.5, sd = sd2)-pnorm(0.5, mean = 3.5, sd = sd2)))
  
  # Generate martingale dice PMF
  ml <- sample(5:9,1)
  mh <- sample(15:20,1)
  exp.r <- 0.032+0.1*runif(1)
  n.m <- sum(pexp(seq(ml-1+0.5,ml+1+0.5,by=1), rate = exp.r)-pexp(seq(ml-1-0.5,ml+1-0.5,by=1), rate = exp.r),pexp(seq(mh-1+0.5,mh+1+0.5,by=1), rate = exp.r)-pexp(seq(mh-1-0.5,mh+1-0.5,by=1), rate = exp.r))
  pmf$martingale <- data.frame(X=c(ml-1,ml,ml+1,mh-1,mh,mh+1)-10, Y=c(pexp(seq(ml-1+0.5,ml+1+0.5,by=1), rate = exp.r)-pexp(seq(ml-1-0.5,ml+1-0.5,by=1), rate = exp.r),pexp(seq(mh-1+0.5,mh+1+0.5,by=1), rate = exp.r)-pexp(seq(mh-1-0.5,mh+1-0.5,by=1), rate = exp.r))/n.m)
  return(pmf)
}

gen.values <- function(){
  df <- data.frame(A=sample(1:6,1),
                   B=sample(0:1,1),
                   C=0.6+0.05*runif(1),
                   D=0.4+0.05*runif(1),
                   E=0.7+0.05*runif(1),
                   'F'=sample(1:4,1),
                   G=0.3+0.05*runif(1))
  df$K <- df$E/(df$E+df$C/2+3/4*df$D)
  return(df)
}

# Values for challenges
padlockSum <- sample(3:6,1)
c2.x <- runif(1)
c2.y <- runif(1)
c2.z <- runif(1)

c3.d <- sample(0:1,1)
c3.p1 <- ifelse(c3.d == 1,0.1+0.5*runif(1),0.4+0.5*runif(1))
c3.p2 <- ifelse(c3.d == 0,0.1+0.5*runif(1),0.4+0.5*runif(1))

gen.c3.samplemean <- function(){
  df <- data.frame(Equipment=c('Guard Dog','Scanner'),Sample.Mean=c(mean(rbinom(3,10,c3.p1)),mean(rbinom(3,10,c3.p2))))
  colnames(df) <- c('Route','Sample Mean')
  return(df)
}

gen.p.matrix <- function(){
  m <- c()
  for(j in 1:6){
    p <- rep(0,6)
    s <- sample((1:6)[-j],3)
    cut.points <- runif(2)
    pi <- c(max(cut.points)-min(cut.points),min(cut.points),1-max(cut.points))
    for(i in 1:3){
      p[s[i]] <- pi[i]
    }
    m <- c(m,p)
  }
  return(matrix(m, nrow = 6, ncol = 6, byrow = T))
}

c4.PMatrix <- gen.p.matrix()
c4.am <- rbind(c(0,1,0,1,0,0),c(1,0,1,0,1,0),c(0,1,0,0,0,1),c(1,0,0,0,1,0),c(0,1,0,1,0,1),c(0,0,1,0,1,0),c(1,1,1,0,0,0))
c4.is <- sample(4:6,1)


# Define getaway car 
car.circuit = model2network("[A][B][C|A:B][E|C][D|C][F|D:G][G][H|F][I|F][P|H][K|P:M:N][J|K][L|K][O|N:I][M][N]")

# UI Function
ui <- fluidPage(
  titlePanel("Prison Break!"),
  tabsetPanel(
    tabPanel('Game Dice',
             fluidRow(tags$hr()),
             fluidRow(
               column(2,
                      wellPanel(
                        actionButton(inputId = 'next.turn', label = tags$h4('Next Turn')),
                        tags$p('Restarts timer, generate new PMFs and random values'),
                        tags$h3(textOutput('timeleft'))
                      ),
                      wellPanel(
                        radioButtons(inputId = 'dice.choice', label = 'Choose your dice', choiceNames = c('Dice 1','Dice 2','Dice 3'), choiceValues = c(1,2,3)),
                        actionButton(inputId = 'submit.dice.choice', label = tags$h4('Roll Dice!')),
                        tags$h3(textOutput('dice.roll'))
                      )
                      ),
               column(10,tags$h3('Dice PMFs')),
               column(10,tags$p('Black line indicates fair dice probabilities')),
               column(3,
                      plotOutput('dice1pmf')
               ),
               column(3,
                      plotOutput('dice2pmf')
               ),
               column(3,
                      plotOutput('dice3pmf')
               ),
               column(10,offset = 2,tags$h4('Random Value Table for use in Questions')),
               column(10,offset = 2,tableOutput('valueTable'))
             ),
             fluidRow(tags$hr())
             ),
    
    # Challenge 1
    tabPanel('Breakout!',
      fluidRow(
        column(12,
           tags$h2('Challenge 1'),
           tags$hr(),
           wellPanel(
             tags$h4('You need to breakout of the cell!'),
             tags$p('You have recently managed to pickpocket a guard to obtain information on unlocking the prison cell. It is given below. In addition, a fellow inmate has told you that the sum of the 3 numbers on the combination padlock is ',tags$b(as.character(padlockSum)),'.')
           )
        )  
      ),
      fluidRow(
        column(12,actionButton(inputId = 'revealC1', label = tags$h3('Reveal the rest of the information!'), width = '100%'))
      ),
      fluidRow(
        column(12,tags$br())
      ),
      fluidRow(
        column(12,conditionalPanel(condition = 'input.revealC1 >= 1',tags$img(src = "CombiLock.png", width = '100%')))
      ),
      fluidRow(
        column(12,
          tags$br(),
          conditionalPanel(condition = 'input.revealC1 >= 1',
           wellPanel(
             tags$p(tags$b('What is the probability that you will open the lock on the first try?')),
             tags$p('Note: Giving the wrong answer will lead to a penalty of losing a turn. Key in answer as numerator over denominator')
           ))
        )
      ),
      fluidRow(
        column(8,
         conditionalPanel(condition = 'input.revealC1 >= 1',
            wellPanel(
              numericInput("cellN", "Numerator:", 1, min = 1),
              numericInput("cellD", "Denominator:", 1, min = 1),
              actionButton(inputId = 'submitC1', label = tags$h4('Check Probability'), width = '100%'),
              tags$h3(textOutput('time1Left')),
              conditionalPanel(condition = 'output.result1Show == "Question By:"',tags$h3(textOutput('result1Text'))),
              conditionalPanel(condition = 'output.reset1Show == "Lin Xiao Hao"',actionButton(inputId = 'restarttimer1', label = 'Try Again'))
            )
          )
        )
      ),
      fluidRow(
        column(12,tags$hr())
      ),
      fluidRow(
        column(12,tags$p(textOutput('result1Show'),textOutput('reset1Show')))
      )
    ),
    
    # Challenge 2
    tabPanel('Bypass Guard Room',
             fluidRow(
               column(12,
                      tags$h2('Challenge 2'),
                      tags$hr(),
                      wellPanel(
                        tags$h4('You need to bypass the patrol guards!'),
                        tags$p('Patrol officer tends to slack during patrol duty. The chance of a patrol officer being in the toilet is',tags$b(as.character(floor(c2.x*100))),'%. A fellow inmate knows exactly when the officer is in the toilet, and says that the officer is in the toilet',tags$b(as.character(floor(c2.y*100))),'% of the time when the officer is in the toilet (hence telling the truth) and',tags$b(as.character(floor(c2.z*100))),'% of the time when the officer is not in the toilet (hence lying). If the inmate tells you that the officer is in the toilet, what is the probability that the officer is really in the toilet? If the probability is more than 50%, it will benefit you if you take his advice.'),
                        tags$p("You may choose to take the inmate's information and risk crossing the danger zone in one step, but a failed attempt will result in missing a turn. If you get caught, you can try the danger crossing again on your next available turn or roll the dice.")
                      )
               )  
             ),
             fluidRow(
               column(12,actionButton(inputId = 'revealC2', label = tags$h3('Reveal math notes!'), width = '100%'))
             ),
             fluidRow(
               column(12,tags$br())
             ),
             fluidRow(
               column(12,conditionalPanel(condition = 'input.revealC2 >= 1',tags$img(src = "Bayes.png", width = '100%')))
             ),
             fluidRow(
               column(12,
                      tags$br()
               )
             ),
             fluidRow(
               column(8,
                      conditionalPanel(condition = 'input.revealC2 >= 1',
                                       wellPanel(
                                         actionButton(inputId = 'submitC2', label = tags$h4('Attempt Danger Crossing!'), width = '100%'),
                                         tags$h3(textOutput('time2Left')),
                                         conditionalPanel(condition = 'output.result2Show == "Question By:"',tags$h3(textOutput('result2Text'))),
                                         conditionalPanel(condition = 'output.reset2Show == "Lin Xiao Hao"',actionButton(inputId = 'restarttimer2', label = 'Try Danger Crossing Again'))
                                       )
                      )
               )
             ),
             fluidRow(
               column(12,tags$hr())
             ),
             fluidRow(
               column(12,tags$p(textOutput('result2Show'),textOutput('reset2Show')))
             )
    ),
    
    # Challenge 3
    tabPanel('Avoid Security Check',
             fluidRow(
               column(12,
                      tags$h2('Challenge 3'),
                      tags$hr(),
                      wellPanel(
                        tags$h4('You need to bypass the guard room!'),
                        tags$p('There are two paths to bypass the guard room. You can choose to go pass the guard dogs or to go pass the scanner. You know when somebody passes by the dog or the scanner, the dog barks with probability p1 and the scanner will work with p2. You do not know what the probabilities are.'),
                        tags$p('However, you found ',tags$b('secret records'),' that detail the security lapses at the guard dog and scanner!'),
                        tags$p('The records show in 3 days, for 10 people that walked past the guard dog and scanner each day, the average number of people caught follows the table below:'),
                        tableOutput('c3SampleMean'),
                        tags$p('You remembered knowledge from 40.001 that can help you solve this problem…')
                      )
               )
             ),
             fluidRow(
               column(12,actionButton(inputId = 'revealC3', label = tags$h3('Reveal 40.001 knowledge!'), width = '100%'))
             ),
             fluidRow(
               column(12,tags$br())
             ),
             fluidRow(
               column(12,conditionalPanel(condition = 'input.revealC3 >= 1',tags$img(src = "Binfer.png", width = '100%')))
             ),
             fluidRow(
               column(12,
                      tags$br()
               )
             ),
             fluidRow(
               column(8,
                      conditionalPanel(condition = 'input.revealC3 >= 1',
                                       wellPanel(
                                         actionButton(inputId = 'submitC3.1', label = tags$h4('Sneak past the dog!')),
                                         actionButton(inputId = 'submitC3.2', label = tags$h4('Sneak past the scanners!')),
                                         tags$h3(textOutput('time3Left')),
                                         conditionalPanel(condition = 'output.result3Show == "Question By:"',tags$h3(textOutput('result3Text'))),
                                         conditionalPanel(condition = 'output.reset3Show == "Loh Zheng Yi"',actionButton(inputId = 'restarttimer3', label = 'Try bypassing again'))
                                       )
                      )
               )
             ),
             fluidRow(
               column(12,tags$hr())
             ),
             fluidRow(
               column(12,tags$p(textOutput('result3Show'),textOutput('reset3Show')))
             )
    ),
    
    # Challenge 4
    tabPanel('Avoid Search Light',
             fluidRow(
               column(12,
                      tags$h2('Challenge 4'),
                      tags$hr(),
                      wellPanel(
                        tags$h4('You need to avoid the search light!'),
                        tags$p("Lucky for you, you managed to find some details on the search light's operations. Click the button below to reveal these details"),
                        tags$h5('Possible Player Movements'),
                        tags$p('Out of squares 1,2,3 on the basketball court, choose one square to move to (see diagram below).'),
                        tags$p('You can only move to squares adjacent to your current square. Diagonal movements are not allowed.'),
                        tags$p('If you are caught by the search light, restart at the square right before the basketball court.')
                      )
               )
             ),
             fluidRow(
               column(12,actionButton(inputId = 'revealC4', label = tags$h3('Reveal search light details!'), width = '100%'))
             ),
             fluidRow(
               column(12,tags$br())
             ),
             fluidRow(
               column(12,conditionalPanel(condition = 'input.revealC4 >= 1',tags$img(src = "Markov.png", width = '100%')))
             ),
             fluidRow(
               column(12,
                      tags$br()
               )
             ),
             fluidRow(
               column(6,
                      conditionalPanel(condition = 'input.revealC4 >= 1',
                                       wellPanel(
                                         tags$p('Initial Searchlight Location: ',tags$h4(c4.is)),
                                         tags$p('Current Searchlight Location: ',tags$h4(textOutput('c4SlState'))),
                                         tags$p('Current Player Location: ',tags$h4(textOutput('c4PState'))),
                                         selectInput("nextMoveC4", "Next Player Location",c(1:3)),
                                         actionButton(inputId = 'submitC4', label = tags$h4('Move!'), width = '100%'),
                                         tags$h3(textOutput('time4Left')),
                                         conditionalPanel(condition = 'output.result4Show == "Question By:"',tags$h3(textOutput('result4Text'))),
                                         conditionalPanel(condition = 'output.reset4Show == "Lin Hao  "',actionButton(inputId = 'restarttimer4a', label = 'Next Move')),
                                         conditionalPanel(condition = 'output.reset4Show == "Lin Hao"',actionButton(inputId = 'restarttimer4', label = 'Restart at entrance'))
                                       )
                      )
               ),
               column(6,
                      conditionalPanel(condition = 'input.revealC4 >= 1',
                        tags$p('Transition Matrix (in %):'),
                        tableOutput(formatC(c4.PMatrix*100, digits = 3, format ='fg')),
                        tags$p('Calculate State Probabilities after n turns'),
                        numericInput("c4MatrixPower", "Turns", 1, min = 1),
                        tableOutput('c4StateProb')
                      )
              )
             ),
             fluidRow(
               column(12,tags$hr())
             ),
             fluidRow(
               column(12,tags$p(textOutput('result4Show'),textOutput('reset4Show')))
             )
    ),
    
    # Challenge 5
    tabPanel('Car Escape',
             fluidRow(
               column(12,
                      tags$h2('Final Challenge'),
                      tags$hr(),
                      wellPanel(
                        tags$h3('Instructions'),
                        tags$p('To complete your escape plan, you have to hotwire a getaway car. The car has a stochastic alarm system. As the car alarm is armed with a nuclear bomb, you have to figure out how to hotwire the car such that electricity does not flow from Node A (orange) to Node O (green). You found a manual in the car that provides you with tips on hotwiring.'),
                        tags$h4(tags$b('You can only hotwire 1 node!')),
                        tags$p('Note: Think of all possible paths from A to O. Each intersection of nodes is one of the following 3 possibilities. You just need to hotwire one node to break the electricity flow.'),
                        tags$p('Choosing the wrong node will lead to a penalty of losing a turn')
                      ),
                      tags$hr()
               )
             ),
             fluidRow(
               column(12,tags$img(src = "HotwireInstructions.png", width = '100%'))
             ),
             fluidRow(
               column(12,tags$br())
             ),
             fluidRow(
               column(12,tags$img(src = "HotwiringEg.png", width = '100%'))
             ),
             fluidRow(
               column(12,tags$hr())
             ),
             fluidRow(
               column(12,actionButton(inputId = 'revealCircuit', label = tags$h3('I am ready to escape! Show me the circuit!'), width = '100%'))
             ),
             fluidRow(
               column(12,tags$br())
             ),
             fluidRow(
               column(4,
                      conditionalPanel(condition = 'input.revealCircuit >= 1',
                                       wellPanel(
                                         tags$h4('Choose node to hotwire'),
                                         selectInput("nodeChoice", "Node", c("B","C","D","E","F","G","H","I","J","K","L","M","N","P")),
                                         actionButton(inputId = 'submitNodeChoice', label = tags$h4('Hotwire this node!')),
                                         tags$h3(textOutput('time5left')),
                                         conditionalPanel(condition = 'output.result5Show == "Circuit By:"',tags$h3(textOutput('result5Text'))),
                                         conditionalPanel(condition = 'output.reset5Show == "Gladwin Lam"',actionButton(inputId = 'restarttimer5', label = 'Try Again'))
                                       )
                      )
                      ),
               column(6,
                      conditionalPanel(condition = 'input.revealCircuit >= 1',
                        tags$img(src = "HotwireCircuit.png", width = "600px")
                      )
                      )
             ),
             fluidRow(
               column(12,tags$hr())
             ),
             fluidRow(
               column(12,tags$p(textOutput('result5Show'),textOutput('reset5Show')))
             )
             )
    
  )
)

# Server Function
server <- function(input, output, session) {
  # Initialize timer variables
  turnTime <- 1*60
  time1 <- 3*60
  time2 <- 3*60
  time3 <- 3*60
  time4 <- 60
  time5 <- 3*60
  
  turnTimer <- reactiveVal(turnTime)
  timer1 <- reactiveVal(time1)
  timer2 <- reactiveVal(time2)
  timer3 <- reactiveVal(time3)
  timer4 <- reactiveVal(time4)
  timer5 <- reactiveVal(time5)
  
  activeTurnTimer <- reactiveVal(F)
  activetimer1 <- reactiveVal(F)
  activetimer2 <- reactiveVal(F)
  activetimer3 <- reactiveVal(F)
  activetimer4 <- reactiveVal(F)
  activetimer5 <- reactiveVal(F)
  
  diceRolled <- reactiveVal(F)

  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(activeTurnTimer())
      {
        turnTimer(turnTimer()-1)
        if(turnTimer()<1)
        {
          activeTurnTimer(F)
          diceRolled(F)
          turnTimer(turnTime)
          showModal(modalDialog(
            title = "Turn Ended",
            "You are out of time!"
          ))
        }
      }
    })
  })
  
  rv <- reactiveValues(choices = sample(1:4,3),
                       roll = 0,
                       pmf = gen.pmf(),
                       values.table = gen.values(),
                       c1.result = NA,
                       result1.show = 'Question By: ',
                       reset1.show = 'Lin Xiao Hao ',
                       c2.result = NA,
                       result2.show = 'Question By: ',
                       reset2.show = 'Lin Xiao Hao ',
                       c3.result = NA,
                       result3.show = 'Question By: ',
                       reset3.show = 'Loh Zheng Yi ',
                       c3.samplemean = gen.c3.samplemean(),
                       c4.result = NA,
                       c4.slstate = c4.is,
                       c4.pState = 7,
                       result4.show = 'Question By: ',
                       reset4.show = 'Lin Hao ',
                       c5.result = NA,
                       result5.show = 'Circuit By: ',
                       reset5.show = 'Gladwin Lam ')
  
  observeEvent(input$next.turn, {
    rv$pmf <- gen.pmf()
    rv$values.table <- gen.values()
    rv$choices <- sample(1:4,3)
    activeTurnTimer(T)
    diceRolled(F)
    turnTimer(turnTime)
    })
  
  observeEvent(input$submit.dice.choice, {
    if(activeTurnTimer()&!diceRolled()){
      rv$roll <- rv$pmf[[rv$choices[as.integer(input$dice.choice)]]]$X[which(runif(1) <= cumsum(rv$pmf[[rv$choices[as.integer(input$dice.choice)]]]$Y))[1]]
      diceRolled(T)
      activeTurnTimer(F)
    }else if(diceRolled()){
      showModal(modalDialog(title = "Dice Rolled","You may not roll the dice more than once per turn"))
    }else{
      showModal(modalDialog(title = "Turn Ended","Please press 'Next Turn' button before rolling dice"))
    }
    })
  
  output$dice1pmf <- renderPlot({
    ggplot(data = rv$pmf[[rv$choices[1]]], aes(x=as.factor(X),y=Y,label=formatC(Y, digits = 3, format ='fg'))) + 
      geom_col(fill = 'pink', width = 1/1.618) + geom_text(nudge_y = 0.001) + geom_hline(aes(yintercept = 1/6)) + 
      labs(x = 'Roll Outcome', y = 'P(X=x)', title = 'Dice 1 Probability Mass Function') + coord_cartesian(ylim=c(0,0.6)) +
      theme_minimal()
  })
  
  output$dice2pmf <- renderPlot({
    ggplot(data = rv$pmf[[rv$choices[2]]], aes(x=as.factor(X),y=Y,label=formatC(Y, digits = 3, format ='fg'))) + 
      geom_col(fill = 'pink', width = 1/1.618) + geom_text(nudge_y = 0.001) + geom_hline(aes(yintercept = 1/6)) + 
      labs(x = 'Roll Outcome', y = 'P(X=x)', title = 'Dice 2 Probability Mass Function') + coord_cartesian(ylim=c(0,0.6)) +
      theme_minimal()
  })
  
  output$dice3pmf <- renderPlot({
    ggplot(data = rv$pmf[[rv$choices[3]]], aes(x=as.factor(X),y=Y,label=formatC(Y, digits = 3, format ='fg'))) + 
      geom_col(fill = 'pink', width = 1/1.618) + geom_text(nudge_y = 0.001) + geom_hline(aes(yintercept = 1/6)) + 
      labs(x = 'Roll Outcome', y = 'P(X=x)', title = 'Dice 3 Probability Mass Function') + coord_cartesian(ylim=c(0,0.6)) +
      theme_minimal()
  })
  
  output$dice.roll <- renderText({
    paste('You have rolled: ',rv$roll)
    })
  
  output$valueTable <- renderTable({
    rv$values.table
  })
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", turnTimer(),'s',sep='')
  })
  
  # Events for challenge 1
  observe({
    invalidateLater(1000, session)
    isolate({
      if(activetimer1())
      {
        timer1(timer1()-1)
        if(timer1()<1)
        {
          activetimer1(F)
          timer1(time1)
          rv$c5.result <- 'Time is out! Try again next turn!'
          rv$result1.show <- 'Question By:'
          rv$reset1.show <- 'Lin Xiao Hao'
        }}})})
  
  observeEvent(input$revealC1, {
    activeTurnTimer(F)
    activetimer1(T)
    timer1(time1)
  })
  
  observeEvent(input$restarttimer1, {
    activetimer1(T)
    timer1(time1)
    rv$result1.show <- 'Question By: '
    rv$reset1.show <- 'Lin Xiao Hao '
  })
  
  observeEvent(input$submitC1, {
    if(input$cellN/input$cellD == 1/(sum(1:(padlockSum-2)))){
      rv$c1.result <- 'Success!'
    }else{
      rv$c1.result <- 'Wrong! Try again next turn!'
      rv$reset1.show <- 'Lin Xiao Hao'
    }
    rv$result1.show <- 'Question By:'
    activetimer1(F)
  })
  
  output$time1Left <- renderText({
    paste("Time left: ", timer1(),'s',sep='')
  })
  
  output$result1Show <- renderText({
    paste(rv$result1.show)
  })
  
  output$reset1Show <- renderText({
    paste(rv$reset1.show)
  })
  
  output$result1Text <- renderText({
    paste(rv$c1.result)
  })
  
  # Events for challenge 2
  observe({
    invalidateLater(1000, session)
    isolate({
      if(activetimer2())
      {
        timer2(timer2()-1)
        if(timer2()<1)
        {
          activetimer2(F)
          timer2(time2)
          rv$c2.result <- 'Time is out! Try again next turn!'
          rv$result2.show <- 'Question By:'
          rv$reset2.show <- 'Lin Xiao Hao'
        }}})})
  
  observeEvent(input$revealC2, {
    activeTurnTimer(F)
    activetimer2(T)
    timer2(time2)
  })
  
  observeEvent(input$restarttimer2, {
    activetimer2(T)
    timer2(time2)
    rv$result2.show <- 'Question By: '
    rv$reset2.show <- 'Lin Xiao Hao '
  })
  
  observeEvent(input$submitC2, {
    if(runif(1)<=c2.y*c2.x/(c2.x*c2.y+c2.z*(1-c2.x))){
      rv$c2.result <- 'Success!'
    }else{
      rv$c2.result <- 'Caught! Lose a turn!'
      rv$reset2.show <- 'Lin Xiao Hao'
    }
    rv$result2.show <- 'Question By:'
    activetimer2(F)
  })
  
  output$time2Left <- renderText({
    paste("Time left: ", timer2(),'s',sep='')
  })
  
  output$result2Show <- renderText({
    paste(rv$result2.show)
  })
  
  output$reset2Show <- renderText({
    paste(rv$reset2.show)
  })
  
  output$result2Text <- renderText({
    paste(rv$c2.result)
  })
  
  # Events for challenge 3
  observe({
    invalidateLater(1000, session)
    isolate({
      if(activetimer3())
      {
        timer3(timer3()-1)
        if(timer3()<1)
        {
          activetimer3(F)
          timer2(time3)
          rv$c3.result <- 'Time is out! Try again next turn!'
          rv$result3.show <- 'Question By:'
          rv$reset3.show <- 'Loh Zheng Yi'
        }}})})
  
  observeEvent(input$revealC3, {
    activeTurnTimer(F)
    activetimer3(T)
    timer3(time3)
  })
  
  observeEvent(input$restarttimer3, {
    activetimer3(T)
    timer3(time3)
    rv$result3.show <- 'Question By: '
    rv$reset3.show <- 'Loh Zheng Yi '
  })
  
  observeEvent(input$submitC3.1, {
    if(runif(1)<=(1-c3.p1)){
      rv$c3.result <- 'Success!'
    }else{
      rv$c3.result <- 'Caught! Try again next turn!'
      rv$reset3.show <- 'Loh Zheng Yi'
    }
    rv$result3.show <- 'Question By:'
    activetimer3(F)
  })
  
  observeEvent(input$submitC3.2, {
    if(runif(1)<=(1-c3.p2)){
      rv$c3.result <- 'Success!'
    }else{
      rv$c3.result <- 'Caught! Try again next turn!'
      rv$reset3.show <- 'Loh Zheng Yi'
    }
    rv$result3.show <- 'Question By:'
    activetimer3(F)
  })
  
  output$time3Left <- renderText({
    paste("Time left: ", timer3(),'s',sep='')
  })
  
  output$result3Show <- renderText({
    paste(rv$result3.show)
  })
  
  output$reset3Show <- renderText({
    paste(rv$reset3.show)
  })
  
  output$result3Text <- renderText({
    paste(rv$c3.result)
  })
  
  output$c3SampleMean <- renderTable({
    rv$c3.samplemean
  })
  
  # Events for challenge 4
  observe({
    invalidateLater(1000, session)
    isolate({
      if(activetimer4())
      {
        timer4(timer4()-1)
        if(timer4()<1)
        {
          activetimer4(F)
          timer4(time4)
          rv$c4.result <- 'Time is out! Restart at Entrance!'
          rv$result4.show <- 'Question By:'
          rv$reset4.show <- 'Lin Hao'
        }}})})
  
  observeEvent(input$revealC4, {
    activeTurnTimer(F)
    activetimer4(T)
    timer4(time4)
  })
  
  observeEvent(input$restarttimer4, { # Hard Reset to entrance
    activetimer4(T)
    timer4(time4)
    rv$result4.show <- 'Question By: '
    rv$reset4.show <- 'Lin Hao '
    rv$c4.pState <- 7
  })
  
  observeEvent(input$restarttimer4a, { # Soft Reset
    activetimer4(T)
    timer4(time4)
    rv$result4.show <- 'Question By: '
    rv$reset4.show <- 'Lin Hao '
  })
  
  observeEvent(input$submitC4, {
    rv$c4.slstate <- which(runif(1)<=cumsum(c4.PMatrix[rv$c4.slstate,]))[1]
    if(rv$c4.slstate!=as.integer(input$nextMoveC4) & as.integer(input$nextMoveC4)==5){
      rv$c4.pState <- as.integer(input$nextMoveC4)
      rv$c4.result <- 'Completed Searchlight Challenge!'
      rv$reset4.show <- 'Lin Hao '
    }else if(rv$c4.slstate!=as.integer(input$nextMoveC4) & as.integer(input$nextMoveC4)!=5){
      rv$c4.pState <- as.integer(input$nextMoveC4)
      rv$c4.result <- 'Evaded searchlight! Continue moving!'
      rv$reset4.show <- 'Lin Hao  '
    }else if(rv$c4.slstate==as.integer(input$nextMoveC4)){
      rv$c4.pState <- as.integer(input$nextMoveC4)
      rv$c4.result <- 'Caught! Restart at entrance!'
      rv$reset4.show <- 'Lin Hao'
    }
    rv$result4.show <- 'Question By:'
    activetimer4(F)
  })
  
  output$time4Left <- renderText({
    paste("Time left: ", timer4(),'s',sep='')
  })
  
  output$c4StateProb <- renderTable({
    sapply(1:6, function(x) ifelse(x==rv$c4.slstate,1,0))%*%(c4.PMatrix%^%input$c4MatrixPower)
  })
  
  output$c4SlState <- renderText({
    paste(rv$c4.slstate)
  })
  
  output$c4PState <- renderText({
    paste(ifelse(rv$c4.pState==7,'Start',rv$c4.pState))
  })
  
  output$result4Show <- renderText({
    paste(rv$result4.show)
  })
  
  output$reset4Show <- renderText({
    paste(rv$reset4.show)
  })
  
  output$result4Text <- renderText({
    paste(rv$c4.result)
  })
  
  observe({
    x <- which(c4.am[rv$c4.pState,]==1)
    # Can also set the label and select items
    updateSelectInput(session, "nextMoveC4",
                      label = "Next Player Location",
                      choices = x
    )
  })
  
  # Events for challenge 5
  observe({
    invalidateLater(1000, session)
    isolate({
      if(activetimer5())
      {
        timer5(timer5()-1)
        if(timer5()<1)
        {
          activetimer5(F)
          timer5(time5)
          rv$c5.result <- 'Nuclear bomb has exploded! Try again next turn!'
          rv$result5.show <- 'Circuit By:'
          rv$reset5.show <- 'Gladwin Lam'
        }}})})
  
  observeEvent(input$revealCircuit, {
    activeTurnTimer(F)
    activetimer5(T)
    timer5(time5)
  })
  
  observeEvent(input$restarttimer5, {
    activetimer5(T)
    timer5(time5)
    rv$result5.show <- 'Circuit By: '
    rv$reset5.show <- 'Gladwin Lam '
  })
  
  observeEvent(input$submitNodeChoice, {
    if(dsep(car.circuit, "A", "O", input$nodeChoice)){
      rv$c5.result <- 'Success'
    }else{
      rv$c5.result <- 'Nuclear bomb has exploded! Try again next turn!'
      rv$reset5.show <- 'Gladwin Lam'
    }
    rv$result5.show <- 'Circuit By:'
    activetimer5(F)
  })
  
  output$time5left <- renderText({
    paste("Time left: ", timer5(),'s',sep='')
  })
  
  output$result5Show <- renderText({
    paste(rv$result5.show)
  })
  
  output$reset5Show <- renderText({
    paste(rv$reset5.show)
  })
  
  output$result5Text <- renderText({
    paste(rv$c5.result)
  })
  
  
}

shinyApp(ui, server)

# choices = sample(1:4,3)
# dice.choice = 3

# as.character(pmf[[choices[dice.choice = 3]]]$X[which(runif(1) <= cumsum(pmf[[choices[dice.choice = 3]]]$Y))[1]])

# 
# ggplot(data = pmf[[1]], aes(x=as.factor(X),y=Y,label=formatC(Y, digits = 3, format ='fg'))) + 
#   geom_col(fill = 'pink', width = 1/1.618) + geom_text(nudge_y = 0.001) + geom_hline(aes(yintercept = 1/6)) + 
#   labs(x = 'Dice Number', y = 'P(X=x)', title = 'Dice 3 Probability Mass Function') + coord_cartesian(ylim=c(0,0.3)) +
#   theme_minimal()
# library(bnlearn)

