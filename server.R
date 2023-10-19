# Load packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(EDAWR)
library(tidyr)   
library(shinyAce)
library(knitr)
library(rLocker)
library(datasets)  
library(rmarkdown) 
library(learnr)    
library(rcis)  
library(shinycssloaders)
library(boastUtils)


#https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf
# If issues develop wiht rcfss or EDAWR these may come in useful
# remotes::install_github("uc-cfss/rcfss")
# devtools::install_github("rstudio/EDAWR")



bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE) #This contains all of the questions, possible responses and answers
#View(bank) #way that you can view the questions, comment when not using
source("helpers.R")

# i button

shinyServer(function(input, output, session) {
  observeEvent(input$info0,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Move the sliders or select from the dropdown menus and view the R code that produces the results.",
      type = "info"
    )
  })
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Move the sliders or select from the dropdown menus and view the R code that produces the results.",
      type = "info"
    )
  })
  observeEvent(input$go2, {
    updateTabItems(session, 'tabs', 'exp1')
  })
  
  
  
  # Explore Data ----
  
  ## unite ----
  
  birth_df<- data.frame(
    first = c("John", "Jane", "Alice", "Bob", "Eve", "Sam", "Lucy", "Tom", "Sue", "Roy"),
    last = c("Doe", "Smith", "Johnson", "Brown", "Davis", "Miller", "Garcia", "Rodriguez", "Martinez", "Hernandez"),
    day = c('12','5', '23', '18', '29', '14', '9', '1', '27', '15'),
    month = c("1", "9", "3", "5", "8", "6", "12", "8", "6", "4"),
    year = c('1990', '1985', '1992', '1988', '1994', '1987', '1991', '1989', '1993', '1996')
  )
  output$uniteData <- renderTable ({
    birth_df
  })
  
  output$uniteOutput2 <- renderTable({
    if (input$un1 == "Day, Month, Year") {
      united_data <- unite(birth_df, "Birth_Date", day, month, year, sep = "-")
    } else if (input$un1 == "First_Name, Last_Name") {
      united_data <- unite(birth_df, "Full_Name", first, last, sep = " ")
    } else {
      united_data <- birth_df
    }
  })
  
  output$uniteUI <- renderUI({
    if (input$un1 == "Day, Month, Year") {
      tags$code("R code: tidyr::unite(birth_df, 'Date', day, month, year, sep = '-')")
    } else if (input$un1 == "First_Name, Last_Name") {
      tags$code("R code: tidyr::unite(birth_df, 'Full_Name', first, last, sep = '-')")
    } else {
      NULL
    }
  })
  
  
  
  ## pivot_longer ----
  output$dwTable5 <- renderTable({
    pollution
  })
  
  output$dwTable6 <- renderTable({
    if (input$dw3 == TRUE) {
      pollution %>%
        tidyr::pivot_wider(names_from = "size", values_from = "amount")
    }
  })
  
  ## pivot_wider ----
  output$dwTable1 <- renderTable({
    cases
  })
  
  output$dwTable2 <- renderTable({
    if (input$dw1 == TRUE) {
      cases %>%
        tidyr::pivot_longer(cols = c("2011","2012","2013"), names_to = "year", values_to = "n")
    }
  })
  
  ## arrange ----
  output$dwTable8 <- renderTable ({
    if (input$dwSTI2 == 'Low to High') {
      head(dplyr::arrange(mtcars, mtcars[ , input$dwSTI1]))
    }
    else if (input$dwSTI2 == 'High to Low') {
      head(dplyr::arrange(mtcars, desc(mtcars[ , input$dwSTI1])))
    }
    else {
      head(head(mtcars))
    }
  })
  
  output$code1 <- renderUI ({
    if (input$dwSTI2 == 'Low to High') {
      tags$code(paste('R Code: dplyr::arrange(mtcars, mtcars[ , ', input$dwSTI1, ']'))
    }
  })
  
  output$code2 <- renderUI ({
    if (input$dwSTI2 == 'High to Low') {
      tags$code(paste('Code: dplyr::arrange(mtcars, descmtcars[ , ', input$dwSTI1, '])'))
    }
  })
  
  
  
  
  # Tidy Data ----
  
  ## pivot_longer 1 ----
  
  RawData <- table4a
  
  
  output$original1 <- renderTable({
    RawData
  })
  
  
  # Specify Outputs pivot_longer1 
  output$userOut1 <- renderTable({
    if(input$userOp1 == 'country')
      op1 <- 'country'
    else if(input$userOp1 == '1999')
      op1 <- '1999'
    else if(input$userOp1 == 'Afghanistan')
      op1 <- 'Afghanistan'
    else
      op1 <- '2000'
    
    
    if(input$userOp2 == 'country')
      op2 <- 'country'
    else if(input$userOp2 == '1999')
      op2 <- '1999'
    else if(input$userOp2 == 'Afghanistan')
      op2 <- 'Afghanistan'
    else
      op2 <- '2000'
    
    
    if(input$userOp3 == '1999')
      op3 <- '1999'
    else if(input$userOp3 == 'cases')
      op3 <- 'cases'
    else if(input$userOp3 == 'year')
      op3 <- 'year'
    else
      op3 <- 'China'
    
    
    if(input$userOp4 == '1999')
      op4 <- '1999'
    else if(input$userOp4 == 'cases')
      op4 <- 'cases'
    else if(input$userOp4 == 'year')
      op4 <- 'year'
    else
      op4 <- 'China'
    
    
    RawData <- RawData %>%
      mutate('1999' = as.character(RawData$'1999')) %>%
      mutate('2000' = as.character(RawData$'2000'))
    tryCatch({
      RawData %>%
        pivot_longer(cols = c(op1,op2), names_to =op3, values_to = op4)
    }, 
    warning <- function(war) {
      
      return("warning")
    },
    error = function(err) {
      return("That code would produce no output")
    }
    )
    
  })
  
  
  # Bottom of options
  
  #dynamic code based on user inputs 
  output$tidyAttemptTable <- renderUI({
    tags$code(paste0('tidyr::pivot_longer(RawData, cols = c("', input$userOp1, '","', input$userOp2, '") ,
             names_to = "', input$userOp3, '", values_to = "', input$userOp4, '")' ))
  })
  
  

  # submit button
  output$sub <- renderUI({
    bsButton("submitcc",
             label = "Check Answer",
             icon("lightbulb"),
             size = "medium",
             style = 'success')
  })
  
  
  # delay submit button
  observeEvent(input$submitcc,{
    withProgress(session, min = 1, max = 15, {
      setProgress(message = 'Checking Answer',
                  detail = '')
      for (i in 1:13) {
        setProgress(value = i)
        Sys.sleep(0.05)
      }
    })
  })
  
  
  # correct/wrong gif 
  
  
  output$resetcc <- renderUI({
    bsButton("retry",
             label = "Try Again",
             icon("retweet"),
             size = "medium",
             style = 'success')
  })
  
  # delay retry button
  observeEvent(input$retry,{
    withProgress(session, min = 1, max = 15, {
      setProgress(message = 'Resetting',
                  detail = '')
      for (i in 1:13) {
        setProgress(value = i)
        Sys.sleep(0.05)
      }
      somethingIsSelected <<- F
      disable("submit")
    })
  })
  
  # Next Question
  output$nextQuestion1 <- renderUI({
    bsButton("next1",
             label = "Next Question",
             size = "medium",
             style = 'success')
  })
  observeEvent(input$next1, {
    updateTabsetPanel(session, "questionTabs",
                      selected = "pivot_longer2")
  })
  
  
  
  
  # hide reset button upon opening app
  hide("resetcc")
  hide("correct")
  hide("wrong")
  hide("nextQuestion1")
  
  
  output$correct <- renderUI({
    tags$img(src = "correct.png", width = 30)
  })
  
  output$wrong <- renderUI({
    tags$img(src = "incorrect.png", width = 30)
  })
  
  
  
  
  # show reset button after submit is clicked, disable dropdown inputs
  observeEvent(input$submitcc,{
    toggle("resetcc")
    toggle("nextQuestion1")
    disable("userOp1")
    disable("userOp2")
    disable("userOp3")
    disable("userOp4")
    disable("submitcc")
    if(input$userOp1 == '1999' & input$userOp2 == '2000'
       & input$userOp3 == 'year' & input$userOp4 == 'cases' || input$userOp1 == '2000' & input$userOp2 == '1999'
       & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      showElement("correct")
    }
    else{
      showElement("wrong")
    }
    
    
  })
  observeEvent(input$retry,{
    enable("userOp1")
    enable("userOp2")
    enable("userOp3")
    enable("userOp4")
    enable("submitcc")
    enable("sub")
    toggle("resetcc")
    hide("correct")
    hide("wrong")
  })
  
  
  
  ## pivot_longer 2 ----
  
  capital <- c("Kabul", "Brasilia", "Beijing")
  
  table4a$capital <- capital
  
  RawData2 <- data.frame("Name" = c("John","Dora","Tim","Rebecca"),
                         "Age" = c('21','19','22','21'),
                         "MonTips" = c('8','7','12','10'),
                         "TueTips" = c('14','10','11','9'),
                         "WedTips" = c('11','14','13','11'))
  
  output$original3 <- renderTable({
    RawData2
  })
  
  # specify outputs for every choice
  
  # This code creates the your tidy attempt output
  output$tidyAttempt2 <- renderTable({
    if(input$userOp5 == 'John')
      op5 <- 'John'
    else if(input$userOp5 == 'MonTips')
      op5 <- 'MonTips'
    else if(input$userOp5 == 'Age')
      op5 <- 'Age'
    else
      op5 <- 'Name'
    
    
    if(input$userOp6 == 'Tim')
      op6 <- 'Tim'
    else if(input$userOp6 == 'MonTips')
      op6 <- 'MonTips'
    else if(input$userOp6 == 'TueTips')
      op6 <- 'TueTips'
    else
      op6 <- '21'
    
    if(input$userOp7 == 'Age')
      op7 <- 'Age'
    else if(input$userOp7 == 'Name')
      op7 <- 'Name'
    else if(input$userOp7 == '7')
      op7 <- '7'
    else
      op7 <- 'WedTips'
    
    
    if(input$userOp8 == '21')
      op8 <- '21'
    else if(input$userOp8 == 'MonTips')
      op8 <- 'MonTips'
    else if(input$userOp8 == 'Tips')
      op8 <- 'Tips'
    else
      op8 <- 'Day'
    
    if(input$userOp9 == '8')
      op9 <- '9'
    else if(input$userOp9 == 'Age')
      op9 <- 'Age'
    else if(input$userOp9 == 'Day')
      op9 <- 'Day'
    else
      op9 <- 'Tips'
    
    tryCatch({
      RawData2 %>%
        pivot_longer(cols = c(op5,op6,op7), names_to = op8, values_to = op9)
    }, 
    warning = function(war) {
      
      return("warning")
    },
    error = function(err) {
      return("That code would produce no output")
    }
    )
    
  })
  
  
  
  
  # show code based on inputs
  output$userOutY <- renderUI({
    tags$code(paste0('tidyr::pivot_longer(RawData2, cols = c("', input$userOp5, '","', input$userOp6, '"),
             names_to = "', input$userOp7, '", values_to = "', input$userOp8, '")' ))
  }) 
  
  #pivot_longer2
  observeEvent(input$retry2,{
    enable("userOp5")
    enable("userOp6")
    enable("userOp7")
    enable("userOp8")
    enable("userOp9")
    enable("submitting")
    toggle("restart")
    hide("cort")
    hide("rong")
  })
  
  
  # submit button
  output$subbed <- renderUI({
    bsButton("submitting",
             label = "Check Answer",
             icon("lightbulb"),
             size = "medium",
             style = 'success')
  })
  
  observeEvent(input$submitting,{
    withProgress(session, min = 1, max = 15, {
      setProgress(message = 'Checking Answer',
                  detail = '')
      for (i in 1:13) {
        setProgress(value = i)
        Sys.sleep(0.05)
      }
    })
  })
  
  
  output$restart <- renderUI({#Ethan 8/3
    bsButton("retry2",
             label = "Try Again",
             icon("retweet"),
             size = "medium",
             style = 'success')
  })
  
  # delay retry button
  observeEvent(input$retryy,{
    withProgress(session, min = 1, max = 15, {
      setProgress(message = 'Resetting',
                  detail = '')
      for (i in 1:13) {
        setProgress(value = i)
        Sys.sleep(0.05)
      }
    })
  })
  
  
  # hide reset button upon opening app
  hide("restart")
  hide("cort")
  hide("rong")
  
  
  output$cort <- renderUI({
    tags$img(src = "correct.png", width = 30)
  })
  
  output$rong <- renderUI({
    tags$img(src = "incorrect.png", width = 30)
  })
  
  
  # pivot_longer2 Check
  observeEvent(input$submitting,{
    toggle("restart")
    disable("userOp5")
    disable("userOp6")
    disable("userOp7")
    disable("userOp8")
    disable("userOp9")
    disable("submitting")
    if(input$userOp5 == 'MonTips' & input$userOp6 == 'TueTips'
       & input$userOp7 == 'WedTips' & input$userOp8 == 'Day' &
       input$userOp9 == 'Tips') {
      showElement("cort")
    }
    else{
      showElement("rong")
    }
    
    
  })
  
  
  observeEvent(input$retryy,{
    hide("restart")
    enable("userOp5")
    enable("userOp6")
    enable("userOp7")
    enable("userOp8")
    showElement("submitting")
    enable("submitting")
    hide("cort")
    hide("rong")
    
  })
  
  observeEvent(input$retryy,{
    reset("userOp5")
    reset("userOp6")
    reset("userOp7")
    reset("userOp8")
    showElement("submitting")
    enable("submitting")
    
  })
  
  
  ## pivot_wider 1 ----
  
  RawData3 <- data.frame("country" = c("Afghanistan","Afghanistan","Australia","Australia","China","China"),
                         "key" = c("GDP","population","GDP","population","GDP","population"),
                         "data" = c("19","37","1434","25","13610","1393"))
  
  output$original2 <- renderTable({
    RawData3
  })
  
  # specify outputs for every choice
  output$userOutA <- renderTable({ 
    if(input$userOpA == 'population')
      op1 <- 'population'
    else if(input$userOpA == '25')
      op1 <- '25'
    else if(input$userOpA == 'key')
      op1 <- 'key'
    else
      op1 <- 'cases'
    
    
    if(input$userOpB == 'key')
      op2 <- 'key'
    else if(input$userOpB == 'data')
      op2 <- 'data'
    else if(input$userOpB == 'GDP')
      op2 <- 'GDP'
    else
      op2 <- '1393'
    
    
    tryCatch({
      RawData3 %>%
        pivot_wider(names_from = op1, values_from = op2)
    }, 
    warning = function(war) {
      
      return("warning")
    },
    error = function(err) {
      return("That code would produce no output")
    }
    )
    
    
    
  })
  
  # Bottom of options 
  
  # show code based on inputs
  output$userOutB <- renderUI({
    tags$code(paste0('tidyr::pivot_wider(RawData3, names_from = "', input$userOpA, '", values_from = "', input$userOpB, '")' ))
  })
  

  # submit button pivot_wider
  output$bus <- renderUI({
    bsButton("submitted",
             label = "Check Answer",
             icon("lightbulb"),
             size = "medium",
             style = 'success')
  })
  
  observeEvent(input$submitted,{
    withProgress(session, min = 1, max = 15, {
      setProgress(message = 'Checking Answer',
                  detail = '')
      for (i in 1:13) {
        setProgress(value = i)
        Sys.sleep(0.05)
      }
    })
  })
  
  
  
  output$redo <- renderUI({
    bsButton("retrying",
             label = "Try Again",
             icon("retweet"),
             size = "medium",
             style = 'success')
  })
  
  # delay retry button
  observeEvent(input$retrying, {
    withProgress(session, min = 1, max = 15, {
      setProgress(message = 'Resetting',
                  detail = '')
      for (i in 1:13) {
        setProgress(value = i)
        Sys.sleep(0.05)
      }
    })
  })
  
  # Next Question 
  output$nextQuestion2 <- renderUI({
    bsButton("next2",
             label = "Next Question",
             size = "medium",
             style = 'success')
  })
  observeEvent(input$next2, {
    updateTabsetPanel(session, "questionTabs",
                      selected = "pivot_wider1")
  })
  
  
  
  # hide reset button upon opening app
  hide("redo")
  hide("cor")
  hide("wro")
  hide("nextQuestion2")
  
  
  output$cor <- renderUI({
    tags$img(src = "correct.png", width = 30)
  })
  
  output$wro <- renderUI({
    tags$img(src = "incorrect.png", width = 30)
  })
  
  
  # show reset button after submit is clicked, disable dropdown inputs
  observeEvent(input$submitted,{
    toggle("redo")
    toggle("nextQuestion2")
    disable("userOpA")
    disable("userOpB")
    disable("userOpC")
    disable("userOpD")
    disable("submitted")
    if(input$userOpA == 'key' & input$userOpB == 'data')
    {
      showElement("cor")
    }
    else{
      showElement("wro")
    }
    
  })
  
  
  observeEvent(input$retrying,{
    hide("redo")
    enable("userOpA")
    enable("userOpB")
    enable("userOpC")
    enable("userOpD")
    showElement("submitted")
    enable("submitted")
    hide("cor")
    hide("wro")
    hide("nextQuestion2")
  })
  
  
  ##################### pivot_wider 2 ###################
  
  capital <- c("Kabul", "Brasilia", "Beijing")
  
  table4b$capital <- capital
  
  RawData4 <- data.frame("Name" = c("John","John","John","Dora","Dora","Dora","Tim","Tim","Tim","Rebecca","Rebecca","Rebecca"),
                         "Age" = c('21','21','21','19','19','19','22','22','22','21','21','21'),
                         "Paycheck" = c('Wage','Tips','Tax','Wage','Tips','Tax','Wage','Tips','Tax','Wage','Tips','Tax'),
                         "Dollars" = c('25','30','14','36','37','21','22','31','13','41','50','24'))
  
  output$original4 <- renderTable({
    RawData4
  })
  
  
  
  # specify outputs for every choice
  output$userOut3 <- renderTable({ #Ethan
    if(input$userOpC == '22')
      op1 <- '22'
    else if(input$userOpC == 'Age')
      op1 <- 'Age'
    else if(input$userOpC == 'Tips')
      op1 <- 'Tips'
    else
      op1 <- 'Paycheck'
    
    
    if(input$userOpD == 'Name')
      op2 <- 'Name'
    else if(input$userOpD == 'Dollars')
      op2 <- 'Dollars'
    else if(input$userOpD == 'Day')
      op2 <- 'Day'
    else
      op2 <- 'Wage'
    
    tryCatch({
      RawData4 %>%
        pivot_wider(names_from =op1, values_from = op2)
    }, 
    warning = function(war) {
      
      return("warning")
    },
    error = function(err) {
      return("That code would produce no output")
    }
    )
    
  })
  
  # Bottom of options 
  
  # show code based on inputs
  output$userOut4 <- renderUI({
    tags$code(paste0('tidyr::pivot_wider(RawData4, 
                     names_from = "', input$userOpC, '", 
                     values_from = "', input$userOpD, '")' ))
  }) 
  
  # submit button
  output$buss <- renderUI({
    bsButton("submitteds",
             label = "Check Answer",
             icon("lightbulb"),
             size = "medium",
             style = 'success')
  })
  
  observeEvent(input$submitteds,{
    withProgress(session, min = 1, max = 15, {
      setProgress(message = 'Checking Answer',
                  detail = '')
      for (i in 1:13) {
        setProgress(value = i)
        Sys.sleep(0.05)
      }
    })
  })
  
  
  
  output$redos <- renderUI({
    bsButton("retryings",
             label = "Try Again",
             icon("retweet"),
             size = "medium",
             style = 'success')
  })
  
  # delay retry button
  observeEvent(input$retryings, {
    withProgress(session, min = 1, max = 15, {
      setProgress(message = 'Resetting',
                  detail = '')
      for (i in 1:13) {
        setProgress(value = i)
        Sys.sleep(0.05)
      }
    })
  })
  
  
  # hide reset button upon opening app
  hide("redos")
  hide("cors")
  hide("wros")
  
  
  output$cors <- renderUI({
    tags$img(src = "correct.png", width = 30)
  })
  
  output$wros <- renderUI({
    tags$img(src = "incorrect.png", width = 30)
  })
  
  
  # show reset button after submit is clicked, disable dropdown inputs
  observeEvent(input$submitteds,{
    toggle("redos")
    disable("userOpJ")
    disable("userOpK")
    disable("userOpL")
    disable("userOpM")
    disable("submitteds")
    if(input$userOpC == 'Paycheck' & input$userOpD == 'Dollars') {
      showElement("cors")
    }
    else{
      showElement("wros")
    }
    
  })
  
  
  observeEvent(input$retryings,{
    hide("redos")
    enable("userOpC")
    enable("userOpD")
    showElement("submitteds")
    enable("submitteds")
    hide("cors")
    hide("wros")
    
  })

  
  ##  Live Code ----
  runButtonWasPressed <<- F #Used to stop error in the knitted output
  disable("submit")
  
  # question bank 
  somethingIsSelected <<- F
  value <- reactiveValues(index =  1, mistake = 0, correct = 0)
  ans <- as.matrix(bank[1:9, 6])
  index_list <- reactiveValues(list = sample(2:9, 8, replace = FALSE))
  
  observeEvent(input$nextq,{
    
    value$answerbox <- value$index
    #Removes the value in the front of the list
    index_list$list = index_list$list[-1]
    
    
    value$index <- index_list$list[1]
    value$answerbox <- value$index
    output$mark <- renderUI({
      img(src = NULL, width = 30)
    })
    somethingIsSelected <<- F
    
    disable("submit")
    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session, "submit", disabled = FALSE)
    
    withBusyIndicatorServer("eval", {
      output$knitDoc <- renderUI({
        return(isolate(HTML(knit2html(text = "Click \"Run\" to test the code", template = FALSE, quiet = TRUE))))
      })
      
      
    })
  })
  
  output$question <- renderUI({#ETHAN radio numbers
    #h4(bank[value$index, 2])
    radioButtons(inputId = 'answer', label=bank[value$index, 2], 
                 choiceNames=list(bank[value$index, 3], bank[value$index, 4], bank[value$index, 5]),
                 choiceValues = list("A","B","C"), selected = character(0))
    
    })
  
  
  ## change table based on question 
  output$acetable <- renderTable({
    #new
    if(value$index == 2 || value$index == 3)
    {
      race
    }
    else if(value$index == 1)
    {
      
    }
    else if(value$index == 4 || value$index == 5)
    {
      results
    }
    else if(value$index == 6 || value$index == 7)
    {
      grades
    }
    else
    {
      table5
    }
  })
  
  output$tableinfo <- renderUI({
    # race data info
    if (bank[value$index, 2] == bank[2, 2] | bank[value$index, 2] == bank[3, 2]) {
      tags$h4('This table depicts times and scores on a running race.')
      #tags$h4('Column names define different lengths of time')
      #tags$h4('Cell values are scores associated with each name and length of time')
      
      # results data info
    } else if (bank[value$index, 2] == bank[4, 2] | bank[value$index, 2] == bank[5, 2]) {
      tags$li('This table depicts clinical trial data')
      #tags$li('Ind - individual participating in the experiment')
      #tags$li('Treatment - trial type (Treat or Cont)')
      #tags$li('value - result of experiment') 
      
      # grades data info
    } else if (bank[value$index, 2] == bank[6, 2] | bank[value$index, 2] == bank[7, 2]) {
      tags$li('This table depicts student test score data')
      tags$li('A tidy case is one individual during one quarter in a given year.')
      tags$li('Each test is unique and should be treated as two separate variables.')
      
      # table 5 data ifo
    } else if (bank[value$index, 2] == bank[8, 2] || bank[value$index, 2] == bank[9, 2]) {
      tags$li('This table shows the population and rate of different countries.')
    }
    
    
  })
  
  
  
  output$editor <- renderUI({
    aceEditor("rmd",
              mode = "markdown",
              if(value$index == 1) {
                value = 'Here you can test out the answer choices before choosing an answer!

Uncomment one line from each section at a time and hit "Run" to see its effect!

Note: If the code does not display/change data, it probably is not the correct answer.
              
There is no interactive R code for this question!'
              } else if (value$index == 2) {
                value = 'No interactive R code for this question!'
              }
              else if (value$index == 3) {
                value = '
```{r}


tidyRace <-
  race %>%
  # pivot_wider(names_from = "Time", values_from = "Score") %>%
  # pivot_longer(cols = c("50","100","150","200","250","300","350"), names_to = "Time", values_to = "Score") %>%
  # unite(col = New, "50", "100", "150","200", sep = "", remove = TRUE) %>%
  arrange(Name)

tidyRace
```
'
              }
              else if (value$index == 4) {
                value = 'No interactive R code for this question!'
              }
              else if (value$index == 5) {
                value = '```{r}


tidyResults <-
  results %>%
  # pivot_wider(names_from = "Treatment", values_from = "value")
  # pivot_longer(cols = c("Treat","Cont"), names_to = "Treatment", values_to = "value")
  # unite(col = new, Treatment, value, sep = "", remove = TRUE)

tidyResults                
```
                '
              }
              else if (value$index == 6) {
                value = 'No interactive R code for this question!'
              }
              else if (value$index == 7) {
                value = '```{r}



tidyGrades <-
  grades %>%
  # pivot_wider(names_from = "Test", values_from = "Score") %>%
  # pivot_longer(cols = c("Fall","Spring","Winter"), names_to = "Quarter", values_to = "Score") %>%
  # unite(col = new, Test, Year, sep = "", remove = TRUE)
tidyGrades
```
'
              }
              else if (value$index == 8) {
                value = '```{r}
library(tidyr)

tidyTable5 <-
  table5 %>%
  # pivot_wider(key = century, value = year)
  # pivot_longer(cols = c("1999","2000"), names_to = century, values_to = year)
  # unite(col = new, century, year, sep = "", remove = TRUE)
  
tidyTable5
```
'
              }
              else {
                value = '```{r}
library(tidyr)

nextStep <-
  table5 %>%
  # unite(col = new, century, year)
  # unite(col = new, century, year, sep = "")
  # unite(col = year, century, year)
                
nextStep
```
'
              }
    )
  })
  
  observeEvent(input$answer, {
    somethingIsSelected <<- T
    enable("submit")
  })
  
  # Once rLocker works properly this may be relevant. 
  # observeEvent(input$answer, {
  #   req(input$answer, input$answer !='')
  #   answer <- isolate(input$answer)
  #   # interacted_statement <- rLocker::createStatement(
  #   #   list(
  #   #     verb = list(
  #   #       display = "selected"),
  #   #     object = list(
  #   #       id = paste0(getCurrentAddress(session), "#", value$index),
  #   #       name = paste('Question', value$index),
  #   #       description = bank[value$index, 2]),
  #   #     result = list(
  #   #       success = any(answer == ans[value$index, 1]),
  #   #       response = paste(getResponseText(value$index, answer), 
  #   #                        as.character(Sys.time()))
  #   #     )
  #   #   )
  #   # )
  #   
  # 
  #   # Store statement in locker and return status
  #   #status <- rLocker::store(session, interacted_statement)
  #   
  #   #print(interacted_statement) # remove me
  #   #print(status) # remove me
  # })
  
  
  getResponseText <- function(index, answer){
    if(answer == 'A'){
      key = 3
    } else if(answer == 'B'){
      key = 4
    } else {
      key = 5
    }
    return(bank[index, key])
  }
  
  observeEvent(input$submit,{
    validate(
      need(input$answer != "", ""),
      errorClass = "inline"
    )
    validate(
      need(somethingIsSelected != F, ""),
      errorClass = "inline"
    )
    
    if(length(index_list$list) == 1){
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session,"submit", disabled = TRUE)
      updateButton(session, "reset", disabled = FALSE)
    }
    else{
      updateButton(session, "nextq", disabled = FALSE)
      updateButton(session,"submit", disabled = TRUE)
      updateButton(session, "reset", disabled = FALSE)
    }
    
    
    answer <- input$answer
    
    statement <- rLocker::createStatement(
      list(
        verb = list(
          display = "answered"
        ),
        object = list(id = paste0(getCurrentAddress(session), "#", value$index),
                      name = paste('Question', value$index),
                      description = bank[value$index, 2]),
        result = list(success = any(answer == ans[value$index, 1]),
                      response = paste(getResponseText(value$index, answer), 
                                       as.character(Sys.time()))
        )
      )
    )
    
    # Store statement in locker and return status
    status <- rLocker::store(session, statement)
    
    
    output$mark <- renderUI({
      if (any(answer == ans[value$index, 1])){
        img(src = "correct.png", width = 30)
      }
      else{
        ig <- img(src = "incorrect.png", width = 30)
        w <- paste("You picked", answer, ", The correct answer is", ans[value$index, 1])
        HTML(paste(ig, w), sep = ' ')
      }
    })
  })
  
  
  observeEvent(input$reset, { 
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session,"reset", disable = TRUE)
    
    #This is what randomly orders the list
    index_list$list <- c(1, sample(2:9, 8, replace = FALSE))
    
    #the first question will always be index 1. Then 2-9 random
    value$index <- 1
    value$answerbox = value$index
    ans <- as.matrix(bank[1:9, 6])
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
    #Sets the question up
    updateRadioButtons(session, "answer", "Another Question", 
                       choiceNames=list(bank[value$index, 3], bank[value$index, 4], bank[value$index, 5]),
                       choiceValues = list("A","B","C"), selected = character(0))
    disable("submit")
  })
  
  # Initialize Learning Locker connection
  connection <- rLocker::connect(session, list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",
    agent = rLocker::createAgent()
  ))
  
  # Setup demo app and user.
  currentUser <- 
    connection$agent
  
  if(connection$status != 200){
    warning(paste(connection$status, "\nTry checking your auth token.")) 
  }
  
  
  # Setup demo app and user.
  
  output$Previewcar<-
    renderTable({
      head(cars, 4)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')
  
  output$Previewtree<-
    renderTable({
      head(trees, 4)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')
  
  output$Previewiris<-
    renderTable({
      head(iris, 4)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')
  
  ### KNITR ----
  observeEvent(input$eval,{ 
    runButtonWasPressed <<- T
    withBusyIndicatorServer("eval", {
      output$knitDoc <- renderUI({
        return(isolate(HTML(knit2html(text = input$rmd, template = FALSE, quiet = TRUE))))
      })
      
      output$output <- renderPrint({
        return(isolate(eval(parse(text = input$code))))
      })  
    })
  })
  
  output$knitDoc <- renderUI({#Ethan
    if(runButtonWasPressed == F)
    {
      return(isolate(HTML(knit2html(text = "Select the \"Run\" button underneath 
                                    the test Your Answer to see the code output below", 
                                    template = FALSE, quiet = TRUE))))
    }
    else{
      return(isolate(HTML(knit2html(text = input$rmd, template = FALSE, quiet = TRUE))))
    }
  })
  
  output$output <- renderPrint({
    #input$eval
    return(isolate(eval(parse(text = input$code))))
  })
  
  # Combining Data Table ----
  disable("check1")
  disable("check2")
  disable("check3")
  disable("check4")
  disable("check5")
  disable("check6")
  observeEvent(input$check1, {
    validate(need(((input$cd1 != "")), ""))
    
    if (input$cd1 == 'left join') {
      #Ethan
      output$checkOrX1 <- renderUI(img(src = "correct.png",width=30))
    }
    else {
      output$checkOrX1 <- renderUI(img(src = "incorrect.png",width=30))
    }
  })
  observeEvent(input$check2, {
    validate(need(((input$cd2 != "")), ""))
    
    if (input$cd2 == 'inner join') {
      output$checkOrX2 <- renderUI(img(src = "correct.png",width=30))
    }
    else {
      output$checkOrX2 <- renderUI(img(src = "incorrect.png",width=30))
    }
  })
  observeEvent(input$check3, {
    validate(need(((input$cd3 != "")), ""))
    
    if (input$cd3 == 'anti join') {
      output$checkOrX3 <- renderUI(img(src = "correct.png",width=30))    }
    else {
      output$checkOrX3 <- renderUI(img(src = "incorrect.png",width=30))
    }
  })
  observeEvent(input$check4, {
    validate(need(((input$cd4 != "")), ""))
    
    if (input$cd4 == 'semi join') {
      output$checkOrX4 <- renderUI(img(src = "correct.png",width=30))    }
    else {
      output$checkOrX4 <- renderUI(img(src = "incorrect.png",width=30))
    }
  })
  observeEvent(input$check5, {
    validate(need(((input$cd5 != "")), ""))
    
    if (input$cd5 == 'full join') {
      output$checkOrX5 <- renderUI(img(src = "correct.png",width=30))    }
    else {
      output$checkOrX5 <- renderUI(img(src = "incorrect.png",width=30))
    }
  })
  observeEvent(input$check6, {
    validate(need(((input$cd6 != "")), ""))
    
    if (input$cd6 == 'right join') {
      output$checkOrX6 <- renderUI(img(src = "correct.png",width=30))    }
    else {
      output$checkOrX6 <- renderUI(img(src = "incorrect.png",width=30))
    }
  })
  
  observeEvent(input$cd1,{enable("check1")})
  observeEvent(input$cd2,{enable("check2")})
  observeEvent(input$cd3,{enable("check3")})
  observeEvent(input$cd4,{enable("check4")})
  observeEvent(input$cd5,{enable("check5")})
  observeEvent(input$cd6,{enable("check6")})
  
  #correct answer: A, C, D, B --- left/inner/full/right
  a <- data.frame("x1" = c("A","B","C"), "x2" = c("1","2","3"))
  b <- data.frame("x1" = c("A","B","D"), "x3" = c("T","F","T"))
  output$titleTableA <- renderTable({a})
  output$titleTableB <- renderTable({b})
  
  output$cdTable1 <- renderTable({
    
    dplyr::left_join(a, b, by = "x1")
    #Ethan
    
  })
  
  output$cdTable2 <- renderTable({
    dplyr::inner_join(a, b, by = "x1")
  })
  
  output$cdTable3 <- renderTable({
    dplyr::anti_join(a, b, by = "x1")
  })
  
  output$cdTable4 <- renderTable({
    dplyr::semi_join(a, b, by = "x1")
  })
  
  output$cdTable5 <- renderTable({
    dplyr::full_join(a, b, by = "x1")
  })
  
  output$cdTable6 <- renderTable({
    dplyr::right_join(a, b, by = "x1")
  })
  
  output$cdExp1 <- renderText ({
    if (input$cd1 == 'left join') {
      paste('Join matching rows from b to a.')
    }
    else if (input$cd1 == 'right join') {
      paste('Join matching rows from a to b.')
    }
    else if (input$cd1 == 'inner join') {
      paste('Join data. Retain only rows in both sets.')
    }
    else {
      paste('Join data. Retain all values, all rows.')
    }
  })
  
  output$cdExp2 <- renderText ({
    if (input$cd2 == 'left join') {
      paste('Join matching rows from b to a.')
    }
    else if (input$cd2 == 'right join') {
      paste('Join matching rows from a to b.')
    }
    else if (input$cd2 == 'inner join') {
      paste('Join data. Retain only rows in both sets.')
    }
    else {
      paste('Join data. Retain all values, all rows.')
    }
  })
  
  output$cdExp3 <- renderText ({
    if (input$cd3 == 'left join') {
      paste('Join matching rows from b to a.')
    }
    else if (input$cd3 == 'right join') {
      paste('Join matching rows from a to b.')
    }
    else if (input$cd3 == 'inner join') {
      paste('Join data. Retain only rows in both sets.')
    }
    else {
      paste('Join data. Retain all values, all rows.')
    }
  })
  
  output$cdExp4 <- renderText ({
    if (input$cd4 == 'left join') {
      paste('Join matching rows from b to a.')
    }
    else if (input$cd4 == 'right join') {
      paste('Join matching rows from a to b.')
    }
    else if (input$cd4 == 'inner join') {
      paste('Join data. Retain only rows in both sets.')
    }
    else {
      paste('Join data. Retain all values, all rows.')
    }
  })
  
})