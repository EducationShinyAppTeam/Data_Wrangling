library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(shinyWidgets)
library(dplyr)
library(EDAWR)
library(mosaic)
library(plot3D)
library(plotly)
library(ggplot2)
library(ggmap)
library(tidyr)


rm(list = ls())

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
    updateTabItems(session, 'tabs', 'exp4')
  })
  
 ############## Tidy Data #################
 
   
 output$original1 <- renderTable({
   table4a
 })
  
  # specify outputs for every choice
  output$userOut1 <- renderTable({
    if (input$userOp1 == '1999' & input$userOp2 == '1999' 
        & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      table4a %>%
        gather(`1999`, `1999`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '1999'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      table4a %>%
        gather(`1999`, `1999`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      table4a %>%
        gather(`1999`, `1999`, key = "year", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      table4a %>%
        gather(`1999`, `1999`, key = "year", value = "year")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      table4a %>%
        gather(`1999`, `2000`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      table4a %>%
        gather(`1999`, `2000`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      table4a %>%
        gather(`1999`, `2000`, key = "year", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      table4a %>%
        gather(`1999`, `2000`, key = "year", value = "year")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '1999'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      table4a %>%
        gather(`2000`, `1999`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '1999'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      table4a %>%
        gather(`2000`, `1999`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      table4a %>%
        gather(`2000`, `1999`, key = "year", value = "cases")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      table4a %>%
        gather(`2000`, `1999`, key = "year", value = "year")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      table4a %>%
        gather(`2000`, `2000`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      table4a %>%
        gather(`2000`, `2000`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      table4a %>%
        gather(`2000`, `2000`, key = "year", value = "cases")
    }
    else {
      table4a %>%
        gather(`2000`, `2000`, key = "year", value = "year")
    }
  })
  
  
  output$change <- renderText({
    if (input$userOp1 == 'Argument 1' || input$userOp2 == 'Argument 2' || input$userOp3 == 'Argument 3' || input$userOp4 == 'Argument 4') {
      show("change")
    }
    else {
      hide("change")
    }
    paste0("Change the Arguments!")
  })
  
  
  # show code based on inputs
 output$userOut2 <- renderUI({
   tags$code('tidyr::gather("table4a",`', input$userOp1, '`,`', input$userOp2, '`,
             key = "', input$userOp3, '", value = "', input$userOp4, '")' )
 }) 
 
 
 #observeEvent(input$submit, {
  # withProgress(session, min = 1, max = 15, {
   #  setProgress(message = 'Checking Answer',
    #             detail = '')
     #for (i in 1:10) {
      # setProgress(value = i)
       #Sys.sleep(0.05)
  #   }
 #  })
# })
 
# op1save <- reactiveValues(input$userOp1 = NULL)
# op2save <- reactiveValues(input$userOp2 = NULL)
# op3save <- reactiveValues(input$userOp3 = NULL)
# op4save <- reactiveValues(input$userOp4 = NULL)
 
  # submit button
   output$sub <- renderUI({
     bsButton("submit",
              label = "Check Answer",
              icon("lightbulb"),
              size = "medium",
              style = 'success')
     })
   
  # correct/wrong gif 
onclick("sub", 
        output$sub <- renderUI({
          if(input$userOp1 == '1999' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
            tags$img(src = "correct.gif", width = 230)
            }
          else{
            tags$img(src = "try.gif", width = 200)
            }
          })
        )

# trying to use sweetalert
  #observeEvent(input$submit, {
  #  if (input$userOp1 == '1999' & input$userOp2 == '2000'
  #      & input$userOp3 == 'year' & input$userOp4 == 'cases') {
   #   sweetalert(imageUrl = 'correct.gif')
    #}
    
 #   else{
  #    sweetalert(imageUrl = 'try.gif')
   # }
  #})



    output$reset <- renderUI({
      bsButton("retry",
               label = "Click to Retry",
               icon("retweet"),
               size = "medium",
               style = 'success')
      })
    
    # hide reset button upon opening app
    hide("reset")
    
    # show reset button after submit is clicked, disable dropdown inputs
    observeEvent(input$submit,{
      toggle("reset")
      disable("userOp1")
      disable("userOp2")
      disable("userOp3")
      disable("userOp4")
    })


  observeEvent(input$retry,{
    hide("reset")
    enable("userOp1")
    enable("userOp2")
    enable("userOp3")
    enable("userOp4")
    
  })
  
  observeEvent(input$retry,{
    reset("userOp1")
    reset("userOp2")
    reset("userOp3")
    reset("userOp4")
    
  })
  
  ### Challenge 2 ###
  
  output$original2 <- renderTable({
    table4b
  })
  
  # specify outputs for every choice
  output$userOutA <- renderTable({
    if (input$userOpA == '1999' & input$userOpB == '1999' 
        & input$userOpC == 'population' & input$userOpD == 'population') {
      table4b %>%
        gather(`1999`, `1999`, key = "population", value = "population")
    }
    else if (input$userOpA == '1999' & input$userOpB == '1999'
             & input$userOpC == 'population' & input$userOpD == 'year') {
      table4b %>%
        gather(`1999`, `1999`, key = "population", value = "year")
    }
    else if (input$userOpA == '1999' & input$userOpB == '1999'
             & input$userOpC == 'year' & input$userOpD == 'population') {
      table4b %>%
        gather(`1999`, `1999`, key = "year", value = "population")
    }
    else if (input$userOpA == '1999' & input$userOpB == '1999'
             & input$userOpC == 'year' & input$userOpD == 'year') {
      table4b %>%
        gather(`1999`, `1999`, key = "year", value = "year")
    }
    else if (input$userOpA == '1999' & input$userOpB == '2000'
             & input$userOpC == 'population' & input$userOpD == 'population') {
      table4b %>%
        gather(`1999`, `2000`, key = "population", value = "population")
    }
    else if (input$userOpA == '1999' & input$userOpB == '2000'
             & input$userOpC == 'population' & input$userOpD == 'year') {
      table4b %>%
        gather(`1999`, `2000`, key = "population", value = "year")
    }
    else if (input$userOpA == '1999' & input$userOpB == '2000'
             & input$userOpC == 'year' & input$userOpD == 'population') {
      table4b %>%
        gather(`1999`, `2000`, key = "year", value = "population")
    }
    else if (input$userOpA == '1999' & input$userOpB == '2000'
             & input$userOpC == 'year' & input$userOpD == 'year') {
      table4b %>%
        gather(`1999`, `2000`, key = "year", value = "year")
    }
    else if (input$userOpA == '2000' & input$userOpB == '1999'
             & input$userOpC == 'population' & input$userOpD == 'population') {
      table4b %>%
        gather(`2000`, `1999`, key = "population", value = "population")
    }
    else if (input$userOpA == '2000' & input$userOpB == '1999'
             & input$userOpC == 'population' & input$userOpD == 'year') {
      table4b %>%
        gather(`2000`, `1999`, key = "population", value = "year")
    }
    else if (input$userOpA == '2000' & input$userOpB == '1999'
             & input$userOpC == 'year' & input$userOpD == 'population') {
      table4b %>%
        gather(`2000`, `1999`, key = "year", value = "population")
    }
    else if (input$userOpA == '2000' & input$userOpB == '1999'
             & input$userOpC == 'year' & input$userOpD == 'year') {
      table4b %>%
        gather(`2000`, `1999`, key = "year", value = "year")
    }
    else if (input$userOpA == '2000' & input$userOpB == '2000'
             & input$userOpC == 'population' & input$userOpD == 'population') {
      table4b %>%
        gather(`2000`, `2000`, key = "population", value = "population")
    }
    else if (input$userOpA == '2000' & input$userOpB == '2000'
             & input$userOpC == 'population' & input$userOpD == 'year') {
      table4b %>%
        gather(`2000`, `2000`, key = "population", value = "year")
    }
    else if (input$userOpA == '2000' & input$userOpB == '2000'
             & input$userOpC == 'year' & input$userOpD == 'population') {
      table4b %>%
        gather(`2000`, `2000`, key = "year", value = "population")
    }
    else {
      table4b %>%
        gather(`2000`, `2000`, key = "year", value = "year")
    }
  })
  
  # show code based on inputs
  output$userOutB <- renderUI({
    tags$code('tidyr::gather("table4b",`', input$userOpA, '`,`', input$userOpB, '`,
              key = "', input$userOpC, '", value = "', input$userOpD, '")' )
  }) 
  
  
  #observeEvent(input$submit, {
  # withProgress(session, min = 1, max = 15, {
  #  setProgress(message = 'Checking Answer',
  #             detail = '')
  #for (i in 1:10) {
  # setProgress(value = i)
  #Sys.sleep(0.05)
  #   }
  #  })
  # })
  
  # op1save <- reactiveValues(input$userOpA = NULL)
  # op2save <- reactiveValues(input$userOpB = NULL)
  # op3save <- reactiveValues(input$userOpC = NULL)
  # op4save <- reactiveValues(input$userOpD = NULL)
  
  # submit button
  output$bus <- renderUI({
    bsButton("submitted",
             label = "Check Answer",
             icon("lightbulb"),
             size = "medium",
             style = 'success')
  })
  
  # correct/wrong gif 
  onclick("bus", 
          output$bus <- renderUI({
            if(input$userOpA == '1999' & input$userOpB == '2000'
               & input$userOpC == 'year' & input$userOpD == 'population') {
              tags$img(src = "correct.gif", width = 230)
            }
            else{
              tags$img(src = "try.gif", width = 200)
            }
          })
  )
  
  # trying to use sweetalert
  #observeEvent(input$submit, {
  #  if (input$userOpA == '1999' & input$userOpB == '2000'
  #      & input$userOpC == 'year' & input$userOpD == 'population') {
  #   sweetalert(imageUrl = 'correct.gif')
  #}
  
  #   else{
  #    sweetalert(imageUrl = 'try.gif')
  # }
  #})
  
  
  
  output$redo <- renderUI({
    bsButton("retrying",
             label = "Click to Retry",
             icon("retweet"),
             size = "medium",
             style = 'success')
  })
  
  # hide reset button upon opening app
  hide("redo")
  
  # show reset button after submit is clicked, disable dropdown inputs
  observeEvent(input$submitted,{
    toggle("redo")
    disable("userOpA")
    disable("userOpB")
    disable("userOpC")
    disable("userOpD")
  })
  
  
  observeEvent(input$retrying,{
    hide("redo")
    enable("userOpA")
    enable("userOpB")
    enable("userOpC")
    enable("userOpD")
    
  })
  
  observeEvent(input$retrying,{
    reset("userOpA")
    reset("userOpB")
    reset("userOpC")
    reset("userOpD")
    
  })
  

### Challenge 3 ###
  
  output$original3 <- renderTable({
    table2
  })
  
  # specify outputs for every choice
  output$userOutX <- renderTable({
    if (input$userOpX == 'year' & input$userOpY == 'year') {
      table2 %>%
        spread(key = "year", value = "year")
    }
    else if (input$userOpX == 'year' & input$userOpY == 'type') {
      table2 %>%
        spread(key = "year", value = "type")
    }
    else if (input$userOpX == 'year' & input$userOpY == 'country') {
      table2 %>%
        spread(key = "year", value = "country")
    }
    else if (input$userOpX == 'year' & input$userOpY == 'count') {
      table2 %>%
        spread(key = "year", value = "count")
    }
    else if (input$userOpX == 'type' & input$userOpY == 'year') {
      table2 %>%
        spread(key = "type", value = "year")
    }
    else if (input$userOpX == 'type' & input$userOpY == 'type') {
      table2 %>%
        spread(key = "type", value = "type")
    }
    else if (input$userOpX == 'type' & input$userOpY == 'country') {
      table2 %>%
        spread(key = "type", value = "country")
    }
    else if (input$userOpX == 'type' & input$userOpY == 'count') {
      table2 %>%
        spread(key = "type", value = "count")
    }
    else if (input$userOpX == 'country' & input$userOpY == 'year') {
      table2 %>%
        spread(key = "country", value = "year")
    }
    else if (input$userOpX == 'country' & input$userOpY == 'type') {
      table2 %>%
        spread(key = "country", value = "type")
    }
    else if (input$userOpX == 'country' & input$userOpY == 'country') {
      table2 %>%
        spread(key = "country", value = "country")
    }
    else if (input$userOpX == 'country' & input$userOpY == 'count') {
      table2 %>%
        spread(key = "country", value = "count")
    }
    else if (input$userOpX == 'count' & input$userOpY == 'year') {
      table2 %>%
        spread(key = "count", value = "year")
    }
    else if (input$userOpX == 'count' & input$userOpY == 'type') {
      table2 %>%
        spread(key = "count", value = "type")
    }
    else if (input$userOpX == 'count' & input$userOpY == 'country') {
      table2 %>%
        spread(key = "count", value = "country")
    }
    else {
      table2 %>%
        spread(key = "count", value = "count")
    }
  })
  
  # show code based on inputs
  output$userOutY <- renderUI({
    tags$code('tidyr::spread("table2", key = "', input$userOpX, '", value = "', input$userOpY, '")' )
  }) 
  
  
  #observeEvent(input$submit, {
  # withProgress(session, min = 1, max = 15, {
  #  setProgress(message = 'Checking Answer',
  #             detail = '')
  #for (i in 1:10) {
  # setProgress(value = i)
  #Sys.sleep(0.05)
  #   }
  #  })
  # })
  
  # op1save <- reactiveValues(input$userOpX = NULL)
  # op2save <- reactiveValues(input$userOpY = NULL)
  # op3save <- reactiveValues(input$userOp3 = NULL)
  # op4save <- reactiveValues(input$userOp4 = NULL)
  
  # submit button
  output$subbed <- renderUI({
    bsButton("submitting",
             label = "Check Answer",
             icon("lightbulb"),
             size = "medium",
             style = 'success')
  })
  
  # correct/wrong gif 
  onclick("subbed", 
          output$subbed <- renderUI({
            if(input$userOpX == 'type' & input$userOpY == 'count') {
              tags$img(src = "correct.gif", width = 230)
            }
            else{
              tags$img(src = "try.gif", width = 200)
            }
          })
  )
  
  # trying to use sweetalert
  #observeEvent(input$submit, {
  #  if (input$userOpX == '1999' & input$userOpY == '2000'
  #      & input$userOp3 == 'year' & input$userOp4 == 'type') {
  #   sweetalert(imageUrl = 'correct.gif')
  #}
  
  #   else{
  #    sweetalert(imageUrl = 'try.gif')
  # }
  #})
  
  
  
  output$restart <- renderUI({
    bsButton("retryy",
             label = "Click to Retry",
             icon("retweet"),
             size = "medium",
             style = 'success')
  })
  
  # hide reset button upon opening app
  hide("restart")
  
  # show reset button after submit is clicked, disable dropdown inputs
  observeEvent(input$submitting,{
    toggle("restart")
    disable("userOpX")
    disable("userOpY")
  })
  
  
  observeEvent(input$retryy,{
    hide("restart")
    enable("userOpX")
    enable("userOpY")

  })
  
  observeEvent(input$retryy,{
    reset("userOpX")
    reset("userOpY")

  })
  
  
 
  
  
  
  
  
############ Reshaping Data ############
  # observeEvent(input$knob1, {
  #   updateKnobInput(session, inputId = 'knob2', label = 'Select the Maximum Value for the First Column', value = input$knob1)
  # })
  # 
  # observe(updateKnobInput(session, inputId = 'knob4', value = input$knob3 + input$knob2 - input$knob1))
  
  # unite
  output$uniteUI <- renderUI ({
    if (input$unite3 == T) {
      tags$code('R code: tidyr::unite(mtcars, "New_Column_Name", c(input$unite1))') 
    }
  })
  
  output$uniteOutput1 <- renderTable ({
    head(mtcars)
  })
  
  output$uniteOutput2 <- renderTable ({
    if (input$unite3 == T) {
      head(tidyr::unite(mtcars, 'New_Column', c(input$unite1)))
    }
  })
  
  # arrange: default setting is low to high
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
      tags$code(paste('Code: dplyr::arrange(mtcars, mtcars[ , ', input$dwSTI1, ']'))
    }
  })
  
  output$code2 <- renderUI ({
    if (input$dwSTI2 == 'High to Low') {
      tags$code(paste('Code: dplyr::arrange(mtcars, descmtcars[ , ', input$dwSTI1, '])'))
    }
  })
  
  # #data frame
  # output$dfCode <- renderUI ({
  #   tags$code('dplyr::data_frame(a =', input$knob1, ':', input$knob2, ', b =', input$knob3, ':', input$knob4, ')')
  # })
  # 
  # output$dwTable7 <- renderDataTable({
  #   dplyr::data_frame(a = input$knob1:input$knob2, b = input$knob3:input$knob4)
  # })
  
  #gather
  output$dwTable1 <- renderTable({
    cases
  })
  
  output$dwTable2 <- renderTable({
    if (input$dw1 == TRUE) {
      tidyr::gather(cases, "year", "n", 2:4)
    }
  })
  
  #spread
  output$dwTable5 <- renderTable({
    pollution
  })
  
  output$dwTable6 <- renderTable({
    if (input$dw3 == TRUE) {
      tidyr::spread(pollution, size, amount)
    }
  })
  
############
  observeEvent(input$check1, {
    if (input$cd1 == 'left join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check2, {
    if (input$cd2 == 'inner join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check3, {
    if (input$cd3 == 'full join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check4, {
    if (input$cd4 == 'right join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check5, {
    if (input$cd5 == 'anti join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check6, {
    if (input$cd6 == 'semi join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  
  #correct answer: A, C, D, B --- left/inner/full/right
  output$cdTable1 <- renderTable({
    dplyr::left_join(a, b, by = "x1")
  })
  
  output$cdTable2 <- renderTable({
    dplyr::inner_join(a, b, by = "x1")
  })
  
  output$cdTable3 <- renderTable({
    dplyr::full_join(a, b, by = "x1")
  })
  
  output$cdTable4 <- renderTable({
    dplyr::right_join(a, b, by = "x1")
  })
  
  output$cdTable5 <- renderTable({
    dplyr::anti_join(a, b, by = "x1")
  })
  
  output$cdTable6 <- renderTable({
    dplyr::semi_join(a, b, by = "x1")
  })
  
  #cd Exp 1-6
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
  
############ Data Visualization ############
  
  ###### Maps ######
  #a. usMap
  output$usMapOut1 <- renderPlot({
    USArrests2 <- USArrests %>% mutate(state = row.names(.))
    if (input$usMap1 == 'borders' & input$usMap2 == 'compact') {
      mUSMap(USArrests2, key = "state", fill = "UrbanPop", plot = 'borders', style = 'compact')
    }
    else if (input$usMap1 == 'borders' & input$usMap2 == 'real') {
      mUSMap(USArrests2, key = "state", fill = "UrbanPop", plot = 'borders', style = 'real')
    }
    else if (input$usMap1 == 'frame' & input$usMap2 == 'compact') {
      mUSMap(USArrests2, key = "state", fill = "UrbanPop", plot = 'frame', style = 'compact')
    }
    else {
      mUSMap(USArrests2, key = "state", fill = "UrbanPop", plot = 'frame', style = 'real')
    }
  })
  
  output$usMapOut2 <- renderUI ({
    tags$code('mUSMap(USArrests2, key = "state", fill = "UrbanPop", plot = "', input$usMap1, '", style = "', input$usMap2, '")')
  })
  
  #plotly US Map - code
  output$plotlyUScode <- renderUI ({
    tags$code('p <- plot_geo(df, locationmode = "USA-states", sizes = c(1, 250))')
  })
  
  #plotly US Map
  output$plotlyUSMap <- renderPlotly({
    df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
    df$q <- with(df, cut(pop, quantile(pop)))
    levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
    df$q <- as.ordered(df$q)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray85"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white")
    )
    
    p <- plot_geo(df, locationmode = 'USA-states', sizes = c(1, 250)) %>%
      add_markers(
        x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
        text = ~paste(df$name, "<br />", df$pop/1e6, " million")
      ) %>%
      layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)
    g
    p
  })
  
  # #b. worldMap
  # output$worldMapOut1 <- renderPlot ({
  #   gdpData <- gdpData %>% mutate(GDPOption = ntiles(-GDP, input$worldMap1, format = "rank"))
  #   mWorldMap(gdpData, key = "country", fill = "GDPOption")
  # })
  # 
  # output$worldMapCode1 <- renderUI ({
  #   tags$code('gdpData <- gdpData %>% mutate(GDPOption = ntiles(-GDP,', input$worldMap1, ', format = "rank"))')
  # })
  # 
  # output$worldMapCode2 <- renderUI ({
  #   tags$code('mWorldMap(gdpData, key = "country", fill = "GDPOption")')
  # })
  
  ###### 3D Plots ######
  #a. Normal Simulation via Plotly
  output$plotly1 <- renderPlotly ({
    plot_ly(x = rnorm(input$Exsel1), y = rnorm(input$Exsel1), z = rnorm(input$Exsel1), 
            type = 'scatter3d', mode = 'markers')
  })
  
  output$ExCode <- renderUI ({
    tags$code('plot_ly(x = rnorm(', input$Exsel1, '), y = rnorm(', input$Exsel1, '), z = rnorm(', input$Exsel1, '), type = "scatter3d", mode = "markers")')
  })
  
  # output$plotly1 <- renderPlotly ({
  #   if (input$Exsel == 'Scatter Plot') {
  #     plot_ly(x = rnorm(input$Exsel1), y = rnorm(input$Exsel1), z = rnorm(input$Exsel1), 
  #             type = 'scatter3d', mode = 'markers')
  #   }
  #   else if (input$Exsel == 'Line Plot') {
  #     plot_ly(x = rnorm(input$Exsel1), y = rnorm(input$Exsel1), z = rnorm(input$Exsel1), 
  #             type = 'scatter3d', mode = 'lines') 
  #   }
  #   else {
  #     plot_ly(x = rnorm(input$Exsel1), y = rnorm(input$Exsel1), z = rnorm(input$Exsel1), 
  #             type = 'mesh3d', mode = 'markers') 
  #   }
  # })
  
  output$hover <- renderPrint({
    dataHover <- event_data("plotly_hover")
    if (is.null(dataHover)) {
      "Hover events appear here (unhover to clear)" 
    }
    else {
      dataHover
    }
  })
  
  output$click <- renderPrint({
    dataClick <- event_data("plotly_click")
    if (is.null(dataClick)) {
      "Click events appear here (double-click to clear)"
    }
    else {
      dataClick
    }
  })
  
  # output$ExCode <- renderUI ({
  #   if (input$Exsel == 'Scatter Plot') {
  #     tags$code('plot_ly(x = rnorm(', input$Exsel1, '), y = rnorm(', input$Exsel1, '), z = rnorm(', input$Exsel1, '), type = "scatter3d", mode = "markers")')
  #   }
  #   else if (input$Exsel == 'Line Plot') {
  #     tags$code('plot_ly(x = rnorm(', input$Exsel1, '), y = rnorm(', input$Exsel1, '), z = rnorm(', input$Exsel1, '), type = "scatter3d", mode = "lines")')
  #   }
  #   else {
  #     tags$code('plot_ly(x = rnorm(', input$Exsel1, '), y = rnorm(', input$Exsel1, '), z = rnorm(', input$Exsel1, '), type = "mesh3d", mode = "markers")')
  #   }
  # })
  
  #b. Basic Scatter Plot
  output$basicRcode <- renderUI ({
    tags$code('scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"), xlab = input$basicX, ylab = input$basicY, zlab = input$basicZ)')
  })
  
  output$bspTable <- renderTable ({
    head(iris)
  })
  
  output$bspOut1 <- renderPlot({
    x <- iris[, input$basicX]
    y <- iris[, input$basicY]
    z <- iris[, input$basicZ]
    scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"), xlab = input$basicX, ylab = input$basicY, zlab = input$basicZ)
  })
  
  # #c.
  # output$bspTableCopy <- renderTable ({
  #   head(iris)
  # })
  # 
  # output$bspOut2 <- renderPlot ({
  #   scatter3D(x, y, z, bty = "g", pch = 18, 
  #             col.var = as.integer(iris$Species), 
  #             col = c("#1B9E77", "#D95F02", "#7570B3"),
  #             pch = 18, ticktype = "detailed",
  #             colkey = list(at = c(2, 3, 4), side = 1, 
  #                           addlines = TRUE, length = 0.5, width = 0.5,
  #                           labels = c("setosa", "versicolor", "virginica")) )
  # })
  
  # #d. 3D Plots with Confidence Intervals
  # output$CIOut <- renderPlot ({
  #   x <- iris[, input$CIX]
  #   y <- iris[, input$CIY]
  #   z <- iris[, input$CIZ]
  #   CI <- list(z = matrix(nrow = length(x),
  #                         data = rep(0.1, 2*length(x))))
  #   scatter3D(x, y, z, phi = 0, bty = "g", col = gg.col(100), 
  #             pch = 18, CI = CI)
  # })
  
  #e. 3D Texts Plot
  output$textRcode <- renderUI ({
    tags$code('with(USArrests, text3D(Murder, Assault, Rape, 
                           labels = rownames(USArrests), colvar = UrbanPop, 
              col = gg.col(100), theta = 60, phi = 20,
              xlab = "Murder", ylab = "Assault", zlab = "Rape", 
              main = "USA arrests", cex = 0.6, 
              bty = "g", ticktype = "detailed", d = 2,
              clab = c("Urban","Pop"), adj = 0.5, font = 2))')
  })
  
  output$textTable <- renderTable ({
    head(USArrests)
  })
  
  output$textOut <- renderPlot ({
    data(USArrests)
    with(USArrests, text3D(Murder, Assault, Rape, 
                           labels = rownames(USArrests), colvar = UrbanPop, 
                           col = gg.col(100), theta = 60, phi = 20,
                           xlab = "Murder", ylab = "Assault", zlab = "Rape", 
                           main = "USA arrests", cex = 0.6, 
                           bty = "g", ticktype = "detailed", d = 2,
                           clab = c("Urban","Pop"), adj = 0.5, font = 2))
  })
  
  ###### 2D Line Plots ######
  output$plotly2 <- renderPlotly ({
    trace_0 <- rnorm(as.numeric(input$LPsel1), mean = as.numeric(input$LPnum1))
    trace_1 <- rnorm(as.numeric(input$LPsel1), mean = as.numeric(input$LPnum2))
    x = c(1:as.numeric(input$LPsel1))
    data <- data.frame(x, trace_0, trace_1)
    if (input$LPSEL1 == 'Lines' & input$LPSEL2 == 'Lines') {
      plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines')
    }
    else if (input$LPSEL1 == 'Markers' & input$LPSEL2 == 'Markers') {
      plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'markers') %>%
        add_trace(y = ~trace_1, name = 'trace 1', mode = 'markers')
    }
    else if (input$LPSEL1 == 'Lines' & input$LPSEL2 == 'Markers') {
      plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~trace_1, name = 'trace 1', mode = 'markers')
    }
    else {
      plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'markers') %>%
        add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines')
    }
  })
  
  output$LPCode <- renderUI ({
    tags$code('plot_ly(data, x = ~x, y = ~trace_0, name = "trace 0", type = "scatter", mode = "lines") %>%
              add_trace(y = ~trace_1, name = "trace 1", mode = "markers + lines")')
  })
  
  ###### Contour Plots and Heatmaps ######
  #contour plot
  output$proteinInt <- renderPlot ({
    potentials <- as.matrix(read.table("MULTIPOT_lu.txt", row.names=1, header=TRUE))
    matrix.axes <- function(data) {
      # Do the rows, las=2 for text perpendicular to the axis
      x <- (1:dim(data)[1] - 1) / (dim(data)[1] - 1);
      axis(side=1, at=x, labels=rownames(data), las=2);
      # Do the columns
      x <- (1:dim(data)[2] - 1) / (dim(data)[2] - 1);
      axis(side=2, at=x, labels=colnames(data), las=2);
    }
    filled.contour(potentials, plot.axes=matrix.axes(potentials), main = "Protein-Protein Interaction Potential")
  })
  
  output$plotly3 <- renderPlotly({
    if (input$contourLabel == FALSE) {
      plot_ly(z = volcano, type = "contour", colors = colorRamp(c("purple", "green")))
    }
    else {
      plot_ly(z = volcano, type = "contour", colors = colorRamp(c("purple", "green")),
              contours = list(showlabels = TRUE))
    }
  })
  
  #contour plot r code
  output$CPCode1 <- renderUI ({
    if (input$contourLabel == FALSE) {
      tags$code('plot_ly(z = volcano, type = "contour", colors = colorRamp(c("purple", "green")))')
    }
    else {
      tags$code('plot_ly(z = volcano, type = "contour", colors = colorRamp(c("purple", "green")), contours = list(showlabels = TRUE))')
    }
  })
  
  #heatmap
  output$plotly4 <- renderPlotly({
    if (input$heatmapCol == 'purple+green') {
      plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("purple", "green")))
    }
    else if (input$heatmapCol == 'yellow+red') {
      plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("yellow", "red")))
    }
    else if (input$heatmapCol == 'pink+purple') {
      plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("pink", "purple")))
    }
    else {
      plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("white", "black")))
    }
  })
  
  #heatmaps r code
  output$CPCode2 <- renderUI ({
    if (input$heatmapCol == 'purple+green') {
      tags$code('plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("purple", "green")))') 
    }
    else if (input$heatmapCol == 'yellow+red') {
      tags$code('plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("yellow", "red")))') 
    }
    else if (input$heatmapCol == 'pink+purple') {
      tags$code('plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("pink", "purple")))') 
    }
    else {
      tags$code('plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("white", "black")))') 
    }
  })
  
  output$cars1 <- renderPlotly ({
    head(mtcars)
    data = as.matrix(mtcars)
    data=apply(data, 2, function(x){x/mean(x)})
    plot_ly(x=colnames(data), y=rownames(data), z = data, type = "heatmap")
  })
  
})