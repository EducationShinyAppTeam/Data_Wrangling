## Challenge 3 (just in case)

RawData3 <- table3

output$original3 <- renderTable({
  RawData3
})

# specify outputs for every choice
output$userOutX <- renderTable({
  if (input$userOpX == 'year' & input$userOpY == 'year') {
    RawData3 %>%
      spread(key = "year", value = "year")
  }
  else if (input$userOpX == 'year' & input$userOpY == 'type') {
    RawData3 %>%
      spread(key = "year", value = "type")
  }
  else if (input$userOpX == 'year' & input$userOpY == 'country') {
    RawData3 %>%
      spread(key = "year", value = "country")
  }
  else if (input$userOpX == 'year' & input$userOpY == 'count') {
    RawData3 %>%
      spread(key = "year", value = "count")
  }
  else if (input$userOpX == 'type' & input$userOpY == 'year') {
    RawData3 %>%
      spread(key = "type", value = "year")
  }
  else if (input$userOpX == 'type' & input$userOpY == 'type') {
    RawData3 %>%
      spread(key = "type", value = "type")
  }
  else if (input$userOpX == 'type' & input$userOpY == 'country') {
    RawData3 %>%
      spread(key = "type", value = "country")
  }
  else if (input$userOpX == 'type' & input$userOpY == 'count') {
    RawData3 %>%
      spread(key = "type", value = "count")
  }
  else if (input$userOpX == 'country' & input$userOpY == 'year') {
    RawData3 %>%
      spread(key = "country", value = "year")
  }
  else if (input$userOpX == 'country' & input$userOpY == 'type') {
    RawData3 %>%
      spread(key = "country", value = "type")
  }
  else if (input$userOpX == 'country' & input$userOpY == 'country') {
    RawData3 %>%
      spread(key = "country", value = "country")
  }
  else if (input$userOpX == 'country' & input$userOpY == 'count') {
    RawData3 %>%
      spread(key = "country", value = "count")
  }
  else if (input$userOpX == 'count' & input$userOpY == 'year') {
    RawData3 %>%
      spread(key = "count", value = "year")
  }
  else if (input$userOpX == 'count' & input$userOpY == 'type') {
    RawData3 %>%
      spread(key = "count", value = "type")
  }
  else if (input$userOpX == 'count' & input$userOpY == 'country') {
    RawData3 %>%
      spread(key = "count", value = "country")
  }
  else {
    RawData3 %>%
      spread(key = "count", value = "count")
  }
})

# show code based on inputs
output$userOutY <- renderUI({
  tags$code('tidyr::spread(RawData3, key = "', input$userOpX, '", value = "', input$userOpY, '")' )
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
           label = "Try Again",
           icon("retweet"),
           size = "medium",
           style = 'success')
})

# hide reset button upon opening app
hide("restart")
hide("cort")
hide("rong")


output$cort <- renderUI({
  tags$img(src = "correct.gif", width = 200)
})

output$rong <- renderUI({
  tags$img(src = "try.gif", width = 200)
})


# show reset button after submit is clicked, disable dropdown inputs
observeEvent(input$submitting,{
  toggle("restart")
  disable("userOpX")
  disable("userOpY")
  disable("submitting")
  if(input$userOpX == 'type' & input$userOpY == 'count') {
    showElement("cort")
  }
  else{
    showElement("rong")
  }
  
  
})


observeEvent(input$retryy,{
  hide("restart")
  enable("userOpX")
  enable("userOpY")
  showElement("submitting")
  enable("submitting")
  hide("cort")
  hide("rong")
  
})

observeEvent(input$retryy,{
  reset("userOpX")
  reset("userOpY")
  showElement("submitting")
  enable("submit")
  
})