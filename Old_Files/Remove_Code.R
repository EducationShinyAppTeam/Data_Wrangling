# Deleted Code from preamble ----
# library(shinyAce)
# library(knitr)
# library(datasets)
# library(rmarkdown)
# library(learnr)
# library(shinycssloaders)
# library(shinyjs)

# devtools::install_github("rstudio/EDAWR")
## used for the pollution data
# library(EDAWR)

# Load additional dependencies and setup functions ----
# source("helpers.R")
# bank <- read.table(file = "questionbank.csv", header = TRUE, sep = ",")

# Deleted Code from UI ----


# menuItem("Tidy Data Challenge", tabName = "tidy", icon = icon("gears")),
# menuItem('Combining Data Challenge', tabName = "comb", icon = icon("gears")),
# tags$li(
#   "When you're ready, check out the Tidy Data Challenge, to put your
#   understandings to the test by writting code to tidy some data."
# ),
# tags$li(
#   "When you're ready, check out the Combining Data Challenge, to put
#   your understandings of wrangling verbs to the test."
# )

# ### Tidy Challenge Page ----
# tabItem(
#   tabName = "tidy",
#   withMathJax(),
#   h2("Wizard"),
#   tabsetPanel(type = 'tabs', id = 'questionTabs',
#               tabPanel(div(style = 'font-size: 125%', 'pivot_longer 1'), value = "pivot_longer1",
#
#                        ## pivot_longer 1 ----
#                        box(
#                          wellPanel(div(style = 'text-align: left; font-size: 100%; display: inline-block',
#
#
#                                        # The statement in the top box
#                                        tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be a country in a year'),
#                                        tags$code('tidyr::pivot_longer(RawData, cols = c("Arg 1", "Arg 2"), names_to = "Arg 3", values_to = "Arg 4")'))),
#
#                          textOutput("tester"),
#                          br(),
#
#                          column(12,
#                                 box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
#                                         (title = 'RawData')),
#                                     style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
#                                     uiOutput("original1"))),
#                          br(),
#                          br(),
#                          br(),
#
#                          # choices for user plot
#                          fluidRow(
#
#                            box(div(style = 'background-color: #ffffff',
#                                    (title = '')),
#                                style = 'background: #ffffff',
#                                width = 3,
#                                selectInput(inputId = 'userOp1',
#                                            label = 'cols[1]=',
#                                            choices = c('country', '1999', 'Afghanistan', '2000'),
#                                            selected = 'country')),
#                            box(div(style = 'background-color: #ffffff',
#                                    (title = '')),
#                                style = 'background: #ffffff',
#                                width = 3,
#                                selectInput(inputId = 'userOp2',
#                                            label = 'cols[2]=',
#                                            choices = c('country', '1999', 'Afghanistan', '2000'),
#                                            selected = 'country')),
#
#                            box(div(style = 'background-color: #ffffff',
#                                    (title = '')),
#                                style = 'background: #ffffff',
#                                width = 3,
#                                selectInput(inputId = 'userOp3',
#                                            label = 'names_to = ',
#                                            choices = c('1999', 'cases', 'year', 'China'),
#                                            selected = '1999')),
#
#
#                            box(div(style = 'background-color: #ffffff',
#                                    (title = '')),
#                                style = 'background-color: #ffffff',
#                                width = 3,
#                                selectInput(inputId = 'userOp4',
#                                            label = 'values_to = ',
#                                            choices = c('1999', 'cases', 'year', 'China'),
#                                            selected = '1999')
#                            )),
#                          # Your R Code based on input
#                          fluidRow(
#                            column(12,
#                                   div(style = 'text-align: left; font-size: 100%',
#                                       wellPanel(tags$strong('Your R code: '),
#                                                 uiOutput('tidyAttemptTable'))))),
#                          br(),
#                          box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 105%; font-weight: bold',
#                                  (title = 'Your Tidy Attempt')),
#                              style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
#                              width = 6,
#                              uiOutput("userOut1"),
#                              div(style = 'font-size: 100%; font-weight: bold')),
#                          width = 10),
#
#
#                        fluidRow(
#                          div(style = 'display: inline-block; position: relative; text-align: right',
#                              uiOutput('correct'),
#                              uiOutput('wrong')),
#
#
#                          br(),
#                          br(),
#
#                          div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
#                              uiOutput("sub"),
#                              br(),
#                              uiOutput("resetcc"),
#                          ),
#                        ),
#
#               ),
#
#               ## pivot_longer 2 ----
#
#               tabPanel(div(style = 'font-size: 125%', 'pivot_longer 2'), value = "pivot_longer2",
#
#                        # Fill in the correct Argument across top
#                        box(wellPanel(div(style = 'text-align: left; font-size: 100%; display: inline-block',
#                                          tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be an employee on a certain day of the week'),
#                                          tags$code('tidyr::pivot_longer(RawData2, cols = c("Arg 1", "Arg 2", "Arg 3"), names_to = "Arg 4", values_to = "Arg 5")'))),
#                            # Raw Data Table
#
#
#                            column(12,
#                                   box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
#                                           (title = 'RawData2')),
#                                       style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
#                                       uiOutput("original3"))),
#
#                            br(),
#                            # The 5 Input values
#                            fluidRow(
#                              # choices for user plot
#                              box(div(style = 'background-color: #ffffff',
#                                      (title = '')),
#                                  style = 'background: #ffffff',
#                                  width = 3,
#                                  selectInput(inputId = 'userOp5',
#                                              label = 'cols[1] =',
#                                              choices = c('John','MonTips','Age','Name'),
#                                              selected = 'John')),
#                              box(div(style = 'background-color: #ffffff',
#                                      (title = '')),
#                                  style = 'background: #ffffff',
#                                  width = 3,
#                                  selectInput(inputId = 'userOp6',
#                                              label = 'cols[2] =',
#                                              choices = c('Tim','Age','TueTips','21'),
#                                              selected = 'Tim')),
#                              box(div(style = 'background-color: #ffffff',
#                                      (title = '')),
#                                  style = 'background: #ffffff',
#                                  width = 3,
#                                  selectInput(inputId = 'userOp7',
#                                              label = 'cols[3] =',
#                                              choices = c('Age', 'Name', '7', 'WedTips'),
#                                              selected = 'Age')),
#                              box(div(style = 'background-color: #ffffff',
#                                      (title = '')),
#                                  style = 'background: #ffffff',
#                                  width = 3,
#                                  selectInput(inputId = 'userOp8',
#                                              label = 'names_to =',
#                                              choices = c('21','MonTips','Tips','Day'),
#                                              selected = '21')),
#
#
#                              box(div(style = 'background-color: #ffffff',
#                                      (title = '')),
#                                  style = 'background-color: #ffffff',
#                                  width = 3,
#                                  selectInput(inputId = 'userOp9',
#                                              label = 'values_to',
#                                              choices = c('8','Age','Day','Tips'),
#                                              selected = '8')
#                              )
#                            ),
#
#                            # Tidy Attempt
#                            fluidRow(
#                              column(12,
#                                     div(style = 'text-align: left; font-size: 100%',
#                                         wellPanel(tags$strong('Your R code: '),
#                                                   uiOutput('userOutY'))
#                                     ))),
#
#                            box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
#                                    (title = 'Your Tidy Attempt')),
#                                style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
#                                width = 6,
#                                uiOutput("tidyAttempt2")),
#                            width = 10),
#
#                        fluidRow(
#                          div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
#                              uiOutput("subbed"),
#                              br(),
#                              uiOutput("restart"),
#                          ),
#                          br(),
#                          br(),
#                          div(style = 'display: inline-block; position: relative; text-align: right',
#                              uiOutput('cort'),
#                              uiOutput('rong')))
#               ),
#
#               ## Pivot Wider 1  ----
#
#               tabPanel(div(style = 'font-size: 125%', 'pivot_wider 1'), value = "pivot_wider1",
#
#                        #  Fill in sample argument
#                        box(wellPanel(div(style = 'text-align: left; font-size: 100%; display: inline-block',
#                                          tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be a country'),
#                                          tags$code('tidyr::pivot_wider(RawData3, names_from = "Arg 1", values_from = "Arg 2")'))),
#
#
#                            # Raw Data
#                            column(12,
#                                   box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
#                                           (title = 'RawData3')),
#                                       style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
#                                       width = 6,
#                                       uiOutput("original2"))),
#
#                            # Drop Down selections
#                            fluidRow(
#                              # choices for user plot
#                              box(div(style = 'background-color: #ffffff',
#                                      (title = '')),
#                                  style = 'background: #ffffff',
#                                  width = 3,
#                                  selectInput(inputId = 'userOpA',
#                                              label = 'names_from =',
#                                              choices = c('population', '25', 'key', 'cases'),
#                                              selected = 'population')),
#                              box(div(style = 'background-color: #ffffff',
#                                      (title = '')),
#                                  style = 'background: #ffffff',
#                                  width = 3,
#                                  selectInput(inputId = 'userOpB',
#                                              label = 'values_from =',
#                                              choices = c('key', 'data', 'GDP', '1393'),
#                                              selected = 'key')),
#
#                            ),
#
#                            # R code based on blanks
#                            fluidRow(
#                              column(12,
#                                     div(style = 'text-align: left; font-size: 100%',
#                                         wellPanel(tags$strong('Your R code: '),
#                                                   uiOutput(paste0('userOutB')))
#                                     ))),
#
#                            # Your Tidy Attempt
#                            box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
#                                    (title = 'Your Tidy Attempt')),
#                                style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
#                                width = 6,
#                                uiOutput("userOutA")),
#                            width = 10),
#
#                        fluidRow(
#                          div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
#                              uiOutput("bus"),
#                              br(),
#                              uiOutput("redo"),
#                          ),
#                          br(),
#                          br(),
#                          div(style = 'display: inline-block; position: relative; text-align: right',
#                              uiOutput('cor'),
#                              uiOutput('wro'))
#                        )),
#
#               ## pivot_wider 2 ----
#
#               tabPanel(div(style = 'font-size: 125%', 'pivot_wider 2'), value = "pivot_wider2",
#
#
#                        box(wellPanel(div(style = 'text-align: left; font-size: 100%; display: inline-block',
#                                          tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be an employee'),
#                                          tags$code('tidyr::pivot_wider(RawData4, names_from = "Arg 1", values_from = "Arg 2")'))),
#
#
#
#                            column(12,
#                                   box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
#                                           (title = 'RawData4')),
#                                       style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
#                                       width = 6,
#                                       uiOutput("original4"))),
#                            br(),
#                            # choices for user plot
#                            fluidRow(
#                              box(div(style = 'background-color: #ffffff',
#                                      (title = '')),
#                                  style = 'background: #ffffff',
#                                  width = 3,
#                                  selectInput(inputId = 'userOpC',
#                                              label = 'names_from =',
#                                              choices = c('22', 'Age', 'Tips', 'Paycheck'),
#                                              selected = '22')),
#                              box(div(style = 'background-color: #ffffff',
#                                      (title = '')),
#                                  style = 'background: #ffffff',
#                                  width = 3,
#                                  selectInput(inputId = 'userOpD',
#                                              label = 'values_from =',
#                                              choices = c('Name', 'Dollars', 'Day', 'Wage'),
#                                              selected = 'Tim'))
#                            ),
#                            fluidRow(
#                              column(12,
#                                     div(style = 'text-align: left; font-size: 100%',
#                                         wellPanel(tags$strong('Your R code: '),
#                                                   uiOutput('userOut4'))
#                                     ))),
#                            br(),
#                            box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
#                                    (title = 'Your Tidy Attempt')),
#                                style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
#                                width = 6,
#                                uiOutput("userOut3")),
#                            width = 10),
#
#                        fluidRow(
#                          div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
#                              uiOutput("buss"),
#                              br(),
#                              uiOutput("redos")),
#                          br(),
#                          br(),
#                          div(style = 'display: inline-block; position: relative; text-align: right',
#                              uiOutput('cors'),
#                              uiOutput('wros'))
#                        )),
#
#
#
#               ## Live code ----
#
#               tabPanel(div(style = 'font-size: 125%', 'Live Code'),
#
#                        fluidRow(
#                          column(6,
#                                 verticalLayout(
#                                   h3("Instructions"),
#                                   wellPanel(
#                                     style = "background-color: #ffffff",
#                                     tags$div(
#                                       tags$li("Attempt the questions."),
#                                       tags$li("Run your code in the R script
#                                          box below and see the output on the right."),
#                                       tags$li("Uncomment the sample code to explore."),
#                                       style = "background-color: #ffffff")),
#                                   h3("Exercises"),
#                                   wellPanel(style = "background-color: #ffffff", #This panel is where the question and options go
#                                             uiOutput("question") %>%
#                                               withSpinner(color = "#ffffff"),
#                                             br(),
#                                             uiOutput("mark"),   #Shows symbol, what was picked and what should have been picked
#                                             tags$style(type = 'text/css', '#question{font-size: 15px;
#                                                   background-color: #ffffff;color: black;}',
#                                                        '.well { padding: 10px; margin-bottom: 15px; max-width: 1000px; }')
#
#                                   ),
#                                   fluidPage(
#                                     fluidRow(     #These are the 3 buttons on the bottom
#                                       column(12, align = "center",
#                                              div(style = "display: inline-block", actionButton(inputId = 'submit',
#                                                                                                label = 'Submit',
#                                                                                                style = "success")),
#                                              div(style = "display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
#                                              div(style = "display: inline-block", bsButton(inputId = "nextq",
#                                                                                            label = "Next",
#                                                                                            disabled = TRUE)),
#                                              div(style = "display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
#                                              div(style = "display: inline-block", bsButton(inputId = "reset",
#                                                                                            label = "Restart",
#                                                                                            style="danger",
#                                                                                            disabled = TRUE)))
#                                     )),
#
#
#                                   # try rLocker statement.   #### WHYYYY ----
#                                   tags$samp(
#                                     htmlOutput("statements")
#                                   ),
#                                   # end
#
#                                   h3("Test Your Answer"),
#                                   uiOutput('editor'),
#                                   column(6,
#                                          withBusyIndicatorUI(
#                                            actionButton("eval", "Run")))
#                                 )),
#                          br(),
#                          h3("Original Table"),
#                          column(6,
#                                 uiOutput('tableinfo'),
#                                 uiOutput('acetable')),
#                          column(6,
#                                 h3("Knitted Output"),
#                                 htmlOutput("knitDoc")
#                          )
#                        )
#               )
#   )
# ),
# ### Combining Challenge ----
# tabItem(
#   tabName = "comb",
#   withMathJax(),
#   h2("Combinging Data Challenge"),
#   fluidRow(
#     column(width = 12,
#            box(div(style = 'font-weight: bold; font-size: 140%', (title = 'Multiple Choice Joins Practice')),
#                style = 'text-align: left',
#                br(),
#                width = NULL,
#                height = NULL,
#                # tags$img(
#                #   class = "centerFigure",
#                #   src = "cds.png",
#                #   width = 300,
#                #   height = 110,
#                #   alt = "Picture of the table"),
#
#                div(style="display:inline-block", tableOutput('titleTableA')),
#                div(style="display: inline-block; font-size: 50px; vertical-align: middle; margin-top: -20px;", "+"),  # Increased font size and aligned vertically
#                div(style="display:inline-block", tableOutput('titleTableB')),
#                div(style="display:inline-block; font-size: 50px; vertical-align: middle; margin-top: -20px;", p(' = ')),
#
#                br(),
#                br(),
#                p("Exercise Instructions:"),
#                p("For the exercise below, please choose the appropriate type of join that results in the table displayed. Select the option that best matches the given output."),
#
#
#                fluidRow(#theme = "bootstrap.css",
#                  column(width = 4,
#                         box(title = NULL,
#                             width = '150px',
#                             height = '400px',
#
#                             radioButtons(inputId = "cd1", label = "Mutating Joins Option",
#                                          c('left join', 'right join', 'inner join', 'full join'),
#                                          selected = character(0)),
#                             #Ethan
#                             tableOutput('cdTable1'),
#                             fluidRow(
#                               column(4,bsButton(inputId = 'check1',
#                                                 label = 'Check',
#                                                 size = 'median')),
#
#                               column(8,(uiOutput('checkOrX1')), offset = 0))
#                         )
#                  ),
#
#                  column(width = 4,
#                         box(title = NULL,
#                             style = 'background-color: #ffffff',
#                             width = '150px',
#                             height = '400px',
#                             radioButtons(inputId = "cd2", label = "Mutating Joins Option",
#                                          c('left join', 'right join', 'inner join', 'full join'),
#                                          selected = character(0)),
#
#                             tableOutput('cdTable2'),
#                             fluidRow(
#                               column(4,bsButton(inputId = 'check2',
#                                                 label = 'Check',
#                                                 size = 'median')),
#
#                               #column(2,"jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj                          "),
#                               column(8,(uiOutput('checkOrX2')), offset = 0))
#                         )
#                  ),
#
#                  column(width = 4,
#                         box(title = NULL,
#                             style = 'background-color: #ffffff',
#                             width = '150px',
#                             height = '400px',
#                             radioButtons(inputId = "cd3", label = "Filtering Joins Option",
#                                          c('semi join', 'anti join'), selected = character(0)),
#                             tableOutput('cdTable3'),
#                             br(),
#                             fluidRow(
#                               column(4,bsButton(inputId = 'check3',
#                                                 label = 'Check',
#                                                 size = 'median')),
#                               column(8,(uiOutput('checkOrX3')), offset = 0)))
#                  ),
#
#                  column(width = 4,
#                         box(title = NULL,
#                             style = 'background-color: #ffffff',
#                             width = '150px',
#                             height = '400px',
#                             radioButtons(inputId = "cd4", label = "Filtering Joins Option",
#                                          c('semi join', 'anti join'), selected = character(0)),
#                             tableOutput('cdTable4'),
#                             fixedRow(
#                               column(4,bsButton(inputId = 'check4',
#                                                 label = 'Check',
#                                                 size = 'median')),
#                               column(8,(uiOutput('checkOrX4')), offset = 0)))
#                  ),
#
#                  column(width = 4,
#                         box(title = NULL,
#                             style = 'background-color: #ffffff',
#                             width = '150px',
#                             height = '400px',
#                             radioButtons(inputId = "cd5", label = "Filtering Joins Option",
#                                          c('left join', 'right join', 'inner join', 'full join'),
#                                          selected = character(0)),
#                             tableOutput('cdTable5'),
#                             fluidRow(
#                               column(4,bsButton(inputId = 'check5',
#                                                 label = 'Check',
#                                                 size = 'median')),
#                               column(8,(uiOutput('checkOrX5')), offset = 0)))
#                  ),
#
#                  column(width = 4,
#                         box(title = NULL,
#                             style = 'background-color: #ffffff',
#                             width = '150px',
#                             height = '400px',
#                             radioButtons(inputId = "cd6", label = "Mutating Joins Option",
#                                          c('left join', 'right join', 'inner join', 'full join'),
#                                          selected = character(0)),
#                             tableOutput('cdTable6'),
#                             fixedRow(
#                               column(4,bsButton(inputId = 'check6',
#                                                 label = 'Check',
#                                                 size = 'median')),
#                               column(8,(uiOutput('checkOrX6')), offset = 0))))))
#     )
#   )
# ),

# Deleted Code from Server ----
#   # Tidy Data Challenge Page----
#
#   ## pivot_longer 1 ----
#
#   RawData <- table4a
#
#   output$original1 <- renderTable({
#     RawData
#   })
#
#   # Specify Outputs pivot_longer1
#   output$userOut1 <- renderTable({
#     if(input$userOp1 == 'country')
#       op1 <- 'country'
#     else if(input$userOp1 == '1999')
#       op1 <- '1999'
#     else if(input$userOp1 == 'Afghanistan')
#       op1 <- 'Afghanistan'
#     else
#       op1 <- '2000'
#
#
#     if(input$userOp2 == 'country')
#       op2 <- 'country'
#     else if(input$userOp2 == '1999')
#       op2 <- '1999'
#     else if(input$userOp2 == 'Afghanistan')
#       op2 <- 'Afghanistan'
#     else
#       op2 <- '2000'
#
#
#     if(input$userOp3 == '1999')
#       op3 <- '1999'
#     else if(input$userOp3 == 'cases')
#       op3 <- 'cases'
#     else if(input$userOp3 == 'year')
#       op3 <- 'year'
#     else
#       op3 <- 'China'
#
#
#     if(input$userOp4 == '1999')
#       op4 <- '1999'
#     else if(input$userOp4 == 'cases')
#       op4 <- 'cases'
#     else if(input$userOp4 == 'year')
#       op4 <- 'year'
#     else
#       op4 <- 'China'
#
#
#     RawData <- RawData %>%
#       mutate('1999' = as.character(RawData$'1999')) %>%
#       mutate('2000' = as.character(RawData$'2000'))
#     tryCatch({
#       RawData %>%
#         pivot_longer(cols = c(op1,op2), names_to =op3, values_to = op4)
#     },
#     warning <- function(war) {
#
#       return("warning")
#     },
#     error = function(err) {
#       return("That code would produce no output")
#     }
#     )
#
#   })
#
#
#   # Bottom of options
#
#   #dynamic code based on user inputs
#   output$tidyAttemptTable <- renderUI({
#     tags$code(paste0('tidyr::pivot_longer(RawData, cols = c("', input$userOp1, '","', input$userOp2, '") ,
#              names_to = "', input$userOp3, '", values_to = "', input$userOp4, '")' ))
#   })
#
#
#
#   # submit button
#   output$sub <- renderUI({
#     bsButton("submitcc",
#              label = "Check Answer",
#              icon("lightbulb"),
#              size = "medium",
#              style = 'success')
#   })
#
#
#   # delay submit button
#   observeEvent(input$submitcc,{
#     withProgress(session, min = 1, max = 15, {
#       setProgress(message = 'Checking Answer',
#                   detail = '')
#       for (i in 1:13) {
#         setProgress(value = i)
#         Sys.sleep(0.05)
#       }
#     })
#   })
#
#
#   # correct/wrong gif
#
#
#   output$resetcc <- renderUI({
#     bsButton("retry",
#              label = "Try Again",
#              icon("retweet"),
#              size = "medium",
#              style = 'success')
#   })
#
#   # delay retry button
#   observeEvent(input$retry,{
#     withProgress(session, min = 1, max = 15, {
#       setProgress(message = 'Resetting',
#                   detail = '')
#       for (i in 1:13) {
#         setProgress(value = i)
#         Sys.sleep(0.05)
#       }
#       somethingIsSelected <<- F
#       disable("submit")
#     })
#   })
#
#   # Next Question
#   output$nextQuestion1 <- renderUI({
#     bsButton("next1",
#              label = "Next Question",
#              size = "medium",
#              style = 'success')
#   })
#   observeEvent(input$next1, {
#     updateTabsetPanel(session, "questionTabs",
#                       selected = "pivot_longer2")
#   })
#
#
#
#
#   # hide reset button upon opening app
#   hide("resetcc")
#   hide("correct")
#   hide("wrong")
#   hide("nextQuestion1")
#
#
#   output$correct <- renderUI({
#     tags$img(src = "correct.png", width = 30)
#   })
#
#   output$wrong <- renderUI({
#     tags$img(src = "incorrect.png", width = 30)
#   })
#
#
#
#
#   # show reset button after submit is clicked, disable dropdown inputs
#   observeEvent(input$submitcc,{
#     toggle("resetcc")
#     toggle("nextQuestion1")
#     disable("userOp1")
#     disable("userOp2")
#     disable("userOp3")
#     disable("userOp4")
#     disable("submitcc")
#     if(input$userOp1 == '1999' & input$userOp2 == '2000'
#        & input$userOp3 == 'year' & input$userOp4 == 'cases' || input$userOp1 == '2000' & input$userOp2 == '1999'
#        & input$userOp3 == 'year' & input$userOp4 == 'cases') {
#       showElement("correct")
#     }
#     else{
#       showElement("wrong")
#     }
#
#
#   })
#   observeEvent(input$retry,{
#     enable("userOp1")
#     enable("userOp2")
#     enable("userOp3")
#     enable("userOp4")
#     enable("submitcc")
#     enable("sub")
#     toggle("resetcc")
#     hide("correct")
#     hide("wrong")
#   })
#
#
#
#   ## pivot_longer 2 ----
#
#   capital <- c("Kabul", "Brasilia", "Beijing")
#
#   table4a$capital <- capital
#
#   RawData2 <- data.frame("Name" = c("John","Dora","Tim","Rebecca"),
#                          "Age" = c('21','19','22','21'),
#                          "MonTips" = c('8','7','12','10'),
#                          "TueTips" = c('14','10','11','9'),
#                          "WedTips" = c('11','14','13','11'))
#
#   output$original3 <- renderTable({
#     RawData2
#   })
#
#   # specify outputs for every choice
#
#   # This code creates the your tidy attempt output
#   output$tidyAttempt2 <- renderTable({
#     if(input$userOp5 == 'John')
#       op5 <- 'John'
#     else if(input$userOp5 == 'MonTips')
#       op5 <- 'MonTips'
#     else if(input$userOp5 == 'Age')
#       op5 <- 'Age'
#     else
#       op5 <- 'Name'
#
#
#     if(input$userOp6 == 'Tim')
#       op6 <- 'Tim'
#     else if(input$userOp6 == 'MonTips')
#       op6 <- 'MonTips'
#     else if(input$userOp6 == 'TueTips')
#       op6 <- 'TueTips'
#     else
#       op6 <- '21'
#
#     if(input$userOp7 == 'Age')
#       op7 <- 'Age'
#     else if(input$userOp7 == 'Name')
#       op7 <- 'Name'
#     else if(input$userOp7 == '7')
#       op7 <- '7'
#     else
#       op7 <- 'WedTips'
#
#
#     if(input$userOp8 == '21')
#       op8 <- '21'
#     else if(input$userOp8 == 'MonTips')
#       op8 <- 'MonTips'
#     else if(input$userOp8 == 'Tips')
#       op8 <- 'Tips'
#     else
#       op8 <- 'Day'
#
#     if(input$userOp9 == '8')
#       op9 <- '9'
#     else if(input$userOp9 == 'Age')
#       op9 <- 'Age'
#     else if(input$userOp9 == 'Day')
#       op9 <- 'Day'
#     else
#       op9 <- 'Tips'
#
#     tryCatch({
#       RawData2 %>%
#         pivot_longer(cols = c(op5,op6,op7), names_to = op8, values_to = op9)
#     },
#     warning = function(war) {
#
#       return("warning")
#     },
#     error = function(err) {
#       return("That code would produce no output")
#     }
#     )
#
#   })
#
#
#
#
#   # show code based on inputs
#   output$userOutY <- renderUI({
#     tags$code(paste0('tidyr::pivot_longer(RawData2, cols = c("', input$userOp5, '","', input$userOp6, '"),
#              names_to = "', input$userOp7, '", values_to = "', input$userOp8, '")' ))
#   })
#
#   #pivot_longer2
#   observeEvent(input$retry2,{
#     enable("userOp5")
#     enable("userOp6")
#     enable("userOp7")
#     enable("userOp8")
#     enable("userOp9")
#     enable("submitting")
#     toggle("restart")
#     hide("cort")
#     hide("rong")
#   })
#
#
#   # submit button
#   output$subbed <- renderUI({
#     bsButton("submitting",
#              label = "Check Answer",
#              icon("lightbulb"),
#              size = "medium",
#              style = 'success')
#   })
#
#   observeEvent(input$submitting,{
#     withProgress(session, min = 1, max = 15, {
#       setProgress(message = 'Checking Answer',
#                   detail = '')
#       for (i in 1:13) {
#         setProgress(value = i)
#         Sys.sleep(0.05)
#       }
#     })
#   })
#
#
#   output$restart <- renderUI({#Ethan 8/3
#     bsButton("retry2",
#              label = "Try Again",
#              icon("retweet"),
#              size = "medium",
#              style = 'success')
#   })
#
#   # delay retry button
#   observeEvent(input$retryy,{
#     withProgress(session, min = 1, max = 15, {
#       setProgress(message = 'Resetting',
#                   detail = '')
#       for (i in 1:13) {
#         setProgress(value = i)
#         Sys.sleep(0.05)
#       }
#     })
#   })
#
#
#   # hide reset button upon opening app
#   hide("restart")
#   hide("cort")
#   hide("rong")
#
#
#   output$cort <- renderUI({
#     tags$img(src = "correct.png", width = 30)
#   })
#
#   output$rong <- renderUI({
#     tags$img(src = "incorrect.png", width = 30)
#   })
#
#
#   # pivot_longer2 Check
#   observeEvent(input$submitting,{
#     toggle("restart")
#     disable("userOp5")
#     disable("userOp6")
#     disable("userOp7")
#     disable("userOp8")
#     disable("userOp9")
#     disable("submitting")
#     if(input$userOp5 == 'MonTips' & input$userOp6 == 'TueTips'
#        & input$userOp7 == 'WedTips' & input$userOp8 == 'Day' &
#        input$userOp9 == 'Tips') {
#       showElement("cort")
#     }
#     else{
#       showElement("rong")
#     }
#
#
#   })
#
#
#   observeEvent(input$retryy,{
#     hide("restart")
#     enable("userOp5")
#     enable("userOp6")
#     enable("userOp7")
#     enable("userOp8")
#     showElement("submitting")
#     enable("submitting")
#     hide("cort")
#     hide("rong")
#
#   })
#
#   observeEvent(input$retryy,{
#     reset("userOp5")
#     reset("userOp6")
#     reset("userOp7")
#     reset("userOp8")
#     showElement("submitting")
#     enable("submitting")
#
#   })
#
#
#   ## pivot_wider 1 ----
#
#   RawData3 <- data.frame("country" = c("Afghanistan","Afghanistan","Australia","Australia","China","China"),
#                          "key" = c("GDP","population","GDP","population","GDP","population"),
#                          "data" = c("19","37","1434","25","13610","1393"))
#
#   output$original2 <- renderTable({
#     RawData3
#   })
#
#   # specify outputs for every choice
#   output$userOutA <- renderTable({
#     if(input$userOpA == 'population')
#       op1 <- 'population'
#     else if(input$userOpA == '25')
#       op1 <- '25'
#     else if(input$userOpA == 'key')
#       op1 <- 'key'
#     else
#       op1 <- 'cases'
#
#
#     if(input$userOpB == 'key')
#       op2 <- 'key'
#     else if(input$userOpB == 'data')
#       op2 <- 'data'
#     else if(input$userOpB == 'GDP')
#       op2 <- 'GDP'
#     else
#       op2 <- '1393'
#
#
#     tryCatch({
#       RawData3 %>%
#         pivot_wider(names_from = op1, values_from = op2)
#     },
#     warning = function(war) {
#
#       return("warning")
#     },
#     error = function(err) {
#       return("That code would produce no output")
#     }
#     )
#
#
#
#   })
#
#   # Bottom of options
#
#   # show code based on inputs
#   output$userOutB <- renderUI({
#     tags$code(paste0('tidyr::pivot_wider(RawData3, names_from = "', input$userOpA, '", values_from = "', input$userOpB, '")' ))
#   })
#
#
#   # submit button pivot_wider
#   output$bus <- renderUI({
#     bsButton("submitted",
#              label = "Check Answer",
#              icon("lightbulb"),
#              size = "medium",
#              style = 'success')
#   })
#
#   observeEvent(input$submitted,{
#     withProgress(session, min = 1, max = 15, {
#       setProgress(message = 'Checking Answer',
#                   detail = '')
#       for (i in 1:13) {
#         setProgress(value = i)
#         Sys.sleep(0.05)
#       }
#     })
#   })
#
#
#
#   output$redo <- renderUI({
#     bsButton("retrying",
#              label = "Try Again",
#              icon("retweet"),
#              size = "medium",
#              style = 'success')
#   })
#
#   # delay retry button
#   observeEvent(input$retrying, {
#     withProgress(session, min = 1, max = 15, {
#       setProgress(message = 'Resetting',
#                   detail = '')
#       for (i in 1:13) {
#         setProgress(value = i)
#         Sys.sleep(0.05)
#       }
#     })
#   })
#
#   # Next Question
#   output$nextQuestion2 <- renderUI({
#     bsButton("next2",
#              label = "Next Question",
#              size = "medium",
#              style = 'success')
#   })
#   observeEvent(input$next2, {
#     updateTabsetPanel(session, "questionTabs",
#                       selected = "pivot_wider1")
#   })
#
#
#
#   # hide reset button upon opening app
#   hide("redo")
#   hide("cor")
#   hide("wro")
#   hide("nextQuestion2")
#
#
#   output$cor <- renderUI({
#     tags$img(src = "correct.png", width = 30)
#   })
#
#   output$wro <- renderUI({
#     tags$img(src = "incorrect.png", width = 30)
#   })
#
#
#   # show reset button after submit is clicked, disable dropdown inputs
#   observeEvent(input$submitted,{
#     toggle("redo")
#     toggle("nextQuestion2")
#     disable("userOpA")
#     disable("userOpB")
#     disable("userOpC")
#     disable("userOpD")
#     disable("submitted")
#     if(input$userOpA == 'key' & input$userOpB == 'data')
#     {
#       showElement("cor")
#     }
#     else{
#       showElement("wro")
#     }
#
#   })
#
#
#   observeEvent(input$retrying,{
#     hide("redo")
#     enable("userOpA")
#     enable("userOpB")
#     enable("userOpC")
#     enable("userOpD")
#     showElement("submitted")
#     enable("submitted")
#     hide("cor")
#     hide("wro")
#     hide("nextQuestion2")
#   })
#
#
#   ##################### pivot_wider 2 ###################
#
#   capital <- c("Kabul", "Brasilia", "Beijing")
#
#   table4b$capital <- capital
#
#   RawData4 <- data.frame("Name" = c("John","John","John","Dora","Dora","Dora","Tim","Tim","Tim","Rebecca","Rebecca","Rebecca"),
#                          "Age" = c('21','21','21','19','19','19','22','22','22','21','21','21'),
#                          "Paycheck" = c('Wage','Tips','Tax','Wage','Tips','Tax','Wage','Tips','Tax','Wage','Tips','Tax'),
#                          "Dollars" = c('25','30','14','36','37','21','22','31','13','41','50','24'))
#
#   output$original4 <- renderTable({
#     RawData4
#   })
#
#
#
#   # specify outputs for every choice
#   output$userOut3 <- renderTable({ #Ethan
#     if(input$userOpC == '22')
#       op1 <- '22'
#     else if(input$userOpC == 'Age')
#       op1 <- 'Age'
#     else if(input$userOpC == 'Tips')
#       op1 <- 'Tips'
#     else
#       op1 <- 'Paycheck'
#
#
#     if(input$userOpD == 'Name')
#       op2 <- 'Name'
#     else if(input$userOpD == 'Dollars')
#       op2 <- 'Dollars'
#     else if(input$userOpD == 'Day')
#       op2 <- 'Day'
#     else
#       op2 <- 'Wage'
#
#     tryCatch({
#       RawData4 %>%
#         pivot_wider(names_from =op1, values_from = op2)
#     },
#     warning = function(war) {
#
#       return("warning")
#     },
#     error = function(err) {
#       return("That code would produce no output")
#     }
#     )
#
#   })
#
#   # Bottom of options
#
#   # show code based on inputs
#   output$userOut4 <- renderUI({
#     tags$code(paste0('tidyr::pivot_wider(RawData4,
#                      names_from = "', input$userOpC, '",
#                      values_from = "', input$userOpD, '")' ))
#   })
#
#   # submit button
#   output$buss <- renderUI({
#     bsButton("submitteds",
#              label = "Check Answer",
#              icon("lightbulb"),
#              size = "medium",
#              style = 'success')
#   })
#
#   observeEvent(input$submitteds,{
#     withProgress(session, min = 1, max = 15, {
#       setProgress(message = 'Checking Answer',
#                   detail = '')
#       for (i in 1:13) {
#         setProgress(value = i)
#         Sys.sleep(0.05)
#       }
#     })
#   })
#
#
#
#   output$redos <- renderUI({
#     bsButton("retryings",
#              label = "Try Again",
#              icon("retweet"),
#              size = "medium",
#              style = 'success')
#   })
#
#   # delay retry button
#   observeEvent(input$retryings, {
#     withProgress(session, min = 1, max = 15, {
#       setProgress(message = 'Resetting',
#                   detail = '')
#       for (i in 1:13) {
#         setProgress(value = i)
#         Sys.sleep(0.05)
#       }
#     })
#   })
#
#
#   # hide reset button upon opening app
#   hide("redos")
#   hide("cors")
#   hide("wros")
#
#
#   output$cors <- renderUI({
#     tags$img(src = "correct.png", width = 30)
#   })
#
#   output$wros <- renderUI({
#     tags$img(src = "incorrect.png", width = 30)
#   })
#
#
#   # show reset button after submit is clicked, disable dropdown inputs
#   observeEvent(input$submitteds,{
#     toggle("redos")
#     disable("userOpJ")
#     disable("userOpK")
#     disable("userOpL")
#     disable("userOpM")
#     disable("submitteds")
#     if(input$userOpC == 'Paycheck' & input$userOpD == 'Dollars') {
#       showElement("cors")
#     }
#     else{
#       showElement("wros")
#     }
#
#   })
#
#
#   observeEvent(input$retryings,{
#     hide("redos")
#     enable("userOpC")
#     enable("userOpD")
#     showElement("submitteds")
#     enable("submitteds")
#     hide("cors")
#     hide("wros")
#
#   })
#
#
#   ##  Live Code ----
#   runButtonWasPressed <<- F #Used to stop error in the knitted output
#   disable("submit")
#
#   # question bank
#   somethingIsSelected <<- F
#   value <- reactiveValues(index =  1, mistake = 0, correct = 0)
#   ans <- as.matrix(bank[1:9, 6])
#   index_list <- reactiveValues(list = sample(2:9, 8, replace = FALSE))
#
#   observeEvent(input$nextq,{
#
#     value$answerbox <- value$index
#     #Removes the value in the front of the list
#     index_list$list = index_list$list[-1]
#
#
#     value$index <- index_list$list[1]
#     value$answerbox <- value$index
#     output$mark <- renderUI({
#       img(src = NULL, width = 30)
#     })
#     somethingIsSelected <<- F
#
#     disable("submit")
#     updateButton(session, "nextq", disabled = TRUE)
#     updateButton(session, "submit", disabled = FALSE)
#
#     withBusyIndicatorServer("eval", {
#       output$knitDoc <- renderUI({
#         return(isolate(HTML(knit2html(text = "Click \"Run\" to test the code", template = FALSE, quiet = TRUE))))
#       })
#
#
#     })
#   })
#
#   output$question <- renderUI({#ETHAN radio numbers
#     #h4(bank[value$index, 2])
#     radioButtons(inputId = 'answer', label=bank[value$index, 2],
#                  choiceNames=list(bank[value$index, 3], bank[value$index, 4], bank[value$index, 5]),
#                  choiceValues = list("A","B","C"), selected = character(0))
#
#   })
#
#
#   ## change table based on question
#   output$acetable <- renderTable({
#     #new
#     if(value$index == 2 || value$index == 3)
#     {
#       race
#     }
#     else if(value$index == 1)
#     {
#
#     }
#     else if(value$index == 4 || value$index == 5)
#     {
#       results
#     }
#     else if(value$index == 6 || value$index == 7)
#     {
#       grades
#     }
#     else
#     {
#       table5
#     }
#   })
#
#   output$tableinfo <- renderUI({
#     # race data info
#     if (bank[value$index, 2] == bank[2, 2] | bank[value$index, 2] == bank[3, 2]) {
#       tags$h4('This table depicts times and scores on a running race.')
#       #tags$h4('Column names define different lengths of time')
#       #tags$h4('Cell values are scores associated with each name and length of time')
#
#       # results data info
#     } else if (bank[value$index, 2] == bank[4, 2] | bank[value$index, 2] == bank[5, 2]) {
#       tags$li('This table depicts clinical trial data')
#       #tags$li('Ind - individual participating in the experiment')
#       #tags$li('Treatment - trial type (Treat or Cont)')
#       #tags$li('value - result of experiment')
#
#       # grades data info
#     } else if (bank[value$index, 2] == bank[6, 2] | bank[value$index, 2] == bank[7, 2]) {
#       tags$li('This table depicts student test score data')
#       tags$li('A tidy case is one individual during one quarter in a given year.')
#       tags$li('Each test is unique and should be treated as two separate variables.')
#
#       # table 5 data ifo
#     } else if (bank[value$index, 2] == bank[8, 2] || bank[value$index, 2] == bank[9, 2]) {
#       tags$li('This table shows the population and rate of different countries.')
#     }
#
#
#   })
#
#
#
#   output$editor <- renderUI({
#     aceEditor("rmd",
#               mode = "markdown",
#               if(value$index == 1) {
#                 value = 'Here you can test out the answer choices before choosing an answer!
#
# Uncomment one line from each section at a time and hit "Run" to see its effect!
#
# Note: If the code does not display/change data, it probably is not the correct answer.
#
# There is no interactive R code for this question!'
#               } else if (value$index == 2) {
#                 value = 'No interactive R code for this question!'
#               }
#               else if (value$index == 3) {
#                 value = '
# ```{r}
#
#
# tidyRace <-
#   race %>%
#   # pivot_wider(names_from = "Time", values_from = "Score") %>%
#   # pivot_longer(cols = c("50","100","150","200","250","300","350"), names_to = "Time", values_to = "Score") %>%
#   # unite(col = New, "50", "100", "150","200", sep = "", remove = TRUE) %>%
#   arrange(Name)
#
# tidyRace
# ```
# '
#               }
#               else if (value$index == 4) {
#                 value = 'No interactive R code for this question!'
#               }
#               else if (value$index == 5) {
#                 value = '```{r}
#
#
# tidyResults <-
#   results %>%
#   # pivot_wider(names_from = "Treatment", values_from = "value")
#   # pivot_longer(cols = c("Treat","Cont"), names_to = "Treatment", values_to = "value")
#   # unite(col = new, Treatment, value, sep = "", remove = TRUE)
#
# tidyResults
# ```
#                 '
#               }
#               else if (value$index == 6) {
#                 value = 'No interactive R code for this question!'
#               }
#               else if (value$index == 7) {
#                 value = '```{r}
#
#
#
# tidyGrades <-
#   grades %>%
#   # pivot_wider(names_from = "Test", values_from = "Score") %>%
#   # pivot_longer(cols = c("Fall","Spring","Winter"), names_to = "Quarter", values_to = "Score") %>%
#   # unite(col = new, Test, Year, sep = "", remove = TRUE)
# tidyGrades
# ```
# '
#               }
#               else if (value$index == 8) {
#                 value = '```{r}
# library(tidyr)
#
# tidyTable5 <-
#   table5 %>%
#   # pivot_wider(key = century, value = year)
#   # pivot_longer(cols = c("1999","2000"), names_to = century, values_to = year)
#   # unite(col = new, century, year, sep = "", remove = TRUE)
#
# tidyTable5
# ```
# '
#               }
#               else {
#                 value = '```{r}
# library(tidyr)
#
# nextStep <-
#   table5 %>%
#   # unite(col = new, century, year)
#   # unite(col = new, century, year, sep = "")
#   # unite(col = year, century, year)
#
# nextStep
# ```
# '
#               }
#     )
#   })
#
#   observeEvent(input$answer, {
#     somethingIsSelected <<- T
#     enable("submit")
#   })
#
#   # Once rLocker works properly this may be relevant.
#   # observeEvent(input$answer, {
#   #   req(input$answer, input$answer !='')
#   #   answer <- isolate(input$answer)
#   #   # interacted_statement <- rLocker::createStatement(
#   #   #   list(
#   #   #     verb = list(
#   #   #       display = "selected"),
#   #   #     object = list(
#   #   #       id = paste0(getCurrentAddress(session), "#", value$index),
#   #   #       name = paste('Question', value$index),
#   #   #       description = bank[value$index, 2]),
#   #   #     result = list(
#   #   #       success = any(answer == ans[value$index, 1]),
#   #   #       response = paste(getResponseText(value$index, answer),
#   #   #                        as.character(Sys.time()))
#   #   #     )
#   #   #   )
#   #   # )
#   #
#   #
#   #   # Store statement in locker and return status
#   #   #status <- rLocker::store(session, interacted_statement)
#   #
#   #   #print(interacted_statement) # remove me
#   #   #print(status) # remove me
#   # })
#
#
#   getResponseText <- function(index, answer){
#     if(answer == 'A'){
#       key = 3
#     } else if(answer == 'B'){
#       key = 4
#     } else {
#       key = 5
#     }
#     return(bank[index, key])
#   }
#
#   observeEvent(input$submit,{
#     validate(
#       need(input$answer != "", ""),
#       errorClass = "inline"
#     )
#     validate(
#       need(somethingIsSelected != F, ""),
#       errorClass = "inline"
#     )
#
#     if(length(index_list$list) == 1){
#       updateButton(session, "nextq", disabled = TRUE)
#       updateButton(session,"submit", disabled = TRUE)
#       updateButton(session, "reset", disabled = FALSE)
#     }
#     else{
#       updateButton(session, "nextq", disabled = FALSE)
#       updateButton(session,"submit", disabled = TRUE)
#       updateButton(session, "reset", disabled = FALSE)
#     }
#
#
#     answer <- input$answer
#
#     statement <- rLocker::createStatement(
#       list(
#         verb = list(
#           display = "answered"
#         ),
#         object = list(id = paste0(getCurrentAddress(session), "#", value$index),
#                       name = paste('Question', value$index),
#                       description = bank[value$index, 2]),
#         result = list(success = any(answer == ans[value$index, 1]),
#                       response = paste(getResponseText(value$index, answer),
#                                        as.character(Sys.time()))
#         )
#       )
#     )
#
#     # Store statement in locker and return status
#     status <- rLocker::store(session, statement)
#
#
#     output$mark <- renderUI({
#       if (any(answer == ans[value$index, 1])){
#         img(src = "correct.png", width = 30)
#       }
#       else{
#         ig <- img(src = "incorrect.png", width = 30)
#         w <- paste("You picked", answer, ", The correct answer is", ans[value$index, 1])
#         HTML(paste(ig, w), sep = ' ')
#       }
#     })
#   })
#
#
#   observeEvent(input$reset, {
#     updateButton(session, "submit", disabled = FALSE)
#     updateButton(session,"reset", disable = TRUE)
#
#     #This is what randomly orders the list
#     index_list$list <- c(1, sample(2:9, 8, replace = FALSE))
#
#     #the first question will always be index 1. Then 2-9 random
#     value$index <- 1
#     value$answerbox = value$index
#     ans <- as.matrix(bank[1:9, 6])
#     output$mark <- renderUI({
#       img(src = NULL,width = 30)
#     })
#     #Sets the question up
#     updateRadioButtons(session, "answer", "Another Question",
#                        choiceNames=list(bank[value$index, 3], bank[value$index, 4], bank[value$index, 5]),
#                        choiceValues = list("A","B","C"), selected = character(0))
#     disable("submit")
#   })
#
#   # Initialize Learning Locker connection
#   connection <- rLocker::connect(session, list(
#     base_url = "https://learning-locker.stat.vmhost.psu.edu/",
#     auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",
#     agent = rLocker::createAgent()
#   ))
#
#   # Setup demo app and user.
#   currentUser <-
#     connection$agent
#
#   if(connection$status != 200){
#     warning(paste(connection$status, "\nTry checking your auth token."))
#   }
#
#
#   # Setup demo app and user.
#
#   output$Previewcar<-
#     renderTable({
#       head(cars, 4)
#     }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')
#
#   output$Previewtree<-
#     renderTable({
#       head(trees, 4)
#     }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')
#
#   output$Previewiris<-
#     renderTable({
#       head(iris, 4)
#     }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')
#
#   ### KNITR ----
#   observeEvent(input$eval,{
#     runButtonWasPressed <<- T
#     withBusyIndicatorServer("eval", {
#       output$knitDoc <- renderUI({
#         return(isolate(HTML(knit2html(text = input$rmd, template = FALSE, quiet = TRUE))))
#       })
#
#       output$output <- renderPrint({
#         return(isolate(eval(parse(text = input$code))))
#       })
#     })
#   })
#
#   output$knitDoc <- renderUI({#Ethan
#     if(runButtonWasPressed == F)
#     {
#       return(isolate(HTML(knit2html(text = "Select the \"Run\" button underneath
#                                     the test Your Answer to see the code output below",
#                                     template = FALSE, quiet = TRUE))))
#     }
#     else{
#       return(isolate(HTML(knit2html(text = input$rmd, template = FALSE, quiet = TRUE))))
#     }
#   })
#
#   output$output <- renderPrint({
#     #input$eval
#     return(isolate(eval(parse(text = input$code))))
#   })
#
#   # Combining Data Table ----
#   disable("check1")
#   disable("check2")
#   disable("check3")
#   disable("check4")
#   disable("check5")
#   disable("check6")
#   observeEvent(input$check1, {
#     validate(need(((input$cd1 != "")), ""))
#
#     if (input$cd1 == 'left join') {
#       #Ethan
#       output$checkOrX1 <- renderUI(img(src = "correct.png",width=30))
#     }
#     else {
#       output$checkOrX1 <- renderUI(img(src = "incorrect.png",width=30))
#     }
#   })
#   observeEvent(input$check2, {
#     validate(need(((input$cd2 != "")), ""))
#
#     if (input$cd2 == 'inner join') {
#       output$checkOrX2 <- renderUI(img(src = "correct.png",width=30))
#     }
#     else {
#       output$checkOrX2 <- renderUI(img(src = "incorrect.png",width=30))
#     }
#   })
#   observeEvent(input$check3, {
#     validate(need(((input$cd3 != "")), ""))
#
#     if (input$cd3 == 'anti join') {
#       output$checkOrX3 <- renderUI(img(src = "correct.png",width=30))    }
#     else {
#       output$checkOrX3 <- renderUI(img(src = "incorrect.png",width=30))
#     }
#   })
#   observeEvent(input$check4, {
#     validate(need(((input$cd4 != "")), ""))
#
#     if (input$cd4 == 'semi join') {
#       output$checkOrX4 <- renderUI(img(src = "correct.png",width=30))    }
#     else {
#       output$checkOrX4 <- renderUI(img(src = "incorrect.png",width=30))
#     }
#   })
#   observeEvent(input$check5, {
#     validate(need(((input$cd5 != "")), ""))
#
#     if (input$cd5 == 'full join') {
#       output$checkOrX5 <- renderUI(img(src = "correct.png",width=30))    }
#     else {
#       output$checkOrX5 <- renderUI(img(src = "incorrect.png",width=30))
#     }
#   })
#   observeEvent(input$check6, {
#     validate(need(((input$cd6 != "")), ""))
#
#     if (input$cd6 == 'right join') {
#       output$checkOrX6 <- renderUI(img(src = "correct.png",width=30))    }
#     else {
#       output$checkOrX6 <- renderUI(img(src = "incorrect.png",width=30))
#     }
#   })
#
#   observeEvent(input$cd1,{enable("check1")})
#   observeEvent(input$cd2,{enable("check2")})
#   observeEvent(input$cd3,{enable("check3")})
#   observeEvent(input$cd4,{enable("check4")})
#   observeEvent(input$cd5,{enable("check5")})
#   observeEvent(input$cd6,{enable("check6")})
#
#   #correct answer: A, C, D, B --- left/inner/full/right
#   a <- data.frame("x1" = c("A","B","C"), "x2" = c("1","2","3"))
#   b <- data.frame("x1" = c("A","B","D"), "x3" = c("T","F","T"))
#   output$titleTableA <- renderTable({a})
#   output$titleTableB <- renderTable({b})
#
#   output$cdTable1 <- renderTable({
#
#     dplyr::left_join(a, b, by = "x1")
#     #Ethan
#
#   })
#
#   output$cdTable2 <- renderTable({
#     dplyr::inner_join(a, b, by = "x1")
#   })
#
#   output$cdTable3 <- renderTable({
#     dplyr::anti_join(a, b, by = "x1")
#   })
#
#   output$cdTable4 <- renderTable({
#     dplyr::semi_join(a, b, by = "x1")
#   })
#
#   output$cdTable5 <- renderTable({
#     dplyr::full_join(a, b, by = "x1")
#   })
#
#   output$cdTable6 <- renderTable({
#     dplyr::right_join(a, b, by = "x1")
#   })
#
#   output$cdExp1 <- renderText ({
#     if (input$cd1 == 'left join') {
#       paste('Join matching rows from b to a.')
#     }
#     else if (input$cd1 == 'right join') {
#       paste('Join matching rows from a to b.')
#     }
#     else if (input$cd1 == 'inner join') {
#       paste('Join data. Retain only rows in both sets.')
#     }
#     else {
#       paste('Join data. Retain all values, all rows.')
#     }
#   })
#
#   output$cdExp2 <- renderText ({
#     if (input$cd2 == 'left join') {
#       paste('Join matching rows from b to a.')
#     }
#     else if (input$cd2 == 'right join') {
#       paste('Join matching rows from a to b.')
#     }
#     else if (input$cd2 == 'inner join') {
#       paste('Join data. Retain only rows in both sets.')
#     }
#     else {
#       paste('Join data. Retain all values, all rows.')
#     }
#   })
#
#   output$cdExp3 <- renderText ({
#     if (input$cd3 == 'left join') {
#       paste('Join matching rows from b to a.')
#     }
#     else if (input$cd3 == 'right join') {
#       paste('Join matching rows from a to b.')
#     }
#     else if (input$cd3 == 'inner join') {
#       paste('Join data. Retain only rows in both sets.')
#     }
#     else {
#       paste('Join data. Retain all values, all rows.')
#     }
#   })
#
#   output$cdExp4 <- renderText ({
#     if (input$cd4 == 'left join') {
#       paste('Join matching rows from b to a.')
#     }
#     else if (input$cd4 == 'right join') {
#       paste('Join matching rows from a to b.')
#     }
#     else if (input$cd4 == 'inner join') {
#       paste('Join data. Retain only rows in both sets.')
#     }
#     else {
#       paste('Join data. Retain all values, all rows.')
#     }
#   })