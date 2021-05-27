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
library(rlocker) 
library(datasets)
library(rmarkdown)
library(learnr)
library(rcfss)
library(shinycssloaders) 
library(boastUtils)




bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)


source("helpers.R")
ui <- list(
  dashboardPage(
    skin = "green",
      dashboardHeader(title = 'Data Wrangling',
                      titleWidth = 250,
                      tags$li(class = "dropdown",actionLink("info", icon("info"), class = "myClass")),
                      tags$li(class = "dropdown",tags$a(href = "https://shinyapps.science.psu.edu/",icon("home")))),
        
    dashboardSidebar(
      width = 250,
      sidebarMenu(id = 'tabs',
              menuItem('Overview', tabName = 'overview', icon = icon("dashboard")),
              menuItem('Prerequisites', tabName = 'prereq', icon = icon('book')),
              menuItem('Explore Data Wrangling', tabName = 'exp1', icon = icon('wpexplorer')),
              menuItem('Tidy Data Challenge', tabName = 'exp4', icon = icon('gears')),
              menuItem('Combining Data Challenge', tabName = 'exp2', icon = icon('gamepad')),
              menuItem('References', tabName = "References", icon = icon("leanpub"))
              #menuItem('Creating Your Own Graph', tabName = 'exp3', icon = icon('refresh'))
      ),
      #PSU logo
      tags$div(class = "sidebar-logo",
               boastUtils::psu_eberly_logo("reversed"))
    ),

    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css"),
      ),
      tags$style(type = "text/css", ".content-wrapper,.right-side {background-color: white;}"),
      #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
      
      useShinyjs(),
      
      tabItems(
        tabItem(tabName = 'overview',
            h1('Data Wrangling App'),
            br(),
            p('This app illustrates R code for tidying, reshaping and combining data.'),
            br(),
            
            h2('Instructions'),
            p(tags$li('In the Exploring Data Wrangling section, go through each tab including unite, pivot_wider, pivot_longer, and arrange.')),
            p(tags$li('In the Tidy Data section, fill in the correct arguments that correctly tidy the data.')),
            p(tags$li('In the Combining Data section, click on the green button to select the transformation corresponding to each data table generated.')),
            br(),
            div(style = 'text-align: center',
                bsButton(inputId = 'go2',
                         label = 'Explore!',
                         icon = icon('bolt'),
                         size = 'large',
                         style = "success",
                         class = "circle grow")),
            br(),
            h2(strong('Acknowledgements:')),
            p('This application was coded and developed by Anna (Yinqi) Zhang 
              and Oluwafunke Alliyu. Special Thanks to Grace (Yubaihe) Zhou 
              for being incredibly helpful with programming issues. Bug fixes
              and style guide along with function updates were implented
              by Ethan Wright (2020).'),
            p('The cheat sheet is provided by RStudio.'),
            div(class = "updated", "Last Update: 07/23/2020 by EJW.")
            ),
        
        tabItem(tabName = 'prereq',
            p('Please refer to the', a(href = 'https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf', 'Data Wrangling cheatsheet', target="_blank"), 'for all the information needed. 
              This includes many examples of data wrangling tools for reshaping data and joins.'),
            p('After clicking on the link a separate tab will open that contains the cheat sheet.'),
            tags$a(href = 'https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf', tags$img(src = 'cheatsheet.png', align = 'left'))),
    
        
        
        ############ Reshaping Data ############
        tabItem(tabName = 'exp1', 
                tabsetPanel(type = 'tabs',
                            tabPanel(div(style = 'font-size: 125%', 'Unite'),
                                     br(),
                                     box(title = 'View An Example',
                                         width = NULL,
                                         style = 'background-color: #ffffff; display: inline-block',
                                         tableOutput('uniteOutput1'),
                                         checkboxGroupInput(inputId = 'unite1',
                                                            label = 'Select Columns to Unite',
                                                            inline = T,
                                                            choices = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb')),
                                         materialSwitch(inputId = 'unite3',
                                                        label = 'View the Transformed Data Set',
                                                        value = FALSE),
                                         uiOutput('uniteUI'),
                                         br(),
                                         tableOutput('uniteOutput2')
                                     )
                            ),
                            
                            tabPanel(div(style = 'font-size: 125%', 'pivot_longer'),
                                     br(),
                                     box(title = 'View An Example',
                                         width = NULL,
                                         style = 'background-color: #ffffff; display: inline-block',
                                         materialSwitch(inputId = 'dw1',
                                                        label = 'View the Transformed Data Set',
                                                        value = FALSE),
                                         tableOutput('dwTable1'),
                                         tags$strong(div('Gather columns into rows.', style = 'color: purple')),
                                         tags$code('tidyr::pivot_longer(cols = c("2011","2012","2013"), names_to = "year", values_to = "n")'),
                                         br(),
                                         br(),
                                         tableOutput('dwTable2')
                                     )
                            ),
                            
                            tabPanel(div(style = 'font-size: 125%', 'pivot_wider'),
                                     br(),
                                     box(title = 'View An Example',
                                         width = NULL,
                                         style = 'background-color: #ffffff; display: inline-block',
                                         materialSwitch(inputId = 'dw3',
                                                        label = 'View the Transformed Data Set',
                                                        value = FALSE),
                                         tableOutput('dwTable5'),
                                         tags$strong(div('0 rows into columns.', style = 'color: purple')),
                                         tags$code('tidyr::pivot_wider(names_from = "size", values_from = "amount")'),
                                         br(),
                                         br(),
                                         tableOutput('dwTable6')
                                     )
                            ),
                            
                            tabPanel(div(style = 'font-size: 125%', 'Arrange'),
                                     br(),
                                     box(title = 'View An Example',
                                         width = NULL,
                                         style = 'background-color: #ffffff; display: inline-block',
                                         fluidRow(
                                           column(4,
                                           selectInput(inputId = "dwSTI2", 
                                                        label = "Select Your Sorting Option",
                                                        choices = c('Random', 'Low to High', 'High to Low'),
                                                        selected = character(0),
                                                        width = '300px'),
                                           ),
                                           column(8,
                                           selectInput(inputId = "dwSTI1", 
                                                        label = "Mutating Joins Option",
                                                        choices = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec'),
                                                        selected = character(0),
                                                        width = '300px'))
                                         ),
                                         
                                         tags$strong('R code: '),
                                         tags$li('Order rows by values of a column (low to high)'),
                                         uiOutput('code1'),
                                         br(),
                                         tags$li('Order rows by values of a column (high to low)'),
                                         uiOutput('code2'),
                                         br(),
                                         br(),
                                         tableOutput('dwTable8'))
                            )
                )
        ),
        
        
        
############# Tidy Data #################



  tabItem(tabName = 'exp4',
          tabsetPanel(type = 'tabs', id = 'questionTabs',
                      tabPanel(div(style = 'font-size: 125%', 'pivot_longer 1'), value = "pivot_longer1",

      ###################### pivot_longer 1 ###########
                               box(
                                 wellPanel(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                 
                               
                            ##### The statement in the top box ############
                               h4(tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be a country in a year')),
                               h4(tags$code('tidyr::pivot_longer(RawData, cols = c("Arg 1", "Arg 2"), names_to = "Arg 3", values_to = "Arg 4")')))),
                               
                               textOutput("tester"),
                           ###### two parellel boxees, first for raw data second for Tidy Attempt ##########
                               fluidRow(
                                 box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'RawData')),
                                     style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("original1")),
                                 box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'Your Tidy Attempt')),
                                     style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("userOut1"),
                                     div(style = 'font-size: 135%; font-weight: bold'))),
                                 br(),
                          ####### Your R Code based on inputs #########
                               fluidRow(
                                 column(12,
                                        div(style = 'text-align: left; font-size: 125%',
                                            wellPanel(tags$strong('Your R code: '),
                                                      uiOutput('tidyAttemptTable'))))),
                                 br(),
                               
                               
                               fluidRow(
                                 # choices for user plot
                                 box(div(style = 'background-color: #ffffff',
                                         (title = '')),
                                     style = 'background: #ffffff',
                                     width = 3,
                                     selectInput(inputId = 'userOp1',
                                                 label = 'cols[1]=',
                                                 choices = c('country', '1999', 'Afghanistan', '2000'),
                                                 selected = 'country')),
                                 box(div(style = 'background-color: #ffffff',
                                         (title = '')),
                                    style = 'background: #ffffff',
                                    width = 3,
                                    selectInput(inputId = 'userOp2',
                                                label = 'cols[2]=',
                                                choices = c('country', '1999', 'Afghanistan', '2000'),
                                                selected = 'country')),
            
                                 box(div(style = 'background-color: #ffffff',
                                         (title = '')),
                                     style = 'background: #ffffff',
                                     width = 3,
                                     selectInput(inputId = 'userOp3',
                                                 label = 'names_to = ',
                                                 choices = c('1999', 'cases', 'year', 'China'),
                                                 selected = '1999')),
                                 
                                 
                                 box(div(style = 'background-color: #ffffff',
                                         (title = '')),
                                     style = 'background-color: #ffffff',
                                     width = 3,
                                     selectInput(inputId = 'userOp4',
                                                 label = 'values_to = ',
                                                 choices = c('1999', 'cases', 'year', 'China'),
                                                 selected = '1999')
                                     )
                                 ),
                               width = 10),
                               
                               fluidRow(
                               div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
                                   uiOutput("sub"),
                                   br(),
                                   uiOutput("resetcc"),
                                   ),
                               
                               br(),
                               br(),
                               
                               div(style = 'display: inline-block; position: relative; text-align: right',
                                   uiOutput('correct'),
                                   uiOutput('wrong')))
                      ),
                      
       ################## pivot_longer 2 ###################
                      
                      tabPanel(div(style = 'font-size: 125%', 'pivot_longer 2'), value = "pivot_longer2", #Ethan 7/8
                               
                          ######## Fill in the correct Argument across top###############
                               box(wellPanel(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                                 h4(tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be an employee on a certain day of the week')),
                                                 h4(tags$code('tidyr::pivot_longer(RawData2, cols = c("Arg 1", "Arg 2", "Arg 3"), names_to = "Arg 4", values_to = "Arg 5")')))),
                          ########## Raw Data Table ###########       

                                   fluidRow(
                                     box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                             (title = 'RawData2')),
                                         style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                         width = 6,
                                         uiOutput("original3")),
                                     box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                             (title = 'Your Tidy Attempt')),
                                         style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                         width = 6,
                                         uiOutput("tidyAttempt2"))),
                                   br(),
                          ######## Tidy Attempt #############
                                   fluidRow(
                                     column(12,
                                            div(style = 'text-align: left; font-size: 125%',
                                                wellPanel(tags$strong('Your R code: '),
                                                          uiOutput('userOutY'))
                                            ))),
                                   br(),
                                   
                              ############# The 5 Input values ##############
                                   fluidRow(
                                     # choices for user plot
                                     box(div(style = 'background-color: #ffffff',
                                             (title = '')),
                                         style = 'background: #ffffff',
                                         width = 3,
                                         selectInput(inputId = 'userOp5',
                                                     label = 'cols[1] =',
                                                     choices = c('John','MonTips','Age','Name'),
                                                     selected = 'John')),
                                     box(div(style = 'background-color: #ffffff',
                                             (title = '')),
                                         style = 'background: #ffffff',
                                         width = 3,
                                         selectInput(inputId = 'userOp6',
                                                     label = 'cols[2] =',
                                                     choices = c('Tim','Age','TueTips','21'),
                                                     selected = 'Tim')),
                                     box(div(style = 'background-color: #ffffff',
                                             (title = '')),
                                         style = 'background: #ffffff',
                                         width = 3,
                                         selectInput(inputId = 'userOp7',
                                                     label = 'cols[3] =',
                                                     choices = c('Age', 'Name', '7', 'WedTips'),
                                                     selected = 'Age')),
                                     box(div(style = 'background-color: #ffffff',
                                             (title = '')),
                                         style = 'background: #ffffff',
                                         width = 3,
                                         selectInput(inputId = 'userOp8',
                                                     label = 'names_to =',
                                                     choices = c('21','MonTips','Tips','Day'),
                                                     selected = '21')),
                                     
                                     
                                     box(div(style = 'background-color: #ffffff',
                                             (title = '')),
                                         style = 'background-color: #ffffff',
                                         width = 3,
                                         selectInput(inputId = 'userOp9',
                                                     label = 'values_to',
                                                     choices = c('8','Age','Day','Tips'),
                                                     selected = '8')
                                     )
                                   ),
                                   width = 10),
                               
                               fluidRow(
                               div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
                                   uiOutput("subbed"),
                                   br(),
                                   uiOutput("restart"),
                                   ),
                               br(),
                               br(),
                               div(style = 'display: inline-block; position: relative; text-align: right',
                                   uiOutput('cort'),
                                   uiOutput('rong')))
                      ),
                      
######################### Pivot Wider 1 ######################################
                      
                      tabPanel(div(style = 'font-size: 125%', 'pivot_wider 1'), value = "pivot_wider1",

                            ############ Fill in sample argument ##########
                               box(wellPanel(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                   h4(tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be a country')),
                                   h4(tags$code('tidyr::pivot_wider(RawData3, names_from = "Arg 1", values_from = "Arg 2")')))), 


                    ######### Raw Data and Tidy Attempt  ##########  
                               fluidRow(
                                 box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'RawData3')),
                                     style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("original2")),
                    ######### Your Tidy Attempt ##############
                                 box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'Your Tidy Attempt')),
                                     style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("userOutA"))),
                               br(),
                    ####### R code based on blanks #######
                               fluidRow(
                                 column(12,
                                        div(style = 'text-align: left; font-size: 125%',
                                            wellPanel(tags$strong('Your R code: '),
                                                      uiOutput(paste0('userOutB')))
                                        ))),
                               br(),
                               
                        #######Drop Down selections
                               fluidRow(
                                 # choices for user plot
                                 box(div(style = 'background-color: #ffffff',
                                         (title = '')),
                                     style = 'background: #ffffff',
                                     width = 3,
                                     selectInput(inputId = 'userOpA',
                                                 label = 'names_from =',
                                                 choices = c('population', '25', 'key', 'cases'),
                                                 selected = 'population')),
                                 box(div(style = 'background-color: #ffffff',
                                         (title = '')),
                                     style = 'background: #ffffff',
                                     width = 3,
                                     selectInput(inputId = 'userOpB',
                                                 label = 'values_from =',
                                                 choices = c('key', 'data', 'GDP', '1393'),
                                                 selected = 'key')),
                                 
                               ),
                               width = 10),
                               
                               fluidRow(
                                 div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
                                     uiOutput("bus"),
                                     br(),
                                     uiOutput("redo"),
                                     ),
                                 br(),
                                 br(),
                                 div(style = 'display: inline-block; position: relative; text-align: right',
                                     uiOutput('cor'),
                                     uiOutput('wro'))
                               )),
                      
 ####################### pivot_wider 2 #######################
                      
                      tabPanel(div(style = 'font-size: 125%', 'pivot_wider 2'), value = "pivot_wider2",
                               
                               
                               box(wellPanel(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                                 h4(tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be an employee')),
                                                 h4(tags$code('tidyr::pivot_wider(RawData4, names_from = "Arg 1", values_from = "Arg 2")')))),
                                             

                                   
                                   fluidRow(
                                     box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                             (title = 'RawData4')),
                                         style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                         width = 6,
                                         uiOutput("original4")),
                                     box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                             (title = 'Your Tidy Attempt')),
                                         style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                         width = 6,
                                         uiOutput("userOut3"))),
                                   br(),
                                   fluidRow(
                                     column(12,
                                            div(style = 'text-align: left; font-size: 125%',
                                                wellPanel(tags$strong('Your R code: '),
                                                          uiOutput('userOut4'))
                                            ))),
                                   br(),
                                   
                                   
                                   fluidRow(
                                     # choices for user plot
                                     box(div(style = 'background-color: #ffffff',
                                             (title = '')),
                                         style = 'background: #ffffff',
                                         width = 3,
                                         selectInput(inputId = 'userOpC',
                                                     label = 'names_from =',
                                                     choices = c('22', 'Age', 'Tips', 'Paycheck'),
                                                     selected = '22')),
                                     box(div(style = 'background-color: #ffffff',
                                             (title = '')),
                                         style = 'background: #ffffff',
                                         width = 3,
                                         selectInput(inputId = 'userOpD',
                                                     label = 'values_from =',
                                                     choices = c('Name', 'Dollars', 'Day', 'Wage'),
                                                     selected = 'Tim'))
                                   ),
                                   width = 10),
                               
                               fluidRow(
                                 div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
                                     uiOutput("buss"),
                                     br(),
                                     uiOutput("redos")),
                                 br(),
                                 br(),
                                 div(style = 'display: inline-block; position: relative; text-align: right',
                                     uiOutput('cors'),
                                     uiOutput('wros'))
                               )),
                      
                    
                 
################# ShinyAce ##################
                 
                 tabPanel(div(style = 'font-size: 125%', 'Live Code'),
                          
                          fluidRow(
                            column(6,
                                   verticalLayout(
                                     h3("Instructions"),
                                     wellPanel(
                                       style = "background-color: #ffffff",
                                       tags$div(
                                         tags$li("Attempt the questions."),
                                         tags$li("Run your code in the R script
                                                 box below and see the output on the right."),
                                         tags$li("Uncomment the sample code to explore."),
                                         style = "background-color: #ffffff")),
                                     h3("Exercises"),
                                     wellPanel(style = "background-color: #ffffff", #This panel is where the question and options go
                                               uiOutput("question") %>% 
                                                 withSpinner(color = "#ffffff"),
                                               br(),
                                               uiOutput("mark"),   #Shows symbol, what was picked and what should have been picked
                                               tags$style(type = 'text/css', '#question{font-size: 15px;
                                                          background-color: #ffffff;color: black;}',
                                                          '.well { padding: 10px; margin-bottom: 15px; max-width: 1000px; }')
                                               
                                     ),
                                     fluidPage(
                                       fluidRow(     #These are the 3 buttons on the bottom
                                         column(12, align = "center",
                                                div(style = "display: inline-block", actionButton(inputId = 'submit',
                                                                                                  label = 'Submit',
                                                                                                  style = "success")),
                                                div(style = "display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                                                div(style = "display: inline-block", bsButton(inputId = "nextq", 
                                                                                              label = "Next",
                                                                                              disabled = TRUE)),
                                                div(style = "display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                                                div(style = "display: inline-block", bsButton(inputId = "reset", 
                                                                                              label = "Restart",
                                                                                              style="danger", 
                                                                                              disabled = TRUE)))
                                       )),
                                     
                                     
                                     ##########try rlocker statement#########
                                     tags$samp(
                                       htmlOutput("statements")
                                     ),
                                     ##########end#############
                                     
                                     h3("Test Your Answer"),  
                                     uiOutput('editor'),
                                          column(6,
                                                 withBusyIndicatorUI(
                                                 actionButton("eval", "Run")))
                                          )),
                                   br(),
                            h3("Original Table"),
                            column(6,
                                   uiOutput('tableinfo'),
                                   uiOutput('acetable')),
                            column(6,
                                   h3("Knitted Output"),
                                   htmlOutput("knitDoc")
                                   )
                            )
                          )
)
),

############ Combining Data Sets ############    
    tabItem(tabName = 'exp2',
           
            fluidRow(
              column(width = 12,
                     box(div(style = 'font-weight: bold; font-size: 140%', (title = 'Multiple Choice Joins Practice')),
                         style = 'text-align: left',
                         width = NULL,
                         height = NULL,    
                         div(style="display:inline-block", tableOutput('titleTableA')),
                         div(style="display:inline-block", p('    +     ')),
                         div(style="display:inline-block", tableOutput('titleTableB')),
                         div(style="display:inline-block", p(' = ')),
                        
            br(),
            br(),
            
            fluidRow(#theme = "bootstrap.css",
              column(width = 4,
                     box(title = NULL,
                         width = '150px',
                         height = '400px',
                         
                         radioButtons(inputId = "cd1", label = "Mutating Joins Option",
                                     c('left join', 'right join', 'inner join', 'full join'),
                                     selected = character(0)),
                         #Ethan
                         tableOutput('cdTable1'),
                         fluidRow(
                           column(4,bsButton(inputId = 'check1',
                                    label = 'Check',
                                    size = 'median')),
                          
                           column(8,(uiOutput('checkOrX1')), offset = 0))
                         )
                     ),
              
              column(width = 4,
                     box(title = NULL,
                         style = 'background-color: #ffffff',
                         width = '150px',
                         height = '400px',
                         radioButtons(inputId = "cd2", label = "Mutating Joins Option",
                                      c('left join', 'right join', 'inner join', 'full join'),
                                      selected = character(0)),
                         
                         tableOutput('cdTable2'),
                         fluidRow(
                           column(4,bsButton(inputId = 'check2',
                                             label = 'Check',
                                             size = 'median')),
                           
                           #column(2,"jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj                          "),
                           column(8,(uiOutput('checkOrX2')), offset = 0))
                         )
              ),
              
              column(width = 4,
                     box(title = NULL,
                         style = 'background-color: #ffffff',
                         width = '150px',
                         height = '400px',
                         radioButtons(inputId = "cd3", label = "Filtering Joins Option",
                                      c('semi join', 'anti join'), selected = character(0)),
                         tableOutput('cdTable3'),
                         br(),
                         fluidRow(
                           column(4,bsButton(inputId = 'check3',
                                             label = 'Check',
                                             size = 'median')),
                           column(8,(uiOutput('checkOrX3')), offset = 0)))
                     ),
              
              column(width = 4,
                     box(title = NULL,
                         style = 'background-color: #ffffff',
                         width = '150px',
                         height = '400px',
                         radioButtons(inputId = "cd4", label = "Filtering Joins Option",
                         c('semi join', 'anti join'), selected = character(0)),
                         tableOutput('cdTable4'),
                         fixedRow(
                           column(4,bsButton(inputId = 'check4',
                                             label = 'Check',
                                             size = 'median')),
                           column(8,(uiOutput('checkOrX4')), offset = 0)))
                     ),
              
              column(width = 4,
                     box(title = NULL,
                         style = 'background-color: #ffffff',
                         width = '150px',
                         height = '400px',
                         radioButtons(inputId = "cd5", label = "Filtering Joins Option",
                                      c('left join', 'right join', 'inner join', 'full join'),
                                      selected = character(0)),
                         tableOutput('cdTable5'),
                         fluidRow(
                           column(4,bsButton(inputId = 'check5',
                                             label = 'Check',
                                             size = 'median')),
                           column(8,(uiOutput('checkOrX5')), offset = 0)))
                     ),
              
              column(width = 4,
                     box(title = NULL,
                         style = 'background-color: #ffffff',
                         width = '150px',
                         height = '400px',
                         radioButtons(inputId = "cd6", label = "Mutating Joins Option",
                                      c('left join', 'right join', 'inner join', 'full join'),
                                      selected = character(0)),
                         tableOutput('cdTable6'),
                         fixedRow(
                           column(4,bsButton(inputId = 'check6',
                                             label = 'Check',
                                             size = 'median')),
                           column(8,(uiOutput('checkOrX6')), offset = 0))))))
                     )
            )
            ),
#######References Tab ###############
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          
          p(class = "hangingindent",
            "Attali, D. (2020). 
  shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds, R package. 
  Available from https://CRAN.R-project.org/package=shinyjs"),
          
          p(class = "hangingindent",
            "Bailey E. (2015). 
  shinyBS: Twitter Bootstrap Components for Shiny, R package.
  Available from https://CRAN.R-project.org/package=shinyBS"),
          
          p(class = "hangingindent", 
            "Barret Schloerke, JJ Allaire and Barbara Borges (2020). 
  learnr: Interactive Tutorials for R, R package. 
  Available from https://CRAN.R-project.org/package=learnr"),
          
          p(class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2018),
  shinydashboard: Create Dashboards with 'Shiny', R package. 
  Available from https://CRAN.R-project.org/package=shinydashboard"),
          
          p(class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and MchPherson, J. (2020),
  shiny: Web Application Framework for R, R package.
  Available from https://CRAN.R-project.org/package=shiny"),
          
          p(class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). 
  boastUtils: BOAST Utilities, R package.
  Available from https://github.com/EducationShinyAppTeam/boastUtils"),
          
          p(class = "hangingindent",
            "Carey R. (2019). 
  rlocker: Learning Locker for Shiny, R package. 
  Available from https://github.com/rpc5102/rlocker"),
          
          p(class = "hangingindent",
            "Grolemund, G. (2020). 
  EDAWR: Expert Data Analysis with R, R package.
  Available from http://github.com/rstudio/EDAWR"),
          
          p(class = "hangingindent",
            "JJ Allaire and Yihui Xie and Jonathan McPherson and Javier Luraschi
            and Kevin Ushey and Aron Atkins and Hadley Wickham and Joe Cheng 
            and Winston Chang and Richard Iannone (2020). 
  rmarkdown: Dynamic Documents for R, R package.
  Available from URL https://rmarkdown.rstudio.com."),
          
          p(class = "hangingindent",
            "Nijs, V., Fang, F., Trestle Technology, LLC and Allen, J. (2019). 
  shinyAce: Ace Editor Bindings for Shiny, R package.
  Available from https://CRAN.R-project.org/package=shinyAce"),
          
          
          p(class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020). 
  shinyWidgets: Custom Inputs Widgets for Shiny, R package. 
  Available from https://CRAN.R-project.org/package=shinyWidgets"),
          
          p(class = "hangingindent",
            "Sali, A. and Attali, D. (2020). 
  shinycssloaders: Add CSS Loading Animations to 'shiny' Outputs, R package.
  Available from https://CRAN.R-project.org/package=shinycssloaders"),
          
          p(class = "hangingindent",
            "Soltoff, B. (2020). 
  rcfss: Helper functions and datasets for UChicago course 
  on Computing for the Social Sciences, R package.
  Available from https://rdrr.io/github/uc-cfss/rcfss/"),
          
          p(class = "hangingindent",
            "Wickham, H. and Lionel, H. (2020).
  tidyr: Tidy Messy Data, R package.
  Available from https://CRAN.R-project.org/package=tidyr"),
          
          p(class = "hangingindent",
            "Wickham, H., Francois, R., Henry L., and Muller K. (2020).  
  dplyr: A Grammar of Data Manipulation, R package. 
  Available from https://CRAN.R-project.org/package=dplyr"),
          
          p(class = "hangingindent",
            "Xie, Y., (2020). 
  knitr: A General-Purpose Package for Dynamic Report Generation in R, R package.
  Available from https://cran.r-project.org/web/packages/knitr/index.html"),
        )
        
)) ))