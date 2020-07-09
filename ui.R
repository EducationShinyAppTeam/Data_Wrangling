library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
#library(shinyDND) #No match
library(shinyWidgets)
#library(dplyr) #just %>% though I think others not seen
#library(mosaic) #nothing detected but tends to throw errors
#library(plotly) # just %>%, think can be deleted
#library(ggplot2) #think can be deleted
library(EDAWR)
#library(plot3D) #nothing
#library(ggmap) #just %>%
library(tidyr) #just %>% could have more problems
library(shinyAce) #nothing, but labeled as header in live code section
#library(devtools) #nothing
library(shinycssloaders) 
#library(knitr) #nothing
library(rlocker) #think this is for later
#library(rcfss)   #input
library(boastUtils) #should keep




bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)


source("helpers.R")
ui <- list(
  dashboardPage(
    skin = "green",
      dashboardHeader(title = 'Data Wrangling',
                      titleWidth = 250,
                      tags$li(class = "dropdown",tags$a(href = "https://shinyapps.science.psu.edu/",icon("home"))),
                      tags$li(class = "dropdown",actionLink("info", icon("info"), class = "myClass"))),
        
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
            #h4(tags$li('In the Data Visualization section, go through each tab including 3D plots, line plots, contour plots, and heat maps.')),
            p(tags$li('In the Exploring Data Wrangling section, go through each tab including unite, gather, spread, data_frame, and arrange.')),
            p(tags$li('In the Tidy Data section, fill in the correct arguments that correctly tidies the data.')),
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
            p('This application was coded and developed by Anna (Yinqi) Zhang and Oluwafunke Alliyu. Special Thanks to Grace (Yubaihe) Zhou for being incredibly helpful with programming issues.'),
            p('The cheat sheet is provided by RStudio.'),
            p('Packages used: dplyr, EDAWR, ggmap, mosaic, plotly, ggplot2, plot3D.'),
            div(class = "updated", "Last Update: 06/23/2020 by EJW.")
            ),
        
        tabItem(tabName = 'prereq',
            p('Please refer to the', a(href = 'https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf', 'Data Wrangling cheatsheet', target="_blank"), 'for all the information needed. 
              This includes many examples of data wrangling tools for reshaping data and joins.'),
            p('After clicking on the link a seporate tab will open that contains the cheat sheet.'),
            tags$a(href = 'https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf', tags$img(src = 'cheatsheet.png', align = 'left'))),
    
############# Tidy Data #################



  tabItem(tabName = 'exp4',
          tabsetPanel(type = 'tabs', id = 'questionTabs',
                      tabPanel(div(style = 'font-size: 125%', 'pivot_longer 1'), value = "pivot_longer1",

      ###################### pivot_longer 1 ###########
                               box(
                                 wellPanel(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                   #titlePanel("Tidy the Original Data"),
                               #div(style = 'text-align: center',
                                 #  h1(strong('Tidy the Original Data')),
                               #div(style = 'text-align: center',
                               
                               
                            ##### The statement in the top box ############
                               h4(tags$b('Fill in the Correct Arguments to Tidy the Data - a case is a country in a year')),
                               h4(tags$code('tidyr::pivot_longer(RawData, cols = c("Arg 1", "Arg 2"), names_to = "Arg 3", values_to = "Arg 4")')))),
                               
                               #pivot_longer(df,cols = c('1999','2000'), names_to = 'years', values_to = 'cases')

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
                                                selected = '1999')),
            
                                 box(div(style = 'background-color: #ffffff',
                                         (title = '')),
                                     style = 'background: #ffffff',
                                     width = 3,
                                     selectInput(inputId = 'userOp3',
                                                 label = 'names_to = ',
                                                 choices = c('1999', 'cases', 'year', 'China'),
                                                 selected = 'year')),
                                 
                                 
                                 box(div(style = 'background-color: #ffffff',
                                         (title = '')),
                                     style = 'background-color: #ffffff',
                                     width = 3,
                                     selectInput(inputId = 'userOp4',
                                                 label = 'values_to = ',
                                                 choices = c('1999', 'cases', 'year', 'China'),
                                                 selected = 'cases')
                                     )
                                 ),
                               width = 10),
                               
                               fluidRow(
                               div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
                                   uiOutput("sub"),
                                   br(),
                                   uiOutput("resetcc"),
                                   uiOutput("nextQuestion1")),
                               
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
                                                 h4(tags$b('Fill in the Correct Arguments to Tidy the Data - a case is a country in a year')),
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
                                   
                              ############# The 5  Input values ##############3
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
                                                     choices = c('Tim','MonTips','TueTips','21'),
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
                                   uiOutput("restart")),
                               br(),
                               br(),
                               div(style = 'display: inline-block; position: relative; text-align: right',
                                   uiOutput('cort'),
                                   uiOutput('rong')))
                      ),
                      
######################### Pivot Wider 1 ######################################
                      
                      tabPanel(div(style = 'font-size: 125%', 'pivot_wider 1'),

                            ############ Fill in sample argument
                               box(wellPanel(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                   #titlePanel("Tidy the Original Data"),
                                   #div(style = 'text-align: center',
                                   #  h1(strong('Tidy the Original Data')),
                                   #div(style = 'text-align: center',
                                   h4(tags$b('Fill in the Correct Arguments to Tidy the Data - a case is a country in a year')),
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
                                                      uiOutput('userOutB'))
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
                                                 choices = c('2000', 'cases', 'country', 'year'),
                                                 selected = '2000')),
                                 box(div(style = 'background-color: #ffffff',
                                         (title = '')),
                                     style = 'background: #ffffff',
                                     width = 3,
                                     selectInput(inputId = 'userOpB',
                                                 label = 'values_from',
                                                 choices = c('country', 'cases', '2000', 'Afghanistan'),
                                                 selected = 'country')),
                                 
                               ),
                               width = 10),
                               
                               fluidRow(
                                 div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
                                     uiOutput("bus"),
                                     br(),
                                     uiOutput("redo")),
                                 br(),
                                 br(),
                                 div(style = 'display: inline-block; position: relative; text-align: right',
                                     uiOutput('cor'),
                                     uiOutput('wro'))
                               )),
                      
                      #### Spread 2 ####
                      
                      tabPanel(div(style = 'font-size: 125%', 'pivot_wider 2'),
                               
                               
                               box(wellPanel(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                                 #titlePanel("Tidy the Original Data"),
                                                 #div(style = 'text-align: center',
                                                 #  h1(strong('Tidy the Original Data')),
                                                 #div(style = 'text-align: center',
                                                 h4(tags$b('Fill in the Correct Arguments to Tidy the Data - a case is a country in a year')),
                                                 h4(tags$code('tidyr::gather(RawData4,"Arg 1", "Arg 2", names_to = "Arg 3", values_to = "Arg 4")')))),
                                             

                                   
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
                                         selectInput(inputId = 'userOpJ',
                                                     label = 'cols[1]',
                                                     choices = c('country', '1999', '2000', 'Afghanistan'),
                                                     selected = '1999')),
                                     box(div(style = 'background-color: #ffffff',
                                             (title = '')),
                                         style = 'background: #ffffff',
                                         width = 3,
                                         selectInput(inputId = 'userOpK',
                                                     label = 'cols[2]',
                                                     choices = c('country', '1999', '2000', 'Afghanistan'),
                                                     selected = '1999')),
                                     
                                     box(div(style = 'background-color: #ffffff',
                                             (title = '')),
                                         style = 'background: #ffffff',
                                         width = 3,
                                         selectInput(inputId = 'userOpL',
                                                     label = 'names_to',
                                                     choices = c('population', 'country', 'Brazil', 'year'),
                                                     selected = 'population')),
                                     
                                     
                                     box(div(style = 'background-color: #ffffff',
                                             (title = '')),
                                         style = 'background: #ffffff',
                                         width = 3,
                                         selectInput(inputId = 'userOpM',
                                                     label = 'values_to',
                                                     choices = c('population', 'country', 'Brazil', 'year'),
                                                     selected = 'year')
                                     )
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
                                         tags$li("Attempt the questions!"),
                                         tags$li("Run your code in the R script
                                                 box below and see the output on the right"),
                                         tags$li("Uncomment the sample code to explore."),
                                         style = "background-color: #ffffff")),
                                     h3("Exercises"),
                                     #uiOutput('progress'), does not appear to serve any purpose
                                     wellPanel(style = "background-color: #ffffff", #This panel is where the question and options go
                                               uiOutput("question") %>% 
                                                 withSpinner(color = "#ffffff"),
                                               uiOutput("options"),  #Outputs all of the possible answers, however this should be output with multiple choice.
                                               br(),
                                               #selectInput("answer", "Answer:", c("","A", "B", "C")), #Ethan
                                               uiOutput("mark"),   #Shows symbol, what was picked and what should have been picked
                                               tags$style(type = 'text/css', '#question{font-size: 15px;
                                                          background-color: #ffffff;color: black;}',
                                                          '.well { padding: 10px; margin-bottom: 15px; max-width: 1000px; }')
                                               
                                     ),
                                     fluidPage(
                                       # tags$head(
                                       #   tags$style(HTML('#submit{background-color:#ffffff; color:white}')),
                                       #   tags$style(HTML('#eval{background-color:#ffffff; color:white}')),      removing this actually made code work better.
                                       #   tags$style(HTML('#nextq{background-color:#ffffff; color:white}'))
                                       # ),
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
                        
                        tabPanel(div(style = 'font-size: 125%', 'Gather'),
                                 br(),
                                 box(title = 'View An Example',
                                     width = NULL,
                                     style = 'background-color: #ffffff; display: inline-block',
                                     materialSwitch(inputId = 'dw1',
                                                    label = 'View the Transformed Data Set',
                                                    value = FALSE),
                                     tableOutput('dwTable1'),
                                     tags$strong(div('Gather columns into rows.', style = 'color: purple')),
                                     tags$code('tidyr::gather(cases, "year", "n", 2:4)'),
                                     br(),
                                     br(),
                                     tableOutput('dwTable2')
                                     )
                                 ),
                        
                        tabPanel(div(style = 'font-size: 125%', 'Spread'),
                                 br(),
                                 box(title = 'View An Example',
                                     width = NULL,
                                     style = 'background-color: #ffffff; display: inline-block',
                                     materialSwitch(inputId = 'dw3',
                                                    label = 'View the Transformed Data Set',
                                                    value = FALSE),
                                     tableOutput('dwTable5'),
                                     tags$strong(div('Spread rows into columns.', style = 'color: white')),
                                     tags$code('tidyr::spread(pollution, size, amount)'),
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
                                     sliderTextInput(inputId = 'dwSTI2',
                                                     label = 'Select Your Sorting Option',
                                                     choices = c('Random', 'Low to High', 'High to Low'),
                                                     grid = TRUE,
                                                     width = '200px',
                                                     force_edges = TRUE),
                                     sliderTextInput(inputId = 'dwSTI1',
                                                     label = 'Select the Variable to Sort By', 
                                                     choices = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec'), 
                                                     grid = TRUE,
                                                     width = '400px',
                                                     force_edges = TRUE),
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

############ Combining Data Sets ############    
    tabItem(tabName = 'exp2',
           
            fluidRow(
              column(width = 12,
                     box(div(style = 'font-weight: bold; font-size: 140%', (title = 'Click the lightbulb to choose the correct join function corresponding to the displayed data!')),
                         style = 'text-align: left',
                         width = NULL,
                         height = NULL,    #Ethan Wright
                         div(style="display:inline-block", tableOutput('titleTableA')),
                         div(style="display:inline-block", p('    +     ')),
                         div(style="display:inline-block", tableOutput('titleTableB')),
                         div(style="display:inline-block", p(' = ')),
                             
                         #tags$img(src = 'combine.png', width = '300px', height = NULL),

              
            br(),
            br(),
            
            fluidRow(#theme = "bootstrap.css",
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #ffffff',
                         width = NULL,
                         height = '330px',
                         
                         radioButtons(inputId = "cd1", label = "Mutating Joins Option",
                                     c('left join', 'right join', 'inner join', 'full join'),
                                     selected = character(0)),
                         
                         
                         #Ethan
                         tableOutput('cdTable1'),
                         fixedRow(
                           column(1,bsButton(inputId = 'check1',
                                    label = 'Check',
                                    size = 'median')),
                           column(4,(uiOutput('checkOrX1')), offset = 1))
                         )
                     ),
              
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #ffffff',
                         width = NULL,
                         height = '330px',
                         radioButtons(inputId = "cd2", label = "Mutating Joins Option",
                                      c('left join', 'right join', 'inner join', 'full join'),
                                      selected = character(0)),
                         # Old method
                         # dropdownButton(circle = TRUE,
                         #                status = 'success',
                         #                size = 'xs',
                         #                icon = icon('lightbulb'),
                         #                up = TRUE,
                         #                right = TRUE,
                         #                tooltip = tooltipOptions(title = "Select Your Answer"),
                         #                sliderTextInput(inputId = 'cd2',
                         #                                label = 'Mutating Joins Option',
                         #                                force_edges = TRUE,
                         #                                grid = TRUE,
                         #                                choices = c('left join', 'right join', 'inner join', 'full join')),
                         #   textOutput('cdExp2')),
                         tableOutput('cdTable2'),
                         fixedRow(
                           column(1,bsButton(inputId = 'check2',
                                             label = 'Check',
                                             size = 'median')),
                           column(4,(uiOutput('checkOrX2')), offset = 1))
                         )
              ),
              
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #ffffff',
                         width = NULL,
                         height = '270px',
                         radioButtons(inputId = "cd3", label = "Filtering Joins Option",
                                      c('semi join', 'anti join'), selected = character(0)),
                         tableOutput('cdTable3'),
                         br(),
                         fixedRow(
                           column(1,bsButton(inputId = 'check3',
                                             label = 'Check',
                                             size = 'median')),
                           column(4,(uiOutput('checkOrX3')), offset = 1)))
                     ),
              
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #ffffff',
                         width = NULL,
                         height = '270px',
                         radioButtons(inputId = "cd4", label = "Filtering Joins Option",
                         c('semi join', 'anti join'), selected = character(0)),
                         tableOutput('cdTable4'),
                         fixedRow(
                           column(1,bsButton(inputId = 'check4',
                                             label = 'Check',
                                             size = 'median')),
                           column(4,(uiOutput('checkOrX4')), offset = 1)))
                     ),
              
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #ffffff',
                         width = NULL,
                         height = NULL,
                         radioButtons(inputId = "cd5", label = "Filtering Joins Option",
                                      c('left join', 'right join', 'inner join', 'full join'),
                                      selected = character(0)),
                         tableOutput('cdTable5'),
                         fixedRow(
                           column(1,bsButton(inputId = 'check5',
                                             label = 'Check',
                                             size = 'median')),
                           column(4,(uiOutput('checkOrX5')), offset = 1)))
                     ),
              
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #ffffff',
                         width = NULL,
                         height = NULL,
                         radioButtons(inputId = "cd6", label = "Mutating Joins Option",
                                      c('left join', 'right join', 'inner join', 'full join'),
                                      selected = character(0)),
                         tableOutput('cdTable6'),
                         fixedRow(
                           column(1,bsButton(inputId = 'check6',
                                             label = 'Check',
                                             size = 'median')),
                           column(4,(uiOutput('checkOrX6')), offset = 1))
                         ))))
                     )
            )
            ),
#######Referencees Tab ###############
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(class = "hangingindent",
            "Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan
  McPherson (2020). shiny: Web Application Framework for R. R package
  version 1.4.0.2. https://CRAN.R-project.org/package=shiny"),
          p(class = "hangingindent",
            "Winston Chang and Barbara Borges Ribeiro (2018). shinydashboard: Create
  Dashboards with 'Shiny'. R package version 0.7.1.
  https://CRAN.R-project.org/package=shinydashboard"),
          p(class = "hangingindent",
            "Eric Bailey (2015). shinyBS: Twitter Bootstrap Components for Shiny. R package
  version 0.61. https://CRAN.R-project.org/package=shinyBS"),
          p(class = "hangingindent",
            "Dean Attali (2020). shinyjs: Easily Improve the User Experience of Your Shiny
  Apps in Seconds. R package version 1.1.
  https://CRAN.R-project.org/package=shinyjs"),
          p(class = "hangingindent",
            "Victor Perrier, Fanny Meyer and David Granjon (2020). shinyWidgets: Custom
  Inputs Widgets for Shiny. R package version 0.5.3.
  https://CRAN.R-project.org/package=shinyWidgets"),
          p(class = "hangingindent",
            "'Garrett Grolemund' (2020). EDAWR: Expert Data Analysis with R. R package
  version 0.1. http://github.com/rstudio/EDAWR"),
          p(class = "hangingindent",
            "Hadley Wickham and Lionel Henry (2020). tidyr: Tidy Messy Data. R package
  version 1.1.0. https://CRAN.R-project.org/package=tidyr"),
          p(class = "hangingindent",
            "Vincent Nijs, Forest Fang, Trestle Technology, LLC and Jeff Allen (2019).
  shinyAce: Ace Editor Bindings for Shiny. R package version 0.4.1.
  https://CRAN.R-project.org/package=shinyAce"),
          p(class = "hangingindent",
            "Andras Sali and Dean Attali (2020). shinycssloaders: Add CSS Loading
  Animations to 'shiny' Outputs. R package version 0.3.
  https://CRAN.R-project.org/package=shinycssloaders"),
          p(class = "hangingindent",
            "Robert Carey (2019). rlocker: Learning Locker for Shiny. R package version
  0.2.3. https://github.com/rpc5102/rlocker"),
          p(class = "hangingindent",
            "Robert Carey and Neil Hatfield (2020). boastUtils: BOAST Utilities. R package
  version 0.1.4. https://github.com/EducationShinyAppTeam/boastUtils"),
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          # Repeat as needed
        )
        
)) ))






# 
# 
# library(shiny)
# library(shinydashboard)
# library(shinyBS)
# library(shinyjs)
# #library(shinyDND) #No match
# library(shinyWidgets)
# #library(dplyr) #just %>% though I think others not seen
# #library(mosaic) #nothing detected but tends to throw errors
# #library(plotly) # just %>%, think can be deleted
# #library(ggplot2) #think can be deleted
# library(EDAWR)
# #library(plot3D) #nothing
# #library(ggmap) #just %>%
# library(tidyr) #just %>% could have more problems
# library(shinyAce) #nothing, but labeled as header in live code section
# #library(devtools) #nothing
# library(shinycssloaders) 
# #library(knitr) #nothing
# library(rlocker) #think this is for later
# library(rcfss)   #input
# library(boastUtils) #should keep
