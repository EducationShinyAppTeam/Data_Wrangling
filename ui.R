library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(shinyWidgets)
library(dplyr)
library(mosaic)
library(plotly)
library(ggplot2)
library(EDAWR)
library(plot3D)
library(ggmap)
library(tidyr)
library(shinyAce)
library(devtools)
library(shinycssloaders)
library(devtools)
library(knitr)
library(rlocker)
library(rcfss)



bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)


source("helpers.R")

header = dashboardHeader(title = 'Data Wrangling',
                         tags$li(class = "dropdown",
                                 tags$a(href = "https://shinyapps.science.psu.edu/",
                                        icon("home"))),
                         tags$li(class = "dropdown",
                                 actionLink("info", icon("info"), class = "myClass")))

sidebar = dashboardSidebar(
  sidebarMenu(id = 'tabs',
              menuItem('Prerequisite', tabName = 'prereq', icon = icon('book')),
              menuItem('Overview', tabName = 'overview', icon = icon("dashboard")),
              menuItem('Explore Data Wrangling', tabName = 'exp1', icon = icon('wpexplorer')),
              menuItem('Tidy Data Challenge', tabName = 'exp4', icon = icon('broom')),
              menuItem('Combining Data Sets', tabName = 'exp2', icon = icon('gamepad'))
              #menuItem('Creating Your Own Graph', tabName = 'exp3', icon = icon('refresh'))
  )
)

body = dashboardBody(
  tags$head( 
    tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
  ),
  tags$style(type = "text/css", ".content-wrapper,.right-side {background-color: white;}"),
  #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  
  useShinyjs(),
  
  tabItems(
    tabItem(tabName = 'prereq',
            h4('Please refer to the', a(href = 'https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf', 'Data Wrangling cheatsheet'), 'for all the information needed.'),
            tags$a(href = 'https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf', tags$img(src = 'cheatsheet.png', align = 'left'))),
    
    tabItem(tabName = 'overview',
            tags$a(href='http://stat.psu.edu/', tags$img(src = 'psu_icon.jpg', align = "left", width = 180)),
            br(),
            br(),
            br(),
            h3(strong('About:')),
            h4('This app illustrates R code for tidying, reshaping and combining data.'),
            br(),
            
            h3(strong('Instructions:')),
            #h4(tags$li('In the Data Visualization section, go through each tab including 3D plots, line plots, contour plots, and heat maps.')),
            h4(tags$li('In the Tidy Data section, fill in the correct arguments that coreectly tidies the data.')),
            h4(tags$li('In the Reshaping Data section, go through each tab including unite, gather, spread, data_frame, and arrange.')),
            h4(tags$li('In the Combining Data Set section, click on the green button to select the transformation corresponding to each data table generated.')),
            br(),
            div(style = 'text-align: center',
                bsButton(inputId = 'go2',
                         label = 'Explore!',
                         icon = icon('bolt'),
                         size = 'large',
                         style = "success",
                         class = "circle grow")),
            br(),
            h3(strong('Acknowledgements:')),
            h4('This application was coded and developed by Anna (Yinqi) Zhang. Special Thanks to Grace (Yubaihe) Zhou for being incredibly helpful with programming issues.'),
            h4('The cheat sheet is provided by RStudio.'),
            h4('Packages used: dplyr, EDAWR, ggmap, mosaic, plotly, ggplot2, plot3D.'),
            h4('The Protein-Protein Interaction Dataset is from the Warwick University - Molecular Organisation and Assembly in Cells.')
            ),

############# Tidy Data #################

#### Gather 1 ####

  tabItem(tabName = 'exp4',
          tabsetPanel(type = 'tabs',
                      tabPanel(div(style = 'font-size: 125%', 'Gather 1'),

                               
                               box(
                                 wellPanel(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                   #titlePanel("Tidy the Original Data"),
                               #div(style = 'text-align: center',
                                 #  h1(strong('Tidy the Original Data')),
                               #div(style = 'text-align: center',
                               
                               h4(tags$b('Fill in the Correct Arguments to Tidy the Data - a Case is a Year in a Country')),
                               h4(tags$code('tidyr::gather(RawData,`Arg 1`, `Arg 2`, key = "Arg 3", value = "Arg 4")')))),
                               
                                 

                            
                               fluidRow(
                                 box(div(style = 'background-image: url("green.png"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'RawData')),
                                     style = 'background-image: url("green.png"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("original1")),
                                 box(div(style = 'background-image: url("green.png"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'Your Tidy Attempt')),
                                     style = 'background-image: url("green.png"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("userOut1"),
                                     div(style = 'font-size: 135%; font-weight: bold'))),
                                 br(),
                               fluidRow(
                                 column(12,
                                        div(style = 'text-align: left; font-size: 125%',
                                            wellPanel(tags$strong('Your R code: '),
                                                      uiOutput('userOut2'))))),
                                 br(),
                               
                               
                               fluidRow(
                                 # choices for user plot
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOp1',
                                                 label = 'Arg 1',
                                                 choices = c('country', '1999', 'Afghanistan', '2000'),
                                                 selected = 'country')),
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                    style = 'background: #b8f28c',
                                    width = 3,
                                    selectInput(inputId = 'userOp2',
                                                label = 'Arg 2',
                                                choices = c('country', '1999', 'Afghanistan', '2000'),
                                                selected = '1999')),
            
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOp3',
                                                 label = 'Arg 3',
                                                 choices = c('1999', 'cases', 'year', 'China'),
                                                 selected = 'year')),
                                 
                                 
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background-color: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOp4',
                                                 label = 'Arg 4',
                                                 choices = c('1999', 'cases', 'year', 'China'),
                                                 selected = 'cases')
                                     )
                                 ),
                               width = 10),
                               
                               fluidRow(
                               div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
                                   uiOutput("sub"),
                                   br(),
                                   uiOutput("resetcc")),
                               
                               br(),
                               br(),
                               
                               div(style = 'display: inline-block; position: relative; text-align: right',
                                   uiOutput('correct'),
                                   uiOutput('wrong')))
                      ),
                      
                      #### Gather 2 ####         
                      
                      tabPanel(div(style = 'font-size: 125%', 'Gather 2'),
                               
                               
                               box(wellPanel(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                                 #titlePanel("Tidy the Original Data"),
                                                 #div(style = 'text-align: center',
                                                 #  h1(strong('Tidy the Original Data')),
                                                 #div(style = 'text-align: center',
                                                 h4(tags$b('Fill in the Correct Arguments to Tidy the Data - a Case is a country in a year')),
                                                 h4(tags$code('tidyr::gather(RawData2,`Arg 1`, `Arg 2`, key = "Arg 3", value = "Arg 4")')))),
                                             
                                             #fluidRow(
                                             #sidebarPanel(
                                             #  div(style = 'text-align: left',
                                             #     bsButton("newtable",
                                             #             label = "New Table",
                                             #            icon("arrow-circle-right"),
                                             #           size = "medium",
                                             #          style = 'success')),
                                             #  br(),
                                   # bsButton("submit",
                                   #         label = "Check Answer",
                                   #        icon("lightbulb"),
                                   #       size = "medium",
                                   #      style = 'success'),
                                   #width = 1)),#),
                                   
                                   
                                   fluidRow(
                                     box(div(style = 'background-image: url("green.png"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                             (title = 'RawData3')),
                                         style = 'background-image: url("green.png"); background-position: center; text-align: center',
                                         width = 6,
                                         uiOutput("original3")),
                                     box(div(style = 'background-image: url("green.png"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                             (title = 'Your Tidy Attempt')),
                                         style = 'background-image: url("green.png"); background-position: center; text-align: center',
                                         width = 6,
                                         uiOutput("userOutX"))),
                                   br(),
                                   fluidRow(
                                     column(12,
                                            div(style = 'text-align: left; font-size: 125%',
                                                wellPanel(tags$strong('Your R code: '),
                                                          uiOutput('userOutY'))
                                            ))),
                                   br(),
                                   
                                   
                                   fluidRow(
                                     # choices for user plot
                                     box(div(style = 'background-color: #b8f28c',
                                             (title = '')),
                                         style = 'background: #b8f28c',
                                         width = 3,
                                         selectInput(inputId = 'userOp5',
                                                     label = 'Arg 1',
                                                     choices = c('country', '1999', 'Afghanistan', '2000'),
                                                     selected = 'country')),
                                     box(div(style = 'background-color: #b8f28c',
                                             (title = '')),
                                         style = 'background: #b8f28c',
                                         width = 3,
                                         selectInput(inputId = 'userOp6',
                                                     label = 'Arg 2',
                                                     choices = c('country', '1999', 'Afghanistan', '2000'),
                                                     selected = '1999')),
                                     
                                     box(div(style = 'background-color: #b8f28c',
                                             (title = '')),
                                         style = 'background: #b8f28c',
                                         width = 3,
                                         selectInput(inputId = 'userOp7',
                                                     label = 'Arg 3',
                                                     choices = c('1999', 'cases', 'year', 'China'),
                                                     selected = 'year')),
                                     
                                     
                                     box(div(style = 'background-color: #b8f28c',
                                             (title = '')),
                                         style = 'background-color: #b8f28c',
                                         width = 3,
                                         selectInput(inputId = 'userOp8',
                                                     label = 'Arg 4',
                                                     choices = c('1999', 'cases', 'year', 'China'),
                                                     selected = 'cases')
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
                      
                      #### Spread 1 #### 
                      
                      tabPanel(div(style = 'font-size: 125%', 'Spread 1'),

                               
                               box(wellPanel(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                   #titlePanel("Tidy the Original Data"),
                                   #div(style = 'text-align: center',
                                   #  h1(strong('Tidy the Original Data')),
                                   #div(style = 'text-align: center',
                                   h4(tags$b('Fill in the Correct Arguments to Tidy the Data')),
                                   h4(tags$code('tidyr::gather(RawData3,`Arg 1`, `Arg 2`, key = "Arg 3", value = "Arg 4")')))),

                               #fluidRow(
                               #sidebarPanel(
                               #  div(style = 'text-align: left',
                               #     bsButton("newtable",
                               #             label = "New Table",
                               #            icon("arrow-circle-right"),
                               #           size = "medium",
                               #          style = 'success')),
                               #  br(),
                               # bsButton("submit",
                               #         label = "Check Answer",
                               #        icon("lightbulb"),
                               #       size = "medium",
                               #      style = 'success'),
                               #width = 1)),#),
                               
                               
                               fluidRow(
                                 box(div(style = 'background-image: url("green.png"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'RawData2')),
                                     style = 'background-image: url("green.png"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("original2")),
                                 box(div(style = 'background-image: url("green.png"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'Your Tidy Attempt')),
                                     style = 'background-image: url("green.png"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("userOutA"))),
                               br(),
                               fluidRow(
                                 column(12,
                                        div(style = 'text-align: left; font-size: 125%',
                                            wellPanel(tags$strong('Your R code: '),
                                                      uiOutput('userOutB'))
                                        ))),
                               br(),
                               
                               
                               fluidRow(
                                 # choices for user plot
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOpA',
                                                 label = 'Arg 1',
                                                 choices = c('country', '1999', '2000', 'Afghanistan'),
                                                 selected = '1999')),
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOpB',
                                                 label = 'Arg 2',
                                                 choices = c('country', '1999', '2000', 'Afghanistan'),
                                                 selected = '1999')),
                                 
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOpC',
                                                 label = 'Arg 3',
                                                 choices = c('population', 'country', 'Brazil', 'year'),
                                                 selected = 'population')),
                                 
                                 
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOpD',
                                                 label = 'Arg 4',
                                                 choices = c('population', 'country', 'Brazil', 'year'),
                                                 selected = 'year')
                                 )
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
                      
                      tabPanel(div(style = 'font-size: 125%', 'Spread 2'),
                               
                               
                               box(wellPanel(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                                 #titlePanel("Tidy the Original Data"),
                                                 #div(style = 'text-align: center',
                                                 #  h1(strong('Tidy the Original Data')),
                                                 #div(style = 'text-align: center',
                                                 h4(tags$b('Fill in the Correct Arguments to Tidy the Data')),
                                                 h4(tags$code('tidyr::gather(RawData4,`Arg 1`, `Arg 2`, key = "Arg 3", value = "Arg 4")')))),
                                             
                                             #fluidRow(
                                             #sidebarPanel(
                                             #  div(style = 'text-align: left',
                                             #     bsButton("newtable",
                                             #             label = "New Table",
                                             #            icon("arrow-circle-right"),
                                             #           size = "medium",
                                             #          style = 'success')),
                                             #  br(),
                                   # bsButton("submit",
                                   #         label = "Check Answer",
                                   #        icon("lightbulb"),
                                   #       size = "medium",
                                   #      style = 'success'),
                                   #width = 1)),#),
                                   
                                   
                                   fluidRow(
                                     box(div(style = 'background-image: url("green.png"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                             (title = 'RawData4')),
                                         style = 'background-image: url("green.png"); background-position: center; text-align: center',
                                         width = 6,
                                         uiOutput("original4")),
                                     box(div(style = 'background-image: url("green.png"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                             (title = 'Your Tidy Attempt')),
                                         style = 'background-image: url("green.png"); background-position: center; text-align: center',
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
                                     box(div(style = 'background-color: #b8f28c',
                                             (title = '')),
                                         style = 'background: #b8f28c',
                                         width = 3,
                                         selectInput(inputId = 'userOpJ',
                                                     label = 'Arg 1',
                                                     choices = c('country', '1999', '2000', 'Afghanistan'),
                                                     selected = '1999')),
                                     box(div(style = 'background-color: #b8f28c',
                                             (title = '')),
                                         style = 'background: #b8f28c',
                                         width = 3,
                                         selectInput(inputId = 'userOpK',
                                                     label = 'Arg 2',
                                                     choices = c('country', '1999', '2000', 'Afghanistan'),
                                                     selected = '1999')),
                                     
                                     box(div(style = 'background-color: #b8f28c',
                                             (title = '')),
                                         style = 'background: #b8f28c',
                                         width = 3,
                                         selectInput(inputId = 'userOpL',
                                                     label = 'Arg 3',
                                                     choices = c('population', 'country', 'Brazil', 'year'),
                                                     selected = 'population')),
                                     
                                     
                                     box(div(style = 'background-color: #b8f28c',
                                             (title = '')),
                                         style = 'background: #b8f28c',
                                         width = 3,
                                         selectInput(inputId = 'userOpM',
                                                     label = 'Arg 4',
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
                                       style = "background-color: #b8f28c",
                                       tags$div(
                                         tags$li("Attempt the questions!"),
                                         tags$li("Run your code in the R script
                                                 box below and see the output on the right"),
                                         tags$li("Uncomment the sample code to explore.")
                                         ,
                                         style = "background-color: #b8f28c")),
                                     h3("Exercises"),
                                     uiOutput('progress'),
                                     wellPanel(style = "background-color: #b8f28c",
                                               uiOutput("question") %>% 
                                                 withSpinner(color = "#1E7B14"),
                                               uiOutput("options"),
                                               br(),
                                               selectInput("answer", "Answer:", c("","A", "B", "C")),
                                               uiOutput("mark"),
                                               tags$style(type = 'text/css', '#question{font-size: 15px;
                                                          background-color: #b8f28c;color: black;}',
                                                          '.well { padding: 10px; margin-bottom: 15px; max-width: 1000px; }')
                                               
                                     ),
                                     fluidPage(
                                       tags$head(
                                         tags$style(HTML('#submit{background-color:#5a992b; color:white}')),
                                         tags$style(HTML('#eval{background-color:#5a992b; color:white}')),
                                         tags$style(HTML('#nextq{background-color:#5a992b; color:white}'))
                                       ),
                                       fluidRow(
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
                                     
                                     
                                     # column(3,
                                     #        actionButton(inputId = 'submit', label = 'Submit', style="success")
                                     # ),
                                     # column(3,
                                     #        bsButton(inputId = "nextq",label = "Next", style='warning', disabled = TRUE)
                                     # ),
                                     # column(3,
                                     #        bsButton(inputId = "reset",label = "Restart", style="danger", )
                                     # )),
                                     br(),
                                     
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
            #div(style="display: inline-block;vertical-align:top;",
            #    tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
            #),
            #div(style="display: inline-block;vertical-align:top;",
            #    circleButton("info0",icon = icon("info"), status = "myClass", size = "xs")
            #),
            tabsetPanel(type = 'tabs',
                        tabPanel(div(style = 'font-size: 125%', 'Unite'),
                                 br(),
                                 box(title = 'View An Example',
                                     width = NULL,
                                     style = 'background-color: #b8f28c; display: inline-block',
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
                                     style = 'background-color: #b8f28c; display: inline-block',
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
                                     style = 'background-color: #b8f28c; display: inline-block',
                                     materialSwitch(inputId = 'dw3',
                                                    label = 'View the Transformed Data Set',
                                                    value = FALSE),
                                     tableOutput('dwTable5'),
                                     tags$strong(div('Spread rows into columns.', style = 'color: green')),
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
                                     style = 'background-color: #b8f28c; display: inline-block',
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
                     box(div(style = 'font-weight: bold; font-size: 140%', (title = 'Click the lightbulb to choose an answer!')),
                         style = 'text-align: left',
                         width = NULL,
                         height = NULL,
                         tags$img(src = 'combine.png', width = '300px', height = NULL),

              
            br(),
            br(),
            
            fluidRow(#theme = "bootstrap.css",
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #b8f28c',
                         width = NULL,
                         height = '220px',
                         dropdownButton(circle = TRUE,
                                        status = 'success',
                                        size = 'xs',
                                        icon = icon('lightbulb'),
                                        up = TRUE,
                                        right = TRUE,
                                        tooltip = tooltipOptions(title = "Select Your Answer"),
                                        sliderTextInput(inputId = 'cd1',
                                                        label = 'Mutating Joins Option',
                                                        force_edges = TRUE,
                                                        grid = TRUE,
                                                        choices = c('left join', 'right join', 'inner join', 'full join')),
                                        textOutput('cdExp1')),
                         tableOutput('cdTable1'),
                         bsButton(inputId = 'check1',
                                  label = 'Check',
                                  size = 'median')
                         )
                     ),
              
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #b8f28c',
                         width = NULL,
                         height = '220px',
                         dropdownButton(circle = TRUE,
                                        status = 'success',
                                        size = 'xs',
                                        icon = icon('lightbulb'),
                                        up = TRUE,
                                        right = TRUE,
                                        tooltip = tooltipOptions(title = "Select Your Answer"),
                                        sliderTextInput(inputId = 'cd2',
                                                        label = 'Mutating Joins Option',
                                                        force_edges = TRUE,
                                                        grid = TRUE,
                                                        choices = c('left join', 'right join', 'inner join', 'full join')),
                           textOutput('cdExp2')),
                         tableOutput('cdTable2'),
                         br(),
                         bsButton(inputId = 'check2',
                                  label = 'Check',
                                  size = 'median')
                         )
              ),
              
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #b2dfdb',
                         width = NULL,
                         height = '180px',
                         dropdownButton(circle = TRUE,
                                        status = 'success',
                                        size = 'xs',
                                        icon = icon('lightbulb'),
                                        up = TRUE,
                                        right = TRUE,
                                        tooltip = tooltipOptions(title = "Select Your Answer"),
                                        sliderTextInput(inputId = 'cd5',
                                                        label = 'Filtering Joins Option',
                                                        force_edges = TRUE,
                                                        choices = c('semi join', 'anti join'))),
                         tableOutput('cdTable5'),
                         br(),
                         bsButton(inputId = 'check5', label = 'Check', size = 'median')
                         )
                     ),
              
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #b2dfdb',
                         width = NULL,
                         height = '180px',
                         dropdownButton(circle = TRUE,
                                        status = 'success',
                                        size = 'xs',
                                        icon = icon('lightbulb'),
                                        up = TRUE,
                                        right = TRUE,
                                        tooltip = tooltipOptions(title = "Select Your Answer"),
                                        sliderTextInput(inputId = 'cd6',
                                                        label = 'Filtering Joins Option',
                                                        force_edges = TRUE,
                                                        choices = c('semi join', 'anti join'))
                         ),
                         tableOutput('cdTable6'),
                         bsButton(inputId = 'check6',
                                  label = 'Check',
                                  size = 'median')
                         )
                     ),
              
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #b8f28c',
                         width = NULL,
                         height = NULL,
                         dropdownButton(circle = TRUE,
                                        status = 'success',
                                        size = 'xs',
                                        icon = icon('lightbulb'),
                                        up = TRUE,
                                        right = TRUE,
                                        tooltip = tooltipOptions(title = "Select Your Answer"),
                                        sliderTextInput(inputId = 'cd3',
                                                        label = 'Mutating Joins Option',
                                                        force_edges = TRUE,
                                                        grid = T,
                                                        choices = c('left join', 'right join', 'inner join', 'full join')),
                                        textOutput('cdExp3')
                                        ),
                         tableOutput('cdTable3'),
                         bsButton(inputId = 'check3',
                                  label = 'Check',
                                  size = 'median')
                         )
                     ),
              
              column(width = 6,
                     box(title = NULL,
                         style = 'background-color: #b8f28c',
                         width = NULL,
                         height = NULL,
                         dropdownButton(
                           circle = TRUE,
                           status = 'success',
                           size = 'xs',
                           icon = icon('adjust'),
                           up = TRUE,
                           right = TRUE,
                           tooltip = tooltipOptions(title = "Select Your Answer"),
                           sliderTextInput(inputId = 'cd4',
                                           label = 'Mutating Joins Option',
                                           force_edges = TRUE,
                                           grid = T,
                                           choices = c('left join', 'right join', 'inner join', 'full join')
                                           ),
                           textOutput('cdExp4')
                         ),
                         tableOutput('cdTable4'),
                         br(),
                         bsButton(inputId = 'check4',
                                  label = 'Check',
                                  size = 'median')
                         ))))
                     )
            )
            )))


shinyUI(dashboardPage(skin = 'green', header, sidebar, body))