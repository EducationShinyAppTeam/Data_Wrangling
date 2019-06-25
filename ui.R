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
              menuItem('Tidy Data', tabName = 'exp4', icon = icon('broom')),
              menuItem('Reshaping Data', tabName = 'exp1', icon = icon('wpexplorer')),
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
            h4(tags$li('In the Tidy Data section, fill in the correct arguments for 1, 2, 3, and 4 that coreectly tidies the data.')),
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

  tabItem(tabName = 'exp4',
          tabsetPanel(type = 'tabs',
                      tabPanel(div(style = 'font-size: 125%', 'Challenge 1'),

                               
                               box(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                   #titlePanel("Tidy the Original Data"),
                               #div(style = 'text-align: center',
                                 #  h1(strong('Tidy the Original Data')),
                               #div(style = 'text-align: center',
                               
                               h4(tags$b('Fill in the Correct Arguments to Tidy the Data')),
                               h4(tags$code('tidyr::gather(table4a,`1`, `2`, key = "3", value = "4")'))),
                               
                               div(style = 'text-align: right; display: inline-block; position: relative; left: 250px',
                                   uiOutput("sub"),
                                   br(),
                                   uiOutput("reset")),
                               div(style = 'z-index: 1000; position: relative; text-align: center',
                                   uiOutput('correct'),
                                   uiOutput('wrong')),
                               
                                 

                               
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
                                         (title = 'Original Data - table4a')),
                                     style = 'background-image: url("green.png"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("original1")),
                                 box(div(style = 'background-image: url("green.png"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'Your Tidy Attempt')),
                                     style = 'background-image: url("green.png"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("userOut1"),
                                     div(style = 'font-size: 135%; font-weight: bold', (textOutput("change"))))),
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
                                                 label = 'Argument 1',
                                                 choices = c('1999', '2000'),
                                                 selected = '1999')),
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                    style = 'background: #b8f28c',
                                    width = 3,
                                    selectInput(inputId = 'userOp2',
                                                label = 'Argument 2',
                                                choices = c('1999', '2000'),
                                                selected = '1999')),
            
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOp3',
                                                 label = 'Argument 3',
                                                 choices = c('cases', 'year'),
                                                 selected = 'year')),
                                 
                                 
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background-color: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOp4',
                                                 label = 'Argument 4',
                                                 choices = c('cases', 'year'),
                                                 selected = 'cases')
                                     )
                                 ),
                               width = 12)),
                      
                      ### Challenge 2 ###
                      
                      tabPanel(div(style = 'font-size: 125%', 'Challenge 2'),

                               
                               box(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                   #titlePanel("Tidy the Original Data"),
                                   #div(style = 'text-align: center',
                                   #  h1(strong('Tidy the Original Data')),
                                   #div(style = 'text-align: center',
                                   h4(tags$b('Fill in the Correct Arguments to Tidy the Data')),
                                   h4(tags$code('tidyr::gather(table4b,`1`, `2`, key = "3", value = "4")'))),

                               #fluidRow(
                               #sidebarPanel(
                               #  div(style = 'text-align: left',
                               #     bsButton("newtable",
                               #             label = "New Table",
                               #            icon("arrow-circle-right"),
                               #           size = "medium",
                               #          style = 'success')),
                               #  br(),
                               div(style = 'text-align: right; left: 250px; display: inline-block; position: relative',
                                   uiOutput("bus"),
                                   br(),
                                   uiOutput("redo")),
                               div(style = 'text-align: center',
                                   uiOutput('cor'),
                                   uiOutput('wro')),
                               # bsButton("submit",
                               #         label = "Check Answer",
                               #        icon("lightbulb"),
                               #       size = "medium",
                               #      style = 'success'),
                               #width = 1)),#),
                               
                               
                               fluidRow(
                                 box(div(style = 'background-image: url("green.png"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'Original Data - table4b')),
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
                                                 label = 'Argument 1',
                                                 choices = c('1999', '2000'),
                                                 selected = '1999')),
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOpB',
                                                 label = 'Argument 2',
                                                 choices = c('1999', '2000'),
                                                 selected = '1999')),
                                 
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOpC',
                                                 label = 'Argument 3',
                                                 choices = c('population', 'year'),
                                                 selected = 'population')),
                                 
                                 
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background: #b8f28c',
                                     width = 3,
                                     selectInput(inputId = 'userOpD',
                                                 label = 'Argument 4',
                                                 choices = c('population', 'year'),
                                                 selected = 'year')
                                 )
                               ),
                               width = 12)),
                               

                      tabPanel(div(style = 'font-size: 125%', 'Challenge 3'),

                               
                               box(div(style = 'text-align: left; font-size: 85%; display: inline-block',
                                   #titlePanel("Tidy the Original Data"),
                                   #div(style = 'text-align: center',
                                   #  h1(strong('Tidy the Original Data')),
                                   #div(style = 'text-align: center',
                                   h4(tags$b('Fill in the Correct Arguments to Tidy the Data')),
                                   h4(tags$code('tidyr::spread(table2, key = "1", value = "2")'))),

                               #fluidRow(
                               #sidebarPanel(
                               #  div(style = 'text-align: left',
                               #     bsButton("newtable",
                               #             label = "New Table",
                               #            icon("arrow-circle-right"),
                               #           size = "medium",
                               #          style = 'success')),
                               #  br(),
                               div(style = 'text-align: right; display: inline-block; position: relative; left: 250px',
                                   uiOutput("subbed"),
                                   br(),
                                   uiOutput("restart")),
                               div(style = 'position: relative; text-align: center',
                                   uiOutput('cort'),
                                   uiOutput('rong')),
                               # bsButton("submit",
                               #         label = "Check Answer",
                               #        icon("lightbulb"),
                               #       size = "medium",
                               #      style = 'success'),
                               #width = 1)),#),
                               
                               
                               fluidRow(
                                 box(div(style = 'background-image: url("green.png"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'Original Data - table2')),
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
                                     width = 6,
                                     selectInput(inputId = 'userOpX',
                                                 label = 'Argument 1',
                                                 choices = c('year', 'type', 'count', 'country'),
                                                 selected = 'year')),
                                 box(div(style = 'background-color: #b8f28c',
                                         (title = '')),
                                     style = 'background: #b8f28c',
                                     width = 6,
                                     selectInput(inputId = 'userOpY',
                                                 label = 'Argument 2',
                                                 choices = c('year', 'type', 'count', 'country'),
                                                 selected = 'country'))
                                 ),
                               width = 12))
                               
                      )
                      
                      
                      ),
          #),
          
        
    
    
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
                                     width = 7,
                                     style = 'background-color: #b8f28c',
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
                                     width = 4,
                                     style = 'background-color: #b8f28c',
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
                        # tabPanel('separate',
                        #          br(),
                        #          box(title = 'View An Example', width = NULL, style = 'background-color: #4dd0e1',
                        #              materialSwitch(inputId = 'dw2', label = 'View the Transformed Data Set', value = FALSE),
                        #              tableOutput('dwTable3'),
                        #              tags$code('Separate one column into several.'),
                        #              br(),
                        #              tableOutput('dwTable4')
                        #              )
                        #          ),
                        tabPanel(div(style = 'font-size: 125%', 'Spread'),
                                 br(),
                                 box(title = 'View An Example',
                                     width = 4,
                                     style = 'background-color: #b8f28c',
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
                                     width = 7,
                                     style = 'background-color: #b8f28c',
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
                         
                         #div(style="display: inline-block;vertical-align:top;",
                          #   tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                         #),
                         #div(style="display: inline-block;vertical-align:top;",
                        #     circleButton("info1",icon = icon("info"), status = "myClass",size = "xs")
                         #),
                     
              
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