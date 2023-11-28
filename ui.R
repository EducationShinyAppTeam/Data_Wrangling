# load Packages ----
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





bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)


source("helpers.R")

# Define UI for the App ----
ui <- list(
  dashboardPage(
    skin = "green",
      dashboardHeader(title = 'Data Wrangling',
                      titleWidth = 250,
                      tags$li(class = "dropdown",actionLink("info", icon("info"), class = "myClass")),
                      tags$li(class = "dropdown",boastUtils::surveyLink(name = "Data_wrangling")),
                      tags$li(class = "dropdown",tags$a(href = "https://shinyapps.science.psu.edu/",icon("home")))),
        
    dashboardSidebar(
      width = 250,
      sidebarMenu(id = 'tabs',
              menuItem('Overview', tabName = 'overview', icon = icon("dashboard")),
              menuItem('Prerequisites', tabName = 'prereq', icon = icon('book')),
              menuItem('Data Wrangling : Part 1', tabName = 'exp1', icon = icon('wpexplorer')),
              menuItem('Data Wrangling : Part 2', tabName = 'exp2', icon = icon('wpexplorer')),
              menuItem('Data Wrangling : Part 3', tabName = 'exp3', icon = icon('wpexplorer')),
              menuItem('Tidy Data Challenge', tabName = 'tidy', icon = icon('gears')),
              menuItem('Combining Data Challenge', tabName = 'comb', icon = icon('gamepad')),
              menuItem('References', tabName = "References", icon = icon("leanpub"))
      ),
      #PSU logo
      tags$div(class = "sidebar-logo",
               boastUtils::sidebarFooter())
    ),

    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css"),
      ),
      tags$style(type = "text/css", ".content-wrapper,.right-side {background-color: white;}"),
      #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
      
      useShinyjs(),
      # Overview Page ----
      
      tabItems(
        tabItem(tabName = 'overview',
            h1('Data Wrangling App'),
            br(),
            p('This app illustrates R code for tidying, reshaping and combining data.'),
            br(),
            
            h2('Instructions'),
            p(tags$li('In the Exploring Data Wrangling section 1, go through each tab including select, group_by, filter and arrange.')),
            p(tags$li('In the Exploring Data Wrangling section 2, go through each tab including unite, seperate, mutate and recode.')),
            p(tags$li('In the Exploring Data Wrangling section 3 , go through each tab including pivot_wider, pivot_longer,
                      seperate_wider_deliminate, seperate_wider_position, summarise')),
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
        
        
        # Prerequisites Page ----
        
        tabItem(tabName = 'prereq',
            p('Please refer to the', a(href = 'https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf', 'Data Wrangling cheatsheet', target="_blank"), 'for all the information needed.'),
            p('This includes many examples of data wrangling tools for reshaping data and joins.')),
           
    
        
        
        # Explore Data Page 1 ----
        tabItem(tabName = 'exp1', 
                tabsetPanel(type = 'tabs',
                            ### Select ----
      
                           tabPanel(div(style = 'font-size: 125%', 'Select'),
                                     br(),
                                     box(title = 'View An Example',
                                     p("The dplyr::select function is used to subset columns in a data frame, choosing only the columns you want to keep."),
                                     br(),
                                     width = NULL,
                                     style = 'background-color: #ffffff; display: inline-block',
                                     
                                     tableOutput('selectData'),
                                     
                                     fluidRow(
                                       column(5,
                                              selectInput(inputId = "se1", 
                                                          label = "Select Your Selecting Option",
                                                          choices = c('Select columns by name', 'Select columns by excluding certain columns',
                                                                      'Select columns by index number', 'Select columns by a range of names',
                                                                      'Rename columns while selecting', 'Select columns that contain a certain string'),
                                                          selected = character(0),
                                                          width = '300px'),
                                       ),
                                       ),
                                     
                                     uiOutput('selectUI'),
                                     br(),
                                     tableOutput('selectOutput2')
                                      ),
                                      ),                 
                            
                           ### GroupBy ----
                           
                           tabPanel(div(style = 'font-size: 125%', 'Group_by'),
                                    br(),
                                    box(title = 'View An Example',
                                        p("The dplyr::group_by function groups a data frame by one or more variables, enabling you to perform aggregate operations on each group."),
                                        br(),
                                        width = NULL,
                                        style = 'background-color: #ffffff; display: inline-block',
                                        
                                        tableOutput('groupData'),
                                        
                                        fluidRow(
                                          column(5,
                                                 selectInput(inputId = "gr1", 
                                                             label = "Select Your Group_by Option",
                                                             choices = c("Cylinders" = "cyl", "Gears" = "gear"),
                                                             selected = "cyl",
                                                             width = '300px'),
                                        ),
                                        ),
                                        uiOutput('grCode'),
                                        br(),
                                        tableOutput("groupedTable")
                                    ),
                                    ),
                   
                           
                           ### Filter ----
                           tabPanel(div(style = 'font-size: 125%', 'Filter'),
                                    br(),
                                    box(title = 'View An Example',
                                        br(),
                                        p("The dplyr::filter function subsets rows in a data frame based on specified conditions, keeping only the rows that meet the criteria."),
                                        br(),
                                        width = NULL,
                                        style = 'background-color: #ffffff; display: inline-block',
                               
                                        tableOutput('FilterData'),
                                        
                                        fluidRow(
                                          column(5,
                                                 selectInput(inputId = "fl1", 
                                                             label = "Select Your Filter Option",
                                                             choices = c("Filter cars with mpg greater than 20",
                                                                         "Filter cars with exactly 6 cylinders",
                                                                         "Filter cars with horsepower between 100 and 200"),
                                                             selected = "Filter cars with mpg greater than 20",
                                                             width = '300px'),
                                          ),
                                        ),
                                        uiOutput('filterCode'),
                                        br(),
                                        tableOutput("filterTable")
                                        
                                    )
                           ),
                           ### Arrange ---- 
                            tabPanel(div(style = 'font-size: 125%', 'Arrange'),
                                     br(),
                                     box(title = 'View An Example',
                                         br(),
                                         p("The tidyr::arrange function in R is used to reorder rows based on specified column values."),
                                         br(),
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
        
        # Explore Data Page 2 ----
        tabItem(tabName = 'exp2',
                tabsetPanel(type = 'tabs',
                            ##### Unite -----
                            tabPanel(div(style = 'font-size: 125%', 'Unite'),
                                     br(),
                                     box(title = 'View An Example',
                                         p("The tidyr::unite() we function in R is used to combine multiple columns into a single new column in a data frame."),
                                         width = NULL,
                                         style = 'background-color: #ffffff; display: inline-block',
                                         tableOutput('uniteData'),
                                         br(),
                                         fluidRow(
                                           column(5,
                                                  selectInput(inputId = "un1", 
                                                              label = "Select Your Sorting Option",
                                                              choices = c('First_Name, Last_Name', 'Day, Month, Year'),
                                                              selected = character(0),
                                                              width = '300px'),
                                           ),
                                         ),
                                         uiOutput('uniteUI'),
                                         br(),
                                         tableOutput('uniteOutput2')
                                     )
                            ),
              
                            
                            ##### Mutate ----
                            tabPanel(div(style = 'font-size: 125%', 'Mutate'),
                                     br(),
                                     box(title = 'View An Example',
                                         p("The dplyr::mutate function is used to add new variables
                                           to a data frame or modify existing ones, using existing variables for computations."),
                                         br(),
                                         width = NULL,
                                         style = 'background-color: #ffffff; display: inline-block',
                                         tableOutput('mutateData'),
                                         br(),
                                         fluidRow(
                                           column(5,
                                                  selectInput(inputId = "mutateOption", 
                                                              label = "Select Your Mutation Option",
                                                              choices = c("Create New Column",
                                                                          "Modify Existing Column" ,
                                                                          "Use Multiple Columns",
                                                                          "Use with Other Functions" ,
                                                                          "Multiple Mutations",
                                                                          "Conditional Mutations"),
                                                              selected = "Create New Column",
                                                              width = '300px'),
                                           ),
                                         ),
                                         
                                         uiOutput('mutateCode'),
                                         br(),
                                         tableOutput('mutateOutput')
                                     ),
                            ),
                    
                            
                            ##### Seperate ----
                            tabPanel(div(style = 'font-size: 125%', 'Separate'),
                                     br(),
                                     box(title = 'View An Example',
                                         p("The tidyr::separate function splits a single
                                           character column into multiple columns using a specified separator."),
                                         br(),
                                         width = NULL,
                                         style = 'background-color: #ffffff; display: inline-block',
                                         
                                         tableOutput("SeperateData"),
                                         br(),
                                         
                                         fluidRow(
                                           column(5,
                                                  selectInput(inputId = "sepOption", 
                                                              label = "Select Your Seprate Option",
                                                              choices = c("Separate Full Name",
                                                                          "Separate Date of Event"),
                                                              selected = "Separate Full Name",
                                                              width = '300px'),
                                           ),
                                         ),
                                         uiOutput("separateCode"),
                                         br(),
                                         tableOutput("separateOutput")
                                         
                                         
                                     ),
                            ),
                
                            
                            ##### Recode ----
                            tabPanel(div(style = 'font-size: 125%', 'recode'),
                                     br(),
                                     box(title = 'View An Example',
                                         p("The dplyr::recode function changes specific values in a vector or column
                                           based on a set of rules, useful for changing factor levels or categorical values."),
                                         br(),
                                         width = NULL,
                                         style = 'background-color: #ffffff; display: inline-block',
                                         
                                         tableOutput("RecodeData"),
                                         br(),
                                         
                                         fluidRow(
                                           column(5,
                                                  selectInput(inputId = "recodeOption", 
                                                              label = "Select Your recode Option",
                                                              choices = c("Recode Satisfaction Level",
                                                                          "Recode Age Group",
                                                                          "Recode Region Code"),
                                                              selected = "Recode Satisfaction Level",
                                                              width = '300px'),
                                           ),
                                         ),
                                         uiOutput("recodeUI"),
                                         br(),
                                         tableOutput("recodeOutput")
                                         
                                     )),
                    )),
# Explore page 3 -----    

    tabItem(tabName = 'exp3', 
            tabsetPanel(type = 'tabs',
                        
                        #### pivot wider ----
                        tabPanel(div(style = 'font-size: 125%', 'Pivot_wider'),
                                 br(),
                                 box(title = 'View An Example',
                                     br(),
                                     p("The tidyr::pivot_wider() function in R is used to spread rows into columns."),
                                     br(),
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
                        
                        ##### pivot longer -----
                        tabPanel(div(style = 'font-size: 125%', 'Pivot_longer'),
                                 br(),
                                 box(title = 'View An Example',
                                     br(),
                                     p('The tidyr::pivot_longer() function in R is used to gather the columns into rows.'),
                                     br(),
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
                                 )),
                        #### seperate_wider_deliminate ----
                        tabPanel(div(style = 'font-size: 125%', 'seperate_wider_deliminate'),
                                 br(),
                                 box(title = 'View An Example',
                                     br(),
                                     p(""),
                                     br(),
                                     width = NULL,
                                     style = 'background-color: #ffffff; display: inline-block',
                                     
                                 )
                        ),
                        #### seperate_wider_position ----
                        tabPanel(div(style = 'font-size: 125%', 'seperate_wider_position'),
                                 br(),
                                 box(title = 'View An Example',
                                     br(),
                                     p(""),
                                     br(),
                                     width = NULL,
                                     style = 'background-color: #ffffff; display: inline-block',
                                     
                                 )
                        ),
                        
                        #### summarise ----
                        tabPanel(div(style = 'font-size: 125%', 'summarise'),
                                 br(),
                                 box(title = 'View An Example',
                                     br(),
                                     p(""),
                                     br(),
                                     width = NULL,
                                     style = 'background-color: #ffffff; display: inline-block',
                                     
                                 )
                        ),
                                 
                                 
                        
                      
                        )),
        
        
       # Tidy Data Page ----



  tabItem(tabName = 'tidy',
          tabsetPanel(type = 'tabs', id = 'questionTabs',
                      tabPanel(div(style = 'font-size: 125%', 'pivot_longer 1'), value = "pivot_longer1",

      ## pivot_longer 1 ----
                               box(
                                 wellPanel(div(style = 'text-align: left; font-size: 100%; display: inline-block',
                                               
                                               
                                               # The statement in the top box 
                                               tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be a country in a year'),
                                               tags$code('tidyr::pivot_longer(RawData, cols = c("Arg 1", "Arg 2"), names_to = "Arg 3", values_to = "Arg 4")'))),
                                 
                                 textOutput("tester"),
                                 br(),
                                 
                                 column(12,
                                        box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                                (title = 'RawData')),
                                            style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                            uiOutput("original1"))),
                                 br(),
                                 br(),
                                 br(),
                                 
                                 # choices for user plot
                                 fluidRow(
                                   
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
                                   )),
                                 # Your R Code based on input
                                 fluidRow(
                                   column(12,
                                          div(style = 'text-align: left; font-size: 100%',
                                              wellPanel(tags$strong('Your R code: '),
                                                        uiOutput('tidyAttemptTable'))))),
                                 br(),
                                 box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 105%; font-weight: bold',
                                         (title = 'Your Tidy Attempt')),
                                     style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("userOut1"),
                                     div(style = 'font-size: 100%; font-weight: bold')),
                                 width = 10),
                          
      
      fluidRow(
        div(style = 'display: inline-block; position: relative; text-align: right',
            uiOutput('correct'),
            uiOutput('wrong')),
        
        
        br(),
        br(),
        
        div(style = 'text-align: right; display: inline-block; position: relative; top: 10px',
            uiOutput("sub"),
            br(),
            uiOutput("resetcc"),
        ),
      ),
      
                      ),
      
       ## pivot_longer 2 ----
                      
      tabPanel(div(style = 'font-size: 125%', 'pivot_longer 2'), value = "pivot_longer2", 
               
               # Fill in the correct Argument across top
               box(wellPanel(div(style = 'text-align: left; font-size: 100%; display: inline-block',
                                 tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be an employee on a certain day of the week'),
                                 tags$code('tidyr::pivot_longer(RawData2, cols = c("Arg 1", "Arg 2", "Arg 3"), names_to = "Arg 4", values_to = "Arg 5")'))),
                   # Raw Data Table        
                   
                   
                     column(12,
                            box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                             (title = 'RawData2')),
                         style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                         uiOutput("original3"))),
                
                   br(),
                   # The 5 Input values
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
                   
                   # Tidy Attempt 
                   fluidRow(
                     column(12,
                            div(style = 'text-align: left; font-size: 100%',
                                wellPanel(tags$strong('Your R code: '),
                                          uiOutput('userOutY'))
                            ))),
                   
                   box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                           (title = 'Your Tidy Attempt')),
                       style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                       width = 6,
                       uiOutput("tidyAttempt2")),
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
                      
## Pivot Wider 1  ----
                      
                      tabPanel(div(style = 'font-size: 125%', 'pivot_wider 1'), value = "pivot_wider1",

                            #  Fill in sample argument
                               box(wellPanel(div(style = 'text-align: left; font-size: 100%; display: inline-block',
                                   tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be a country'),
                                   tags$code('tidyr::pivot_wider(RawData3, names_from = "Arg 1", values_from = "Arg 2")'))), 


                            # Raw Data 
                               column(12,
                                 box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                         (title = 'RawData3')),
                                     style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                     width = 6,
                                     uiOutput("original2"))),
                               
                            # Drop Down selections
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
                         
                              # R code based on blanks 
                              fluidRow(
                                column(12,
                                       div(style = 'text-align: left; font-size: 100%',
                                           wellPanel(tags$strong('Your R code: '),
                                                     uiOutput(paste0('userOutB')))
                                       ))),
                            
                              # Your Tidy Attempt 
                              box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                      (title = 'Your Tidy Attempt')),
                                  style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                  width = 6,
                                  uiOutput("userOutA")),
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
                      
 ## pivot_wider 2 ----
                      
                      tabPanel(div(style = 'font-size: 125%', 'pivot_wider 2'), value = "pivot_wider2",
                               
                               
                               box(wellPanel(div(style = 'text-align: left; font-size: 100%; display: inline-block',
                                                 tags$b('Fill in the Correct Arguments to Tidy the Data - a case will be an employee'),
                                                 tags$code('tidyr::pivot_wider(RawData4, names_from = "Arg 1", values_from = "Arg 2")'))),
                                             

                                   
                                    column(12,
                                     box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                             (title = 'RawData4')),
                                         style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                         width = 6,
                                         uiOutput("original4"))),
                                   br(),
                                   # choices for user plot
                                   fluidRow(
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
                                   fluidRow(
                                     column(12,
                                            div(style = 'text-align: left; font-size: 100%',
                                                wellPanel(tags$strong('Your R code: '),
                                                          uiOutput('userOut4'))
                                            ))),
                                   br(),
                                   box(div(style = 'background-image: url("white.jpeg"); background-position: center; text-align: left; font-size: 115%; font-weight: bold',
                                           (title = 'Your Tidy Attempt')),
                                       style = 'background-image: url("white.jpeg"); background-position: center; text-align: center',
                                       width = 6,
                                       uiOutput("userOut3")),
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
                      
                    
                 
## Live code ----
                 
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
                                     
                                     
                                     # try rLocker statement.   #### WHYYYY ----
                                     tags$samp(
                                       htmlOutput("statements")
                                     ),
                                     # end 
                                     
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

# Combining Data Page ----
    tabItem(tabName = 'comb',
           
            fluidRow(
              column(width = 12,
                     box(div(style = 'font-weight: bold; font-size: 140%', (title = 'Multiple Choice Joins Practice')),
                         style = 'text-align: left',
                         br(),
                         width = NULL,
                         height = NULL, 
                         tags$img(
                           class = "centerFigure",
                           src = "cds.png",
                           width = 300,
                           height = 110,
                           alt = "Picture of the table"),
                      
                         # div(style="display:inline-block", tableOutput('titleTableA')),
                         # div(style="display: inline-block", "+"),
                         # div(style="display:inline-block", tableOutput('titleTableB')),
                         # div(style="display:inline-block", p(' = ')),
                        
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
# References Tab ----
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
  rLocker: Learning Locker for Shiny, R package. 
  Available from https://github.com/rpc5102/rLocker"),
          
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
          br(), 
          br(), 
          br(), 
          boastUtils::copyrightInfo() 
        )
        
)) ))

#.GROUP = "KEEP"
# BETWEEN FUNCTION OF TIDYVERSE FOR OPTION 3 OF FILTER
# CASE_WHEN -- .DEFAULT FOR MUTATE FUNCTION ---- 
# CASE MATCH FOR RECODE 