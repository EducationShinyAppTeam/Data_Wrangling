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


header = dashboardHeader(title = 'Data Wrangling',
                         tags$li(class = "dropdown",
                                 tags$a(href = "https://shinyapps.science.psu.edu/",
                                        icon("home"))),
                         tags$li(class = "dropdown",
                                 actionLink("info", icon("info"), class = "myClass")))

sidebar = dashboardSidebar(
  sidebarMenu(id = 'tabs',
              menuItem('Overview', tabName = 'overview', icon = icon("dashboard")),
              #menuItem('Data Visualization', tabName = 'exp4', icon = icon('wpexplorer')),
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
    tabItem(tabName = 'overview',
            tags$a(href='http://stat.psu.edu/', tags$img(src = 'psu_icon.jpg', align = "left", width = 180)),
            br(),
            br(),
            br(),
            h3(strong('About:')),
            h4('This app illustrates R code for data visulization, reshaping and combining data. Please refer to the', a(href='https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf', 'Data Wrangling cheatsheet'), 'for all the information needed.'),
            br(),
            
            h3(strong('Instructions:')),
            #h4(tags$li('In the Data Visualization section, go through each tab including 3D plots, line plots, contour plots, and heat maps.')),
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

############ Reshaping Data ############
    tabItem(tabName = 'exp1', 
            #div(style="display: inline-block;vertical-align:top;",
            #    tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
            #),
            #div(style="display: inline-block;vertical-align:top;",
            #    circleButton("info0",icon = icon("info"), status = "myClass", size = "xs")
            #),
            tabsetPanel(type = 'tabs',
                        tabPanel('unite',
                                 br(),
                                 box(title = 'View An Example', width = NULL, style = 'background-color: #ff8a80',
                                     tableOutput('uniteOutput1'),
                                     checkboxGroupInput(inputId = 'unite1', label = 'Select Columns to Unite', inline = T,
                                                        choices = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb')
                                                        ),
                                     materialSwitch(inputId = 'unite3', label = 'View the Transformed Data Set', value = FALSE),
                                     uiOutput('uniteUI'),
                                     br(),
                                     tableOutput('uniteOutput2')
                                     )
                                 ),
                        
                        tabPanel('gather',
                                 br(),
                                 box(title = 'View An Example', width = NULL, style = 'background-color: #f8bbd0',
                                     materialSwitch(inputId = 'dw1', label = 'View the Transformed Data Set', value = FALSE),
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
                        tabPanel('spread',
                                 br(),
                                 box(title = 'View An Example', width = NULL, style = 'background-color: #e0f7fa',
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
                       
                        tabPanel('arrange',
                                 br(),
                                 box(title = 'View An Example', width = NULL, style = 'background-color: #b2dfdb',
                                     sliderTextInput(inputId = 'dwSTI2', label = 'Select Your Sorting Option',
                                                     choices = c('Random', 'Low to High', 'High to Low'),
                                                     grid = TRUE, force_edges = TRUE
                                                     ),
                                     sliderTextInput(inputId = 'dwSTI1', label = 'Select the Variable to Sort By', 
                                                     choices = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec'), 
                                                     grid = TRUE, force_edges = TRUE
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

############ Combining Data Sets ############    
    tabItem(tabName = 'exp2',
           
            fluidRow(
              column(width = 12,
                     box(title = NULL, style = 'background-color: #b2dfdb', width = NULL, height = NULL,
                         tags$h2('Data Wrangling --- Combining Data Sets'),
                         
                         #div(style="display: inline-block;vertical-align:top;",
                          #   tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                         #),
                         #div(style="display: inline-block;vertical-align:top;",
                        #     circleButton("info1",icon = icon("info"), status = "myClass",size = "xs")
                         #),
                         bsPopover(id = 'info1', title = " ", content = 'Exercise! Within each box is a transformed dataset. Click on the green label on the upper left corner to select the correct transformation.', placement = 'buttom'),
                         div(style = 'text-align: center', tags$img(src = 'cds.png', width = '300px', height = NULL)),
                         br(),
                         br()
                         )
                     )
            ),
            
            fluidRow(#theme = "bootstrap.css",
              column(width = 6,
                     box(title = NULL, style = 'background-color: #ffe0b2', width = NULL, height = '220px',
                         dropdownButton(
                           circle = TRUE, status = 'success', size = 'xs', icon = icon('cogs'),
                           up = TRUE, right = TRUE, tooltip = tooltipOptions(title = "Select Your Answer"),
                           sliderTextInput(inputId = 'cd1', label = 'Mutating Joins Option', force_edges = TRUE, grid = TRUE,
                                           choices = c('left join', 'right join', 'inner join', 'full join')
                                           ),
                           textOutput('cdExp1')
                         ),
                         tableOutput('cdTable1'),
                         bsButton(inputId = 'check1', label = 'Check', size = 'median')
                         )
                     ),
              
              column(width = 6,
                     box(title = NULL, style = 'background-color: #ffe0b2', width = NULL, height = '220px',
                         dropdownButton(
                           circle = TRUE, status = 'success', size = 'xs', icon = icon('cogs'),
                           up = TRUE, right = TRUE, tooltip = tooltipOptions(title = "Select Your Answer"),
                           sliderTextInput(inputId = 'cd2', label = 'Mutating Joins Option', force_edges = TRUE, grid = TRUE,
                                           choices = c('left join', 'right join', 'inner join', 'full join')),
                           textOutput('cdExp2')
                         ),
                         tableOutput('cdTable2'),
                         br(),
                         bsButton(inputId = 'check2', label = 'Check', size = 'median')
                         )
              ),
              
              column(width = 6,
                     box(title = NULL, style = 'background-color: #ffccbc', width = NULL, height = '180px',
                         dropdownButton(
                           circle = TRUE, status = 'success', size = 'xs', icon = icon('cogs'),
                           up = TRUE, right = TRUE, tooltip = tooltipOptions(title = "Select Your Answer"),
                           sliderTextInput(inputId = 'cd5', label = 'Filtering Joins Option', force_edges = TRUE,
                                           choices = c('semi join', 'anti join')
                                           )
                         ),
                         tableOutput('cdTable5'),
                         br(),
                         bsButton(inputId = 'check5', label = 'Check', size = 'median')
                         )
                     ),
              
              column(width = 6,
                     box(title = NULL, style = 'background-color: #ffccbc', width = NULL, height = '180px',
                         dropdownButton(
                           circle = TRUE, status = 'success', size = 'xs', icon = icon('cogs'),
                           up = TRUE, right = TRUE, tooltip = tooltipOptions(title = "Select Your Answer"),
                           sliderTextInput(inputId = 'cd6', label = 'Filtering Joins Option', force_edges = TRUE,
                                           choices = c('semi join', 'anti join')
                           )
                         ),
                         tableOutput('cdTable6'),
                         bsButton(inputId = 'check6', label = 'Check', size = 'median')
                         )
                     ),
              
              column(width = 6,
                     box(title = NULL, style = 'background-color: #ffe0b2', width = NULL, height = NULL,
                         dropdownButton(
                           circle = TRUE, status = 'success', size = 'xs', icon = icon('cogs'),
                           up = TRUE, right = TRUE, tooltip = tooltipOptions(title = "Select Your Answer"),
                           sliderTextInput(inputId = 'cd3', label = 'Mutating Joins Option', force_edges = TRUE, grid = T,
                                           choices = c('left join', 'right join', 'inner join', 'full join')
                                           ),
                           textOutput('cdExp3')
                         ),
                         tableOutput('cdTable3'),
                         bsButton(inputId = 'check3', label = 'Check', size = 'median')
                         )
                     ),
              
              column(width = 6,
                     box(title = NULL, style = 'background-color: #ffe0b2', width = NULL, height = NULL,
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
                         bsButton(inputId = 'check4', label = 'Check', size = 'median')
                         )
                     )
            )
            )))


shinyUI(dashboardPage(skin = 'green', header, sidebar, body))