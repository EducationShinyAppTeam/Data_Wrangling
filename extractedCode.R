## Group_by and Arrange tabsets ----
### GroupBy ----
tabPanel(
  div(style = 'font-size: 125%', 'Group_by'),
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
### Arrange ----
tabPanel(div(style = 'font-size: 125%', 'Arrange'),
         br(),
         box(title = 'View An Example',

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

             tableOutput('dwTable8'))
)