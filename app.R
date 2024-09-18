# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(DT)
library(palmerpenguins)
library(dplyr)
library(tidyr)
library(shinyAce)
library(knitr)
library(datasets)
library(rmarkdown)
library(learnr)
library(shinycssloaders)
library(shinyjs)

# devtools::install_github("rstudio/EDAWR")
## used for the pollution data
library(EDAWR)

# Load additional dependencies and setup functions ----
source("helpers.R")
bank <- read.table(file = "questionbank.csv", header = TRUE, sep = ",")

verbSelectors <- read.table(file = "verbSelectors.csv", header = TRUE, sep = ",")


# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "green",
    ## Header ----
    dashboardHeader(
      title = "Data Wrangling",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Data_Wrangling")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          id = "home",
          href = 'https://shinyapps.science.psu.edu/',
          icon("house")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Wrangling: Part 1", tabName = "exp1", icon = icon("wpexplorer")),
        menuItem("Wrangling: Part 2", tabName = "exp2", icon = icon("wpexplorer")),
        menuItem("Wrangling: Part 3", tabName = "exp3", icon = icon("wpexplorer")),
        menuItem("Wrangling: Part 4", tabName = "exp4", icon = icon("wpexplorer")),
        menuItem("Wrangling: Part 5", tabName = "exp5", icon = icon("wpexplorer")),
        menuItem("Wrangling: Part 6", tabName = "exp6", icon = icon("wpexplorer")),
        # menuItem("Tidy Data Challenge", tabName = "tidy", icon = icon("gears")),
        # menuItem('Combining Data Challenge', tabName = "comb", icon = icon("gears")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Exploring the Verbs of Data Wrangling"),
          p(
            "This app allows you to explore various data verbs connected to the
            concept of", tags$em("Data Wrangling."), "This includes actions related
            to tidying, reshaping, and combining data frames."
          ),
          h2('Instructions'),
          tags$ol(
            tags$li("Review the Prerequisites page for any concepts you might need
                    a refresher on."),
            tags$li("Explore six groups of data verbs that are commonly used in
                    Data Wrangling, along with some helper verbs."),
            tags$ol(
              type = "1",
              tags$li("In Wrangling Part 1, explore the verbs of Selecting,
                      Filtering, and Slicing."),
              tags$li("In Wrangling Part 2, explore the verbs of Grouping,
                      Arranging, and (Re-)Naming."),
              tags$li("In Wrangling Part 3, explore the verbs of Uniting,
                      Separating, and Mutating."),
              tags$li("In Wrangling Part 4, explore the verbs of Binding and Joining."),
              tags$li("In Wrangling Part 5, explore the verbs of Pivoting."),
              tags$li("In Wrangling Part 6, explore the verb of Summarizing.")
            ),
            # tags$li(
            #   "When you're ready, check out the Tidy Data Challenge, to put your
            #   understandings to the test by writting code to tidy some data."
            # ),
            # tags$li(
            #   "When you're ready, check out the Combining Data Challenge, to put
            #   your understandings of wrangling verbs to the test."
            # )
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go2",
              label = "Explore Wrangling Part 1!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This application was coded and developed by Anna (Yinqi) Zhang
            and Oluwafunke Alliyu. Special Thanks to Grace (Yubaihe) Zhou
            for being incredibly helpful with programming issues. Bug fixes and
            style guide along with function updates were implented by Ethan Wright
            (2020) and Aisiri Cherrimane Narendra (2023). App redesigned in 2024
            by Neil Hatfield.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 9/18/2024 by NJH.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("Please take a moment to look through the following concepts and
            helpful cheatsheets/coding guides."),
          p("A", tags$strong("case"), "refers to the underlying object or living
            being about which we have observed/measured different attributes and
            recorded those values to create data."),
          p("An", tags$strong("observation"), "refers to an instance where we
            have measured/categorized the value of one more attributes for a
            particular case at either a particular time or situation. We can make
            multiple observations of the same case."),
          p("We say that a data frame is", tags$strong("tidy"), "when the data
            frame meets three conditions:"),
          tags$ol(
            tags$li("Each row of the data frame represents a unique case."),
            tags$li("Each column of the data frame represents one attribute or
                    characteristic that all cases possess."),
            tags$li("Each cell is the intersection of a particular row and column,
                    and only contains a singular value. The value is that case's
                    instantiation of that attribute.")
          ),
          p("The following cheatsheets/coding guides are useful resources as you
            engage in Data Wrangling."),
          tags$ul(
            tags$li(
              tags$a(
                href = "https://rstudio.github.io/cheatsheets/html/tidyr.html",
                "Data tidying with", tags$code("{tidyr}"),
                target = "_blank",
                class = "bodylinks"
              )
            ),
            tags$li(
              tags$a(
                href = "https://rstudio.github.io/cheatsheets/html/data-transformation.html",
                "Data transformation with", tags$code("{dplyr}"),
                target = "_blank",
                class = "bodylinks"
              )
            )
          )
        ),
        ### Explore Part 1 ----
        tabItem(
          tabName = "exp1",
          withMathJax(),
          h2("Data Wrangling Part 1"),
          p("The data verbs that you'll explore here do different things. However,
            they share a common bond. Explore them and see if you can come up with
            how they are connected."),
          p("Use the Example Selector to explore different examples and how they
            impact the displayed data table. The original data table appears
            below and to the left. Example code for the wrangling and a resulting
            data frame appear to the right."),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Wrangling Verb"),
                tabsetPanel(
                  id = "caseP_subset",
                  type = "tabs",
                  tabPanel(
                    title = "Selecting",
                    br(),
                    p(tags$em("Selecting"), "refers to the action of choosing
                      which case attributes/characteristics we want to keep from
                      an input data frame for an output data frame. For tidy data,
                      this means that we are selecting which columns to keep and
                      which to leave behind. The function", tags$code("select"),
                      "from the", tags$code("{dplyr}"), "package allows us to
                      apply the selecting action."),
                    p("There are numerous ways we can select columns and we can
                      use several different helping verbs."),
                  ),
                  tabPanel(
                    title = "Filtering",
                    br(),
                    p(tags$em("Filtering"), "refers to the action keeping the
                      cases that satisify our conditions. For tidy data, this
                      means that we are filtering the rows to only those that
                      meet certain conditions. The function", tags$code("filter"),
                      "from the", tags$code("{dplyr}"), "package allows us to
                      apply the filtering action."),
                    p("There are numerous ways we can filter rows and we can
                      use several different helping verbs.")
                  ),
                  tabPanel(
                    title = "Slicing",
                    br(),
                    p(tags$em("Slicing"), "refers to selecting rows according to
                      their position in the data frame, their rank according to
                      one or more attributes, or randomly. The output data frame
                      will generally contain fewer rows than the original input.
                      However, you can end up with the same row repeated. If
                      there is grouping to the data frame, slicing will be repeated
                      within each group. The ", tags$code("{dplyr}"), "package
                      has several functions that handle slicing:", tags$code("slice"),
                      ",", tags$code("slice_head"), ",", tags$code("slice_tail"),
                      ",", tags$code("slice_min"), ",", tags$code('slice_max'),
                      ", and", tags$code("slice_sample")
                    ),
                  )
                ),
                selectInput(
                  inputId = "exp1Select",
                  label = "Example Selector",
                  choices = c("To be generated")
                )
              ),
              DTOutput(outputId = "explorePart1")
            ),
            column(
              width = 6,
              h3("Example Code"),
              uiOutput(outputId = "exp1Code"),
              DTOutput(outputId = "exp1Result")
            )
          ),
          h2("Verb Connections"),
          p("How do these data verbs connect to one another?")
        ),
        ### Explore Part 2 ----
        tabItem(
          tabName = "exp2",
          withMathJax(),
          h2("Data Wrangling Part 2"),
          p("The data verbs that you'll explore here do different things. However,
            they share a common bond. Explore them and see if you can come up with
            how they are connected."),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Wrangling Verb"),
                tabsetPanel(
                  id = "caseP_strA_valP",
                  type = "tabs",
                  tabPanel(
                    title = "Grouping",
                    br(),
                    p(tags$em("Grouping"), "refers to the action of changing the
                      structure of a data frame so that the data frame now
                      contains grouping meta-information. This will cause other
                      wrangling verbs to be recursively applied to to each
                      grouping within the data frame. The most common approach to
                      add grouping structural information to a data frame is
                      through the function", tags$code("group_by"), "from the",
                      tags$code("{dplyr}"), "package. This function's inverse is
                      the", tags$code("ungroup"), "function."),
                    p("A second way that you can temporarily adding grouping
                      meta-information is via the", tags$code("by"), "argument
                      of certain other functions (see Part 6 for an example).")
                  ),
                  tabPanel(
                    title = "Arranging",
                    br(),
                    p(tags$em("Arranging"), "refers to the action of re-ordering
                      the rows of a data frame according to their values for
                      certain attributes (i.e., columns). The function",
                      tags$code("arrange"), "from the", tags$code("{dplyr}"),
                      "package allows us to re-arrange the data table."),
                    p("We can re-arrange a data table using multiple attributes
                      and we can use several helper verbs.")
                  ),
                  tabPanel(
                    title = "Renaming",
                    br(),
                    p(tags$em("Renaming"), "refers to the action of us changing
                      the variable that we use to refer to a particular attribute
                      (i.e., column in a tidy data frame). We can carry out this
                      action by using the function", tags$code("rename"),
                      "from the", tags$code("{dplyr}"), "package. Alternatively,
                      we can also use the", tags$code("names"), "function of base
                      R along with the assignment operator to change the name
                      of a column."
                    ),
                  )
                ),
                p("Use the Example Selector to explore different examples and
                  how they impact the displayed data table. Example code will
                  appear below."),
                selectInput(
                  inputId = "exp2Select",
                  label = "Example Selector",
                  choices = c("To be generated")
                )
              ),
              h3("Example Code"),
              uiOutput(outputId = "exp2Code"),
            ),
            column(
              width = 6,
              DTOutput(outputId = "explorePart2")
            )
          ),
          h2("Verb Connections"),
          p("How do these data verbs connect to one another?"),
          hr(),
          h2("OLD"),
          h2("Challenge Yourself"),
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

                      ##### Recode ----
                      tabPanel(div(style = 'font-size: 125%', 'Recode'),
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
          )
        ),
        ### Explore Part 3 ----
        tabItem(
          tabName = "exp3",
          withMathJax(),
          h2("Data Wrangling Part 3"),
          p("The data verbs that you'll explore here do different things. However,
            they share a common bond. Explore them and see if you can come up with
            how they are connected."),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Wrangling Verb"),
                tabsetPanel(
                  id = "caseP_strA_valA",
                  type = "tabs",
                  tabPanel(
                    title = "Uniting",
                    br(),
                    p(tags$em("Uniting"), "is the action of combining several
                      existing columns into a new single column. The function",
                      tags$code("unite"), "from the", tags$code("{tidyr}"),
                      "package enables us to carry out this action.")
                  ),
                  tabPanel(
                    title = "Separating",
                    br(),
                    p(tags$em("Separating"), "is the action of taking a column
                      and breaking that column into several new columns. The",
                      tags$code("{tidyr}"), "package has several functions that
                      help us. We can break a column at a particular character or
                      delimiter using", tags$code("separate_wider_delim"), "or we
                      can separate a column at certain positions using",
                      tags$code("separate_wider_position"), ". For more complicated
                      cases, we can also use regular expressions by using the",
                      tags$code("separate_wider_regex"), "function."
                    )
                  ),
                  tabPanel(
                    title = "Mutating",
                    br(),
                    p(tags$em("Mutating"), "is a powerful action that allows us
                      to alter a data frame in multiple ways. We can change the
                      data frame by changing the values within existing columns
                      as well as creating brand new columns that are transformations
                      of one or more existing columns. This second usage of
                      mutation allows us to create", tags$em("derived variables"),
                      "for each case in our tidy data."),
                    p("We will use the", tags$code("mutate"), "function from the",
                      tags$code("{dplyr}"), "package to carry out this action. It
                      is important to note that", tags$code("mutate"), "is a
                      wrapper fuction meaning that you will need to specify the
                      actions you want to apply to which columns before you run
                      the command."
                    )
                  )
                ),
                p("Use the Example Selector to explore different examples and
                  how they impact the displayed data table. Example code will
                  appear below."),
                selectInput(
                  inputId = "exp3Select",
                  label = "Example Selector",
                  choices = c("To be generated")
                )
              ),
              h3("Example Code"),
              uiOutput(outputId = "exp3Code"),
            ),
            column(
              width = 6,
              DTOutput(outputId = "explorePart3")
            )
          ),
          h2("Verb Connections"),
          p("How do these data verbs connect to one another?"),
          hr(),
          h2("OLD"),
          h2("Practice/Test Yourself with [Type of Game]"),
          tabsetPanel(type = 'tabs',
                      #### pivot wider ----
                      tabPanel(div(style = 'font-size: 125%', 'Pivot_wider'),
                               br(),
                               box(title = 'View An Example',

                                   p("The tidyr::pivot_wider() function in R is used to spread rows into columns."),
                                   br(),
                                   width = NULL,
                                   style = 'background-color: #ffffff; display: inline-block',
                                   materialSwitch(inputId = 'dw3',
                                                  label = 'View the Transformed Data Set',
                                                  value = FALSE),
                                   tableOutput('dwTable5'),

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

                                   p('The tidyr::pivot_longer() function in R is used to gather the columns into rows.'),
                                   br(),
                                   width = NULL,
                                   style = 'background-color: #ffffff; display: inline-block',
                                   materialSwitch(inputId = 'dw1',
                                                  label = 'View the Transformed Data Set',
                                                  value = FALSE),
                                   tableOutput('dwTable1'),

                                   tags$code('tidyr::pivot_longer(cols = c("2011","2012","2013"), names_to = "year", values_to = "n")'),
                                   br(),
                                   br(),
                                   tableOutput('dwTable2')
                               )),

                      #### summarize ----
                      tabPanel(div(style = 'font-size: 125%', 'Summarize'),
                               br(),
                               box(title = 'View An Example',

                                   p("The dplyr::summarize() function in R is used to reduce data to a single row summary
                                       per group or overall, applying functions to each group."),
                                   br(),
                                   width = NULL,
                                   style = 'background-color: #ffffff; display: inline-block',
                                   tableOutput("SummarizeData"),
                                   br(),

                                   fluidRow(
                                     column(5,
                                            selectInput(inputId = "summarizeOption",
                                                        label = "Select Your recode Option",
                                                        choices = c("Average Salary by Department",
                                                                    "Maximum Age in Each Department",
                                                                    "Total Years With Company",
                                                                    "Employee Count by Department"),
                                                        selected = "Average Salary by Department",
                                                        width = '300px'),
                                     ),
                                   ),
                                   uiOutput("summarizeUI"),
                                   br(),
                                   tableOutput("summarizeOutput")

                               )),
          )
        ),
        ### Explore Part 4 ----
        tabItem(
          tabName = "exp4",
          withMathJax(),
          h2("Data Wrangling Part 4"),
          p("The data verbs that you'll explore here do different things. However,
            they share a common bond. Explore them and see if you can come up with
            how they are connected."),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Wrangling Verb"),
                tabsetPanel(
                  id = "caseP_StrA_valN",
                  type = "tabs",
                  tabPanel(
                    title = "Binding",
                    br(),
                    p(tags$em("Binding"), "is action of choosing attaching sets
                      of rows and/or columns together to make a new data frame."),
                    p("There are two functions from the", tags$code("{dplyr}"),
                      "package that perform this action for us. If we want to
                      stick sets of rows together, we use the",
                      tags$code("bind_rows"), "function. If we want to stick
                      sets of columns together, we use the", tags$code("bind_cols"),
                      "function. It is important to know that neither of these
                      functions attend to issues of case--that is, you can use
                      these to combine rows/columns taht come from different kinds
                      of cases."),
                    p("When combining rows, all columns will be kept. If columns
                      have the same name, they will get combined. When combining
                      columns, rows are matched by index.")
                  ),
                  tabPanel(
                    title = "Joining",
                    br(),
                    p(tags$em("Joining"), "is similar to column binding, but
                      safer. Joining allows us to combine columns from two data
                      frames while matching observations/cases by id keys. Within
                      the", tags$code("{dplyr}"), "package there are four join
                      functions."),
                    tags$ul(
                        tags$li("The", tags$code("inner_join"), "function will
                                only keep all of the observations that exist in
                                both data frames along with all of their columns."),
                        tags$li("The", tags$code("left_join"), "function will
                                keep all of the observations that exist in the
                                first (left) data frame along with all of their
                                columns from both data frames. If an
                                observation/case only exists in the second data
                                frame, that observation/case is dropped."),
                        tags$li("The", tags$code("right_join"), "function will
                                keep all of the observations/cases found in the
                                second (right) data frame along with all of their
                                columns from both data frames. Any observations/cases
                                found only in the first data frame will be dropped."),
                        tags$li("The", tags$code("full_join"), "function will keep
                                all observations/cases and their columns from
                                both data frames. If an observation/case exists
                                in both data frames, it will be merged together
                                into a single entry. Observations/cases that only
                                exist in one data frame will be added as new rows.")
                      ),
                    p("To help with key matching, we will need to use the",
                      tags$code("join_by"), "helper function.")
                  )
                ),
                p("Use the Example Selector to explore different examples and
                  how they impact the displayed data table. Example code will
                  appear below."),
                selectInput(
                  inputId = "exp4Select",
                  label = "Example Selector",
                  choices = c("To be generated")
                )
              ),
              h3("Example Code"),
              uiOutput(outputId = "exp4Code"),
            ),
            column(
              width = 6,
              DTOutput(outputId = "explorePart4")
            )
          ),
          h2("Verb Connections"),
          p("How do these data verbs connect to one another?"),
        ),
        ### Explore Part 5 ----
        tabItem(
          tabName = "exp5",
          withMathJax(),
          h2("Data Wrangling Part 5"),
          p("The data verbs that you'll explore here do different things. However,
            they share a common bond. Explore them and see if you can come up with
            how they are connected."),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Wrangling Verb"),
                p(tags$em("Pivoting"), "is a reshaping action we can apply to a
                  data frame. This allows us to change the number of rows and
                  columns in a data frame without adding/removing any cases.
                  Rather, pivoting focuses on reconceputalizing what constitutes
                  a case. There are two types of pivot actions."),
                tabsetPanel(
                  id = "pivots",
                  type = "tabs",
                  tabPanel(
                    title = "Pivoting Wider",
                    br(),
                    p(tags$em("Pivoting Wider"), "results in more columns and
                      fewer rows. In the input data frame, each row represents
                      a unique observation of a case. Each case appears as part
                      of multiple observations and thus is connected to several
                      rows. By pivoting wider, we are reducing the number of rows
                      to be the same as the number of cases. We then transform
                      the observations into separate columns. The function",
                      tags$code("pivot_wider"), "from the", tags$code("{tidyr}"),
                      "package allows us to pivot wider.")
                  ),
                  tabPanel(
                    title = "Pivoting Longer",
                    br(),
                    p(tags$em("Pivoting Longer"), " results in more rows and
                      fewer columns. In the input data frame each row represents
                      a unique case. For each case we'll have multiple columns
                      that all refer to same attribute but observed at different
                      times. By pivoting longer, we'll combine each observation
                      with each case and create a separate row so that we have
                      a single column for the underlying attribute. The function",
                      tags$code("pivot_longer"), "from the", tags$code("{tidyr}"),
                      "package allows us to pivot longer.")
                  )
                ),
                p("Use the Example Selector to explore different examples and
                  how they impact the displayed data table. Example code will
                  appear below."),
                selectInput(
                  inputId = "exp5Select",
                  label = "Example Selector",
                  choices = c("To be generated")
                )
              ),
              h3("Example Code"),
              uiOutput(outputId = "exp5Code"),
            ),
            column(
              width = 6,
              DTOutput(outputId = "explorePart5")
            )
          ),
          h2("Verb Connections"),
          p("How do these data verbs connect to one another?"),
        ),
        ### Explore Part 6 ----
        tabItem(
          tabName = "exp6",
          withMathJax(),
          h2("Data Wrangling Part 6"),
          p("The data verbs that you'll explore here do different things. However,
            they share a common bond. Explore them and see if you can come up with
            how they are connected."),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Wrangling Verb"),
                tabsetPanel(
                  id = "caseA_Summary",
                  type = "tabs",
                  tabPanel(
                    title = "Summarizing",
                    br(),
                    p(tags$em("Summarizing"), "is the action of creating a new
                      data frame out of an existing data frame through the use
                      of statistics. The output data frame has a",
                      tags$em("data collection"), "as its case, not the individual
                      object/living being of the input data collection. If there
                      is grouping meta-information, the summarizing actions will
                      be applied to each group. We use the", tags$code("summarize"),
                      "function from the", tags$code("{dplyr}"), "package. Keep
                      in mind that", tags$code("summarize"), "is a wrapper function:
                      we have to specify the actions that we want to take. Each
                      inner action (typically, a separate statistic) will tell
                      us about a different attribute of the input data frame.."),
                    p("There are multiple existing functions which we can use
                      as helper functions."),
                  ),
                  tabPanel(
                    title = "Aggregating",
                    br(),
                    p(tags$em("Aggregating"), "is the idea of computing summary
                      statistics on a data collection. Much like the summarizing
                      verb, aggregating focuses on condensing an entire data
                      collection into a set of descriptive values. We use the",
                      tags$code('aggregate'), "function that is part of base R to
                      acheive this effect.")
                  )
                ),
                p("Use the Example Selector to explore different examples and
                  how they impact the displayed data table. Example code will
                  appear below."),
                selectInput(
                  inputId = "exp6Select",
                  label = "Example Selector",
                  choices = c("To be generated")
                )
              ),
              h3("Example Code"),
              uiOutput(outputId = "exp6Code"),
            ),
            column(
              width = 6,
              DTOutput(outputId = "explorePart6")
            )
          ),
          h2("Verb Connections"),
          p("How do these data verbs connect to one another?")
        ),
        ### Tidy Challenge Page ----
        tabItem(
          tabName = "tidy",
          withMathJax(),
          h2("Wizard"),
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
        ### Combining Challenge ----
        tabItem(
          tabName = "comb",
          withMathJax(),
          h2("Combinging Data Challenge"),
          fluidRow(
            column(width = 12,
                   box(div(style = 'font-weight: bold; font-size: 140%', (title = 'Multiple Choice Joins Practice')),
                       style = 'text-align: left',
                       br(),
                       width = NULL,
                       height = NULL,
                       # tags$img(
                       #   class = "centerFigure",
                       #   src = "cds.png",
                       #   width = 300,
                       #   height = 110,
                       #   alt = "Picture of the table"),

                       div(style="display:inline-block", tableOutput('titleTableA')),
                       div(style="display: inline-block; font-size: 50px; vertical-align: middle; margin-top: -20px;", "+"),  # Increased font size and aligned vertically
                       div(style="display:inline-block", tableOutput('titleTableB')),
                       div(style="display:inline-block; font-size: 50px; vertical-align: middle; margin-top: -20px;", p(' = ')),

                       br(),
                       br(),
                       p("Exercise Instructions:"),
                       p("For the exercise below, please choose the appropriate type of join that results in the table displayed. Select the option that best matches the given output."),


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
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p("You'll need to fill in this page with all of the appropriate
            references for your app."),
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
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions",
        text = "IMPROVE ME! Move the sliders or select from the dropdown menus and view the R code that produces the results.",
        type = "info"
      )
    }
  )
  ## Go button ----
  observeEvent(
    eventExpr = input$go2,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = 'pages',
        selected = 'exp1'
      )
    }
  )

  ## Wrangling Part 1 ----
  ### Original Data Table ----
  output$explorePart1 <- renderDT(
    expr = iris,
    caption = "Original Data Table",
    style = "bootstrap4",
    rownames = TRUE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      ordering = FALSE,
      paging = TRUE,
      lengthChange = TRUE,
      pageLength = 10,
      searching = FALSE,
      info = TRUE,
      columnDefs = list(
        list(className = "dt-center", targets = 1:4)
      )
    )
  )

  ### Explore 1 Actions ----
  exp1Cases <- eventReactive(
    eventExpr = input$caseP_subset,
    valueExpr = {
      verbSelectors %>%
        filter(page == "exp1" & tabName == input$caseP_subset)
    }
  )
  observeEvent(
    eventExpr = exp1Cases(),
    handlerExpr = {
      choices <- exp1Cases()$choice
      updateSelectInput(
        session = session,
        inputId = "exp1Select",
        choices = choices
      )
    }
  )

  observeEvent(
    eventExpr = input$exp1Select,
    handlerExpr = {
      #### Update Example Code ----
      currentCase <- filter(exp1Cases(), choice == input$exp1Select)
      output$exp1Code <- renderUI(
        expr = {
          tagList(
            tags$pre(tags$code(currentCase$code)),
            if (input$caseP_subset == "Selecting") {
              p("Note: you might need to use", tags$code("dplyr::select"),
                "due to a name conflict.")
            }
          )
        }
      )
      #### Create Modified Table ----
      modTable <- eval(str2lang(currentCase$code[1]))

      #### Update Result Table ----
      output$exp1Result <- renderDT(
        expr = modTable,
        caption = "Modified Data Table",
        style = "bootstrap4",
        rownames = TRUE,
        options = list(
          responsive = TRUE,
          scrollX = TRUE,
          ordering = FALSE,
          paging = TRUE,
          lengthChange = TRUE,
          pageLength = 10,
          searching = FALSE,
          info = TRUE
        )
      )

    }
  )
#
#
#
#   # Explore Data 1 ----
#   ## Select ----
#   employee_data <- data.frame(
#     EmployeeID = c('101', '102', '103', '104', '105', '106', '107', '108', '109', '110'),
#     FirstName = c('John', 'Ava', 'Maria', 'James', 'Emily', 'Daniel', 'Sara', 'William', 'Sophia', 'Alex'),
#     LastName = c('Doe', 'Smith', 'Johnson', 'Brown', 'Davis', 'Miller', 'Wilson', 'Taylor', 'Anderson', 'Thomas'),
#     Department = c('HR', 'Finance', 'IT', 'Marketing', 'Sales', 'IT', 'HR', 'Sales', 'Finance', 'Marketing'),
#     Salary = c('70000', '80000', '55000', '75000', '62000', '58000', '71000', '64000', '83000', '54000'),
#     StartDate = c("2021-01-31" , "2021-02-28" , "2021-03-31" , "2021-04-30" ,"2021-05-31" ,"2021-06-30" ,"2021-07-31" , "2021-08-31" , "2021-09-30" , "2021-10-31"),
#     Age = c('29', '33', '26', '28', '42', '36', '30', '31', '29', '27')
#   )
#   output$selectData <- renderTable ({
#     employee_data
#   })
#   output$selectOutput2 <- renderTable ({
#     if (input$se1 == 'Select columns by name'){
#       select_nData <- select(employee_data, EmployeeID, FirstName, LastName)
#     } else if (input$se1 == 'Select columns by excluding certain columns'){
#       select_nData <- select(employee_data, -Department, -Age)
#     } else if (input$se1 == 'Select columns by index number'){
#       select_nData <- select(employee_data, 1:3)
#     } else if (input$se1 == 'Select columns by a range of names'){
#       select_nData <- select(employee_data, EmployeeID:Department)
#     } else if (input$se1 == 'Rename columns while selecting'){
#       select_nData <- select(employee_data, ID = EmployeeID, Dept = Department)
#     } else if (input$se1 == 'Select columns that contain a certain string'){
#       select_nData <- select(employee_data, contains("Date"))
#     }
#   })
#   ### switch
#   output$selectUI <- renderUI({
#     if (input$se1 == "Select columns by name") {
#       tags$code(HTML("R code:<br>dplyr::select(employee_data, EmployeeID, FirstName, LastName)"))
#     } else if(input$se1 == "Select columns by excluding certain columns") {
#       tags$code(HTML("R code:<br>dplyr::select(employee_data, -Department, -Age)"))
#     } else if(input$se1 == "Select columns by index number:") {
#       tags$code(HTML("R code:<br>dplyr::select(employee_data, 1:3)"))
#     } else if(input$se1 == "Select columns by a range of names") {
#       tags$code(HTML("R code:<br>dplyr::select(employee_data, EmployeeID:Department)"))
#     } else if(input$se1 == "Rename columns while selecting") {
#       tags$code(HTML("R code:<br>dplyr::select(employee_data, ID = EmployeeID, Dept = Department)"))
#     } else if(input$se1 == "Select columns that contain a certain string") {
#       tags$code(HTML("R code:<br>dplyr::select(employee_data, contains('Date'))"))
#     } else {
#       NULL
#     }
#   })
#
#
#   ## Group_by ----
#
#   # dataset
#
#   output$groupData <- renderTable({
#     head(mtcars, 10)
#   })
#
#   # table output
#   output$groupedTable <- renderTable({
#     if (input$gr1 == "cyl") {
#       mtcars %>%
#         group_by(cyl) %>%
#         summarize(mean_mpg = mean(mpg), .groups = 'keep') %>%
#         head(10)  # Show only the first 10 rows for brevity
#     } else if (input$gr1 == "gear") {
#       mtcars %>%
#         group_by(gear) %>%
#         summarize(mean_mpg = mean(mpg), .groups = 'keep') %>%
#         head(10)  # Show only the first 10 rows for brevity
#     }
#   })
#   # ui
#
#   output$grCode <- renderUI({
#     if (input$gr1 == "cyl") {
#       tags$code(HTML("R code:<br>mtcars %>%<br> group_by(cyl) %>%<br> summarize(mean_mpg = mean(mpg), .groups = 'keep')"))
#     } else if (input$gr1 == "gear") {
#       tags$code(HTML("R code:<br>mtcars %>%<br> group_by(gear) %>%<br> summarize(mean_mpg = mean(mpg), .groups = 'keep')"))
#     } else {
#       NULL
#     }
#   })
#
#   ## Filter ----
#   output$FilterData <- renderTable({
#     head(mtcars, 10)
#   })
#
#   # table output
#   output$filterTable <- renderTable({
#     if (input$fl1 == "Filter cars with mpg greater than 20") {
#       mtcars %>%
#         filter(mpg > 20) %>%
#         head(5)
#     } else if (input$fl1 == "Filter cars with exactly 6 cylinders") {
#       mtcars %>%
#         filter(cyl == 6) %>%
#         head(5)
#     } else if (input$fl1 == "Filter cars with horsepower between 100 and 200") {
#       mtcars %>%
#         filter(between(hp, 100, 200)) %>%
#         head(5)
#     } else {
#       NULL
#     }
#   })
#
#   # code
#   output$filterCode <- renderUI({
#     if (input$fl1 == "Filter cars with mpg greater than 20") {
#       tags$code(HTML("R code:<br>mtcars %>%<br>filter(mpg > 20)"))
#     } else if (input$fl1 == "Filter cars with exactly 6 cylinders") {
#       tags$code(HTML("R code:<br>mtcars %>%<br>filter(cyl == 6)"))
#     } else if (input$fl1 == "Filter cars with horsepower between 100 and 200") {
#       tags$code(HTML("R code:<br>mtcars %>%<br>filter(between(hp, 100, 200))"))
#     } else {
#       tags$code(HTML("R code:<br>mtcars"))
#     }
#   })
#
#
#
#   ## Arrange ----
#   output$dwTable8 <- renderTable ({
#     if (input$dwSTI2 == 'Low to High') {
#       head(dplyr::arrange(mtcars, mtcars[ , input$dwSTI1]))
#     }
#     else if (input$dwSTI2 == 'High to Low') {
#       head(dplyr::arrange(mtcars, desc(mtcars[ , input$dwSTI1])))
#     }
#     else {
#       head(head(mtcars))
#     }
#   })
#   output$code1 <- renderUI({
#     if (input$dwSTI2 == 'Low to High') {
#       tags$code(HTML(paste('R Code:<br>dplyr::arrange(mtcars, mtcars[ , ', input$dwSTI1, '])', sep = '')))
#     }
#   })
#
#   output$code2 <- renderUI({
#     if (input$dwSTI2 == 'High to Low') {
#       tags$code(HTML(paste('R Code:<br>dplyr::arrange(mtcars, desc(mtcars[ , ', input$dwSTI1, ']))', sep = '')))
#     }
#   })
#
#
#
#   # Explore Data 2 ----
#   ## unite ----
#
#   birth_df<- data.frame(
#     first = c("John", "Jane", "Alice", "Bob", "Eve", "Sam", "Lucy", "Tom", "Sue", "Roy"),
#     last = c("Doe", "Smith", "Johnson", "Brown", "Davis", "Miller", "Garcia", "Rodriguez", "Martinez", "Hernandez"),
#     day = c('12','5', '23', '18', '29', '14', '9', '1', '27', '15'),
#     month = c("1", "9", "3", "5", "8", "6", "12", "8", "6", "4"),
#     year = c('1990', '1985', '1992', '1988', '1994', '1987', '1991', '1989', '1993', '1996')
#   )
#   output$uniteData <- renderTable ({
#     birth_df
#   })
#
#   output$uniteOutput2 <- renderTable({
#     if (input$un1 == "Day, Month, Year") {
#       united_data <- unite(birth_df, "Birth_Date", day, month, year, sep = "-")
#     } else if (input$un1 == "First_Name, Last_Name") {
#       united_data <- unite(birth_df, "Full_Name", first, last, sep = " ")
#     } else {
#       united_data <- birth_df
#     }
#   })
#
#   output$uniteUI <- renderUI({
#     if (input$un1 == "Day, Month, Year") {
#       tags$code(HTML("R code:<br>tidyr::unite(birth_df, 'Date', day, month, year, sep = '-')"))
#     } else if (input$un1 == "First_Name, Last_Name") {
#       tags$code(HTML("R code:<br>tidyr::unite(birth_df, 'Full_Name', first, last, sep = '-')"))
#     } else {
#       NULL
#     }
#   })
#
#   ## Mutate ----
#   output$mutateData <- renderTable({
#     head(mtcars, 7)
#   })
#
#   output$mutateOutput <- renderTable({
#     if (input$mutateOption == "Create New Column") {
#       mutated_data <- mtcars %>%
#         mutate(new_column = mpg * cyl) %>%
#         head(5)
#     } else if (input$mutateOption == "Modify Existing Column") {
#       mutated_data <- mtcars %>%
#         mutate(mpg = mpg / 2) %>%
#         head(5)
#     } else if (input$mutateOption == "Use Multiple Columns") {
#       mutated_data <- mtcars %>%
#         mutate(power_to_weight = hp / wt) %>%
#         head(5)
#     } else if (input$mutateOption == "Use with Other Functions") {
#       mutated_data <- mtcars %>%
#         mutate(log_mpg = log(mpg)) %>%
#         head(5)
#     } else if (input$mutateOption == "Multiple Mutations") {
#       mutated_data <- mtcars %>%
#         mutate(
#           log_mpg = log(mpg),
#           wt_kg = wt * 453.592,
#           power_to_weight = hp / wt
#         ) %>%
#         head(5)
#     } else if (input$mutateOption == "Conditional Mutations") {
#       mutated_data <- mtcars %>%
#         mutate(
#           efficiency = case_when(
#             mpg > 20 ~ "High",
#             mpg <= 20 ~ "Low",
#             TRUE ~ NA_character_
#           )
#         ) %>%
#         head(5)
#     } else {
#       NULL
#     }
#
#   })
#
#   output$mutateCode <- renderUI({
#     if (input$mutateOption == "Create New Column") {
#       div(
#         tags$code(HTML("R code:<br>mtcars %>%<br>mutate(new_column = mpg * cyl)")),
#         p("Creates a new column by multiplying miles per gallon (mpg) with the number of cylinders (cyl).")
#       )
#     } else if (input$mutateOption == "Modify Existing Column") {
#       div(
#         tags$code(HTML("R code:<br>mtcars %>%<br>mutate(mpg = mpg / 2)")),
#         p("Modifies the existing mpg column by dividing each value by 2.")
#       )
#     } else if (input$mutateOption == "Use Multiple Columns") {
#       div(
#         tags$code(HTML("R code:<br>mtcars %>%<br>mutate(power_to_weight = hp / wt)")),
#         p("Creates a new column for power-to-weight ratio by dividing horsepower (hp) by weight (wt).")
#       )
#     } else if (input$mutateOption == "Use with Other Functions") {
#       div(
#         tags$code(HTML("R code:<br>mtcars %>%<br>mutate(log_mpg = log(mpg))")),
#         p("Creates a new column with the natural logarithm of the miles per gallon (mpg) values.")
#       )
#     } else if (input$mutateOption == "Multiple Mutations") {
#       div(
#         tags$code(HTML("R code:<br>mtcars %>%<br>mutate(log_mpg = log(mpg), wt_kg = wt * 453.592, power_to_weight = hp / wt)")),
#         p("Performs multiple mutations to create new columns: log_mpg, weight in kilograms (wt_kg), and power-to-weight ratio.")
#       )
#     } else if (input$mutateOption == "Conditional Mutations") {
#       div(
#         tags$code(HTML("R code:<br>mtcars %>%<br>mutate(efficiency = case_when(mpg > 20 ~ 'High', mpg <= 20 ~ 'Low', TRUE ~ NA_character_))")),
#         p("Creates a new column to categorize efficiency as 'High' or 'Low' based on mpg being greater than or equal to 20.")
#       )
#     } else {
#       NULL
#     }
#   })
#
#   ## Seperate ----
#   people_events <- data.frame(
#     full_name = c("John_Doe", "Jane_Smith", "Alice_Johnson", "Bob_Brown", "Eve_Davis", "Sam_Miller", "Lucy_Garcia"),
#     date_of_event = c("2021-01-05", "2021-02-15", "2021-03-25", "2021-04-10", "2021-05-20", "2021-06-30", "2021-07-15")
#   )
#
#   output$SeperateData <- renderTable ({
#     people_events
#   })
#
#   output$separateOutput <- renderTable({
#     if (input$sepOption == "Separate Full Name") {
#       separated_data <- people_events %>%
#         separate(full_name, into = c("first_name", "last_name"), sep = "_")
#     } else if (input$sepOption == "Separate Date of Event") {
#       separated_data <- people_events %>%
#         separate(date_of_event, into = c("year", "month", "day"), sep = "-")
#     } else {
#       NULL
#     }
#   })
#
#   # Output for renderUI to show the R code
#   output$separateCode <- renderUI({
#     if (input$sepOption == "Separate Full Name") {
#       tags$code("R code: people_events %>% separate(full_name, into = c('first_name', 'last_name'), sep = '_')")
#
#     } else if (input$sepOption == "Separate Date of Event") {
#       tags$code("R code: people_events %>% separate(date_of_event, into = c('year', 'month', 'day'), sep = '-')")
#
#     } else {
#       NULL
#     }
#   })
#
#   ## Recode ----
#
#   survey_data <- data.frame(
#     age_group = c("0-18", "19-35", "36-55", "56+", "19-35", "0-18", "36-55"),
#     satisfaction_level = c("Very Unhappy", "Unhappy", "Neutral", "Happy", "Very Happy", "Neutral", "Happy"),
#     region_code = c("R1", "R2", "R3", "R1", "R2", "R3", "R1")
#   )
#
#   output$RecodeData <- renderTable ({
#     survey_data
#   })
#
#   # Output
#   output$recodeOutput <- renderTable({
#     if (input$recodeOption == "Recode Satisfaction Level") {
#       recoded_data <- survey_data %>%
#         mutate(satisfaction_score = recode(satisfaction_level,
#                                            "Very Unhappy" = 1,
#                                            "Unhappy" = 2,
#                                            "Neutral" = 3,
#                                            "Happy" = 4,
#                                            "Very Happy" = 5))
#     } else if (input$recodeOption == "Recode Age Group") {
#       recoded_data <- survey_data %>%
#         mutate(age_group_label = recode(age_group,
#                                         "0-18" = "Youth",
#                                         "19-35" = "Young Adult",
#                                         "36-55" = "Adult",
#                                         "56+" = "Senior"))
#     } else if (input$recodeOption == "Recode Region Code") {
#       recoded_data <- survey_data %>%
#         mutate(region_name = recode(region_code,
#                                     "R1" = "North",
#                                     "R2" = "East",
#                                     "R3" = "West"))
#     } else {
#       NULL
#     }
#   })
#
#   # UI
#   output$recodeUI <- renderUI({
#     if (input$recodeOption == "Recode Satisfaction Level") {
#       tags$code(HTML("R code:<br>survey_data %>%<br>mutate(satisfaction_score = recode(satisfaction_level, 'Very Unhappy' = 1, 'Unhappy' = 2, 'Neutral' = 3, 'Happy' = 4, 'Very Happy' = 5))"))
#     } else if (input$recodeOption == "Recode Age Group") {
#       tags$code(HTML("R code:<br>survey_data %>%<br>mutate(age_group_label = recode(age_group, '0-18' = 'Youth', '19-35' = 'Young Adult', '36-55' = 'Adult', '56+' = 'Senior'))"))
#     } else if (input$recodeOption == "Recode Region Code") {
#       tags$code(HTML("R code:<br>survey_data %>%<br>mutate(region_name = recode(region_code, 'R1' = 'North', 'R2' = 'East', 'R3' = 'West'))"))
#     } else {
#       NULL
#     }
#   })
#
#
#   # Explore data 3 ----
#
#   ## pivot_longer ----
#   output$dwTable5 <- renderTable({
#     pollution
#   })
#
#   output$dwTable6 <- renderTable({
#     if (input$dw3 == TRUE) {
#       pollution %>%
#         tidyr::pivot_wider(names_from = "size", values_from = "amount")
#     }
#   })
#
#   ## pivot_wider ----
#   output$dwTable1 <- renderTable({
#     cases
#   })
#
#   output$dwTable2 <- renderTable({
#     if (input$dw1 == TRUE) {
#       cases %>%
#         tidyr::pivot_longer(cols = c("2011","2012","2013"), names_to = "year", values_to = "n")
#     }
#   })
#
#   ## Summarise ----
#
#   sample_data <- data.frame(
#     EmployeeID = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110),
#     Age = c(29, 33, 26, 28, 42, 36, 30, 31, 29, 27),
#     Salary = c(70000, 80000, 55000, 75000, 62000, 58000, 71000, 64000, 83000, 54000),
#     Department = c("Sales", "HR", "IT", "Sales", "IT", "HR", "HR", "Sales", "IT", "Sales"),
#     YearsWithCompany = c(3, 4, 2, 5, 3, 2, 1, 5, 6, 4)
#   )
#
#   output$SummarizeData <- renderTable ({
#     sample_data
#   })
#
#   output$summarizeOutput <- renderTable({
#     if (input$summarizeOption == "Average Salary by Department") {
#       summarize_data <- sample_data %>%
#         group_by(Department) %>%
#         summarize(AverageSalary = mean(Salary))
#     } else if (input$summarizeOption == "Maximum Age in Each Department") {
#       summarize_data <- sample_data %>%
#         group_by(Department) %>%
#         summarize(MaxAge = max(Age))
#     } else if (input$summarizeOption == "Total Years With Company") {
#       summarize_data <- sample_data %>%
#         summarize(TotalYearsWithCompany = sum(YearsWithCompany))
#     } else if (input$summarizeOption == "Employee Count by Department") {
#       summarize_data <- sample_data %>%
#         group_by(Department) %>%
#         summarize(EmployeeCount = n())
#     } else {
#       NULL
#     }
#   })
#
#   output$summarizeUI <- renderUI({
#     if (input$summarizeOption == "Average Salary by Department") {
#       tags$code(HTML("R code:<br>sample_data %>%<br>group_by(Department) %>%<br>summarize(AverageSalary = mean(Salary))"))
#     } else if (input$summarizeOption == "Maximum Age in Each Department") {
#       tags$code(HTML("R code:<br>sample_data %>%<br>group_by(Department) %>%<br>summarize(MaxAge = max(Age))"))
#     } else if (input$summarizeOption == "Total Years With Company") {
#       tags$code(HTML("R code:<br>sample_data %>%<br>summarize(TotalYearsWithCompany = sum(YearsWithCompany))"))
#     } else if (input$summarizeOption == "Employee Count by Department") {
#       tags$code(HTML("R code:<br>sample_data %>%<br>group_by(Department) %>%<br>summarize(EmployeeCount = n())"))
#     } else {
#       NULL
#     }
#   })
#
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

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
