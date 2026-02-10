# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(DT)
library(dplyr)
library(tidyr)
library(readr)

# Load Data ----
verbSelectors <- read.table(file = "verbSelectors.csv", header = TRUE, sep = ",")
data(iris)
oreoData <- readRDS(file = "oreoData.RDS")
psuFbRoster <- readRDS(file = "psuFootball24.RDS")
womenTrailblazers <- readRDS(file = "womenTrailblazers.RDS")
load("mmCandies.RData")
load("dataCareers.RData")
load("pivots.RData")

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
            )
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
            div(class = "updated", "Last Update: 2/10/2026 by NJH.")
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
            recorded those values to create data. Depending on the context a case
            could refer to a singluar individual or to a group of individuals."),
          p("An", tags$strong("observation"), "refers to an instance where we
            have measured/categorized the value of one more attributes for a
            particular case at either a particular time or situation. We can make
            multiple observations of the same case."),
          p("We say that a data frame is", tags$strong("tidy"), "when the data
            frame meets two conditions:"),
          tags$ol(
            tags$li("Each row of the data frame represents a unique case."),
            tags$li("Each column of the data frame represents a specific attribute or
                    characteristic that all cases possess.")
          ),
          p("How we think about each cell in a data frame (i.e., the intersection
            of a particular row and column) is directly impacted by the two rules
            of tidy data. The value contained in any cell reflects that particular
            case's instantiation of the current attribute."
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
          p("For each wrangling verb, use the Example Selector to explore different
            examples and how they impact the displayed data table. The original
            data table appears below and to the left. Example code for the
            wrangling and a resulting data frame appear to the right."),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Wrangling Verb"),
                tabsetPanel(
                  id = "caseP_subset",
                  type = "tabs",
                  tabPanel(
                    title = "Selecting",
                    br(),
                    p(tags$em("Selecting"), "refers to the action of choosing
                      which case attributes (or characteristics) we want to keep
                      from an input data frame for an output data frame. For tidy
                      data, this means that we are selecting which columns to
                      keep and remove. The function", tags$code("select"),
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
              h3("Verb Connections"),
              p("How do these data verbs connect to one another? Think about what
            these verbs all have in common when we think about the following
            questions:"),
              tags$ul(
                tags$li("How are these verbs all alike in how they treat what
                    makes up a case?"),
                tags$li("How are these verbs all alike in how they treat values of
                    case attributes?"),
                tags$li("How are these verbs all alike in their general purpose of
                    their actions?")
              )
            ),
            column(
              width = 8,
              h3("Verb in Action"),
              p("Check out the original data table, the example code, and the
                resulting/modified data table for each verb."),
              DTOutput(outputId = "explorePart1"),
              uiOutput(outputId = "exp1Code"),
              DTOutput(outputId = "exp1Result")
            )
          )
        ),
        ### Explore Part 2 ----
        tabItem(
          tabName = "exp2",
          withMathJax(),
          h2("Data Wrangling Part 2"),
          p("The data verbs that you'll explore here do different things. However,
            they share a common bond. Explore them and see if you can come up with
            how they are connected."),
          p("For each wrangling verb, use the Example Selector to explore different
            examples and how they impact the displayed data table. The original
            data table appears below and to the left. Example code for the
            wrangling and a resulting data frame appear to the right."),
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
                selectInput(
                  inputId = "exp2Select",
                  label = "Example Selector",
                  choices = c("To be generated")
                )
              ),
              h3("Verb Connections"),
              p("How do these data verbs connect to one another? Think about what
            these verbs all have in common when we think about the following
            questions:"),
              tags$ul(
                tags$li("How are these verbs all alike in how they treat what
                    makes up a case?"),
                tags$li("How are these verbs all alike in how they treat values of
                    case attributes?"),
                tags$li("How are these verbs all alike in their general purpose of
                    their actions?")
              )
            ),
            column(
              width = 6,
              h3("Verb in Action"),
              p("Check out the original data table, the example code, and the
                resulting/modified data table for each verb."),
              DTOutput(outputId = "explorePart2"),
              uiOutput(outputId = "exp2Code"),
              DTOutput(outputId = "exp2Result")
            )
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
          p("For each wrangling verb, use the Example Selector to explore different
            examples and how they impact the displayed data table. The original
            data table appears below and to the left. Example code for the
            wrangling and a resulting data frame appear to the right."),
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
                selectInput(
                  inputId = "exp3Select",
                  label = "Example Selector",
                  choices = c("To be generated")
                )
              ),
              h3("Verb Connections"),
              p("How do these data verbs connect to one another? Think about what
            these verbs all have in common when we think about the following
            questions:"),
              tags$ul(
                tags$li("How are these verbs all alike in how they treat what
                    makes up a case?"),
                tags$li("How are these verbs all alike in how they treat values of
                    case attributes?"),
                tags$li("How are these verbs all alike in their general purpose of
                    their actions?")
              )
            ),
            column(
              width = 6,
              h3("Verb in Action"),
              p("Check out the original data table, the example code, and the
                resulting/modified data table for each verb."),
              DTOutput(outputId = "explorePart3"),
              uiOutput(outputId = "exp3Code"),
              DTOutput(outputId = "exp3Result")
            )
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
          p("For each wrangling verb, use the Example Selector to explore different
            examples and how they impact the displayed data table. The original
            data table appears below and to the left. Example code for the
            wrangling and a resulting data frame appear to the right."),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Wrangling Verb"),
                tabsetPanel(
                  id = "caseP_strA_valN",
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
                selectInput(
                  inputId = "exp4Select",
                  label = "Example Selector",
                  choices = c("To be generated")
                )
              ),
              h3("Verb Connections"),
              p("How do these data verbs connect to one another? Think about what
            these verbs all have in common when we think about the following
            questions:"),
              tags$ul(
                tags$li("How are these verbs all alike in how they treat what
                    makes up a case?"),
                tags$li("How are these verbs all alike in how they treat values of
                    case attributes?"),
                tags$li("How are these verbs all alike in their general purpose of
                    their actions?")
              )
            ),
            column(
              width = 6,
              h3("Verb in Action"),
              p("Check out the original data tables, the example code, and the
                resulting/modified data table for each verb."),
              DTOutput("explorePart4A"),
              DTOutput("explorePart4B"),
              uiOutput(outputId = "exp4Code"),
              DTOutput(outputId = "exp4Result")
            )
          )
        ),
        ### Explore Part 5 ----
        tabItem(
          tabName = "exp5",
          withMathJax(),
          h2("Data Wrangling Part 5"),
          p("The data verbs that you'll explore here do different things. However,
            they share a common bond. Explore them and see if you can come up with
            how they are connected."),
          p("For each wrangling verb, use the Example Selector to explore different
            examples and how they impact the displayed data table. The original
            data table appears below and to the left. Example code for the
            wrangling and a resulting data frame appear to the right."),
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
                selectInput(
                  inputId = "exp5Select",
                  label = "Example Selector",
                  choices = c("To be generated")
                )
              ),
              h3("Verb Connections"),
              p("How do these data verbs connect to one another? Think about what
            these verbs all have in common when we think about the following
            questions:"),
              tags$ul(
                tags$li("How are these verbs all alike in how they treat what
                    makes up a case?"),
                tags$li("How are these verbs all alike in how they treat values of
                    case attributes?"),
                tags$li("How are these verbs all alike in their general purpose of
                    their actions?")
              )
            ),
            column(
              width = 6,
              h3("Verb in Action"),
              p("Check out the original data table, the example code, and the
                resulting/modified data table for each verb."),
              DTOutput(outputId = "explorePart5"),
              uiOutput(outputId = "exp5Code"),
              DTOutput(outputId = "exp5Result")
            )
          )
        ),
        ### Explore Part 6 ----
        tabItem(
          tabName = "exp6",
          withMathJax(),
          h2("Data Wrangling Part 6"),
          p("The data verbs that you'll explore here do different things. However,
            they share a common bond. Explore them and see if you can come up with
            how they are connected."),
          p("For each wrangling verb, use the Example Selector to explore different
            examples and how they impact the displayed data table. The original
            data table appears below and to the left. Example code for the
            wrangling and a resulting data frame appear to the right."),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Wrangling Verb"),
                tabsetPanel(
                  id = "caseA_summary",
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
                selectInput(
                  inputId = "exp6Select",
                  label = "Example Selector",
                  choices = c("To be generated")
                )
              ),
              h3("Verb Connections"),
              p("How do these data verbs connect to one another? Think about what
            these verbs all have in common when we think about the following
            questions:"),
              tags$ul(
                tags$li("How are these verbs all alike in how they treat what
                    makes up a case?"),
                tags$li("How are these verbs all alike in how they treat values of
                    case attributes?"),
                tags$li("How are these verbs all alike in their general purpose of
                    their actions?")
              )
            ),
            column(
              width = 6,
              h3("Verb in Action"),
              p("Check out the original data table, the example code, and the
                resulting/modified data table for each verb."),
              DTOutput(outputId = "explorePart6"),
              uiOutput(outputId = "exp6Code"),
              DTOutput(outputId = "exp6Result")
            )
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(class = "hangingindent",
            "Anderson, E. (1935). The irises of the Gaspe Peninsula. Bulletin of
            the American Iris Society, 59, 2-5. [Data]."
          ),
          p(class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(class = "hangingindent",
            "Billard, L., & Wallman, K. K. (2020). Women trailblazers in the
            statistical profession. International Statistical Review, 88(2),
            280–301. https://doi.org/10.1111/insr.12386 [Data]."
          ),
          p(class = "hangingindent",
            "Burnham, K. (2024). The top 10 highest-paying big data careers. Blog
            post at Northeastern University. Available from
            # https://graduate.northeastern.edu/resources/highest-paying-big-data-careers/
            [Data]."
          ),
          p(class = "hangingindent",
            "Carey, R. and Hatfield, N. (2024). boastUtils: BOAST utilities.
            (0.1.12.2). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., MchPherson, J., Dipert, A., and Borges, B. (2024). shiny:
            Web application framework for R. (v 1.9.1). [R package]. Available from
            https://CRAN.R-project.org/package=shiny"
          ),
          p(class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2021). shinydashboard: Create
            dashboards with 'Shiny'. (v 0.7.2). [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(class = "hangingindent",
            "Data USA. (n.d.). State College, PA: Population and diversity.
            Available from https://datausa.io/profile/geo/state-college-pa?redirect=true
            [Data]."
          ),
          p(class = "hangingindent",
            "Hatfield, N. J. (2024). Oreo cookie data: large set one. [Data]."
          ),
          p(class = "hangingindent",
            "Hatfield, N. J. (2024). Paper airplane data: Stat461-Spring 2024.
            [Data]."
          ),
          p(class = "hangingindent",
            "Hatfield, N. J. (2023). Snickerdoodle taste test data: Stat461-Fall
            2023. [Data]."
          ),
          p(class = "hangingindent",
            "Penn State Athletics. (2024). Penn State Football 2024 Roster. [Data].
            Available from https://www.espn.com/college-football/team/roster/_/id/213"
          ),
          p(class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2024). shinyWidgets: Custom
            inputs widgets for shiny. (v 0.8.6). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(class = "hangingindent",
            "United States Bureau of Labor Statistics. (2024). Employment
            projections. Available from https://data.bls.gov/projections/occupationProj
            [Data]."
          ),
          p(class = "hangingindent",
            "Wicham, H., François, R., Henry, L., Müller, K., and Vaughan, D.
            (2023). dplyr: A grammar of data manipulation. (v 1.1.4). [R package].
            Available from https://CRAN.R-project.org/package=dplyr"
          ),
          p(class = "hangingindent",
            "Wickham, H., Hester, J., and Bryan, J. (2024). readr: Read rectangular
            text data. (v 2.1.5). [R package]. Available from
            https://CRAN.R-project.org/package=readr"
          ),
          p(class = "hangingindent",
            "Wickham, H., Vaughan, D., and Girlich, M. (2024). tidyr: Tidy messy
            data. (v 1.3.1). [R package]. Available from
            https://CRAN.R-project.org/package=tidyr"
          ),
          p(class = "hangingindent",
            "Wicklin, R. (2017). The distribution of colors for plain M&M candies.
            The DO Loop blog of SAS. Available at
            https://blogs.sas.com/content/iml/2017/02/20/proportion-of-colors-mandms.html
            [Data]"
          ),
          p(class = "hangingindent",
            "Xie, Y., Cheng, J., and Tan, X. (2024). DT: A wrapper of the JavaScript
            library 'DataTables'. (v 0.33). [R package]. Available from
            https://CRAN.R-project.org/package=DT"
          ),
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
    caption = "Original Data Table-Iris Data",
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
      verbSelectors |>
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
            h4("Example Code"),
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

  ## Wrangling Part 2 ----
  ### Original Data Table ----
  output$explorePart2 <- renderDT(
    expr = oreoData,
    caption = "Original Data Table-Oreo Data",
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
      info = TRUE#,
      # columnDefs = list(
      #   list(className = "dt-center", targets = 1:4)
      # )
    )
  )

  ### Explore 2 Actions ----
  exp2Cases <- eventReactive(
    eventExpr = input$caseP_strA_valP,
    valueExpr = {
      verbSelectors |>
        filter(page == "exp2" & tabName == input$caseP_strA_valP)
    }
  )
  observeEvent(
    eventExpr = exp2Cases(),
    handlerExpr = {
      choices <- exp2Cases()$choice
      updateSelectInput(
        session = session,
        inputId = "exp2Select",
        choices = choices
      )
    }
  )

  observeEvent(
    eventExpr = input$exp2Select,
    handlerExpr = {
      #### Update Example Code ----
      currentCase <- filter(exp2Cases(), choice == input$exp2Select)
      output$exp2Code <- renderUI(
        expr = {
          tagList(
            h4("Example Code"),
            tags$pre(tags$code(currentCase$code)),
            if (input$caseP_strA_valP == "Grouping") {
              p("Notice that there is no visual change to the data frame when
                we alter the grouping structural information.")
            }
          )
        }
      )
      #### Create Modified Table ----
      modTable <- eval(str2lang(currentCase$code[1]))

      ### Update Result Table ----
      output$exp2Result <- renderDT(
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

  ## Wrangling Part 3 ----
  ### Explore 3 Actions ----
  exp3Cases <- eventReactive(
    eventExpr = input$caseP_strA_valA,
    valueExpr = {
      verbSelectors |>
        filter(page == "exp3" & tabName == input$caseP_strA_valA)
    }
  )
  observeEvent(
    eventExpr = exp3Cases(),
    handlerExpr = {
      choices <- exp3Cases()$choice
      updateSelectInput(
        session = session,
        inputId = "exp3Select",
        choices = choices
      )
    }
  )

  observeEvent(
    eventExpr = input$exp3Select,
    handlerExpr = {
      e3Data <- switch(
        EXPR = input$caseP_strA_valA,
        Uniting = list(data = womenTrailblazers, title = "Women in Stats"),
        Separating = list(data = psuFbRoster, title = "PSU Football Roster"),
        Mutating = list(data = psuFbRoster, title = "PSU Football Roster")
      )

      #### Original Data Table ----
      output$explorePart3 <- renderDT(
        expr = e3Data[["data"]],
        caption = paste0("Original Data Table-",e3Data[["title"]]),
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
      #### Update Example Code ----
      currentCase <- filter(exp3Cases(), choice == input$exp3Select)
      output$exp3Code <- renderUI(
        expr = {
          tagList(
            h4("Example Code"),
            tags$pre(tags$code(currentCase$code)),
            if (input$exp3Select %in% c("Parse Number", "Rescaling")) {
              p("We used the", tags$code("parse_number"), "function from the",
                tags$code("{readr}"), "package to automatically convert the",
                tags$code("WT"), "variable to a numeric data type (instead of the
                original character data type)." )
            }
          )
        }
      )
      #### Create Modified Table ----
      modTable <- eval(str2lang(currentCase$code[1]))

      #### Update Result Table ----
      output$exp3Result <- renderDT(
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

  ## Wrangling Part 4 ----
  ### Explore 4 Actions ----
  exp4Cases <- eventReactive(
    eventExpr = input$caseP_strA_valN,
    valueExpr = {
      verbSelectors |>
        filter(page == "exp4" & tabName == input$caseP_strA_valN)
    }
  )
  observeEvent(
    eventExpr = exp4Cases(),
    handlerExpr = {
      choices <- exp4Cases()$choice
      updateSelectInput(
        session = session,
        inputId = "exp4Select",
        choices = choices
      )
    }
  )

  observeEvent(
    eventExpr = input$exp4Select,
    handlerExpr = {
      #### Original Data Table A ----
      data4A <- switch(
        EXPR = input$caseP_strA_valN,
        Binding = list(data = plainMMsA, title = "Plain M&M Color Data A"),
        Joining = list(data = dataJobs, title = "Data Job Median Salaries")
      )

      data4B <- switch(
        EXPR = input$caseP_strA_valN,
        Binding = switch(
          EXPR = input$exp4Select,
          `Adding Rows` = list(data = plainMMsB, title = "Plain M&M Color Data B"),
          `Adding Columns` = list(data = plainMMsC, title = "Plain M&M Color Data C")
        ),
        Joining = list(data = dataProjections, title = "Data Career Employment Projections")
      )

      output$explorePart4A <- renderDT(
        expr = data4A[["data"]],
        caption = paste0("Original Left Data Table-", data4A[["title"]]),
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
      ### Original Data Table B ----
      output$explorePart4B <- renderDT(
        expr = data4B[["data"]],
        caption = paste0("Original Right Data Table-", data4B[["title"]]),
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

      #### Update Example Code ----
      currentCase <- filter(exp4Cases(), choice == input$exp4Select)
      output$exp4Code <- renderUI(
        expr = {
          tagList(
            h4("Example Code"),
            tags$pre(tags$code(currentCase$code))
          )
        }
      )
      #### Create Modified Table ----
      modTable <- eval(str2lang(currentCase$code[1]))

      #### Update Result Table ----
      output$exp4Result <- renderDT(
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

  ## Wrangling Part 5 ----
  ### Explore 5 Actions ----
  exp5Cases <- eventReactive(
    eventExpr = input$pivots,
    valueExpr = {
      verbSelectors |>
        filter(page == "exp5" & tabName == input$pivots)
    }
  )
  observeEvent(
    eventExpr = exp5Cases(),
    handlerExpr = {
      choices <- exp5Cases()$choice
      updateSelectInput(
        session = session,
        inputId = "exp5Select",
        choices = choices
      )
    }
  )

  observeEvent(
    eventExpr = input$exp5Select,
    handlerExpr = {
      #### Original Data Table ----
      data5 <- switch(
        EXPR = input$pivots,
        `Pivoting Wider` = switch(
          EXPR = input$exp5Select,
          `Multiple Time Points` = list(data = paperPlanesLong, title = "Paper Airplanes"),
          `Multiple Conditions` = list(data = cookieDataLong, title = "Snickerdoodle Taste Testing")
        ),
        `Pivoting Longer` = switch(
          EXPR = input$exp5Select,
          `Multiple Time Points` = list(data = population, title = "State College, PA Population"),
          `Multiple Conditions` = list(data = cookieDataWide, title = "Snickerdoodle Taste Testing")
        )
      )
      ##### Display table
      output$explorePart5 <- renderDT(
        expr = data5[["data"]],
        caption = paste("Original Data Table-", data5[["title"]]),
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

      #### Update Example Code ----
      currentCase <- filter(exp5Cases(), choice == input$exp5Select)
      output$exp5Code <- renderUI(
        expr = {
          tagList(
            h4("Example Code"),
            tags$pre(tags$code(currentCase$code))
          )
        }
      )
      #### Create Modified Table ----
      modTable <- eval(str2lang(currentCase$code[1]))

      #### Update Result Table ----
      output$exp5Result <- renderDT(
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

  ## Wrangling Part 6 ----
  ### Original Data Table ----
  output$explorePart6 <- renderDT(
    expr = oreoData,
    caption = "Original Data Table-Oreo Data",
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

  ### Explore 6 Actions ----
  exp6Cases <- eventReactive(
    eventExpr = input$caseA_summary,
    valueExpr = {
      verbSelectors |>
        filter(page == "exp6" & tabName == input$caseA_summary)
    }
  )
  observeEvent(
    eventExpr = exp6Cases(),
    handlerExpr = {
      choices <- exp6Cases()$choice
      updateSelectInput(
        session = session,
        inputId = "exp6Select",
        choices = choices
      )
    }
  )

  observeEvent(
    eventExpr = input$exp6Select,
    handlerExpr = {
      #### Update Example Code ----
      currentCase <- filter(exp6Cases(), choice == input$exp6Select)
      output$exp6Code <- renderUI(
        expr = {
          tagList(
            h4("Example Code"),
            tags$pre(tags$code(currentCase$code)),
            if (input$exp6Select == "Using a Formula for Summary") {
              p("The", tags$code("summary"), "function returns an array as the
                value for each cell. To tidy the resulting data frame, we pass
                the output of ", tags$code("aggregate"), "to a set of commands
                that expand the array and give us just the rows and columns we
                want.")
            }
          )
        }
      )
      #### Create Modified Table ----
      modTable <- eval(str2lang(currentCase$code[1]))

      #### Update Result Table ----
      output$exp6Result <- renderDT(
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

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
