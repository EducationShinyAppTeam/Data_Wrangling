library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(shinyWidgets)
library(dplyr)
library(EDAWR)
library(mosaic)
library(plot3D)
library(plotly)
library(ggplot2)
library(ggmap)
library(tidyr)
library(knitr)
library(datasets)
library(rmarkdown)
library(learnr)
library(rcfss)
library(rLocker)
library(datasets)  
library(rmarkdown) 
library(learnr)    
library(rcis)  
library(shinycssloaders)
library(boastUtils)

#https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf
# If issues develop wiht rcfss or EDAWR these may come in useful
# remotes::install_github("uc-cfss/rcfss")
# devtools::install_github("rstudio/EDAWR")

bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE) #This contains all of the questions, possible responses and answers
#View(bank) #way that you can view the questions, comment when not using


source("helpers.R")

## i button
shinyServer(function(input, output, session) {
  observeEvent(input$info0,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Move the sliders or select from the dropdown menus and view the R code that produces the results.",
      type = "info"
    )
  })
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Move the sliders or select from the dropdown menus and view the R code that produces the results.",
      type = "info"
    )
  })
  observeEvent(input$go2, {
    updateTabItems(session, 'tabs', 'exp1')
  })





  ################# Tidy Data #################

  #### Explore Data ############
  
  # unite
  output$uniteUI <- renderUI ({
    if (input$unite3 == T) {
      if(length(input$unite1) == 1)
        tags$code(paste('R code: tidyr::unite(mtcars, "New_Column_Name", ',input$unite1[[1]],')'))
      else if(length(input$unite1) == 2)
        tags$code(paste('R code: tidyr::unite(mtcars, "New_Column_Name", ',input$unite1[[1]],',', input$unite1[[2]],')'))
      else if(length(input$unite1) == 3)
        tags$code(paste('R code: tidyr::unite(mtcars, "New_Column_Name", ',input$unite1[[1]],',', input$unite1[[2]],',',
                        input$unite1[[3]],')'))
      else if(length(input$unite1) == 4)
        tags$code(paste('R code: tidyr::unite(mtcars, "New_Column_Name", ',input$unite1[[1]],',', input$unite1[[2]],',',
                        input$unite1[[3]],',', input$unite1[[4]],')'))
      else if(length(input$unite1) == 5)
        tags$code(paste('R code: tidyr::unite(mtcars, "New_Column_Name", ',input$unite1[[1]],',', input$unite1[[2]],',',
                        input$unite1[[3]],',', input$unite1[[4]],',', input$unite1[[5]],')'))
      else if(length(input$unite1) == 6)
        tags$code(paste('R code: tidyr::unite(mtcars, "New_Column_Name", ',input$unite1[[1]],',', input$unite1[[2]],',',
                        input$unite1[[3]],',', input$unite1[[4]],',', input$unite1[[5]],',', input$unite1[[6]],')'))
      else if(length(input$unite1) == 7)
        tags$code(paste('R code: tidyr::unite(mtcars, "New_Column_Name", ',input$unite1[[1]],',', input$unite1[[2]],',',
                        input$unite1[[3]],',', input$unite1[[4]],',', input$unite1[[5]],',', input$unite1[[6]],',', input$unite1[[7]],')'))
      
    }
  })
  
  output$uniteOutput1 <- renderTable ({
    head(mtcars)
  })
  
  output$uniteOutput2 <- renderTable ({
    if (input$unite3 == T && length(input$unite1) != 0) {
      head(tidyr::unite(data = mtcars, col = 'New_Column', input$unite1))}
  })
  
  # arrange: default setting is low to high
  output$dwTable8 <- renderTable ({
    if (input$dwSTI2 == 'Low to High') {
      head(dplyr::arrange(mtcars, mtcars[ , input$dwSTI1]))
    }
    else if (input$dwSTI2 == 'High to Low') {
      head(dplyr::arrange(mtcars, desc(mtcars[ , input$dwSTI1])))
    }
    else {
      head(head(mtcars))
    }
  })
  
  output$code1 <- renderUI ({
    if (input$dwSTI2 == 'Low to High') {
      tags$code(paste('R Code: dplyr::arrange(mtcars, mtcars[ , ', input$dwSTI1, ']'))
    }
  })
  
  output$code2 <- renderUI ({
    if (input$dwSTI2 == 'High to Low') {
      tags$code(paste('Code: dplyr::arrange(mtcars, descmtcars[ , ', input$dwSTI1, '])'))
    }
  })
  
  
  
  #pivot_wider
  output$dwTable1 <- renderTable({
    cases
  })
  
  output$dwTable2 <- renderTable({
    if (input$dw1 == TRUE) {
      cases %>%
        tidyr::pivot_longer(cols = c("2011","2012","2013"), names_to = "year", values_to = "n")
    }
  })
  
  #pivot_longer
  output$dwTable5 <- renderTable({
    pollution
  })
  
  output$dwTable6 <- renderTable({
    if (input$dw3 == TRUE) {
      pollution %>%
        tidyr::pivot_wider(names_from = "size", values_from = "amount")
    }
  })
  
  
  
  ####Tidy Data ----
  ##### pivot_longer 1 ----
  
  RawData <- table4a
  
  
  output$original1 <- renderTable({
    RawData
  })
  
  
  # Specify Outputs pivot_longer1 
  output$userOut1 <- renderTable({
    if (input$userOp1 == '1999' & input$userOp2 == '1999'
        & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`1999`, `1999`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '1999'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`1999`, `1999`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`1999`, `1999`, key = "year", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`1999`, `1999`, key = "year", value = "year")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`1999`, `2000`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`1999`, `2000`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`1999`, `2000`, key = "year", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`1999`, `2000`, key = "year", value = "year")
    }

    #added
    else if (input$userOp1 == '1999' & input$userOp2 == 'country'
        & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`1999`, `country`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == 'country'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`1999`, `country`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == 'country'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`1999`, `country`, key = "year", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == 'country'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`1999`, `country`, key = "year", value = "year")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`1999`, `Afghanistan`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`1999`, `Afghanistan`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`1999`, `Afghanistan`, key = "year", value = "cases")
    }
    else if (input$userOp1 == '1999' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`1999`, `Afghanistan`, key = "year", value = "year")
    }


    ### 2000

    else if (input$userOp1 == '2000' & input$userOp2 == '1999'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`2000`, `1999`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '1999'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`2000`, `1999`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`2000`, `1999`, key = "year", value = "cases")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`2000`, `1999`, key = "year", value = "year")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`2000`, `2000`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`2000`, `2000`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`2000`, `2000`, key = "year", value = "cases")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`2000`, `2000`, key = "year", value = "year")
    }

    #added
    else if (input$userOp1 == '2000' & input$userOp2 == 'country'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`2000`, `country`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == 'country'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`2000`, `country`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == 'country'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`2000`, `country`, key = "year", value = "cases")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == 'country'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`2000`, `country`, key = "year", value = "year")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`2000`, `Afghanistan`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`2000`, `Afghanistan`, key = "cases", value = "year")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`2000`, `Afghanistan`, key = "year", value = "cases")
    }
    else if (input$userOp1 == '2000' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`2000`, `Afghanistan`, key = "year", value = "year")
    }

    ### coutry

    else if (input$userOp1 == 'country' & input$userOp2 == 'country'
        & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`country`, `country`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == 'country'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`country`, `country`, key = "cases", value = "year")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == 'country'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`country`, `country`, key = "year", value = "cases")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == 'country'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`country`, `country`, key = "year", value = "year")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == '1999'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`country`, `1999`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == '1999'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`country`, `1999`, key = "cases", value = "year")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`country`, `1999`, key = "year", value = "cases")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`country`, `1999`, key = "year", value = "year")
    }
    #added
    else if (input$userOp1 == 'country' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`country`, `2000`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`country`, `2000`, key = "cases", value = "year")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`country`, `2000`, key = "year", value = "cases")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`country`, `2000`, key = "year", value = "year")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`country`, `Afghanistan`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`country`, `Afghanistan`, key = "cases", value = "year")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`country`, `Afghanistan`, key = "year", value = "cases")
    }
    else if (input$userOp1 == 'country' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`country`, `Afghanistan`, key = "year", value = "year")
    }

    ### SECOND HALF

      else if (input$userOp1 == '1999' & input$userOp2 == '1999'
          & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`1999`, `1999`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == '1999'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`1999`, `1999`, key = "1999", value = "Afghanistan")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == '1999'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`1999`, `1999`, key = "Afghanistan", value = "1999")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == '1999'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`1999`, `1999`, key = "Afghanistan", value = "Afghanistan")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == '2000'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`1999`, `2000`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == '2000'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`1999`, `2000`, key = "cases", value = "year")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == '2000'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`1999`, `2000`, key = "year", value = "cases")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == '2000'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`1999`, `2000`, key = "Afghanistan", value = "Afghanistan")
      }

      #added
      else if (input$userOp1 == '1999' & input$userOp2 == 'country'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`1999`, `country`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == 'country'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`1999`, `country`, key = "1999", value = "Afghanistan")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == 'country'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`1999`, `country`, key = "Afghanistan", value = "1999")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == 'country'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`1999`, `country`, key = "Afghanistan", value = "Afghanistan")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`1999`, `Afghanistan`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`1999`, `Afghanistan`, key = "1999", value = "Afghanistan")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`1999`, `Afghanistan`, key = "Afghanistan", value = "1999")
      }
      else if (input$userOp1 == '1999' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`1999`, `Afghanistan`, key = "Afghanistan", value = "Afghanistan")
      }


      ### 2000

      else if (input$userOp1 == '2000' & input$userOp2 == '1999'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`2000`, `1999`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == '1999'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`2000`, `1999`, key = "1999", value = "Afghanistan")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == '1999'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`2000`, `1999`, key = "Afghanistan", value = "1999")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == '1999'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`2000`, `1999`, key = "Afghanistan", value = "Afghanistan")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == '2000'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`2000`, `2000`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == '2000'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`2000`, `2000`, key = "1999", value = "Afghanistan")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == '2000'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`2000`, `2000`, key = "Afghanistan", value = "1999")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == '2000'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`2000`, `2000`, key = "Afghanistan", value = "Afghanistan")
      }

      #added
      else if (input$userOp1 == '2000' & input$userOp2 == 'country'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`2000`, `country`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == 'country'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`2000`, `country`, key = "1999", value = "Afghanistan")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == 'country'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`2000`, `country`, key = "Afghanistan", value = "1999")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == 'country'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`2000`, `country`, key = "Afghanistan", value = "Afghanistan")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`2000`, `Afghanistan`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`2000`, `Afghanistan`, key = "1999", value = "Afghanistan")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`2000`, `Afghanistan`, key = "Afghanistan", value = "1999")
      }
      else if (input$userOp1 == '2000' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`2000`, `Afghanistan`, key = "Afghanistan", value = "Afghanistan")
      }

      ### country

      else if (input$userOp1 == 'country' & input$userOp2 == 'country'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`country`, `country`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == 'country'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`country`, `country`, key = "1999", value = "Afghanistan")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == 'country'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`country`, `country`, key = "Afghanistan", value = "1999")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == 'country'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`country`, `country`, key = "Afghanistan", value = "Afghanistan")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == '1999'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`country`, `1999`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == '1999'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`country`, `1999`, key = "1999", value = "Afghanistan")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == '1999'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`country`, `1999`, key = "Afghanistan", value = "1999")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == '1999'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`country`, `1999`, key = "Afghanistan", value = "Afghanistan")
      }
      #added
      else if (input$userOp1 == 'country' & input$userOp2 == '2000'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`country`, `2000`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == '2000'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`country`, `2000`, key = "1999", value = "Afghanistan")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == '2000'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`country`, `2000`, key = "Afghanistan", value = "1999")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == '2000'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`country`, `2000`, key = "Afghanistan", value = "Afghanistan")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == '1999' & input$userOp4 == '1999') {
        RawData %>%
          gather(`country`, `Afghanistan`, key = "1999", value = "1999")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`country`, `Afghanistan`, key = "1999", value = "Afghanistan")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
        RawData %>%
          gather(`country`, `Afghanistan`, key = "Afghanistan", value = "1999")
      }
      else if (input$userOp1 == 'country' & input$userOp2 == 'Afghanistan'
               & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
        RawData %>%
          gather(`country`, `Afghanistan`, key = "Afghanistan", value = "Afghanistan")
      }

    ### Afghanistan

    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "cases", value = "year")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "year", value = "cases")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "year", value = "year")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '1999'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`Afghanistan`, `1999`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '1999'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`Afghanistan`, `1999`, key = "cases", value = "year")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`Afghanistan`, `1999`, key = "year", value = "cases")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '1999'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`Afghanistan`, `1999`, key = "year", value = "year")
    }
    #added
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`Afghanistan`, `2000`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '2000'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`Afghanistan`, `2000`, key = "cases", value = "year")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`Afghanistan`, `2000`, key = "year", value = "cases")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '2000'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`Afghanistan`, `2000`, key = "year", value = "year")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'cases' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "cases", value = "cases")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'cases' & input$userOp4 == 'year') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "cases", value = "year")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "year", value = "cases")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'year' & input$userOp4 == 'year') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "year", value = "year")
    }

    ## first half
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == '1999' & input$userOp4 == '1999') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "1999", value = "1999")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "Afghanistan", value = "Afghanistan")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '1999'
             & input$userOp3 == '1999' & input$userOp4 == '1999') {
      RawData %>%
        gather(`Afghanistan`, `1999`, key = "1999", value = "1999")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '1999'
             & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
      RawData %>%
        gather(`Afghanistan`, `1999`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '1999'
             & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
      RawData %>%
        gather(`Afghanistan`, `1999`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '1999'
             & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
      RawData %>%
        gather(`Afghanistan`, `1999`, key = "Afghanistan", value = "Afghanistan")
    }
    #added
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '2000'
             & input$userOp3 == '1999' & input$userOp4 == '1999') {
      RawData %>%
        gather(`Afghanistan`, `2000`, key = "1999", value = "1999")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '2000'
             & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
      RawData %>%
        gather(`Afghanistan`, `2000`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '2000'
             & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
      RawData %>%
        gather(`Afghanistan`, `2000`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == '2000'
             & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
      RawData %>%
        gather(`Afghanistan`, `2000`, key = "Afghanistan", value = "Afghanistan")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == '1999' & input$userOp4 == '1999') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "1999", value = "1999")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'Afghanistan'
             & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
      RawData %>%
        gather(`Afghanistan`, `Afghanistan`, key = "Afghanistan", value = "Afghanistan")
    }

    # adding country
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'country'
             & input$userOp3 == '1999' & input$userOp4 == '1999') {
      RawData %>%
        gather(`Afghanistan`, `country`, key = "1999", value = "1999")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'country'
             & input$userOp3 == '1999' & input$userOp4 == 'Afghanistan') {
      RawData %>%
        gather(`Afghanistan`, `country`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'country'
             & input$userOp3 == 'Afghanistan' & input$userOp4 == '1999') {
      RawData %>%
        gather(`Afghanistan`, `country`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp1 == 'Afghanistan' & input$userOp2 == 'country'
             & input$userOp3 == 'Afghanistan' & input$userOp4 == 'Afghanistan') {
      RawData %>%
        gather(`Afghanistan`, `country`, key = "Afghanistan", value = "Afghanistan")
    }




  })

  #### Bottom of options ####

  # dynamic code based on user inputs
 output$userOut2 <- renderUI({
   tags$code('tidyr::gather(RawData,`', input$userOp1, '`,`', input$userOp2, '`,
             key = "', input$userOp3, '", value = "', input$userOp4, '")' )
 })


 #observeEvent(input$submit, {
  # withProgress(session, min = 1, max = 15, {
   #  setProgress(message = 'Checking Answer',
    #             detail = '')
     #for (i in 1:10) {
      # setProgress(value = i)
       #Sys.sleep(0.05)
  #   }
 #  })
# })

# op1save <- reactiveValues(input$userOp1 = NULL)
# op2save <- reactiveValues(input$userOp2 = NULL)
# op3save <- reactiveValues(input$userOp3 = NULL)
# op4save <- reactiveValues(input$userOp4 = NULL)
  
  
  
  #dynamic code based on user inputs 
  output$tidyAttemptTable <- renderUI({
    tags$code(paste0('tidyr::pivot_longer(RawData, cols = c("', input$userOp1, '","', input$userOp2, '") ,
             names_to = "', input$userOp3, '", values_to = "', input$userOp4, '")' ))
  })

  # submit button
   output$sub <- renderUI({
     bsButton("submitcc",
              label = "Check Answer",
              icon("lightbulb"),
              size = "medium",
              style = 'success')
     })


   # delay submit button
   observeEvent(input$submitcc,{
     withProgress(session, min = 1, max = 15, {
       setProgress(message = 'Checking Answer',
                   detail = '')
       for (i in 1:13) {
         setProgress(value = i)
         Sys.sleep(0.05)
       }
     })
   })


  # correct/wrong gif

# trying to use sweetalert
  #observeEvent(input$submit, {
  #  if (input$userOp1 == '1999' & input$userOp2 == '2000'
  #      & input$userOp3 == 'year' & input$userOp4 == 'cases') {
   #   sweetalert(imageUrl = 'correct.gif')
    #}

 #   else{
  #    sweetalert(imageUrl = 'try.gif')
   # }
  #})

    output$resetcc <- renderUI({
      bsButton("retry",
               label = "Try Again",
               icon("retweet"),
               size = "medium",
               style = 'success')
      })

    # delay retry button
    observeEvent(input$retry,{
      withProgress(session, min = 1, max = 15, {
        setProgress(message = 'Resetting',
                    detail = '')
        for (i in 1:13) {
          setProgress(value = i)
          Sys.sleep(0.05)
        }
      })
    })


    # hide reset button upon opening app
    hide("resetcc")
    hide("correct")
    hide("wrong")


    output$correct <- renderUI({
      tags$img(src = "correct.gif", width = 150)
    })


      output$wrong <- renderUI({
        tags$img(src = "try.gif", width = 150)
      })




    # show reset button after submit is clicked, disable dropdown inputs
    observeEvent(input$submitcc,{
      toggle("resetcc")
      disable("userOp1")
      disable("userOp2")
      disable("userOp3")
      disable("userOp4")
      disable("submitcc")
      if(input$userOp1 == '1999' & input$userOp2 == '2000'
         & input$userOp3 == 'year' & input$userOp4 == 'cases' || input$userOp1 == '2000' & input$userOp2 == '1999'
         & input$userOp3 == 'year' & input$userOp4 == 'cases') {
        showElement("correct")
      }
      else{
        showElement("wrong")
      }


    })
  })
  
  # Next Question ############
  output$nextQuestion1 <- renderUI({
    bsButton("next1",
             label = "Next Question",
             size = "medium",
             style = 'success')
  })
  observeEvent(input$next1, {
    updateTabsetPanel(session, "questionTabs",
                      selected = "pivot_longer2")
  })
  
  
  
  
  # hide reset button upon opening app
  hide("resetcc")
  hide("correct")
  hide("wrong")
  hide("nextQuestion1")
  
  
  output$correct <- renderUI({
    tags$img(src = "correct.png", width = 30)
  })
  
  output$wrong <- renderUI({
    tags$img(src = "incorrect.png", width = 30)
  })
  
  
  
  
  # show reset button after submit is clicked, disable dropdown inputs
  observeEvent(input$submitcc,{
    toggle("resetcc")
    toggle("nextQuestion1")
    disable("userOp1")
    disable("userOp2")
    disable("userOp3")
    disable("userOp4")
    disable("submitcc")
    if(input$userOp1 == '1999' & input$userOp2 == '2000'
       & input$userOp3 == 'year' & input$userOp4 == 'cases' || input$userOp1 == '2000' & input$userOp2 == '1999'
       & input$userOp3 == 'year' & input$userOp4 == 'cases') {
      showElement("correct")
    }
    else{
      showElement("wrong")
    }
    
    
  })
  observeEvent(input$retry,{
    hide("resetcc")
    enable("userOp1")
    enable("userOp2")
    enable("userOp3")
    enable("userOp4")
    showElement("submitcc")
    enable("submitcc")
    hide("correct")
    hide("wrong")

  })

  observeEvent(input$retry,{
    reset("userOp1")
    reset("userOp2")
    reset("userOp3")
    reset("userOp4")
    showElement("submitcc")
    enable("submitcc")

  })

  ###############Shiny Ace#################


  observeEvent(input$nextq, {
    # updateButton(session, "submit", disabled = FALSE)
    # updateButton(session, "nextq", disabled = TRUE)
    updateSelectInput(session, "answer", "Answer:", c("","A", "B", "C"))

  runButtonWasPressed <<- F #Used to stop error in the knitted output
  disable("submit")
  # question bank 
  somethingIsSelected <<- F
  value <- reactiveValues(index =  1, mistake = 0, correct = 0)
  ans <- as.matrix(bank[1:9, 6])
  index_list <- reactiveValues(list = sample(2:9, 8, replace = FALSE))
  
  observeEvent(input$nextq,{
    
    value$answerbox <- value$index
    #Removes the value in the front of the list
    index_list$list = index_list$list[-1]
    
    
    value$index <- index_list$list[1]
    value$answerbox <- value$index

    output$mark <- renderUI({
      img(src = NULL, width = 30)
    })
  })




  #### question bank ####
  value <- reactiveValues(index =  1, mistake = 0, correct = 0)
  ans <- as.matrix(bank[1:9, 6])
  #ans <- data.frame(ans)
  index_list <- reactiveValues(list = sample(2:9, 8, replace = FALSE))

  observeEvent(input$nextq,{
    value$answerbox <- value$index
    index_list$list = index_list$list[-1]
    value$index <- index_list$list[1]
    value$answerbox <- value$index

    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session, "submit", disabled = FALSE)
  })

  output$question <- renderUI({
    h4(bank[value$index, 2])
    # radioButtons(inputId = bank[value$index,1], label= bank[value$index, 2],
    #              choiceNames=c(bank[value$index, 3], bank[value$index, 4], bank[value$index, 5]),
    #              choiceValues = c("A", "B", "C"))
  })

  output$options <- renderUI({
    str1 <- paste("A.", bank[value$index, 3])
    str2 <- paste("B.", bank[value$index, 4])
    str3 <- paste("C.", bank[value$index, 5])
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })


 ## change table based on question
  output$acetable <- renderTable({
    if (bank[value$index, 2] %in% c(bank[2, 2], bank[3, 2])) {

      race

    } else {
      if (bank[value$index, 2] %in% c(bank[4, 2], bank[5, 2])) {

      results

        } else {
        if (bank[value$index, 2] %in% c(bank[6, 2], bank[7, 2])) {

          grades

          } else {
          if (bank[value$index, 2] %in% c(bank[8, 2], bank[9, 2])) {

          table5
          }
          }
        }
    }
    })

  output$tableinfo <- renderUI({
    # race data info
    if (bank[value$index, 2] == bank[2, 2] | bank[value$index, 2] == bank[3, 2]) {
      tags$h4('This table depicts times and scores on a running race.')
     # tags$h4('Column names define different lengths of time')
    #  tags$h4('Cell values are scores associated with each name and length of time')

      # results data info
      } else if (bank[value$index, 2] == bank[4, 2] | bank[value$index, 2] == bank[5, 2]) {
        tags$li('This table depicts clinical trial data')
       # tags$li('Ind - individual participating in the experiment')
      #  tags$li('Treatment - trial type (Treat or Cont)')
      #  tags$li('value - result of experiment')

        # grades data info
      } else if (bank[value$index, 2] == bank[6, 2] | bank[value$index, 2] == bank[7, 2]) {
        tags$li('This table depicts student test score data')
      #  tags$li('A tidy case is one individual during one quarter in a given year.')
      #  tags$li('Each test is unique and should be treated as two separate variables.')

        # table 5 data ifo
      } else if (bank[value$index, 2] == bank[8, 2] || bank[value$index, 2] == bank[9, 2]) {
        tags$li('This table shows the population and rate of different countries.')
      }


    })



  output$editor <- renderUI({
    aceEditor("rmd",
              mode = "markdown",
              if(bank[value$index, 2] == bank[1, 2]) {
              value = 'Here you can test out the answer choices before choosing an answer!

Uncomment one line from each section at a time and hit "Run" to see its effect!

Note: If the code does not display/change data, it probably is not the correct answer.

There is no interactive R code for this question!'
              } else if (bank[value$index, 2] == bank[2, 2]) {
                value = 'No interactive R code for this question!'
                }
              else if (bank[value$index, 2] == bank[3, 2]) {
                    value = '
```{r}
library(rcfss)

tidyRace <-
  race %>%
  # spread(key = Time, value = Score, -Name, convert = TRUE) %>%
  # gather(key = Time, value = Score, -Name, convert = TRUE) %>%
  # unite(key = Time, value = Score, -Name, convert = TRUE) %>%
  arrange(Name, Time)

tidyRace
```
'
                  }
              else if (bank[value$index, 2] == bank[4, 2]) {
                      value = 'No interactive R code for this question!'
              }
              else if (bank[value$index, 2] == bank[5, 2]) {
                value = '```{r}
library(rcfss)

tidyResults <-
  results %>%
  # spread(key = Treatment, value = value)
  # gather(key = Treatment, value = value)
  # unite(key = Treatment, value = value)

tidyResults
```
                '
              }
              else if (bank[value$index, 2] == bank[6, 2]) {
                value = 'No interactive R code for this question!'
              }
              else if (bank[value$index, 2] == bank[7, 2]) {
                value = '```{r}
library(stringr)
library(rcfss)

tidyGrades <-
  grades %>%
  gather(key = Quarter, value = Score, Fall:Winter) %>%
  mutate(Test = str_c("Test", Test)) %>%

  # spread(key = Test, value = Score) %>%
  # gather(key = Test, value = Score) %>%
  arrange(ID, Year, Quarter)

tidyGrades
```
'
              }
              else if (bank[value$index, 2] == bank[8, 2]) {
                value = '```{r}
library(tidyr)

tidyTable5 <-
  table5 %>%
  # spread(key = century, value = year)
  # gather(key = century, value = year)
  # unite(col = new, century, year, sep = "")

tidyTable5
```
'
              }
              else {
                value = '```{r}
library(tidyr)

nextStep <-
  table5 %>%
  # unite(col = new, century, year)
  # unite(col = new, century, year, sep = "")
  # unite(col = year, century, year)

nextStep
```
'
              }
              )
  })



  # observeEvent(input$answer, {
  #   req(input$answer, input$answer !='')
  #   answer <- isolate(input$answer)
  #   interacted_statement <- rlocker::createStatement(
  #     list(
  #       verb = list(
  #         display = "selected"),
  #       object = list(
  #         id = paste0(getCurrentAddress(session), "#", value$index),
  #         name = paste('Question', value$index),
  #         description = bank[value$index, 2]),
  #       result = list(
  #         success = any(answer == ans[value$index, 1]),
  #         response = paste(getResponseText(value$index, answer),
  #                          as.character(Sys.time()))
  #       )
  #     )
  #   )
  #
  #
  #   # Store statement in locker and return status
  #   status <- rlocker::store(session, interacted_statement)
  #
  #   print(interacted_statement) # remove me
  #   print(status) # remove me

  
  observeEvent(input$answer, {
    somethingIsSelected <<- T
    enable("submit")
  })
  
  # Once rLocker works properly this may be relevant. 
  # observeEvent(input$answer, {
  #   req(input$answer, input$answer !='')
  #   answer <- isolate(input$answer)
  #   # interacted_statement <- rLocker::createStatement(
  #   #   list(
  #   #     verb = list(
  #   #       display = "selected"),
  #   #     object = list(
  #   #       id = paste0(getCurrentAddress(session), "#", value$index),
  #   #       name = paste('Question', value$index),
  #   #       description = bank[value$index, 2]),
  #   #     result = list(
  #   #       success = any(answer == ans[value$index, 1]),
  #   #       response = paste(getResponseText(value$index, answer), 
  #   #                        as.character(Sys.time()))
  #   #     )
  #   #   )
  #   # )
  #   
  # 
  #   # Store statement in locker and return status
  #   #status <- rLocker::store(session, interacted_statement)
  #   
  #   #print(interacted_statement) # remove me
  #   #print(status) # remove me

  # })


  getResponseText <- function(index, answer){
    if(answer == 'A'){
      key = 3
    } else if(answer == 'B'){
      key = 4
    } else {
      key = 5
    }
    return(bank[index, key])
  }

  observeEvent(input$submit,{
    if(length(index_list$list) == 1){
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session,"submit", disabled = TRUE)
      updateButton(session, "reset", disabled = FALSE)
    }
    else{
      updateButton(session, "nextq", disabled = FALSE)
      updateButton(session,"submit", disabled = TRUE)
      updateButton(session, "reset", disabled = FALSE)
    }


    # output$progress<-renderUI({
    #   paste("You are currently on problem", 11-length(index_list$list), "/10")
    # })

    answer <- isolate(input$answer)

    # statement <- rlocker::createStatement(
    #   list(
    #     verb = list(
    #       display = "answered"
    #     ),
    #     object = list(id = paste0(getCurrentAddress(session), "#", value$index),
    #                   name = paste('Question', value$index),
    #                   description = bank[value$index, 2]),
    #     result = list(success = any(answer == ans[value$index, 1]),
    #                   response = paste(getResponseText(value$index, answer),
    #                                    as.character(Sys.time()))
    #                   )
    #     )
    #   )
    #
    # # Store statement in locker and return status
    # status <- rlocker::store(session, statement)
    #
    # print(statement) # remove me
    # print(status) # remove me


    
    
    answer <- input$answer
    
    statement <- rLocker::createStatement(
      list(
        verb = list(
          display = "answered"
        ),
        object = list(id = paste0(getCurrentAddress(session), "#", value$index),
                      name = paste('Question', value$index),
                      description = bank[value$index, 2]),
        result = list(success = any(answer == ans[value$index, 1]),
                      response = paste(getResponseText(value$index, answer), 
                                       as.character(Sys.time()))
        )
      )
    )
    
    # Store statement in locker and return status
    status <- rLocker::store(session, statement)
    
    

    output$mark <- renderUI({
      if (any(answer == ans[value$index, 1])){
        img(src = "correct.png", width = 30)
      }
      else{
        ig <- img(src = "incorrect.png", width = 30)
        w <- paste("You picked", answer, ", The correct answer is", ans[value$index, 1])
        HTML(paste(ig, w), sep = ' ')
      }
    })
  })

  observeEvent(input$reset, {
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session,"reset", disable = TRUE)
    updateSelectInput(session, "answer", "Answer:", c("","A", "B", "C"))
    index_list$list <- c(index_list$list, sample(2:9, 8, replace = FALSE))
    value$index <- 1
    value$answerbox = value$index
    ans <- as.matrix(bank[1:9, 6])
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
  })

  # Initialize Learning Locker connection

  # connection <- rlocker::connect(session, list(
  #   base_url = "https://learning-locker.stat.vmhost.psu.edu/",
  #   auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",
  #   agent = rlocker::createAgent()
  # ))
  #
  # # Setup demo app and user.
  # currentUser <-
  #   connection$agent
  #
  # if(connection$status != 200){
  #   warning(paste(connection$status, "\nTry checking your auth token."))
  # }



  connection <- rLocker::connect(session, list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",
    agent = rLocker::createAgent()
  ))
  

  # Setup demo app and user.

  output$Previewcar<-
    renderTable({
      head(cars, 4)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')

  output$Previewtree<-
    renderTable({
      head(trees, 4)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')

  output$Previewiris<-
    renderTable({
      head(iris, 4)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')
  ###########KNITR############
  
  ##### KNITR ----
  observeEvent(input$eval,{ 
    runButtonWasPressed <<- T

    withBusyIndicatorServer("eval", {
      output$knitDoc <- renderUI({
        return(isolate(HTML(knit2html(text = input$rmd, fragment.only = TRUE, quiet = FALSE))))
      })

      output$output <- renderPrint({
        return(isolate(eval(parse(text = input$code))))
      })
    })
  })

  output$knitDoc <- renderUI({
    input$eval
    return(isolate(HTML(knit2html(text = input$rmd, fragment.only = TRUE, quiet = FALSE))))
  })

  output$output <- renderPrint({
    input$eval
    return(isolate(eval(parse(text = input$code))))
  })



  ############ Combining Data Table ###################################################
  disable("check1")
  disable("check2")
  disable("check3")
  disable("check4")
  disable("check5")
  disable("check6")

  observeEvent(input$check1, {
    if (input$cd1 == 'left join') {
      sendSweetAlert(session, title = NULL, text = 'Correct!', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check2, {
    if (input$cd2 == 'inner join') {
      sendSweetAlert(session, title = NULL, text = 'Correct!', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check3, {
    if (input$cd3 == 'full join') {
      sendSweetAlert(session, title = NULL, text = 'Correct!', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check4, {
    if (input$cd4 == 'right join') {
      sendSweetAlert(session, title = NULL, text = 'Correct!', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check5, {
    if (input$cd5 == 'anti join') {
      sendSweetAlert(session, title = NULL, text = 'Correct!', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check6, {
    if (input$cd6 == 'semi join') {
      sendSweetAlert(session, title = NULL, text = 'Correct!', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })

  #correct answer: A, C, D, B --- left/inner/full/right
  output$cdTable1 <- renderTable({
    dplyr::left_join(a, b, by = "x1")
  })

  output$cdTable2 <- renderTable({
    dplyr::inner_join(a, b, by = "x1")
  })

  output$cdTable3 <- renderTable({
    dplyr::full_join(a, b, by = "x1")
  })

  output$cdTable4 <- renderTable({
    dplyr::right_join(a, b, by = "x1")
  })

  output$cdTable5 <- renderTable({
    dplyr::anti_join(a, b, by = "x1")
  })

  output$cdTable6 <- renderTable({
    dplyr::semi_join(a, b, by = "x1")
  })

  #cd Exp 1-6
  output$cdExp1 <- renderText ({
    if (input$cd1 == 'left join') {
      paste('Join matching rows from b to a.')
    }
    else if (input$cd1 == 'right join') {
      paste('Join matching rows from a to b.')
    }
    else if (input$cd1 == 'inner join') {
      paste('Join data. Retain only rows in both sets.')
    }
    else {
      paste('Join data. Retain all values, all rows.')
    }
  })

  output$cdExp2 <- renderText ({
    if (input$cd2 == 'left join') {
      paste('Join matching rows from b to a.')
    }
    else if (input$cd2 == 'right join') {
      paste('Join matching rows from a to b.')
    }
    else if (input$cd2 == 'inner join') {
      paste('Join data. Retain only rows in both sets.')
    }
    else {
      paste('Join data. Retain all values, all rows.')
    }
  })

  output$cdExp3 <- renderText ({
    if (input$cd3 == 'left join') {
      paste('Join matching rows from b to a.')
    }
    else if (input$cd3 == 'right join') {
      paste('Join matching rows from a to b.')
    }
    else if (input$cd3 == 'inner join') {
      paste('Join data. Retain only rows in both sets.')
    }
    else {
      paste('Join data. Retain all values, all rows.')
    }
  })

  output$cdExp4 <- renderText ({
    if (input$cd4 == 'left join') {
      paste('Join matching rows from b to a.')
    }
    else if (input$cd4 == 'right join') {
      paste('Join matching rows from a to b.')
    }
    else if (input$cd4 == 'inner join') {
      paste('Join data. Retain only rows in both sets.')
    }
    else {
      paste('Join data. Retain all values, all rows.')
    }
  })

})
