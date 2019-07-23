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
library(rlocker)
library(datasets)
library(rmarkdown)
library(learnr)



bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

source("helpers.R")

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
  
    
  
  
 ############## Tidy Data #################
  
  RawData <- table4a
 
   
 output$original1 <- renderTable({
   RawData
 })
  
  # specify outputs for every choice
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
 
  # submit button
   output$sub <- renderUI({
     bsButton("submit",
              label = "Check Answer",
              icon("lightbulb"),
              size = "medium",
              style = 'success')
     })
   
   observeEvent(input$submit,{
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
   
    output$reset <- renderUI({
      bsButton("retry",
               label = "Try Again",
               icon("retweet"),
               size = "medium",
               style = 'success')
      })
    
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
    hide("reset")
    hide("correct")
    hide("wrong")
    
    
    output$correct <- renderUI({
      tags$img(src = "correct.gif", width = 200)
    })
      
      output$wrong <- renderUI({
        tags$img(src = "try.gif", width = 200)
      })
        


    
    # show reset button after submit is clicked, disable dropdown inputs
    observeEvent(input$submit,{
      toggle("reset")
      disable("userOp1")
      disable("userOp2")
      disable("userOp3")
      disable("userOp4")
      disable("submit")
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
    hide("reset")
    enable("userOp1")
    enable("userOp2")
    enable("userOp3")
    enable("userOp4")
    showElement("submit")
    enable("submit")
    hide("correct")
    hide("wrong")

  })
  
  observeEvent(input$retry,{
    reset("userOp1")
    reset("userOp2")
    reset("userOp3")
    reset("userOp4")
    showElement("submit")
    enable("submit")
    
  })
  
  
  #### Spread 1 ####
  
   RawData3 <- table4b
  
  output$original2 <- renderTable({
    RawData3
  })
  
  # specify outputs for every choice
  output$userOutA <- renderTable({
    if (input$userOpA == '1999' & input$userOpB == '1999' 
        & input$userOpC == 'population' & input$userOpD == 'population') {
      RawData3 %>%
        gather(`1999`, `1999`, key = "population", value = "population")
    }
    else if (input$userOpA == '1999' & input$userOpB == '1999'
             & input$userOpC == 'population' & input$userOpD == 'year') {
      RawData3 %>%
        gather(`1999`, `1999`, key = "population", value = "year")
    }
    else if (input$userOpA == '1999' & input$userOpB == '1999'
             & input$userOpC == 'year' & input$userOpD == 'population') {
      RawData3 %>%
        gather(`1999`, `1999`, key = "year", value = "population")
    }
    else if (input$userOpA == '1999' & input$userOpB == '1999'
             & input$userOpC == 'year' & input$userOpD == 'year') {
      RawData3 %>%
        gather(`1999`, `1999`, key = "year", value = "year")
    }
    else if (input$userOpA == '1999' & input$userOpB == '2000'
             & input$userOpC == 'population' & input$userOpD == 'population') {
      RawData3 %>%
        gather(`1999`, `2000`, key = "population", value = "population")
    }
    else if (input$userOpA == '1999' & input$userOpB == '2000'
             & input$userOpC == 'population' & input$userOpD == 'year') {
      RawData3 %>%
        gather(`1999`, `2000`, key = "population", value = "year")
    }
    else if (input$userOpA == '1999' & input$userOpB == '2000'
             & input$userOpC == 'year' & input$userOpD == 'population') {
      RawData3 %>%
        gather(`1999`, `2000`, key = "year", value = "population")
    }
    else if (input$userOpA == '1999' & input$userOpB == '2000'
             & input$userOpC == 'year' & input$userOpD == 'year') {
      RawData3 %>%
        gather(`1999`, `2000`, key = "year", value = "year")
    }
    else if (input$userOpA == '2000' & input$userOpB == '1999'
             & input$userOpC == 'population' & input$userOpD == 'population') {
      RawData3 %>%
        gather(`2000`, `1999`, key = "population", value = "population")
    }
    else if (input$userOpA == '2000' & input$userOpB == '1999'
             & input$userOpC == 'population' & input$userOpD == 'year') {
      RawData3 %>%
        gather(`2000`, `1999`, key = "population", value = "year")
    }
    else if (input$userOpA == '2000' & input$userOpB == '1999'
             & input$userOpC == 'year' & input$userOpD == 'population') {
      RawData3 %>%
        gather(`2000`, `1999`, key = "year", value = "population")
    }
    else if (input$userOpA == '2000' & input$userOpB == '1999'
             & input$userOpC == 'year' & input$userOpD == 'year') {
      RawData3 %>%
        gather(`2000`, `1999`, key = "year", value = "year")
    }
    else if (input$userOpA == '2000' & input$userOpB == '2000'
             & input$userOpC == 'population' & input$userOpD == 'population') {
      RawData3 %>%
        gather(`2000`, `2000`, key = "population", value = "population")
    }
    else if (input$userOpA == '2000' & input$userOpB == '2000'
             & input$userOpC == 'population' & input$userOpD == 'year') {
      RawData3 %>%
        gather(`2000`, `2000`, key = "population", value = "year")
    }
    else if (input$userOpA == '2000' & input$userOpB == '2000'
             & input$userOpC == 'year' & input$userOpD == 'population') {
      RawData3 %>%
        gather(`2000`, `2000`, key = "year", value = "population")
    }
    else {
      RawData3 %>%
        gather(`2000`, `2000`, key = "year", value = "year")
    }
  })
  
  #### Bottom of options ####
  
  # show code based on inputs
  output$userOutB <- renderUI({
    tags$code('tidyr::gather(RawData3,`', input$userOpA, '`,`', input$userOpB, '`,
              key = "', input$userOpC, '", value = "', input$userOpD, '")' )
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
  
  # op1save <- reactiveValues(input$userOpA = NULL)
  # op2save <- reactiveValues(input$userOpB = NULL)
  # op3save <- reactiveValues(input$userOpC = NULL)
  # op4save <- reactiveValues(input$userOpD = NULL)
  
  # submit button
  output$bus <- renderUI({
    bsButton("submitted",
             label = "Check Answer",
             icon("lightbulb"),
             size = "medium",
             style = 'success')
  })
  
  # trying to use sweetalert
  #observeEvent(input$submit, {
  #  if (input$userOpA == '1999' & input$userOpB == '2000'
  #      & input$userOpC == 'year' & input$userOpD == 'population') {
  #   sweetalert(imageUrl = 'correct.gif')
  #}
  
  #   else{
  #    sweetalert(imageUrl = 'try.gif')
  # }
  #})
  
  
  
  output$redo <- renderUI({
    bsButton("retrying",
             label = "Try Again",
             icon("retweet"),
             size = "medium",
             style = 'success')
  })
  
  # hide reset button upon opening app
  hide("redo")
  hide("cor")
  hide("wro")
  
  
  output$cor <- renderUI({
    tags$img(src = "correct.gif", width = 200)
  })
  
  output$wro <- renderUI({
    tags$img(src = "try.gif", width = 200)
  })
  
  
  # show reset button after submit is clicked, disable dropdown inputs
  observeEvent(input$submitted,{
    toggle("redo")
    disable("userOpA")
    disable("userOpB")
    disable("userOpC")
    disable("userOpD")
    disable("submitted")
    if(input$userOpA == '1999' & input$userOpB == '2000'
       & input$userOpC == 'year' & input$userOpD == 'population') {
      showElement("cor")
    }
    else{
      showElement("wro")
    }
    
  })
  
  
  observeEvent(input$retrying,{
    hide("redo")
    enable("userOpA")
    enable("userOpB")
    enable("userOpC")
    enable("userOpD")
    showElement("submitted")
    enable("submitted")
    hide("cor")
    hide("wro")
    
  })
  
  observeEvent(input$retrying,{
    reset("userOpA")
    reset("userOpB")
    reset("userOpC")
    reset("userOpD")
    showElement("submitted")
    enable("submitted")
    
  })
  
  
  #### Spread 2 ####
  
  capital <- c("Kabul", "Brasília", "Beijing")
  
  table4b$capital <- capital
  
  RawData4 <- table4b
  
  output$original4 <- renderTable({
    RawData4
  })
  
  # specify outputs for every choice
  output$userOut3 <- renderTable({
    if (input$userOpJ == '1999' & input$userOpK == '1999' 
        & input$userOpL == 'population' & input$userOpM == 'population') {
      RawData4 %>%
        gather(`1999`, `1999`, key = "population", value = "population")
    }
    else if (input$userOpJ == '1999' & input$userOpK == '1999'
             & input$userOpL == 'population' & input$userOpM == 'year') {
      RawData4 %>%
        gather(`1999`, `1999`, key = "population", value = "year")
    }
    else if (input$userOpJ == '1999' & input$userOpK == '1999'
             & input$userOpL == 'year' & input$userOpM == 'population') {
      RawData4 %>%
        gather(`1999`, `1999`, key = "year", value = "population")
    }
    else if (input$userOpJ == '1999' & input$userOpK == '1999'
             & input$userOpL == 'year' & input$userOpM == 'year') {
      RawData4 %>%
        gather(`1999`, `1999`, key = "year", value = "year")
    }
    else if (input$userOpJ == '1999' & input$userOpK == '2000'
             & input$userOpL == 'population' & input$userOpM == 'population') {
      RawData4 %>%
        gather(`1999`, `2000`, key = "population", value = "population")
    }
    else if (input$userOpJ == '1999' & input$userOpK == '2000'
             & input$userOpL == 'population' & input$userOpM == 'year') {
      RawData4 %>%
        gather(`1999`, `2000`, key = "population", value = "year")
    }
    else if (input$userOpJ == '1999' & input$userOpK == '2000'
             & input$userOpL == 'year' & input$userOpM == 'population') {
      RawData4 %>%
        gather(`1999`, `2000`, key = "year", value = "population")
    }
    else if (input$userOpJ == '1999' & input$userOpK == '2000'
             & input$userOpL == 'year' & input$userOpM == 'year') {
      RawData4 %>%
        gather(`1999`, `2000`, key = "year", value = "year")
    }
    else if (input$userOpJ == '2000' & input$userOpK == '1999'
             & input$userOpL == 'population' & input$userOpM == 'population') {
      RawData4 %>%
        gather(`2000`, `1999`, key = "population", value = "population")
    }
    else if (input$userOpJ == '2000' & input$userOpK == '1999'
             & input$userOpL == 'population' & input$userOpM == 'year') {
      RawData4 %>%
        gather(`2000`, `1999`, key = "population", value = "year")
    }
    else if (input$userOpJ == '2000' & input$userOpK == '1999'
             & input$userOpL == 'year' & input$userOpM == 'population') {
      RawData4 %>%
        gather(`2000`, `1999`, key = "year", value = "population")
    }
    else if (input$userOpJ == '2000' & input$userOpK == '1999'
             & input$userOpL == 'year' & input$userOpM == 'year') {
      RawData4 %>%
        gather(`2000`, `1999`, key = "year", value = "year")
    }
    else if (input$userOpJ == '2000' & input$userOpK == '2000'
             & input$userOpL == 'population' & input$userOpM == 'population') {
      RawData4 %>%
        gather(`2000`, `2000`, key = "population", value = "population")
    }
    else if (input$userOpJ == '2000' & input$userOpK == '2000'
             & input$userOpL == 'population' & input$userOpM == 'year') {
      RawData4 %>%
        gather(`2000`, `2000`, key = "population", value = "year")
    }
    else if (input$userOpJ == '2000' & input$userOpK == '2000'
             & input$userOpL == 'year' & input$userOpM == 'population') {
      RawData4 %>%
        gather(`2000`, `2000`, key = "year", value = "population")
    }
    else {
      RawData4 %>%
        gather(`2000`, `2000`, key = "year", value = "year")
    }
  })
  
  #### Bottom of options ####
  
  # show code based on inputs
  output$userOut4 <- renderUI({
    tags$code('tidyr::gather(RawData4,`', input$userOpJ, '`,`', input$userOpK, '`,
              key = "', input$userOpL, '", value = "', input$userOpM, '")' )
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
  
  # op1save <- reactiveValues(input$userOpJ = NULL)
  # op2save <- reactiveValues(input$userOpK = NULL)
  # op3save <- reactiveValues(input$userOpL = NULL)
  # op4save <- reactiveValues(input$userOpM = NULL)
  
  # submit button
  output$buss <- renderUI({
    bsButton("submitteds",
             label = "Check Answer",
             icon("lightbulb"),
             size = "medium",
             style = 'success')
  })
  
  # trying to use sweetalert
  #observeEvent(input$submit, {
  #  if (input$userOpJ == '1999' & input$userOpK == '2000'
  #      & input$userOpL == 'year' & input$userOpM == 'population') {
  #   sweetalert(imageUrl = 'correct.gif')
  #}
  
  #   else{
  #    sweetalert(imageUrl = 'try.gif')
  # }
  #})
  
  
  
  output$redos <- renderUI({
    bsButton("retryings",
             label = "Try Again",
             icon("retweet"),
             size = "medium",
             style = 'success')
  })
  
  # hide reset button upon opening app
  hide("redos")
  hide("cors")
  hide("wros")
  
  
  output$cors <- renderUI({
    tags$img(src = "correct.gif", width = 200)
  })
  
  output$wros <- renderUI({
    tags$img(src = "try.gif", width = 200)
  })
  
  
  # show reset button after submit is clicked, disable dropdown inputs
  observeEvent(input$submitteds,{
    toggle("redos")
    disable("userOpJ")
    disable("userOpK")
    disable("userOpL")
    disable("userOpM")
    disable("submitteds")
    if(input$userOpJ == '1999' & input$userOpK == '2000'
       & input$userOpL == 'year' & input$userOpM == 'population') {
      showElement("cors")
    }
    else{
      showElement("wros")
    }
    
  })
  
  
  observeEvent(input$retryings,{
    hide("redos")
    enable("userOpJ")
    enable("userOpK")
    enable("userOpL")
    enable("userOpM")
    showElement("submitteds")
    enable("submitteds")
    hide("cors")
    hide("wros")
    
  })
  
  observeEvent(input$retryings,{
    reset("userOpJ")
    reset("userOpK")
    reset("userOpL")
    reset("userOpM")
    showElement("submitteds")
    enable("submitteds")
    
  })
  
  

#### Gather 2 ####
  
  capital <- c("Kabul", "Brasília", "Beijing")
  
  table4a$capital <- capital
  
  RawData2 <- table4a
  
  output$original3 <- renderTable({
    RawData2
  })
  
  # specify outputs for every choice
  output$userOutX <- renderTable({
    if (input$userOp5 == '1999' & input$userOp6 == '1999' 
        & input$userOp3 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`1999`, `1999`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '1999'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`1999`, `1999`, key = "cases", value = "year")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '1999'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`1999`, `1999`, key = "year", value = "cases")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '1999'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`1999`, `1999`, key = "year", value = "year")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '2000'
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`1999`, `2000`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '2000'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`1999`, `2000`, key = "cases", value = "year")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '2000'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`1999`, `2000`, key = "year", value = "cases")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '2000'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`1999`, `2000`, key = "year", value = "year")
    }
    
    #added
    else if (input$userOp5 == '1999' & input$userOp6 == 'country' 
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`1999`, `country`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'country'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`1999`, `country`, key = "cases", value = "year")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'country'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`1999`, `country`, key = "year", value = "cases")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'country'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`1999`, `country`, key = "year", value = "year")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`1999`, `Afghanistan`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`1999`, `Afghanistan`, key = "cases", value = "year")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`1999`, `Afghanistan`, key = "year", value = "cases")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`1999`, `Afghanistan`, key = "year", value = "year")
    }
    
    
    ### 2000
    
    else if (input$userOp5 == '2000' & input$userOp6 == '1999'
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`2000`, `1999`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '1999'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`2000`, `1999`, key = "cases", value = "year")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '1999'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`2000`, `1999`, key = "year", value = "cases")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '1999'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`2000`, `1999`, key = "year", value = "year")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '2000'
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`2000`, `2000`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '2000'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`2000`, `2000`, key = "cases", value = "year")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '2000'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`2000`, `2000`, key = "year", value = "cases")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '2000'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`2000`, `2000`, key = "year", value = "year")
    }
    
    #added
    else if (input$userOp5 == '2000' & input$userOp6 == 'country' 
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`2000`, `country`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'country'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`2000`, `country`, key = "cases", value = "year")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'country'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`2000`, `country`, key = "year", value = "cases")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'country'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`2000`, `country`, key = "year", value = "year")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`2000`, `Afghanistan`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`2000`, `Afghanistan`, key = "cases", value = "year")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`2000`, `Afghanistan`, key = "year", value = "cases")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`2000`, `Afghanistan`, key = "year", value = "year")
    }
    
    ### coutry
    
    else if (input$userOp5 == 'country' & input$userOp6 == 'country' 
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`country`, `country`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'country'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`country`, `country`, key = "cases", value = "year")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'country'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`country`, `country`, key = "year", value = "cases")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'country'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`country`, `country`, key = "year", value = "year")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '1999'
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`country`, `1999`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '1999'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`country`, `1999`, key = "cases", value = "year")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '1999'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`country`, `1999`, key = "year", value = "cases")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '1999'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`country`, `1999`, key = "year", value = "year")
    }
    #added
    else if (input$userOp5 == 'country' & input$userOp6 == '2000' 
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`country`, `2000`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '2000'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`country`, `2000`, key = "cases", value = "year")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '2000'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`country`, `2000`, key = "year", value = "cases")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '2000'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`country`, `2000`, key = "year", value = "year")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`country`, `Afghanistan`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`country`, `Afghanistan`, key = "cases", value = "year")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`country`, `Afghanistan`, key = "year", value = "cases")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`country`, `Afghanistan`, key = "year", value = "year")
    }
    
    ### SECOND HALF
    
    else if (input$userOp5 == '1999' & input$userOp6 == '1999' 
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`1999`, `1999`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '1999'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`1999`, `1999`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '1999'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`1999`, `1999`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '1999'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`1999`, `1999`, key = "Afghanistan", value = "Afghanistan")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '2000'
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`1999`, `2000`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '2000'
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`1999`, `2000`, key = "cases", value = "year")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '2000'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`1999`, `2000`, key = "year", value = "cases")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == '2000'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`1999`, `2000`, key = "Afghanistan", value = "Afghanistan")
    }
    
    #added
    else if (input$userOp5 == '1999' & input$userOp6 == 'country' 
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`1999`, `country`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'country'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`1999`, `country`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'country'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`1999`, `country`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'country'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`1999`, `country`, key = "Afghanistan", value = "Afghanistan")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`1999`, `Afghanistan`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`1999`, `Afghanistan`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`1999`, `Afghanistan`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == '1999' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`1999`, `Afghanistan`, key = "Afghanistan", value = "Afghanistan")
    }
    
    
    ### 2000
    
    else if (input$userOp5 == '2000' & input$userOp6 == '1999'
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`2000`, `1999`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '1999'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`2000`, `1999`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '1999'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`2000`, `1999`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '1999'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`2000`, `1999`, key = "Afghanistan", value = "Afghanistan")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '2000'
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`2000`, `2000`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '2000'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`2000`, `2000`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '2000'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`2000`, `2000`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == '2000'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`2000`, `2000`, key = "Afghanistan", value = "Afghanistan")
    }
    
    #added
    else if (input$userOp5 == '2000' & input$userOp6 == 'country' 
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`2000`, `country`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'country'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`2000`, `country`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'country'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`2000`, `country`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'country'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`2000`, `country`, key = "Afghanistan", value = "Afghanistan")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`2000`, `Afghanistan`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`2000`, `Afghanistan`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`2000`, `Afghanistan`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == '2000' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`2000`, `Afghanistan`, key = "Afghanistan", value = "Afghanistan")
    }
    
    ### country
    
    else if (input$userOp5 == 'country' & input$userOp6 == 'country' 
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`country`, `country`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'country'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`country`, `country`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'country'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`country`, `country`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'country'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`country`, `country`, key = "Afghanistan", value = "Afghanistan")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '1999'
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`country`, `1999`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '1999'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`country`, `1999`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '1999'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`country`, `1999`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '1999'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`country`, `1999`, key = "Afghanistan", value = "Afghanistan")
    }
    #added
    else if (input$userOp5 == 'country' & input$userOp6 == '2000' 
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`country`, `2000`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '2000'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`country`, `2000`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '2000'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`country`, `2000`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == '2000'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`country`, `2000`, key = "Afghanistan", value = "Afghanistan")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`country`, `Afghanistan`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`country`, `Afghanistan`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`country`, `Afghanistan`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == 'country' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`country`, `Afghanistan`, key = "Afghanistan", value = "Afghanistan")
    }
    
    ### Afghanistan
    
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan' 
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "cases", value = "year")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "year", value = "cases")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "year", value = "year")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '1999'
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`Afghanistan`, `1999`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '1999'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`Afghanistan`, `1999`, key = "cases", value = "year")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '1999'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`Afghanistan`, `1999`, key = "year", value = "cases")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '1999'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`Afghanistan`, `1999`, key = "year", value = "year")
    }
    #added
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '2000' 
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`Afghanistan`, `2000`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '2000'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`Afghanistan`, `2000`, key = "cases", value = "year")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '2000'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`Afghanistan`, `2000`, key = "year", value = "cases")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '2000'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`Afghanistan`, `2000`, key = "year", value = "year")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'cases' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "cases", value = "cases")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'cases' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "cases", value = "year")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "year", value = "cases")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'year' & input$userOp8 == 'year') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "year", value = "year")
    }
    
    ## first half
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan' 
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "Afghanistan", value = "Afghanistan")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '1999'
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`Afghanistan`, `1999`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '1999'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`Afghanistan`, `1999`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '1999'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`Afghanistan`, `1999`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '1999'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`Afghanistan`, `1999`, key = "Afghanistan", value = "Afghanistan")
    }
    #added
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '2000' 
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`Afghanistan`, `2000`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '2000'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`Afghanistan`, `2000`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '2000'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`Afghanistan`, `2000`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == '2000'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`Afghanistan`, `2000`, key = "Afghanistan", value = "Afghanistan")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'Afghanistan'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`Afghanistan`, `Afghanistan`, key = "Afghanistan", value = "Afghanistan")
    }
    
    # adding country
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'country' 
             & input$userOp7 == '1999' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`Afghanistan`, `country`, key = "1999", value = "1999")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'country'
             & input$userOp7 == '1999' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`Afghanistan`, `country`, key = "1999", value = "Afghanistan")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'country'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == '1999') {
      RawData2 %>%
        gather(`Afghanistan`, `country`, key = "Afghanistan", value = "1999")
    }
    else if (input$userOp5 == 'Afghanistan' & input$userOp6 == 'country'
             & input$userOp7 == 'Afghanistan' & input$userOp8 == 'Afghanistan') {
      RawData2 %>%
        gather(`Afghanistan`, `country`, key = "Afghanistan", value = "Afghanistan")
    }
    })
  
  #### Bottom of Options ####  
  #(convenience purpouses)
  
  # show code based on inputs
  output$userOutY <- renderUI({
    tags$code('tidyr::gather(RawData2,`', input$userOp5, '`,`', input$userOp6, '`,
             key = "', input$userOp7, '", value = "', input$userOp8, '")' )
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
  
  # op1save <- reactiveValues(input$userOpX = NULL)
  # op2save <- reactiveValues(input$userOpY = NULL)
  # op3save <- reactiveValues(input$userOp3 = NULL)
  # op4save <- reactiveValues(input$userOp4 = NULL)
  
  # submit button
  output$subbed <- renderUI({
    bsButton("submitting",
             label = "Check Answer",
             icon("lightbulb"),
             size = "medium",
             style = 'success')
  })
  

  # trying to use sweetalert
  #observeEvent(input$submit, {
  #  if (input$userOpX == '1999' & input$userOpY == '2000'
  #      & input$userOp3 == 'year' & input$userOp4 == 'type') {
  #   sweetalert(imageUrl = 'correct.gif')
  #}
  
  #   else{
  #    sweetalert(imageUrl = 'try.gif')
  # }
  #})
  
  
  
  output$restart <- renderUI({
    bsButton("retryy",
             label = "Try Again",
             icon("retweet"),
             size = "medium",
             style = 'success')
  })
  
  # hide reset button upon opening app
  hide("restart")
  hide("cort")
  hide("rong")
  
  
  output$cort <- renderUI({
    tags$img(src = "correct.gif", width = 200)
  })
  
  output$rong <- renderUI({
    tags$img(src = "try.gif", width = 200)
  })
  
  
  # show reset button after submit is clicked, disable dropdown inputs
  observeEvent(input$submitting,{
    toggle("restart")
    disable("userOp5")
    disable("userOp6")
    disable("userOp7")
    disable("userOp8")
    disable("submitting")
    if(input$userOp5 == '1999' & input$userOp6 == '2000'
       & input$userOp7 == 'year' & input$userOp8 == 'cases' || input$userOp1 == '2000' & input$userOp2 == '1999'
       & input$userOp7 == 'year' & input$userOp8 == 'cases') {
      showElement("cort")
    }
    else{
      showElement("rong")
    }
    
    
  })
  
  
  observeEvent(input$retryy,{
    hide("restart")
    enable("userOp5")
    enable("userOp6")
    enable("userOp7")
    enable("userOp8")
    showElement("submitting")
    enable("submitting")
    hide("cort")
    hide("rong")

  })
  
  observeEvent(input$retryy,{
    reset("userOp5")
    reset("userOp6")
    reset("userOp7")
    reset("userOp8")
    showElement("submitting")
    enable("submit")

  })
  
  
  
  ###############Shiny Ace#################
  
  observeEvent(input$nextq, {
    # updateButton(session, "submit", disabled = FALSE)
    # updateButton(session, "nextq", disabled = TRUE)
    updateSelectInput(session, "answer", "pick an answer from below", c("","A", "B", "C"))
    output$mark <- renderUI({
      img(src = NULL, width = 30)
    })
  })
  
  
  #### question bank ####
  value <- reactiveValues(index =  1, mistake = 0,correct = 0)
  ans <- as.matrix(bank[1:9, 6])
  #ans <- data.frame(ans)
  index_list <- reactiveValues(list = 1:9)
  
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
  
  
  observeEvent(input$answer,{
    req(input$answer, input$answer !='')
    answer <- isolate(input$answer)
    interacted_statement <- rlocker::createStatement(
      list(
        verb = list(
          display = "selected"
        ),
        object = list(
          id = paste0(getCurrentAddress(session), "#", value$index),
          name = paste('Question', value$index),
          description = bank[value$index, 2]
          
        ),
        result = list(
          success = any(answer == ans[value$index,1]),
          response = paste(getResponseText(value$index, answer), 
                           as.character(Sys.time()))
        )
      )
    )
    
    # Store statement in locker and return status
    status <- rlocker::store(session, interacted_statement)
    
    print(interacted_statement) # remove me
    print(status) # remove me
  })
  
  
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
    
    statement <- rlocker::createStatement(
      list(
        verb = list(
          display = "answered"
        ),
        object = list(
          id = paste0(getCurrentAddress(session), "#", value$index),
          name = paste('Question', value$index),
          description = bank[value$index, 2]
        ),
        result = list(
          success = any(answer == ans[value$index,1]),
          response = paste(getResponseText(value$index, answer), 
                           as.character(Sys.time()))
        )
      )
    )
    
    # Store statement in locker and return status
    status <- rlocker::store(session, statement)
    
    print(statement) # remove me
    print(status) # remove me
    
    output$mark <- renderUI({
      if (any(answer == ans[value$index,1])){
        img(src = "correct.png",width = 30)
      }
      else{
        ig <- img(src = "incorrect.png",width = 30)
        w <- paste("You picked", answer, ", The correct answer is", ans[value$index, 1])
        HTML(paste(ig, w), sep = ' ')
      }
    })
  })
  
  observeEvent(input$reset,{
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session,"reset",disable = TRUE)
    updateSelectInput(session,"answer", "pick an answer from below", c("","A", "B", "C"))
    index_list$list <- c(index_list$list,1:9)
    value$index <- 1
    value$answerbox = value$index
    ans <- as.matrix(bank[1:9,9])
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
  })  
  
  # Initialize Learning Locker connection
  connection <- rlocker::connect(session, list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",
    agent = rlocker::createAgent()
  ))
  
  # Setup demo app and user.
  currentUser <- 
    connection$agent
  
  if(connection$status != 200){
    warning(paste(connection$status, "\nTry checking your auth token.")) 
  }
  
  
  # Setup demo app and user.
  
  output$Previewcar<-
    renderTable({
      head(cars, 4)
    }, striped = TRUE, hover=TRUE, bordered = TRUE, spacing = 'xs')
  
  output$Previewtree<-
    renderTable({
      head(trees, 4)
    }, striped = TRUE, hover=TRUE, bordered = TRUE, spacing = 'xs')
  
  output$Previewiris<-
    renderTable({
      head(iris, 4)
    }, striped = TRUE, hover=TRUE, bordered = TRUE, spacing = 'xs')
  
  ###########KNITR############
  observeEvent(input$eval,{
    withBusyIndicatorServer("eval",{
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
  
  
############ Reshaping Data ############
  # observeEvent(input$knob1, {
  #   updateKnobInput(session, inputId = 'knob2', label = 'Select the Maximum Value for the First Column', value = input$knob1)
  # })
  # 
  # observe(updateKnobInput(session, inputId = 'knob4', value = input$knob3 + input$knob2 - input$knob1))
  
  # unite
  output$uniteUI <- renderUI ({
    if (input$unite3 == T) {
      tags$code('R code: tidyr::unite(mtcars, "New_Column_Name", c(input$unite1))') 
    }
  })
  
  output$uniteOutput1 <- renderTable ({
    head(mtcars)
  })
  
  output$uniteOutput2 <- renderTable ({
    if (input$unite3 == T) {
      head(tidyr::unite(mtcars, 'New_Column', c(input$unite1)))
    }
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
      tags$code(paste('Code: dplyr::arrange(mtcars, mtcars[ , ', input$dwSTI1, ']'))
    }
  })
  
  output$code2 <- renderUI ({
    if (input$dwSTI2 == 'High to Low') {
      tags$code(paste('Code: dplyr::arrange(mtcars, descmtcars[ , ', input$dwSTI1, '])'))
    }
  })
  
  # #data frame
  # output$dfCode <- renderUI ({
  #   tags$code('dplyr::data_frame(a =', input$knob1, ':', input$knob2, ', b =', input$knob3, ':', input$knob4, ')')
  # })
  # 
  # output$dwTable7 <- renderDataTable({
  #   dplyr::data_frame(a = input$knob1:input$knob2, b = input$knob3:input$knob4)
  # })
  
  #gather
  output$dwTable1 <- renderTable({
    cases
  })
  
  output$dwTable2 <- renderTable({
    if (input$dw1 == TRUE) {
      tidyr::gather(cases, "year", "n", 2:4)
    }
  })
  
  #spread
  output$dwTable5 <- renderTable({
    pollution
  })
  
  output$dwTable6 <- renderTable({
    if (input$dw3 == TRUE) {
      tidyr::spread(pollution, size, amount)
    }
  })
  
############
  observeEvent(input$check1, {
    if (input$cd1 == 'left join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check2, {
    if (input$cd2 == 'inner join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check3, {
    if (input$cd3 == 'full join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check4, {
    if (input$cd4 == 'right join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check5, {
    if (input$cd5 == 'anti join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
    }
    else {
      sendSweetAlert(session, title = NULL, text = 'Check Your Answer Again', type = 'error', closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$check6, {
    if (input$cd6 == 'semi join') {
      sendSweetAlert(session, title = NULL, text = 'Congratulations', type = 'success', closeOnClickOutside = TRUE)
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