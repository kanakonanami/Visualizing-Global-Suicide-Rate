library(shiny)
library(shinythemes)
library(tidyverse)
library(scales)
library(rsconnect)
library(DT)


# Load the data set
sui_dat <- read_csv("suicide_data.csv")

# Clean the data 
sui_dat <- rename(sui_dat, "Sex" = "Dim1")

sui_dat$Dim2 <- gsub("years", "", as.character(sui_dat$Dim2))
sui_dat$Dim2 <- gsub("of age", "", as.character(sui_dat$Dim2))
sui_dat$Dim2 <- gsub(" ", "", as.character(sui_dat$Dim2))

# For TAB 1
location_choices <- sort(unique(sui_dat$Location))
sex_choices <- c("Female", "Male", "Both sexes")
age_group_choices <- c("15-24", "25-34", "35-44", "45-54", 
                       "55-64", "65-74", "75-84", "85+")


fluidPage(
  titlePanel("Crude Suicide Rate"),
    
  # Tab 1 Line Plot & Bar Chart
  tabPanel(
    title = "Crude Suicide Rate by Country",
      
    sidebarLayout(
        
      sidebarPanel(
        titlePanel("Crude Suicide Rate by Country"),
          
        selectInput(
          inputId = "loc",
          label = "Location:",
          choices = location_choices,
          selected = "United States of America"
        ),
        
        radioButtons(
          inputId = "sx",
          label = "Sex:",
          choices = sex_choices,
          selected = "Female"
        )
      ),
      
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Crude Suicide Rate", plotOutput(outputId = "csrlocPlot")),
          tabPanel("Crude Suicide Rate Table", tableOutput(outputId = "csrlocTable")),
          tabPanel("Definition", verbatimTextOutput(outputId = "csrText")),
          tabPanel("Age-Specific Suicide Rate", plotOutput(outputId = "csragePlot")),
          tabPanel("Age-Specific Suicide Rate Table (2019)", DT::dataTableOutput(outputId = "suiageTable")) 
        )
      )
    )
  ),
    
    
    # Tab 2 Box Plot
  tabPanel(
    title = "Crude Suicide Rate by Region",
      
    sidebarLayout(
        
      sidebarPanel(
        titlePanel("Crude Suicide Rate by Region"),
          
        sliderInput(
          inputId = "yr",
          label = "Year:",
          min = 2000,
          max = 2018,
          value = 2010
        ),
          
        radioButtons(
          inputId = "sx_tb2",
          label = "Sex:",
          choices = sex_choices,
          selected = "Female"
        ),
          
        checkboxInput(
          inputId = "outl",
          label = "Include Outliers?",
          value = FALSE
        )
      ),
        
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Crude Suicide Rate by Region", plotOutput(outputId = "csrregPlot")),
          tabPanel("Table", DT::dataTableOutput(outputId = "csrregTable"))
        )
      )
    ),
    
    # Footer
    hr(),
    print("Data From:"),
    print("World Health Organization. (2022). Suicide mortality rate (per 100 000 population)."),
    print("Visualization by Raphaella Xie")
  ) 
)
  
  

