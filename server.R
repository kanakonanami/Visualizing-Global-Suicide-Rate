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



############
# server   #
############
function(input, output, session) {
  
  # Tab 1
  
  # CSR by country during 2000~2018
  csrloc_dat <- reactive({
    if (input$sx == "Both sexes") {
      sui_dat %>%
        filter(Period < 2019,
               Location %in% input$loc) %>%
        select(Period, Sex, FactValueNumeric, Value) %>%
        group_by(Sex)
    } else {
      sui_dat %>%
        filter(Location %in% input$loc,
               Sex == input$sx,
               Period < 2019) %>% 
        select(Period, Sex, FactValueNumeric, Value)
    }
  })
  
  
  # Tab 1 Line Plot
  output$csrlocPlot <- renderPlot({
    
    # Set the col value "Sexes" in ggplot aesthetics
    if (input$sx == "Both sexes") {
      Sexes <- csrloc_dat()$Sex
    } else {
      Sexes <- NULL
    }
    
    # Visualization of CSR of a selected sex by country 
    ggplot(data = csrloc_dat(), 
           aes(x = Period, 
               y = FactValueNumeric,
               col = Sexes)) +
      geom_point() + 
      geom_line() +
      theme_bw(base_size = 13) + 
      scale_x_continuous(breaks = pretty_breaks(9)) + 
      labs(title = paste("Crude Suicide Rate of", input$sx ,"in", 
                         input$loc, "during 2000-2018"),
           subtitle = "number of suicide deaths per 100,100 population",
           x = "Year",
           y = "Count")
  })
  
  
  # Tab 1 Crude Suicide Rate Table
  output$csrlocTable <- renderTable({
    
    loc_table <- csrloc_dat() %>%
      select(Sex, Period, Value)
    
    # Set the column names displayed
    names(loc_table) <- c("Sex", "Year", "Value (with 95% CI)")
    loc_table$Year <- as.integer(loc_table$Year)
    
    loc_table
  })
  
  
  # Tab 1 Bar Chart: 
  
  # Age-Specific Suicide Mortality Rate in selected location & sex in 2019
  agcsr_dat <- reactive({
    sui_dat %>%
      filter(Period == 2019,
             Location %in% input$loc,
             Sex == input$sx,
             Dim2 %in% age_group_choices) %>%
      select(Dim2, Sex, FactValueNumeric, Value) %>%
      arrange(Dim2)
  })
  
  
  # Visualization of Age-Specific Suicide Mortality Rate
  output$csragePlot <- renderPlot({
    
    ggplot(data = agcsr_dat(), 
           aes(x = Dim2, 
               y = FactValueNumeric)) +
      geom_bar(stat = "identity",
               color = "black",
               fill = "lightblue") + 
      theme_bw(base_size = 13) + 
      labs(title = paste("Age-Specific Suicide Rate in", 
                         input$loc, "in 2019"),
           subtitle = "number of suicide deaths per 100,100 people in a selected age group",
           x = "Age Group",
           y = "Count")
  })
  
  
  # Tab 1 Table: Age-Specific Suicide Rate 
  output$suiageTable <- DT::renderDataTable({
    
    suiage_table <- agcsr_dat() %>%
      select(Dim2, Sex, Value)
    
    names(suiage_table) <- c("Age Group", "Sex", "Value")
    
    suiage_table
  })
  
  
  
  # Tab 1 Text: Definition of Crude Suicide Rate
  output$csrText <- renderText({
    
    # Set the definition of Crude Suicide Rate
    def_csr <- paste("Crude Suicide Rate, also defined as Suicide Mortality Rate:
    -- the number of suicide deaths in a year, 
    divided by the population and multiplied by 100 000.")
    
    def_csr
    
  })
  
  
  # Tab 2
  
  # CSR by region in a selected year
  csrreg_dat <- reactive({
    sui_dat %>%
      filter(Period == input$yr) %>%  ####
      group_by(ParentLocation) %>% 
      select(ParentLocation, Location, Sex, FactValueNumeric, Value)
  })
  
  # Tab 2 Box Plot: Crude Suicide Rate by Region
  output$csrregPlot <- renderPlot({
    
    # Local Crude Suicide Rate Data
    local_csr <- csrreg_dat()
    
    # Set the scale of y-axis and caption
    if (input$outl == FALSE) {
      y_scale <- quantile(csrreg_dat()$FactValueNumeric, c(0, 0.9))
      cap <- "outliers (values greater than 90-percentiles) were removed"
    } else {
      y_scale <- NULL
      cap <- NULL
    }
    
    # Set the ggplot col aesthetics
    Sexes <- NULL
    

    # Set the local data by sex input
    if (input$sx_tb2 == "Both sexes") {
      Sexes <- local_csr$Sex
      
      local_csr <- local_csr %>%
        group_by(Sex)
      
    } else {
      local_csr <- local_csr %>%
        filter(Sex == input$sx_tb2) %>%
        group_by(Sex)
    }
    
    
    # Visualization of Crude Suicide Rate by Region
    ggplot(data = local_csr,
           aes(x = ParentLocation,
               y = FactValueNumeric,
               col = Sexes)) + 
      geom_boxplot() +
      scale_y_continuous(limits = y_scale) + 
      labs(title = paste("Crude Suicide Rate of", input$sx_tb2 ,"by Region in", input$yr),
           subtitle = "number of suicide deaths per 100,000 population",
           x = "",
           y = "Count",
           caption = cap) +
      theme_bw(base_size = 13) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 13),
            plot.caption = element_text(size = 13))
  })
  
  
  # Tab 2 Table
  output$csrregTable <- DT::renderDataTable({
    
    csrreg_table <- csrreg_dat() %>%
      filter(Sex == input$sx_tb2) %>% #####
    ungroup() %>%
      select(Location, Sex, Value) %>%
      arrange(Location)
    
    #Set the column names displayed
    names(csrreg_table) <- c("Location", "Sex", "Value (with 95% CI)")
    
    csrreg_table
    
  })
  
}
