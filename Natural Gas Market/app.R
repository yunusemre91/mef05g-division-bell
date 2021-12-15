library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readr)
library(stringr)
library(knitr)
library(kableExtra)
library(scales)
library(gridExtra)
library(ggrepel)
library(DT)

options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")

df <- read_rds("https://raw.githubusercontent.com/pjournal/mef05g-division-bell/gh-pages/natural_gas_data.rds")

# Define UI for application that draws a histogram

ui <- fluidPage(
    tabsetPanel(
        tabPanel("Gas Reference Price",
                 sidebarLayout(
                     sidebarPanel(
                         dateInput(inputId = "date1", 
                                   label = "Choose the first date", 
                                   format = "yyyy-mm-dd", 
                                   value = min(df$Date), 
                                   startview = "month"), 
                         
                         # Second date -----------------------------------------
                         
                         dateInput(inputId = "date2", 
                                   label = "Choose the last date", 
                                   format = "yyyy-mm-dd", 
                                   value = max(df$Date), 
                                   startview = "month"),
                         
                         # The plot -------------------------------------------
                         DT::dataTableOutput("Daily_GRP")
                     ),
                     mainPanel(plotOutput(outputId = "grp_plot"))
                 )),
        
        tabPanel("Total Trade Volume",
                 sidebarLayout(
                     sidebarPanel(
                         dateInput(inputId = "date3", 
                                   label = "Choose the first date", 
                                   format = "yyyy-mm-dd", 
                                   value = min(df$Date), 
                                   startview = "month"), 
                         
                         # Second date -----------------------------------------
                         
                         dateInput(inputId = "date4", 
                                   label = "Choose the last date", 
                                   format = "yyyy-mm-dd", 
                                   value = max(df$Date), 
                                   startview = "month"),
                         
                         # The plot -------------------------------------------
                         DT::dataTableOutput("Daily_TTV")
                     ),
                     mainPanel(plotOutput(outputId = "ttv_plot"))
                 )),
        
    ))

server <- function(input, output, session) {
    
    output$grp_plot <- renderPlot({
            ggplot(filter(df,between(Date, input$date1, input$date2)),
                   aes(x = Date, y = Gas_Reference_Price, color=Gas_Reference_Price)) +
            geom_line(size=1) + ylab("Gas Reference Price")}
        
    )
    
    output$Daily_GRP <- DT::renderDataTable(rownames=FALSE, {
        df[c(1,8)] 
    })
    
    output$ttv_plot <- renderPlot({
            ggplot(filter(df,between(Date, input$date3, input$date4)), 
                   aes(x = Date, y = Total_Trade_Volume/(10**6))) +
            geom_line(color="red", size=0.8) + ylab("Total Trade Volume (millions)")
        }
    )
    
    output$Daily_TTV <- DT::renderDataTable(rownames=FALSE, {
        df[c(1,2)] 
    })
    
}

shinyApp(ui = ui, server = server)



