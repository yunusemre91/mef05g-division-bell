library(shiny)
library(DT)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readr)

setwd("/Users/nejatugurakin/Documents/DATA SCIENCE/Data Analytics Essentials/Week 4/Shiny_App/izmir_balik/shiny_balik/fishprice")
izmir_balik <- read_csv2("balik_hal_fiyatlari.csv",col_names = TRUE,show_col_types = FALSE) 
izmir_balik$ASGARI_UCRET<-as.numeric(izmir_balik$ASGARI_UCRET)
izmir_balik$YILAYGUN<- as.Date(izmir_balik$TARIH)
izmir_balik2 <- izmir_balik %>% mutate(YILAY = format(YILAYGUN,'%Y-%m'))
izmir_balik3<-izmir_balik2%>%
    group_by(YILAY,MAL_ADI)%>%
    summarise("Average_Maximum_Price"=mean(AZAMI_UCRET),"Average_Minimum_Price"=mean(ASGARI_UCRET))

izmir_balik4<-izmir_balik3%>%
    mutate(Ratio=(Average_Maximum_Price/Average_Minimum_Price))%>%
    arrange(desc(MAL_ADI)) 

level_order <- c("2021-01", "2021-02" ,"2021-03", "2021-04", "2021-05" ,"2021-06" ,"2021-07", "2021-08" ,"2021-09" ,"2021-10")


ui <- fluidPage(
    # Uygulama başlığının girilmesi
    titlePanel("İzmir Daily Fish Prices"),
    tabsetPanel(
        tabPanel(Position="left", "Upload Data",
                 sidebarPanel(
                     br(),
                     fileInput("balik_hal_fiyatlari", "Choose CSV File",
                               multiple = FALSE),
                     checkboxInput("header", "Header", TRUE),
                     br(),
                     radioButtons("sep", "Separator",
                                  choices = c(Comma = ",",
                                              Semicolon = ";",
                                              Tab = "\t"),
                                  selected = ";"),
                     br(),
                     radioButtons("disp", "Display",
                                  choices = c(Head = "head",
                                              All = "all"),
                                  selected = "head")             
                 ),tableOutput("contents")
        ),
        tabPanel(position= "left", "İzmir Daily Fish Prices",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("MAL_ADI", "Mal Adı: ",
                                     choices = list("ALL" = "ALL","BALIK"=unique(izmir_balik4$MAL_ADI)), selected = "ZARGANA (DENİZ)"),
                         selectInput("YILAY", "Tarih: ", 
                                     choices = list("ALL" = "ALL","TARIH"=unique(izmir_balik4$YILAY)), selected = "2021-01")
                         
                     ),
                     # Ana panonun tanımlanması
                     mainPanel(
                         tableOutput("mal_adi"),
                         plotOutput("plot",width = "100%"))
                 )
        )))




server <- function(input, output) {
    output$contents <- renderTable({
        
        # req() fonksiyonu ile önce girdi değerlerinin olup olmadığı kontrol edilir, eğer bir yanlışlık varsa işlem durdurulur
        req(input$balik_hal_fiyatlari)
        # tryCatch() fonksiyonu ile belirli koşullar belirterek olağandışı durumların yönetimi ile ilgili işlemler yapılır
        # Örneğin aşağıda ayrıştırma ile ilgili ve verinin gösterimi ile ilgili bir problem olduğu durumlarda
        # hangi işlemleri yapması gerektiğini belirtiyoruz.
        tryCatch(
            {
                
                df <- read.csv2(input$balik_hal_fiyatlari$datapath,
                                header = input$header,
                                sep = input$sep,
                                quote = input$quote)
                
                error = function(e) {
                    
                    stop(safeError(e))
                }
                
                if(input$disp == "head") {
                    return(head(df))
                }
                else {
                    return(df)
                }
                
                
            })
        
    })
    
    
    output$mal_adi <- renderTable({
        if(input$MAL_ADI == "ALL"  & input$YILAY  == "ALL"){
            izmir_balik4
        }
        else if(input$MAL_ADI != "ALL" & input$YILAY != "ALL"){
            izmir_balik4 %>% filter(MAL_ADI == input$MAL_ADI & YILAY == input$YILAY)
            
        }
        else if (input$MAL_ADI != "ALL" & input$YILAY == "ALL"){
            izmir_balik4 %>% filter(MAL_ADI == input$MAL_ADI)
        }
        else {izmir_balik4
        }
    },caption = "Fish Market Minimum and Maximum Prices' Ratio by Month ",caption.placement = getOption("xtable.caption.placement", "top"),striped =TRUE) 
    
    output$plot <- renderPlot({
        if(input$MAL_ADI == "ALL"  & input$YILAY  == "ALL"){
            
            paste("None")
        }
        else if (input$MAL_ADI != "ALL" & input$YILAY != "ALL"){
            
            paste("None")
        }
        else if(input$MAL_ADI != "ALL" & input$YILAY == "ALL"){
            
            ggplot(izmir_balik4 %>% filter(MAL_ADI == input$MAL_ADI | YILAY == input$YILAY) ,aes(x=YILAY,y=Ratio,color = MAL_ADI, group = MAL_ADI)) + geom_line()
        }
    })
}

shinyApp(ui=ui, server=server)
