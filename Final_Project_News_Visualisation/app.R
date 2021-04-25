
## 1. Data preprocessing ############################

## 1.1. Install and Load R packages ############################
packages = c('tidyverse', 'readxl', 'dplyr', 'skimr', 'tibble', 
             'ggstatsplot','ggpubr','corrplot','seriation', 'dendextend',
             'tibbletime', 'anomalize', 'timetk','multcomp','devtools', 'car', 
             'DT', 'reshape2', 'plotly', 'ggforce','lubridate','tidyquant','ggplot2','shiny')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

## 1.2 Import data file #############################################

article_raw <- read_csv("data/zeroten_final.csv")
article_raw <- article_raw %>%  rename( 'date' = 'Published')

## 1.3. Handling date field #############################################

article_raw$date <- as.Date(article_raw$date, format = "%Y/%m/%d")
article_raw$month <- as.numeric(format(as.Date(article_raw$date), "%m"))
article_raw$month <- as.factor(article_raw$month)

## 1.4. Aggregate data set by date, Source, and Topic ####################

# article_daily <- article_raw %>% 
#   group_by(Source, date, Topic) %>% 
#   summarise(mean_interaction =  mean(Total, na.rm= TRUE))

#article_daily$mean_interaction <- round(article_daily$mean_interaction, 0)


## 2. Create R Shiny app ####################

ui <- 
  fluidPage(theme = shinytheme("cerulean"),
            navbarPage("Visualisation of News Headlines",
                       tabPanel("ANOVA Test", 
                                titlePanel("ANOVA Test of the average daily interactions"),
                                sidebarPanel(
                                  tags$h4("Filter"),
                                  selectInput(inputId = "choosetopic_anova",
                                              label = "Choose topic",
                                              choices = unique(article_daily$Topic))
                                ),
                                # Main panel for displaying outputs ----
                                mainPanel(
                                  # Output: Text output of selected date
                                  textOutput(outputId= ""),
                                  plotOutput(outputId = "anovatest",width = "100%", height = "500px")
                                  )
                              ),
                       tabPanel("Timeseries Anomaly Detection",
                                titlePanel("Timeseries Anomaly Detection of the average daily interactions"),
                                sidebarPanel(
                                  tags$h4("Filter"),
                                  selectInput(inputId = "choosetopic_anomal",
                                              label = "Choose topic",
                                              choices = unique(article_daily$Topic)),
                                  selectInput(inputId = "choosesource_anomal",
                                              label = "Choose media",
                                              choices = unique(article_daily$Source))
                                ),
                                # Main panel for displaying outputs ----
                                mainPanel(
                                  # Output: Text output of selected date
                                  textOutput(outputId= ""),
                                  plotOutput(outputId = "anomaly",width = "100%", height = "500px"),
                                  tableOutput(outputId= "tablesummary")
                                )
                              )
                       )
            )


server <- function(input, output, session) {
  
  #Text output for chosen date
  output$seldate <- renderText({
    paste("Selected topic = " ,(input$choosetopic))
    
  })
  
  output$anovatest <- renderPlot({
    ggstatsplot::ggbetweenstats(
      data = article_daily %>% filter(Topic == input$choosetopic_anova),
      x = Source,
      y = mean_interaction,
      mean.plotting = TRUE,
      mean.ci = TRUE,
      pairwise.comparisons = TRUE, # display results from pairwise comparisons
      notch = TRUE,
      type = "np",
      messages = FALSE
    )
  })
  
  ######## Anomaly detection
  
  output$anomaly <- renderPlot({

    article_daily2 <- article_raw %>% 
      filter(Topic == input$choosetopic_anomal & Source == input$choosesource_anomal) %>% 
      group_by(Topic, Source, date) %>% 
      summarise(mean_interaction =  round(mean(Total, na.rm= TRUE),0)) 
    
    df_tibble <- as_tibble(article_daily2 %>% select(date,mean_interaction)) 
    anomal_result <- df_tibble %>%
      time_decompose(mean_interaction,
                     frequency = "auto",
                     trend = "auto") %>%
      anomalize(remainder, alpha = 0.05, max_anoms = 0.2) %>%
      time_recompose() %>%
      plot_anomalies(time_recomposed = TRUE) +
      ggtitle("alpha = 0.05")
    
    anomal_result
    # time_plot <- df_tibble %>% timetk::plot_anomaly_diagnostics(date,mean_interaction) %>%  ungroup()
    #time_plot$date <- as.character(time_plot$date)
    # time_plot %>% plot_ly(x = ~date, y = ~ observed, type="scatter", 
    #                       mode = "lines", color = I('dartk green'), name="mean")
    
  })
  
  output$tablesummary <- renderTable({
    
    article_daily2 <- article_raw %>% 
      filter(Topic == input$choosetopic_anomal & Source == input$choosesource_anomal) %>% 
      group_by(Topic, Source, date) %>% 
      summarise(mean_interaction =  round(mean(Total, na.rm= TRUE),0)) 
    
    df_tibble <- as_tibble(article_daily2 %>% select(date,mean_interaction)) 
    anomal_result_days <- df_tibble %>%
      time_decompose(mean_interaction,
                     frequency = "auto",
                     trend = "auto") %>%
      anomalize(remainder, alpha = 0.05, max_anoms = 0.2) %>%
      time_recompose() %>% filter(anomaly == 'Yes')
    
    anomal_days <- unique(anomal_result_days$date)

    
    #df_tibble$date <- as.character(df_tibble$date)
    #date_filter <- subset(df_tibble,df_tibble$date == (input$choosedate))
    # time_table <- df_tibble %>% timetk::tk_anomaly_diagnostics(date, mean_interaction) #%>% filter(anomaly=='Yes')
    #anomal_result_days$date <- as.character(anomal_result_days$date)
    
    #trend <- round(anomal_list2$trend,0)
    
    anomal_article <- article_raw %>%
      filter(Topic == input$choosetopic_anomal & Source == input$choosesource_anomal & 
             date == anomal_days) %>% 
      select(Topic, Source, date, Headline, Total)
    #anomal_article$Trend <- trend
    anomal_article$date <- as.character(anomal_article$date)
    anomal_article
    
  })
}

shinyApp(ui, server)