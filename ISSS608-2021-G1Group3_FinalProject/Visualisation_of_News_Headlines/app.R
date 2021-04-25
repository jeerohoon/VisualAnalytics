
## 1. Data preprocessing ############################

## 1.1.Load R packages ############################
library(shinydashboard)
library(tidyverse)
library(readxl)
library(dplyr)
library(skimr)
library(tibble)
library(ggstatsplot)
library(ggpubr)
library(corrplot)
library(seriation)
library(dendextend)
library(tibbletime)
library(anomalize)
library(timetk)
library(multcomp)
library(devtools)
library(car)
library(DT)
library(reshape2)
library(plotly)
library(ggforce)
library(lubridate)
library(tidyquant)
library(ggplot2)
library(shiny)
library(shinythemes)
library(purrrlyr)
library(tidytext)
library(LDAvis)
library(shinyjs)
library(shinyWidgets)

## 1.2 Import data file #############################################

article_raw <- read_csv("data/zeroten_final.csv")
article_raw <- article_raw %>%  rename( 'date' = 'Published')



## 1.3. Handling date field #############################################

article_raw$date <- as.Date(article_raw$date, format = "%Y/%m/%d")
article_raw$month <- as.numeric(format(as.Date(article_raw$date), "%m"))
article_raw$month <- as.factor(article_raw$month)

## 2. Create R Shiny app ############################################

ui <- 
    fluidPage(theme = shinytheme("cerulean"),
              navbarPage("Visualisation of News Headlines",
                         
                         ########################################################
                         tabPanel("Welcome",
                                  fluidPage(
                                      
                                      tags$h1("Welcome to Our App"),
                                      tags$h3("This App is the work of Atticus Foo, Jihun Nam and Gerald Chee."), 
                                      tags$h3("The aim of this dashboard help readers of local news sites
                                    (both mainstream and non-mainstream) quickly navigate and understand news headlines from 2020. "),
                                      tags$h3("Please refer to the user guide on how to use the app."),
                                      actionButton("", "User Guide", onclick="window.open('User guide.pdf')"),
                                      tags$h3("For more information, do refer to our research paper."),
                                      actionButton("", "Research Paper", onclick="window.open('Final-Report.pdf')"),
                                      tags$h3("Academic Poster"),
                                      actionButton("", "Poster", onclick="window.open('Visualising News Headlines from 2020 with R Shiny.pdf')")
                                      
                                  )
                         ),
                         ########################################################
                         tabPanel("Explore",
                                  fluidPage(
                                      tabsetPanel(
                                          tabPanel("Overview of 2020 articles",
                                                   sidebarPanel(width = 2,
                                                                tags$h4("Comparison 1: Choose below"),
                                                                selectInput(inputId = "choosetopic_overview1",
                                                                            label = "Choose topic-1",
                                                                            choices = unique(article_raw$Topic)),
                                                                selectInput(inputId = "choosesource_overview1",
                                                                            label = "    Choose media-1",
                                                                            choices = unique(article_raw$Source)),
                                                                tags$h4("\n"),
                                                                tags$h4("Comparison 2: Choose below"),
                                                                selectInput(inputId = "choosetopic_overview2",
                                                                            label = "    Choose topic-2",
                                                                            choices = unique(article_raw$Topic), selected="Covid-Domitory Cases"),
                                                                
                                                                selectInput(inputId = "choosesource_overview2",
                                                                            label = "    Choose media-2",
                                                                            choices = unique(article_raw$Source), selected="MSN"),
                                                                tags$h4("\n"),
                                                                tags$h4("Select Date period"),
                                                                dateRangeInput('dateRange_overview',
                                                                               label = 'Select Date range',
                                                                               start = "2020-01-01", end = "2020-12-31"),
                                                   ),
                                                   # Main panel for displaying outputs ----
                                                   mainPanel(                    
                                                       textOutput(outputId= ""),
                                                       fluidRow(
                                                           valueBoxOutput("topic1_num"),
                                                           valueBoxOutput("topic1_interaction"),
                                                           
                                                       ),
                                                       fluidRow(
                                                           valueBoxOutput("topic2_num"),
                                                           valueBoxOutput("topic2_interaction")
                                                       )
                                                   )
                                          ),
                                          ########################################################
                                          tabPanel("Engagement by Media",
                                                   titlePanel("Average Engagement by Media Source"),
                                                   sidebarPanel(width = 2,
                                                                
                                                                tags$h4("Select filters below"),
                                                                
                                                                selectInput(inputId = "choosetopic_interaction",
                                                                            label = "Choose topic",
                                                                            choices = unique(article_raw$Topic)),
                                                                dateRangeInput('dateRange_interaction',
                                                                               label = 'Select Date range',
                                                                               start = "2020-01-01", end = "2020-12-31")
                                                   ),
                                                   mainPanel(                    
                                                       textOutput(outputId= ""),
                                                       plotlyOutput(outputId = "interaction_plot",width = "120%", height = "500px")
                                                   )
                                          ),                                  
                                          ########################################################
                                          tabPanel("Total Engagement Across Topics",
                                                   titlePanel("Engagement Across Topics By Source"),
                                                   sidebarPanel(width = 2,
                                                                tags$h4("Select filters below"),
                                                                dateRangeInput('dateRange_engesum',
                                                                               label = 'Select Date range',
                                                                               start = "2020-01-01", end = "2020-12-31")
                                                                
                                                   ),
                                                   mainPanel(
                                                       textOutput(outputId= ""),
                                                       plotlyOutput(outputId = "engesum_plot",width = "120%", height = "500px")
                                                   )
                                          ),
                                          ########################################################
                                          tabPanel("Total Engagement Across Source",
                                                   titlePanel("Engagement Across Sources by Topic"),
                                                   sidebarPanel(width = 2,
                                                                tags$h4("Select filters below"),
                                                                dateRangeInput('dateRange_engesum2',
                                                                               label = 'Select Date range',
                                                                               start = "2020-01-01", end = "2020-12-31")
                                                                
                                                   ),
                                                   mainPanel(
                                                       textOutput(outputId= ""),
                                                       plotlyOutput(outputId = "engesum_plot2",width = "120%", height = "500px")
                                                   )
                                          ),
                                          
                                          
                                          ########################################################
                                          tabPanel("Topics through the Year",
                                                   titlePanel("Visualising Topics across time"),
                                                   sidebarPanel(width = 2,
                                                                tags$h4("Select filters below"),
                                                                radioGroupButtons(inputId = "viewoption", #to change view options for plot to daily or monthly
                                                                                  label = "Option",
                                                                                  choices = c("Daily","Monthly"),selected = "Monthly"),
                                                                dateRangeInput('dateRange_vis',
                                                                               label = 'Select Date range',
                                                                               start = "2020-01-01", end = "2020-12-31"),
                                                                selectInput(inputId = "choosetopic_vis",
                                                                            label = "Choose topic",
                                                                            choices = unique(article_raw$Topic)),
                                                                selectInput(inputId = "choosesource_vis",
                                                                            label = "Choose media",
                                                                            choices = unique(article_raw$Source)),
                                                                prettySwitch(inputId = "compareplot",
                                                                             label = "Compare Plot?",
                                                                             status = "success",
                                                                             fill = TRUE),#to toggle comparison
                                                                conditionalPanel(condition = "input.compareplot==true", 
                                                                                 selectInput(inputId = "choosetopic_vis2",label = "Choose topic",choices = unique(article_raw$Topic)),
                                                                                 selectInput(inputId = "choosesource_vis2", label = "Choose media",choices = unique(article_raw$Source))
                                                                ),
                                                                actionButton("ApplytopButton", "Apply")
                                                   ),
                                                   # Main panel for displaying outputs ----
                                                   mainPanel(                    
                                                       textOutput(outputId= ""),
                                                       plotlyOutput(outputId = "topic_plot",width = "120%", height = "500px")
                                                   )
                                          ),
                                          
                                          ########################################################
                                          tabPanel("LDA Viz", 
                                                   titlePanel("LDA Visualisation"),
                                                   visOutput("lda")
                                          ),
                                          ########################################################
                                          tabPanel("Coporaexplorer",
                                                   actionButton("ApplyButton_corpora", "Go To CorporaExplorer",onclick ="window.open('https://cheekahwengerald.shinyapps.io/my_app/', '_blank')"))
                                      ))),
                         
                         ########################################################
                         tabPanel("Discover",
                                  fluidPage(
                                      tabsetPanel(
                                          tabPanel("Text relationship",
                                                   titlePanel("Visualising Word Sequences in Local Media Coverage"),
                                                   sidebarPanel(width = 2,
                                                                tags$h4("Select filter below"),
                                                                textInput(inputId= "text1" ,  label =  "Input Text1" ,value = 'sony'),
                                                                textInput(inputId= "text2" ,  label =  "Input Text2" ,value = 'samsung'),
                                                                textInput(inputId= "n_word" ,  label =  "n_word" ,value = 20),
                                                                textInput(inputId= "n_top" ,  label =  "n_top" ,value = 150),
                                                                actionButton("ApplyButton_text_relation", "Apply")
                                                   ),
                                                   mainPanel(
                                                       plotOutput(outputId = "textRelation",width = "120%", height = "700px")
                                                   )      
                                          )
                                      )
                                  )
                         ),
                         
                         ########################################################
                         tabPanel("Detect",
                                  fluidPage(
                                      tabsetPanel(
                                          tabPanel("Timeseries Anomaly Detection",
                                                   titlePanel("Timeseries Anomaly Detection of the average daily interactions"),
                                                   sidebarPanel(width = 2,
                                                                tags$h4("Select filters below"),
                                                                selectInput(inputId = "choosetopic_anomal",
                                                                            label = "Choose topic",
                                                                            choices = unique(article_raw$Topic)),
                                                                selectInput(inputId = "choosesource_anomal",
                                                                            label = "Choose media",
                                                                            choices = unique(article_raw$Source)),
                                                                dateRangeInput('dateRange_anomaly',
                                                                               label = 'Select Date range',
                                                                               start = "2020-01-01", end = "2020-12-31"),
                                                                sliderInput("alpha", "alpha:",
                                                                            min = 0, max = 30,
                                                                            value = 5, step = 1, post = '%'),
                                                                sliderInput("max_anoms", "Maximum % of anomaly:",
                                                                            min = 0, max = 30,
                                                                            value = 5, step = 1, post = '%'),
                                                                actionButton("ApplyButton", "Apply")
                                                   ),
                                                   # Main panel for displaying outputs ----
                                                   mainPanel(
                                                       # Output: Text output of selected date
                                                       textOutput(outputId= ""),
                                                       plotlyOutput(outputId = "anomaly",width = "120%", height = "500px"),
                                                       dataTableOutput(outputId= "tablesummary",width = "120%")
                                                   )
                                          )
                                          
                                          
                                      )
                                  )
                         )
              )
    )





server <- function(input, output, session) {
    
    ######## Overview #####################################################
    
    # Preprocessing for data set
    article_daily_overview1 <- reactive({
        
        a <- article_raw %>% 
            filter(Topic == input$choosetopic_overview1 & 
                       Source == input$choosesource_overview1 &
                       date >= input$dateRange_anomaly[1] & date <= input$dateRange_anomaly[2]) %>% 
            group_by(Topic, Source) %>% 
            summarise(n =n(), mean_interaction =  round(mean(Total, na.rm= TRUE),0)) 
        
    })
    
    article_daily_overview2 <- reactive({
        
        b <- article_raw %>% 
            filter(Topic == input$choosetopic_overview2 & 
                       Source == input$choosesource_overview2 &
                       date >= input$dateRange_overview[1] & date <= input$dateRange_overview[2]) %>% 
            group_by(Topic, Source) %>% 
            summarise(n =n(), mean_interaction =  round(mean(Total, na.rm= TRUE),0)) 
        
    })
    
    
    output$topic1_num <- renderInfoBox({
        
        valueBox(paste(article_daily_overview1()$n),paste("Daily Average Number of Articles in topic1: ", input$choosetopic_overview1, " & media1: ", input$choosesource_overview1), input$choosetopic_overview1, icon = icon(""), color = "aqua")
    })
    
    output$topic1_interaction <- renderInfoBox({
        
        valueBox(paste(article_daily_overview1()$mean_interaction), paste("Daily Average Number of Articles in topic1: ", input$choosetopic_overview1, " & media1: ", input$choosesource_overview1), input$choosetopic_overview1, icon = icon(""), color = "aqua")
    })
    
    output$topic2_num <- renderInfoBox({
        
        valueBox(paste(article_daily_overview2()$n), paste("Daily Average Number of Articles in topic2: ", input$choosetopic_overview2, " & media2: ", input$choosesource_overview2), input$choosetopic_overview2, icon = icon(""), color = "aqua")
    })
    
    output$topic2_interaction <- renderInfoBox({
        
        valueBox(paste(article_daily_overview2()$mean_interaction), paste("Daily Average Number of Articles in topic2: ", input$choosetopic_overview2, " & media2: ", input$choosesource_overview2), input$choosetopic_overview2, icon = icon(""), color = "aqua")
    })
    
    # Interaction data set ############################################
    article_daily_interaction <- reactive({
        
        aa<-article_raw %>% 
            filter(Topic == input$choosetopic_interaction, 
                   date >= input$dateRange_interaction[1] & date <= input$dateRange_interaction[2]) %>% 
            group_by(Source, Topic) %>% 
            summarise(mean_interaction =  round(mean(Total, na.rm= TRUE),0)) 
        
    })
    
    output$interaction_plot <- renderPlotly({
        
        fig <- plot_ly(data = article_daily_interaction(), x = ~Source, y=~mean_interaction,
                       type = 'bar', name = input$choosetopic_interaction)
        
        fig <- fig %>% layout(yaxis = list(title = 'Avg. of Interactions'), barmode = 'group')
    })
    
    # Total Engagement Plot1 ############################################
    article_engesum<- reactive({
        
        aa<-article_raw %>% 
            filter(date >= input$dateRange_engesum[1] & date <= input$dateRange_engesum[2]) %>% 
            group_by(Source, Topic) %>% 
            summarise('sum'=sum(Total)) 
        
    })
    
    
    output$engesum_plot <- renderPlotly({
        
        fig <- ggplotly(ggplot(article_engesum(), aes(x=Source,y=sum,fill=Topic))+
                            geom_bar(stat = "identity",position = "dodge")+
                            labs(title="Sum of Engagement Across Topics by Source",
                                 x ="Source", y = "Sum")+
                            scale_y_continuous(labels = scales::number_format())+
                            theme_classic())
    })
    # Total Engagement Plot2 ############################################
    article_engesum2<- reactive({
        
        bb<-article_raw %>% 
            filter(date >= input$dateRange_engesum2[1] & date <= input$dateRange_engesum2[2]) %>% 
            group_by(Source, Topic) %>% 
            summarise('sum'=sum(Total)) 
        
    })
    
    
    output$engesum_plot2 <- renderPlotly({
        
        fig <- ggplotly(ggplot(article_engesum2(), aes(x=Topic,y=sum,fill=Source))+
                            geom_bar(stat = "identity",position = "dodge")+
                            labs(title="Sum of Engagement Across Sources by Topic",
                                 x ="Source", y = "Sum")+
                            scale_y_continuous(labels = scales::number_format())+
                            theme_classic()+theme(axis.text.x = element_text(angle = 90))+
                            scale_fill_brewer(palette="Set2"))
    })
    
    ######## LDA Visualisaion ############################################
    
    output$lda <- renderVis(read_lines("data/LDAvis.json"))
    
    ######## textRelation_graph ############################################
    
    textRelation_graph <- reactive({
        
        n_word <- as.numeric(input$n_word)
        n_top <- as.numeric(input$n_top)
        n_gramming <- 3
        
        trigrams <- tibble(text = article_raw$Cleaned) %>%
            unnest_tokens(trigram, text, token = "ngrams", n = n_gramming)
        
        start_words <- c(input$text1, input$text2)
        
        #
        pattern <- str_c("^", start_words, " ", collapse = "|")
        top_words <- trigrams %>%
            filter(str_detect(trigram, pattern)) %>%
            count(trigram, sort = TRUE) %>%
            slice(seq_len(n_top)) %>%
            pull(trigram)
        
        trigrams <- trigrams %>%
            filter(trigram %in% top_words)
        
        #nodes
        str_nth_word <- function(x, n, sep = " ") {
            str_split(x, pattern = " ") %>%
                map_chr(~ .x[n])
        }
        
        nodes <- map_df(seq_len(n_gramming),
                        ~ trigrams %>%
                            mutate(word = str_nth_word(trigram, .x)) %>%
                            count(word, sort = TRUE) %>%
                            slice(seq_len(n_word)) %>% 
                            mutate(y = seq(from = n_word + 1, to = 0, 
                                           length.out = n() + 2)[seq_len(n()) + 1],
                                   x = .x))
        
        nodes %>% 
            ggplot(aes(x, y, label = word)) +
            geom_text()
        
        sigmoid <- function(x_from, x_to, y_from, y_to, scale = 5, n = 100) {
            x <- seq(-scale, scale, length = n)
            y <- exp(x) / (exp(x) + 1)
            tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
                   y = y * (y_to - y_from) + y_from)
        }
        
        egde_lines <- function(trigram, from_word, to_word, scale = 5, n = 50, 
                               x_space = 0) {
            
            from_word <- from_word %>%
                select(-n) %>%
                set_names(c("from", "y_from", "x_from"))
            
            to_word <- to_word %>%
                select(-n) %>%
                set_names(c("to", "y_to", "x_to"))
            
            links <- crossing(from = from_word$from, 
                              to = to_word$to) %>%
                mutate(word_pair = paste(from, to),
                       number = map_dbl(word_pair, 
                                        ~ sum(str_detect(trigram$trigram, .x)))) %>%
                left_join(from_word, by = "from") %>%
                left_join(to_word, by = "to")
            
            links %>%
                by_row(~ sigmoid(x_from = .x$x_from + 0.2 + x_space,
                                 x_to = .x$x_to - 0.05, 
                                 y_from = .x$y_from, y_to = .x$y_to, 
                                 scale = scale, n = n) %>%
                           mutate(word_pair = .x$word_pair,
                                  number = .x$number,
                                  from = .x$from)) %>%
                pull(.out) %>%
                bind_rows()
        }
        
        egde_lines(trigram = trigrams, 
                   from_word = filter(nodes, x == 1), 
                   to_word = filter(nodes, x == 2)) %>%
            filter(number > 0) %>%
            ggplot(aes(x, y, group = word_pair, alpha = number, color = from)) +
            geom_line()
        
        # egdes between first and second column
        egde1 <- egde_lines(trigram = trigrams, 
                            from_word = filter(nodes, x == 1), 
                            to_word = filter(nodes, x == 2), 
                            n = 50) %>%
            filter(number > 0) %>%
            mutate(id = word_pair)
        
        # Words in second colunm
        ## That start with he
        second_word_he <- nodes %>%
            filter(x == 2) %>%
            select(-n) %>%
            left_join(
                trigrams %>% 
                    filter(str_nth_word(trigram, 1) == start_words[1]) %>%
                    mutate(word = str_nth_word(trigram, 2)) %>%
                    count(word), 
                by = "word"
            ) %>%
            replace_na(list(n = 0))
        
        ## That start with she
        second_word_she <- nodes %>%
            filter(x == 2) %>%
            select(-n) %>%
            left_join(
                trigrams %>% 
                    filter(str_nth_word(trigram, 1) == start_words[2]) %>%
                    mutate(word = str_nth_word(trigram, 2)) %>%
                    count(word), 
                by = "word"
            ) %>%
            replace_na(list(n = 0))
        
        # Words in third colunm
        ## That start with he
        third_word_he <- nodes %>%
            filter(x == 3) %>%
            select(-n) %>%
            left_join(
                trigrams %>% 
                    filter(str_nth_word(trigram, 1) == start_words[1]) %>%
                    mutate(word = str_nth_word(trigram, 3)) %>%
                    count(word), 
                by = "word"
            ) %>%
            replace_na(list(n = 0))
        
        ## That start with she
        third_word_she <- nodes %>%
            filter(x == 3) %>%
            select(-n) %>%
            left_join(
                trigrams %>% 
                    filter(str_nth_word(trigram, 1) == start_words[2]) %>%
                    mutate(word = str_nth_word(trigram, 3)) %>%
                    count(word), 
                by = "word"
            ) %>%
            replace_na(list(n = 0))
        
        # egdes between second and third column that starts with he
        egde2_he <- egde_lines(filter(trigrams, 
                                      str_detect(trigram, paste0("^", start_words[1], " "))), 
                               second_word_he, third_word_he, n = 50) %>%
            mutate(y = y + 0.05,
                   from = start_words[1],
                   id = str_c(from, word_pair, sep = " ")) %>%
            filter(number > 0)
        
        # egdes between second and third column that starts with she
        egde2_she <- egde_lines(filter(trigrams, 
                                       str_detect(trigram, paste0("^", start_words[2], " "))), 
                                second_word_she, third_word_she, n = 50) %>%
            mutate(y = y - 0.05,
                   from = start_words[2],
                   id = str_c(from, word_pair, sep = " ")) %>%
            filter(number > 0)
        
        # All edges
        edges <- bind_rows(egde1, egde2_he, egde2_she)
        
        result <- nodes %>% 
            ggplot(aes(x, y, label = word, size = n)) +
            geom_text(hjust = 0, color = "black") +
            theme_void() +
            geom_line(data = edges,
                      aes(x, y, group = id, color = from, alpha = sqrt(number),size=1),
                      inherit.aes = FALSE) +
            theme(plot.background = element_rect(fill = "white"),
                  text = element_text(color = "black", size = 15)) +
            guides(alpha = "none", color = "none", size = "none") +
            xlim(c(0.9, 3.2)) +
            scale_color_manual(values = c("#c10534", "#1f77b4")) +
            labs(title = " Vizualizing Trigrams in Local Media Coverage") + 
            scale_size(range = c(3, 8))
        
        return(result)
    })
    
    output$textRelation <- renderPlot({
        input$ApplyButton_text_relation
        isolate({
            textRelation_graph() + 
                ggtitle(paste("Visualising Word Sequences in Local Media Coverage '", input$text1, "' & '", input$text2, "'", "\n")) +
                theme(plot.title = element_text(hjust = 0.5))
        })
    })
    
    
    ######## Anomaly detection ############################################
    
    # Preprocessing for data set
    article_daily_reactive <- reactive({
        
        anomaly_raw<-article_raw %>% 
            filter(Topic == input$choosetopic_anomal & 
                       Source == input$choosesource_anomal &
                       date >= input$dateRange_anomaly[1] & date <= input$dateRange_anomaly[2]) %>% 
            group_by(Topic, Source, date) %>% 
            summarise(mean_interaction =  round(mean(Total, na.rm= TRUE),0)) 
        
    })
    
    # Preprocessing for data set
    df_tibble <- reactive({
        
        as_tibble(article_daily_reactive() %>% select(date,mean_interaction))
        
    })
    
    # Create anomal result plot  
    anomal_result_plot <- reactive({
        
        df_tibble() %>%
            time_decompose(mean_interaction,
                           frequency = "auto",
                           trend = "auto") %>%
            anomalize(remainder, alpha = input$alpha/100, max_anoms = input$max_anoms/100) %>%
            time_recompose() %>%
            plot_anomalies(time_recomposed = TRUE, alpha_circles = 0.8, size_dots = 1, size_circles = 1.6) +
            ggtitle(paste("alpha =", input$alpha, "%", "| Maximum % of anomaly =", input$max_anoms, "%", "\n")) +
            theme(plot.title = element_text(hjust = 0.5))
        
    })
    
    # Create anomal_result_table
    anomal_result_table <- reactive({
        
        df_tibble() %>%
            time_decompose(mean_interaction,
                           frequency = "auto",
                           trend = "auto") %>%
            anomalize(remainder, alpha = input$alpha/100, max_anoms = input$max_anoms/100) %>%
            time_recompose() %>%
            filter(anomaly == 'Yes') 
        
    })
    
    # data set for anomal date and trend figure of the date
    anomal_days <- reactive({
        
        anomal_result_table() %>% 
            select(date, trend)
        
    })
    
    # find anomal article headline and values
    anomal_article <- reactive({
        
        t1 <- left_join(article_raw %>%
                            filter(Topic == input$choosetopic_anomal &
                                       Source == input$choosesource_anomal &
                                       date %in% anomal_days()$date) %>%
                            select(date, Headline, Total), anomal_days())
        t1$trend <- round(t1$trend, 0)
        t1$'Total/trend' <- round((t1$Total/t1$trend), 1)
        return (t1)
    })
    
    ##############################################################################
    ## Time series anomaly detection plot 
    output$anomaly <- renderPlotly({
        input$ApplyButton
        isolate({
            anomal_result_plot()
        })
        
    })
    
    ## Time series anomaly detection table
    
    output$tablesummary <- renderDataTable({
        
        clickData <- event_data(event = "plotly_click")
        clicked_date <- as.Date(clickData[,c(3)])
        
        t1 <- left_join(article_raw %>%
                            filter(Topic == input$choosetopic_anomal &
                                       Source == input$choosesource_anomal &
                                       date %in% clicked_date) %>%
                            select(date, Headline, Total), anomal_days())
        t1$trend <- round(t1$trend, 0)
        t1$'Total/trend' <- round((t1$Total/t1$trend), 1)
        return (t1)
        
    })
    ##############################################################################
    ## Topics Through Time
    
    # Preprocessing for data set for Daily view
    topic_daily_reactive <- reactive({
        
        topic_daily_raw<-article_raw %>% 
            filter(Topic == input$choosetopic_vis & 
                       Source == input$choosesource_vis &
                       date >= input$dateRange_vis[1] & date <= input$dateRange_vis[2]) %>% 
            group_by(date) %>% summarise(count = n())
        
        
        if(input$compareplot==TRUE){  
            topic_daily_raw2 <- article_raw %>% 
                filter(Topic == input$choosetopic_vis2 & 
                           Source == input$choosesource_vis2 &
                           date >= input$dateRange_vis[1] & date <= input$dateRange_vis[2]) %>% 
                group_by(date) %>% summarise(count = n())
            
            
            b<-merge(topic_daily_raw,topic_daily_raw2,by = 'date',all=TRUE)
            b[is.na(b)]<-0
            
            fig <- plot_ly(b, 
                           x = ~date, y = ~count.x, 
                           name = "Plot 1",
                           type = "scatter",
                           mode = "lines") #lines+markers
            fig<-fig%>%add_trace(y = ~count.y, 
                                 name = "Plot 2",
                                 type = "scatter",
                                 mode = "lines")
            fig <- fig %>% layout(title = paste('The number of articles through year by', 
                                                '\n','Topic:', input$choosetopic_vis, 
                                                ' & ', 'Media:', input$choosesource_vis,'against',
                                                'Topic:', input$choosetopic_vis2, 
                                                ' & ', 'Media:', input$choosesource_vis2))
            
            
            
            return(fig)
        }else{    fig <- plot_ly(topic_daily_raw, 
                                 x = ~date, y = ~count, 
                                 name = "Plot 1",
                                 type = "scatter",
                                 mode = "lines")
        fig <- fig %>% layout(title = paste('The number of articles through year by', 
                                            '\n','Topic:', input$choosetopic_vis, 
                                            ' & ', 'Media:', input$choosesource_vis, '\n'))
        
        
        
        
        return(fig)
        }
    })
    
    # Preprocessing for data set for monthly view
    topic_monthly_reactive <- reactive({
        
        topic_monthly_raw <- article_raw %>% 
            filter(Topic == input$choosetopic_vis & 
                       Source == input$choosesource_vis &
                       date >= input$dateRange_vis[1] & date <= input$dateRange_vis[2]) %>% 
            group_by(month) %>% summarise(count = n())
        
        if(input$compareplot==TRUE){  
            topic_monthly_raw2 <- article_raw %>% 
                filter(Topic == input$choosetopic_vis2 & 
                           Source == input$choosesource_vis2 &
                           date >= input$dateRange_vis[1] & date <= input$dateRange_vis[2]) %>% 
                group_by(month) %>% summarise(count = n())
            
            
            g<-data.frame(topic_monthly_raw,topic_monthly_raw2)
            
            fig <- plot_ly(g, 
                           x = ~month, y = ~count, 
                           name = "Plot 1",
                           type = "scatter",
                           mode = "lines") #lines+markers
            fig<-fig%>%add_trace(y = ~count.1, 
                                 name = "Plot 2",
                                 type = "scatter",
                                 mode = "lines")
            fig <- fig %>% layout(title = paste('The number of articles through year by', 
                                                '\n','Topic:', input$choosetopic_vis, 
                                                ' & ', 'Media:', input$choosesource_vis,'against',
                                                'Topic:', input$choosetopic_vis2, 
                                                ' & ', 'Media:', input$choosesource_vis2))
            
            
            
            return(fig)
        }else{    fig <- plot_ly(topic_monthly_raw, 
                                 x = ~month, y = ~count, 
                                 name = "Plot 1",
                                 type = "scatter",
                                 mode = "lines")
        fig <- fig %>% layout(title = paste('The number of articles through year by', 
                                            '\n','Topic:', input$choosetopic_vis, 
                                            ' & ', 'Media:', input$choosesource_vis, '\n'))
        
        
        
        
        return(fig)
        }
    })
    
    
    
    # Create Topic result plot  
    output$topic_plot <- renderPlotly({
        
        input$ApplytopButton
        isolate({
            
            if(input$viewoption == "Daily") {
                topic_daily_reactive()
            } else {
                if(input$viewoption == "Monthly") {
                    topic_monthly_reactive()
                    
                }
            }
        })
        
    })
    
    
    ##############################################################################
    
}

shinyApp(ui, server)