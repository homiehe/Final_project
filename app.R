library(shiny)
library(shinydashboard)
library(tidytext)
library(tidyverse)
library(scales)
library(tm)
library(wordcloud)
library(ggthemes)
library(ggplot2)
library(reshape2)
library(knitr) # for dynamic reporting
# install.packages("kableExtra")
library(kableExtra) # create a nicely formated HTML table
# install.packages("formattable")
library(formattable) # for the color_tile function
library(lubridate)
# install.packages("ggrepel")
library(ggrepel) #`geom_label_repel`
# install.packages("gridExtra")
library(gridExtra) #`grid.arrange()` for multi-graphs
# install.packages("circlize")
library(circlize) #Visualizations - chord diagram
# install.packages("memery")
library(memery) #Memes - images with plots
# install.packages("magick")
library(magick) #Memes - images with plots (image_read)
# install.packages("yarrr")
library(yarrr)  #Pirate plot
# install.packages("radarchart")
library(radarchart) #Visualizations
# install.packages("igraph")
library(igraph) #ngram network diagrams
# install.packages("ggraph")
library(ggraph) #ngram network diagrams
# Define UI for application that draws a histogram
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")
comment_new <- read.csv("comment_new.csv",stringsAsFactors = FALSE)
token_new <- read.csv("token_new.csv", stringsAsFactors = FALSE)
comment_new_t <- comment_new %>%
  mutate(pop = 
           ifelse(comment_new$likeCount > 200, "vp", 
                  ifelse(comment_new$likeCount > 50, "p",
                         ifelse(comment_new$likeCount > 1, "good","normal"))))

theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}

my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

grid.col = c("period 1" = my_colors[1], "period 2" = my_colors[2], "period 3" = my_colors[3], "period 4" = my_colors[4], "period 5" = my_colors[5], "popular" = "grey", "good" = "grey") #assign chord colors

AA <- read.csv("AA.csv")

token_mood <- token_new %>% 
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

ui <- dashboardPage(
  dashboardHeader(title = 'Comment Analysis on Missile Strike'),
  
  dashboardSidebar(
    sidebarMenu(menuItem("Introduction", icon = icon("address-card"), tabName = "1"),
                
                menuItem("Text Analysis",icon = icon("cogs"), tabName = "2",
                         menuSubItem("Likecount Analysis", tabName = "2_1"),
                         menuSubItem("Top liked comment", tabName = "2_2"),
                         menuSubItem("Top Liked Word Frequency", tabName = "2_3"),
                         menuSubItem("Pirate Chart", tabName = "2_4"),
                         menuSubItem("Word Length Distribution", tabName = "2_5"),
                         menuSubItem("Lexical Diversity", tabName = "2_6")
                ),
                
                menuItem("Sentiment Analysis",icon = icon("laptop"), tabName = "3",
                         menuSubItem("Word Cloud", tabName = "3_1"),
                         menuSubItem("Polar Sentiment Analysis", tabName = "3_2"),
                         menuSubItem("Mood Ring", tabName = "3_3"),
                         menuSubItem("Nrc analysis", tabName = "3_4")
                )
    )
  ),
  
  dashboardBody(
    tabItems(

      ###### tab 1      
      tabItem(tabName = "1",
              fluidRow(
                column(width = 6,
                       box(title = "Youtube Comment on Analysis Missile Strikes in 2018 and in 2014", status = "danger", width = NULL,
                           helpText("  This project crawl all the Youtube video with comments more than 500 on US Missile Strike against Syria in 2014 and 2018."),
                           hr(),         
                           helpText("  A Text analysis with word frequency, likeCount analysis are done to show a rough picture of the study in the general analysis."),
                           hr(),
                           helpText("  Then, we do sentiment analysis to see the difference in people's emotion when they express their opinion on missile strike videos.")
                       )
                ),
                column(width = 6, 
                       box(status = "danger", width = NULL,
                           img(src='1.JPG',width="100%")
                       )
                )
              )
      ),
      
      
      ##### tab 2      
      tabItem(tabName = "2_1",
              fluidRow(
                column(width = 3,
                       box(title = "Likecount Analysis",
                           status = "danger", width = NULL,
                           selectInput("year",
                                       "Year:",
                                       c(2014,2018)),
                           helpText("Select a year you want"),
                           sliderInput("max", 
                                       label = "Time Period:",
                                       min = 1, max = 5, value = c(1,5)),
                           helpText("Select the range of time period you want"),
                           helpText("We use comments of 20 days from the day after the release of the video, divide them into 5 periods (1, 2, 3, 4, 5)"),
                           hr(),
                           helpText("1. very popular : likeCount > 200"),
                           helpText("2. popular : likeCount > 50"),
                           helpText("3. good : likeCount > 1"),
                           helpText("4. normal : likeCount = 0")
                       )
                ),
                column(width = 9,
                       tabBox(
                         id="tableset2",
                         tabPanel("Likecount Timeseries",
                                  status="success",
                                  plotOutput("graph1")
                         )
                       )
                )
              )     
      ),
      
      ##### tab 3
      tabItem(tabName = "2_2",
              fluidRow(
                column(width = 5,
                       box(title = "Top Liked Comments",
                           status = "danger", width = NULL,
                           selectInput("year2",
                                       "Year:",
                                       c(2014,2018)),
                           helpText("Select a year you want"),
                           sliderInput("max2", 
                                       label = "Minimum amount of likecounts:",
                                       min = 100, max = 500, value = c(400)),
                           helpText("Select the number of likecount you want")
                       )
                ),
                column(width = 12,
                       tabBox(
                         id="tableset3",
                         tabPanel(
                           "Comments Table",
                           tableOutput("kable1")
                           
                         )
                       )
                )
              )     
      ),
      
      ##### tab 4
      tabItem(tabName = "2_3",
              fluidRow(
                column(width = 5,
                       box(title = "Top Liked Word Frequency",
                           status = "danger", width = NULL,
                           selectInput("year3",
                                       "Year:",
                                       c(2014,2018)),
                           helpText("Select a year you want")
                       )
                ),
                column(width = 12,
                       tabBox(
                         id="tableset4",
                         tabPanel("Word Frequency",
                                  status="success",
                                  plotOutput("graph2")
                         )
                       )
                )     
              )
      ),
      
      ##### tab 5
      tabItem(tabName = "2_4",
              fluidRow(
                column(width = 5,
                       box(title = "Relationship between period and LikeCount",
                           status = "danger", width = NULL,
                           selectInput("year4",
                                       "Year:",
                                       c(2014,2018)),
                           helpText("Select a year you want")
                       )
                ),
                column(width = 12,
                       tabBox(
                         id="tableset5",
                         tabPanel("Pirate Chart",
                                  status="success",
                                  plotOutput("graph3")
                         )
                       )
                )     
              )
      ),
      
      ##### tab 6
      tabItem(tabName = "2_5",
              fluidRow(
                column(width = 5,
                       box(title = "Word Length Distribution",
                           status = "danger", width = NULL,
                           selectInput("year5",
                                       "Year:",
                                       c(2014,2018)),
                           helpText("Select a year you want"),
                           selectInput("pop",
                                       "Popularity:",
                                       c("popular","very popular","good","normal")),
                           helpText("Select the popularity shown by likeCount you want"),
                           helpText("1. very popular : likeCount > 200"),
                           helpText("2. popular : likeCount > 50"),
                           helpText("3. good : likeCount > 1"),
                           helpText("4. normal : likeCount = 0")
                       )
                ),
                column(width = 12,
                       tabBox(
                         id="tableset6",
                         tabPanel("Word Length Distribution",
                                  status="success",
                                  plotOutput("graph4")
                         )
                       )
                )     
              )
      ),
      
      ##### tab 7
      tabItem(tabName = "2_6",
              fluidRow(
                column(width = 5,
                       box(title = "Lexical Diversity",
                           status = "danger", width = NULL,
                           selectInput("year6",
                                       "Year:",
                                       c(2014,2018)),
                           helpText("Select a year you want"),
                           sliderInput("max3", 
                                       label = "Time Period:",
                                       min = 1, max = 5, value = c(1,5)),
                           helpText("Select the range of time period you want")
                       )
                ),
                column(width = 12,
                       tabBox(
                         id="tableset7",
                         tabPanel("Lexical Diversity",
                                  status="success",
                                  plotOutput("graph5")
                         )
                       )
                )     
              )
      ),
      
      ###### tab 8
      tabItem(tabName = "3_1",
              fluidRow(
                column(width = 5,
                       box(title = "Sentiment Word Cloud",
                           status = "danger", width = NULL,
                           selectInput("year7",
                                       "Year:",
                                       c(2014,2018)),
                           helpText("Select a year you want"),
                           helpText("Blue for negative word, Orange for positive word")
                       )
                ),
                column(width = 12,
                       tabBox(
                         id="tableset7",
                         tabPanel("Sentiment Word Cloud",
                                  status="success",
                                  plotOutput("graph6")
                         )
                       )
                )     
              )
      ),
      
      ###### tab 9
      tabItem(tabName = "3_2",
              fluidRow(
                column(width = 5,
                       box(title = "Polar Sentiment Analysis",
                           status = "danger", width = NULL,
                           selectInput("year8",
                                       "Year:",
                                       c(2014,2018)),
                           helpText("Select a year you want"),
                           sliderInput("max4", 
                                       label = "Time Period:",
                                       min = 1, max = 20, value = c(1,20)),
                           helpText("Select the range of time period you want"),
                           helpText("Blue line represents the overall regression trend of bing sentiment, pink line is the smooth line that represents every periods' change. ")
                       )
                ),
                column(width = 12,
                       tabBox(
                         id="tableset7",
                         tabPanel("Polar Sentiment Analysis",
                                  status="success",
                                  plotOutput("graph7")
                         )
                       )
                )     
              )
      ),
      
      ##### tab 10
      tabItem(tabName = "3_3",
              fluidRow(
                column(width = 5,
                       box(title = "Mood Ring",
                           status = "danger", width = NULL,
                           selectInput("year9",
                                       "Year:",
                                       c(2014,2018)),
                           helpText("Select a year you want"),
                           helpText("Bottom for time period"),
                           helpText("Top for mood")
                       )
                ),
                column(width = 12,
                       tabBox(
                         id="tableset7",
                         tabPanel("Mood Ring",
                                  status="success",
                                  plotOutput("graph8")
                         )
                       )
                )     
              )
      ),
      
      ##### tab 11
      tabItem(tabName = "3_4",
              fluidRow(
                column(width = 5,
                       box(title = "Nrc analysis",
                           status = "danger", width = NULL,
                           selectInput("year10",
                                       "Year:",
                                       c(2014,2018)),
                           helpText("Select a year you want")
                       )
                ),
                column(width = 12,
                       tabBox(
                         id="tableset7",
                         tabPanel("Nrc analysis",
                                  status="success",
                                  plotOutput("graph9")
                         )
                       )
                )     
              )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  a <- reactive({
    filter(comment_new, year == input$year) %>% 
      filter(time >= input$max[1] & time <= input$max[2]) %>% 
      group_by(time, popularity) %>%
      summarise(number_of_comment = n())
  })
  
  b <- reactive({
    filter(comment_new, year == input$year2) %>% 
      filter(likeCount > input$max2) 
  })
  
  
  c <- reactive({
    token_new %>%
      filter(year == input$year3) %>% 
      group_by(popularity) %>%
      count(word, popularity, sort = TRUE) %>%
      slice(seq_len(8)) %>%
      ungroup() %>%
      arrange(popularity,n) %>%
      mutate(row = row_number())
  })
  
  d <- reactive({
    comment_new_t %>% 
      filter(year == input$year4) %>% 
      count(time, pop)
  })
  
  e <- reactive({
    comment_new %>%
      filter(year == input$year5) %>% 
      filter(popularity %in% input$pop) %>% 
      unnest_tokens(word,textOriginal) %>%
      group_by(X,date) %>%
      distinct() %>%
      mutate(word_length = nchar(word)) 
  })
  
  f <- reactive({
    comment_new %>%
      filter(year == input$year6) %>% 
      filter(time >= input$max3[1] & time <= input$max3[2]) %>% 
      unnest_tokens(word,textOriginal) %>%
      group_by(X,time) %>%
      summarise(lex_diversity = n_distinct(word)) %>%
      arrange(desc(lex_diversity)) 
  })
  
  g <- reactive({
    token_new %>%
      filter(year== input$year7) %>% 
      filter(word != "trump") %>% 
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) 
  })
  
  h <- reactive({
    AA %>%
      filter(year == input$year8) %>% 
      filter(date >= input$max4[1] & date <= input$max4[2]) %>% 
      count(sentiment, date) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(polarity = positive - negative,
             percent_positive = positive / (positive + negative) * 100)
  })
  
  i <- reactive({
    token_mood %>%
      filter(year == input$year9) %>% 
      count(sentiment, time) %>%
      group_by(time, sentiment) %>%
      summarise(sentiment_sum = sum(n)) %>%
      ungroup()
  })
  
  j <- reactive({
    token_new %>%
      filter(year == input$year10) %>% 
      mutate(linenumber = row_number()) %>% 
      inner_join(get_sentiments("bing")) %>%
      count(year, index = linenumber %/% 80, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
  })
  
  output$graph1 <- renderPlot({
    a() %>%
      ggplot() + 
      geom_bar(aes(x = time, y = number_of_comment, 
                   fill = popularity), stat = "identity")  +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_blank(),
            panel.grid.minor = element_blank()) +
      ggtitle(paste("Comments in", input$year , sep="")) +
      labs(x = "Time Period", y = "Comment Count") +
      ylim(0,1200)
  })
  
  output$kable1 <- function() {
    b() %>%
      select(year, Text = textOriginal, likeCount) %>%
      arrange(year) %>%
      mutate(year = color_tile("lightblue", "lightgreen")(year)) %>%
      mutate(likeCount = color_tile("lightgreen", "lightgreen")(likeCount)) %>%
      kable("html", escape = FALSE, align = "c", caption = "Comments with Top likeCount") #%>%
    # kable_styling(bootstrap_options = 
    #                 c("striped", "condensed", "bordered"), 
    #               full_width = FALSE)
  }
  
  output$graph2 <- renderPlot({
    c() %>% 
      ggplot(aes(row, n, fill = popularity)) +
      geom_col(show.legend = NULL) +
      labs(x = NULL, y = "Comment Count") +
      ggtitle(paste("Popular Words by likeCount in ", input$year3, sep="")) +
      facet_wrap(~popularity, scales = "free") + 
      scale_x_continuous(  # This handles replacement of row
        breaks = c()$row, # notice need to reuse data frame
        labels = c()$word) + 
      coord_flip()
  })
  
  output$graph3 <- renderPlot({
    circos.clear() #Very important - Reset the circular layout parameters!
    chordDiagram(d(), grid.col = grid.col, transparency = .2)
    title(paste("Relationship Between LikeCount and Time in ",input$year4,sep=""))
  })
  
  output$graph4 <- renderPlot({
    e() %>%
      count(word_length, sort = TRUE) %>%
      ggplot(aes(word_length), 
             binwidth = 10) + 
      geom_histogram(aes(fill = ..count..),
                     breaks = seq(1,25, by = 2), 
                     show.legend = FALSE) + 
      xlab("Word Length") + 
      ylab("Word Count") +
      ggtitle(paste("Word Length Distribution of Comments in ",input$year5,sep="")) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.minor = element_blank())
  })
  
  output$graph5 <- renderPlot({
    f() %>%
      ggplot(aes(time, lex_diversity)) +
      geom_point(color = my_colors[3],
                 alpha = .4, 
                 size = 4, 
                 position = "jitter") + 
      geom_smooth(aes(x = time, y = lex_diversity), se = FALSE,
                  color = "blue", lwd = 0.5, method = "loess") +
      ggtitle(paste("Lexical Diversity of Comments in ", input$year6 , sep="")) +
      xlab("Days after the Release of Video") + 
      ylab("Number of the Unique Word") +
      scale_color_manual(values = my_colors) +
      theme_classic()
  })
  
  
  output$graph6 <- renderPlot({
      g() %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(max.words = 100)
  })
  
  output$graph7 <- renderPlot({
    h() %>%
      ggplot(aes(date, polarity, color = ifelse(polarity >= 0,my_colors[5],my_colors[4]))) +
      geom_col() +
      geom_smooth(method = "loess", se = FALSE) +
      geom_smooth(method = "lm", se = FALSE, aes(color = my_colors[1])) +
      theme_lyrics() + theme(plot.title = element_text(size = 11)) +
      xlab(NULL) + ylab(NULL) +
      ggtitle(paste("Polarity Over Time in ", input$year8 , sep=""))
  })
  
  output$graph8 <- renderPlot({
    circos.clear() #Very important - Reset the circular layout parameters!
    chordDiagram(i(), grid.col = grid.col, transparency = .2)
    title(paste("Relationship Between Mood and Time Period in ",input$year4,sep=""))
  })
  
  output$graph9 <- renderPlot({
    ggplot(j(), aes(index, sentiment, fill = year)) +
      geom_col(show.legend = FALSE)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

