library(ggplot2)
library(shiny)
library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)

#youtube <- read.csv("USVideos.csv", stringsAsFactors = F)

#bland <- function(x) {
#  replacement <- x %>% 
#    str_replace_all("Ã©", "E") %>% 
#    str_replace_all("Ãº", "U") %>%
#    str_replace_all("Ã¬", "I") %>% 
#    str_replace_all("Ã¡", "A") %>% 
#    str_replace_all("Ã§", "C") %>% 
#    str_replace_all("ã€", "\\'") %>% 
#    str_replace_all("â€™", "\\'") %>% 
#    str_replace_all("â€", "\\'") %>% 
#    str_replace_all("\\'s", "") %>% 
#    str_replace_all("\\'S", "") %>% 
#    str_replace_all("[^[:alnum:] ]", " ") %>% 
#    str_replace_all("\\s+", " ") %>% 
#    toupper()
#}

# Create vector of all words
#list_title <- unlist(lapply(youtube$title, bland))

#title_df <- data.frame(list_title)

#write.csv(title_df, "TitleList.csv", row.names = F)

###########################################
###########################################
###########################################
#list_all <- str_split(list_title, " ") %>% 
#  unlist()

# Create vector of unique words
#list_unique <- unique(list_all)

#df_u <- data.frame(list_unique, stringsAsFactors = F)

#colnames(df_u)[1] <- "word"

#count_strings <- function(x) {
#  sum(str_count(list_all, x))   
#}

#df_u <- mutate(df_u, number = lapply(word, count_strings))

#x <- vapply(df_u$Number, length, 1L)       
#df_u <- df_u[rep(rownames(df_u), x), ]   
#df_u$Number <- unlist(df_u$Number, use.names = FALSE)

#write.csv(df_u, file = "WordListFromTitles.csv", row.names = F)
##########################################################################
##########################################################################
##########################################################################
df_u <- read.csv("WordListFromTitles.csv", stringsAsFactors = F, header = T)
title_df <- read.csv("TitleList.csv", stringsAsFactors = F)

colnames(df_u)[1] <- "word"
colnames(df_u)[2] <- "number"

df_u <- filter(df_u, word != " ")

bing_sentiments <- get_sentiments("bing")
bing_sentiments$word <- toupper(bing_sentiments$word) 

df_bing <- inner_join(bing_sentiments, df_u, by = "word")

plain_words <- c("THE", "THEIR", "THEY", "THEYRE", "YOUR", "YOU" , "A", "AN", "IS", "ISNT", "WILL",
                 "WONT", "DID", "DIDNT", "HAVE", "HAD", "WHEN", "WHERE", "HOW", "WHAT", "wHY", 
                 "HAVENT", "NOT", "SHOULD", "WOULD", "COULD", "BE", "BEING", "GET", "HADNT", "WE",
                 "THIS", "THERE")

my_ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("choice",  label = strong("Word Searches"), 
                  choices = list("Default" = "default",
                                 "Positive" = "positive",
                                 "Negative" = "negative",
                                 "All" = "all"))
    ),
    mainPanel(
      plotOutput('wordBar', click = "my_click"),
      verbatimTextOutput('wordText')
    )
  )
)

my_server <- function(input, output) {
  
  decipher <- reactive({
    if(input$choice == "positive" | input$choice == "negative") {
      df_main <- df_bing %>% 
        filter(sentiment == input$choice)
    } else if (input$choice == "default") {
      df_main <- df_u %>% 
        filter(word != plain_words) 
    } else {
      df_main <- df_u
    }
    df_main <- top_n(df_main, n = 10, wt = desc(number))
    df_main$word <- factor(df_main$word)
    df_main
  })
  
  
  output$wordBar <- renderPlot({
    ggplot(data = decipher()) +
      geom_bar(aes(x = word, y = number), stat = "identity", fill = "red") +
      theme(axis.text = element_text(size = 5)) +
      coord_flip() +
      labs(title = "TITLE", x = "Word", y = "Number")
  })
  
  output$wordText <- renderPrint({
    if (is.null(input$my_click$y)) {
      return("")
    } else {
      lvls <- levels(decipher()$word)
      string <- lvls[round(input$my_click$y)]
      
      title_samp <- filter[title_df == string, ]
      title_sample <- title_samp[sample(nrow(title_samp), 3), ]
      
      string
      cat(paste0(string, "\nNumber = ", df_u["word" == string, "number"], 
                 "\n", title_sample[1, 1],
                 "\n", title_sample[2, 1],
                 "\n", title_sample[3, 1]))
    }
  })
}


shinyApp(my_ui, my_server)

