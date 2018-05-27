library(ggplot2)
library(shiny)
library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)

#  youtube <- read.csv("USVideos.csv", stringsAsFactors = F)
# 
#  bland <- function(x) {
#    replacement <- x %>%
#      str_replace_all("Ã€", "A") %>%
#      str_replace_all("Ã‚", "A") %>%
#      str_replace_all("Ãƒ", "A") %>%
#      str_replace_all("Ã„", "A") %>%
#      str_replace_all("Ã…", "A") %>%
#      str_replace_all("Ã†", "AE") %>%
#      str_replace_all("Ã‡", "C") %>%
#      str_replace_all("Ãˆ", "E") %>%
#      str_replace_all("Ã‰", "E") %>%
#      str_replace_all("ÃŠ", "E") %>%
#      str_replace_all("Ã‹", "E") %>%
#      str_replace_all("ÃŒ", "I") %>%
#      str_replace_all("ÃŽ", "I") %>%
#      str_replace_all("Ã‘", "N") %>%
#      str_replace_all("Ã’", "O") %>%
#      str_replace_all("Ã“", "O") %>%
#      str_replace_all("Ã”", "O") %>%
#      str_replace_all("Ã•", "O") %>%
#      str_replace_all("Ã–", "O") %>%
#      str_replace_all("Ã—", "X") %>%
#      str_replace_all("Ã˜", "0") %>%
#      str_replace_all("Ã™", "U") %>%
#      str_replace_all("Ãš", "U") %>%
#      str_replace_all("Ã›", "U") %>%
#      str_replace_all("Ãœ", "U") %>%
#      str_replace_all("Ãž", "D") %>%
#      str_replace_all("ÃŸ", "B") %>%
#      str_replace_all("Ã¡", "A") %>%
#      str_replace_all("Ã¢", "A") %>%
#      str_replace_all("Ã£", "A") %>%
#      str_replace_all("Ã¤", "A") %>%
#      str_replace_all("Ã¥", "A") %>%
#      str_replace_all("Ã¦", "AE") %>%
#      str_replace_all("Ã§", "C") %>%
#      str_replace_all("Ã¨", "E") %>%
#      str_replace_all("Ã©", "E") %>%
#      str_replace_all("Ãª", "E") %>%
#      str_replace_all("Ã«", "E") %>%
#      str_replace_all("Ã¬", "I") %>%
#      str_replace_all("Ã®", "I") %>%
#      str_replace_all("Ã¯", "I") %>%
#      str_replace_all("Ã°", "O") %>%
#      str_replace_all("Ã±", "N") %>%
#      str_replace_all("Ã²", "O") %>%
#      str_replace_all("Ã³", "O") %>%
#      str_replace_all("Ã´", "O") %>%
#      str_replace_all("Ãµ", "O") %>%
#      str_replace_all("Ã¶", "O") %>%
#      str_replace_all("Ã·", "/") %>%
#      str_replace_all("Ã¸", "0") %>%
#      str_replace_all("Ã¹", "U") %>%
#      str_replace_all("Ãº", "U") %>%
#      str_replace_all("Ã»", "U") %>%
#      str_replace_all("Ã¼", "U") %>%
#      str_replace_all("Ã½", "Y") %>%
#      str_replace_all("Ã¾", "P") %>%
#      str_replace_all("Ã¿", "Y") %>%
#      str_replace_all("Å¸", "Y") %>%
#      str_replace_all("Å¾", "Z") %>%
#      str_replace_all("Å“", "CE") %>%
#      str_replace_all("Å", "S") %>%
#      str_replace_all("Å½", "Z") %>%
#      str_replace_all("Æ’", "F") %>%
#      str_replace_all("ã€", "\\'") %>%
#      str_replace_all("â€™", "\\'") %>%
#      str_replace_all("â€", "\\'") %>%
#      str_replace_all("â€˜", "\\'") %>%
#      str_replace_all("â€“", "-") 
#      str_replace_all("\\'s", "") %>%
#      str_replace_all("\\'S", "") %>%
#      str_replace_all("\\'", "") %>%
#      str_replace_all("[^[:alnum:]]", " ") %>%
#      str_replace_all("\\s+", " ") %>%
#      toupper()
#  }
# 
# # Create vector of all words
#  list_title <- unlist(lapply(youtube$title, bland))
# 
#  title_df <- data.frame(list_title)
# 
#  write.csv(title_df, "TitleList.csv", row.names = F)
###########################################
###########################################
###########################################
# list_all <- str_split(list_title, " ") %>%
#   unlist()
# # 
# # # Create vector of unique words
#  list_unique <- unique(list_all)
#  
#  df_u <- data.frame(list_unique, stringsAsFactors = F)
# 
#  colnames(df_u)[1] <- "word"
# 
#  count_strings <- function(x) {
#    sum(str_count(list_all, x))
#  }
# 
#  df_u <- mutate(df_u, number = lapply(word, count_strings))
#  
#  df_u <- filter(df_u, word != " ")
# 
# x <- vapply(df_u$number, length, 1L)
# df_u <- df_u[rep(rownames(df_u), x), ]
# df_u$number <- unlist(df_u$number, use.names = FALSE)
# 
# write.csv(df_u, file = "WordListFromTitles.csv", row.names = F)
##########################################################################
##########################################################################
##########################################################################
df_u <- read.csv("WordListFromTitles.csv", stringsAsFactors = F, header = T)
df_u <- filter(df_u, word != c(" ", "  "))
df_t <- read.csv("TitleList.csv", stringsAsFactors = F)

colnames(df_u)[1] <- "word"
colnames(df_u)[2] <- "number"

df_u <- filter(df_u, word != " ")

bing_sentiments <- get_sentiments("bing")
bing_sentiments$word <- toupper(bing_sentiments$word) 

df_bing <- inner_join(bing_sentiments, df_u, by = "word")

plain_words <- c("THE", "THEIR", "THEY", "THEYRE", "YOUR", "YOU" , "A", "AN", "IS", "ISNT", "WILL",
                 "WONT", "DID", "DIDNT", "HAVE", "HAD", "WHEN", "WHERE", "HOW", "WHAT", "wHY", 
                 "HAVENT", "NOT", "SHOULD", "WOULD", "COULD", "BE", "BEING", "GET", "HADNT", "WE",
                 "THIS", "THERE", "IN", "MY", "TO", "AS")

df_c <- filter(df_u, word != plain_words)

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
      df_main <- df_c
    } else {
      df_main <- df_u
    }
    df_main <- top_n(df_main, n = 10, wt = number)
    df_main
  })
  
  factorize <- reactive({
    df_main <- decipher()
    df_main$word <- factor(decipher()$word)
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
      lvls <- levels(factor()$word)
      string <- lvls[round(input$my_click$y)]
      
      df_samp <- filter[df_t == string, ]
      t_samp <- df_samp[sample(nrow(df_samp), 3), ]
      
      string
      cat(paste0(string, "\nNumber = ", subset(decipher(), word == string)[, "number"], 
                 "\n", t_samp[1, 1],
                 "\n", t_samp[2, 1],
                 "\n", t_samp[3, 1]))
    }
  })
}


shinyApp(my_ui, my_server)

