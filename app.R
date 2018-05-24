library(ggplot2)
library(shiny)
library(stringr)
library(dplyr)

youtube <- read.csv("USVideos.csv", stringsAsFactors = F)

youtube_titles <- youtube %>% select(title, category_id, likes, dislikes)

# We don't replace $ because we think $ can be a significant title choice
bland <- function(x) {
  replacement <- str_replace_all(toupper(x), "([.,?*:#/\\\\<>!\\{\\}\\[\\]\\(\\)\\|])", "")
  replacement <- str_replace_all(replacement, "Â€™", "\\'")
  replacement <- str_replace_all(replacement, "Â€Œ", "\\'")
  replacement <- str_replace_all(replacement, "Â€", "\\'")
  replacement <- str_replace_all(replacement, "\\'S", "")
  replacement
}

#uniquechars <- function(x) {
 # unique(strsplit(x, "")[[1]])
#}

# Mutates the titles into a format we can use to compare
youtube_titles <- mutate(youtube_titles, bland_title = lapply(title, bland))

# Create a vector of our newly formatted youtube titles
list_t <- unlist(youtube_titles$bland_title)

# Create a vector of every single word
list_split <- unlist(str_split(list_t, " "))

# Find all special characters 
all_char <- paste0(list_split, collapse = " ") 

spec_char <- gsub("[^[:alnum:] ]", "", all_char)

list_all <- unlist(str_split(spec_char, " "))

list_unique <- unique(list_all)

df_u <- data.frame(list_unique, stringsAsFactors = F)

colnames(df_u)[1] <- "uniques"

#MAKE SURE TO ACCOUNT FOR ' AGAIN

count_strings <- function(x) {
  sum(str_count(list_all, x))   
}

number = lapply(list_unique, count_strings)

##########################################################################
##########################################################################
##########################################################################

ggplot(data = measurement()) +
  geom_bar(aes(x = name, y = rate, fill = "lightblue"), stat = "identity", fill = "lightblue") +
  theme(axis.text = element_text(size = 12)) +
  coord_flip() +
  labs(title = paste("Top Titles ", input$year),
       x = "State", y = x_title())