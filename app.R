library(ggplot2)
library(shiny)
library(stringr)
library(dplyr)

youtube <- read.csv("USVideos.csv", stringsAsFactors = F)

youtube_titles <- youtube %>% select(title, category_id, likes, dislikes)

# We don't replace $ because we think $ can be a significant title choice
bland <- function(x) {
  replacement <- str_replace_all(x, "L.A.", "LA")
  replacement <- str_replace_all(replacement, "([.,?*:#/\\\\<\\'>!\\{\\}\\[\\]\\(\\)\\|])", " ")
  all_up <- toupper(replacement)
  all_up
}

# Mutates the titles into a format we can use to compare
youtube_titles <- mutate(youtube_titles, bland_title = lapply(title, bland))

# Create a vector of our newly formatted youtube titles
list_t <- unlist(youtube_titles$bland_title)

# Create a vector of every single word
list_split <- unlist(str_split(list_t, " "))


  spec_char <- gsub("[QWERTYUIOPASDFGHJKLZXCVBNM1230456789$-]", "", list_split)
  spec_char <- spec_char[spec_char != ""]


# Create a vector of unique words
list_unique <- unique(list_split)

df_u <- data.frame(list_unique, stringsAsFactors = F)

colnames(df_u)[1] <- "uniques"

count_strings <- function(x) {
  sum(str_count(list_split, x))   
}

df_u <- mutate(df_u, number = lapply(uniques, count_strings))
