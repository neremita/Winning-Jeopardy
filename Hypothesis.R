'''
Title: Winning Jeopardy

Intro: In this project, we take an analysis to determine how likely it is to win Jeopardy!, the trivia TV show.
'''

'''
1. Getting To Know Jeopardy Data
'''

library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)

df <- read.csv('~/Google Drive/Work/Data Analyst in R/Guided Projects/Winning Jeopardy/JEOPARDY_CSV.csv',
               stringsAsFactors = FALSE)

#head(df, 5)
#Looks as if the there are 7 columns in the data frame: Show Number, Episode Air Date, Round, Category, Value, Question, and Answer.

colnames(df) <- c('show_number', 'air_date', 'round', 'category', 'value', 'question', 'answer')

#for (i in 1:ncol(df)) {
#  print(typeof(df[1,i]))
#}

#It looks like the first column contains integers while the other 6 are characters.

'''
2. Fixing Data Types
'''

#Next up, we should convert the value column into numbers instead of character types and delete the 'None'-valued rows.

df <- df %>%
  filter(value != 'None') %>%
  mutate(value = as.numeric(str_replace_all(value, '[$,]', '')))

'''
3. Normalizing Text
'''

#Time to normalize the main text columns of the data frame.

pattern <- "([!\\#$%&'()*+,./:;<=>?@\\[\\]^_‘{|}~£])+"

df <- df %>%
  mutate(question = tolower(str_replace_all(question, pattern, ''))) %>%
  mutate(question = str_replace_all(question, '"', '')) %>%
  mutate(question = str_replace_all(question, '\\-', ' ')) %>%
  mutate(answer = tolower(str_replace_all(answer, pattern, ''))) %>%
  mutate(answer = str_replace_all(answer, '"', '' )) %>%
  mutate(answer = str_replace_all(answer, '\\-', ' ')) %>%
  mutate(category = tolower(str_replace_all(category, pattern, ''))) %>%
  mutate(category = str_replace_all(category, '"', '')) %>%
  mutate(category = str_replace_all(category, '\\-', ' ')) 

'''
4. Making Dates More Accessible
'''

#Now, we will convert the air_date variable into year, month, and day using separate and make it numeric.

df <- df %>%
  separate(air_date, c('year', 'month', 'day'), sep = '[\\-]+', convert = TRUE)

'''
5. Focusing On Particular Subject Areas
'''

#Now let's see how likely we can have the categories science, history, and Shakespeare pop up.

p_category_expected <-   1/3369 
p_not_category_expected <- 3368/3369 
p_expected <- c(p_category_expected, p_not_category_expected)

n_science = 0
n_history = 0
n_shakespeare = 0
range = nrow(df)

for (i in 1:nrow(df)) {
  if (str_detect(df$category[i], 'science')) {
    n_science = n_science + 1
  } else if (str_detect(df$category[i], 'history')) {
    n_history = n_history + 1
  } else if (str_detect(df$category[i], 'shakespeare')) {
    n_shakespeare = n_shakespeare + 1
  } else {
    
  }
}

n_science_total = c(n_science, range - n_science)
n_history_total = c(n_history, range - n_history)
n_shakespeare_total = c(n_shakespeare, range - n_shakespeare)

chisq.test(n_science_total, p = p_expected)
chisq.test(n_history_total, p = p_expected)
chisq.test(n_shakespeare_total, p = p_expected)

#Each chi-squared test resulted in a p-value less than 0.05, meaning we can reject the null hypothesis.

'''
6. Unique Terms In Questions
'''

#Now it is time to see if there are any duplicate questions being asked. We will be focusing on any word with 6 characters or more to see if there is a match within the dataset.

df <- df %>%
  arrange(show_number)

terms_used <- c() #vector used to list all the words with 6 or more characters.

for (a in 1:nrow(df)) {
  str <- str_split(df$question[a], " ", simplify = TRUE)
  for (i in 1:length(str)) {
    if (str_length(str[i]) >= 6) {
      terms_used <- c(terms_used, str[i])
    } else {
    
    }
  }
}

terms_used <- unique(terms_used)

'''
7. Terms In Low and High Value Questions
'''

#We are now going to test a hypothesis: Given that there a value of $800 and higher is considered high in value and lower than $800 low, we expect that each term will have a 2:3 high-to-low
#ratio in how often that occur in the question poll, allowing us to only study only particular topics instead.

#The null hypothesis, therefore, would be to see if there is no definite distribution with the questions on hand.

probability  <- data.frame(term = NULL, high_value = NULL, low_value = NULL, p_value = NULL)
colnames(probability) <- c('term', 'high_value', 'low_value', 'p_value')

high_ratio <- 2/5
low_ratio <- 3/5

for (b in 1:length(terms_used)) {
  high_value = 0
  low_value = 0
  for (a in 1:nrow(df)) {
    str <- str_split(df$question[a], " ", simplify = TRUE)
    for (i in 1:length(str)) {
      if (str[i] == terms_used[b]) {
        if (df$round[a] == 'Jeopardy!' & df$year[a] < 2001) {
          if (df$value[a] >= 400) {
          high_value = high_value + 1
          } else {
            low_value = low_value + 1
          }
        } else if (df$round[a] == 'Jeopardy!' & df$year[a] >= 2001) {
          if (df$value[a] >= 800) {
            high_value = high_value + 1
          } else {
            low_value = low_value + 1
          }
        } else if (df$round[a] == 'Double Jeopardy!' & df$year[a] < 2001) {
          if (df$value[a] >= 800) {
            high_value = high_value + 1
          } else {
            low_value = low_value + 1
          }
        } else if (df$round[a] == 'Double Jeopardy!' & df$year[a] >= 2001) {
          if (df$value[a] >= 1600) {
            high_value = high_value + 1
          } else {
            low_value = low_value + 1
          }
        } else {
        
        }
      }
    }
  }
  total <- high_value + low_value
  test_stat <- (high_value/total + high_ratio)^2/high_ratio + (low_value/total + low_ratio)^2/low_ratio
  p_value <- -(pchisq(test_stat, df = 1)) + 1
  row <- data.frame(terms_used[b], high_value, low_value, p_value)
  probability <- rbind(probability, row)
  if (b%%100 == 0) {
    print(b)
  } else {
    
  }
}
#We concluded that, based on the information in the table, if you were study these particular topics,
#you would have a very fair shot at winning Jeopardy! Granted, some of these topics are not completely
#relevant to any thing or are spelling errors from cleaning, but that can be fixed with inspection, say
#using a dictionary to sort out the odd words.
