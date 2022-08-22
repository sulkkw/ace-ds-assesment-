# clean up the environment before starting
rm(list = ls())
library(slam)
library(tm)
library(SnowballC)
library(ggplot2)

#Q1 
my_data <- read.delim("ace.txt")

#text pre-processing
clean <- function(x){
  
  x <-tolower(x)
  
  x <-removePunctuation(x)
  
  x <-removeNumbers(x)
  
  x <-stripWhitespace(x)
  
  
  return(x) }

#clean the text
clean_txt <- clean(my_data)

#split each words
clean_txt <- strsplit(paste(unlist(clean_txt), collapse = " "), ' ')[[1]]
#convert into table
clean_txt <- table(clean_txt)
#convert into data frame
text_df <- as.data.frame(clean_txt)
#calculate probability of data occurring
prob <- (text_df[text_df$clean_txt == 'data',2] / sum(text_df$Freq)) * 100

cat("The probability of word data occuring in each line is: ", prob) #5.7


#Q2
#change column name 
colnames(text_df) <- c("words", "freq")
#histogram visualization
ggplot(data=text_df, aes(x=words, y=freq))  + geom_histogram(stat="identity") + theme_minimal() 

#Q3
#probability calculation
prob2 <- (text_df[text_df$words == 'analytics',2] / text_df[text_df$words == 'data',2]) * 100
cat("The probability of word analytics occuring after data is: ", prob2)
