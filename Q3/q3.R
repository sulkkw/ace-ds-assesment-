# clean up the environment before starting
rm(list = ls())
library(slam)
library(tm)
library(SnowballC)
library(ggplot2)
library(reshape2)

#Q1 
my_data <- read.delim("ace.txt")

#text pre-processing
clean <- function(x){
  
  x <-tolower(x)
  
  x <-removePunctuation(x)
  
  x <-removeNumbers(x)
  
  x <-stripWhitespace(x)
  
  
  return(x) }


#Q1 
#read the file 
data <- read.delim("ace.txt")

#clean the text
data <- clean(data)

#create corpus
myCorpus <- Corpus(VectorSource(data))

#create term document matrix
tdm_data <- TermDocumentMatrix(myCorpus)

#convert to dataframe
tdm_data <- as.data.frame(as.matrix(tdm_data))

#tranpose the data
tdm_data <- t(tdm_data)

#convert to dataframe
tdm_data <- as.data.frame(tdm_data)

#rename the column
rownames(tdm_data) <- c("Freq")

#calculate probability
prob <- (tdm_data$data / rowSums(tdm_data)) * 100 

#check the probability
cat("The probability of word data occuring in each line is: ", prob) #6.938776


#Q2

#Reshape data
clean_df <- as.data.frame(rows= row.names(tdm_data), stack(tdm_data))

#rename the column
colnames(clean_df) <- c("freq", "variables")

#Histogram
ggplot(clean_df, aes(x=variables, y=freq)) + geom_histogram(stat="identity") + theme_minimal() 


#Q3
#calculate probability of word analytics occuring after word data 
prob_2 <- (clean_df[clean_df$variables == "analytics",1] / clean_df[clean_df$variables == "data",1]) * 100

#check the probability
cat("The probability of word analytics occuring after data is: ", prob_2)
