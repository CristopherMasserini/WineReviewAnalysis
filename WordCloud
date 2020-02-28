# A function that takes a reviewer, or all, and point range
# Will create a word cloud of the descriptions of the wines that fit that criteria
ReviewerWordCloud <- function(reviewer, low_point, high_point) {
  library(wordcloud)
  library(tm)
  library(SnowballC)
  
  # Creates a table with only the reviewer named between the low and high points
  name = reviewer
  if (name != "All")
  {
    rows1 = which(wine$taster_name == name)
    wineTaste = wine[rows1,]
  }
  rows2 = which(low_point <= wineTaste$points)
  wineTaste = wineTaste[rows2,]
  rows3 = which(wineTaste$points <= high_point)
  wineTaste = wineTaste[rows3,]
  
  # Gets the descriptions and forms it into one big string
  num = nrow(wineTaste)
  all_descriptions = ""
  for (i in 1:num)
  {
    all_descriptions = paste(all_descriptions, wineTaste$description[i], sep = " ")
  }
  
  # Load the data as a corpus
  descript = VCorpus(VectorSource(all_descriptions))
  # Convert the text to lower case
  descript = tm_map(descript, content_transformer(tolower))
  # Remove numbers
  descript = tm_map(descript, removeNumbers)
  # Remove stopwords, common and custom
  descript = tm_map(descript, removeWords, stopwords("english"))
  descript = tm_map(descript, removeWords, c("wine", "region", "great", "age", "flavor", "year", "years", "drink", "many")) 
  # Remove punctuations
  descript = tm_map(descript, removePunctuation)
  
  dtm = TermDocumentMatrix(descript)
  m = as.matrix(dtm)
  v = sort(rowSums(m),decreasing=TRUE)
  d = data.frame(word = names(v),freq=v)
  
  # Saves the word cloud
  jpeg(file="wine_word_cloud.jpeg")
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  dev.off()
}
