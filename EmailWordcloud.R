library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)
library(Rgraphviz) # Correlation plots.
library(RColorBrewer)
library(tm.plugin.mail)

getwd()
spam.path <- getwd() #working directory was set to files pane location - folder on my desktop spam.path is the folder with the emails
spam.path#emailz


###################################################### Wordcloud development

spam.docs <- dir(spam.path)
corpus <- spam.docs

control <- list(stopwords=TRUE,removePunctuation=TRUE,
                removeNumbers=TRUE,minDocFreq=1)

spam.corpus <- Corpus(VectorSource(spam.docs))
spam.tdm <- TermDocumentMatrix(spam.corpus,control)
spam.dtm <- DocumentTermMatrix(spam.corpus,control)


corpus <- tm_map(spam.corpus, stripWhitespace)
corpus <- tm_map(corpus,  content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(PlainTextDocument))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c("eml","pearlcoop")) #this removeWords function should remove many words, I chose to remove names
                                                            # "eml" appears a lot too, remove all outliers that do not appear pleasing

corpus <- tm_map(corpus, removeWords, c("23")) #even remove numbers

corpus <- Corpus(VectorSource(corpus))
corpus2 <- TermDocumentMatrix(corpus)
corpus3 <- removeSparseTerms(corpus2, .9) #take out spare terms
mcorpus <- as.matrix(corpus3)
vecorpus <- sort(rowSums(mcorpus),decreasing=TRUE)
dcorpus <- data.frame(word = names(vecorpus),freq=vecorpus)  ### Prepping the dataframe of frequent words
dfcorpus <- data.frame(head(dcorpus, 500)) #500 most frequent words, we will crop this down later
ddcorpus <- as.matrix(dfcorpus)
write.csv(ddcorpus, file = "FREQ1.csv")
# at this point I would go into excel and cherry pick which words/lines I dont want in the final product. 
#It is easier to see which ones than using removeWords function. Finally, truncate the final list to desired number of words, ~100-200

hundo <- read.table("FREQ1.csv", header = TRUE, sep = ",")

wordcloud(hundo$word, freq = hundo$freq, rot.per = 0.35,colors=brewer.pal(8, "Dark2"), random.order = FALSE, scale = c(8,.6))

################ Associated words

findAssocs(spam.tdm, "shit", .0) #use the TermDocumentMatrix to find associations
findAssocs(spam.tdm, "money", .0)
