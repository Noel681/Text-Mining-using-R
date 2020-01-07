cd = read.csv("G:/Amazon_Unlocked_Mobile.csv")

a = cd[c("Brand.Name","Reviews")]

##################Loading Libraries################
library(stringr)
library(tm)
library(wordcloud)
library(slam)
library(sentimentr)

###########################Applying Preprocessing techniques in Text-Mining##################################

#Delete the leading spaces

a$Reviews = str_trim(a$Reviews)

#Select only text columns
 
a=data.frame(a[1:1000,2])

names(a)= "Reviews" #we need to change the column name in df 'a'
                    # as we took a sample from'a'foreasy processs , the column name is changed to default name

#as we are dealing with chracter type of data its better to change the class of our datatype

a$Reviews = as.character(a$Reviews)

#convert comments into corpus

postCorpus = Corpus(VectorSource(a$Reviews))

writeLines(as.character(postCorpus[[55]]))# here writelines function is used to extract comment which is convereted into Corpus
                                          # corpus is collection of documents

#ostCorpus = tm_map(postCorpus, PlainTextDocument)

#case folding
postCorpus = tm_map(postCorpus, tolower)

#remove stop words
postCorpus = tm_map(postCorpus, removeWords, stopwords('english'))

#remove punctuation marks
postCorpus = tm_map(postCorpus, removePunctuation)

#remove numbers
postCorpus = tm_map(postCorpus, removeNumbers)

#remove unnecesary spaces
postCorpus = tm_map(postCorpus, stripWhitespace)

#convert into plain text
 postCorpus = tm_map(postCorpus, PlainTextDocument)
# 
 #create corpus
 postCorpus = Corpus(VectorSource(postCorpus))

 
#################################### Term_Document_Matrix ####################################
 
 #Build document term matrix
 tdm = TermDocumentMatrix(postCorpus)
 
 #tdm_min = TermDocumentMatrix(postCorpus, control=list(weighting=weightTfIdf, minWordLength=4, minDocFreq=10)) #we can also use this syntax for creating Term_Document matrix (which is called IF-IDF method)
                                                                                                                #here minimum word length is 4 as given in syntax & minfrequency of word appearing is 10
 #Convert term document matrix into dataframe
 TDM_data = as.data.frame(t(as.matrix(tdm))) 
 
 ##calculate the terms frequency
 words_freq = rollup(tdm, 2, na.rm=TRUE, FUN = sum) # here we used 2 as we are dealing with columns 
                                                    #na.rm=TRUE , remove na in table , FUN = sum , to sum all repeated words (we can also perform avg,count)
 #Convert into matrix
 words_freq = as.matrix(words_freq)
 
 #Convert to proper dataframe
 words_freq = data.frame(words_freq)
 
 #Convert row.names into index
 words_freq$words = row.names(words_freq)
 row.names(words_freq) = NULL
 words_freq = words_freq[,c(2,1)]
 names(words_freq) = c("Words", "Frequency")
 
 #Most frequent terms which appears in atleast 100 times
 findFreqTerms(tdm, 30)
 
 ##wordcloud
 postCorpus_WC = postCorpus
 
 pal2 = brewer.pal(8,"Dark2")
 png("wordcloud_v2.png", width = 12, height = 8, units = 'in', res = 300)
 wordcloud(postCorpus_WC, scale = c(5,.2), min.freq = 30, max.words = 150, random.order = FALSE, rot.per = .15, colors = pal2)
 dev.off()
 
 #Remove the defined stop words
 postCorpus_WC = tm_map(postCorpus_WC, removeWords, c('will', 'also', 'can',
                                                      stopwords('english')))
 postCorpus_WC = tm_map(postCorpus_WC, removeWords, stop_words$StopWords)
 
############################################Sentimental Analysis###############################
 
 #sentiment Analysis
 #Another method
 library(RSentiment)
 #post = data.frame(post[1:200,])
 
 a=data.frame(a[1:50,2])
 names(post) = 'comments'
 
 df = calculate_sentiment(a$Reviews) # caluculate_sentiment function removes punctuations ,keywords and buld sentimental analysis on it
 
 #Another method
 #Install sentiment library using binay source
 #install.packages("E:/Others/Edwisor/ContentRevamp/Advanced Predictive Analytics/Sentiment Analysis/sentiment_0.2.tar.gz", repos = NULL, type="source")
 library(sentiment)
 
 #classifying the corpus as negative and positive and neutral
 polarity = classify_polarity(a$Reviews, algorithm = "bayes", verbose = TRUE) #here we are using Naivebayes algoritham
                                                                              #verbose = TRUE says that to display output in Rconsole i.e processing of sentimental analysis
 polarity = data.frame(polarity)
 
 #Attached sentiments to the comments
 newdocs = cbind(a, polarity) #ultimate sentiment analysis
 
 #Pie chart to visualise polarity
 df = data.frame(table(newdocs$BEST_FIT))
 
 #Interactive visualisations using plotly
 library(plotly)
 
 plot_ly(df, labels = ~Var1, values = ~Freq, type = 'pie') %>%            #%>% is used as seperator
         layout(title = 'Sentiment Analysis',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
 
 
 
 
  