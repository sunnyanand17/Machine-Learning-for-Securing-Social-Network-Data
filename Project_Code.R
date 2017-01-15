library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)

getwd()

key="key"
secret="secret key"
accessToken="access token"
accessSecret="access secret"
setwd("Your working directory")

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="C:/Users/Arpan_Agrawal/Documents/cacert.pem",
              method="auto")
authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret,accessToken,accessSecret)
save(authenticate, file="twitter authentication.Rdata")


#####################
N=2000  # tweets to request from each query
S=250  # radius in miles
#Lets create the latitude vector
lats_vec=c(38.9,40.7,37.8,39,36)
#The Longitude vector
lons_vec=c(-77,-74,-122,-105.5, -97)

#cities=DC,New York,San Fransisco,Colorado,Dallas

search_res=do.call(rbind,lapply(1:length(lats_vec), function(i) searchTwitter('Hillary+DOnald',
                                                                      lang="en",n=N,resultType="recent",
                                                                      geocode=paste(lats_vec[i],lons_vec[i],paste0(S,"mi"),sep=","))))

############################
search_reslat=sapply(search_res, function(x) as.numeric(x$getLatitude()))
search_reslat=sapply(search_reslat, function(z) ifelse(length(z)==0,NA,z))  
search_reslat<-rep(lats_vec, times = 1, each = 100)
search_reslat<-as.list(search_reslat)

search_reslon=sapply(search_res, function(x) as.numeric(x$getLongitude()))
search_reslon=sapply(search_reslon, function(z) ifelse(length(z)==0,NA,z))  
search_reslon<-rep(lons_vec, times = 1, each = 100)
search_reslon<-as.list(search_reslon)

search_resdate=lapply(search_res, function(x) x$getCreated())
search_resdate=sapply(search_resdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

search_restext=sapply(search_res, function(x) x$getText())
search_restext=unlist(search_restext)

isretweet=sapply(search_res, function(x) x$getIsRetweet())
retweeted=sapply(search_res, function(x) x$getRetweeted())
retweetcount=sapply(search_res, function(x) x$getRetweetCount())

favoritecount=sapply(search_res, function(x) x$getFavoriteCount())
favorited=sapply(search_res, function(x) x$getFavorited())

data=as.data.frame(cbind(tweet=search_restext,date=search_resdate,lat=search_reslat,lon=search_reslon,
                         isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))

################################
# Create corpus
corpus=Corpus(VectorSource(data$tweet))
# Convert to lower-case
#corpus <- tm_map(corpus, content_transformer(tolower))
#corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,tolower)

# Remove stopwords

corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

col=brewer.pal(8,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

#########################Getting address of tweets#######
data=filter(data, !is.na(lat),!is.na(lon))
lonlat=select(data,lon,lat)
result <- do.call(rbind, lapply(1:nrow(lonlat),
                                function(i) revgeocode(as.vector(c(as.double(lonlat[i,1]),as.double(lonlat[i,2]))))))

#from each of the data rows find tha adress,city,stzip,zipcode,state,data2
data2=lapply(result,  function(x) unlist(strsplit(x,",")))
address=sapply(data2,function(x) paste(x[1:3],collapse=''))
city=sapply(data2,function(x) x[2])
stzip=sapply(data2,function(x) x[3])
zipcode = as.numeric(str_extract(stzip,"[0-9]{5}"))   
state=str_extract(stzip,"[:alpha:]{2}")
data2=as.data.frame(list(address=address,city=city,zipcode=zipcode,state=state))

#Separate the tweets out for analyzing if the tweets are positive or neagtive
data=cbind(data,data2)
tweet=data$tweet
tweet_list=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweet_list=lapply(tweet, function(x) gsub("htt.*",' ',x))
tweet=unlist(tweet_list)
data$tweet=tweet

#Get the list of positive and negative words so we can compare each word and 
#calculate the score based on the index position of each word in the tweet
positives= readLines("C:/Users/sunny anand/Documents/positive-words.txt")
negatives = readLines("C:/Users/sunny anand/Documents/negative-words.txt")

#Initilaize a word to be True or False
positive_matches=as.list(c("TRUE"))
negative_matches=as.list(c("FALSE"))

#function to determine the sentiment scores based on these positive and negative
#words
sentiment_scores = function(tweets, positive_words, negative_words, .progress='none'){
  
  scores = laply(tweets,
                 function(tweet, positive_words, negative_words){
                   tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
                   tweet = gsub("[[:cntrl:]]", "", tweet)   # remove control characters
                   tweet = gsub('\\d+', '', tweet)          # remove digits
                   
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   tweet = sapply(tweet, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word_list = str_split(tweet, "\\s+")
                   #print(word_list)
                   words = unlist(word_list)
                   #print(class(as.list(words)))
                   words=as.list(words)
                   #print(positive.matches)
                   # compare words to the dictionaries of positive & negative terms
                   positive.matches = match(words, positive_words)
                   print(positive.matches)
                   negative.matches = match(words, negative_words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive_matches = !is.na(positive_matches)
                   negative_matches = !is.na(negative_matches)
                   # final score
                   score = sum(positive_matches) - sum(negative_matches)
                   return(score)
                 }, positive_matches, negative_matches, .progress=.progress )
  return(scores)
}
match(as.list(c("bad","bad","happy")), negatives)
score = sentiment_scores(tweet, positives, negatives, .progress='text')
score = rep(score, times = 50, each = 1)
data$score=score

#Plot a histogram of the sentiment score:
hist(score,xlab=" ",main="Sentiment of sample tweets that have Donald Trump in them ",
     border="black",col="skyblue")

