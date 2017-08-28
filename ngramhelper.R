# N-gram helper to check performance of functions to calculate the better way to use the entire training dataset

path1 <- "C:/Public/Data-Science[JHU-Coursera]/project_nlp/"
foldernames <- c("DE", "US", "FI", "RU")
filenames_n<-character(0)
for (j in list.dirs(path = path1)) {
  if (length(unlist(strsplit(basename(j),"_")[[1]])) == 2) {
    for (k in list.files(path = j)) {
      if (grepl(".txt",k, perl = TRUE)) {
        filenames_n <- c(filenames_n, unlist(strsplit(k, "\\.txt"))[1]) 
      }
    }
  }
}

library(stringi)
library(strip)
library(dplyr)
library(data.table)
library(tidyr)
library(tidytext)

countern<<-0
lines<-NULL
filesize <- c()

for (j in list.dirs(path = path1)) {
  #print(unlist(strsplit(basename(j),"_")[[1]])[1])
  if ((length(unlist(strsplit(basename(j),"_")[[1]])) == 2) & (unlist(strsplit(basename(j),"_")[[1]])[1] == "en") ) {
    for (k in list.files(path = j)) {
      # Grep based on Categories and then read
      if (grepl(".txt", k, perl = TRUE)) {
        countern<<-countern+1
        pathtemp <- paste(j,k, sep = "/")
        print(pathtemp)
        filesize[countern]<-file.size(pathtemp)
        nam <- unlist(strip(strsplit(unlist(strip(strsplit(pathtemp, "/")))[8], "\\.")))[2]
        assign(nam, NULL)
        conntemp <- file(pathtemp, open = "r")
        lines <- readLines(conntemp)
        assign(nam , lines)
        close(conntemp) 
      }
    }
  }
}

blogs.dt <- as.data.table(blogs)
news.dt <- as.data.table(news)
twitter.dt <- as.data.table(twitter)

rm(lines, blogs, news, twitter)
gc()

blogs.dt$line <- c(1:blogs.dt[,.N])
news.dt$line <- c(1:news.dt[,.N])
twitter.dt$line <- c(1:twitter.dt[,.N])

blogs.train<-NULL
blogs.test<-NULL
news.train<-NULL
news.test<-NULL
twitter.train<-NULL
twitter.test<-NULL

blogs.train<-sample_frac(blogs.dt, 0.6)
blogs.test<-blogs.dt[-blogs.train$line,]

news.train<-sample_frac(news.dt,0.6)
news.test<-news.dt[-news.train$line,]

twitter.train<-sample_frac(twitter.dt,0.6)
twitter.test<-twitter.dt[-twitter.train$line,]


library(stringr)
library(data.table)
pattern <- "â???T|â???"|â???o|â???Tll|Ãª|â???|\\.|\\,|\\-|[[:punct:]]|~|@|[[:digit:]]|Â.|Â£|[^A-Za-z///' ]"
#gsub(pattern = pattern, "",x)
blogs.train[, 'blogs':=lapply(.SD, function(x){
  gsub(pattern = pattern, "", x)
}),.SDcols = c('blogs')]

head(news.train)
news.train[, 'news':=lapply(.SD, function(x){
  gsub(pattern = pattern, "", x)
}), .SDcols=c('news')]

head(twitter.train)
twitter.train[, 'twitter':=lapply(.SD, function(x){
  gsub(pattern = pattern, "", x)
}), .SDcols = c('twitter')]

rm(blogs.dt, news.dt, twitter.dt, tempstring)
gc()

# Now to use just the Blogs training data set to find the Tesvector predictions and see the fastest way in doing so

library(ngram)

blogs.train[, 'count':=lapply(.SD, function(x){
  sapply(strsplit(x, "\\s+"), length)
}), .SDcols=c('blogs')]

news.train[, 'count':=lapply(.SD, function(x){
  sapply(strsplit(x, "\\s+"), length)
}), .SDcols=c('news')]

twitter.train[, 'count':=lapply(.SD, function(x){
  sapply(strsplit(x, "\\s+"), length)
}), .SDcols=c('twitter')]

blogs.train.sub<-blogs.train[with(blogs.train, count>=4),]
l1.blogs <- length(blogs.train.sub$blogs)

news.train.sub<-news.train[with(news.train, count>=4),]
l1.news <- length(news.train.sub$news)

twitter.train.sub<-twitter.train[with(twitter.train, count>=4),]
l1.twitter <- length(twitter.train.sub$twitter)



# 1-gram
# Start the clock!

ptm <- proc.time()

library(tidytext)
library(dplyr)
blg_uni<-blogs.train %>% unnest_tokens(word, blogs) %>% 
  count(word, sort=TRUE)
blg_uni <- blg_uni %>% mutate(prob = n/sum(blg_uni$n))

news_uni<-news.train %>% unnest_tokens(word, news) %>%
  count(word, sort=TRUE)
news_uni<- news_uni %>% mutate(prob = n/sum(news_uni$n))

twit_uni<-twitter.train %>% unnest_tokens(word, twitter) %>%
  count(word, sort=TRUE)
twit_uni<- twit_uni %>% mutate(prob = n/sum(twit_uni$n))

# Stop the clock
proc.time() - ptm


#2-gram
# Start the clock
ptm <- proc.time()
blogs.train_bigrams<-NULL
news.train_bigrams<-NULL
twitter.train_bigrams<-NULL

blogs.train_bigrams <- blogs.train %>% unnest_tokens(bigram, blogs, token="ngrams", n=2) 
blogs.train_bigrams <- blogs.train_bigrams %>% count(bigram, sort=TRUE)

news.train_bigrams <- news.train %>% unnest_tokens(bigram, news, token="ngrams", n=2)
news.train_bigrams <- news.train_bigrams %>% count(bigram, sort=TRUE)

twitter.train_bigrams <- twitter.train %>% unnest_tokens(bigram, twitter, token="ngrams", n=2)
twitter.train_bigrams <- twitter.train_bigrams %>% count(bigram, sort=TRUE)

blogs.train_bigrams <- blogs.train_bigrams %>% mutate(prob = n/sum(blogs.train_bigrams$n))
news.train_bigrams <- news.train_bigrams %>% mutate(prob = n/sum(news.train_bigrams$n))
twitter.train_bigrams <- twitter.train_bigrams %>% mutate(prob=n/sum(news.train_bigrams$n))


blogs.train_bigrams[blogs.train_bigrams$bigram=="reduce your",]$n #32
news.train_bigrams[news.train_bigrams$bigram == "reduce your",]$n #1
twitter.train_bigrams[twitter.train_bigrams$bigram=="reduce your",]$n #18

blogs.train_bigrams[blogs.train_bigrams$bigram=="your stress",]$n #9
news.train_bigrams[news.train_bigrams$bigram == "your stress",]$n #0
twitter.train_bigrams[twitter.train_bigrams$bigram=="your stress",]$n #12


# Stop the clock
proc.time() - ptm

#3-gram
# Start the clock!
ptm <- proc.time()

res.ng3.dt1<-NULL
res.ng3.dt2<-NULL
res.ng3.dt3<-NULL
res.ng3.dt4<-NULL
res.ng3.dt5<-NULL

res.ng3.dt1<-as.data.table(get.phrasetable(ngram(blogs.train.sub[1:100000]$blogs,n=3)))
res.ng3.dt2<-as.data.table(get.phrasetable(ngram(blogs.train.sub[100001:200000]$blogs,n=3)))
res.ng3.dt3<-as.data.table(get.phrasetable(ngram(blogs.train.sub[200001:300000]$blogs,n=3)))
res.ng3.dt4<-as.data.table(get.phrasetable(ngram(blogs.train.sub[300001:400000]$blogs,n=3)))
res.ng3.dt5<-as.data.table(get.phrasetable(ngram(blogs.train.sub[400001:l1.blogs]$blogs,n=3)))

# Stop the clock
proc.time() - ptm

res.ng3.news.dt1<-as.data.table(get.phrasetable(ngram(news.train.sub$news,n=3)))
res.ng3.news.dt1[res.ng3.news.dt1$ngrams=="reduce your stress",]$freq


res.ng3.twit.dt1<-NULL
res.ng3.twit.dt2<-NULL
res.ng3.twit.dt3<-NULL
res.ng3.twit.dt4<-NULL
res.ng3.twit.dt5<-NULL
res.ng3.twit.dt6<-NULL
res.ng3.twit.dt7<-NULL
res.ng3.twit.dt8<-NULL
res.ng3.twit.dt9<-NULL
res.ng3.twit.dt10<-NULL
res.ng3.twit.dt11<-NULL

# Start the clock!
ptm <- proc.time()

res.ng3.twit.dt1<-as.data.table(get.phrasetable(ngram(twitter.train.sub[1:100000]$twitter,n=3)))
res.ng3.twit.dt2<-as.data.table(get.phrasetable(ngram(twitter.train.sub[100001:200000]$twitter,n=3)))
res.ng3.twit.dt3<-as.data.table(get.phrasetable(ngram(twitter.train.sub[200001:300000]$twitter,n=3)))
res.ng3.twit.dt4<-as.data.table(get.phrasetable(ngram(twitter.train.sub[300001:400000]$twitter,n=3)))
res.ng3.twit.dt5<-as.data.table(get.phrasetable(ngram(twitter.train.sub[400001:500000]$twitter,n=3)))
res.ng3.twit.dt6<-as.data.table(get.phrasetable(ngram(twitter.train.sub[500001:600000]$twitter,n=3)))
res.ng3.twit.dt7<-as.data.table(get.phrasetable(ngram(twitter.train.sub[600001:700000]$twitter,n=3)))
res.ng3.twit.dt8<-as.data.table(get.phrasetable(ngram(twitter.train.sub[700001:800000]$twitter,n=3)))
res.ng3.twit.dt9<-as.data.table(get.phrasetable(ngram(twitter.train.sub[800001:900000]$twitter,n=3)))
res.ng3.twit.dt10<-as.data.table(get.phrasetable(ngram(twitter.train.sub[900001:1000000]$twitter,n=3)))
res.ng3.twit.dt11<-as.data.table(get.phrasetable(ngram(twitter.train.sub[1000001:l1.twitter]$twitter,n=3)))

l = list(res.ng3.twit.dt1,
         res.ng3.twit.dt2,
         res.ng3.twit.dt3,
         res.ng3.twit.dt4,
         res.ng3.twit.dt5,
         res.ng3.twit.dt6,
         res.ng3.twit.dt7,
         res.ng3.twit.dt8,
         res.ng3.twit.dt9,
         res.ng3.twit.dt10,
         res.ng3.twit.dt11)

twit.DT<-rbindlist(l)

l = list(res.ng3.dt1,
         res.ng3.dt2,
         res.ng3.dt3,
         res.ng3.dt4,
         res.ng3.dt5)

blogs.DT<-rbindlist(l)


# Stop the clock
proc.time() - ptm

# Remove the spaces from the end
blogs.DT$ngrams<-sub('.{1}$','',blogs.DT$ngrams)
res.ng3.news.dt1$ngrams<-sub('.{1}$','',res.ng3.news.dt1$ngrams)


blg_uni[blg_uni$word=="stress",]$n
blogs.train_bigrams[blogs.train_bigrams$bigram=="your stress",]$n

#Remove the last space and we can compare

# Trigrams separated 
library(tidytext)
library(tidyr)
twit.DT.sep <- twit.DT %>% separate(ngrams, c("word1","word2","word3"), sep=" ")
write.csv(twit.DT.sep, file = "C:/Public/Data-Science[JHU-Coursera]/project_nlp/foo.csv",row.names=FALSE)
write.csv(twitter.train, file = "C:/Public/Data-Science[JHU-Coursera]/project_nlp/twitter-train.csv",row.names=FALSE)



rm(res.ng3.twit.dt1,res.ng3.twit.dt2,res.ng3.twit.dt3,res.ng3.twit.dt4,res.ng3.twit.dt5,
   res.ng3.twit.dt6,res.ng3.twit.dt7,res.ng3.twit.dt8,res.ng3.twit.dt9,res.ng3.twit.dt10,
   res.ng3.twit.dt11,res.ng3.dt1,res.ng3.dt2,res.ng3.dt3,res.ng3.dt4,res.ng3.dt5)
gc()

testvector <- c("Talking to your mom has the same effect as a hug and helps reduce your happiness",
                "Talking to your mom has the same effect as a hug and helps reduce your stress",
                "Talking to your mom has the same effect as a hug and helps reduce your hunger",
                "Talking to your mom has the same effect as a hug and helps reduce your sleepiness")