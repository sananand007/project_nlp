# N-gram helper to check performance of functions to calculate the better way to use the entire training dataset

ngramhelper<-function(text) {
  
  testvector <- text
  
  blogs <- readRDS(file = "./data/blogs.rds")
  news <- readRDS(file = "./data/news.rds")
  twitter <- readRDS(file = "./data/twitter.rds")
  
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
  
  blogs.train<-sample_frac(blogs.dt, 0.00005)
  blogs.test<-blogs.dt[-blogs.train$line,]
  
  news.train<-sample_frac(news.dt,0.00005)
  news.test<-news.dt[-news.train$line,]
  
  twitter.train<-sample_frac(twitter.dt,0.00005)
  twitter.test<-twitter.dt[-twitter.train$line,]
  
  library(stringr)
  library(data.table)
  pattern <- "Ã|\\.|\\,|\\-|[[:punct:]]|[[:digit:]]|[^A-Za-z///' ]|[^[:alpha:] ']"
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
  
  res.ng3.dt1<-as.data.table(get.phrasetable(ngram(blogs.train.sub[1:l1.blogs]$blogs,n=3)))
  #res.ng3.dt2<-as.data.table(get.phrasetable(ngram(blogs.train.sub[100001:200000]$blogs,n=3)))
  #res.ng3.dt3<-as.data.table(get.phrasetable(ngram(blogs.train.sub[200001:300000]$blogs,n=3)))
  #res.ng3.dt4<-as.data.table(get.phrasetable(ngram(blogs.train.sub[300001:400000]$blogs,n=3)))
  #res.ng3.dt5<-as.data.table(get.phrasetable(ngram(blogs.train.sub[400001:l1.blogs]$blogs,n=3)))
  # l = list(res.ng3.dt1,
  #          res.ng3.dt2,
  #          res.ng3.dt3,
  #          res.ng3.dt4,
  #          res.ng3.dt5)
  l = list(res.ng3.dt1)
  blogs.ng3.DT<-rbindlist(l) #Blogs 3-gram DT
  news.ng3.DT<-as.data.table(get.phrasetable(ngram(news.train.sub$news,n=3))) #News 3-gram DT
  
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
  
  
  res.ng3.twit.dt1<-as.data.table(get.phrasetable(ngram(twitter.train.sub[1:l1.twitter]$twitter,n=3)))
  # res.ng3.twit.dt2<-as.data.table(get.phrasetable(ngram(twitter.train.sub[100001:200000]$twitter,n=3)))
  # res.ng3.twit.dt3<-as.data.table(get.phrasetable(ngram(twitter.train.sub[200001:300000]$twitter,n=3)))
  # res.ng3.twit.dt4<-as.data.table(get.phrasetable(ngram(twitter.train.sub[300001:400000]$twitter,n=3)))
  # res.ng3.twit.dt5<-as.data.table(get.phrasetable(ngram(twitter.train.sub[400001:500000]$twitter,n=3)))
  # res.ng3.twit.dt6<-as.data.table(get.phrasetable(ngram(twitter.train.sub[500001:600000]$twitter,n=3)))
  # res.ng3.twit.dt7<-as.data.table(get.phrasetable(ngram(twitter.train.sub[600001:700000]$twitter,n=3)))
  # res.ng3.twit.dt8<-as.data.table(get.phrasetable(ngram(twitter.train.sub[700001:800000]$twitter,n=3)))
  # res.ng3.twit.dt9<-as.data.table(get.phrasetable(ngram(twitter.train.sub[800001:900000]$twitter,n=3)))
  # res.ng3.twit.dt10<-as.data.table(get.phrasetable(ngram(twitter.train.sub[900001:1000000]$twitter,n=3)))
  # res.ng3.twit.dt11<-as.data.table(get.phrasetable(ngram(twitter.train.sub[1000001:l1.twitter]$twitter,n=3)))
  
  # l = list(res.ng3.twit.dt1,
  #          res.ng3.twit.dt2,
  #          res.ng3.twit.dt3,
  #          res.ng3.twit.dt4,
  #          res.ng3.twit.dt5,
  #          res.ng3.twit.dt6,
  #          res.ng3.twit.dt7,
  #          res.ng3.twit.dt8,
  #          res.ng3.twit.dt9,
  #          res.ng3.twit.dt10,
  #          res.ng3.twit.dt11)
  l = list(res.ng3.twit.dt1)
  twit.ng3.DT<-rbindlist(l) #Twitter 3-gram DT
  
  rm(res.ng3.twit.dt1,res.ng3.twit.dt2,res.ng3.twit.dt3,res.ng3.twit.dt4,res.ng3.twit.dt5,
     res.ng3.twit.dt6,res.ng3.twit.dt7,res.ng3.twit.dt8,res.ng3.twit.dt9,res.ng3.twit.dt10,
     res.ng3.twit.dt11,res.ng3.dt1,res.ng3.dt2,res.ng3.dt3,res.ng3.dt4,res.ng3.dt5)
  gc()
  
  # Stop the clock
  proc.time() - ptm
  
  # Remove the spaces from the end
  blogs.ng3.DT$ngrams<-sub('.{1}$','',blogs.ng3.DT$ngrams)
  news.ng3.DT$ngrams<-sub('.{1}$','',news.ng3.DT$ngrams)
  twit.ng3.DT$ngrams<-sub('.{1}$','',twit.ng3.DT$ngrams)
  
  
  # Debug-Present Here
  
  getObsTrigs<-function(bigPre, trig1, trig2, trig3){
    trigs.win1A <- data.frame(ngrams=vector(mode = 'character', length = 0), freq = vector(mode = 'integer', length = 0))
    trigs.win2A <- data.frame(ngrams=vector(mode = 'character', length = 0), freq = vector(mode = 'integer', length = 0))
    trigs.win3A <- data.frame(ngrams=vector(mode = 'character', length = 0), freq = vector(mode = 'integer', length = 0))
    regex <- sprintf("%s%s%s", "^", bigPre, " ")
    trigram_indices1 <- grep(regex, trig1$ngrams)
    trigram_indices2 <- grep(regex, trig2$ngrams)
    trigram_indices3 <- grep(regex, trig3$ngrams)
    if(length(trigram_indices1) > 0) {
      trigs.win1A <- trig1[trigram_indices1, ][,c("ngrams","freq")]
    }
    if(length(trigram_indices2) > 0) {
      trigs.win2A <- trig2[trigram_indices2, ][,c("ngrams","freq")]
    }
    if(length(trigram_indices3) > 0) {
      trigs.win3A <- trig3[trigram_indices3, ][,c("ngrams","freq")]
    }
    
    trigs.winA<-rbind(trigs.win1A, trigs.win2A, trigs.win3A)
    
    return(trigs.winA)
  }
  
  
  getObsTriProbs<-function(obsTrigs, bg1, bg2, bg3, bigPre, trigdisc=0.5){
    if (nrow(obsTrigs)<1) return(NULL)
    bigrs <- rbind(bg1,bg2,bg3)
    obsCount <- filter(bigrs, bigram==bigPre)$n[1]
    #print(obsCount)
    obsTrigProbs <- mutate(obsTrigs, freq=((freq-trigdisc)/obsCount))
    colnames(obsTrigProbs) <- c('ngram', 'prob')
    
    return(obsTrigProbs)
  }
  
  #Find unobserved trigram tail words
  
  getUnobsTrigTails <- function(qbo_obs_trg, unigs) {
    #unigs <- rbind(unig1, unig2, unig3)
    obs_trig_tails <- str_split_fixed(qbo_obs_trg," ",3)[,3]
    unobs_trig_tails <- unigs[!(unigs$word %in% obs_trig_tails),]$word
    return(unobs_trig_tails)
  }
  
  
  #Calculate discounted probability mass at the bigram level
  getAlphaBigram  <- function(unigram, bigrams, bigdisc=0.5) {
    #bigrams <- rbind(bg1, bg2, bg3)
    regex <- sprintf("%s%s%s", "^", unigram$word[1], " ")
    bigsThatStartWithUnig <- bigrams[grep(regex, bigrams$bigram),]
    if(nrow(bigsThatStartWithUnig) < 1) return(0)
    alphaBi <- 1 - (sum(bigsThatStartWithUnig$n - bigdisc) / sum(unigram$n))
    
    return(alphaBi)
  }
  
  getBoBigrams <- function(bigPre, unobsTrigTails) {
    w_i_minus1 <- str_split(bigPre, " ")[[1]][2]
    boBigrams <- paste(w_i_minus1, unobsTrigTails, sep = " ")
    return(boBigrams)
  }
  
  getObsBoBigrams <- function(bigPre, unobsTrigTails, bigrs) {
    boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
    obs_bo_bigrams <- bigrs[bigrs$bigram %in% boBigrams, ]
    return(obs_bo_bigrams)
  }
  
  getUnobsBoBigrams <- function(bigPre, unobsTrigTails, obsBoBigram) {
    boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
    unobs_bigs <- boBigrams[!(boBigrams %in% obsBoBigram$bigram)]
    return(unobs_bigs)
  }
  
  getObsBigProbs <- function(obsBoBigrams, unigs, bigDisc=0.5) {
    first_words <- str_split_fixed(obsBoBigrams$bigram, " ", 2)[, 1]
    first_word_freqs <- unigs[unigs$word %in% first_words, ]
    obsBigProbs <- (obsBoBigrams$n - bigDisc) / sum(first_word_freqs$n)
    obsBigProbs <- data.frame(ngram=obsBoBigrams$bigram, prob=obsBigProbs)
    
    return(obsBigProbs)
  }
  
  getQboUnobsBigrams <- function(unobsBoBigrams, unigs, alphaBig) {
    # get the unobserved bigram tails
    qboUnobsBigs <- str_split_fixed(unobsBoBigrams, " ", 2)[, 2]
    w_in_Aw_iminus1 <- unigs[!(unigs$word %in% qboUnobsBigs), ]
    # convert to data.frame with counts
    qboUnobsBigs <- unigs[unigs$word %in% qboUnobsBigs, ]
    denom <- sum(qboUnobsBigs$n)
    # converts counts to probabilities
    qboUnobsBigs <- data.frame(ngram=unobsBoBigrams,
                               prob=(alphaBig * qboUnobsBigs$n / denom))
    
    return(qboUnobsBigs)
  }
  
  # Calculate discounted probability mass at the trigram level 
  getAlphaTrigram <- function(obsTrigs, bigram, triDisc=0.5) {
    if(nrow(obsTrigs) < 1) return(1)
    alphaTri <- 1 - sum((obsTrigs$freq - triDisc) / sum(bigram$n))
    
    return(alphaTri)
  }
  
  
  #Calculate unobserved trigram probabilities qBO
  
  getUnobsTriProbs <- function(bigPre, qboObsBigrams,
                               qboUnobsBigrams, alphaTrig) {
    qboBigrams <- rbind(qboObsBigrams, qboUnobsBigrams)
    qboBigrams <- qboBigrams[order(-qboBigrams$prob), ]
    sumQboBigs <- sum(qboBigrams$prob)
    first_bigPre_word <- str_split(bigPre, " ")[[1]][1]
    unobsTrigNgrams <- paste(first_bigPre_word, qboBigrams$ngram, sep=" ")
    unobsTrigProbs <- alphaTrig * qboBigrams$prob / sumQboBigs
    unobsTrigDf <- data.frame(ngram=unobsTrigNgrams, prob=unobsTrigProbs)
    
    return(unobsTrigDf)
  }
  
  #Select wiwi with the highest
  
  getPredictionMsg <- function(qbo_trigs) {
    # pull off tail word of highest prob trigram
    prediction <- str_split(qbo_trigs$ngram[1], " ")[[1]][3]
    result <- sprintf("%s%s%s%.4f", "highest prob prediction is >>> ", prediction,
                      " <<< which has probability = ", qbo_trigs$prob[1])
    return(prediction)
  }
  
  # Calculate Probabilities of Words Completing Observed Trigrams
  pattern2 = "[[:punct:]]|\\,|\\'"
  helperfunc1<-function(gamma2, gamma3, testvector, bg1, bg2, bg3, unig1, unig2, unig3) {
    unigs <- rbind(unig1, unig2, unig3)
    bigrs <- rbind(bg1, bg2, bg3)
    alpha_big <- c()
    indx <- 0
    qbo_obs_trigrams_fin = data.frame(ngram=vector(mode = 'character', length = 0), prob = vector(mode = 'numeric', length = 0))
    for (i in 1:length(testvector)) {
      testvector[i]<-gsub(pattern = pattern2, "", testvector[i])
      res1<-unlist(strip(strsplit(tolower(testvector[i])," ")))
      #print(length(res1))
      for (j in 2:length(res1)-1){
        
        bigPre <- paste(res1[j], res1[j+1], sep = " ")
        #print(bigPre)
        res.df1<-getObsTrigs(bigPre = bigPre, blogs.ng3.DT, news.ng3.DT, twit.ng3.DT) # get trigrams and counts
        
        # convert counts to probabilities
        qbo_obs_trigrams <- getObsTriProbs(obsTrigs = res.df1, blogs.train_bigrams, news.train_bigrams, twitter.train_bigrams, bigPre = bigPre,gamma3)
        
        
        unobs_trig_tails <- getUnobsTrigTails(qbo_obs_trg = qbo_obs_trigrams, unigs)
        
        # Get the final qbo_obs_trig df
        qbo_obs_trigrams_fin <- rbind(qbo_obs_trigrams_fin,qbo_obs_trigrams)
        
        
        # calculating Probability mass at the bigram level
        unig <- str_split(bigPre, " ")[[1]][2]
        unig <- unigs[unigs$word==unig,]
        #print(unig)
        alpha_big <- getAlphaBigram(unig, bigrs, bigdisc = gamma2) # construct a vector of this
        #print(alpha_big)
        #count<-count+1
        
        
        
        #Step 4. iii. Calculate backed off probabilities qBOqBO for bigrams
        #The code below implements equation 10. to calculate qBO
        #for observed bigrams and equation 16. for unobserved bigrams:
        bo_bigrams<-getBoBigrams(bigPre, unobs_trig_tails)  # get backed off bigrams
        
        # separate bigrams which use eqn 10 and those that use 16
        obs_bo_bigrams <- getObsBoBigrams(bigPre, unobs_trig_tails, bigrs)
        unobs_bo_bigrams <- getUnobsBoBigrams(bigPre, unobs_trig_tails, obs_bo_bigrams)
        
        # calc obs'd bigram prob's from eqn 10
        qbo_obs_bigrams <- getObsBigProbs(obs_bo_bigrams, unigs, gamma2) #ngram     probs
        
        # calc alpha_big & unobs'd bigram prob's from eqn 16             #the_house 0.3125
        unig <- str_split(bigPre, " ")[[1]][2]
        unig <- unigs[unigs$word == unig,]
        
        # distrib discounted bigram prob mass to unobs bigrams in prop to unigram ML
        qbo_unobs_bigrams <- getQboUnobsBigrams(unobs_bo_bigrams, unigs, alpha_big)
        qbo_bigrams <- rbind(qbo_obs_bigrams, qbo_unobs_bigrams)
        #print(head(qbo_bigrams,10))
        
        bigram <- bigrs[bigrs$bigram %in% bigPre, ]
        
        alpha_trig <- getAlphaTrigram(obsTrigs = res.df1, bigram, gamma3)
        #print(alpha_trig)
        
        qbo_unobs_trigrams <- getUnobsTriProbs(bigPre, qbo_obs_bigrams,
                                               qbo_unobs_bigrams, alpha_trig)
        #print(head(qbo_unobs_trigrams))
        
        qbo_trigrams <- rbind(qbo_obs_trigrams, qbo_unobs_trigrams)
        qbo_trigrams <- qbo_trigrams[order(-qbo_trigrams$prob), ]  # sort by desc prob
        out_msg <- getPredictionMsg(qbo_trigrams)
        #print(out_msg)
        
      }
    }
    return(out_msg)
  }
  
  #testvector <- c("When you breathe")
  #testvector <- c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd eat")
  
  
  # Get the discounts here for KBO . 
  gamma2 = 0.44 #Bigram discount
  gamma3 = 0.55 #Trigram discount
  
  
  #rm(qbo_obs_trg, unobs_trig_tails)
  
  output<-helperfunc1(gamma2 = gamma2, gamma3 = gamma3, testvector = testvector, 
                      bg1 = blogs.train_bigrams, bg2 = news.train_bigrams,bg3 = twitter.train_bigrams, 
                      unig1 = blg_uni, unig2 = news_uni, unig3 = twit_uni)
  
  
  return(output)
  
  
}



#twit.ng3.DT[twit.ng3.DT$ngrams=="reduce your stress",]$freq

# # Probability function for checking the 3-gram Probability chaining model
# pattern2 = "[[:punct:]]|\\,|\\'"
# findprob2 <- function(testvector) {
#   for(i in 1:length(testvector)) {
#     finalprob <- c()
#     testvector[i]<-gsub(pattern = pattern2, "", testvector[i])
#     ans1<-unlist(strip(strsplit(tolower(testvector[i])," ")))
#     # The counter starts with 2 as for some weird reason 2 is the start
#     for(j in 2:length(ans1)-2) {
#       res.ng3 <- paste(ans1[j], ans1[j+1], ans1[j+2], sep = " ")
#       res.ng2 <- paste(ans1[j], ans1[j+1], sep = " ")
#       
#       #Getting the counts of 3-grams and 2-grams
#       sum.3<-sum(blogs.ng3.DT[blogs.ng3.DT$ngrams==res.ng3,]$freq, news.ng3.DT[news.ng3.DT$ngrams==res.ng3,]$freq, 
#                  twit.ng3.DT[twit.ng3.DT$ngrams==res.ng3,]$freq)
#       sum.2<-sum(blogs.train_bigrams[blogs.train_bigrams$bigram==res.ng2,]$n,
#                  news.train_bigrams[news.train_bigrams$bigram==res.ng2,]$n,
#                  twitter.train_bigrams[twitter.train_bigrams$bigram==res.ng2,]$n)
#       cat(i," SUM 2 = ",sum.2, ", SUM 3 = ",sum.3, "\n")
#       
#       #Get the Probability here
#       if (sum.2 != 0) {
#         finalprob[j-1] <- sum.3/sum.2 
#       }
#       else {finalprob[j-1]<-1}
#     }
#     cat("Probability of the sentence ", i, ":", prod(finalprob),"\n")
#   }
# }
# 
# # Get the counts for 2-grams above
# findprob<-function(testvector) {
#   for(i in 1:length(testvector)) {
#     finalprob <- c()
#     testvector[i]<-gsub(pattern = pattern2, "", testvector[i])
#     ans1<-unlist(strip(strsplit(tolower(testvector[i])," ")))
#     for(j in 2:length(ans1)-1) {
#       res <- paste(ans1[j],ans1[j+1],sep = " ")
#       res1 <- sum(blogs.train_bigrams[blogs.train_bigrams$bigram==res,]$n ,news.train_bigrams[news.train_bigrams$bigram==res,]$n, twitter.train_bigrams[twitter.train_bigrams$bigram==res,]$n)
#       
#       #Get the final Probability here
#       res.1<-sum(blg_uni[blg_uni$word==ans1[j],]$n, news_uni[news_uni$word==ans1[j],]$n, twit_uni[twit_uni$word==ans1[j],]$n)
#       finalprob[j-1] <- res1/res.1
#       cat(i," SUM 1 = ",res.1, ", SUM 2 = ",res1, "\n")
#     }
#     cat("Probability of the sentence ", i, ":", prod(finalprob),"\n")
#   }
# }

# Trigrams separated 
#twit.DT.sep <- twit.DT %>% separate(ngrams, c("word1","word2","word3"), sep=" ")
#write.csv(twit.DT.sep, file = "C:/Public/Data-Science[JHU-Coursera]/project_nlp/foo.csv",row.names=FALSE)
#write.csv(twitter.train, file = "C:/Public/Data-Science[JHU-Coursera]/project_nlp/twitter-train.csv",row.names=FALSE)

#testvector <- c("I'll be there for you, I'd live and I'd eat",
#                "I'll be there for you, I'd live and I'd sleep",
#                "I'll be there for you, I'd live and I'd give",
#                "I'll be there for you, I'd live and I'd die")

#findprob(testvector = testvector)
#findprob2(testvector = testvector)