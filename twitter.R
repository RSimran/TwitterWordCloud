accesstokensecret='[twitter access token]'
api_key= "[api-key]"
api_secret= "[api-access]"
access_token= "[access-token]"
access_token_secret= "[access-token-scret]"

#install.packages("twitteR")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("qdap")
#install.packages("wordcloud")
#install.packages("httpuv")
#install.packages("base64enc")
library(twitteR)
library(dplyr)
library(stringr)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)

setup_twitter_oauth(apikey,apisecret)
findfd="UCSB"
number= 4000

tweetsearch=searchTwitter(findfd,number)
tweerSt=lapply(tweetsearch,function(t)t$getText())

tryTolower = function(x)
{
y = NA
try_error = tryCatch(tolower(x), error = function(e) e)
if (!inherits(try_error, "error"))
y = tolower(x)
return(y)
}

clean=function(t){
t=gsub('[[:punct:]]','',t)
t=gsub('[[:cntrl:]]','',t)
t=gsub('\\d+','',t)
t=gsub('[[:digit:]]','',t)
t=gsub('@\\w+','',t)
t=gsub('http\\w+','',t)
t=gsub("^\\s+|\\s+$", "", t)
t=sapply(t,function(x) tryTolower(x))
t=str_split(t," ")
t=unlist(t)
return(t)
}
cleantweet=lapply(tweerSt,function(x) clean(x))
head(cleantweet,5)
test&lt;-unlist(cleantweet)

poswords=scan('positive-words.txt', what='character', comment.char=';')
negativewords=scan('negative-words.txt', what='character', comment.char=';')

sentimentcount=function(tweeter){
positive.match=match(tweeter,poswords)
positive.match=!is.na(positive.match)
positive.score=sum(positive.match)
return(positive.score)
}

pos.scores=lapply(cleantweet, function(x) sentimentcount(x))

positive.counts=0
for(i in 1:length(pos.scores)){

positive.counts=positive.counts+pos.scores[[i]]

}

positive.counts

posdata.vector=data.frame(NULL)
for(i in 1:length(cleantweet)){
postest=match(cleantweet[[i]],poswords)
poswordstest=cleantweet[[i]][!is.na(postest)]
posdata.vector=c(poswordstest,posdata.vector)
}
head(posdata.vector,10)
postable=data.frame(table(unlist(posdata.vector)))
postablefilter=postable[which(postable$Freq&gt;15),]
postablefilter[1]=lapply(postablefilter[1],as.character)

library(ggplot2)
ggplot(postablefilter,aes(Var1,Freq))+geom_bar(stat="identity", fill="lightblue")+theme_bw()+labs(x="Positive Words", y="Frequency of Words",title=paste("Major Positive Words And Occurace in #UCSB Twitter Feed"))+geom_text(aes(Var1,Freq,label=Freq), size=6)+ theme(plot.title = element_text(size=18))+theme(axis.text=element_text(size=8))+theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))

library(wordcloud)
twittercorpus=Corpus(VectorSource(cleantweet))
twittercorpus=tm_map(tweetercorpus,removeWords,stopwords("en"))
library(wordcloud)
wordcloud()

wordcloud(twittercorpus,scale=c(10,1),random.order = FALSE,rot.per = 0.20,use.r.layout = FALSE,colors = brewer.pal(6,"Dark2"),min.freq=32)
