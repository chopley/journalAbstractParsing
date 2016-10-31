#install.packages("oai")
#install.packages("aRxiv")
install.packages('scholar')
install.packages('RCurl')
library('oai')
library(aRxiv)
library('scholar')
library(XML)
require(RCurl)
library(stringr)
require(wordcloud)
require(Rcpp)
require(tm)


arxiv_search('au:"*Copley*"')
a<-arxiv_search('au:"Copley"')

site <- getForm("http://www.google.com/search", hl="en",
                lr="", q="r-project", btnG="Search")
htmlTreeParse(site)

id("http://export.arxiv.org/oai2")
a<-list_identifiers(from = '2011-05-01T', until = '2011-20-01T')

b<-get_records(c("oai:oai.datacite.org:32153", "oai:oai.datacite.org:32200"))



d<-NULL
d$title[1]<-b$title[1]
temp<-b$creator[1]
temp<-c(temp,b$creator.1[1])

c<-get_records(a$identifier[1])
cc<-get_records(a$identifier[2])
d<-get_records(a$identifier[1:5])
