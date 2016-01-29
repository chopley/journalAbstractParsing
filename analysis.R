#This script is used to read the data from the autotrader website in a specified format.
#The data are then written to text file for further analysis by the learn.R function
#CJC Rev A 2015/06/20

require(rvest)
require(ggplot2)


require(pipeR) # %>>% will be faster than %>%
require(httr)
require(RCurl)
require(dplyr)
library('caTools')
library(igraph)
library('stringdist')
library('rgl')

#open up the user specific functions defined in functions.R- this has definitions of the web page format etc.
source('./functions.R')


#First we ppopulate the websites for the different journals
#----------------------------------------------------------------------


setClass("Journal",
         slots = list(base = "character", extension= "character",nextIssue = "character", abstract="character", metaNodes= "character", 
                      metaNames = "character", metaContent = "character",authorSearch = "character",
                      institutionSearch = "character",doiSearch = "character",dateSearch = "character",
                      emailSearch = "character"))

mnras <- new("Journal", base = "http://mnras.oxfordjournals.org/", extension = 'content/440/1.toc', 
             metaNodes= 'meta', metaNames = 'name', metaContent = 'content',authorSearch = "^citation_author$",
             institutionSearch = "^citation_author_institution$", doiSearch="^citation_doi$",dateSearch="^citation_date$",
             emailSearch = "^citation_author_email$")

#header to define the mnras layout
mnras <- new("Journal", base = "http://mnras.oxfordjournals.org/", extension = 'content/313/1.toc', nextIssue = 'Next issue', abstract = '*abstract*',
             metaNodes= 'meta', metaNames = 'name', metaContent = 'content',authorSearch = "^citation_author$",
             institutionSearch = "^citation_author_institution$", doiSearch="^citation_doi$",dateSearch="^citation_date$",
             emailSearch = "^citation_author_email$")

#header to define the Astronomy and Astrophysics layout
astast <- new("Journal", base = "http://www.aanda.org/", extension = 'articles/aa/abs/2001/01/contents/contents.html', nextIssue = 'Next issue', 
              abstract = '/aa/abs/.*aa.*\\.html$', metaNodes= 'meta', metaNames = 'name', metaContent = 'content',authorSearch = "^citation_author$",
             institutionSearch = "^citation_author_institution$", doiSearch="^citation_doi$",dateSearch="^citation_publication_date$",
             emailSearch = "^citation_author_email$")

#header to define the ApJ
astApj <- new("Journal", base = "http://iopscience.iop.org/", extension = '0004-637X/809/1', nextIssue = 'next issue', 
              abstract = '/aa/abs/.*aa.*\\.html$', metaNodes= 'meta', metaNames = 'name', metaContent = 'content',authorSearch = "^citation_author$",
              institutionSearch = "^citation_author_institution$", doiSearch="^citation_doi$",dateSearch="^citation_publication_date$",
              emailSearch = "^citation_author_email$")

#get base by looking at the page info

# MNRAS 300 is ~1998
# MNRAS 325 11 August 2001
# MNRAS 400 is ~2004

abstractLinksmnras <- getWebPageDataJournal(mnras,200)
abstractLinksaa <- getWebPageDataJournal(astast,2)
abstractLinksapj <- getWebPageDataJournal(astApj,2)


mnrasData2<- parseAbstracts(mnras,abstractLinksmnras[1:6125],length(abstractLinksmnras[1:6125]))
mnrasData<- parseAbstracts(mnras,abstractLinksmnras[1:1270],length(abstractLinksmnras[1:1270]))

aaData<- parseAbstracts(astast,abstractLinksaa,380)
apjData<- parseAbstracts(astApj,abstractLinksapj,5)
#glob2rx("/aa/abs/*aa*.html")
ddEdges <- data.frame(V1= character(0), V2= character(0))

ddEdges<-edges(mnrasData,"author")




a<-sort(unique(mnrasData1$affiliation1))
lenA<-length(a)

i<-1
while(i < lenA){
  #print(a[i])
  dist.name<-adist(a[i],mnrasData1$affiliation1, partial = TRUE, ignore.case = TRUE)
  
  average<-mean(na.omit(t(dist.name)))
  stddev<-sd(na.omit(t(dist.name)))
  bbb<-which(dist.name<(average-5*stddev))
  #print(mnrasData1$affiliation1[bbb])
  mnrasData1$affiliation1[bbb]<-a[i] #set all the values to the first affiliation
  dist.name2<-adist(a[i],a, partial = TRUE, ignore.case = TRUE)
  average2<-mean(na.omit(t(dist.name2)))
  stddev2<-sd(na.omit(t(dist.name2)))
  ccc<-which(dist.name2<(average2-5*stddev2))
  if(length(ccc)>1){
    indexC<-ccc[2:length(ccc)]
    #print(a[indexC])
    a<-a[-indexC] #remove the names from the list that are similar
  }
  lenA<-length(a)
  i<-i+1
  print(lenA)
  print(i)
}

  ddEdges<-edges(mnrasData1,"affiliation1")
  affiliations<-mnrasData1$affiliation1[V(g)]

  g <- graph.data.frame(ddEdges, directed=FALSE)
  V(g)$label <- V(g)$name
  opar <- par()$mar; par(mar=rep(0, 4)) #Give the graph lots of room
  set.seed(3952)
  plot.igraph(g, layout=layout.fruchterman.reingold(g),vertex.label=NA,vertex.size=1)
  layout <- layout.reingold.tilford(g, circular=T)
  plot.igraph(g, vertex.label=NA,vertex.size=1,layout=layout)
  plot.igraph(g, vertex.label=NA,vertex.size=1)
  par(mar=opar)
  