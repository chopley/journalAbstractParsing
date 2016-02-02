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
require(doParallel)
require(foreach)
require(networkD3)
install.packages("networkD3")
install.packages("extrafont")

library(extrafont)
font_import()

#open up the user specific functions defined in functions.R- this has definitions of the web page format etc.
source('./functions.R')



nCores<-detectCores()
cl<-makeCluster(nCores)
registerDoParallel(cl)
getDoParWorkers()
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
getDoParName()

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
              abstract = '/article/.*meta', metaNodes= 'meta', metaNames = 'name', metaContent = 'content',authorSearch = "^citation_author$",
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


nAffiliations<-length(unique((mnrasData2$affiliation1)))
affiliationsSorted<- sort(unique(mnrasData2$affiliation1))
a=NULL
for(i in 1:length(affiliationsSorted)){
  dist.name<-adist(affiliationsSorted[i],affiliationsSorted, partial = TRUE, ignore.case = TRUE)
  a=rbind(a,dist.name)
}

#sort the affiliations nicely so it combines affiliations that are close in name
mnrasData2<-sortAffiliations(mnrasData2)

x = data.frame(num = 1:26, let = letters, LET = LETTERS)
set.seed(10)
tt<-split(x, sample(rep(1:2, 13)))

  ddEdges<-edges(mnrasData1,"affiliation1")
  affiliations<-mnrasData1$affiliation1[V(g)]

  g <- graph.data.frame(ddEdges, directed=FALSE)
  a<-as.numeric(degree(g))
  which(a<2)
  
  V(g)$label <- V(g)$name
  opar <- par()$mar; par(mar=rep(0, 4)) #Give the graph lots of room
  set.seed(3952)
  plot.igraph(g, layout=layout.fruchterman.reingold(g),vertex.label=NA,vertex.size=1)
  layout <- layout.reingold.tilford(g, circular=T)
  net <- simplify(g2, remove.multiple = F, remove.loops = T)
  plot.igraph(g, vertex.label=NA,vertex.size=1,layout=layout)
  plot.igraph(net, vertex.label=NA,vertex.size=1)
  d3g2<-as.data.frame(get.edgelist(g2))
  simpleNetwork(Data = d3g2,  height = 1000, width = 1000,  charge = -50)
  
  
  par(mar=opar)
  