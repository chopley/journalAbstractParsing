#This script is used to read the data from differen academic journals
# the format is defined in the journal class
#CJC Rev A 2016/02/01

install.packages("networkD3")
install.packages("extrafont")
install.packages("RCurl")
install.packages("caTools")
install.packages("stringdist")
install.packages("rgl")
install.packages("rvest")
install.packages("pipeR")
install.packages("dplyr")
install.packages("XML")


require(rvest)
require(XML)
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
require(plotly)
library(magrittr)
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
         slots = list(base = "character", extension= "character",nextIssue = "character", abstract="character", 
                      metaNodes= "character", metaNames = "character", metaContent = "character", 
                      authorNodes= "character",  authorSplit = "character", authorExtractString = "character",authorAffiliationIndex = "character",
                      affiliationNodes = "character", affiliationSplit = "character", affiliationExtractString = "character", 
                      authorSearch = "character", institutionSearch = "character",doiSearch = "character",dateSearch = "character",
                      emailSearch = "character",websiteLayout = "character"))
#decsription of Journal Layout
#base - The base URL of the website
#extension - the extension that is added to the base to get the URL of each of the journal volume abstract list
#metaNodes - This is a node in the XML that is used for all the author names, affiliations etc. Only used for MNRAS, A&A in getData
#metaNames - This is an attribute of the nodes that extracts the labels of each XML data field. Only used for MNRAS, A&A in getData
#metaContent - This is an attribute of the nodes that extracts the content in each XML data field. Only used for MNRAS, A&A in getData
#authorNodes
#authorSplit
#authorExtractString
#affiliationNodes
#affiliationSplit
#affiliationExtractString
#authorSearch
#instiutionSearch
#doiSearch
#dateSearch
#emailSearch
#websiteLayout


#header to define the mnras layout
mnras <- new("Journal", base = "http://mnras.oxfordjournals.org/", extension = 'content/313/1.toc', nextIssue = 'Next issue', abstract = '*abstract*',
             metaNodes= 'meta', metaNames = 'name', metaContent = 'content',
             authorSearch = "^citation_author$",institutionSearch = "^citation_author_institution$", doiSearch="^citation_doi$",dateSearch="^citation_date$",
             emailSearch = "^citation_author_email$")

#header to define the Astronomy and Astrophysics layout
astast <- new("Journal", base = "http://www.aanda.org/", extension = 'articles/aa/abs/2001/01/contents/contents.html', nextIssue = 'Next issue', 
              abstract = '/aa/abs/.*aa.*\\.html$', metaNodes= 'meta', metaNames = 'name', metaContent = 'content',authorSearch = "^citation_author$",
             institutionSearch = "^citation_author_institution$", doiSearch="^citation_doi$",dateSearch="^citation_publication_date$",
             emailSearch = "^citation_author_email$")

#header to define the ApJ
astApj <- new("Journal", base = "http://iopscience.iop.org/", extension = '0004-637X/471/1', nextIssue = 'next issue', 
              abstract = '/article/.*meta', metaNodes= 'meta', metaNames = 'name', metaContent = 'content',
              authorNodes= ".mb-0, span", authorSplit = "span", authorExtractString = '.*?\"name\">(.*?)</.*', authorAffiliationIndex = '.*?<sup>(.*?)</sup>.*',
              affiliationNodes = ".wd-jnl-art-author-affiliations",affiliationSplit = "</sup>", affiliationExtractString = ".*sup>.*</sup>.*", 
              institutionSearch = "^citation_author_institution$", doiSearch="^citation_doi$",
              dateSearch="^citation_publication_date$", emailSearch = "^citation_author_email$", websiteLayout = "character")


#get base by looking at the page info

# MNRAS 300 is ~1998
# MNRAS 325 11 August 2001
# MNRAS 400 is ~2004

# extract the html links for the abstracts of 
#MNRAS,
abstractLinksmnras <- getWebPageDataJournal(mnras,500)
#Astrononmy and Astrophysics, 
abstractLinksaa <- getWebPageDataJournal(astast,500)
#and Atrophysical Journal
abstractLinksapj <- getWebPageDataJournal(astApj,500)


#Parse the abstracts to produce a dataframe containing
#[1] "author"       "affiliation1" "affiliation2" "affiliation3" "affiliation4" "affiliation5"
#[7] "doi"          "authorEmail"  "abstractURL"  "date"         "webpage"      "email"  
mnrasData<- parseAbstracts(mnras,abstractLinksmnras,5)
#mnrasData<- parseAbstracts(mnras,abstractLinksmnras[1:1270],length(abstractLinksmnras[1:1270]))

aaData<- parseAbstracts(astast,abstractLinksaa,380)
apjData<- parseAbstracts(astApj,abstractLinksapj,3)
#apj needs a different routine for handling the data
apjData<- parseAbstracts2(astApj,abstractLinksapj,6)

#glob2rx("/aa/abs/*aa*.html")
#ddEdges <- data.frame(V1= character(0), V2= character(0))

#sort the affiliations nicely so it combines affiliations that are close in name
mnrasData<-sortAffiliations(mnrasData)
#extract the graph edges from the dataframe
ddEdgesMNRASAuthor<-edges(mnrasData,"author")
ddEdgesAffiliation1<-edges(mnrasData,"affiliation1")

#MNRAS March 21, 2000 -> 
#This is from saved date
load('ddEdgesMNRAS.rdata')
write.csv(ddEdgesMNRAS,"MNRAS_edges.csv")



affiliations<-apjData$affiliation1[V(g)]
  
  
 
  