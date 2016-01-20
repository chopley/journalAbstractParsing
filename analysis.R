#This script is used to read the data from the autotrader website in a specified format.
#The data are then written to text file for further analysis by the learn.R function
#CJC Rev A 2015/06/20

require(rvest)
require(ggplot2)


require(pipeR) # %>>% will be faster than %>%
require(httr)
require(RCurl)
require(dplyr)
#open up the user specific functions defined in functions.R- this has definitions of the web page format etc.
source('./functions.R')


#First we ppopulate the websites for the different journals
#----------------------------------------------------------------------


setClass("Journal",
         slots = list(base = "character", extension= "character",metaNodes= "character", 
                      metaNames = "character", metaContent = "character",authorSearch = "character",
                      institutionSearch = "character",doiSearch = "character",dateSearch = "character",
                      emailSearch = "character"))
mnras <- new("Journal", base = "http://mnras.oxfordjournals.org/", extension = 'content/440/1.toc', 
             metaNodes= 'meta', metaNames = 'name', metaContent = 'content',authorSearch = "^citation_author$",
             institutionSearch = "^citation_author_institution$", doiSearch="^citation_doi$",dateSearch="^citation_date$",
             emailSearch = "^citation_author_email$")

mnras <- new("Journal", base = "http://mnras.oxfordjournals.org/", extension = 'content/400/1.toc', 
             metaNodes= 'meta', metaNames = 'name', metaContent = 'content',authorSearch = "^citation_author$",
             institutionSearch = "^citation_author_institution$", doiSearch="^citation_doi$",dateSearch="^citation_date$",
             emailSearch = "^citation_author_email$")


abstractLinks <- getWebPageDataJournal(mnras,10)
mnrasData<- parseAbstracts(mnras,abstractLinks,500)








