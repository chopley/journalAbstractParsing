getWebPageDataJournal <- function(journal,nArticles){
  #function that will get abstracts from a journal.
  #Set the timeout nice and long because sometimes they take a LONG time
  defaultOptions<- curlOptions(timeout = 60,httpauth = 1L) #need to name the useragent for certain websites
  #See http://www.omegahat.org/RCurl/FAQ.html
  options(RCurlOptions = defaultOptions) 
  
  nextWebPage<-paste(journal@base,journal@extension,sep="")
  papers=NULL
  for(i in 1:nArticles){
    print(paste("Accessing URL",i))
    b<-getURL(nextWebPage,ssl.verifypeer = FALSE, useragent = "R") #Use this because it allows for a longer timeout by directly using RCurl 
    #Need the useragent and ssl.verifypeer for certain websites it seems
    hrefLinks<- read_html(b)%>>% html_nodes("a") %>>% html_attr("href")
    nextIssue<-read_html(b)%>>% html_nodes("a")
    nextIssueURL<-getNextIssue(journal,nextIssue)
    
    #We get the lines that have the abstract html links
    abstractLinks <- hrefLinks[grep(journal@abstract,hrefLinks)]
    #abstractLinks <- hrefLinks[grep('*/aa/abs/*html$',hrefLinks)]
    #We create the addressable html links
    papers <-c(papers,paste(journal@base,abstractLinks,sep=""))
    nextWebPage<-paste(journal@base,nextIssueURL,sep="")
    if(length(nextIssueURL)==0){
      print('Reached end of Journal')
      break
    }
  }
  #and we read in the papers, extracting the parts of relevance (i.e. 'meta' labelled)
  return(papers)
}

getNextIssue<-function(journal,htmlPage){
  #function to extract the next issue
  nextIssueURL<-htmlPage[grep(journal@nextIssue,htmlPage)]%>>%html_attr("href")
  return(nextIssueURL)
}

getData<-function(journal,searchString,htmlPage){
  webPage<-read_html(htmlPage) #read html to xml
  nodes<-html_nodes(webPage,journal@metaNodes) #find the nodes (e.g. xml dataframes) that have all the stuff we need as defined in the journal definition file
  typeDef <- html_attr(nodes,journal@metaNames) #get the tags associated with each xml data frame
  content<-html_attr(nodes,journal@metaContent)
  error<-0
  index<-grep(searchString,typeDef)
  values<-content[index]
  aa<-cbind(values,index)
  coln<-colnames(aa)
  if(length(index)!=0){
    return(as.data.frame(aa))}
  else{
    bb<-as.data.frame(t(c('NA','NA')))
    colnames(bb)<-(coln)
    return(bb)
  }
}

matchAuthors<-function(authors,institutions,df){
  #function to match author to affiliation.
  #it assumes that the xml file will have the author information followed by the affiliation information
  #e.g. if the 18th element is of type author, and the 19th and 20th are of type affiliation, then it will match the 19th and 20th 
  #affiliations to the author given in the 18th element.
  dd <- data.frame(author= character(0), affiliation1= character(0),affiliation2= character(0),affiliation3=character(0),
                   affiliation4=character(0),affiliation5=character(0))
  j<-1
  for(i in 1:length(authors$index)){
    authorIndex<-authors$index[i] #get the vector index of the author
    instituteIndex<-j #get the vector index of the institution
    if(j<length(institutions$index)){ #check that we won't overrun the vector length
      while(as.numeric(as.character(institutions$index[j+1]))==as.numeric(as.character(institutions$index[j]))+1){
        j<-j+1
        instituteIndex<-c(as.numeric(instituteIndex),j)
        #must break out the while loop if the institution index is getting too high
         if(j>length(institutions$index)-1){
          break 
          }
      }
      j<-j+1
    }
    #print(instituteIndex)
  a<-as.data.frame(t(c(as.character(authors$values[i]),as.character(institutions$values[instituteIndex]))  ),stringsAsFactors=FALSE)
  names<-colnames(dd)
  newNames<-names[1:ncol(a)]
  colnames(a)<-newNames
  df<-bind_rows((df),as.data.frame(a))
  
  }
  
  return(df)    
  
  
}


parseAbstracts <-function(journal,abstracts,nAbstracts){
  defaultOptions<- curlOptions(timeout = 300)
  options(RCurlOptions = defaultOptions)

  output <- data.frame(author= character(0), affiliation1= character(0),affiliation2= character(0),affiliation3=character(0),
                   affiliation4=character(0),affiliation5=character(0),doi=character(0),authorEmail=character(0),
                   abstractURL=character(0),date=character(0),webpage=character(0))
  for(i in 1:nAbstracts){
    print(paste('Abstract ',i))
    #create a clear dataframe for each abstract and append this to the eventual output
    dd <- data.frame(author= character(0), affiliation1= character(0),affiliation2= character(0),affiliation3=character(0),
                     affiliation4=character(0),affiliation5=character(0),doi=character(0),authorEmail=character(0),
                     abstractURL=character(0),date=character(0),webpage=character(0))
      b<-getURL(abstracts[i]) 
      authors<-getData(journal,journal@authorSearch,b) #get the authors
      institutions<-getData(journal,journal@institutionSearch,b) #get the institutions
      institutions$values<-gsub('^[[:digit:]]+','',institutions$values) #remove any preceding digits from the institutions
      email<-getData(journal,journal@emailSearch,b) #get the icorresponding email
      doi<-getData(journal,journal@doiSearch,b) #get the doi
      date<-getData(journal,journal@dateSearch,b) #get the date
      dd<-matchAuthors(authors,institutions,dd)
      try({
      dd$doi <- rep(doi$values,nrow(dd)) 
      dd$email<- rep(email$values,nrow(dd))
      dd$date<- rep(date$values,nrow(dd))
      dd$abstractURL <-rep(abstracts[i],nrow(dd))
      dd$webpage<-rep(journal@base,nrow(dd))
      output<-rbind(output,dd)
      })
      #Impact factor
      #Date xx
      #Journal Name xx
      #Number of citations?
      #Author Email xx
      #URL to abstract xx
      #URL to Journal Issue??
  }
     
    return(output)
}

edges <-function(data,field){
  ddEdges <- data.frame(V1= character(0), V2= character(0))
  doi<-(unique(data$doi)) #use the doi to get the unique papers
  for(i in 1:length(unique(doi))){ 
    a<-split(data,data$doi==doi[i])$'TRUE'
    
      fieldPaste<-paste('a$',field,sep="") #construct variable to use
      vector<-unique(eval(parse(text = fieldPaste)))
    if(length(vector)>1){  
      b<-combs(1:length(vector),2) #get the author links using n choose r
      gg<-vector[b]
      Edges<-as.data.frame(matrix(gg,ncol=2))
      colnames(Edges)<-colnames(ddEdges)
      ddEdges<-rbind(ddEdges,Edges)
    }
  }
  return(ddEdges)
}

