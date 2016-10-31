sortAffiliations <-function(data){
  a<-sort(unique(data$affiliation1))
  lenA<-length(a)
  
  i<-1
  while(i < lenA){
    #print(a[i])
    dist.name<-adist(a[i],data$affiliation1, partial = TRUE, ignore.case = TRUE)
    average<-mean(na.omit(t(dist.name)))
    stddev<-sd(na.omit(t(dist.name)))
    bbb<-which(dist.name<(average-5*stddev))
    #print(mnrasData1$affiliation1[bbb])
    data$affiliation1[bbb]<-a[i] #set all the values to the first affiliation
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
  return(data)
}



getWebPageDataJournal <- function(journal,nArticles){
  #function that will get abstracts from a journal.
  #Set the timeout nice and long because sometimes they take a LONG time
  defaultOptions<- curlOptions(timeout = 100,httpauth = 1L) #need to name the useragent for certain websites
  #See http://www.omegahat.org/RCurl/FAQ.html
  options(RCurlOptions = defaultOptions) 
  
  nextWebPage<-paste(journal@base,journal@extension,sep="")
  papers=NULL
  for(i in 1:nArticles){
    print(paste("Accessing URL",i))
    b<-getURL(nextWebPage,ssl.verifypeer = FALSE, useragent = "R",.opts=curlOptions(followlocation = TRUE)) #Use this because it allows for a longer timeout by directly using RCurl 
    #Need the useragent and ssl.verifypeer for certain websites it seems
    hrefLinks<- read_html(b) %>>% html_nodes("a") %>>% html_attr("href")
    nextIssue<-read_html(b) %>>% html_nodes("a")
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

getData2<-function(journal,searchString,htmlPage,metaNodes,metaNames,metaContent){
  #require slightly more flexible input to allow different metaNames depending on which register is used
  webPage<-read_html(htmlPage) #read html to xml
  nodes<-html_nodes(webPage,metaNodes) #find the nodes (e.g. xml dataframes) that have all the stuff we need as defined in the journal definition file
  #nodes<-html_nodes(webPage,'div')
  #typeDef <- html_attr(nodes,'class') for apJ
  typeDef <- html_attr(nodes,metaNames) #get the tags associated with each xml data frame
  authorContent<-html_nodes(webPage,journal@authorNodes)
  authorIndex<-grep('itemtype="http://schema.org/Person" itemprop="author" class="nowrap">\n  <span itemprop="name">',authorContent)
  authorContent2<-strsplit(as.character(authorContent[authorIndex]), 'span', fixed = FALSE, perl = FALSE, useBytes = FALSE)
  author<-NULL
  authorAffiliationIndex<-NULL
  authorAffiliation<-NULL
  for(i in 1:length(authorContent2)){
    tempAuthor<-authorContent2[[i]][3] #get the author name
    tempAffiliationIndex<-authorContent2[[i]][4] #get the affiliation index
    authorAffiliationIndex<-rbind(authorAffiliationIndex,tempAffiliationIndex)
    author<-rbind(author,tempAuthor)
  }
  #" itemprop=\"name\">E. Churazov</"
  author2 <- sub(".*?\"name\">(.*?)</.*", "\\1", as.character(author))
  #tempAffiliationIndex ">\n  <sup>1</sup>\n</" We need to extract the value so we use sub as below
  authorAffiliationIndex2 <- sub(".*?<sup>(.*?)</sup>.*", "\\1", as.character(authorAffiliationIndex))
  content2<-html_nodes(webPage,'.wd-jnl-art-author-affiliations') #get the author affiliations (use the selector tool)
  affiliationIndices<-html_children(content2) 
  index<-grep('.*sup>.*</sup>.*',affiliationIndices)
  values<-strsplit(as.character(affiliationIndices[index]),"</sup>", fixed = FALSE, perl = FALSE, useBytes = FALSE)
  for(i in 1:length(author)){
    authorAffiliation<-rbind(authorAffiliation,(values[[as.numeric(authorAffiliationIndex2[i])]][2]))
  }
  
  nodesDOI<-html_nodes(webPage,'.wd-jnl-art-doi')
  DOI<-html_text(nodesDOI)
  DOI <- sub(".*http:(.*?)\n.*", "\\1", as.character(DOI))
  DOI <-paste("http:",DOI,sep="")
  
  
  nodesEmail<-html_nodes(webPage,'.mb-0')
  emailIndex<-grep('.*mailto:.*',nodesEmail)
  email <- sub(".*?mailto:(.*?)\" title.*", "\\1", as.character(nodesEmail[emailIndex]))
  
  
  nodesDates<-html_nodes(webPage,'.wd-jnl-art-dates')
  dates<-html_text(nodesDates)
  dates<- sub(".*Accepted (.*?)\n.*", "\\1", as.character(dates))
  
  
  data<-cbind(author2,authorAffiliation)
  dates<-rep(dates[1],nrow(data))
  email<-rep(email[1],nrow(data))
  DOI<-rep(DOI,nrow(data))
  data<-cbind(data,DOI,email,dates)
  #output <- data.frame(author= character(0), affiliation1= character(0),affiliation2= character(0),affiliation3=character(0),
  #                     affiliation4=character(0),affiliation5=character(0),doi=character(0),authorEmail=character(0),
  #                     abstractURL=character(0),date=character(0),webpage=character(0))
  colnames(data)<-c("author","affiliation1","doi","authorEmail","date")
  
  return(data)
}


getData<-function(journal,searchString,htmlPage,metaNodes,metaNames,metaContent){
  #getData routine for certain Journals
  webPage<-read_html(htmlPage) #read html to xml
  nodes<-html_nodes(webPage,metaNodes) #find the nodes (e.g. xml dataframes) that have all the stuff we need as defined in the journal definition file
  #nodes<-html_nodes(webPage,'div')
  #typeDef <- html_attr(nodes,'class') for apJ
  typeDef <- html_attr(nodes,metaNames) #get the tags associated with each xml data frame
  content<-html_attr(nodes,metaContent)
  
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
  #The primary data parser. This will go through a list of abstract links and try to pull the data from them
  #largely works with the following Journals
  #1. MNRAS
  #2. Astronomy and Astrophysics
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
      b<-getURL(abstracts[i],ssl.verifypeer = FALSE, useragent = "R",.opts=curlOptions(followlocation = TRUE)) 
      authors<-getData(journal,journal@authorSearch,b,
                       journal@metaNodes,journal@metaNames,journal@metaContent) #get the authors
      institutions<-getData(journal,journal@institutionSearch,b,
                            journal@metaNodes,journal@metaNames,journal@metaContent) #get the institutions
      institutions$values<-gsub('^[[:digit:]]+','',institutions$values) #remove any preceding digits from the institutions
      email<-getData(journal,journal@emailSearch,b,
                     journal@metaNodes,journal@metaNames,journal@metaContent) #get the icorresponding email
      doi<-getData(journal,journal@doiSearch,b,
                   journal@metaNodes,journal@metaNames,journal@metaContent) #get the doi
      date<-getData(journal,journal@dateSearch,b,
                    journal@metaNodes,journal@metaNames,journal@metaContent) #get the date
      dd<-matchAuthors(authors,institutions,dd)
      try({
      dd$doi <- rep(doi$values,nrow(dd)) 
      dd$email<- rep(email$values,nrow(dd))
      dd$date<- rep(date$values,nrow(dd))
      dd$abstractURL <-rep(abstracts[i],nrow(dd))
      dd$webpage<-rep(journal@base,nrow(dd))
      output<-rbind(output,dd)
      })
      save(output,file='data.R')
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

parseAbstracts2 <-function(journal,abstracts,nAbstracts){
  #this is a slight variation on parseAbstract and is written to support the Astorphysics Journal data format.
  #Still a work in progress.
  #largely works with the following Journals
  #1. Astrophysics Journal- Probably with other IOP publications?

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
    try({
    b<-getURL(abstracts[i],ssl.verifypeer = FALSE, useragent = "R",.opts=curlOptions(followlocation = TRUE)) 
    })
    
    try({
      authorsAffiliations<-getData2(journal,journal@authorSearch,b,
                 journal@metaNodes,journal@metaNames,journal@metaContent)
      dd<-as.data.frame(authorsAffiliations)
      })#get the authors
    #colnames(authorsAffiliations)<-c('Author','Affiliation1')

   
    
    try({
      dd$abstractURL <-rep(abstracts[i],nrow(dd))
      dd$webpage<-rep(journal@base,nrow(dd))
      output<-rbind(output,dd)
    })
    try({save(output,file='data2.R')})
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

generate3Dplot<- function(g,filename){
  
  g <- simplify(g, remove.multiple = F, remove.loops = T)
  # Calculate degree for all nodes
  degAll <- degree(g, v = V(g), mode = "all")
  coord3D <- layout.fruchterman.reingold(g, dim = 3) 
  nType <- 1
  
  write.table("@<TRIPOS>MOLECULE", file = filename, row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table("Network Plot", file = filename, row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  write.table(data.frame(vcount(g), ecount(g), nType), file = filename, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  write.table("SMALL", file = filename, row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  write.table("NO_CHARGES\n", file = filename, row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  write.table("@<TRIPOS>ATOM", file = filename, row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  
  isType <- 1
  for (i in 1:vcount(g))
  {
    if (degAll[i] == 1)
      isIn <- "He"
    
    if (degAll[i] == 2)
      isIn <- "Na"
    
    if ((degAll[i] > 2) & (degAll[i] <= 5))
      isIn <- "O"
    
    if ((degAll[i] > 5) & (degAll[i] <= 10))
      isIn <- "Au"
    
    if ((degAll[i] > 10) & (degAll[i] <= 15))
      isIn <- "P"
    
    if (degAll[i] > 15)
      isIn <- "Cl"
    
    hlpL <- data.frame(i, V(g)[i]$name, coord3D[i, 1], coord3D[i, 2], coord3D[i, 3], isIn, isType, 0.0 )
    
    write.table(hlpL, file = filename, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  }
  
  write.table("@<TRIPOS>BOND", file = filename, row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  isType <- 1
  
  for (i in 1:ecount(g))
  {
    hlpL <- data.frame(i, get.edge(g,i)[1], get.edge(g,i)[2], isType)
    
    write.table(hlpL, file = filename, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  }
  
  
}

