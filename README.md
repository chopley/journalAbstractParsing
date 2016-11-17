# journalAbstractParsing
## Introduction
Parser of various journals using R. This is still a work in progress, but the idea is that to scrape a few academic journals extracting metainformation from them. Currently I support Monthly Notices of the Royal Astronomical society, Astronomy and AStrophysics and the Astrophysical Journal. By associating authors and institutions through co-publication we are able to generate a graph where influence and article importance can be measured.

## File Descriptions

1. **analysis.R** high level script that handles calling various sub-functions in functions.R
2. **functions.R**  sub-functions supporting analysis.R
3. **ddEdgesMNRAS.rdata** example data from the Monthly Notices of the Royal Astronomical Society
4. **social network analysis example arxiv parsing.json** Zeppelin file that uses Spark to analyse the data above.

##Scraping the Arxiv Data
1. Run the analysis.R script. Define the various descriptives used for the different journals XML parsing using the Journal class e.g.

``` 
mnras <- new("Journal", base = "http://mnras.oxfordjournals.org/", extension = 'content/313/1.toc', 
			nextIssue = 'Next issue', abstract = '*abstract*', metaNodes= 'meta', metaNames = 'name', 
			metaContent = 'content',authorSearch = "^citation_author$",institutionSearch = "^citation_author_institution$",
			doiSearch="^citation_doi$",dateSearch="^citation_date$", emailSearch = "^citation_author_email$") 
```
             
You can select the appropriate start date for a journal as well as the number of journal pages that need to be read in using the 

*abstractLinksmnras <- getWebPageDataJournal(mnras,500)* 

function. This will return a list of webpages that link to the abstracts for all of the papers for the relevent journal.

After this we parse each of the abstracts using the 

*mnrasData<- parseAbstracts(mnras,abstractLinksmnras,5)*

Here we pass the journal definition class as well as the webpages for the abstracts. This returns a dataframe 

```
#[1] "author"       "affiliation1" "affiliation2" "affiliation3" "affiliation4" "affiliation5"
#[7] "doi"          "authorEmail"  "abstractURL"  "date"         "webpage"      "email"  
```

We run through the dataframe using 

```
mnrasData<-sortAffiliations(mnrasData)
```
This will clean up the Institutions, making sure that affiliations that have similar names will end up with identical names. It uses the *adist* function from the R *html* package.

> https://stat.ethz.ch/R-manual/R-devel/library/utils/html/adist.html 
> 
> Compute the approximate string distance between character vectors. The distance is a generalized Levenshtein distance, giving the minimal possibly weighted number of insertions, deletions and substitutions needed to transform one string into another.

It will ensure that Insitutions that are *close* to each other as calculated by the Levenshtein distance (LD) will be substituted for the first Institution name that appears. I define close as those that have 

LD < &#956; - 5&#963; 

Then we can create a edge list that can be read in by the Spark infrastructure in 

**social network analysis example arxiv parsing.json**

##Spark Data Analysis
Spark is used to read in the edge list created by the R script. From this I used the *graphframes* package to create a graph from the data. Nodes are defined as Institutions and the edges are defined as by co-authorship. I calculate the following properties

1. Pagerank
2. Clustering
3. Degree
4. In-degree
5. Out-degree

Once these are calculated they are output to a csv file for reading into Neo4J 

## Neo4J
Neo4J is a graph database the allows graphs to be visualised and interacted with. The data are stored on the DB together with the graph attributes calculated in Spark.

We can query the graph database with Cypher queries e.g.

*MATCH (n) where toFloat(n.pagerank)>2 RETURN n LIMIT 25*



