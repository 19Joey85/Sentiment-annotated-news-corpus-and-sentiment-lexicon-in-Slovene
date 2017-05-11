# Web crawler for Dnevnik
# Author: Joze Bucar, Laboratory of Data Technologies, Faculty of Information Studies in Novo mesto, Slovenia (contact: joze.bucar@gmail.com)
# Last Update: 2016-02-14
#########################
# tutorial RSelenium!!! http://ropensci.github.io/RSelenium/

# Libraries
#(.libPaths( c( .libPaths(), "~/userLibrary") )) # keep all packages in one library
#install.packages("stringr");install.packages("bitops");install.packages("RCurl");install.packages("XML");install.packages("RJSONIO");install.packages("gdata");install.packages("tm");install.packages("gsubfn");install.packages("plyr"); install.packages("car"); install.packages("httr"); install.packages("RSelenium")
require(stringr); require("bitops"); require(RCurl); require(XML); require(RJSONIO); require(gdata); require(tm); require(gsubfn); require(plyr); require(car); require(httr); require(RSelenium)

# Set working directory and create directory
sources = c("RTVSLO", "24UR", "DNEVNIK", "ZURNAL24", "FINANCE")
setwd("E:/")
#dir.create(sources[3]) # create directory Dnevnik

# Set options
options(stringsAsFactors = FALSE) # setting options

# Replace HTML special character
gsub2 <- function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, ...)
  x
}

replace.HTML.char <- function(datalines.content){
  from <- c("&Agrave;","&agrave;","&Aacute;","&aacute;","&Acirc;","&acirc;","&Atilde;","&atilde;","&Auml;","&auml;","&Aring;","&aring;","&AElig;","&aelig;","&ordf;","&Ccedil;","&ccedil;","&Egrave;","&egrave;","&Eacute;","&eacute;","&Ecirc;","&ecirc;","&Euml;","&euml;","&fnof;","&Igrave;","&igrave;","&Iacute;","&iacute;","&Icirc;","&icirc;","&Iuml;","&iuml;","&Ntilde;","&ntilde;","&Ograve;","&ograve;","&Oacute;","&oacute;","&Ocirc;","&ocirc;","&Otilde;","&otilde;","&Ouml;","&ouml;","&Oslash;","&oslash;","&Scaron;","&scaron;","&szlig;","&Ugrave;","&ugrave;","&Uacute;","&uacute;","&Ucirc;","&ucirc;","&Uuml;","&uuml;","&Yuml;","&yuml;","&Yacute;","&yacute;","&frac14;","&frac12;","&frac34;","&sup1;","&sup2;","&sup3;","&deg;","&iexcl;","&amp;","&iquest;","&brvbar;","&bull;","&ndash;","&mdash;","&trade;","&copy;","&reg;","&euro;","&cent;","&pound;","&curren;","&yen;","&quot;","&lsquo;","&rsquo;","&ldquo;","&rdquo;","&acute;","&tilde;","&circ;","&lsaquo;","&rsaquo;","&laquo;","&raquo;","&times;","&divide;","&plusmn;","&sbquo;","&bdquo;","&hellip;","&dagger;","&Dagger;","&permil;","&OElig;","&oelig;","&sect;","&uml;","&not;","&shy;","&micro;","&para;","&middot;","&cedil;","&ordm;","&ETH;","&THORN;","&eth;","&thorn;","&nbsp;","&spades;","&clubs;","&hearts;","&diams;","&oline;","&larr;","&uarr;","&rarr;","&darr;","&brkbar;","&die;","$amp,","ĂĽ","Ĺ ","Ĺˇ","â€","&#8220;","&#8221;","â€ť","â€ś","<U+2028>")
  to <- c("À","à","Á","á","Â","â","Ã","ã","Ä","ä","Å","å","Æ","æ","ª","Ç","ç","È","è","É","é","Ê","ê","Ë","ë","ƒ","Ì","ì","Í","í","Î","î","Ï","ï","Ñ","ñ","Ò","ò","Ó","ó","Ô","ô","Õ","õ","Ö","ö","Ø","ø","Š","š","ß","Ù","ù","Ú","ú","Û","û","Ü","ü","Ÿ","ÿ","Ý","ý","1/4","1/2","3/4","¹","²","³","°","¡","&","¿","¦","•","–","—","™","©","®","€","¢","£","¤","¥","\"","‘","’","“","”","´","˜","ˆ","‹","›","«","»","×","÷","±","‚","„","…","†","‡","‰","Œ","œ","§","¨","¬"," ","µ","¶","·","¸","º","Ð","Þ","ð","þ"," ","♠","♣","♥","♦","‾","←","↑","→","↓","¦","¨","","ü","Š","š","-","","","","","")
  datalines.content = gsub2(from, to, datalines.content)
  datalines.content = gsub("&lt;", "<", datalines.content)
  datalines.content = gsub("&gt;", ">", datalines.content)
  #datalines.content = gsub("[:]", " ", datalines.content)
  #datalines.content = gsub("[()]", "", datalines.content)
  #datalines.content = gsub("[)]", "", datalines.content)
  #datalines.content = gsub("[()]", "", datalines.content)
  #datalines.content = trim(datalines.content)
  datalines.content = gsub(' {2,}',' ',gsub('^ *| *$','',datalines.content)) # replace multiple spaces with one
}

checkForServer()
startServer()
remDr <- remoteDriver(browserName = "chrome")
#remDr <- remoteDriver(browserName = "phantomjs")
#remDr <- remoteDriver(remoteServerAddr = "localhost",port = 4444,browserName = "chrome")
#remDr <- remoteDriver(browserName = "phantomjs")
remDr$open(silent = TRUE)
remDr$navigate("https://www.dnevnik.si/posel/novice?p=322") # manually login #joze.bucar@gmail.com #hxik55894i

#remDr$close()

get.DNEVNIK <- function(url){
  if (url.exists(url)==TRUE){
    setwd("E:/DNEVNIK")
    thepage=NULL
    #url = "https://www.dnevnik.si/1042618272/posel/novice/musarjevi-zavladali-druzbi"
    #url="https://www.dnevnik.si/1042730191/posel/novice/za-te-brestanica-bo-skrbel-stenmark-"
    #url="https://www.dnevnik.si/265706/posel/novice/265706"
    #url="https://www.dnevnik.si/266123/posel/novice/266123"
    remDr$navigate(url)
    Sys.sleep(10)
    #setImplicitWaitTimeout(milliseconds = 10000) #ali setTimeout(type = "page load", milliseconds = 10000)
    thepage = remDr$getPageSource()[[1]]
    
    # get main URL
    url.main = substr(url, 9, 22)
    
    # get URL
    url = url
    
    # get title
    if (grepl("<title>", thepage) == TRUE) {
      datalines.title = sapply(strsplit(thepage,"</title>",fixed = TRUE),"[[",1)
      datalines.title = sapply(strsplit(datalines.title,"<title>",fixed = TRUE),"[[",2)
      datalines.title = trim(gsub("[|] Dnevnik","",datalines.title))
      title = trim(gsub("&lt;/b&gt;&lt;/p&gt;&lt;p&gt;", "", datalines.title))
      title = gsub("\t"," ", title)
      title = replace.HTML.char(title)
      title = gsub(' {2,}',' ',gsub('^ *| *$','',title)) # replace multiple spaces with one
      title_file = gsub("ć","c", title); title_file = gsub("Ć","C",title_file); title_file = gsub("č","c",title_file); title_file = gsub("Č","C",title_file); title_file = gsub("đ","dz",title_file); title_file = gsub("Đ","Dz",title_file); title_file = gsub("š","s",title_file); title_file = gsub("Š","S",title_file); title_file = gsub("ž","z",title_file); title_file = gsub("Ž","Z",title_file); title_file = gsub("[[:punct:]]","",title_file); title_file = gsub("€","eur",title_file)
      title_file=gsub("[^[:alnum:] ]", "", title_file)
      title_file = gsub(' {2,}',' ',gsub('^ *| *$','',title_file)) # replace multiple spaces with one
      if (nchar(title) >= 50) {
        title_file = substring(title_file,1,50)
      }
    } else {
      title = ""
      title_file = ""
    }
    
    # get keywords
    if (grepl("<meta name=\"keywords\" content=\"", thepage) == TRUE) {
      datalines.keywords = sapply(strsplit(thepage,"<meta name=\"keywords\" content=\"",fixed = TRUE),"[[",2)
      keywords = trim(sapply(strsplit(datalines.keywords,"\" />",fixed = TRUE),"[[",1))
      keywords = replace.HTML.char(keywords)
      keywords = gsub(' {2,}',' ',gsub('^ *| *$','',keywords))
    } else {
      keywords = ""
    }
    
    # get date
    if (grepl("<div class=\"dtstamp\" title=\"", thepage) == TRUE) {
      datalines.date = sapply(strsplit(thepage,"<div class=\"dtstamp\" title=\"",fixed = TRUE),"[[",2)
      datalines.date = sapply(strsplit(datalines.date,"T",fixed = TRUE),"[[",1)
      date.year=substr(datalines.date, 1, 4)
      date.month=substr(datalines.date, 6, 7)
      date.day=substr(datalines.date, 9, 10)
      date = paste(date.day, ".", date.month, ".", date.year, sep = "")
    } else {
      date.day = ""; date.month = ""; date.year = ""
      date = ""
    }
    
    # get author
    if (grepl("<div class=\"article-source\">", thepage) == TRUE) {
      datalines.author = trim(sapply(strsplit(thepage,"<div class=\"article-source\">",fixed = TRUE),"[[",2))
      datalines.author = trim(sapply(strsplit(datalines.author,"</div>",fixed = TRUE),"[[",1))
      datalines.author = trim(sapply(strsplit(datalines.author,">",fixed = TRUE),"[[",2))
      datalines.author = trim(sapply(strsplit(datalines.author,"<",fixed = TRUE),"[[",1))
      datalines.author = gsub("<.*?>", "", datalines.author)
      author = gsub("[()]", "", datalines.author)
      author = replace.HTML.char(author)
      author = gsub(' {2,}',' ',gsub('^ *| *$','',author))
    } else if (grepl("<span style=\"font-weight: bold;\">", thepage) == TRUE) {
      datalines.author = trim(sapply(strsplit(thepage,"<span style=\"font-weight: bold;\">",fixed = TRUE),"[[",2))
      datalines.author = trim(sapply(strsplit(datalines.author,"</span>",fixed = TRUE),"[[",1))
      datalines.author = gsub("<.*?>", "", datalines.author)
      author = gsub("[()]", "", datalines.author)
      author = replace.HTML.char(author)
      author = gsub(' {2,}',' ',gsub('^ *| *$','',author))
      } else if (grepl("<span style=\"FONT-WEIGHT: bold\">", thepage) == TRUE) {
        datalines.author = trim(sapply(strsplit(thepage,"<span style=\"FONT-WEIGHT: bold\">",fixed = TRUE),"[[",2))
        datalines.author = trim(sapply(strsplit(datalines.author,"</span>",fixed = TRUE),"[[",1))
        datalines.author = gsub("<.*?>", "", datalines.author)
        author = gsub("[()]", "", datalines.author)
        author = replace.HTML.char(author)
        author = gsub(' {2,}',' ',gsub('^ *| *$','',author))
      } else {
      author = ""
    }
    
    summary=""
    # get summary
    if (grepl("<p class=\"articlesupertext\">", thepage) == TRUE) {
      datalines.summary = sapply(strsplit(thepage,"<p class=\"articlesupertext\">",fixed = TRUE),"[[",2)
      datalines.summary = sapply(strsplit(datalines.summary,"<div class=\"articlerunningtext\">",fixed = TRUE),"[[",1)
      datalines.summary = unlist(strsplit(datalines.summary, "<p>"))
      datalines.summary = gsub("<.*?>", "", datalines.summary)
      datalines.summary = trim(datalines.summary)
      datalines.summary <- datalines.summary[sapply(datalines.summary, nchar) > 0]
      datalines.summary = replace.HTML.char(datalines.summary)
      datalines.summary = gsub("\n", "", datalines.summary)
      datalines.summary = gsub(' {2,}',' ',gsub('^ *| *$','',datalines.summary)) # replace multiple spaces with one
      datalines.summary = trim(datalines.summary)
      summary = gsub(' {2,}',' ',gsub('^ *| *$','',datalines.summary))
      summary=summary[summary != ""]
      } else {
        summary = ""
      }
    
    content=""
    # get content
    if (grepl("<div class=\"articlerunningtext\">", thepage)) {
      datalines.content = trim(sapply(strsplit(thepage,"<div class=\"articlerunningtext\">",fixed = TRUE),"[[",2))
      datalines.content = trim(sapply(strsplit(datalines.content,"<div class=\"viewbtn\">",fixed = TRUE),"[[",1))
      #datalines.content = trim(sapply(strsplit(datalines.content,"</div>",fixed = TRUE),"[[",1))
      datalines.content = unlist(strsplit(datalines.content, "<p>"))
      datalines.content = gsub("<.*?>", "", datalines.content)
      datalines.content = trim(datalines.content)
      datalines.content <- datalines.content[sapply(datalines.content, nchar) > 0]
      datalines.content = replace.HTML.char(datalines.content)
      datalines.content = gsub("\n", "", datalines.content)
      datalines.content = gsub(' {2,}',' ',gsub('^ *| *$','',datalines.content)) # replace multiple spaces with one
      datalines.content = trim(datalines.content)
      if (grepl("Fotografije »", datalines.content)) {
        up = trim(sapply(strsplit(datalines.content,"Fotografije »",fixed = TRUE),"[[",1))
        down = trim(sapply(strsplit(datalines.content,"api_descriptions), //} }",fixed = TRUE),"[[",2))
        datalines.content=paste(up," ",down)
      }
      content = gsub(' {2,}',' ',gsub('^ *| *$','',datalines.content))
      content=content[content != ""]
      if(length(content)==0){
        content=""
      }
      } else {
      content = ""
    }
    
    #     if ((content == "") && (grepl("<p class=\"intro-box\">", thepage) == TRUE)) {
    #       datalines.content = trim(sapply(strsplit(thepage,"<p class=\"intro-box\">",fixed = TRUE),"[[",2))
    #       datalines.content = trim(sapply(strsplit(datalines.content,"<div class=\"article\">",fixed = TRUE),"[[",1))
    #       datalines.content = gsub("</div>", "", datalines.content)
    #       datalines.content = unlist(strsplit(datalines.content, "<p>"))
    #       datalines.content = gsub("<.*?>", "", datalines.content)
    #       datalines.content = trim(datalines.content)
    #       datalines.content <- datalines.content[sapply(datalines.content, nchar) > 0]
    #       datalines.content = replace.HTML.char(datalines.content)[-1]
    #       content = gsub(' {2,}',' ',gsub('^ *| *$','',datalines.content))
    #     }
    
    # cat
    if (nchar(content) > 0) {
      if (title_file == "Posel Novice"){
        id = unlist(strsplit(url, "/"))    
        id = gsub("[[:punct:]]","", id[length(unlist(strsplit(url, "/")))])
        file = sprintf("%s_%s_%s_%s_%s_%s.txt", sources[3], date.year, date.month, date.day, gsub("[[:punct:]]", "", title_file), id)
        file = gsub(' {2,}',' ',gsub('^ *| *$','',file))
      } else {
        file = sprintf("%s_%s_%s_%s_%s.txt", sources[3], date.year, date.month, date.day, gsub("[[:punct:]]", "", title_file))
        file = gsub(' {2,}',' ',gsub('^ *| *$','',file))
      }
      sink(file)
      cat("# URL main: \n", url.main, "\n", "# URL: \n", url, "\n", "# Date: \n", date, "\n", "# Author: \n", author, "\n", "# Keywords: \n", keywords, "\n", "# Title: \n", title, "\n", "# Summary: \n", summary, "\n", "# Content: \n", sep = "")
      for (i in 1:length(content)){
        cat(content[i])
        cat("\n")
      }
      sink()
    }
  }
  print(title)
}

all.pages = NULL #325:1
for (i in 3950:1){ # set pages in archive, set dates (to get articles from 1.9.2007 to 31.12.2013)
  # i=325 od 1.1.2014 do 31.1.2016
  # i=3950 od 1.9.2007 do 31.1.2016
  #for (i in 3760:1) # per partes!!!
  #to get all news from Dnevink archive: for (i in 4855:1)
  print("START SESSION")
  print(Sys.time())
  all.pages = NULL
  #url2="http://www.dnevnik.si/posel/novice?p=312"
  url2 = sprintf("http://www.dnevnik.si/posel/novice?p=%s", i)
  Sys.sleep(10)
  #setImplicitWaitTimeout(milliseconds = 10000)
  
  remDr$navigate(url2)
  thepage2 = remDr$getPageSource()[[1]]
  
  if (grepl("<div class=\"search-results\">", thepage2) == TRUE) {
    archive = trim(sapply(strsplit(thepage2,"<div class=\"search-results\">",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pager pagging\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<div class=\"search-result-item\">",fixed = TRUE))[-1] # split string and unlist list to get links
    archive = trim(sapply(strsplit(archive,"<a href=",fixed = TRUE),"[[",2))
    archive = trim(gsub("\"/","/", archive))
    archive = trim(gsub("\'/","/", archive))
    pages = trim(sapply(strsplit(archive,"\"",fixed = TRUE),"[[",1))
    #archive = unlist(strsplit(archive,"<a href=",fixed = TRUE)) # split string and unlist list to get links
    #archive = trim(sapply(strsplit(archive,"\">Več",fixed = TRUE),"[[",1))
    #archive = trim(sapply(strsplit(archive,"\">",fixed = TRUE),"[[",1))
    #archive = trim(gsub("\""," ", archive))
    #archive = trim(gsub("\'"," ", archive))
    #archive = archive[seq(1, length(archive), 4)] # extract every 4rd element of a vector
    #archive = trim(sapply(strsplit(archive,"class= search-result-thumbnail",fixed = TRUE),"[[",1))
    #pages = sapply(strsplit(archive,"\" class=",fixed = TRUE),"[[",1)
    pages = sprintf("%s%s", "http://www.dnevnik.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    #get.DNEVNIK(all.pages)
    
    print("START")
    print(i)
    for(m in 1:length(all.pages)){
      print(m)
      get.DNEVNIK(all.pages[m])
    }
    
    #sapply(get.DNEVNIK, all.pages)
  }
  print(Sys.time())
  print("FINISH")
  print(i)
}

#setwd("E:/")
#get.DNEVNIK("http://www.dnevnik.si/posel/novice/266119")
#get.DNEVNIK("http://www.dnevnik.si/posel/novice/ajdovski-gradbeni-veljak-znova-na-prostosti")
#get.DNEVNIK("http://www.dnevnik.si/posel/novice/276119")
#get.DNEVNIK("http://www.dnevnik.si/posel/novice/336119")
#get.DNEVNIK("http://www.dnevnik.si/posel/novice/1042254498")
#get.DNEVNIK("http://www.dnevnik.si/posel/novice/1042300943")
#get.DNEVNIK("http://www.dnevnik.si/posel/novice/1042259794")
#get.DNEVNIK("https://www.dnevnik.si/1042618371/posel/novice/zapustila-jih-je-vec-kot-cetrtina-clanov")
#i=3950 error
#get.DNEVNIK("https://www.dnevnik.si/265636/posel/novice/265636")
#get.DNEVNIK("https://www.dnevnik.si/265634/posel/novice/265634")
