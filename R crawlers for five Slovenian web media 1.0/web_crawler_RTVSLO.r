# Web crawler for RTVSLO
# Author: Joze Bucar, Laboratory of Data Technologies, Faculty of Information Studies in Novo mesto, Slovenia (contact: joze.bucar@gmail.com)
# Last Update: 2016-02-14
#########################

# Libraries
#(.libPaths( c( .libPaths(), "~/userLibrary") )) # keep all packages in one library
#install.packages("stringr");install.packages("bitops");install.packages("RCurl");install.packages("XML");install.packages("RJSONIO");install.packages("gdata");install.packages("tm");install.packages("gsubfn");install.packages("plyr"); install.packages("car")
library(stringr); library("bitops"); library(RCurl); library(XML); library(RJSONIO); library(gdata); library(tm); library(gsubfn); library(plyr); library(car)

# Set working directory and create directory
sources = c("RTVSLO", "24UR", "DNEVNIK", "ZURNAL24", "FINANCE")
setwd("E:/")
#dir.create(sources[1]) # create directory RTVSLO

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

get.RTVSLO <- function(url){
  if (url.exists(url)==TRUE){
    setwd("E:/RTVSLO")
    #url = "http://www.rtvslo.si/gospodarstvo/marca-v-sloveniji-vec-potnikov/88316"
    #url = "http://www.rtvslo.si/gospodarstvo/facebookove-delnice-z-rekordom-evro-na-letosnjem-dnu/342681"
    
    thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
    
    if (grepl("<div class=\"social-contents\" style=\"display:none;\"></div>", thepage) == TRUE) {
      article = sapply(strsplit(thepage,"<div class=\"social-contents\" style=\"display:none;\"></div>",fixed = TRUE),"[[",2)
      article = sapply(strsplit(article,"<h1>",fixed = TRUE),"[[",2)
      article = sapply(strsplit(article,"<div class=\"clear\"></div>",fixed = TRUE),"[[",1)
    } else {
      article = ""
    }
    
    # get main URL
    url.main = substr(url, 8, 20)
    
    # get URL
    url = url
    
    # get title
    if (grepl("<meta name=\"title\" content=\"", thepage) == TRUE) {
      datalines.title = sapply(strsplit(thepage,"<meta name=\"title\" content=\"",fixed = TRUE),"[[",2)
      datalines.title = trim(sapply(strsplit(datalines.title,"\"/>",fixed = TRUE),"[[",1))
      title = trim(gsub("&lt;/b&gt;&lt;/p&gt;&lt;p&gt;", "", datalines.title))
      title = replace.HTML.char(title)
      title = gsub(' {2,}',' ',gsub('^ *| *$','',title)) # replace multiple spaces with one
      title_file = gsub("\t"," ", title); title_file = gsub("Ä‡","c", title); title_file = gsub("Ä†","C",title_file); title_file = gsub("ÄŤ","c",title_file); title_file = gsub("ÄŚ","C",title_file); title_file = gsub("Ä‘Â","dz",title_file); title_file = gsub("Ä","Dz",title_file); title_file = gsub("Ĺˇ","s",title_file); title_file = gsub("Ĺ ","S",title_file); title_file = gsub("Ĺľ","z",title_file); title_file = gsub("Ĺ˝","Z",title_file); title_file = gsub("[[:punct:]]","",title_file); title_file = gsub("â‚¬","eur",title_file)
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
      keywords = trim(sapply(strsplit(datalines.keywords,"\"/>",fixed = TRUE),"[[",1))
      keywords = replace.HTML.char(keywords)
      keywords = gsub(' {2,}',' ',gsub('^ *| *$','',keywords))
    } else {
      keywords = ""
    }
    
    # get date
    if (grepl("<div class=\"info\">", article) == TRUE) {
      datalines.date = sapply(strsplit(article,"<div class=\"info\">",fixed = TRUE),"[[",2)
      datalines.date = sapply(strsplit(datalines.date,"ob ",fixed = TRUE),"[[",1)
      datalines.date = read.table(textConnection(datalines.date))    
      if (nchar(datalines.date$V1) == 1) {
        date.day = paste("0", datalines.date$V1, sep = "")
      } else {
        date.day = datalines.date$V1
      }
      
      if (datalines.date$V2 == "januar" || datalines.date$V2 == "Januar") {
        date.month = paste("01", sep = "")
      } else if (datalines.date$V2 == "februar" || datalines.date$V2 == "Februar") {
        date.month = paste("02", sep = "")
      } else if (datalines.date$V2 == "marec" || datalines.date$V2 == "Marec") {
        date.month = paste("03", sep = "")
      } else if (datalines.date$V2 == "april" || datalines.date$V2 == "April") {
        date.month = paste("04", sep = "")
      } else if (datalines.date$V2 == "maj" || datalines.date$V2 == "Maj") {
        date.month = paste("05", sep = "")
      } else if (datalines.date$V2 == "junij" || datalines.date$V2 == "Junij") {
        date.month = paste("06", sep = "")
      } else if (datalines.date$V2 == "julij" || datalines.date$V2 == "Julij") {
        date.month = paste("07", sep = "")
      } else if (datalines.date$V2 == "avgust" || datalines.date$V2 == "Avgust") {
        date.month = paste("08", sep = "")
      } else if (datalines.date$V2 == "september" || datalines.date$V2 == "September") {
        date.month = paste("09", sep = "")
      } else if (datalines.date$V2 == "oktober" || datalines.date$V2 == "Oktober") {
        date.month = paste("10", sep = "")
      } else if (datalines.date$V2 == "november" || datalines.date$V2 == "November") {
        date.month = paste("11", sep = "")
      } else if (datalines.date$V2 == "december" || datalines.date$V2 == "December") {
        date.month = paste("12", sep = "")
      } else {
        datalines.date$V2 = datalines.date$V2
      }
      date.year = datalines.date$V3
      date = paste(date.day, ".", date.month, ".", date.year, sep = "")
    } else {
      date.day = ""; date.month = ""; date.year = ""
      date = ""
    }
    
    # get author
    if (grepl("<div id=\"author\">", article) == TRUE) {
      datalines.author = sapply(strsplit(article,"<div id=\"author\">",fixed = TRUE),"[[",2)
      datalines.author = sapply(strsplit(datalines.author,"</div>",fixed = TRUE),"[[",1)
      datalines.author = gsub("<.*?>", "", datalines.author)
      author = trim(gsub("\t", "", datalines.author))
      author = gsub("[()]", "", author)
      author = replace.HTML.char(author)
      author = gsub(' {2,}',' ',gsub('^ *| *$','',author))
    } else {
      author = ""
    }
    
    # get summary
    if (grepl("<div class=\"sub\">", article) == TRUE) {
      datalines.summary = sapply(strsplit(thepage,"<div class=\"sub\">",fixed = TRUE),"[[",2)
      datalines.summary = sapply(strsplit(datalines.summary,"</div>",fixed = TRUE),"[[",1)
      datalines.summary = gsub("\t", "", datalines.summary)
      summary = trim(gsub("<.*?>", "", datalines.summary))
      summary = replace.HTML.char(summary)
      summary = gsub(' {2,}',' ',gsub('^ *| *$','',summary)) 
    } else {
      summary = ""
    }
    
    # get content
    datalines.content = gsub("<P>", "<p>", article)
    datalines.content = gsub("</P>", "</p>", datalines.content)
    datalines.content = gsub("<P class=MsoNormal>", "<p>", datalines.content)
    datalines.content = gsub("<p class=MsoNormal>", "<p>", datalines.content)
    datalines.content = gsub("<BR>", "<br>", datalines.content)
    datalines.content = gsub("<TD>", "<td>", datalines.content)
    datalines.content = gsub("</TD>", "</td>", datalines.content)
    
    if (grepl("<p>", datalines.content) == TRUE) {
      if (grepl("<div id=\"author\">", datalines.content) == TRUE) {
        datalines.content = sapply(strsplit(datalines.content,"<div id=\"author\">",fixed = TRUE),"[[",1)
      }
      datalines.content = sapply(strsplit(datalines.content,"<div class=\"info\">",fixed = TRUE),"[[",2)
      datalines.content = gsub("<P>", "<p>", datalines.content)
      remove=sapply(strsplit(datalines.content,"<p>",fixed = TRUE),"[[",1)
      datalines.content = gsub(remove,"", datalines.content)
      
      datalines.content = unlist(strsplit(datalines.content, "<p>"))
      datalines.content = unlist(strsplit(datalines.content, "</p>"))
      datalines.content = unlist(strsplit(datalines.content, "</ p>"))
      datalines.content = unlist(strsplit(datalines.content, "<br>"))
      datalines.content = unlist(strsplit(datalines.content, "<br/>"))
      datalines.content = unlist(strsplit(datalines.content, "<br />"))
      datalines.content = unlist(strsplit(datalines.content, "<td>"))
      datalines.content = unlist(strsplit(datalines.content, "</td>"))
      datalines.content = unlist(strsplit(datalines.content, "</ td>"))
      
      datalines.content = replace.HTML.char(datalines.content)
      
      datalines.content = gsub("<.*?>", "", datalines.content)
      datalines.content = trim(datalines.content)
      content <- datalines.content[sapply(datalines.content, nchar) > 0]
      content = gsub(' {2,}',' ',gsub('^ *| *$','',content))
      if (author == ""){
        author = content[length(content)]
        if(nchar(content[length(content)]) < 20){
          content = content[-length(content)]
        }
      }    
    } else {
      content = ""
    }
    
    # cat
    if (nchar(content) > 0) {
      file = sprintf("%s_%s_%s_%s_%s.txt", sources[1], date.year, date.month, date.day, gsub("[[:punct:]]", "", title_file))
      file = gsub(' {2,}',' ',gsub('^ *| *$','',file))
      sink(file)
      cat("# URL main: \n", url.main, "\n", "# URL: \n", url, "\n", "# Date: \n", date, "\n", "# Author: \n", author, "\n", "# Keywords: \n", keywords, "\n", "# Title: \n", title, "\n", "# Summary: \n", summary, "\n", "# Content: \n", sep = "")
      for (i in 1:length(content)){
        cat(content[i])
        cat("\n")
      }
      sink()
    }
  }
}

all.pages = NULL # set for (j in 151:151);for (j in 150:150); ...
### RUN ONE FOR LOOP AFTER ANOTHER !!!
for (j in 675:651){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 650:601){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 600:551){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 550:501){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 500:451){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 450:401){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 400:351){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 350:301){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 300:275){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 275:251){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 250:201){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 200:151){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 150:101){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 100:51){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
for (j in 50:0){
  all.pages = NULL
  
  url = sprintf("http://www.rtvslo.si/gospodarstvo/arhiv/?date_from=2002-01-01&date_to=2016-02-15&page=%s",j) # page=151 ... page=150 ... page=0
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<div class=\"body section_list\" id=\"cnt_\">", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<div class=\"body section_list\" id=\"cnt_\">",fixed = TRUE),"[[",2)
    archive = sapply(strsplit(archive,"<div class=\"rpagin\">",fixed = TRUE),"[[",1)
    pages = strsplit(archive, "<a href=\"")[[1]]
    pages = sapply(strsplit(pages,"\" class=\"img\">",fixed = TRUE),"[[",1)
    st = seq(from=2, to=length(pages), by=2)
    pages = pages[st]
    pages = sprintf("%s%s", "http://www.rtvslo.si", pages)
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/slovenija-gre-spet-po-denar/332822", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/ruska-jabolka-bodo-ponovno-razdeljena-humanitarnim-organizacijam/371658", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/gospodarstvo/do-konca-leta-resitev-za-lipico-turizem/377896", "", all.pages) # error on this page
    all.pages = gsub("http://www.rtvslo.si/moja-generacija/novice/slovenija/starejsi-od-50-let-predstavljajo-dobro-tretjino-vseh-brezposelnih/380323", "", all.pages) # error on this page
  }
  
  for(i in 1:length(all.pages)){
    get.RTVSLO(all.pages[i])
  }
}
#get.RTVSLO("http://www.rtvslo.si/gospodarstvo/stavke-v-muri-vendarle-ne-bo/97456")
#get.RTVSLO("http://www.rtvslo.si/gospodarstvo/scenarij-prenove-tudi-pri-hildi-tovsak/228000")

#setwd("C:/Users/Joze Bucar/Desktop/")
#get.RTVSLO("http://www.rtvslo.si/gospodarstvo/borzni-komentar/goldman-sachs-ne-verjame-v-nadaljnjo-rast-zlata/332783")
#get.RTVSLO("http://www.rtvslo.si/uspesna-slovenija/klima-ki-v-celotni-sezoni-porabi-le-za-evro-elektrike/356146")
#get.RTVSLO("http://www.rtvslo.si/gospodarstvo/surs-2-9-odstotna-gospodarska-rast-v-prvih-treh-mesecih/366228")
#get.RTVSLO("http://www.rtvslo.si/gospodarstvo/marca-v-sloveniji-vec-potnikov/88316")
