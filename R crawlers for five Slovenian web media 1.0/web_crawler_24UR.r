# Web crawler for 24UR
# Author: Joze Bucar, Laboratory of Data Technologies, Faculty of Information Studies in Novo mesto, Slovenia (contact: joze.bucar@gmail.com)
# Last Update: 2016-02-14
#########################

# Libraries
#(.libPaths( c( .libPaths(), "~/userLibrary") )) # keep all packages in one library
#install.packages("stringr");install.packages("bitops");install.packages("RCurl");install.packages("XML");install.packages("RJSONIO");install.packages("gdata");install.packages("tm");install.packages("gsubfn");install.packages("plyr"); install.packages("car")
library(bitops); library(car); library(gsubfn); library(gdata); library(plyr); library(RCurl); library(RJSONIO); library(stringr); library(tm); library(XML)

# Set working directory and create directory
sources = c("RTVSLO", "24UR", "DNEVNIK", "ZURNAL24", "FINANCE")
setwd("E:/")
#dir.create(sources[2]) # create directory 24UR

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

ptm1 = proc.time()
get.24UR <- function(url){
  if (url.exists(url)==TRUE){
    setwd("E:/24UR")
    #url = "http://www.24ur.com/chelsea-iznicil-zaostanek-s-prve-tekme-real-se-je-igral-z-ognjem.html"
    #url = "http://www.24ur.com/novice/gospodarstvo/vstop-v-svet-polnega-formata-canon-eos-6d.html"
    thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
    
    # get main URL
    url.main = substr(url, 8, 19)
    
    # get URL
    url = url
    
    # get title
    if (grepl("<title>", thepage) == TRUE) {
      datalines.title = trim(sapply(strsplit(thepage,"<!-- content -->",fixed = TRUE),"[[",1))
      datalines.title = sapply(strsplit(thepage,"<title>",fixed = TRUE),"[[",2)
      datalines.title = trim(sapply(strsplit(datalines.title,"</title>",fixed = TRUE),"[[",1))
      datalines.title = trim(gsub("24ur.com - ", "", datalines.title))
      title = trim(gsub("&lt;/b&gt;&lt;/p&gt;&lt;p&gt;", "", datalines.title))
      title = replace.HTML.char(title)
      title = gsub(' {2,}',' ',gsub('^ *| *$','',title)) # replace multiple spaces with one
      title_file = gsub("\t"," ", title); title_file = gsub("ć","c", title); title_file = gsub("Ć","C",title_file); title_file = gsub("č","c",title_file); title_file = gsub("Č","C",title_file); title_file = gsub("đ","dz",title_file); title_file = gsub("Đ","Dz",title_file); title_file = gsub("š","s",title_file); title_file = gsub("Š","S",title_file); title_file = gsub("ž","z",title_file); title_file = gsub("Ž","Z",title_file); title_file = gsub("[[:punct:]]","",title_file); title_file = gsub("€","eur",title_file)
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
    if (grepl("<div class=\"containerLeftSide\">", thepage) == TRUE) {
      datalines.date.author = sapply(strsplit(thepage,"<div class=\"containerLeftSide\">",fixed = TRUE),"[[",2)
      datalines.date.author = sapply(strsplit(datalines.date.author,"<h3>",fixed = TRUE),"[[",2); datalines.date.author
      datalines.date.author = sapply(strsplit(datalines.date.author,"</h3>",fixed = TRUE),"[[",1); datalines.date.author
      #datalines.date = sapply(strsplit(datalines.date.author,",",fixed = TRUE),"[[",2); datalines.date
      #datalines.date = trim(sapply(strsplit(datalines.date,",",fixed = TRUE),"[[",1)); datalines.date
      datalines.date = trim(strsplit(datalines.date.author,",",fixed = TRUE))
      
      datalines.date = strapply(datalines.date, "\\d+", as.numeric, simplify = TRUE)
      date.day = datalines.date[1]
      if (nchar(date.day) == 1) {
        date.day = paste("0", date.day, sep = "")
      } else {
        date.day = date.day
      }
      date.month = datalines.date[2]
      if (nchar(date.month) == 1) {
        date.month = paste("0", date.month, sep = "")
      } else {
        date.month = date.month
      }
      date.year = datalines.date[3]
      date = paste(date.day, ".", date.month, ".", date.year, sep = "")
    } else {
      date.day = ""; date.month = ""; date.year = ""
      date = ""
    }
    
    # get author
    if (grepl("|", datalines.date.author) == TRUE) {
      datalines.author = trim(sapply(strsplit(datalines.date.author,"|",fixed = TRUE),"[[",2))
      datalines.author = gsub("<.*?>", "", datalines.author)
      author = gsub("[()]", "", datalines.author)
      author = replace.HTML.char(author)
      author = gsub(' {2,}',' ',gsub('^ *| *$','',author))
    } else {
      author = ""
    }
    
    # get summary
    if (grepl("<!-- summary -->", thepage) == TRUE) {
      datalines.summary = sapply(strsplit(thepage,"<!-- summary -->",fixed = TRUE),"[[",2)
      datalines.summary = sapply(strsplit(datalines.summary,"</div>",fixed = TRUE),"[[",1)
      datalines.summary = gsub("\t", "", datalines.summary)
      datalines.summary = trim(gsub("<.*?>", "", datalines.summary))
      datalines.summary = replace.HTML.char(datalines.summary)
      summary = gsub(' {2,}',' ',gsub('^ *| *$','',datalines.summary))    
    } else {
      summary = ""
    }
    
    # get content
    if (grepl("<!-- content -->", thepage) == TRUE) {
      datalines.content = trim(sapply(strsplit(thepage,"<!-- content -->",fixed = TRUE),"[[",2))
      datalines.content = trim(sapply(strsplit(datalines.content,"<!-- content end -->",fixed = TRUE),"[[",1))
      datalines.content = trim(sapply(strsplit(datalines.content,"<div class=\"div_banner_H\"><div id=\"div_banner_H\"></div></div>",fixed = TRUE),"[[",2))
      if (grepl("<title>", datalines.content) == TRUE) {
        datalines.content = sapply(strsplit(datalines.content,"<title>",fixed = TRUE),"[[",2)
      }
      
      datalines.content = gsub("< p>", "<p>", datalines.content)
      datalines.content = gsub("<p >", "<p>", datalines.content)
      datalines.content = gsub("< p >", "<p>", datalines.content)
      datalines.content = gsub("< /p>", "</p>", datalines.content)
      datalines.content = gsub("</p >", "</p>", datalines.content)
      datalines.content = gsub("< /p >", "</p>", datalines.content)
      datalines.content = gsub("<P>", "<p>", datalines.content)
      datalines.content = gsub("< P>", "<p>", datalines.content)
      datalines.content = gsub("<P >", "<p>", datalines.content)
      datalines.content = gsub("< P >", "<p>", datalines.content)
      datalines.content = gsub("</P>", "</p>", datalines.content)
      datalines.content = gsub("< /P>", "</p>", datalines.content)
      datalines.content = gsub("</P >", "</p>", datalines.content)
      datalines.content = gsub("</P >", "</p>", datalines.content)
      datalines.content = gsub("<P class=MsoNormal>", "<p>", datalines.content)
      datalines.content = gsub("<p class=MsoNormal>", "<p>", datalines.content)
      datalines.content = gsub("<p class=\"MsoNormal\">", "<p>", datalines.content)
      datalines.content = gsub("<P align=left>", "<p>", datalines.content)
      datalines.content = gsub("<p align=left>", "<p>", datalines.content)
      datalines.content = gsub("<P class=MsoNormal style=\"MARGIN: 0cm 0cm 0pt\">", "<p>", datalines.content)
      datalines.content = gsub("<h4>", "<p>", datalines.content)
      datalines.content = gsub("< h4>", "<p>", datalines.content)
      datalines.content = gsub("<h4 >", "<p>", datalines.content)
      datalines.content = gsub("< h4 >", "<p>", datalines.content)
      datalines.content = gsub("</h4>", "<p>", datalines.content)
      datalines.content = gsub("< /h4>", "<p>", datalines.content)
      datalines.content = gsub("</h4 >", "<p>", datalines.content)
      datalines.content = gsub("< /h4 >", "<p>", datalines.content)
      datalines.content = gsub("<q>", "<p>", datalines.content)
      datalines.content = gsub("< q>", "<p>", datalines.content)
      datalines.content = gsub("<q >", "<p>", datalines.content)
      datalines.content = gsub("< q >", "<p>", datalines.content)
      datalines.content = gsub("</q>", "<p>", datalines.content)
      datalines.content = gsub("< /q>", "<p>", datalines.content)
      datalines.content = gsub("</q >", "<p>", datalines.content)
      datalines.content = gsub("< /q >", "<p>", datalines.content)
      datalines.content = gsub("<blockquote>", "<p>", datalines.content)
      datalines.content = gsub("/<blockquote>", "<p>", datalines.content)
      
      #### !!! remove pictures in case there are 10 pictures
      #     if (grepl("<span class=\"gray\">", datalines.content) == TRUE) {
      #       remove1 = sapply(strsplit(datalines.content,"<span class=\"gray\">",fixed = TRUE),"[[",2); remove1 = sapply(strsplit(remove1,"</span>",fixed = TRUE),"[[",1); datalines.content = gsub(remove1, "", datalines.content)}
      #     if (grepl("<span class=\"gray\">", datalines.content) == TRUE) {
      #       remove2 = sapply(strsplit(datalines.content,"<span class=\"gray\">",fixed = TRUE),"[[",2); remove2 = sapply(strsplit(remove2,"</span>",fixed = TRUE),"[[",1); datalines.content = gsub(remove2, "", datalines.content)}
      #     if (grepl("<span class=\"gray\">", datalines.content) == TRUE) {
      #       remove3 = sapply(strsplit(datalines.content,"<span class=\"gray\">",fixed = TRUE),"[[",2); remove3 = sapply(strsplit(remove3,"</span>",fixed = TRUE),"[[",1); datalines.content = gsub(remove3, "", datalines.content)}
      #     if (grepl("<span class=\"gray\">", datalines.content) == TRUE) {
      #       remove4 = sapply(strsplit(datalines.content,"<span class=\"gray\">",fixed = TRUE),"[[",2); remove4 = sapply(strsplit(remove4,"</span>",fixed = TRUE),"[[",1); datalines.content = gsub(remove4, "", datalines.content)}
      #     if (grepl("<span class=\"gray\">", datalines.content) == TRUE) {
      #       remove5 = sapply(strsplit(datalines.content,"<span class=\"gray\">",fixed = TRUE),"[[",2); remove5 = sapply(strsplit(remove5,"</span>",fixed = TRUE),"[[",1); datalines.content = gsub(remove5, "", datalines.content)}
      #     if (grepl("<span class=\"gray\">", datalines.content) == TRUE) {
      #       remove6 = sapply(strsplit(datalines.content,"<span class=\"gray\">",fixed = TRUE),"[[",2); remove6 = sapply(strsplit(remove6,"</span>",fixed = TRUE),"[[",1); datalines.content = gsub(remove6, "", datalines.content)}
      #     if (grepl("<span class=\"gray\">", datalines.content) == TRUE) {
      #       remove7 = sapply(strsplit(datalines.content,"<span class=\"gray\">",fixed = TRUE),"[[",2); remove7 = sapply(strsplit(remove7,"</span>",fixed = TRUE),"[[",1); datalines.content = gsub(remove7, "", datalines.content)}
      #     if (grepl("<span class=\"gray\">", datalines.content) == TRUE) {
      #       remove8 = sapply(strsplit(datalines.content,"<span class=\"gray\">",fixed = TRUE),"[[",2); remove8 = sapply(strsplit(remove8,"</span>",fixed = TRUE),"[[",1); datalines.content = gsub(remove8, "", datalines.content)}
      #     if (grepl("<span class=\"gray\">", datalines.content) == TRUE) {
      #       remove9 = sapply(strsplit(datalines.content,"<span class=\"gray\">",fixed = TRUE),"[[",2); remove9 = sapply(strsplit(remove9,"</span>",fixed = TRUE),"[[",1); datalines.content = gsub(remove9, "", datalines.content)}
      #     if (grepl("<span class=\"gray\">", datalines.content) == TRUE) {
      #       remove10 = sapply(strsplit(datalines.content,"<span class=\"gray\">",fixed = TRUE),"[[",2); remove10 = sapply(strsplit(remove10,"</span>",fixed = TRUE),"[[",1); datalines.content = gsub(remove10, "", datalines.content)}
      
      if (grepl("<div class=\"picture\">", datalines.content) == TRUE) {
        remove11 = sapply(strsplit(datalines.content,"<div class=\"picture\">",fixed = TRUE),"[[",2); remove11 = sapply(strsplit(remove11,"</a>",fixed = TRUE),"[[",1);  datalines.content = gsub(remove11, "", datalines.content)}
      if (grepl("<div class=\"picture\">", datalines.content) == TRUE) {
        remove12 = sapply(strsplit(datalines.content,"<div class=\"picture\">",fixed = TRUE),"[[",2); remove12 = sapply(strsplit(remove12,"</a>",fixed = TRUE),"[[",1);  datalines.content = gsub(remove12, "", datalines.content)}
      if (grepl("<div class=\"picture\">", datalines.content) == TRUE) {
        remove13 = sapply(strsplit(datalines.content,"<div class=\"picture\">",fixed = TRUE),"[[",2); remove13 = sapply(strsplit(remove13,"</a>",fixed = TRUE),"[[",1);  datalines.content = gsub(remove13, "", datalines.content)}
      if (grepl("<div class=\"picture\">", datalines.content) == TRUE) {
        remove14 = sapply(strsplit(datalines.content,"<div class=\"picture\">",fixed = TRUE),"[[",2); remove14 = sapply(strsplit(remove14,"</a>",fixed = TRUE),"[[",1);  datalines.content = gsub(remove14, "", datalines.content)}
      if (grepl("<div class=\"picture\">", datalines.content) == TRUE) {
        remove15 = sapply(strsplit(datalines.content,"<div class=\"picture\">",fixed = TRUE),"[[",2); remove15 = sapply(strsplit(remove15,"</a>",fixed = TRUE),"[[",1);  datalines.content = gsub(remove15, "", datalines.content)}
      if (grepl("<div class=\"picture\">", datalines.content) == TRUE) {
        remove16 = sapply(strsplit(datalines.content,"<div class=\"picture\">",fixed = TRUE),"[[",2); remove16 = sapply(strsplit(remove16,"</a>",fixed = TRUE),"[[",1);  datalines.content = gsub(remove16, "", datalines.content)}
      if (grepl("<div class=\"picture\">", datalines.content) == TRUE) {
        remove17 = sapply(strsplit(datalines.content,"<div class=\"picture\">",fixed = TRUE),"[[",2); remove17 = sapply(strsplit(remove17,"</a>",fixed = TRUE),"[[",1);  datalines.content = gsub(remove17, "", datalines.content)}
      if (grepl("<div class=\"picture\">", datalines.content) == TRUE) {
        remove18 = sapply(strsplit(datalines.content,"<div class=\"picture\">",fixed = TRUE),"[[",2); remove18 = sapply(strsplit(remove18,"</a>",fixed = TRUE),"[[",1);  datalines.content = gsub(remove18, "", datalines.content)}
      if (grepl("<div class=\"picture\">", datalines.content) == TRUE) {
        remove19 = sapply(strsplit(datalines.content,"<div class=\"picture\">",fixed = TRUE),"[[",2); remove19 = sapply(strsplit(remove19,"</a>",fixed = TRUE),"[[",1);  datalines.content = gsub(remove19, "", datalines.content)}
      if (grepl("<div class=\"picture\">", datalines.content) == TRUE) {
        remove20 = sapply(strsplit(datalines.content,"<div class=\"picture\">",fixed = TRUE),"[[",2); remove20 = sapply(strsplit(remove20,"</a>",fixed = TRUE),"[[",1);  datalines.content = gsub(remove20, "", datalines.content)}
      ####
      
      datalines.content = gsub("<br>", "<p>", datalines.content)
      datalines.content = gsub("<br/>", "<p>", datalines.content)
      datalines.content = gsub("< br/>", "<p>", datalines.content)
      datalines.content = gsub("<br />", "<p>", datalines.content)
      datalines.content = gsub("< br />", "<p>", datalines.content)
      datalines.content = gsub("<BR>", "<p>", datalines.content)
      datalines.content = gsub("< BR>", "<p>", datalines.content)
      datalines.content = gsub("<BR >", "<p>", datalines.content)
      datalines.content = gsub("< BR >", "<p>", datalines.content)
      datalines.content = gsub("<tr>", "<p>", datalines.content)
      datalines.content = gsub("</tr>", "<p>", datalines.content)
      datalines.content = gsub("<TR>", "<p>", datalines.content)
      datalines.content = gsub("</TR>", "<p>", datalines.content)
      
      datalines.content = unlist(strsplit(datalines.content, "<p>"))
      datalines.content = gsub("\t", "", datalines.content)
      datalines.content = trim(datalines.content)
      
      datalines.content = gsub("<.*?>", "", datalines.content)
      datalines.content = trim(datalines.content)
      datalines.content <- datalines.content[sapply(datalines.content, nchar) > 0]
      datalines.content = replace.HTML.char(datalines.content)
      content = gsub(' {2,}',' ',gsub('^ *| *$','',datalines.content))
    }
    
    # cat
    if (nchar(content) > 0) {
      file = sprintf("%s_%s_%s_%s_%s.txt", sources[2], date.year, date.month, date.day, gsub("[[:punct:]]", "", title_file))
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
proc.time() - ptm1

ptm2 = proc.time()
all.pages = NULL
### POPRAVLJAJ V FOR ZANKI PAGE PO PAGE 34:34, 33:33, ... !!! 34:1
for (i in 35:1){ # set pages in archive, set dates (to get articles from 1.9.2007 to 31.12.2013)
  #for (i in 184:1) # per partes!!!
  #to get all news from 24UR archive: for (i in 358:1)
  url = sprintf("http://www.24ur.com/arhiv/novice/gospodarstvo/%s.html?&page_archive=%s", i, i)
  
  thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
  
  if (grepl("<!-- archive start -->", thepage) == TRUE) {
    archive = sapply(strsplit(thepage,"<!-- archive start -->",fixed = TRUE),"[[",2)
    
    archive = sapply(strsplit(archive,"<div id=\"newsarchive_page\">",fixed = TRUE),"[[",2)
    
    archive = sapply(strsplit(archive,"<!-- archive end -->",fixed = TRUE),"[[",1)
    
    pages = strsplit(archive, "<a href=\"")[[1]]; pages
    pages = sapply(strsplit(pages,"\">",fixed = TRUE),"[[",1)
    pages = sprintf("%s%s", "http://www.24ur.com", pages)[-1]
    all.pages = append(all.pages, pages, after = length(all.pages))
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/.html", "", all.pages)  #(!!!website does not exist errror: 29.04.2008  Janković se bo pritožil)
    all.pages = gsub("http://www.24ur.com/cestni_pripetljaji/drazja-dizel-in-kurilno-olje.html", "", all.pages)  #(!!!website does not exist errror: 24.08.2009  Dražja dizel in kurilno olje)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/pet-slovenskih-podjetij-v-minsku-gradi-kompleks-kempinski.html", "", all.pages)  #(!!!website does not exist errror: 5.01.2014  Pet slovenskih podjetij v Minsku gradi kompleks Kempinski)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pogovarjali-so-se-o-lacnih-sami-pa-imeli-hrane-in-pijace-v-izobilju.html", "", all.pages)  #(!!!website does not exist errror: 21.01.2014  Pogovarjali so se o la?nih, sami pa imeli hrane in pija?e v izobilju)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zidan-o-placah-v-slabi-banki-ko-sem-slisal-stevilke-mi-je-bilo-grozno.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/prodaja-mercatorja-se-vedno-negotova.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/na-danskem-vec-kot-4000-pri-nas-1.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/prvi-vecji-uspeh-slabe-banke.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/mussomeli-slovensko-gospodarstvo-spominja-na-trznico-v-istanbulu.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/lastnik-podjetja-zasluzil-160-000-evrov-delavcem-pa-bi-znizal-placo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kaj-storiti-s-praznimi-tovarnami.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/nova-socialna-bomba-za-majhen-kraj.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/cas-za-prodajo-mercatorja-se-izteka.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zlata-doba-zlata.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/slaba-banka-objavila-pogodbi-izvrsnih-direktorjev.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/agrokor-v-pogajanjih-z-bankami-upnicami.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/krka-ustvarja-visoke-dobicke.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kako-vnovciti-slovensko-pamet.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/durs-aha-muro-posilja-v-stecaj.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/dolgotrajni-stecajni-postopki-opeharjeni-delavci-ali-premalo-placani-stecajni-upravitelji.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pipistrel-letosnja-proizvodnja-ze-razprodana.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/je-bilo-res-vec-kot-1800-delavcev-zavedenih-in-opeharjenih.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/v-alpininih-cevljih-osvojili-ze-12-medalj.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/spiritov-razpis-skoraj-1000-podjetij-ostalo-praznih-rok.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/tezave-z-bajno-placanimi-sefi-slabe-banke.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/gzs-to-da-se-zadolzujemo-ni-razlog-za-evforijo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/koliko-bomo-placali-za-resevanje-manjsih-bank.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/revizija-dokazuje-milijonsko-oskodovanje-cimosa.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kaksna-je-resitev-za-cimos.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/se-en-stecaj-mure-direktorica-popolnoma-sokirana.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/poceni-poleti-na-enem-mestu.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kaj-se-dogaja-s-privatizacijo-heliosa.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/praprotnik-na-nlb-stevilna-vprasanja-malo-odgovorov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/sportni-center-pohorje-prosi-obcino-za-pomoc.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/novih-160-milijonov-za-slovenske-banke.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/mura-po-petih-letih-spet-pred-stecajem.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/litostrojski-delavci-zelijo-vedeti-kaj-jih-caka.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/slaba-banka-lahko-razkrije-podatke-o-slabih-posojilih.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/banki-celje-bomo-dali-160-milijonov-evrov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/skrivnostni-finec-opeharil-slovenski-elan.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/vrednotnice-manj-ali-vec-dela-na-crno.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/medja-v-eni-od-poslovalnic-zaposlenim-predstavil-nova-pravila.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nekoc-mura-zgodba-o-uspehu-danes-mura-druzba-pred-propadom.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/dobre-novice-in-nove-tezave-za-nlb.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/kadrovanja-v-zadnjih-izdihljajih.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kava-v-loncku-je-vroca-ne-susite-zivali-v-mikrovalovni-pecici.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/kriza-nacela-navticno-industrijo-manj-razstavljalcev-in-celo-micnih-hostes.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/golob-obracunal-s-pahorjem-radicevo-in-cuferjem.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/druzina-gostinca-stormana-razkriva-svojo-plat-zgodbe.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/tako-varcuje-nlb-na-eni-strani-drasticno-zatiskanje-pasu-na-drugi-trosenje.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bomo-potegnili-kratko-v-igri-velikih.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/vlaki-se-nekaj-casa-ne-bodo-prevazali-potnikov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/milijardo-evrov-vreden-projekt-namesto-sloveniji-v-roke-avstriji.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pogovor-z-bostjanom-jazbecem.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/ali-lahko-prodaja-letalisca-prinese-cenejse-letalske-vozovnice.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/visok-vlozek-visok-dobitek.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/slovenski-paradiznik-zaposlil-tudi-murine-delavke.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/silicijevi-vrtovi-ker-z-drzavo-ni-nic-si-podjetniki-pomagajo-sami.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kaj-pocnejo-s-hidrio.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/cimos-prosi-za-nove-milijone.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/poleg-steklenic-za-viski-in-konjak-si-zelijo-izdelovati-tudi-steklenicke-za-parfume.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/ko-v-ponudbi-poleg-prenocisca-dobis-celo-zidanico-vkljucno-s-cvickom.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/so-tudi-podjetja-ki-vlagajo-v-zdravje-in-zadovoljstvo-delavcev.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/termoelektrarna-trbovlje-oskodovana-za-milijon-evrov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/novih-gradbenih-del-ni-na-vidiku-grabinci-v-skrbeh.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/medja-ostaja-v-nlb-ker-drzava-ne-najde-boljsega-kandidata.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/sindikati-zdruzili-glas-proti-raz-prodaji.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/delamaris-zapusca-izolo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/vecina-verjetno-nikoli-ne-bo-odgovarjala-za-svoja-dejanja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kdo-bo-napojil-zejnega-pivovarja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/abanka.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/stecajni-upravitelj-primorja-predlaga-stecaj-grepa.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/ima-cevljarska-industrija-s-tradicijo-sploh-prihodnost.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/sesti-blok-tes-na-testu.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zgodba-o-uspehu-papirnici-radece-se-je-z-dna-uspelo-povzpeti-na-vrh.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/obupani-delavci-se-sprasujejo-od-cesa-naj-zivimo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/po-znanem-scenariju-se-potaplja-se-en-gradbinec.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nekdanji-direktor-iz-stecajne-mase-zahteva-800-tisocakov-delavci-niso-dobili-nic.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/slovenska-zgodba-o-uspehu.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/aerodrom-tik-pred-prodajo-za-telekom-se-cas-izteka.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zakaj-delodajalci-zaposlujejo-na-crno-zakaj-delavci-to-sprejemajo-in-zakaj-obrtniki-neradi-izdajo-racun.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nafta-petrochem-kako-je-spet-odpovedala-drzava.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/cpl-razprodaja-stroje-to-pa-je-sele-zacetek.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nekaj-bi-prodali-nekaj-obdrzali.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/gospodarstveniki-si-zelijo-odlocnejsih-ukrepov-ne-le-okvirnih-obljub.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/lahko-izberete-sveze-ribe-lahko-pa-poceni.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/mercedes-se-je-odlocil-za-cehe-na-nitki-visi-530-delovnih-mest.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/francozi-izboljsali-ponudbo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/iz-stormana-v-celeio-so-pred-propadlim-hotelom-boljsi-casi.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nesreca-nikoli-ne-pride-sama-poleg-dezja-udaril-se-embargo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bo-komisija-cakala-do-morebitne-prodaje-elana.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/letalo-prihodnosti-pipistrelovo-letalo-na-elektricni-pogon.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zapleti-pri-privatizaciji.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/todoric-obvladuje-ze-dobrih-80-odstotkov-mercatorja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/stavka-v-petrochemu.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/aerodrom-ljubljana-malcek-v-druzbi-velikanov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/tes-bi-lahko-placal-manj-a-je-zamudil-rok.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/prevent-halog-izgubil-posel-brez-dela-bo-ostalo-od-400-do-450-ljudi.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/tretji-najbogatejsi-hrvat-bi-kupil-vina-koper.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bodo-rimske-terme-ozivele-kot-karlovy-vary.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/usoda-podjetja-vprasljiva-a-zgodilo-se-ni-nic.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/izolski-delmar-v-stecaju.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/cufer-se-vraca-na-nlb-kako-visoko.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/izboljsajmo-konkurencnost-in-znizajmo-place-nimamo-nobene-druge-strategije.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/hrvaski-stecajni-upravitelj-tako-priljubljen-da-ga-hocejo-za-predsednika-drzave.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nepravicno-varcevanje-znova-na-udaru-javni-sektor.html", "", all.pages) 
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/trboveljski-dimnik-najvisji-v-evropi-bi-lahko-postal-turisticna-znamenitost.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/za-zdaj-vse-po-starem-a-vprasanje-je-koliko-casa-se.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bo-zaradi-ebole-cokolada-drazja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/gospodarstveniki-vladi-sporocajo-bodite-bolj-ambiciozni.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/s-skrivnostnim-rusom-so-se-pogajali-dve-leti-in-pol-pa-ga-niti-ne-poznajo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nove-tezave-zvoneta-stormana-najemniki-ostali-brez-elektrike.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/avstrijci-razumejo-da-je-treba-v-zeleznice-vlagati-kaj-pa-pri-nas.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/drugi-tir.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/v-stecaj-tudi-aha-hoja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/kaj-nas-lahko-naucijo-italijani.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/440-000-evrov-za-svetovalce-in-odvetnike.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/portoroslki-hoteli-v-roke-hrvatov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kocevskemu-koncu-grozi-nova-socialna-bomba.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pozotivne-novice-iz-elana.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/ne-zmenijo-se-za-krizo-in-korakajo-po-poti-do-uspeha.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/veletrgovec-v-rdecih-stevilkah.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/na-pohorju-tik-pred-sezono-vendarle-stekla-pripravljalna-dela.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/resitelj-pohorja-letosnjo-zimo-bo-mariborsko-pohorje-zelo-belo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/slovenci-na-kickstarterju-prave-zvezde-zagar-ze-zbral-50-tisocakov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/placa-bi-se-jim-znizala-a-se-vedno-bi-dobili-11-000-evrov-mesecno.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/slaba-banka-pri-prodaji-skupine-pivovarna-lasko-ni-ne-hitra-ne-strokovna-ne-profesionalna.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/akrapoviceva-selitev-proizvodnje-v-belo-krajino-prinasa-upanje-za-nova-delovna-mesta.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/v-novi-gorici-bo-zazivel-milijonski-racunalniski-center.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nolik-zaprl-vrata.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/telekom-se-vztrajno-pripravlja-na-prodajo-tudi-z-odpuscanjem.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/odpusceni-delavci-ustanovili-podjetje-in-uspeli.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/odvetnik-mastno-sluzi-banka-pa-bo-nazadnje-lahko-imela-vec-skode-kot-koristi.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/trboveljski-delavci-v-soku-ljubljana-je-izgubila-kompas.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/hrvaska-kupuje-slovenijo-po-drogi-kolinski-in-mercatorju-zdaj-se-portoroski-hoteli.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/prodaja-elana-visi-na-nitki.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/akrapovic-z-ekstravagantnim-motorjem-navdusil-svetovne-medije.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/napisali-dve-uredbi-a-obe-v-nasprotju-z-zakonom.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/o-energetskih-izkaznicah-ostaja-veliko-nejasnosti.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/po-novem-delo-po-sistemu-vrednotnic.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/peko-se-bo-razdelil.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nekdanji-direktor-kljuc-do-uspesne-resitve-polzele-so-novi-trgi-ne-denar.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kdo-se-skriva-za-racunom-ki-kupuje-petrol.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/je-drugi-najvecji-lastnik-petrola-pa-sploh-ni-znan.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bodo-cene-goriva-pri-nas-se-naprej-padale.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nasa-vlada-o-krizi-sele-teden-po-njenem-izbruhu.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/usoda-seawaya-odvisna-od-dogovora-z-bankami.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zaradi-energetskih-izkaznic-trpi-nepremicninski-trg.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/podpisi-za-privatizacijo-politiki-razjahajte-gospodarstvo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/svetovno-prvenstvo-v-rokometu-v-znamenju-elana.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/bi-delavec-malo-pred-upokojitvijo-sam-dal-odpoved.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/izvrsba-lahko-petanov-dzs-pahne-v-stecaj.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/sodisce-zahteva-konkretne-dokaze-o-tem-kako-bi-naj-uros-rotnik-zlorabljal-polozaj.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/se-nam-obeta-bencinski-davek.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/v-kocevju-bodo-spet-solali-kovinarje-tudi-stipendije-bodo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pivo-bo-odslej-teklo-kar-po-pivovodu.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/na-slabi-banki-se-spet-branijo-nismo-delovali-nezakonito-neucinkovito-in-drago.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bodo-poligon-kjer-je-zraslo-vec-uspesnih-inovativnih-idej-zaprli.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/hidria-sklenila-vecmilijonski-posel.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/todoric-od-slovenskih-dobaviteljev-zahteva-dodatne-rabate-popuste-in-akcije.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/it/eksplodirajoce-macke-navdusujejo-v-dveh-tednih-do-vec-kot-stirih-milijonov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pivovarna-lasko-korak-blize-prodaji.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bi-imeli-drzavljani-vec-koristi-od-dobickov-drzavnih-firm-ali-od-njihove-prodaje.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/novi-posli-novo-upanje-v-prihodnjih-petih-letih-racunajo-na-podvojitev-delovnih-mest.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/oglasi-svoj-ucinek-so-dosegli-ko-ne-gre-mimo-nas-neopazen.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/gospodarski-oskarji-za-leto-2014-na-izbor-kriminalisticne-preiskave-niso-vplivale.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pred-stecajem-drazba-v-seawayu.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/tes-6-bo-v-prvih-4-letih-pridelal-210-milijonov-evrov-izgube.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/stenmark.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/intervju-z-glavnim-ekonomistom-saxo-banke-slovenija-se-zaveda-svojih-tezav-zdaj-jih-mora-le-se-resiti.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/iz-premogovnika-velenje-odteka-denar-za-nepotrebne-provizije.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/podjetje-zgradili-na-pogoriscu-starega-in-tudi-prihodnost-je-videti-svetla.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/je-hse-pred-sesutjem.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/placas-eno-dobis-tri-podjetja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/politika-nerada-govori-o-kdd.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pescica-posameznikov-prevzela-kdd.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/se-vrh-slabe-banke-s-torbjornom-m-nssonom-na-celu-posljavlja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/dan-odprtih-vrat-v-slovenskih-startupih.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kdo-bo-novi-lastnik-pivovarne-lasko.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/drzava-prodala-za-milijardo-evrov-20-letnih-obveznic.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/elektricna-vozila-v-sloveniji-je-to-prihodnost.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/mladi-imajo-bancne-racune-a-financna-pismenost-je-podpovprecna.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/lastniska-hobotnica-okoli-koloseja-se-je-zacela-sesuvati-sama-vase.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pospesevalnik-ki-naj-bi-podjetnike-zadrzal-v-sloveniji.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bi-drzava-morala-delavcem-omogociti-da-odkupijo-podjetja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/cerar-naj-bi-zdaj-podpiral-prodajo-telekoma.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/cena-za-telekom-in-lasko.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/sdh-se-vedno-brez-primernih-nadzornikov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/kaj-pretehta-ko-se-odlocate-za-nakup-izdelka-z-isto-ceno.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kocevska-obcina-se-je-lotila-posla-z-lesom.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/slaba-banka-bo-sistemski-tehniki-iskala-novega-lastnika.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/naucite-se-kako-zasluziti-denar-na-svetovni-borzi.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/prehitro-sklenili-posel.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/kaj.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/izklicna-cena-za-rog-le-tisoc-evrov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/legendarna-blagovna-znamka-rog-naprodaj.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bi-morala-vsa-podjetja-dobicek-deliti-med-zaposlene.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/v-nekaj-letih-iz-milijonske-izgube-v-milijonski-dobicek.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/koncno-blize-izbiri-novih-nadzornikov-sdh.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/slovenski-bankirji-pod-veliko-vecjim-nadzorom-za-nepravilnosti-pa-kazni-do-5-milijonov-evrov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/skoraj-5-milijonov-evrov-za-nicvredne-delnice.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/iz-idrije-v-mehiko-nov-uspeh-slovenskega-podjetja-kolektor.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/sklenili-30-milijonski-posel.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/z-domnevno-spornimi-posli-naj-bi-nkbm-oskodovali-za-30-milijonov-evrov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/gradnja-vetrnic-nam-ne-gre-od-rok.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kaj-ne-prodati.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/prodaja-ali-razprodaja-kaj-s-podjetji-v-drzavni-lasti.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/klasje-zaprlo-vrata-delavci-dobili-knjizice.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kisik-prodaja-v-plocevinkah.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/drzne-ideje-postale-podjetja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kaj-se-bo-zgodilo-z-nasimi-biseri-v-tujih-rokah.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/prodaja-telekoma-da-ali-ne.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/najbolj-inovativna-jelovica.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/gospodarstveniki-poudarjajo-kljuc-do-uspeha-je-sodelovanje.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kaj-privablja-turiste-oh-o-kremsniti-mi-pa-sploh-ne-govorite.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/proizvodnja-v-seawayu-ponovno-stekla-a-ne-brez-birokratskih-zapletov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zvon-ena-in-dva-kredit-nlb-prenakazali-na-zasebno-podjetje-enega-od-vodilnih.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/arasidovo-maslo-na-slovenski-nacin.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/gozd-nase-najvecje-bogastvo-bogastvo-ki-nam-polzi-skozi-prste.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/izboljsanje-poslovnih-odnosov-s-turki.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/po-prodaji-v-skrbi-zaradi-odpuscanj.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/svet/tezave-turistov-na-grskih-otokih-omejitev-pri-tocenju-goriva-zavracanje-kartic-in-nedelujoci-bankomati.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pipistrelovo-letalo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/v-seawayu-se-vedno-ne-morejo-zaceti-delati.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kam-s-pre-vec-denarja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kako-je-agrokor-znizal-mercatorjev-dolg.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/tudi-vi-sumite-da-je-banka-ponaredila-podpis-kontaktirajte-policijo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/delavke-mure-sedle-za-sivalne-stroje.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/avstrijsko-ustavno-sodisce-nad-nacin-resevanja-bank.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/podjetniki-ce-ne-razvijas-novih-proizvodov-te-konkurenca-prehiti-in-kar-naenkrat-izgines.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/najboljsi-sosed-bo-odslej-najemnik.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/dobra-gospodarska-novica.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/banke-peku-nocejo-dati-denarja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kaksna-je-usoda-alpine.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bo-peko-prezivel.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/v-cimosu-se-koncno-odpirajo-vrata-za-uspeh.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/hrvaski-lastniki-slovenskih-podjetij.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bi-200-metrov-pristajalne-steze-okrepilo-obalni-turizem.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/posel-ki-cveti-in-disi-brezposelni-biolog-z-jezerskega-hobi-spremenil-v-posel.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/miro-senica-se-pred-obtozbami-o-pranju-denarja-in-napeljevanju-k-zlorabi-polozaja-brani-tudi-s-pomocjo-dekana-pravne-fakultete.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/skoraj-vsak-drugi-slovenec-dopustuje-na-hrvaskem.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/it/sloveniji-se-odpirajo-vrata-v-vesolje.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/vodilnim-v-slabi-banki-se-obetajo-se-visje-place.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zanimajo-se-za-nakup-adrie-tehnike-a-jim-prodajalec-niti-odgovarja-ne.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/apetiti-so-veliki-a-zadolzevanje-bo-omejevalo-fiskalno-pravilo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/slaba-banka-se-sodeluje-s-quartz-co.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/iskraemeca-lastnik-iz-egipta-znova-postavil-v-vrh.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nasa-podjetja-med-500-najvecjimi.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nekoc-prva-v-jugoslaviji-danes-borba-za-obstoj.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/sindikati-zahtevajo-konec-varcevanja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kaj-je-novega-na-sejmu.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nic-ni-bilo-narobe.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zanimajo-se-za-nakup-adrie-tehnike-a-jim-prodajalec-niti-odgovarja-ne.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zaposleni-se-zanimajo-za-nakup-podjetja-v-katerem-delajo.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/don-don-postaja-najvecji-pek-v-regiji.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pozabljeni-poklici-dobra-poslovna-ideja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/dutb-3.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/factor-banka-prodaja-umetnine-nekatere-vredne-tudi-pol-milijona-evrov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/krka-naredila-prvi-korak-v-sirjenju-proizvodnje.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/cene-goriva-po-evropsko-tudi-pri-nas-naj-bi-dobili-prost-trg.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/bancniki-hypa-pridobili-28-milijonov-evrov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/pekarna-grosuplje-kjer-dnevno-pripravijo-vec-kot-50-ton-izdelkov-ima-novega-lastnika.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/dutb-za-svoje-odlocitve-so-bili-drago-placani-pa-so-sploh-veljavne.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/odprtje-trga-pogonskih-goriv.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/davcna-reforma-razburja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kitajci-pripravljajo-teren-v-mariboru.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/igor-akrapovic-slavil-100-zmago.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/evro-na-mesec-obresti-za-vodenje-racuna-pa-dva-evra-mesecno.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/kljub-ocitkom-brez-pojasnil-in-ukrepov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/opozicija-zahteva-pojasnila.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kako-dobro-znamo-trziti-nas-kraski-biser.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/nova-tovarna-zdravil.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/sporno-kadrovanje-ki-ga-cerar-ne-zeli-videti.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/vlada-bi-rada-veliko-vec-denarja-za-uporabo-termalne-vode-bo-to-katastrofa-za-zdraviliski-turizem.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/srbi-osvajajo-slovenski-trg-v-njihovi-lasti-dva-luksuzna-hotela.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/japonski-roboti-iz-slovenije.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/v-sloveniji-manjsi-davki-in-manj-birokracije.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/naprodaj-znamka-hk-jesenice.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/stara-sta-komaj-23-let-in-ze-upravljata-9-milijonov-evrov-vredno-podjetje.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/davcna-petletka.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kordez-dobil-sluzbo-pri-poslovnezu-ki-je-v-stecaj-spravil-vecino-svojih-druzb.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/it/slovenska-aplikacija-ki-spodbuja-zdravo-zivljenje-na-voljo-na-kitajskem-trgu.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/stara-sta-komaj-23-let-in-ze-upravljata-9-milijonov-evrov-vredno-podjetje.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/po-sanjsko-sluzbo-v-lek.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zvestoba-blagovnim-znamkam-ostaja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/zakaj-so-svinjske-klobase-precej-drazje-od-salam.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/tus-brez-mirka-tusa.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/za-nenavadne-ideje-so-si-upali-tvegati-vse.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/tovsakova-se-je-za-pricanje-odlocila-sama.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/vino-prisegate-na-kakovost-ali-na-nizko-ceno.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/cene-nafte-dol-a-pri-nas-se-to-ne-bo-poznalo-prihaja-bencinski-cent.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/24ur-zvecer-s-kartonom-in-plastiko-na-svetovni-trg.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/v-alpini-bodo-odpuscali.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/prekladanje-terjatev-do-ach-za-bogate-se-dodatni-milijoni-iz-davkoplacevalskih-zepov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kuhar-ce-razpade-tus-bo-to-imelo-sirse-posledice.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/spoznajte-slovenca-iz-silicijeve-doline-pionirja-odkrivanja-gps-navigacije.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/je-na-vrsti-nlb.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/politika-ima-novo-igracko-gozdno-podjetje-bo-bdelo-nad-gozdovi-ki-letno-ustvarijo-okoli-70-milijonov-evrov.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/slovensko-podjetje-v-san-franciscu-uvrscajo-jih-med-15-startupov-ki-jih-je-vredno-spremljati-v-letu-2016.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/kaj-se-bo-zgodilo-ko-vlada-ne-bo-vec-dolocala-cen-nekaterih-vrst-bencina-in-kurilnega-olja.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/v-peku-zalost-delavci-ob-crni-zastavi-in-svecah-zakljucujejo-zadnja-narocila.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/slovenija/tiri-pokajo-v-luki-obup.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/slovensko-znanje-je-navdusilo-svetovne-strokovnjake.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/v-alpini-bodo-odpuscali.html", "", all.pages)
    all.pages = gsub("http://www.24ur.com/novice/gospodarstvo/crni-oblaki-nad-alpino-vodstvo-in-dragi-svetovalci-grozijo-z-odpuscanjem-in-selitvijo-proizvodnje.html", "", all.pages)
    
        
    all.pages = all.pages[sapply(all.pages, nchar) > 0]
  }
}
proc.time() - ptm2

ptm3 = proc.time()
for(i in 1:length(all.pages)){
  get.24UR(all.pages[i])
}
proc.time() - ptm3

#setwd("E:/")
#get.24UR("http://www.24ur.com/chelsea-iznicil-zaostanek-s-prve-tekme-real-se-je-igral-z-ognjem.html")
#get.24UR("http://www.24ur.com/ruske-investitorje-zanima-nakup-vec-slovenskih-podjetij.html")
#get.24UR("http://www.24ur.com/novice/gospodarstvo/dokapitalizacija-nlb-v-prvi-polovici-2010.html")