# Web crawler for Zurnal24
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
#dir.create(sources[4]) # create directory Finance

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

get.ZURNAL24 <- function(url){
  if(url.exists(url)==TRUE){
    setwd("E:/ZURNAL24")
    #url = "http://www.zurnal24.si/za-gneco-na-obrezju-krivi-turki-clanek-6975"
    #url = "http://www.zurnal24.si/prvi-pacient-z-ebolo-v-zda-clanek-233600"
    
    thepage = paste(readLines(url, encoding = "UTF-8"), collapse = "")
    
    # get main URL
    url.main = substr(url, 8, 22)
    
    # get URL
    url = url
    
    # get title
    if (grepl("<title>", thepage) == TRUE) {
      datalines.title = sapply(strsplit(thepage,"</title>",fixed = TRUE),"[[",1)
      datalines.title = sapply(strsplit(datalines.title,"<title>",fixed = TRUE),"[[",2)
      datalines.title = trim(gsub(" - zurnal24", "", datalines.title))
      title = trim(gsub("&lt;/b&gt;&lt;/p&gt;&lt;p&gt;", "", datalines.title))
      title = gsub("\t"," ", title)
      title = replace.HTML.char(title)
      title = gsub(' {2,}',' ',gsub('^ *| *$','',title)) # replace multiple spaces with one
      title_file = gsub("Ä‡","c", title); title_file = gsub("Ä†","C",title_file); title_file = gsub("ÄŤ","c",title_file); title_file = gsub("ÄŚ","C",title_file); title_file = gsub("Ä‘Â","dz",title_file); title_file = gsub("Ä","Dz",title_file); title_file = gsub("Ĺˇ","s",title_file); title_file = gsub("Ĺ ","S",title_file); title_file = gsub("Ĺľ","z",title_file); title_file = gsub("Ĺ˝","Z",title_file); title_file = gsub("[[:punct:]]","",title_file); title_file = gsub("â‚¬","eur",title_file)
      title_file = gsub(' {2,}',' ',gsub('^ *| *$','',title_file)) # replace multiple spaces with one
      if (nchar(title) >= 50) {
        title_file = substring(title_file,1,50)
      }
    } else {
      title = ""
      title_file = ""
    }
    
    # get keywords #no keywords 
    if (grepl("<meta name=\"keywords\" content=\"", thepage) == TRUE) {
      datalines.keywords = sapply(strsplit(thepage,"<meta name=\"keywords\" content=\"",fixed = TRUE),"[[",2)
      keywords = trim(sapply(strsplit(datalines.keywords,"\" />",fixed = TRUE),"[[",1))
      keywords = replace.HTML.char(keywords)
      keywords = gsub(' {2,}',' ',gsub('^ *| *$','',keywords))
    } else {
      keywords = ""
    }
    
    # get date
    if (grepl("<p class=\"main_info\">", thepage) == TRUE) {
      datalines.date = sapply(strsplit(thepage,"<p class=\"main_info\">",fixed = TRUE),"[[",2)
      datalines.date = sapply(strsplit(datalines.date,"<aside id=\"service_related\">",fixed = TRUE),"[[",1)
      datalines.date = sapply(strsplit(thepage,"<time datetime=\"",fixed = TRUE),"[[",2)
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
    if (grepl("<p class=\"main_info\">", thepage) == TRUE) {
      datalines.author = sapply(strsplit(thepage,"<p class=\"main_info\">",fixed = TRUE),"[[",2)
      datalines.author = sapply(strsplit(datalines.author,"<aside id=\"service_related\">",fixed = TRUE),"[[",1)
      datalines.author = trim(sapply(strsplit(datalines.author,"<strong class=\"author\">",fixed = TRUE),"[[",2))
      datalines.author = sapply(strsplit(datalines.author,"</strong>",fixed = TRUE),"[[",1)
      datalines.author = gsub("<.*?>", "", datalines.author)
      author = gsub("[()]", "", datalines.author)
      author = replace.HTML.char(author)
      author = gsub(' {2,}',' ',gsub('^ *| *$','',author))
    } else {
      author = ""
    }
    
    # get summary
    if (grepl("<meta name=\"description\" content=\"", thepage) == TRUE) {
      datalines.summary = sapply(strsplit(thepage,"<meta name=\"description\" content=\"",fixed = TRUE),"[[",2)
      datalines.summary = sapply(strsplit(datalines.summary,"\" />",fixed = TRUE),"[[",1)
      datalines.summary = replace.HTML.char(datalines.summary)
      summary = gsub(' {2,}',' ',gsub('^ *| *$','',datalines.summary))    
    } else {
      summary = ""
    }
    
    # get content
    if (grepl("<div class=\"entry\">", thepage) == TRUE) {
      datalines.content = trim(sapply(strsplit(thepage,"<div class=\"entry\">",fixed = TRUE),"[[",2))
      datalines.content = trim(sapply(strsplit(datalines.content,"<footer id=\"detail_footer\">",fixed = TRUE),"[[",1))
      
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
      
      #datalines.content = gsub("<em>", "<p>", datalines.content)
      #datalines.content = gsub("< em>", "<p>", datalines.content)
      #datalines.content = gsub("<em >", "<p>", datalines.content)
      #datalines.content = gsub("< em >", "<p>", datalines.content)
      datalines.content = gsub("<br /> ", "<p>", datalines.content)    
      datalines.content = gsub("<br/>", "<p>", datalines.content)
      datalines.content = gsub("<br/ >", "<p>", datalines.content)
      datalines.content = gsub("< br/>", "<p>", datalines.content)
      datalines.content = gsub("<br />", "<p>", datalines.content)
      datalines.content = gsub("< br />", "<p>", datalines.content)
      datalines.content = gsub("<br / >", "<p>", datalines.content)
      
      datalines.content = gsub("</div>", "", datalines.content)
      datalines.content = unlist(strsplit(datalines.content, "<p>"))
      datalines.content = unlist(strsplit(datalines.content, "</p>"))
      datalines.content = gsub("<.*?>", "", datalines.content)
      datalines.content = trim(datalines.content)
      datalines.content <- datalines.content[sapply(datalines.content, nchar) > 0]
      datalines.content = replace.HTML.char(datalines.content)
      content = gsub(' {2,}',' ',gsub('^ *| *$','',datalines.content))
    } else {
      content = ""
    }
    
    # cat
    file = sprintf("%s_%s_%s_%s_%s.txt", sources[4], date.year, date.month, date.day, gsub("[[:punct:]]", "", title_file))
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

j=28 #may 2013
for (k in 57:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.05.2013&form_name_to_publish_date=31.05.2013&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=27 #jan 2013
for (k in 65:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.01.2013&form_name_to_publish_date=31.01.2013&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=26 #jul 2011
for (k in 54:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.07.2011&form_name_to_publish_date=31.07.2011&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=25 #jun 2011
for (k in 68:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.06.2011&form_name_to_publish_date=30.06.2011&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=24 #jan 2014
for (k in 53:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.01.2014&form_name_to_publish_date=31.01.2014&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=23 #feb 2014
for (k in 52:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.02.2014&form_name_to_publish_date=28.02.2014&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
      archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
      archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
      archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
      pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
      pages = gsub("/", "http://www.zurnal24.si/", pages)
      cat("j=", j, "k=", k, "\n")
      
      for(l in 1:length(pages)){
        get.ZURNAL24(pages[l])
      }
      
    }
}

j=22 #mar 2014
for (k in 48:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.03.2014&form_name_to_publish_date=31.03.2014&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=21 #apr 2014
for (k in 49:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.04.2014&form_name_to_publish_date=30.04.2014&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=20 #may 2014
for (k in 9:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.05.2014&form_name_to_publish_date=31.05.2014&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

#no articles in jun 2014

j=19 #jul 2014
for (k in 33:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.07.2014&form_name_to_publish_date=31.07.2014&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=18 #aug 2014
for (k in 33:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.08.2014&form_name_to_publish_date=31.08.2014&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=17 #sep 2014
for (k in 37:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.09.2014&form_name_to_publish_date=30.09.2014&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=16 #oct 2014
for (k in 36:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.10.2014&form_name_to_publish_date=31.10.2014&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=15 #nov 2014
for (k in 33:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.11.2014&form_name_to_publish_date=30.11.2014&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=14 #dec 2014
for (k in 40:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.12.2014&form_name_to_publish_date=31.12.2014&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=13 #jan 2015
for (k in 32:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.01.2015&form_name_to_publish_date=31.01.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=12 #feb 2015
for (k in 34:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.02.2015&form_name_to_publish_date=28.02.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=11 #mar 2015
for (k in 32:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.03.2015&form_name_to_publish_date=31.03.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=10 #apr 2015
for (k in 30:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.04.2015&form_name_to_publish_date=30.04.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=9 #may 2015
for (k in 26:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.05.2015&form_name_to_publish_date=31.05.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=8 #jun 2015
for (k in 26:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.06.2015&form_name_to_publish_date=30.06.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=7 #jul 2015
for (k in 29:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.07.2015&form_name_to_publish_date=31.07.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=6 #aug 2015
for (k in 24:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.08.2015&form_name_to_publish_date=31.08.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=5 #sep 2015
for (k in 29:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.09.2015&form_name_to_publish_date=30.09.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=4 #oct 2015
for (k in 30:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.10.2015&form_name_to_publish_date=31.10.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=3 #nov 2015
for (k in 27:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.11.2015&form_name_to_publish_date=30.11.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=2 #dec 2015
for (k in 28:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.12.2015&form_name_to_publish_date=31.12.2015&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}

j=1 #jan 2016
for (k in 27:1){ # in url3 set dates; in k set pages in archive for that time period)
  url3 = sprintf("http://www.zurnal24.si/index.php?ctl=show_category_content&category_level=1&form_name_from_publish_date=01.01.2016&form_name_to_publish_date=31.01.2016&category_id[]=6&form_name_content_type=article&page=%s", k)
  thepage3 = paste(readLines(url3, encoding = "UTF-8"), collapse = "")
  if (grepl("<h1>Več vsebin</h1>", thepage3) == TRUE) {
    archive = trim(sapply(strsplit(thepage3,"<h1>Več vsebin</h1>",fixed = TRUE),"[[",2))
    archive = trim(sapply(strsplit(archive,"<div class=\"pagination\">",fixed = TRUE),"[[",1))
    archive = unlist(strsplit(archive,"<a href=\"",fixed = TRUE))[-1] # split string and unlist list to get links; "<a class=\"title \" href=\" is for open articles and <a class=\"title sub\" href=" is for articles where you pay for access
    pages = trim(sapply(strsplit(archive,"\" title=\"",fixed = TRUE),"[[",1))
    pages = gsub("/", "http://www.zurnal24.si/", pages)
    cat("j=", j, "k=", k, "\n")
    
    for(l in 1:length(pages)){
      get.ZURNAL24(pages[l])
    }
    
  }
}