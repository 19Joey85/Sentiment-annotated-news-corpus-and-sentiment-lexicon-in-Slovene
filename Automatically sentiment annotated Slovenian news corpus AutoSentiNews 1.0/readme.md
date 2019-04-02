Title: Automatically sentiment annotated Slovenian news corpus AutoSentiNews 1.0

Author: Jože Bučar, Faculty of Information Studies Novo mesto (contact: joze.bucar@gmail.com)

Abstract:
The corpus contains 256,567 documents from the Slovenian news portals 24ur, Dnevnik, Finance, Rtvslo, and Žurnal24. These portals contain political, business, economic and financial content. The submission contains 7 files: 5 of them, which are named after the news portal, contain raw news in txt format retrieved with R crawlers for five Slovenian web media 1.0. The file AutoSentiNews contains of 5 text files that contain 256,567 news articles annotated as positive, negative or neutral at the document level. 10,427 of them were manually annotated (cf. Manually sentiment annotated Slovenian news corpus SentiNews 1.0) and the remaining 246,140 news were annotated automatically. The file SloStopWords contains of 1,784 stop words for Slovene.

Automatic annotation was carried out by using the Multinomial Naive Bayes classifier, which achieved the best F-score with 77.76% within the three-class (positive, negative and neutral) document-based sentiment classification. We used 5 times 10-fold cross-validation and the following pre-processing options: TF-IDF, transforming upper-case letters to lower-case letters, the removal of stop words, using a combination of unigrams and bigrams, and without lemmatization.

Keywords:
news corpus, sentiment classification, opinion mining

Web resources:
- Slovenian news texts with political, business, economic and financial content published between 1 September 2007 and 31 December 2013 from five Slovenian web media from five web media: www.24ur.com, www.dnevnik.si, www.finance.si, www.rtvslo.si, www.zurnal24.si

Type and size:
- .zip (raw files); size (.txt): 668 MB
- .zip (AutoSentiNews); size (.txt): 635 MB
- .txt (SloStopWords); size: 19 KB

Encoding: UTF-8

Year: 2017-05-09

Attributes:

nid - News ID [integer; from 1 to 256,567]

main_url - Uniform Resource Locator (URL) of the resource (web medium) [string; www.24ur.com, www.dnevnik.si, www.finance.si, www.rtvslo.si, www.zurnal24.si]

url - URL of the news [string]

title - Title of the news [string]

keywords - Keywords of the news [string]

content - Content of the news (document) [string]

date - Date of publishing the news [string; format: yyyy-mm-dd]

author - Author of the news [string]

sentiment - Estimated sentiment [string; negative, neutral, positive]

Acknowledgements:
This work was supported by the European Union, The European Regional Development Fund, Slovene Human Resources Development and Scholarship Fund, Ministry of Education, Science and Sport, Slovenia and Young Researcher Programme by Slovenian Research Agency. Our research was carried out within the framework of the Operational Programme for Strengthening Regional Development Potentials for the period 2007-2013, Development Priority 1: Competitiveness and research excellence, Priority Guideline 1.1: Improving the competitive skills and research excellence.

Citation:
If you are a scientist and use the wordlists or code you can cite this publication:

@article{buvcar2018annotated,
  title={Annotated news corpora and a lexicon for sentiment analysis in Slovene},
  author={Bu{\v{c}}ar, Jo{\v{z}}e and {\v{Z}}nidar{\v{s}}i{\v{c}}, Martin and Povh, Janez},
  journal={Language Resources and Evaluation},
  volume={52},
  number={3},
  pages={895--919},
  year={2018},
  publisher={Springer}
}

or

@inproceedings{buvcar2016sentiment,
  title={Sentiment Classification of the Slovenian News Texts},
  author={Bu{\v{c}}ar, Jo{\v{z}}e and Povh, Janez and {\v{Z}}nidar{\v{s}}i{\v{c}}, Martin},
  booktitle={Proceedings of the 9th International Conference on Computer Recognition Systems CORES 2015},
  pages={777--787},
  year={2016},
  organization={Springer}
}

Copyright:
This work is publicly available under Creative Commons copyright license Attribution-ShareAlike 4.0 International (CC BY-SA 4.0 or newer version). This license allows others to use, distribute, reproduce, and build upon this work as long as they credit this work by clearly mentioning its author and title. Moreover, they can license their new work under the identical terms, so any derivatives will also allow commercial use. For more details, check out the information available on the website https://creativecommons.org/licenses/by-sa/4.0/.
