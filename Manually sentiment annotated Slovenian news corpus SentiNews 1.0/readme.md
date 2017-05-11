Title: Manually sentiment annotated Slovenian news corpus SentiNews 1.0

Author: Jože Bučar, Faculty of Information Studies Novo mesto (contact: joze.bucar@gmail.com)

Abstract:
Between 2 and 6 annotators independently sentiment annotated a stratified random sample of 10,427 documents from the Slovenian news portals 24ur, Dnevnik, Finance, Rtvslo, and Žurnal24. These portals contain political, business, economic and financial content. The texts were annotated using the five-level Lickert scale (1 – very negative, 2 – negative, 3 – neutral, 4 – positive, and 5 – very positive) on three levels of granularity, i.e. on the document, paragraph, and sentence level.

Annotation: 6 annotators, it took more than a year to manually annotate the sample; instructions to annotators: „Please specify the sentiment from the perspective of an average Slovenian web user. How did you feel after reading the news?“; five-level Lickert scale (1 – very negative, 2 – negative, 3 – neutral, 4 – positive, and 5 – very positive)

Levels of granularity: Document level, paragraph level, sentence level

Sentiment allocation: Negative (if average of scores ≤ 2.4); neutral (if average of scores is between 2.4 and 3.6); positive (average of annotated scores ≥ 3.6)

Short statistics: 10,427 documents; 89,999 paragraphs; 168,899 sentences; 326,1327 words; 214,705 unique words

Keywords:
news corpus, sentiment classification, opinion mining

Web resources:
- Slovenian news texts with political, business, economic and financial content published between 1 September 2007 and 31 December 2013 from five Slovenian web media from five web media: www.24ur.com, www.dnevnik.si, www.finance.si, www.rtvslo.si, www.zurnal24.si

Type and size:
- .txt: Document level (22.66 MB), Paragraph level (23.21 MB), Sentence level (25.96 MB)

Encoding: UTF-8

Year: 2017-05-09

Attributes:

nid - News ID [integer; from 1 to 10,427]

pid - Paragraph ID [integer; from 1 to 94]

sid - Sentence ID [integer; from 1 to 150]

main_url - Uniform Resource Locator (URL) of the resource (web medium) [string; www.24ur.com, www.dnevnik.si, www.finance.si, www.rtvslo.si, www.zurnal24.si]

url - URL of the news [string]

title - Title of the news [string]

keywords - Keywords of the news [string]

content - Content of the news (document) or its part (paragraph, sentence) [string]

date - Date of publishing the news [string; format: yyyy-mm-dd]

author - Author of the news [string]

Ann1 - Ann6 - Manual annotations from 6 annotators, they include scores on five-level Lickert scale (1 – very negative, 2 – negative, 3 – neutral, 4 – positive, and 5 – very positive) [integer; from 1 to 5]

avg_sentiment - Average of scores (Ann1 - Ann6) [float; from 1 to 5]

sd_sentiment - Standard deviation of scores (Ann1 - Ann6) [float; document level: from 0 to 1.225, paragraph level: from 0 to 2.828, sentence level: from 0 to 2.828]

sentiment - Sentiment allocation according to avg_sentiment score [string; negative, neutral, positive]

Acknowledgements:
This work was supported by the European Union, The European Regional Development Fund, Slovene Human Resources Development and Scholarship Fund, Ministry of Education, Science and Sport, Slovenia and Young Researcher Programme by Slovenian Research Agency. Our research was carried out within the framework of the Operational Programme for Strengthening Regional Development Potentials for the period 2007-2013, Development Priority 1: Competitiveness and research excellence, Priority Guideline 1.1: Improving the competitive skills and research excellence.

Citation:
If you are a scientist and use the wordlists or code you can cite this publication:

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