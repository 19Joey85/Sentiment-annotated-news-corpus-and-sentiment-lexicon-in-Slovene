Title: Slovene sentiment lexicon JOB 1.0

Author: Jože Bučar, Faculty of Information Studies Novo mesto (contact: joze.bucar@gmail.com)

Abstract:
The JOB lexicon for sentiment analysis of Slovenian texts contains a list of 25,524 headwords from the List of Slovenian headwords 1.1 (http://hdl.handle.net/11356/1038) extended with sentiment ratings based on the AFINN model with an integer between -5 (very negative) and +5 (very positive). The ratings are derived from the lemmatized version of the Manually sentiment annotated Slovenian (sentence-based) news corpus SentiNews 1.0 (http://hdl.handle.net/11356/1110).

Model:
The original sentence-level annotations were based on the five-level Lickert scale (integer between one (very negative) and five (very positive)). Therefore, we used linear transformation to transform average scores of sentences from the Lickert model to the AFINN (score one within the Lickert model transformed to minus five within the AFINN model, score five within the Lickert model transformed to plus five within the AFINN model). For every headword listed in the Slovenian words data base we calculated the average sentiment by deducting the average sentiment of the corpus (avg_AFINN) and standard deviation (sd_AFINN) of AFINN values in the corpus. Finally, we obtained the AFINN score for every headword in the list by rounding the avg_AFINN score.

Keywords:
sentiment lexicon, opinion lexicon

Web resources:
- Manually sentiment annotated Slovenian news corpus SentiNews 1.0 (based on annotations on sentence level, lemmatized); Slovenian news texts with political, business, economic and financial content published between 1 September 2007 and 31 December 2013 from five Slovenian web media from five web media: www.24ur.com, www.dnevnik.si, www.finance.si, www.rtvslo.si, www.zurnal24.si
- ToTaLe text analyser for Slovene texts - Tokanizer, Tagger and Lemmatizer for Slovene texts, Department of Knowledge Technologies, Jožef Stefan Institute (http://nl.ijs.si/analyse/)
- List of Slovenian headwords 1.1, Fran Ramovš Institute of Slovenian Language ZRC SAZU (http://hdl.handle.net/11356/1038)

Type and size:
- .txt; size: 693 KB

Encoding: UTF-8

Year: 2017-05-09

Attributes:

Word - Headword (lemmatized) from the Slovenian words database [string]

AFINN - Rounded avg_AFINN score [integer; from -5 to +5]

freq - Headword frequency (total number of occurrences in the annotated web-crawled sentence-based news corpus) [integer; from 0 to 260,931]

avg_AFINN - Average of AFINN values in the Manually sentiment annotated Slovenian (sentence-based) news corpus SentiNews 1.0 deducted by the average sentiment of the corpus [float; from -4.610 to +5.390]

sd_AFINN - Standard deviation of AFINN values in the Manually sentiment annotated Slovenian (sentence-based) news corpus SentiNews 1.0 [float; from 0 to 7.071]

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