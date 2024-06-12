
# Wordmap: Semi-supervised Multinomial Document Classifier

**wordmap** is a naive Bayesian model for multinomial document
classification originally created for
[Newsmap](https://github.com/koheiw/newsmap). **wordmap** is separated
from **newsmap** to expand the score of its application beyond
geographical classification of news.

The semi-supervised classifier has been used to for various purposes
such as classificaiton of [speeches in terms of
topics](https://journals.sagepub.com/doi/full/10.1177/0894439320907027).
The simplicity of naive Bayesian algorithm makes it useful for
extracting features from vary larger corpora to create dictionaries The
detail of the algorithm is explained in [Watanabe
(2018)](https://www.tandfonline.com/eprint/dDeyUTBrhxBSSkHPn5uB/full).

## How to install

**wordmap** is available on CRAN since the v1.0. You can install the
package using the R command.

``` r
install.packages("wordmap")
```

If you want to the latest version, please install by running this
command in R. You need to have **devtools** installed beforehand.

``` r
install.packages("devtools")
devtools::install_github("koheiw/wordmap")
```

## Example

`data_corpus_ungd2017` contains transcripts of speeches delivered at the
United Nations General Assembly in 2017. The dictionary is adopted from
[Watanabe & Zhou
(2020)](https://journals.sagepub.com/doi/full/10.1177/0894439320907027).

``` r
require(quanteda)
## Loading required package: quanteda
## Warning: package 'quanteda' was built under R version 4.3.3
## Warning in .recacheSubclasses(def@className, def, env): undefined subclass
## "ndiMatrix" of class "replValueSp"; definition not updated
## Package version: 4.0.2
## Unicode version: 15.1
## ICU version: 74.1
## Parallel computing: 16 of 16 threads used.
## See https://quanteda.io for tutorials and examples.
```

``` r
require(wordmap)
## Loading required package: wordmap
## Warning in .recacheSubclasses(def@className, def, env): undefined subclass
## "ndiMatrix" of class "replValueSp"; definition not updated
```

``` r

dict <- dictionary(file = "dict/dictionary.yml")
print(dict)
## Dictionary object with 6 key entries.
## - [greeting]:
##   - greet*, thank*, congratulat*, sir, express*
## - [un]:
##   - united nations, international court*, security council, general assembly, organization*, reform*, secretary-general, resolution*, permanent member*, charter*, session*, conference*
## - [security]:
##   - secur*, kill*, attack*, dispute*, victim*, peac*, terror*, weapon*, nuclear*, conflict*, war*, disarmament*, threat*, cris*, solution*, settlement*, force*, destruction*, militar*, violence* [ ... and 2 more ]
## - [human]:
##   - human rights, violat*, race*, dignit*, protect*, citizen*, educat*
## - [democracy]:
##   - democra*, autocra*, dictator*, vote*, represent*, elect*, leader*, president*, government*, leadership*
## - [development]:
##   - develop*, market*, investment*, econom*, climate change, assistance*, sustain*, povert*, trade*, grow*, social*, environment*, prosperit*, progress*, financ*, cooperation*
```

``` r

corp <- data_corpus_ungd2017 %>% 
    corpus_reshape()

toks <- tokens(corp, remove_url = TRUE, remove_numbers = TRUE) %>% 
    tokens_remove(stopwords("en"), min_nchar = 2, padding = TRUE)
    
dfmt_feat <- dfm(toks, remove_padding = TRUE)
dfmt_label <- tokens_lookup(toks, dict) %>% 
    dfm()

map <- textmodel_wordmap(dfmt_feat, dfmt_label)
coef(map)
## $greeting
##         express    congratulate           thank          thanks congratulations 
##        7.277352        7.106288        7.044730        6.197432        6.037089 
##       expressed       greetings      expression             sir  congratulating 
##        5.763796        5.427324        5.386502        5.299490        5.098820 
## 
## $un
##           session      organization            reform secretary-general 
##          6.828217          6.817435          6.603732          6.527039 
##        resolution       resolutions        conference           charter 
##          6.116124          5.994235          5.981812          5.975542 
##     organizations           reforms 
##          5.841112          5.551029 
## 
## $security
##     peace  security   nuclear terrorism   weapons  conflict       war  peaceful 
##  7.668924  7.528566  6.517346  6.515252  6.266629  6.191280  6.038095  5.975356 
## conflicts    threat 
##  5.931232  5.840797 
## 
## $human
##   citizens  education    protect protection    dignity violations protecting 
##   7.012266   6.894483   6.740332   6.574540   6.470999   6.074584   5.830962 
##  violation  protected       race 
##   5.554708   5.554708   5.171716 
## 
## $democracy
##  government   president  democratic   democracy  leadership     leaders 
##    7.784677    7.360770    6.921754    6.508384    6.440665    6.289694 
##    election governments   elections   represent 
##    6.259617    5.999540    5.589112    5.558807 
## 
## $development
## development sustainable    economic cooperation      social     poverty 
##    8.016598    7.495269    7.006202    6.684847    6.367819    6.233840 
##    progress  developing environment  assistance 
##    6.201050    6.014274    5.885831    5.833736
```

### Predict geographical focus of texts

``` r
dat <- data.frame(text = corp, topic = predict(map))
```

|                | text                                                                                                                                                                                                                  | topic       |
|:---------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------------|
| Afghanistan.1  | As I stand here before the General Assembly today, I am reminded that the wise men and women of 1945 displayed a unique capacity to learn from and act on the lessons of history.                                     | un          |
| Afghanistan.2  | Shaped by the Great Depression and tempered by the carnage of the Second World War, they established global order through institutions that would provide security and stability for generations to come.             | security    |
| Afghanistan.3  | The United Nations, the International Monetary Fund, the World Bank and other organizations were founded to coordinate responses to international challenges and to make crimes against humanity a thing of the past. | un          |
| Afghanistan.4  | There can be little doubt that today the scale, scope and speed of their imagination and efforts have not yet been matched.                                                                                           | security    |
| Afghanistan.5  | But future historians will judge those institutions on how they respond to the challenges of today and the challenges we must confront in the future.                                                                 | development |
| Afghanistan.6  | As global leaders, we seek certainty and familiarity in the rules of the game that dominated the twentieth century.                                                                                                   | democracy   |
| Afghanistan.7  | But in today’s ever-changing world, the dominant contextual characteristic defining our times is extreme uncertainty.                                                                                                 | human       |
| Afghanistan.8  | It is easy to illustrate this uncertainty by looking at threats we are facing - to our economies, our security and our values.                                                                                        | security    |
| Afghanistan.9  | There is an emerging consensus that advanced economies have yet to arrive at proper growth models to overcome high unemployment, decreasing income and wealth inequality.                                             | development |
| Afghanistan.10 | The threat of economic crisis, therefore, still hangs over us.                                                                                                                                                        | security    |

### Create a dictionary

Create a **quanteda** dictionary object from the extracted features. The
dictionary can use to perform analysis of other corpora.

``` r
as.dictionary(map, n = 100)
## Dictionary object with 6 key entries.
## - [greeting]:
##   - express, congratulate, thank, thanks, congratulations, expressed, greetings, expression, sir, congratulating, expressing, lajčák, miroslav, expresses, congratulates, outset, expressions, warm, lajcak, lajcák [ ... and 80 more ]
## - [un]:
##   - session, organization, reform, secretary-general, resolution, resolutions, conference, charter, organizations, reforms, seventy-second, seventy-first, seventy, organization's, reforming, commissioner, lajčák, miroslav, reformed, repositioning [ ... and 80 more ]
## - [security]:
##   - peace, security, nuclear, terrorism, weapons, conflict, war, peaceful, conflicts, threat, solution, crisis, violence, fight, threats, terrorist, force, military, crises, destruction [ ... and 80 more ]
## - [human]:
##   - citizens, education, protect, protection, dignity, violations, protecting, violation, protected, race, violate, violated, citizenship, educational, citizen, violates, protectionism, educated, violating, protects [ ... and 80 more ]
## - [democracy]:
##   - government, president, democratic, democracy, leadership, leaders, election, governments, elections, represent, representative, government's, represents, elected, represented, representation, representatives, leader, electoral, president's [ ... and 80 more ]
## - [development]:
##   - development, sustainable, economic, cooperation, social, poverty, progress, developing, environment, assistance, prosperity, growth, economy, financial, trade, developed, financing, investment, environmental, growing [ ... and 80 more ]
```
