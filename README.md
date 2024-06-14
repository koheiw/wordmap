
# Wordmap: Semi-supervised Multinomial Document Classifier

**wordmap** is a semi-supervised algorithm for multinomial document
classification originally created for
[newsmap](https://github.com/koheiw/newsmap). **wordmap** is separated
from **newsmap** to expand the score of its application beyond
geographical classification of news.

The algorithm is also useful in extracting features associated with
document meta-data (industry group, patent class etc.) from vary larger
corpora. The list of features could be used to create a lexicon to
perform dictionary analysis.

## How to install

**wordmap** is available on CRAN since the v0.9.1 You can install the
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

In this example, we identify topics of sentences from using a seed topic
dictionary adopted from [Watanabe & Zhou
(2020)](https://journals.sagepub.com/doi/full/10.1177/0894439320907027).
`data_corpus_ungd2017` contains transcripts of speeches delivered at the
United Nations General Assembly in 2017.

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
```

``` r

dict <- data_dictionary_topic
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
    tokens_remove(stopwords("en"), min_nchar = 2, padding = TRUE) #%>% 
    #tokens_remove("^[A-Z]", valuetype = "regex", case_insensitive = FALSE, padding = TRUE)
    
dfmt_feat <- dfm(toks, remove_padding = TRUE) %>% 
    dfm_trim(min_termfreq = 5)
dfmt_label <- tokens_lookup(toks, dict) %>% 
    dfm()

map <- textmodel_wordmap(dfmt_feat, dfmt_label)
coef(map)
## $greeting
##         express    congratulate           thank          thanks congratulations 
##        7.675771        7.504707        7.443149        6.595851        6.435508 
##       expressed       greetings      expression             sir  congratulating 
##        6.162215        5.825743        5.784921        5.697909        5.497239 
## 
## $un
##           session      organization            reform secretary-general 
##          6.882983          6.872202          6.658498          6.581806 
##        resolution       resolutions        conference           charter 
##          6.170891          6.049001          6.036579          6.030309 
##     organizations           reforms 
##          5.895879          5.605796 
## 
## $security
##     peace  security   nuclear terrorism   weapons  conflict       war  peaceful 
##  7.680667  7.540309  6.529089  6.526995  6.278372  6.203023  6.049838  5.987099 
## conflicts    threat 
##  5.942974  5.852540 
## 
## $human
##   citizens  education    protect protection    dignity violations protecting 
##   7.244883   7.127100   6.972950   6.807157   6.703617   6.307201   6.063579 
##  violation  protected       race 
##   5.787326   5.787326   5.404334 
## 
## $democracy
##  government   president  democratic   democracy  leadership     leaders 
##    7.933901    7.509994    7.070978    6.657607    6.589889    6.438918 
##    election governments   elections   represent 
##    6.408841    6.148764    5.738336    5.708031 
## 
## $development
## development sustainable    economic cooperation      social     poverty 
##    8.045268    7.523939    7.034871    6.713516    6.396488    6.262509 
##    progress  developing environment  assistance 
##    6.229719    6.042943    5.914501    5.862406
```

### Predict topics of sentences

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
| Afghanistan.7  | But in today’s ever-changing world, the dominant contextual characteristic defining our times is extreme uncertainty.                                                                                                 | security    |
| Afghanistan.8  | It is easy to illustrate this uncertainty by looking at threats we are facing - to our economies, our security and our values.                                                                                        | security    |
| Afghanistan.9  | There is an emerging consensus that advanced economies have yet to arrive at proper growth models to overcome high unemployment, decreasing income and wealth inequality.                                             | development |
| Afghanistan.10 | The threat of economic crisis, therefore, still hangs over us.                                                                                                                                                        | security    |

### Create a topic dictionary

Create a **quanteda** dictionary object from the extracted features. The
dictionary could be use to perform analysis of other corpora.

``` r
as.dictionary(map, n = 100)
## Dictionary object with 6 key entries.
## - [greeting]:
##   - express, congratulate, thank, thanks, congratulations, expressed, greetings, expression, sir, congratulating, expressing, lajčák, miroslav, expresses, congratulates, outset, expressions, warm, election, warmly [ ... and 80 more ]
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
