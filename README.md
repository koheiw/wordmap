
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

**wordmap** is available on CRAN since the v0.8.0 You can install the
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
## Package version: 4.1.0
## Unicode version: 15.1
## ICU version: 74.1
## Parallel computing: 16 of 16 threads used.
## See https://quanteda.io for tutorials and examples.
require(wordmap)
## Loading required package: wordmap

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
##       11.986522       11.814299       11.752273       10.894463       10.730899 
##       expressed       greetings      expression             sir  congratulating 
##       10.450737       10.102669       10.060145        9.969254        9.758163 
## 
## $un
##           session      organization            reform secretary-general 
##          9.148177          9.137369          8.923089          8.846157 
##        resolution       resolutions        conference           charter 
##          8.433594          8.311070          8.298578          8.292274 
##     organizations           reforms 
##          8.157032          7.864775 
## 
## $security
##     peace  security   nuclear terrorism   weapons  conflict       war  peaceful 
##  9.487776  9.347336  8.335005  8.332907  8.083790  8.008265  7.854679  7.791758 
## conflicts    threat 
##  7.747497  7.656765 
## 
## $human
##   citizens  education    protect protection    dignity violations protecting 
##  10.615543  10.497122  10.342014  10.175012  10.070607   9.669904   9.422653 
##  violation  protected       race 
##   9.141037   9.141037   8.747592 
## 
## $democracy
##  government   president  democratic   democracy  leadership     leaders 
##   10.751528   10.326801    9.886474    9.471213    9.403103    9.251157 
##    election governments   elections   represent 
##    9.220866    8.958655    8.543480    8.512740 
## 
## $development
## development sustainable    economic cooperation      social     poverty 
##   10.045804    9.524120    9.034501    8.712604    8.394841    8.260474 
##    progress  developing environment  assistance 
##    8.227581    8.040148    7.911177    7.858847
```

### Predict topics of sentences

``` r
dat <- data.frame(text = corp, topic = predict(map))
```

|  | text | topic |
|:---|:---|:---|
| Afghanistan.1 | As I stand here before the General Assembly today, I am reminded that the wise men and women of 1945 displayed a unique capacity to learn from and act on the lessons of history. | un |
| Afghanistan.2 | Shaped by the Great Depression and tempered by the carnage of the Second World War, they established global order through institutions that would provide security and stability for generations to come. | security |
| Afghanistan.3 | The United Nations, the International Monetary Fund, the World Bank and other organizations were founded to coordinate responses to international challenges and to make crimes against humanity a thing of the past. | un |
| Afghanistan.4 | There can be little doubt that today the scale, scope and speed of their imagination and efforts have not yet been matched. | security |
| Afghanistan.5 | But future historians will judge those institutions on how they respond to the challenges of today and the challenges we must confront in the future. | human |
| Afghanistan.6 | As global leaders, we seek certainty and familiarity in the rules of the game that dominated the twentieth century. | democracy |
| Afghanistan.7 | But in today’s ever-changing world, the dominant contextual characteristic defining our times is extreme uncertainty. | security |
| Afghanistan.8 | It is easy to illustrate this uncertainty by looking at threats we are facing - to our economies, our security and our values. | security |
| Afghanistan.9 | There is an emerging consensus that advanced economies have yet to arrive at proper growth models to overcome high unemployment, decreasing income and wealth inequality. | development |
| Afghanistan.10 | The threat of economic crisis, therefore, still hangs over us. | security |

### Create a topic dictionary

Create a **quanteda** dictionary object from the extracted features. The
dictionary could be use to perform analysis of other corpora.

``` r
as.dictionary(map, n = 100)
## Dictionary object with 6 key entries.
## - [greeting]:
##   - express, congratulate, thank, thanks, congratulations, expressed, greetings, expression, sir, congratulating, expressing, expresses, congratulates, expressions, lajčák, miroslav, outset, warmly, warm, preside [ ... and 80 more ]
## - [un]:
##   - session, organization, reform, secretary-general, resolution, resolutions, conference, charter, organizations, reforms, seventy-first, seventy, organization's, reforming, commissioner, seventy-second, reformed, repositioning, twenty-third, organizational [ ... and 80 more ]
## - [security]:
##   - peace, security, nuclear, terrorism, weapons, conflict, war, peaceful, conflicts, threat, solution, crisis, violence, fight, threats, terrorist, force, military, crises, destruction [ ... and 80 more ]
## - [human]:
##   - citizens, education, protect, protection, dignity, violations, protecting, violation, protected, race, violate, violated, citizenship, educational, citizen, violates, protectionism, educated, violating, protects [ ... and 80 more ]
## - [democracy]:
##   - government, president, democratic, democracy, leadership, leaders, election, governments, elections, represent, representative, government's, represents, elected, represented, representation, representatives, leader, electoral, president's [ ... and 80 more ]
## - [development]:
##   - development, sustainable, economic, cooperation, social, poverty, progress, developing, environment, assistance, prosperity, growth, economy, financial, trade, developed, financing, investment, environmental, growing [ ... and 80 more ]
```
