
# Newsmap: geographical document classifier

<!-- badges: start -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/newsmap)](https://CRAN.R-project.org/package=newsmap)
[![Downloads](https://cranlogs.r-pkg.org/badges/newsmap)](https://CRAN.R-project.org/package=newsmap)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/newsmap?color=orange)](https://CRAN.R-project.org/package=newsmap)
[![R build
status](https://github.com/koheiw/newsmap/workflows/R-CMD-check/badge.svg)](https://github.com/koheiw/newsmap/actions)
[![codecov](https://codecov.io/gh/koheiw/newsmap/branch/master/graph/badge.svg)](https://codecov.io/gh/koheiw/newsmap)
<!-- badges: end -->

Semi-supervised Bayesian model for geographical document classification.
Newsmap automatically constructs a large geographical dictionary from a
corpus to accurate classify documents. Currently, the **newsmap**
package contains seed dictionaries in multiple languages that include
*English*, *German*, *French*, *Spanish*, *Portuguese*, *Russian*,
*Italian*, *Arabic*, *Turkish*, *Hebrew*, *Japanese*, *Chinese*.

The detail of the algorithm is explained in [Newsmap: semi-supervised
approach to geographical news
classification](https://www.tandfonline.com/eprint/dDeyUTBrhxBSSkHPn5uB/full).
**newsmap** has also been used in scientific research in various fields
([Google
Scholar](https://scholar.google.com/scholar?oi=bibs&hl=en&cites=3438152153062747083)).

## How to install

**newsmap** is available on CRAN since the version 0.6. You can install
the package using R Studio GUI or the command.

``` r
install.packages("newsmap")
```

If you want to the latest version, please install by running this
command in R. You need to have **devtools** installed beforehand.

``` r
install.packages("devtools")
devtools::install_github("koheiw/wordmap")
```

## Example

In this example, using a text analysis package
[**quanteda**](https://quanteda.io) for preprocessing of textual data,
we train a geographical classification model on a [corpus of news
summaries collected from Yahoo
News](https://www.dropbox.com/s/e19kslwhuu9yc2z/yahoo-news.RDS?dl=1) via
RSS in 2014.

### Download example data

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

``` r
as.dictionary(map, n = 20)
## Dictionary object with 6 key entries.
## - [greeting]:
##   - express, congratulate, thank, thanks, congratulations, expressed, greetings, expression, sir, congratulating, expressing, lajčák, miroslav, expresses, congratulates, outset, expressions, warm, lajcak, lajcák
## - [un]:
##   - session, organization, reform, secretary-general, resolution, resolutions, conference, charter, organizations, reforms, seventy-second, seventy-first, seventy, organization's, reforming, commissioner, lajčák, miroslav, reformed, repositioning
## - [security]:
##   - peace, security, nuclear, terrorism, weapons, conflict, war, peaceful, conflicts, threat, solution, crisis, violence, fight, threats, terrorist, force, military, crises, destruction
## - [human]:
##   - citizens, education, protect, protection, dignity, violations, protecting, violation, protected, race, violate, violated, citizenship, educational, citizen, violates, protectionism, educated, violating, protects
## - [democracy]:
##   - government, president, democratic, democracy, leadership, leaders, election, governments, elections, represent, representative, government's, represents, elected, represented, representation, representatives, leader, electoral, president's
## - [development]:
##   - development, sustainable, economic, cooperation, social, poverty, progress, developing, environment, assistance, prosperity, growth, economy, financial, trade, developed, financing, investment, environmental, growing
```
