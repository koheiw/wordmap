% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textmodel.R
\name{textmodel_wordmap}
\alias{textmodel_wordmap}
\title{A model for multinomial feature extraction and document classification}
\usage{
textmodel_wordmap(
  x,
  y,
  label = c("all", "max"),
  smooth = 0.01,
  boolean = FALSE,
  drop_label = TRUE,
  entropy = c("none", "global", "local", "average"),
  residual = FALSE,
  verbose = quanteda_options("verbose"),
  ...
)
}
\arguments{
\item{x}{a dfm or fcm created by \code{\link[quanteda:dfm]{quanteda::dfm()}}}

\item{y}{a dfm or a sparse matrix that record class membership of the
documents. It can be created applying \code{\link[quanteda:dfm_lookup]{quanteda::dfm_lookup()}} to \code{x}.}

\item{label}{if "max", uses only labels for the maximum value in each row of
\code{y}.}

\item{smooth}{the amount of smoothing in computing coefficients.
When \code{smooth = 0.01}, 1\% of the mean frequency of words in each class
is added to smooth likelihood ratios.}

\item{boolean}{if \code{TRUE}, only consider presence or absence of features in
each document to limit the impact of words repeated in few documents.}

\item{drop_label}{if \code{TRUE}, drops empty columns of \code{y} and ignore their
labels.}

\item{entropy}{the scheme to compute the entropy to
regularize likelihood ratios. The entropy of features are computed over
labels if \code{global} or over documents with the same labels if \code{local}. Local
entropy is averaged if \code{average}. See the details.}

\item{residual}{if \code{TRUE}, a residual class is added to \code{y}. It is named
"other" but can be changed via \code{base::options(wordmap_residual_name)}.}

\item{verbose}{if \code{TRUE}, shows progress of training.}

\item{...}{additional arguments passed to internal functions.}
}
\value{
Returns a fitted textmodel_wordmap object with the following
elements: \item{model}{a matrix that records the association between
classes and features.}
\item{data}{the original input of \code{x}.}
\item{feature}{the feature set in \code{x}}
\item{class}{the class labels in \code{y}.}
\item{concatenator}{the concatenator in \code{x}.}
\item{entropy}{the scheme to compute entropy weights.}
\item{boolean}{the use of the Boolean transformation of \code{x}.}
\item{call}{the command used to execute the function.}
\item{version}{the version of the wordmap package.}
}
\description{
Wordmap is a model for multinomial feature extraction and document
classification. Its naive Bayesian algorithm allows users to train the model
on a large corpus with noisy labels given by document meta-data or keyword
matching.
}
\details{
Wordmap learns association between words in \code{x} and classes in \code{y}
based on likelihood ratios. The large
likelihood ratios tend to concentrate to a small number of features but the
entropy of their frequencies over labels or documents helps to disperse the
distribution.

A residual class is created internally by adding a new column to \code{y}.
The column is given 1 if the other values in the same row are all zero
(i.e. \code{rowSums(y) == 0}); otherwise 0. It is useful when users cannot create
an exhaustive dictionary that covers all the categories.
}
\examples{
require(quanteda)

# split into sentences
corp <- corpus_reshape(data_corpus_ungd2017)

# tokenize
toks <- tokens(corp, remove_punct = TRUE) \%>\%
   tokens_remove(stopwords("en"))

# apply seed dictionary
toks_dict <- tokens_lookup(toks, data_dictionary_topic)

# form dfm
dfmt_feat <- dfm(toks)
dfmt_dict <- dfm(toks_dict)

# fit wordmap model
map <- textmodel_wordmap(dfmt_feat, dfmt_dict)
coef(map)
predict(map)

}
\references{
Watanabe, Kohei (2018). "Newsmap: semi-supervised approach to
geographical news classification". doi.org/10.1080/21670811.2017.1293487,
\emph{Digital Journalism}.

Watanabe, Kohei & Zhou, Yuan (2020). "Theory-Driven Analysis of
Large Corpora: Semisupervised Topic Classification of the UN Speeches".
doi:10.1177/0894439320907027. \emph{Social Science Computer Review}.
}
