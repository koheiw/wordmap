#' UN General Debate speeches from 2017
#'
#' A corpus of 196 speeches from the 2017 UN General Debate (Mikhaylov and
#' Baturo, 2017). The economic data for 2017 (GDP and GDP per capita) are
#' downloaded from the World Bank website.
#' @format
#'   The corpus includes the following document variables: \describe{
#'   \item{country_iso}{ISO3c country code, e.g. "AFG" for Afghanistan}
#'   \item{un_session}{UN session, a numeric identifier (in this case, 72)}
#'   \item{year}{4-digit year (2017).}
#'   \item{country}{country name, in English.}
#'   \item{continent}{continent of the country, one of: Africa, Americas, Asia,
#'   Europe, Oceania. Note that the speech delivered on behalf of the
#'   European Union is coded as "Europe".}
#'   \item{gdp}{GDP in $US for 2017, from the World Bank. Contains missing
#'   values for 9 countries.}
#'   \item{gdp_per_capita}{GDP per capita in $US for 2017, derived from the
#'   World Bank. Contains missing values for 9 countries.}
#'   }
#' @source Mikhaylov, M., Baturo, A., & Dasandi, N. (2017). "United Nations
#'   General Debate Corpus". doi:10.7910/DVN/0TJX8Y. Harvard Dataverse, V4.
#' @references Baturo, A., Dasandi, N., & Mikhaylov, S. (2017). "Understanding
#'   State Preferences With Text As Data: Introducing the UN General Debate
#'   Corpus". doi:10.1177/2053168017712821. *Research and Politics*.
"data_corpus_ungd2017"

#' Seed topic dictionary
#'
#' A dictionary with seed words for size common topics at the United Nations
#' General Assembly (Watanabe and Zhou, 2020).
#' @name data_dictionary_topic
#' @docType data
#' @author Kohei Watanabe \email{watanabe.kohei@gmail.com}
#' @references Watanabe, Kohei & Zhou, Yuan (2020). "Theory-Driven Analysis of
#'   Large Corpora: Semisupervised Topic Classification of the UN Speeches".
#'   doi:10.1177/0894439320907027. *Social Science Computer Review*.
#' @keywords data
"data_dictionary_topic"
