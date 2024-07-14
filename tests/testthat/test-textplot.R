require(quanteda)

toks_test <- tokens(data_corpus_inaugural, remove_punct = TRUE)
dfmt_test <- dfm(toks_test) %>%
    dfm_remove(stopwords("en"))

smat_test <- xtabs( ~ docid(dfmt_test) + dfmt_test$Party, sparse = TRUE)
smat_test <- smat[,c("Republican", "Democratic")]

test_that("textplot_terms works with dictionary", {

    map1 <- textmodel_wordmap(dfmt_test, smat_test)

    expect_silent(print(
        textplot_terms(map1, data_dictionary_LSD2015, max_highlighted = 10)
    ))
    expect_silent(print(
        textplot_terms(map1, dictionary(list(none = "xxxxx")))
    ))
})

