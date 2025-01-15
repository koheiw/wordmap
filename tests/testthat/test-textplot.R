require(quanteda)

toks_test <- tokens(data_corpus_inaugural, remove_punct = TRUE)
dfmt_test <- dfm(toks_test) %>%
    dfm_remove(stopwords("en"))

smat_test <- xtabs( ~ docid(dfmt_test) + dfmt_test$Party, sparse = TRUE)
smat_test <- smat_test[,c("Republican", "Democratic")]

test_that("textplot_terms works with dictionary", {

    wmp1 <- textmodel_wordmap(dfmt_test, smat_test)

    expect_s3_class(
        textplot_terms(wmp1, data_dictionary_LSD2015, max_highlighted = 10),
        c("gg", "ggplot")
    )

    expect_s3_class(
        textplot_terms(wmp1, dictionary(list(none = "xxxxx"))),
        c("gg", "ggplot")
    )

    wmp2 <- textmodel_wordmap(dfmt_test, smat_test, entropy = "global")

    expect_s3_class(
        textplot_terms(wmp2, data_dictionary_LSD2015, max_highlighted = 10),
        c("gg", "ggplot")
    )

    expect_s3_class(
        textplot_terms(wmp2, dictionary(list(none = "xxxxx"))),
        c("gg", "ggplot")
    )
})

