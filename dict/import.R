require(quanteda)

data_dictionary_topic <- dictionary(file = 'dict/dictionary.yml')
save(data_dictionary_topic, file = 'data/data_dictionary_topic.RData')
