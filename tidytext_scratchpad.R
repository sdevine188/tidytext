library(tidyverse)
library(tidytext)
library(janeaustenr)
library(tm)

# https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
# https://tidytextmining.com

# https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
# https://tidytextmining.com

# prepare jane austen data
original_books <- austen_books() %>%
        group_by(book) %>%
        mutate(line = row_number(),
               chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                       ignore_case = TRUE)))) %>%
        ungroup()
original_books

# tokenize text variable to get one word per row
tidy_books <- original_books %>%
        unnest_tokens(output = word, input = text, token = "words")
tidy_books

# can also tokenize with ngrams
tidy_books_2ngram <- original_books %>% 
        unnest_tokens(output = word, input = text, token = "ngrams", n = 2, drop = FALSE)
tidy_books_2ngram

# can also tokenize with regex
# this drops the regex pattern, but if you need to keep it, first replace with something like "azzz123zzz",
# then just use "zzz123" as the regex pattern, which will drop, but leave "a"
tidy_books_regex <- original_books %>% 
        unnest_tokens(output = a_chunks, input = text, token = "regex", pattern = "a|A", drop = FALSE)
tidy_books_regex

# can also tokenize by moving window called character_shingles
tidy_books_4shingles <- original_books %>% 
        unnest_tokens(output = a_chunks, input = text, token = "character_shingles", n = 4, drop = FALSE)
tidy_books_4shingles


#####################################################


# tf-idf

book_words <- austen_books() %>%
        unnest_tokens(output = word, input = text) %>%
        count(book, word) %>% arrange(desc(n)) %>% rename(word_count_per_book = n)
book_words

# number_of_books_containing_each_word
number_of_books_containing_each_word <- book_words %>% count(book, word) %>%
        group_by(word) %>% count() %>% ungroup() %>% arrange(desc(word)) %>% 
        rename(number_of_books_containing_word = n)
number_of_books_containing_each_word

# total_words
total_words_per_book <- book_words %>% 
        group_by(book) %>% 
        summarize(total_words_per_book = sum(word_count_per_book))
total_words_per_book

# get tf-idf score for words
total_number_questions <- nrow(total_words_per_question)
book_words %>% left_join(., total_words_per_book, by = "book") %>%
        left_join(., number_of_books_containing_each_word, by = "word") %>%
        bind_tf_idf(term = word, document = book, n = word_count_per_book) %>%
        mutate(manual_tf = word_count_per_book / total_words_per_book, 
               manual_idf = log(total_number_books / number_of_books_containing_word),
               manual_tf_idf = manual_tf * manual_idf) %>%
        arrange(desc(tf_idf))


######################################################################


# sentiments
# https://www.tidytextmining.com/sentiment.html
# see also sentimentr package, which includes valence shifters: https://github.com/trinker/sentimentr#installation
# also syushet, which sentimentr wraps: https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
sentiments
sentiments %>% count(sentiment)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# stopwords
stop_words
stop_words %>% count(lexicon)

# get tidy_books
tidy_books <- austen_books() %>%
        group_by(book) %>%
        mutate(linenumber = row_number(),
               chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                       ignore_case = TRUE)))) %>%
        ungroup() %>%
        unnest_tokens(word, text)
tidy_books

# get positive/negative sentiment
# note that %/% is integer division, so 5 %/% 2 = 2, which is used to bucket 80 line chunks into each index number
jane_austen_sentiment <- tidy_books %>%
        inner_join(get_sentiments("bing")) %>%
        count(book, index = linenumber %/% 80, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
jane_austen_sentiment


########################################################################
#########################################################################
#########################################################################


# document-term matrix
# https://www.tidytextmining.com/dtm.html
library(topicmodels)
data("AssociatedPress", package = "topicmodels")
AssociatedPress

terms <- Terms(AssociatedPress)
tibble(term = terms) %>% count(term)
str(terms)
head(terms)

# can get same tidy count of terms using  tidy function on associated_press dtm
ap_td <- tidy(AssociatedPress)
ap_td






