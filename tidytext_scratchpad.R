library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)

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


# other functions for stopwords, and word-level sentiment
# see also sentimentr package for sentence-level sentiment



