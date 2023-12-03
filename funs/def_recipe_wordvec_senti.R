def_recipe_wordvec_senti <- function(data_train, path_wordvec) {
  
  # get project configuration:
  #config <- config::get()
  
  # load libs:
  library(syuzhet)
  library(tidymodels)
  library(dplyr)
  library(stringr)
  library(textfeatures)
  library(textrecipes)
  library(arrow)
  
  # import data:
  data("sentiws", package = "pradadata")
  data("wild_emojis", package = "pradadata")
  data("schimpfwoerter", package = "pradadata")
  
  # preprocess dictionaries:
  sentiws$word <- tolower(sentiws$word)
  schimpfwoerter$value <- 1
  
  wild_emojis$value <- 1
  
  # import word embeddings:
  wiki_de_embeds <- arrow::read_feather(file = path_wordvec)
  names(wiki_de_embeds)[1] <- "word"
  wiki <- as_tibble(wiki_de_embeds)   
  
  
  # reduce data to 3 columns for efficiency:
  d_reduced <- data_train %>% select(text, c1, id)
  
  # define preprocessing of data:
  recipe_def <-
    
    # define model term: output ~ input variables:
    recipe(c1 ~ ., data = d_reduced) %>%
    
    # exclude id variable from modelling:
    update_role(id, new_role = "id") %>%
    
    # ascify text:
    step_text_normalization(text) %>%
    
    # count words with emotional connotation:
    step_mutate(emo_count = get_sentiment(text,
                                          method = "custom",
                                          lexicon = sentiws)) %>% 
    
    # count abusive words:
    step_mutate(schimpf_count = get_sentiment(text,
                                              method = "custom",
                                              lexicon = schimpfwoerter)) %>% 
    
    # # count wild emojis:
    # step_mutate(wild_emoji_count =  get_sentiment(text, 
    #                                               method = "custom",
    #                                               lexicon = wild_emojis)) |> 
    
    # count emojis:
    step_mutate(emoji_count =  str_count(text, "\\p{So}")) %>%   # code for Emojis/Sonderzeichen
    
    
    # copy text column:
    step_mutate(text_copy = text) %>% 
    
    # convert text column into text features such as character count etc:
    step_textfeature(text_copy) %>% 
    
    # tokenize texts (words):
    step_tokenize(text) %>%
    
    # remove stopwords: 
    step_stopwords(text, language = "de", stopword_source = "snowball") %>%
    
    # add word embeddings:
    step_word_embeddings(text,
                           embeddings = wiki,
                           aggregation = "mean")  |> 
         
    # remove zero variance (zv) variables:
    step_zv(all_predictors()) %>%
    
    # z-transform:
    step_normalize(all_numeric_predictors()) %>% 
    
    # impute missing values:
    step_impute_mean(all_numeric_predictors()) %>% 
    
    # transform integer to numeric (some models appear to like that):
    step_mutate(across(where(is.integer), as.numeric))
  
  return(recipe_def)
}


