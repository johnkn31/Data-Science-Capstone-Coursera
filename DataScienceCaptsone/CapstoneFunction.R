# functions to use

# read in databases
bigram_db<-readRDS("bigram_db.rds")
trigram_db<-readRDS("trigram_db.rds")
quadgram_db<-readRDS("quadgram_db.rds")

# Create Ngram Matching Functions
bigram <- function(input_words){
    # get word length
    word_length<-length(input_words)
    
    answer<-filter(bigram_db, first_word==input_words[word_length])%>%
        top_n(n = 1) # return the first row if found
    
    answer<-answer$second_word
    
    return(answer)
    
}

trigram <- function(input_words){
    word_length <- length(input_words)
    
    answer<-filter(trigram_db, first_word == input_words[word_length-1], 
                   second_word == input_words[word_length])%>% 
        top_n(n = 1)
    
    answer<-answer$third_word
    
    return(answer)
    
}

quadgram <- function(input_words){
    word_length<-length(input_words)
        
    answer<-filter(quadgram_db, first_word == input_words[word_length-2], 
                   second_word == input_words[word_length-1], 
                   third_word == input_words[word_length])%>% 
        top_n(n = 1)
    
    answer<-answer$fourth_word
    return(answer)
}

# get user input function
clean_up_phrase<-function(input){
    # take in the word_phrase
    # separate out the words into column and need to get the length of word
    input<-data.frame(input)
    input<-iconv(input, to = "ASCII//TRANSLIT")
    input<-tolower(input)
    obj_corpus <- VCorpus(VectorSource(input))
    obj_corpus <- tm_map(obj_corpus, removePunctuation)
    obj_corpus <- tm_map(obj_corpus, stripWhitespace)
    update_obj_corpus <- as.character(obj_corpus[[1]])
    update_obj_corpus <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", update_obj_corpus)
    
    clean_input<-update_obj_corpus
    return(clean_input)
    
}

# predicted word
phrase<-function(word){
    # split the word
    placement_word <- unlist(str_split(word, pattern = " ")) # returns a list; so use unlist
    word_count <- length(placement_word)
    
    # call out the n grams to use
    output<-ifelse(word_count >= 3, quadgram(placement_word),
                   ifelse(word_count == 2, trigram(placement_word),
                          ifelse(word_count == 1, bigram(placement_word), "")))
    
    return(output)
}
















