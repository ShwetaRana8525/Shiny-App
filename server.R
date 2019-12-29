#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # read input - file and keyword list
    Dataset <- reactive({
        if (is.null(input$corpus)) { return(NULL) }
        else{
            corpus_text <- readLines(input$corpus$datapath)
            return(corpus_text)
        }
    })  
    
    keywordstr <- reactive({
        
        return(input$keywords)
    })
    
    # global frame declaration for bar chart
    keyworddf <- data.frame(word=NA, count = 0)
    
    
    # function to search the keyword in corpus by tokenising at sentences
    keywordsearch <- function() {
            corpus_text = Dataset() 
            
            require(tibble)
            library(stringr)
            require(tidytext); 
            require(tidyverse)

            # split the key word by comma 
            keywordList <- unlist(strsplit(keywordstr(), ","))
           
           
           # generate output when both inputs are given
           if(is.null(corpus_text) || length(keywordList) < 2) {
              return(NULL)
            }
            
            textdf = tibble(text = corpus_text) 
            
          
            # Tokenizing in sentences``````````````````
            sent_tokenized = textdf %>% 
                unnest_tokens(sentence, text, token = "sentences") %>% 
                mutate(sent_id = row_number()) %>% 
                select(sent_id,sentence)
            
            
            sent_tokenized$count <- 0
            
            i = 1;
            # set the count at sentence level and also word level
            for (current_word in keywordList) {
                count_keyword <- sent_tokenized$sentence %>% str_count(current_word) 
                sent_tokenized$count <- count_keyword + sent_tokenized$count
                
                keyworddf[i,'word'] <<- current_word
                keyworddf[i,'count'] <<- sum(count_keyword)
                i = i+1
            }
            
            return(sent_tokenized)
        }
        
    
   # render first tab and print sentences containing having any of given keywords
    output$keywordfiltered = renderTable({
        out = keywordsearch()
        out[out$count > 0,"sentence"]
    })
    
    # Display the relative frequencies of the occurrence of the keywords in your corpus as a bar-chart
    output$relativefreq = renderPlot({
        # print(keyworddf)
        ggplot(keyworddf, aes(word, count/sum(count))) +
            geom_bar(stat = "identity", col = "red", fill = "red")
    })
    

 
    # Helper method to generate dtm out of corpus
    dtm_build <- function(raw_corpus, tfidf=FALSE)
    {                  # func opens
        
        require(tidytext); require(tibble); require(tidyverse)
        
        # converting raw corpus to tibble to tidy DF
        textdf = data_frame(text = raw_corpus);    textdf  
        
        tidy_df = textdf %>%   
            mutate(doc = row_number()) %>%
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>%
            group_by(doc) %>%
            count(word, sort=TRUE)
        
        textdf1 = tidy_df %>% rename(value = n) 
        
        dtm = textdf1 %>% cast_sparse(doc, word, value);    
        return(dtm)  
    }   # func ends
    
    # Helper method to build wordcloud
    build_wordcloud <- function(dtm){          # write within double quotes
        
        require(wordcloud)
        tsum = apply(dtm, 2, sum)
        
        tsum = tsum[order(tsum, decreasing = T)]       # terms in decreasing order of freq
        
        wordcloud(names(tsum), 
                  tsum,     # words, their freqs
                  scale = c(3.5, 0.5),     # range of word sizes
                  max.words = 50,       # max #words
                  colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
        
    } # func ends
    
    
    # print word cloud in third tab
    output$wordcloudplot <- renderPlot({
        out = Dataset()
        
        corpus = as.vector(out)
        
        dtm_data = dtm_build(corpus)
        
        build_wordcloud(dtm_data)
        
    })
    

})


