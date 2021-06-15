## app.R ##
#Libraries
library(shinydashboard)
library(shiny)
library(plotly)
library(DT)
library(RColorBrewer)

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("list-alt")),
        menuItem("Introduction", tabName = "intro", icon = icon("cog", lib = "glyphicon")),
        menuItem("Word Frequencies", tabName = "WF", icon = icon("bar-chart-o")),
        menuItem("Naive Bayes ", tabName = "n_b", icon = icon("table")),
        menuItem("Yes Or No ", tabName = "yes_no", icon = icon("th")),
        menuItem("Bigrams", tabName = "bigram", icon = icon("th")),
        menuItem("Correlations", tabName = "run", icon = icon("th")),
        menuItem("Overall Sentiments", tabName = "sentid", icon = icon("th")),
        menuItem("Insights", tabName = "insight", icon = icon("dashboard")),
        menuItem("Next Steps", tabName = "Recomm", icon = icon("dashboard"))
    )),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "home",
                    tags$img(src="https://user-images.githubusercontent.com/65662486/107910942-53bd3200-6f10-11eb-8ece-3a47ce7ca99c.jpg",
                             height = 1000, width = 1500)
            ),
            #INtrodiction
            tabItem(
                tabName = "intro", # Setting Intro Tab
                h1("Purpose", color = "purple"),
                h1("Are you going to buy an electric vehicle for your next car?"),
                h2("Question 1: What are your thoughts on global warming?"),
                h2("Question 2: What car would you take your favourite celebrity to the beach with?"),
                h2("Question 3: What do you normally use your car for?"),
                h2("Question 4: What would be a great freature for your own imaginery car?"),
                h2("Question 5: Bob Marley or Avicii and why?")
            ),
            
            #Word Frequency
            tabItem(tabName = "WF",
                    fluidRow(
                        plotlyOutput("plot7", height = 800),
                        
                    )
            ),
            #Naive bayes
            tabItem(tabName = "n_b",
                    fluidRow(
                        verbatimTextOutput("naivite"),
                        
                    )
            ),
            #Yes or No Tab 
            tabItem(tabName = "yes_no",
                    fluidRow(
                        
                        
                        tabBox(width = "500px", height = "650px",
                               tabPanel("Total",
                                plotlyOutput("plot3", height = 800)),
                               tabPanel("YES",
                                        plotlyOutput("avgraph1", height = 800)),
                               tabPanel("NO",
                                        plotlyOutput("avgraph2", height = 800))
                               
                        )
                    )
            ),
            #Correlation
            tabItem(
                tabName = "run", # Setting Token Tab
                titlePanel("Corelations per question"),
                fluidRow(tabBox(width = "500px", height = "650px",
                       tabPanel("Question 2",
                                verbatimTextOutput("a")),
                       tabPanel("Question 3",
                                verbatimTextOutput("b")),
                       tabPanel("Question 4",
                                verbatimTextOutput("c")),
                       tabPanel("Question 5",
                                verbatimTextOutput("d"))
                ))
            ),
            #Bigrams 
            tabItem(
                tabName = "bigram", # Setting Token Tab
                titlePanel("Bigrams divided per question"),
                tabBox(width = "500px", height = "650px",
                       tabPanel("YES",
                                plotOutput("plot5", height = 800)),
                       tabPanel("NO",
                                plotOutput("plot6", height = 800))
                )
            ),
            
            #Sentiment Analysis  
            #Bigrams 
            tabItem(
                tabName = "sentid", # Setting Token Tab
                titlePanel("WordCloud | Sentiments"),
                tabBox(width = "500px", height = "650px",
                       tabPanel("Question 1",
                                plotOutput("plot11", height = 700, width =1000)),
                       tabPanel("Question 2",
                                plotOutput("plot12", height = 900, width = 1200)),
                       tabPanel("Question 3",
                                plotOutput("plot13", height = 700, width =1000)),
                       tabPanel("Question 4",
                                plotOutput("plot14", height = 900, width = 1200)),
                       tabPanel("Question 5",
                                plotOutput("plot15", height = 700, width =1000)),
                       tabPanel("Sentiments Total",
                                plotlyOutput("plot16", height = 900, width = 1200)),
                       tabPanel("Sentiments Yes",
                                plotOutput("yes_nrc", height = 700, width =1000)),
                       tabPanel("Sentiments No",
                                plotOutput("no_nrc", height = 900, width = 1200))
                       
                )
            ),
            
            # Final Tab Contenet
            tabItem(tabName = "insight",
                    tags$img(src="https://user-images.githubusercontent.com/65662486/108139409-e70f7800-7074-11eb-801b-4688376186bf.jpg",
                             height = 1000, width = 1500)
            ),
            # Recommended tab
            tabItem(tabName = "Recomm",
                    tags$img(src="https://user-images.githubusercontent.com/65662486/108139580-335ab800-7075-11eb-8e9e-1e283816d932.png",
                             height = 1000, width = 1500)
            )
        )
    )
    ,skin = "yellow")

server <- function(input, output) {
    
 
    
    library(readtext)
    library(tidyverse)
    library(ggplot2)
    library(plotly)
    library(textreadr)
    library(quanteda)
    library(RColorBrewer)
    library(quanteda.textmodels)
    library(ggplot2)
    library(tm)
    library(tidytext)
    library(tidyverse)
    library(dplyr)
    
   
    
    #Naive Bayes part of the Question 
    setwd("/Users/simbamariwande/Desktop/NLP Team Assignment /Edited Documents ")
    nm <- list.files(path="/Users/simbamariwande/Desktop/NLP Team Assignment /Edited Documents ")
    mydf <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))
    
    yes_no <- c(1,1,1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,0,0,1,1,1,1,1,1,1,0,1,0)
    
    msg.dfm <- dfm(mydf, tolower = TRUE) #generating document 
    
    #let's split the docs into training and testing data
    msg.dfm.train<-msg.dfm[1:28,]
    yes_no_train <- yes_no[1:28]
    msg.dfm.test<-msg.dfm[29:30,]
    
    #building the Naive Bayes model:
    NB_classifier <- textmodel_nb(msg.dfm.train, yes_no_train)
    NB_classifier
    summary(NB_classifier)
    
    
    output$naivite <- renderPrint({NB_classifier <- textmodel_nb(msg.dfm.train, yes_no_train)
                NB_classifier
                print(summary(NB_classifier))
    })
    
    
    #####Predictor
    # predicting the testing data
    pred <- predict(NB_classifier, msg.dfm.test)
    pred
    
    
    #Distribution of answers per question 
    surveys_local_path <- "/Users/simbamariwande/Desktop/NLP Team Assignment /Edited Documents "
    setwd(surveys_local_path)
    survey_file_names <- list.files(path = surveys_local_path)
    yes_no <- c(1,1,1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,0,0,1,1,1,1,1,1,1,0,1,0)
    
    
    readFile <- function(fileName) {
        file_data <- data.frame(answer = read_lines(fileName, skip_empty_rows = TRUE)) %>%
            mutate(question = row_number(),
                   file_name = fileName)
        return(file_data)
    }
    
    survey_data <- bind_rows(lapply(survey_file_names, readFile))
    
    for (i in 1:nrow(survey_data)){
        for (j in 1:length(survey_file_names)){
            if (survey_data[i,"file_name"]==survey_file_names[j]){
                survey_data[i,"yes_no"] <- yes_no[j]
            }
        }
    }
    
    
    survey_tokens <- survey_data %>%
        unnest_tokens(word, answer) %>%
        anti_join(stop_words) %>%
        group_by(question) %>%
        count(file_name, word,yes_no, sort = TRUE)
    
    question_01 <- survey_tokens %>%
        filter(question == "1")  %>%
        count(word, yes_no,sort = TRUE)
    
    
    
    questions <- survey_tokens %>%
        group_by(question) %>%
        count(yes_no)
    
    questions$Electric <- "0"
    
    
    for (i in 1:nrow(questions)){
        if (questions[i,"yes_no"]=="1"){
            questions[i,"Electric"] <- "yes"
        } else{
            questions[i,"Electric"] <- "no"
        }
    }
    
    questions <- questions[1:10,]
    
    
    output$plot1 <- renderPlotly({
        ggplotly(
            ggplot(questions, aes(x=question, y=n, fill= Electric))+
                geom_bar(stat= "identity", position=position_dodge())+
                labs(title= "Yes or No per Question", x= "Question Number",
                     y="Words Contribution to Answer")
        
            
        )
    })
    
    output$plot2<-renderPlot({
        
        ggplot(questions, aes(x=question, y=n, fill= Electric))+
            geom_bar(stat= "identity", position=position_dodge())},
        height = 400,width = 600)
    
    output$plot3 <- renderPlotly({
        ggplotly(
            ggplot(questions, aes(x=question, y=n, fill= Electric))+
                geom_bar(stat= "identity", position=position_dodge())+
                labs(title= "Yes or No per Question", x= "Question Number",
                     y="Words Contribution to Answer")+
                scale_fill_hue(l=30)
            
            
        )
    })
    
    
    output$plot4 <- renderPlotly({
        ggplotly(
            ggplot(questions, aes(x=question, y=n, fill= Electric))+
                geom_bar(stat= "identity", position=position_dodge())+
                labs(title= "Yes or No per Question", x= "Question Number",
                     y="Words Contribution to Answer")+
                scale_fill_hue(l=30)
            
            
        )
    })
    
    
    ####### token frequency histograms
    # graphs divide per question who said yes for electric cars
    # WORDS that contributed to answer as "yes"
    library(ggplot2)
    freq_hist <- survey_tokens %>%
        group_by(question)%>%  
        filter(yes_no == 1, n>1) %>%
        count(word, sort=TRUE) %>%
        mutate(word=reorder(word, n)) %>%
        ggplot(aes(word, n))+
        geom_col()+
        xlab(NULL)+
        coord_flip()
    print(freq_hist)
    
    ggplotly(freq_hist)
    
    output$avgraph1<-renderPlotly({print(freq_hist)})
    
    # WORDS that contributed to answer as "no"
    library(ggplot2)
    freq_hist_2 <- survey_tokens %>%
        group_by(question)%>%  
        filter(yes_no == 0, n>1) %>%
        count(word, sort=TRUE) %>%
        mutate(word=reorder(word, n)) %>%
        ggplot(aes(word, n))+
        geom_col()+
        xlab(NULL)+
        coord_flip()
    ggplotly(freq_hist_2)
    
    output$avgraph2<-renderPlotly({print(freq_hist_2)})
    
    
    
    
    
    #Alban
    #Team 15
    library(dplyr)
    library(stringr)
    library(tidytext)
    library(tidyverse)
    library(ggraph)
    library(igraph)
    
    
    txt_file_path = "/Users/simbamariwande/Desktop/NLP Team Assignment /Edited Documents "
    setwd(txt_file_path)
    nm <- list.files(path = txt_file_path)
    
    # using read document to import the data of the text file
    first_file_as_lines <- read_document(file = nm[1]) #This comes out as a vector
    first_file <- paste(first_file_as_lines, collapse = " ") # This will give us a concatenated vector
    
    # function that imports all txt files into a table
    txt_files_data <- do.call(rbind, lapply(nm, function(x) paste(read_document(file = x), collapse = " ")))
    
    test1 <- data.frame(txt_files_data)
    test1$ID <- seq.int(nrow(test1))
    
    yes_no <- c(1,1,1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,0,0,1,1,1,1,1,1,1,0,1,0)
    yes_no_df <- data.frame(yes_no)
    yes_no_df$ID <- seq.int(nrow(yes_no_df))
    y_n <- left_join(test1, yes_no_df)
    
    
    interviews <- y_n %>%
        unnest_tokens(word, txt_files_data) %>%
        count(yes_no, word, ID, sort=TRUE) %>%
        ungroup()
    
    
    total_words <- interviews %>%
        group_by(yes_no) %>%
        summarize(total = sum(n))
    
    inter_words <- left_join(interviews, total_words)
    
    
    
    inter_words <- inter_words %>%
        bind_tf_idf(word, yes_no, n)
    
    inter_words_tfidf <- inter_words %>%
        arrange(desc(tf_idf)) %>%
        filter(n <10)
    
    inter_words_tfidf
    
    
    ###########################
    # Bi-grams for yes and no #
    ###########################
    yes_df <- y_n %>%
        filter(yes_no==1)
    
    no_df <- y_n %>%
        filter(yes_no==0)
    
    #Bi grams for Yes
    yes_bigrams <- yes_df %>%
        unnest_tokens(bigram, txt_files_data, token = "ngrams", n=2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>% 
        filter(!word1 %in% stop_words$word) %>% 
        filter(!word2 %in% stop_words$word) %>% 
        count(word1, word2, sort = TRUE)
    
    yes_bigrams_graph <- yes_bigrams %>%
        filter(n>0.5) %>% #modify the filter to increase the number of connections
        graph_from_data_frame()
    
    
    ###
    # Bi gram for NO
    no_bigrams <- no_df %>%
        unnest_tokens(bigram, txt_files_data, token = "ngrams", n=2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>% 
        filter(!word1 %in% stop_words$word) %>% 
        filter(!word2 %in% stop_words$word) %>% 
        count(word1, word2, sort = TRUE)
    
    no_bigrams_graph <- no_bigrams %>%
        filter(n>0.8) %>% #modify the filter to increase the number of connections
        graph_from_data_frame()
    
    
    output$plot5 <- renderPlot({
        ## Visualisation of the bigrams
        ggraph(yes_bigrams_graph, layout = "fr") +
            geom_edge_link()+
            geom_node_point()+
            geom_node_text(aes(label=name),  vjust =2, hjust=1) 
        
    })
    
    
    
    output$plot6 <- renderPlot({
        # Visualisation of the bigrams
        ggraph(no_bigrams_graph, layout = "fr") +
            geom_edge_link()+
            geom_node_point()+
            geom_node_text(aes(label=name),  vjust =2, hjust=1) 
    })
    
    #Aveka Code 
    nm <- list.files(path="/Users/simbamariwande/Desktop/NLP Team Assignment /Edited Documents ")
    mydf <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))

    my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))
    my_txt_text <- data.frame(my_txt_text)

    ######## tokenizing the mydf dataframe

    token_list <-my_txt_text %>%
        unnest_tokens(word, my_txt_text)
    #no punctutation, no upper case letters
    print(token_list)
    
    ######## token frequencies
    
    frequencies_tokens <- my_txt_text %>%
        unnest_tokens(word, my_txt_text) %>%
        count(word, sort=TRUE)
    print(frequencies_tokens)
    
    
    ######## stop words
    
    library(stringr)
    
    data(stop_words)
    frequencies_tokens_nostop <-my_txt_text%>%
        unnest_tokens(word, my_txt_text) %>%
        anti_join(stop_words) %>% #here's where we remove tokens
        count(word, sort=TRUE) %>%
        filter(n>3) %>%
        mutate(word=reorder(word, n)) 
    
    
    print(frequencies_tokens_nostop)
    
    ####### token frequency histograms

    
    output$plot7 <- renderPlotly({
        ggplotly(
            ggplot(frequencies_tokens_nostop, aes(x=word, y=n))+
                geom_bar(stat= "identity", position=position_dodge())+
                labs(title= "Word Frequencies", x= "Words",
                     y="Number of words")+
                coord_flip()
            
            
        )
    })
    
    library(tidyr)
    frequency <- survey_tokens %>%
        mutate(word=str_extract(word, "[a-z']+")) %>%
        count(question, word) %>%
        group_by(question) %>%
        mutate(proportion = n/sum(n))%>%
        select(-n) %>%
        spread(question, proportion) %>%
        gather(question, proportion, `2`, `3`,`4`,`5`)
    
    frequency <- frequency[,-c(3,4)]
    
    
    #Correlations 
    output$a <- renderPrint({a <- cor.test(data=frequency[frequency$question == "2",],
                  ~proportion + `1`)
    print(a)
    })
    
    output$b <- renderPrint({b <- cor.test(data=frequency[frequency$question == "3",],
                  ~proportion + `1`)
    print(b)
    })
    
    output$c <- renderPrint({c <- cor.test(data=frequency[frequency$question == "4",],
                  ~proportion + `1`)
    print(c)
    })
    
    
    output$d <- renderPrint({d <- cor.test(data=frequency[frequency$question == "5",],
                  ~proportion + `1`)
    print(d)
    })
    
    #Jenny
    library(textreadr)
    #Importing all .txt files from one directory # a txt works like a csv file with multiple rows
    library(dplyr)
    library(stringr)
    # install.packages("tidytext")
    # install.packages("tidyverse")
    library(tidytext)
    library(tidyverse)
    library(stringr)
    
    setwd("/Users/simbamariwande/Desktop/NLP Team Assignment /Edited Documents ")
    Interviews <- list.files(path="/Users/simbamariwande/Desktop/NLP Team Assignment /Edited Documents ")
    my_Interviews <- read_document(file=Interviews[1]) #This comes out as a vector
    my_Interviews_together <- paste(my_Interviews, collapse = " ") # This will give us a concatenated vector
    my_Interviews_txt <- do.call(rbind, lapply(Interviews, function(x) paste(read_document(file=x), collapse = " ")))
    
    
    my_Interviews_df <- data.frame(line=1:30, text=my_Interviews_txt)
    
    ########## Tokenizing mydf dataframe ###########
    
    token_list1 <- my_Interviews_df %>%
        unnest_tokens(word, text)
    #no punctutation, no upper case letters
    
    ############# Removing Stop-words ##################
    
    
    #stop words are words that are commonly used in English 
    # e.g. is, I, are, you, me, the, of, etc.
    #we will use the anti_join(stop_words) to remove the stop words
    
    
    data(stop_words)
    my_Interviews_words <- my_Interviews_df %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>% #here's where we remove tokens
        count(word, sort=TRUE)
    
    
    print(my_Interviews_df)
    
    ######################################################
    ############# Sentiment Analysis #####################
    ######################################################
    
    library(textdata)
    library(dplyr)
    library(stringr)
    library(tidyverse)
    library(tidytext)
    afinn <- get_sentiments("afinn")
    nrc <- get_sentiments("nrc")
    nrc <- lexicon_nrc()
    bing <- get_sentiments("bing")
    
    sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                            mutate(nrc, lexicon= "nrc"),
                            mutate(bing, lexicon="bing")
    )
    
    sentiments %>%
        filter(lexicon == "nrc")
    
    # install.packages("tidyr")
    library(tidyr)
    
    # negative
    nrcnegative <- get_sentiments("nrc") %>%
        filter(sentiment == "negative")
    
    ############## word cloud ################
    ##########################################
    #install.packages("wordcloud")
    library(wordcloud)
    data("stop_words")
    
    output$plot9 <- renderPlot({my_Interviews_words %>%
        with(wordcloud(word, n, max.words = 40, colors = brewer.pal(8,"Dark2"), scale=c(1, 3)))
        })
    
    
    ###################################################
    #### Adding positive and negative sentiments ######
    ###################################################
    #install.packages(("reshape2"))
    library(reshape2)
    
    output$plot10 <- renderPlot({
        
    my_Interviews_words %>%
        inner_join(get_sentiments("nrc")) %>%
        count(word, sentiment, sort=TRUE) %>%
        acast(word ~sentiment, value.var="n", fill=0) %>% 
        comparison.cloud(color = c("red4", "blue4"),
                         max.words=75, scale=c(1, 2))
    
    })
    
    
    #Jenny Code Sentiments
    library(readtext)
    library(tidyverse)
    library(ggplot2)
    library(plotly)
    
    #Distribution of answers per question 
    surveys_local_path <- "/Users/simbamariwande/Desktop/NLP Team Assignment /Edited Documents "
    setwd(surveys_local_path)
    survey_file_names <- list.files(path = surveys_local_path)
    yes_no <- c(1,1,1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,0,0,1,1,1,1,1,1,1,0,1,0)
    
    
    readFile <- function(fileName) {
        file_data <- data.frame(answer = read_lines(fileName, skip_empty_rows = TRUE)) %>%
            mutate(question = row_number(),
                   file_name = fileName)
        return(file_data)
    }
    
    survey_data <- bind_rows(lapply(survey_file_names, readFile))
    
    for (i in 1:nrow(survey_data)){
        for (j in 1:length(survey_file_names)){
            if (survey_data[i,"file_name"]==survey_file_names[j]){
                survey_data[i,"yes_no"] <- yes_no[j]
            }
        }
    }
    
    
    survey_tokens <- survey_data %>%
        unnest_tokens(word, answer) %>%
        anti_join(stop_words) %>%
        group_by(question) %>%
        count(file_name, word,yes_no, sort = TRUE)
    
    

    question_01 <- survey_tokens %>%
        filter(question == "1")  %>%
        count(word, yes_no,sort = TRUE)
    
    question_02 <- survey_tokens %>%
        filter(question == "2")  %>%
        count(word, yes_no,sort = TRUE)
    
    question_03 <- survey_tokens %>%
        filter(question == "3")  %>%
        count(word, yes_no,sort = TRUE)
    
    question_04 <- survey_tokens %>%
        filter(question == "4")  %>%
        count(word, yes_no,sort = TRUE)
    
    question_05 <- survey_tokens %>%
        filter(question == "5")  %>%
        count(word, yes_no,sort = TRUE)
    
    question_06 <- survey_tokens %>%
        filter(question == "6")  %>%
        count(word, yes_no,sort = TRUE)
    
    questions <- survey_tokens %>%
        group_by(question) %>%
        count(yes_no)
    
    questions$Electric <- "0"
    
    for (i in 1:nrow(questions)){
        if (questions[i,"yes_no"]=="1"){
            questions[i,"Electric"] <- "yes"
        } else{
            questions[i,"Electric"] <- "no"
        }
    }
    
    ######################################################
    ############# Sentiment Analysis #####################
    ######################################################
    
    library(textdata)
    library(dplyr)
    library(stringr)
    library(tidyverse)
    library(tidytext)
    afinn <- get_sentiments("afinn")
    nrc <- get_sentiments("nrc")
    nrc <- lexicon_nrc()
    bing <- get_sentiments("bing")
    
    sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                            mutate(nrc, lexicon= "nrc"),
                            mutate(bing, lexicon="bing")
    )
    
    sentiments %>%
        filter(lexicon == "nrc")
    
    # install.packages("tidyr")
    library(tidyr)
    
    
    ############################################################
    #### Word Cloud with Positive and Negative Sentiments ######
    ##################### by Questions #########################
    ############################################################
    
    #install.packages(("reshape2"))
    
    #Graphs for sentiments per question
    
    library(reshape2)
    
    
    #Graph 1 
   
    
    
    output$plot11<- renderPlot({question_01 %>%
        inner_join(get_sentiments("nrc")) %>%
        count(word, sentiment, sort=TRUE) %>%
        acast(word ~sentiment, value.var="n", fill=0) %>% 
        comparison.cloud(color = c("red", "blue"),
                         max.words=50, scale=c(1, 0.7))
    })
    
    
    output$plot12<-renderPlot({question_02 %>%
        inner_join(get_sentiments("nrc")) %>%
        count(word, sentiment, sort=TRUE) %>%
        acast(word ~sentiment, value.var="n", fill=0) %>% 
        comparison.cloud(color = c("red", "blue"),
                         max.words=50, scale=c(1, 0.7))
    })
    
    
    output$plot13 <- renderPlot({question_03 %>%
        inner_join(get_sentiments("nrc")) %>%
        count(word, sentiment, sort=TRUE) %>%
        acast(word ~sentiment, value.var="n", fill=0) %>% 
        comparison.cloud(color = c("red", "blue"),
                         max.words=50, scale=c(1, 0.7))
    })
    
    output$plot14 <- renderPlot({question_04 %>%
        inner_join(get_sentiments("nrc")) %>%
        count(word, sentiment, sort=TRUE) %>%
        acast(word ~sentiment, value.var="n", fill=0) %>% 
        comparison.cloud(color = c("red", "blue"),
                         max.words=50, scale=c(1, 0.7))
    })
    
    output$plot15 <- renderPlot({question_05 %>%
        inner_join(get_sentiments("nrc")) %>%
        count(word, sentiment, sort=TRUE) %>%
        acast(word ~sentiment, value.var="n", fill=0) %>% 
        comparison.cloud(color = c("red", "blue"),
                         max.words=50, scale=c(1, 0.7))
    })
    
    ############################################################
    #### Word Cloud with Positive and Negative Sentiments ######
    ###################### by Yes/No ###########################
    ############################################################
    
    yes_df <- survey_tokens %>%
        filter(yes_no==1)
    
    no_df <- survey_tokens %>%
        filter(yes_no==0)
    
    output$yes_nrc <- renderPlot({yes_df %>%
        inner_join(get_sentiments("nrc")) %>%
        count(word, sentiment, sort=TRUE) %>%
        acast(word ~sentiment, value.var="n", fill=0) %>% 
        comparison.cloud(color = c("red", "blue"),
                         max.words=50, scale=c(1, 0.7))})
    
    output$no_nrc <- renderPlot({no_df %>%
        inner_join(get_sentiments("nrc")) %>%
        count(word, sentiment, sort=TRUE) %>%
        acast(word ~sentiment, value.var="n", fill=0) %>% 
        comparison.cloud(color = c("red", "blue"),
                         max.words=50, scale=c(1, 0.7))})
    
    ############################################################
    ############## Bing Analysis by Questions ##################
    ############################################################ 
    
    library(tidyr)
    
    survey_sentiment <- survey_tokens %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, index = n %/% 80, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
    
    library(ggplot2)
    
   output$plot16<-renderPlotly({ ggplotly(ggplot(survey_sentiment, aes(index, sentiment, fill = word)) +
                 geom_col(show.legend = FALSE) +
                 facet_wrap(~question, ncol = 2, scales = "free_x")
    )
    })
    
    
   
}


shinyApp(ui, server)

