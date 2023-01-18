#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(easyPubMed)
library(tidyverse)
library(kableExtra)
library(wordcloud2)
library(tm)
library(wordcloud)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Create a Wordcloud from Pubmed Abstracts"),
  h5("Autor: Husain Attarwala"),
  
  # Text in put
  sidebarLayout(
    sidebarPanel(
      
      h5("This app is a fun tool for researchers and academics who want to gain insight into the work of a specific author without having to read through countless abstracts. By simply providing the author's name, the app creates a word cloud based on the frequency of words in the abstracts, giving you the opportunity to see the most frequently used words in the author's research, and maybe discover that your favorite author is obsessed with {pizza} or {cat videos} in his/her abstracts."),
      textInput("author",
                "Author name:"
      ),
      
      #Action button
      actionButton("do","Generate Wordcloud"),
      h5("Note: Please hit refresh before generating new wordcloud, else app will take longer time to load. \n Some searches may take longer - please wait a min or two.")),
    
    # Show a plot of the generated distribution
    mainPanel(
      wordcloud2Output("cloud"),br(),
      br()
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  # Create word cloud function
  
  library(easyPubMed)
  library(tidyverse)
  library(kableExtra)
  library(wordcloud2)
  library(tm)
  library(wordcloud)
  
  
  # Query
  
  
  pubmed_wordcloud <- function(Author="Husain Attarwala",keywords=NULL){
    my_query=str_c(Author,'[AUTH]',"AND ",keywords)
    
    my_query <- get_pubmed_ids(my_query)
    
    # Fetch data
    my_abstracts_xml <- fetch_pubmed_data(my_query)  
    
    # Store Pubmed Records as elements of a list
    all_xml <- articles_to_list(my_abstracts_xml)
    
    t.start <- Sys.time()
    
    # Perform operation (use lapply here, no further parameters)
    final_df <- do.call(rbind, lapply(all_xml, article_to_df, 
                                      max_chars = -1, getAuthors = FALSE))
    
    # Final time: record
    t.stop <- Sys.time()
    
    # How long did it take?
    print(t.stop - t.start)
    final_df[,c("pmid", "year", "abstract")]  %>%
      head() %>% kable() %>% kable_styling(bootstrap_options = 'striped')
    
    
    
    #Create a vector containing only the text
    text <- final_df$abstract
    # Create a corpus  
    docs <- Corpus(VectorSource(text))
    
    
    
    docs <- docs %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    
    
    # Slice top 250 words
    
    df1 <- df %>% arrange(desc(freq)) %>% slice(1:250)
    
    # Draw word cloud
    
           wordcloud2(df1, color = brewer.pal(8, "Dark2"),shape = "circle",fontWeight = "bold",size=0.7,gridSize = 0,fontFamily = "Avenir",backgroundColor = "white") 
    
    
  }
  
  # modular function to draw word clould, takes author name as an argument - try typing in your name
  
  
  # in case there others that share same name, you can add a keyword argument to the function
  pubmed_wordcloud(Author = "husain attarwala",keywords = "dose")
  observeEvent(input$do,{
    updateActionButton(session, "do",
                       label = "Please wait, it may take a few mins")
    output$cloud <- renderWordcloud2({
      
      pubmed_wordcloud(Author = input$author)})
    updateActionButton(session, "do",
                       label = "Generate wordcloud")
    #shinyjs::reset("form")
  } )
  
  
  
  
  
  
  
  
  
  
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
