##############################
# LIBRARIES

# R and Shiny
library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
library(shinybusy)
library(shinyalert)
library(DT)

# Wordclouds
# SEE: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
library(wordcloud)
library(RColorBrewer)

# PDF handling
library(pdftools)
library(pdfsearch)

# Text mining and summarisation
library(tm)
library(lexRankr)

# OCR
library(tesseract)

# Scraping webpages
library(tidyverse)
library(rvest)

# String manipulation
library(stringr)
library(stringi)

# CSV read handling
library(readr)


##############################
# DATA
# Read in the corpus
# User readr version of read_csv to handle EOF in quoted strings
PFIZER.CORPUS <- readr::read_csv("corpus-staging.csv", quote = "\"'", show_col_types = FALSE)
PFIZER.SOURCES <- PFIZER.CORPUS  %>%
  group_by(filename) %>%
  filter(page_num == max(as.numeric(page_num))) 

# Reformat the filename
PFIZER.SOURCES$filename <- paste0(tools::file_path_sans_ext(PFIZER.SOURCES$filename), ".pdf")  
PFIZER.SOURCES$filename <- sub(".-", "", PFIZER.SOURCES$filename)  


##############################
# FUNCTIONS

# Cleaning up strings for regex and escaping reserved characters
# SEE: https://stackoverflow.com/questions/14836754/is-there-an-r-function-to-escape-a-string-for-regex-characters
quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}


################################################
# CODE

# Code to press the 'search' button on enter
# SEE: https://stackoverflow.com/questions/32335951/using-enter-key-with-action-button-in-r-shiny
js.code.enter <- '
$(document).keyup(function(event) {
  if ($("#keywords").is(":focus") && (event.key == "Enter")) {
    $("#keyword_search_btn").click();
  }
});
'

# CSS for the UI
button_color_css <- "
#SearchBtnBlue {
  background: DodgerBlue;
  font-size: 15px;
}
.rightAlign{float:right;}

.centreDiv {
  text-align: center;
  vertical-align: middle;
}
.centreBox {
  margin: 10px 0px 10px 0px;
  height: 130px;
  text-align: center;
  vertical-align: middle;
  padding-top: 15px;
  color: antiquewhite;
}
.centreBox a {
  color: #8df1ec;
}
.ageBackground { background: #f3f3f3; 
                 color: #ffffff; 
                 border: 2px solid #b3adad;
}
.plumBackground { background: #d75e85; }
.plumBackground h2 { font-size: 35px; font-weight: 500; font-family: system-ui; }
.plumBackground2 { background: #bf3b65; }
.plumBackground2 h2 { font-size: 35px; font-weight: 500; font-family: system-ui; }
.plumBackground3 { background: #95284a; }
.plumBackground3 h2 { font-size: 35px; font-weight: 500; font-family: system-ui; }

"

##############################
# UI
ui <- fluidPage(
  #Navbar structure for UI
  navbarPage("Abstractor: STAGING SERVER", theme = shinytheme("flatly"),
             tabPanel("Keywords", fluid = TRUE, icon = icon("file-alt"),
                      tags$style(button_color_css),
                      tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
                      tags$script(type="text/javascript", src="//s7.addthis.com/js/300/addthis_widget.js#pubid=ra-55a2c0ea679e0df1"),
  
                      # Google Analytics
                      tags$head(includeHTML(("google-analytics.html"))),

                      tags$head(tags$link(rel="shortcut icon", href="favicon/database-32-171970.png", sizes="16x16")),
                      tags$head(tags$link(rel="shortcut icon", href="favicon/database-32-171970.png", sizes="32x32")),
                      tags$head(tags$link(rel="canonical", href="https://vaccines.shinyapps.io/abstractor/", sizes="32x32")),

                      # Open Graph
                      tags$head(tags$meta(property="og:title", content="Abstractor - Open Source Text Summarizer/ML project for Pfizer Document Analysis")),
                      tags$head(tags$meta(property="og:site_name", content="Abstractor")),
                      tags$head(tags$meta(property="og:url", content="https://vaccines.shinyapps.io/abstractor/")),
                      tags$head(tags$meta(property="og:description", content="Search, summarise, analyse and discover information from the Pfizer documents released under FOIA")),
                      tags$head(tags$meta(property="og:type", content="website")),
                      tags$head(tags$meta(property="og:image", content="https://images.unsplash.com/photo-1618961734760-466979ce35b0?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=1032&q=80")),

                      # Default search when 'Enter' is pressed
                      # SEE: https://stackoverflow.com/questions/32335951/using-enter-key-with-action-button-in-r-shiny
                      tags$head(tags$script(HTML(js.code.enter))),

                      # Other pre-load setup                      
                      #shinythemes::themeSelector(),
                      add_busy_bar(color = "DodgerBlue", height = "5px"),

                      sidebarLayout(
                        sidebarPanel(
                          width=3,
                          HTML("<h4>Please <a href='https://forms.gle/f8dW79YUmExrq9Nf8' target='blank'>subscribe here</a> for update information</h4>"),
                          helpText("This tool looks at the Pfizer documents and searches them for keywords.  Results are displayed by document and page number with links to the documents matching the keyword(s)"),
                          #HTML("<p><b>Usage:</b><ol><li>Choose one or more PDFs from the list<li>Enter one or more keywords to find<li>Press the 'Search' button<li>Consider getting a coffee (or <a href='https://ko-fi.com/S6S36J13C' target='blank'>donating</a>)</i></ol>"),
                          titlePanel("Keywords finder"),
                          br(),
                          textInput(inputId = "keywords", 
                                    label = 'Keywords (seperate with commas)',
                                    value = '',
                                    placeholder = 'e.g. antibody,HIV,BNT162b2'
                          ),
                          br(),
                          actionButton(inputId = "keyword_search_btn", label = " Search", icon = icon("search"), style = "width: 100%"),
                          helpText("NOTE: Searches are case sensitive"),
                          br(),
                          h4("Donations"),
                          p("We made this site to be free of advertising and without a paywall. If you found these tools useful, please consider making a small contribution towards the server costs using the link below.  This site runs on coffee! Many thanks in advance."),
                          HTML("Author(s): mk_hostile17 | dagger59<br>"),
                          HTML("Contact: <a href='mailto:pfizerdox@protonmail.com'>pfizerdox@protonmail.com</a>"),
                          br(),
                          br(),
                          HTML("<iframe id='kofiframe' src='https://ko-fi.com/dagger59/?hidefeed=true&widget=true&embed=true&preview=true' style='border:none;width:100%;padding:10px;background:#ffffff;' height='712' title='jasonmorphett'></iframe>"),
                          br(),
                          br(),
                          HTML('<div class="addthis_inline_share_toolbox"></div>'),
                        ),
                        mainPanel(
                          width=9,
                          fluidRow(
                            column(
                              htmlOutput("keywordsOutput"), 
                            width = 10)
                          ),
                          br()
                        )
                      )
             ),

             navbarMenu("More", icon = icon("info-circle"),
                        tabPanel("Get Involved", fluid = TRUE,
                                 fluidRow(
                                   column(12,
                                          h3(p("Calling all R/Shiny developers!")),
                                          p("We are an independent team of analysts, computer scientists and clinicians. The team have analysed, published and made videos on the dangers of SARS-CoV-2 'vaccines' and supported legal efforts against the vaccine regulators."),
                                          p("Abstractor is an Open Source project looking to assist anyone processing the ", a("55,000 pages", href = "https://aaronsiri.substack.com/p/instead-of-fdas-requested-500-pages", target='blank'), " of monthly FOIA data from Pfizer being released by the FDA via the Public Health and Medical Professionals for Transparency (", a("PHMPT", href = "https://phmpt.org/", target='blank'), ") organization.  The first tranche of ", a("documents", href = "https://phmpt.org/pfizers-documents/", target='blank'), " was released on March 1st 2022 and subsequent to that, there will be a further release of 10s of thousands of pages of documents over the coming months."),
                                          p("We would like to reach out to interested developers with skills in R/Shiny and in particular text mining, text summarization, machine learning, AI, text analysis, document processing etc. to join us in extending Abstractor."),
                                          p("Abstractor will become the 'goto' resource for lawyers, the press, independent researchers and anyone interested in finding out what Pfizer submitted in support of it obtaining a EUA in record time."),
                                          br(),
                                          p("If you would like to get involved, please contact us ", a("here", href = "mailto:pfizerdox@protonmail.com?subject=Abstractor: GETTING INVOLVED")),
                                          br(),
                                          h3(p("Getting started")),
                                          br(),
                                          HTML("<ol>"),
                                          HTML("<li><b>Get in contact</b>: Send us an <a href='mailto:pfizerdox@protonmail.com?subject=Abstractor: GETTING INVOLVED'>email</a> with a brief description about yourself and your idea for either a tool"),
                                          HTML("<li><b>We'll get back to you</b>: We'll take a look at your email and invite you to a Zoom call"),
                                          HTML("<li><b>Let's have a chat</b>: Nothing formal, just a 'getting to know each other' type call"),
                                          HTML("<li><b>Download Abstractor</b>: Great! you're on board. Download <a href='#'>Abstractor</a> from GitHub"),
                                          HTML("<li><b>Start coding!</b>: Follow our online tutorial on how to get started in under 5 minutes"),
                                          HTML("</ol>"),
                                          br(),
                                          p("Depending on the complexity of your tool, you could be published and live on Abstractor in a matter of a few days!"),
                                          br(),
                                          hr(),
                                          h3(p("Contact us")),
                                          p("Get in touch: pfizerdox@protonmail.com"),
                                          br()
                                   ))
                        ),
                        tabPanel("About Us", fluid = TRUE,
                                 fluidRow(
                                   column(4,
                                          h3(p("About the Project")),
                                          h5(p("Having previously written", a("CAVE", href = "https://vaccines.shinyapps.io/cave/"), " I already had a server capable of doing data analytics. So knowing that Pfizer were going to be publishing 50,000 pages of FOIA data per month, it was obvious that some tools were needed to parse the output.  Unhelpfully, many of the PDFs are scanned images making them difficult to search, so our initial tool is a keyword finder.")),
                                          h5(p("Other tools are in the pipeline though and we're extending an offer to get involved to other developers with R/Shiny skills")),
                                          h5(p("Contact us: pfizerdox@protonmail.com")),
                                          h3(p("Donate")),
                                          h5(p("We have a small amount of overhead maintaning the server. We can only survive and continue making Abstractor free to use by donations. So if you can spare a couple of bucks, it'll really help us continue this work.")),
                                          HTML("<script type='text/javascript' src='https://storage.ko-fi.com/cdn/widget/Widget_2.js'></script><script type='text/javascript'>kofiwidget2.init('Support This Website on Ko-fi', '#256885', 'S6S36J13C');kofiwidget2.draw();</script>"), 
                                          br(),
                                          br(),
                                          h4(p("Spread The Word")),
                                          h5(p("Please help spread the word by sharing it on your social networks and/or sending it to a friend"),
                                             HTML('<div class="addthis_inline_share_toolbox"></div>')
                                          ),
                                   ),
                                   column(7,
                                          h3(p("About the Team")),
                                          h5(p("We are a small dedicated but global group of independent software developers, analysts, clinicians and mathematicians with a common interest in pursuing the truth. We have appeared on numerous media, written about adverse side effects and work with law firms, select politicans and other 'like-minded' people who share our values."),
                                             p("Oh, and I love the hills (and coffee)"),
                                             
                                          ),
                                          hr(),
                                          HTML('<img src="pen-y-fan.jpg", height="300px"'),
                                          br(),
                                          br(),
                                   )
                                 ),
                                 br(),
                                 hr(),
                                 h5("Credits:"),
                                 h6(
                                   p("Photo by Brano on ",
                                     a("Unsplash",
                                       href = "https://unsplash.com/photos/QSuou3VAtf4"))),
                                 h6(
                                   p("Photo by Nathan Dumlao on ",
                                     a("Unsplash",
                                       href = "https://unsplash.com/photos/Y3AqmbmtLQI"))),
                                 h5("Built with",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                    ".")
                        )
             )
  )
)

##########################################################
# SERVER
server <- function(input, output, session) {
  # Inline functions
  #source('db.R')
  source('search.R')
  
  #shinyalert(title = "Title", text = "Some text", type = "info")

  # Search for keywords from a list of PDFs
  keywordsSearchPDFs <- function() {
    ####################################################      
    # Your code goes here
    #print("keywordsSearchPDFs called")
    
    # Set inputs from UI 
    rVals$keywords_keywords <- input$keywords

    # Remove leading and trailing whitespace and split into vector
    keywords <- rVals$keywords_keywords
    keywords <- unlist(strsplit(keywords, ","))
    keywords <- trimws(keywords)

    # Do we have any keywords?    
    total.matches <- 0
    total.results <- data.frame()
    
    # Check the user hasn't forgotten to enter some keywords      
    if(length(keywords) == 0) {
      showNotification("Please enter keywords", closeButton=FALSE, duration = 5)
    }
    else {
      showNotification("Searching", closeButton=FALSE, duration = 5)

      # Loop through the keywords      
      for(keyword in keywords) {
          result <- searchPfizerDox(PFIZER.CORPUS, PFIZER.SOURCES, keyword, 100)
          total.results <- rbind(total.results, result)
      }
      # This will invalidate the data table and force a redraw
      if(!is.na(total.results[1,])) {
        rVals$keywords_results = total.results
        
        # Save data to MySQL
        #saveData(session$token, rVals$keywords_keywords, total.matches)
      }
      else {
        showNotification("No results", closeButton=FALSE, duration = 5)
        rVals$keywords_results = ""
      }
    }
  }
  

  # Keyword finder tool
  # SEE: https://cran.r-project.org/web/packages/pdfsearch/vignettes/intro_to_pdfsearch.html
  output$keywordsOutput <- renderUI({
      ####################################################      
      # Your code goes here
      #print("keywordsOutput called")
    
      results <- rVals$keywords_results

      # Do we have results?
      if(results != "") {
        # Show the datatable
        # See: https://cran.r-project.org/web/packages/DT/DT.pdf
        DT::datatable(results,
                      class='compact', 
                      #style="jqueryui", 
                      selection = "none",
                      rownames = FALSE, 
                      escape = FALSE, 
                      width = "120%",
                      height = 400,
                      filter = 'bottom', #Position of column filters
                      extensions = 'Buttons', 
                      options = list(ordering=F, 
                                     language = list(search = 'Filter:'),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#2C3E50', 'color': '#fff'});",
                                       "}"
                                     ),
                                     displayLength=10,  #Records to show/page
                                     lengthChange = 1,  #Show/hide records/page
                                     dom = 'Blfrtip',   #Reqiured for buttons
                                     buttons = c('copy', 'csv', 'excel', 'print'),
                                     searchHighlight = TRUE
                      ),
                      colnames = c("Keyword" = "keyword", "Filename" = "filename", "Page" = "page_num", "Source" = "source", "Extract" = "extract")
        )
      }
      else {
        HTML("<hr><i>Results will appear here...</i>")
      }
  })
  
  


  # -------------------------
  # observe event for updating reactiveValues
  observeEvent(input$keyword_search_btn,
  {
    #print("keyword_search_btn called")
    keywordsSearchPDFs()
  })
  

  # -------------------------
  # See: https://riptutorial.com/shiny/example/32342/reactivevalues
  rVals <- reactiveValues(
    keywords_keywords="",                       # Keywords tool
    keywords_results="",                        # Keywords tool
  )
}

# Run the application
shinyApp(ui = ui, server = server)
