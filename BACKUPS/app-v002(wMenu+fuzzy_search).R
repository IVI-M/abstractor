############################# #
# LIBRARIES ----

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
library(data.table) # replaced CNTR+F data.frame with data.table

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
# library(tesseract)

# Scraping webpages
library(tidyverse)
library(rvest)

# String manipulation
library(stringr)
library(stringi)


############################# #
# DATA


############################# #
# FUNCTIONS ----

# Cleaning up strings for regex and escaping reserved characters
# SEE: https://stackoverflow.com/questions/14836754/is-there-an-r-function-to-escape-a-string-for-regex-characters
quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}


############################################### #
# CODE
# CSS for the UI ----
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

############################# #
# ui <- fluidPage( ----
ui <- fluidPage(
  #Navbar structure for UI
  navbarPage("Abstractor: STAGING SERVER", theme = shinytheme("flatly"),
             tabPanel("Keywords", fluid = TRUE, icon = icon("file-alt"),
                      tags$style(button_color_css),
                      tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
                      tags$script(type="text/javascript", src="//s7.addthis.com/js/300/addthis_widget.js#pubid=ra-55a2c0ea679e0df1"),
  
                      # Google Analytics
                      # tags$head(includeHTML(("google-analytics.html"))),

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

                      # Other pre-load setup                      
                      #shinythemes::themeSelector(),
                      add_busy_bar(color = "DodgerBlue", height = "5px"),

                      sidebarLayout(
                        sidebarPanel(
                          width=4,
                          HTML("<h4>Please <a href='https://forms.gle/f8dW79YUmExrq9Nf8' target='blank'>subscribe here</a> for update information</h4>"),
                          helpText("This tool looks at pdfs and searches them for keywords.  Results are displayed by document and page number with links to the documents matching the keyword(s)"),
                          HTML("<p><b>Usage:</b><ol><li>Choose one or more PDFs from the list<li>Enter one or more keywords to find<li>Press the 'Search' button<li>Consider getting a coffee (or <a href='https://ko-fi.com/S6S36J13C' target='blank'>donating</a>)</i></ol>"),
                          titlePanel("Keywords finder"),
                          #textInput(inputId = "pdfURL", 
                          #          label = 'Location of PDFs',
                          #          value = 'https://phmpt.org/pfizers-documents/'
                          #          #value = 'C:\\Enigma\\Pfizer'
                          #),
                          selectInput("pdfURL", label = ("Location of PDFs"), 
                                      choices = list("https://phmpt.org/" = "https://phmpt.org/pfizers-documents/",
                                                     "www.IVIM.ca/data/phac1" = "https://IVIM.ca/data/phac1",
                                                     "www.IVIM.ca/data/uk1" = "https://IVIM.ca/data/uk1",
                                                     "https://www.icandecide.org/" = "https://www.icandecide.org/pfizer-documents/"
                                                     ), 
                                      selected = 1),                          
                          #actionButton(inputId = "KeywordScanBtn", label = " Scan", icon = icon("search"), style = "width: 100%"),
                          p(),
                          multiInput(
                            inputId = "pdf_files", 
                            label = "PDFs (0)",
                            choices = c("Files will appear here"),
                            width = "100%",
                            options = list(
                              enable_search = TRUE,
                              non_selected_header = "Choose from:",
                              selected_header = "You have selected:"
                            )                            
                          ),
                          checkboxInput("select_all_pdfs", "Select all", FALSE),
                          # checkboxInput("use_wildcards", "Use wildcards (advanced)", FALSE),
                          
                          
                          # pickerInput(
                          #  inputId = "adv_search_options",
                          #  label = "Advanced options (not yet implemented)",
                          #  choices = c("Apply phonetic matching", "Apply probabilistic (fuzzy) matching", "Use synonyms", "Extract tables"),
                          # selected = c("Fuzzy matching", "Use synonyms"),
                          # # choices = list(
                          # #  Group1 = c("Option 1", "Option 2", "Option 3", "Option 4"),
                          # #  Group2 = c("Option A", "Option B", "Option C", "Option D")
                          # # ),
                          #  multiple = TRUE,
                          #  options = list(
                          # `selected-text-format`= "count",
                          # `count-selected-text` = "{0} of {1} options",
                          #    `actions-box` = TRUE
                          #  )
                          # ),
                          
                          
                          
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
                          width=8,
                          fluidRow(
                            column(
                              htmlOutput("keywordsOutput"), 
                            width = 10)
                          ),
                          br()
                        )
                      )
             ),

            # tabPanel("Summarizer", fluid = TRUE, icon = icon("vote-yea"),
            #          sidebarLayout(
            #            sidebarPanel(
            #              width=4,
            #              helpText("This tool summarizes a document. It takes a PDF and chooses what it considers to be the most important 3 sentences and presents them in the order they appear in the document. NOTE: There is a server limit of 3 PDFs per query"),
            #              titlePanel("Summarizer"),
            #              textInput(inputId = "pdfURL2", 
            #                        label = 'Location of PDFs',
            #                        value = 'https://phmpt.org/wp-content/uploads/2021/11/5.3.6-postmarketing-experience.pdf'
            #              ),
            #              actionButton(inputId = "KeywordScanBtn2", label = " Scan", icon = icon("search"), style = "width: 100%"),
            #              p(),
            #              multiInput(
            #                inputId = "pdf_files2", 
            #                label = "PDFs (0)",
            #                choices = c("Files will appear here"),
            #                width = "100%",
            #                options = list(
            #                  #limit = 3,
            #                  enable_search = TRUE,
            #                  non_selected_header = "Choose from:",
            #                  selected_header = "You have selected:"
            #                )                            
            #              ),
            #              br(),
            #              actionButton(inputId = "SummarizeBtn", label = " Summarize", icon = icon("search"), style = "width: 100%"),
            #              br(),
            #              br(),
            #              HTML("Author(s): dagger59"),
            #              br(),
            #              HTML("Contact: <a href='mailto:pfizerdox@protonmail.com'>pfizerdox@protonmail.com</a>"),
            #              p(),
            #              br(),
            #              h4("Donations"),
            #              p("We made this site to be free of advertising and without a paywall. If you found these tools useful, please consider making a small contribution towards the server costs using the link below.  This site runs on coffee! Many thanks in advance."),
            #              br(),
            #              HTML("<iframe id='kofiframe' src='https://ko-fi.com/dagger59/?hidefeed=true&widget=true&embed=true&preview=true' style='border:none;width:100%;padding:10px;background:#ffffff;' height='712' title='jasonmorphett'></iframe>"),
            #              br(),
            #              br(),
            #              HTML('<div class="addthis_inline_share_toolbox"></div>'),
            #            ),
            #            mainPanel(
            #              width=8,
            #              htmlOutput("summarizerOutput"),
            #              br()
            #            )
            #          )
            # ),
             
            # tabPanel("Next Tool", fluid = TRUE, icon = icon("glasses"),
            #          fluidRow(
            #            div(class="centreDiv", 
            #                p("This is a blank panel where additional tools can be added"),
            #                p("People/Teams can contribute to a tool"),
            #                h2("Project Enigma")
            #            )
            #          )             
            # ),

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
                      #  tabPanel("Sources and Data", fluid = TRUE,
                      #           fluidRow(
                      #             column(6,
                      #                    h3(p("VAERS")),
                      #                    h5(p("CAVE uses data from the Vaccine Adverse Events Reporting System (", a("VAERS", href = "https://vaers.hhs.gov/"), "). VAERS is a database used to record and detect possible safety issues with vaccines. Data for adverse events from vaccines date back to 1990 and data is made publicly available to medical practitioners and the general public alike."),
                      #                       p("VAERS data can be entered by medical personnel and individuals and contains reports of their experiences."),
                      #                       p("The Center for Disease Control (", a("CDC", href = "https://www.cdc.gov/"), ") and Food and Drug Administration (", a("FDA", href = "https://www.fda.gov/"), ") jointly administer VAERS and respond to reports."),
                      #                       p("The data is comprehensive and provided in a way that can be analysed by the public, though skills in data handling are required to process it.  However, the CDC does provide a GUI 'like' tool called ", a("CDC WONDER", href = "https://wonder.cdc.gov/vaers.html"), " which is available to the public."),
                      #                       p("CAVE is kept up to date by periodically collecting the latest 500,000 adverse event records from VAERS and removes some fields to fit within the limits imposed by the website hosting company. A description of the fields can be made available by contacting me at uea15577@gmail.com."),
                      #                       br(),
                      #                       img(src = "vaers-logo.png", height = "40px"),
                      #                       br(),
                      #                       br()
                      #                    )
                      #             ),
                      #             column(6,
                      #                    h3(p("Other Sources")),
                      #                    h5(p("The following sources are used for additional data"),
                      #                       HTML("<ul><li>Population statistics - <a href='https://www.census.gov/topics/population.html'>US Census Department</a></li><li>COVID cases (US) - <a href='https://covid.cdc.gov/covid-data-tracker/#datatracker-home'>CDC Covid Data Tracker</a></li><li>Medical definitions - <a href='https://encyclopedia.thefreedictionary.com/'>The Free Dictionary</a></li></ul>")
                      #                    ),
                      #                    br(),
                      #                    h3(p("Adverse Event Reporting")),
                      #                    h5(p("Use following links to report any adverse events"),
                      #                       HTML("<ul><li>In the US - <a href='https://vaers.hhs.gov/reportevent.html'>VAERS</a></li><li>In the UK - <a href='https://coronavirus-yellowcard.mhra.gov.uk/'>Yellow Card</a></li></ul>")
                      #                    ),
                      #                    br()
                      #             ))
                      #           
                      #  ),
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

######################################################## ##
# SERVER
server <- function(input, output, session) {
  # Inline functions
  # source('db.R') ----
  
  #shinyalert(title = "Title", text = "Some text", type = "info")

  # Search for keywords from a list of PDFs
  keywordsSearchPDFs <- function() {
    ################################################### #      
    # Your code goes here
    #print("keywordsSearchPDFs called")
    
    # Set inputs from UI 
    rVals$keywords_keywords <- input$keywords
    rVals$keywords_pdfs <- input$pdf_files

    # Remove leading and trailing whitespace and split into vector
    keywords <- rVals$keywords_keywords
    keywords <- unlist(strsplit(keywords, ","))
    keywords <- trimws(keywords)
    #use_wildcards <- input$use_wildcards

    # Do we have any keywords?    
    total.matches <- 0
    total.results <- NULL
    
    # Check the user hasn't forgotten to enter some keywords      
    if(length(keywords) == 0) {
      showNotification("Please enter keywords", closeButton=FALSE, duration = 5)
    }
    if(length(keywords) > 0) {
  
      # Get the selected PDFs 
      selected.pdfs <- rVals$keywords_pdfs
      selected.urls <- filter(rVals$keywords_data, name %in% selected.pdfs)
      
      # Loop through each PDF
      n <- nrow(selected.urls)
      m <- 1

      # Check the user hasn't forgotten to select a PDF      
      if(n == 0) {
        showNotification("Please select a PDF", closeButton=FALSE, duration = 5)
      }
      for (pdfURL in selected.urls$url){ 
        matches <- 0
        results <- NULL
        
        msg <- paste0("Processing item ", m, " of ", n, " [", basename(pdfURL), "]")
        showNotification(msg, closeButton=FALSE, duration = 5)
        
        # Get PDF text
        pdf <- pdf_text(pdfURL) %>% strsplit(split = "\n")   
        
        # Search by page
        if(length(pdf) > 0) {
          for(page in 1:length(pdf)) {
            # Search by line
            if(length(pdf[[page]]) > 0) {
              for(line in 1:length(pdf[[page]])) {
                #print(paste0(page, ":", line))
                # Search by keyword (e.g. BNT162b2)
                # And collapse whitespace into ' '
                for(keyword in keywords) {
                  text <- gsub("\\s+", " ", str_trim(pdf[[page]][line]))
                  
                  # Which type of search are we doing?
                  #search.params <- getKeywordSearchType(keyword)
                  #keyword <- search.params$term
                  
                  ###################### #
                  # [1] Wildcard search ----
                  ###################### #
                  if(use_wildcards) {
                  #  glob.keyword <- keyword
                  #  first.char <- substring(glob.keyword, 1, 1)
                  #  # Add a '*' to the beginning of the string if it isn't there
                  #  if((first.char != '*') | (first.char != '^')) { 
                  #    glob.keyword <- paste0('*', glob.keyword)
                  #  }
                  #  grx <- glob2rx(glob.keyword)
                  #  if(grepl(pattern=grx, x=text)) {
                  #    
                  #    # Create link
                  #    pdf_link <- paste0("<a href=", pdfURL, "#page=", page, " target='blank'>", basename(pdfURL), "</a>")
                  #    result <- data.table(pdf.link=pdf_link,keyword=keyword,page.num=page,line.num=line,line=text)
                  #    results <- rbind(results, result)
                  #    
                  #    # Update matches
                  #    matches <- matches+1
                  #  }
                  }
                  ###################### #
                  # [2] Basic search ----
                  ###################### #
                  if (T) {
                    match <- str_locate(text, keyword)
                    if(!is.na(match[1,]["start"])) {
                      # Create link
                      pdf_link <- paste0("<a href=", pdfURL, "#page=", page, " target='blank'>", basename(pdfURL), "</a>")
                      result <- data.table(pdf.link=pdf_link,keyword=keyword,page.num=page,line.num=line,line=text)
                      results <- rbind(results, result)
                      
                      # Update matches
                      matches <- matches+1
                    }
                  }
                  # [3] Dmitry: Advanced search (Dmitry) ----
                  if (F) {
                    
                    
                    
                  }
                }
              }
            }
          }
          msg <- paste0("Found ", matches, " matches")
          showNotification(msg, closeButton=FALSE, duration = 5)
          total.matches <- total.matches+matches
          total.results <- rbind(total.results, results)
          m <- m+1
        }
      }
    }
    # This will invalidate the data table and force a redraw
    if(total.matches > 0) {
      rVals$keywords_results = total.results
      
      # Save data to MySQL
      saveData(session$token, rVals$keywords_keywords, total.matches)
    }
    else {
      rVals$keywords_results = ""
    }
  }
  
  # Get a list of PDFs from a given URL
  keywordsGetPDFs <- function() {
      ################################################### #      
      # Your code goes here
      #print("keywordsGetPDFs called")
  
      # Clear the 'Select All' option
      updateCheckboxInput(
        session = session,
        inputId = "select_all_pdfs",
        value = FALSE
      )
      # Clear the control
      updateMultiInput(
        session = session,
        inputId = "pdf_files",
        choices = c("Files will appear here"),
        label = paste("PDFs (0)")
      )        

      # Set inputs from UI 
      rVals$keywords_url <- input$pdfURL
      pdfURL <- rVals$keywords_url
      
      # Notify
      showNotification("Working", closeButton=FALSE, duration = 5)
        
      ## Get PDF list from website
      # SEE: https://cran.r-project.org/web/packages/pdfsearch/vignettes/intro_to_pdfsearch.html
      # AD: https://towardsdatascience.com/scraping-downloading-and-storing-pdfs-in-r-367a0a6d9199
      path <- pdfURL
      page <- read_html(path)
      
      # Parse out PDFs        
      pdf_list <- page %>% # takes the page above for which we've read the html
        html_nodes("a") %>%  # find all links in the page
        html_attr("href") %>% # get the url for these links
        str_subset("\\.pdf") # find those that end in pdf only
      
      # Put them in a data frame
      pdf_urls <- NULL
      pdf_names <- NULL
      for (url in pdf_list){ 
        # Correct for urls
        if(url == basename(url)) {
          pdf_urls <- c(pdf_urls, paste0(path, url))
        }
        else {
          pdf_urls <- c(pdf_urls, url)
        }
        pdf_names <- c(pdf_names, basename(url))
      }
      rVals$keywords_data <- data.table(url=pdf_urls, name=pdf_names)
      
      # Populate the multiInput
      updateMultiInput(
        session = session,
        inputId = "pdf_files",
        choices = pdf_names,
        label = paste0("PDFs (", nrow(rVals$keywords_data), ")")
      )
  }
    
  # Keyword finder tool ----
  # SEE: https://cran.r-project.org/web/packages/pdfsearch/vignettes/intro_to_pdfsearch.html
  output$keywordsOutput <- renderUI({
      # output$keywordsOutput <- renderUI ##################################################      
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
                      colnames = c("Filename" = "pdf.link", "Keyword" = "keyword", "Page" = "page.num", "Line" = "line.num", "Text" = "line")
        )
      }
      else {
        HTML("<p><b>Notes:</b><ul><li>Some PDFs are image files<li>We are currently working on an OCR tool to read these<li>Processing lots of files can take time<li>The blue bar indicates the tool is working</ul><p><hr><i>Results will appear here...</i>")
        #HTML("<p><b>Notes:</b><ul><li>Some PDFs are image files<li>We are currently working on an OCR tool to read these<li>Processing lots of files can take time<li>The blue bar indicates the tool is working</ul><br>",
        #     "<p><b>Wildcards:</b><p>Wildcards are available See the following for examples on how to use them:<ul><li>blue* - Match anything with 'blue' in (e.g. 'blue', 'blues')<li>blue|Blue* - Match anything with 'blue' OR 'Blue' in (e.g. 'blue', blues, 'Blue')<li>* blue * - Match ONLY with 'blue' in (e.g. 'blue' not 'blues')<li>blue?? * - Match anything with 'blue' and two characters (e.g. 'blue12')<li>* blue/? * - Match ONLY 'blue' followed by a slash and a character<li>* blue/* * - Match ONLY 'blue' followed by a slash and ANY characters<li>^But * - Match ONLY where 'But' is the first word</ul><p><hr><i>Results will appear here...</i>")
      }
  })
  
  
  # Keyword finder tool ----
  # SEE: https://adamspannbauer.github.io/2017/12/17/summarizing-web-articles-with-r/
  output$summarizerOutput <- renderUI({
    #  output$summarizerOutput <- renderUI ###################################################      
    # Your code goes here
    #print("summarizerOutput called")
    pdfURL <- rVals$summarizer_pdf
    str1 <- "<h4>Full Text (page 1)</h4>"
    str2 <- "--------------------<br>"
    str3 <- "<h4>Summary Text (whole document)</h4>"

    # Do we have a URL?
    if(nchar(pdfURL) > 1) {
      showNotification("Working", closeButton=FALSE, duration = 5)
      
      # Get PDF text
      pdf <- pdf_text(pdfURL)    

      # Summarise PDF
      showNotification("Summarizing PDF", closeButton=FALSE, duration = 5)
      top_3 = lexRankr::lexRank(pdf,
                                #only 1 article; repeat same docid for all of input vector
                                docId = rep(1, length(pdf)),
                                #return 3 sentences to mimick /u/autotldr's output
                                n = 3,
                                continuous = TRUE)
      
      # Reorder the top 3 sentences to be in order of appearance in article
      order_of_appearance = order(as.integer(gsub("_","",top_3$sentenceId)))
      
      # Extract sentences in order of appearance
      ordered_top_3 = top_3[order_of_appearance, "sentence"]
      
      HTML(paste(str1, pdf[1], str2, str3, "<h4>#1</h4>", ordered_top_3[1], "<br><h4>#2</h4>", ordered_top_3[2], "<br><h4>#3</h4>", ordered_top_3[3], sep = '<br/>'))
    }
    else {
      HTML("<p><b>Usage:</b><ol><li>Choose a Location to search for PDFs<li>Press the 'Scan' button to get a list of pdf filenames<li>Choose <b>no more than 3 PDFs</b> from the list<li>Press the 'Summarize' button<li>Consider getting a coffee (or <a href='https://ko-fi.com/S6S36J13C' target='blank'>donating</a>)<li>Results will be displayed in a table</ol><br><b>Notes:</b><ul><li>Some PDFs are image files<li>We are currently working on an OCR tool to read these<li>Processing lots of files can take time<li>The blue bar indicates the tool is working</ul><p><hr><i>Results will appear here...</i>")
    }
  })
  
  # ------------------------ -
  # observe event for updating reactiveValues -----
  observeEvent(input$SummarizeBtn,
  {
    # Set inputs from UI 
    rVals$summarizer_pdf <- input$pdfURL2
  })
  
  # ------------------------ -
  # observe event for updating reactiveValues ----
  #observeEvent(input$KeywordScanBtn,
  #{
  #  #print("KeywordScanBtn called")
  #  keywordsGetPDFs()
  #  rVals$keywords_results=""
  #})
  
  # ------------------------ -
  # observe event for pdfURL (Keyword Finder) ----
  observeEvent(input$pdfURL,
  { 
    #print("pdfURL called")
    keywordsGetPDFs()
    rVals$keywords_results=""
  })

  # ----------------------- --
  # observe event for updating reactiveValues -----
  observeEvent(input$KeywordScanBtn2,
  {
    #print("KeywordScanBtn2 called")
  })

  # ----------------------- --
  # observe event for updating reactiveValues ----
  observeEvent(input$keyword_search_btn,
  {
    #print("keyword_search_btn called")
    keywordsSearchPDFs()
  })
  
  # ------------------------ -
  # observe event for updating reactiveValues ----
  observeEvent(input$select_all_pdfs,
  {
    #print(paste("select_all_pdfs called: ", input$select_all_pdfs))
    
    rVals$keywords_select_all <- input$select_all_pdfs
    select_all_pdfs <- rVals$keywords_select_all
    
    # Set the selection
    select <- "Files will appear here"
    choice <- "Files will appear here"
    label <- "PDFs (0)"

    if(nrow(rVals$keywords_data) > 0) { 
      choice <- rVals$keywords_data$name
      select <- rVals$keywords_data$name
      label  <- paste0("PDFs (", nrow(rVals$keywords_data), ")")
    }

    # Select everything
    if(select_all_pdfs == TRUE) {
      updateMultiInput(
        session = session,
        inputId = "pdf_files",
        choices = choice,
        selected = select,
        label = label
      )        
    }
    else {
      # Select nothing
      updateMultiInput(
        session = session,
        inputId = "pdf_files",
        choices = choice,
        label = label
      )        
    }    
  })

  # ----------------------- --
  # See: https://riptutorial.com/shiny/example/32342/reactivevalues -----
  rVals <- reactiveValues(
    summarizer_pdf="",                          # Summarizer tool
    keywords_url="",                            # Keywords tool
    keywords_pdfs="",                           # Keywords tool
    keywords_data=data.table(url=character(),   # Keywords tool
                             name=character()), # Keywords tool
    keywords_keywords="",                       # Keywords tool
    keywords_results="",                        # Keywords tool
    keywords_use_ocr=FALSE,                     # Keywords tool
    keywords_select_all=FALSE,                  # Keywords tool
  )
}

# Run the application
options(shiny.host='127.0.0.1')
options(shiny.port= 2022 )
shinyApp(ui = ui, server = server)

