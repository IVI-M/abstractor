library(RSelenium)
library(rvest)
library(tidyverse)

# install docker ----
# next, pull the RSelenium image from docker ("standalone-chrome-debug:latest" version)

# run the following line in docker ----
# docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-chrome-debug:latest

# set up the VNCViewer ----
# skipped

# open the VNCViewer ----
# 192.168.99.100::5901
# secret

# setup a RSelenium object ----
remDr <- remoteDriver(remoteServerAddr="192.168.99.100",
                      port=4445L,
                      browser="chrome")
remDr$open() #open a new page in RSelenium Chrome (use the VNC Viewer to view)

# [1] "Connecting to remote server"
# Error in checkError(res) : 
#   Undefined error in httr call. httr output: Timeout was reached: [192.168.99.100:4445] Connection timed out after 10000 milliseconds
# > 

load <- function(your_remDr=remDr){
    your_remDr$navigate("https://health-infobase.canada.ca/covid-19/vaccine-safety/")
    your_remDr$getPageSource()[[1]] %>% # get all source codes (for JS applications too)
        read_html() %>%  # read the html
        html_table() # get the tables
}

load <- function(your_remDr=remDr){
  your_remDr$navigate("https://health-infobase.canada.ca/en/covid-19/vaccine-safety/")
  your_remDr$getPageSource()[[1]] %>% # get all source codes (for JS applications too)
    read_html() %>%  # read the html
    html_table() # get the tables
}

print(load()[[5]]) # the fifth table is the table of interest

# results ----
# > print(load()[[5]]) # the fifth table is the table of interest
# AESI Category                                                                                     AESI Total number of events
# 1                              Auto-immune diseases                                                                 Guillain-Barr? Syndrome1                    123
# 2                              Auto-immune diseases                                                  Thrombocytopenia (low blood platelets)1                    256
# 3                              Auto-immune diseases                                                                                 Subtotal                    379
# 4                             Cardiovascular system                                                                           Cardiac arrest                     42
# 5                             Cardiovascular system                                                                          Cardiac failure                     47
# 6                             Cardiovascular system                                                     Myocardial infarction (heart attack)                    105
# 7                             Cardiovascular system Myocarditis1/Pericarditis (inflammation of the heart muscle and lining around the heart)                  1,886
# 8                             Cardiovascular system                                                                                 Subtotal                  2,051
# 9                                Circulatory system                                                       Cerebral venous (sinus) thrombosis                     20
# 10                               Circulatory system                                                                      Cerebral thrombosis                     10
# 11                               Circulatory system                                                                     Cutaneous vasculitis                     37
# 12                               Circulatory system                                                                     Deep vein thrombosis                    300
# 13                               Circulatory system                                                                                 Embolism                     16
# 14                               Circulatory system                                                                   Haemorrhage (bleeding)                     67
# 15                               Circulatory system                                                                       Pulmonary embolism                    439
# 16                               Circulatory system                                                                  Thrombosis (blood clot)                    299
# 17                               Circulatory system               Thrombosis with thrombocytopenia syndrome1 (blood clot with low platelets)                    108
# 18                               Circulatory system                                                                                 Subtotal                  1,296
# 19         Hepato-gastrointestinal and renal system                                                                      Acute kidney injury                     57
# 20         Hepato-gastrointestinal and renal system        Glomerulonephritis (kidney inflammation) and nephrotic syndrome (kidney disorder)                     18
# 21         Hepato-gastrointestinal and renal system                                                                             Liver injury                     38
# 22         Hepato-gastrointestinal and renal system                                                                                 Subtotal                    113
# 23                Nerves and central nervous system                                                           Bell's Palsy1/facial paralysis                    845
# 24                Nerves and central nervous system                                                        Cerebrovascular accident (stroke)                    217
# 25                Nerves and central nervous system                                       Transverse myelitis (inflammation of spinal cord)2                     13
# 26                Nerves and central nervous system                                                                                 Subtotal                  1,075
# 27                                     Other system                                                                             Anaphylaxis2                    795
# 28                                     Other system                                                                                COVID-193                    370
# 29                                     Other system                                                       Multisystem inflammatory syndrome2                     13
# 30                                     Other system                                                                                 Subtotal                  1,178
# 31                              Pregnancy outcomes4                                                                 Fetal growth restriction                      5
# 32                              Pregnancy outcomes4                                                                     Spontaneous abortion                     73
# 33                              Pregnancy outcomes4                                                                                 Subtotal                     78
# 34                               Respiratory system                                                      Acute respiratory distress syndrome                      5
# 35                               Respiratory system                                                                                 Subtotal                      5
# 36 Skin and mucous membrane, bone and joints system                                                                               Chilblains                     25
# 37 Skin and mucous membrane, bone and joints system                                               Erythema multiforme (immune skin reaction)                     44
# 38 Skin and mucous membrane, bone and joints system                                                                                 Subtotal                     69
# 39                              All AESI categories                                                                                    Total                  6,273




# From help: Examples
## Not run: 
# start the server if one isnt running
startServer()

# use default server initialisation values
remDr <- remoteDriver$new()

# send request to server to initialise session
remDr$open()

# navigate to R home page
remDr$navigate("http://www.r-project.org")

# navigate to www.bbc.co.uk notice the need for http://
remDr$navigate("http://www.bbc.co.uk")

# go backwards and forwards
remDr$goBack()

remDr$goForward()

remDr$goBack()

# Examine the page source
frontPage <- remDr$getPageSource()

# The R homepage contains frames
webElem <- remDr$findElements(value = "//frame")
sapply(webElem, function(x) {
  x$getElementAttribute("name")
})

# The homepage contains 3 frames: logo, contents and banner
# switch to the `contents` frame
webElem <- remDr$findElement(using = "name", value = "contents")
remDr$switchToFrame(webElem$elementId)

# re-examine the page source

contentPage <- remDr$getPageSource()
identical(contentPage, frontPage) # false we hope!!

# Find the link for the search page on R homepage. Use xpath as default.
webElem <- remDr$findElement(value = '//a[@href = "search.html"]')
webElem$getElementAttribute("href")
# http://www.r-project.org/search.html

# click the search link
webElem$clickElement()

# FILL OUT A GOOGLE SEARCH FORM
remDr$navigate("http://www.google.com")

# show different methods of accessing DOM components

webElem1 <- remDr$findElement(using = "name", value = "q")
webElem2 <- remDr$findElement(
  using = "id",
  value = webElem1$getElementAttribute("id")[[1]]
)
webElem3 <- remDr$findElement(
  using = "xpath",
  value = '//input[@name = "q"]'
)

# Enter some text in the search box

webElem1$sendKeysToElement(list("RSelenium was here"))

# clear the text previously entered

webElem1$clearElement()

# show an example of sending a key press
webElem1$sendKeysToElement(list("R", key = "enter"))

# Collate the results for the `R` search
googLinkText <- remDr$findElements(value = "//h3[@class = 'r']")
linkHeading <- sapply(googLinkText, function(x) x$getElementText())
googLinkDesc <- remDr$findElements(value = "//div[@class = 's']")
linkDescription <- sapply(googLinkDesc, function(x) x$getElementText())
googLinkHref <- remDr$findElements(value = "//h3[@class = 'r']/a")
linkHref <- sapply(
  googLinkHref,
  function(x) x$getElementAttribute("href")
)

data.frame(
  heading = linkHeading,
  description = linkDescription, href = linkHref
)

# Example of javascript call
remDr$executeScript("return arguments[0] + arguments[1];", args = 1:2)
# Example of javascript async call
jsscript <-
  "arguments[arguments.length - 1](arguments[0] + arguments[1]);"
remDr$executeAsyncScript(jsscript, args = 1:2)

# EXAMPLE INJECTING INTO PHANTOMJS using phantomExecute
require(RSelenium)
pJS <- wdman::phantomjs(port = 4932L)
remDr <- remoteDriver(browserName = "phantomjs", port = 4932L)
remDr$open(silent = TRUE)
remDr$navigate("http://ariya.github.com/js/random/")
# returns a set of random numbers
remDr$findElement("id", "numbers")$getElementText()[[1]]
#  # now try injecting a new Math,random function
result <- remDr$phantomExecute("var page = this;
                               page.onInitialized = function () {
                               page.evaluate(function () {
                               Math.random = function() {return 42/100}
                               })
                               }", list())
remDr$navigate("http://ariya.github.com/js/random/")
# Math.random returns our custom function
remDr$findElement("id", "numbers")$getElementText()[[1]]
remDr$close()
pJS$stop()

## End(Not run)