##############################
# LIBRARIES

# String manipulation
library(stringr)
library(stringi)

# DPLYR
library(dplyr)


##############################
# DATA


##############################
# FUNCTIONS
# Function to search corpus and return formatted results
searchPfizerDox <- function(corp.us, sourc.es, srch.term, off.set=50) {
  browser()
  
  df1 <- corp.us %>% filter(grepl(srch.term, pdf_text)) 
  df2 <- sourc.es
  #View(df)
  
  # Get indices of keyword location
  if(nrow(df1)>0) {
    # Reformat the filename
    df1$filename <- paste0(tools::file_path_sans_ext(df1$filename), ".pdf")  
    df1$filename <- sub(".-", "", df1$filename)  
    
    # Format the source link
    df <- inner_join(df1, df2, by = "filename") %>%
      select("filename",
             "link" = "pdf_text.y",
             "pdf_text" = "pdf_text.x",
             "page_num" = "page_num.x"
      )
    df$source <- paste0("<a href=", df$link, "#page=", df$page_num, " target='blank'>link</a>")  
    
    # Format (highlight) the keyword by finding where it is
    df$start <- str_locate(df$pdf_text, srch.term)[,"start"]
    df$end <- str_locate(df$pdf_text, srch.term)[,"end"]
    
    # Set indices to be +/- offset around the word
    df$start <- ifelse(df$start-off.set < 0, 0, df$start-off.set)
    df$end <- ifelse(df$end+off.set > nchar(df$pdf_text), nchar(df$pdf_text), df$end+off.set)
    
    # Set keyword
    df$keyword <- srch.term
    
    # Set keyword extract
    df$extract <- substr(df$pdf_text, df$start, df$end)
    df$extract <- str_replace_all(df$extract, srch.term, paste0("<font color='red'>", srch.term, "</font>"))
    
    # Tidy the string by matching everything from the first to last space
    # SEE: https://stackoverflow.com/questions/49541652/r-regex-match-string-between-first-and-last-space
    df$extract <- sub(".*?\\s+(.*)\\s.*", "\\1", df$extract, perl=TRUE)
    
    # Subset the results
    df <- df  %>%
      select("keyword", "filename", "page_num", "extract", "source") 
    
    return(df)
  }
  else {
    return(NA)
  }
}

if (F) { # test: dmitry ----
  
  
  
  # PFIZER.CORPUS <- readr::read_csv("corpus-staging.csv", quote = "\"'", show_col_types = FALSE)
  # Error in readr::read_csv("corpus-staging.csv", quote = "\"'", show_col_types = FALSE) : 
  #   unused argument (show_col_types = FALSE)

  
  PFIZER.CORPUS <- read_csv("corpus-staging.csv") 
  View(PFIZER.CORPUS)
  
  
  # dtPFIZER.CORPUS <- fread("corpus-staging.csv")
  dtPFIZER.CORPUS <- PFIZER.CORPUS %>% as.data.table()
  dtPFIZER.CORPUS[1]$pdf_text

  
  
  PFIZER.SOURCES <- PFIZER.CORPUS  %>%
    group_by(filename) %>%
    filter(page_num == max(as.numeric(page_num))) 
  PFIZER.SOURCES[1,]
  
  # Reformat the filename
  PFIZER.SOURCES$filename <- paste0(tools::file_path_sans_ext(PFIZER.SOURCES$filename), ".pdf")  
  PFIZER.SOURCES$filename <- sub(".-", "", PFIZER.SOURCES$filename)  
  
  keyword="female"
  keyword="fimale" # <- test with this
  
  keyword="h.i.v."
  keyword="hiv"
  
  result <- searchPfizerDox(PFIZER.CORPUS, PFIZER.SOURCES, keyword, 100)
  
  
  df1 <- PFIZER.CORPUS %>% filter(grepl(keyword, pdf_text)) 
  df1
  df1$pdf_text[1]
  
  # %ilike% innstead of grepl ----
  
  dt1 <- dtPFIZER.CORPUS[grepl(keyword, pdf_text)];dt1
  
  
  dt1 <- dtPFIZER.CORPUS[
    pdf_text %ilike% keyword # much faster
  ];dt1
  
  dt1 <- dtPFIZER.CORPUS[
    pdf_text %ilike% paste0("(?i)", keyword) # case-insensitive
  ];dt1
  

  
  
  # agrep {base}  ----
  
  max.distance=0.1
  
  
  dt1 <- dtPFIZER.CORPUS[
    agrep(keyword, pdf_text, max.distance = 0.1, costs = NULL,
                               ignore.case = T, value = FALSE, fixed = TRUE,
                               useBytes = FALSE)
    ];dt1
  dt1 <- dtPFIZER.CORPUS[
    agrep(keyword, pdf_text, max.distance = 0.2, costs = NULL,
          ignore.case = T, value = FALSE, fixed = TRUE,
          useBytes = FALSE)
  ];dt1 
  

  # #  phonics Does not work with long texts-----
  # 
  # library("phonics")
  # 
  # dt1 <- dtPFIZER.CORPUS[
  #   mra_compare( mra_encode(keyword), mra_encode(pdf_text) )
  # ];dt1
  
  
  # stringdist ----
  
  
  library(stringdist)
  
  # The Optimal String Alignment distance (method=‘osa’) is like the Levenshtein distance but also allows transposition of adjacent characters. Here, each substring may be edited only once. (For example, a character cannot be transposed twice to move it forward in the string).
  
  dt1 <- dtPFIZER.CORPUS[
    grabl(pdf_text, keyword, method="osa",  maxDist=1) 
  ];dt1; dt1 %>% nrow
  dt1 <- dtPFIZER.CORPUS[
    grabl(pdf_text, keyword,  method="osa", maxDist=2) 
  ];dt1; dt1 %>% nrow
  
  

  dt1 <- dtPFIZER.CORPUS[
    grabl(pdf_text, keyword, method="qgram",  maxDist=5, q=2) 
  ];dt1; dt1 %>% nrow
  dt1 <- dtPFIZER.CORPUS[
    grabl(pdf_text, keyword,  method="qgram", maxDist=2, q=2) 
  ];dt1; dt1 %>% nrow
  dt1 <- dtPFIZER.CORPUS[
    grabl(pdf_text, keyword, method="qgram",  maxDist=5, q=1) 
  ];dt1; dt1 %>% nrow
  dt1 <- dtPFIZER.CORPUS[
    grabl(pdf_text, keyword, method="qgram",  maxDist=5, q=3) 
  ];dt1; dt1 %>% nrow
  
  
  dt1 <- dtPFIZER.CORPUS[
    grabl(pdf_text, keyword, method="soundex", maxDist=1) 
  ];dt1; dt1 %>% nrow
  dt1 <- dtPFIZER.CORPUS[
    grabl(pdf_text, keyword, method="soundex", maxDist=2) 
  ];dt1; dt1 %>% nrow
  

}

