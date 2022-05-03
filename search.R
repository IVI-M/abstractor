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

