#############################################
# Title: Edgar SEC Form 13F Extraction
# Author: Abhinav Krishnan
# Date: 30 Octob er
# Purpose: 
# Inputs: 
# Outputs: 
#############################################

# ================= STEPS ================= 
# 1. scrape stock table from information table
# 2. automate opening information table from filing page
# 3. automate opening filing from company page
# 4. create url list by looping through CIK numbers

# Three levels of webpages:
# Company Search Results Page
  # Filing Detail (click on Filing)
  # Xpath: /html/body/div[4]/div[2]/div/table/tbody/tr[4]/td[3]/a
    # Form 13F Information Table (Click on html information table)
    # Xpath: /html/body/table[2]/tbody

# SETUP

  # clear working environment
  rm(ls = list())
  
  # ============== #
  # Load Libraries
  # ============== #
  
    library(data.table)
    library(ggplot2)
    library(stringr)
    library(foreign)
    library(rvest)      # xml2 is a required package for rvest, so you only need to install rvest
    library(xml2)
    library(httr)
  
  # ============== #
  # Set Parameters
  # ============== #
  
    # set export toggle
    p_export <- F
    
    # set timestamp
    p_timestamp <- Sys.time()
    
    # set output directory
    p_dir_out <- "/Users/abhinavskrishnan/Documents/Projects/SEC-Edgar/Output"
    
    # set input directory
    p_dir_in_base <- "/Users/abhinavskrishnan/Documents/Projects/SEC-Edgar/"
    
    # set SEC base URL
    p_url_base <- "https://www.sec.gov"
    p_url_data <- paste0(p_url_base, "/Archives/edgar/data")

# ============================== STAGE ONE: Infotable ==============================
    
  # ============== #
  # Load Data
  # ============== #
    
    # set infotable url as an object
    # inftbl.url <- "https://www.sec.gov/Archives/edgar/data/314957/000110465920115172/xslForm13F_X01/infotable.xml"
    
    # adding user agent to url header to spoof request
    inftbl.spoof <- GET(inftbl.url, add_headers('user-agent' = 'SEC-13F-Scraper ([[abhinav.s.krishnan@vanderbilt.edu]])'))
    
      # source: https://stackoverflow.com/questions/35690914/web-scraping-the-iis-based-website
    
    # read html using modified url with headers
    inftbl.html <- inftbl.spoof %>% 
      read_html() %>% 
      html_node(xpath = '/html/body/table[2]') %>% # this seems to be the xml path for all tables
      html_table()
    
    # for some reason, specifying .FormData in html_node() yields an empty table. 
    # Using xpath = [xmlpath] works well!
    
  # ============== #
  # Clean Data
  # ============== #
    
    
# ============================== STAGE TWO: Filing Page ==============================

  # set filing url as an object
  # filing.url <- "https://www.sec.gov/Archives/edgar/data/314957/000110465920115172/0001104659-20-115172-index.htm"
  
  # now that we can generate the filing url from the cik number and the ascension numbers, we don't need to manually set it
    
  # adding user agent to url header to spoof request
  filing.spoof <- GET(filing.url, add_headers('user-agent' = 'SEC-13F-Scraper ([[abhinav.s.krishnan@vanderbilt.edu]])'))
 
  # read html using modified url with headers
  filing.html <- filing.spoof %>% 
    read_html() %>% 
    html_elements('a') %>% # here, using the html class works better than trying to use the xpath
    html_attrs()
  
  # this code produces a list of attributes for each html element with tag 'a' on the website
    # the a tag, along with href =, is used to denote hyperlinks
  
  # unlist list of hyperlinks obtained from filing webpage
  filing.links <- data.table(unlist(filing.html))
  
  # filter to hyperlink to Form 13F html infotable
  inftbl.url.partial <- filing.links[grepl("infotable.xml", V1) & grepl("Form13F", V1)]
  # NOTE: Here we're assuming that all infotables are marked as such. we may need to do this by reference instead
  
  # now we want to take the partial url from the datatable and add the base url to that. 
  # lets see if we can do that within the datatable syntax
  
  # append base url to partial url
  inftbl.url <- inftbl.url.partial[, paste0(p_url_base, V1)] # yay this works too!
  
  
  # Now the URL extracted can be appended to the base url to obtain the url to the infotable!!
  
  # I should be able to repeat this process to extract information from the company search page!
  
# ============================== STAGE TWO: Company Search Page ==============================
  
  # set filing url as an object
  search.url <- "https://www.sec.gov/edgar/browse/?CIK=314957"
  
  # adding user agent to url header to spoof request
  search.spoof <- GET(search.url, add_headers('user-agent' = 'SEC-13F-Scraper ([[abhinav.s.krishnan@vanderbilt.edu]])'))
  
  # Create generalized object
  search.html <- search.spoof %>% 
    read_html() 
  
  # read html using modified url with headers
  search.html %>% 
    html_elements('a') %>% # here, using the html class works better than trying to use the xpath
    html_attrs()
  
  # hmm okay so specifying the a tag doesnt return any hyperlinks within the table on the site
  # lets try identifying the table first and then extracting the elements i.e. html_node -> html_elements
  
  # read html using modified url with headers
  search.html %>% 
    html_node() %>% 
    html_elements('a') %>% # here, using the html class works better than trying to use the xpath
    html_attrs()
  
  # okay this isn't working either. I just noticed that there's an rss feed option

  # RSS FEED OPTION
  
  # okay so I could construct the rss feed url using the cik number. There is an R package that will parse
  # rss feeds: tidyrss. Let's try that!
  
  library(tidyRSS)
  
  # set url
  rss.url <- "https://data.sec.gov/rss?cik=314957&count=40"
  
  # spoof agent
  rss.spoof <- GET(rss.url, add_headers('user-agent' = 'SEC-13F-Scraper ([[abhinav.s.krishnan@vanderbilt.edu]])'))
  
  # extract rss feed
  rss.feed <- tidyfeed(feed = rss.url) # okay you can't feed the spoof directly to tidyfeed
  # it should be possible to feed arguments to GET() through the config argument to set the user-agent header
  
  # okay so this isn't returning what I expected - it's just a table with the feed_url and a bunch of other metadata. Let me try the website url instead
  
  # rss.feed <- tidyfeed(feed = rss.url) # okay this doesn't work, you have to use the RSS feed
  
  # I noticed that I can construct the filing url using the "Ascension Number" in the table. If I can extract the table, I should be able to get this data (and more) for each filing entry (which will be useful long term)
  
  # ASCENSION NUMBER METHOD

  search.html %>% 
    html_elements('table') %>% 
    html_table()
  
  # This returns an empty tibble
    
  search.html %>% 
    html_node(xpath = '//*[@id="filingsTable"]') %>% # this seems to be the xml path for all tables
    html_table()
  
  # also returns an empty table when using an xpath for the same element
  
  search.html %>% 
    html_node(xpath = '//*[@id="filingsTable"]/tbody') %>% 
    html_attrs()
  
  # returns NA. ugh let's try RSelenium
  
  
  # RSELENIUM METHOD
  
    # # Load the Library
    # library(RSelenium)
    # 
    # # start the server and browser(you can use other browsers here)
    # rD <- rsDriver(browser=c("firefox"))
    # 
    # driver <- rD[["client"]]
    # 
    # # navigate to an URL
    # driver$navigate("http://books.toscrape.com/")
    # 
    # #close the driver
    # driver$close()
    # 
    # #close the server
    # rD[["server"]]$stop()
  
  # this solution seems to complex because it requires something called Docker - I'd prefer not to get too deep into this

# ============================== STAGE 2.5: Filing Table Extract ==============================  
  # for the purposes of this project, I am okay with downloading each csv of filings manually
  # I can take the ascension numbers from that data table and continue with the other steps of this process!
  
  # WAIT: I'm assuming that all 13F filings title their infotable the same way. This assumption might not hold
    # we'll deal with that in a bit

  # let's manually set the CIK for now:
  cik.uchicago <- "314957"
  
  # create object for filing tables directory
  dir_filing_tables <- paste0(p_dir_in_base, "Filing_Tables/")
  
  # import dataset
  filing_tables <- list.files(dir_filing_tables, full.names = TRUE)
  
  # import first element of filing_tables
  fltbl.uchicago <- as.data.table(read.csv(filing_tables[1])) # yay this works!
  # later, we can automate this process with a for loop
  
  # extract first ascension number
  filing.1 <- fltbl.uchicago[Film.number.s. == "201240648", Accession.number]
  
  # construct complete url to filing page
  
  # we're trying to recreate this: 
  # https://www.sec.gov/Archives/edgar/data/314957/000110465920115172/0001104659-20-115172-index.htm
  # https://www.sec.gov/Archives/edgar/data/314957/000110465920115172/0001104659-20-115172/-index.htm
  
  p_filing_suffix <- "-index.htm"
  
  filing.url <- paste(p_url_data, cik.uchicago, filing.1, p_filing_suffix, sep = "/")
  # https://www.sec.gov/Archives/edgar/data/314957/0001104659-20-115172/-index.htm
  
  # okay we're missing a collapsed ascension number in between the cik number and the hyphenated asccension number. weird right?
  
  # try again
  filing.url <- paste(p_url_data, cik.uchicago, gsub("-", "", x = filing.1), paste0(filing.1, p_filing_suffix), sep = "/")
  
  # https://www.sec.gov/Archives/edgar/data/314957/000110465920115172/0001104659-20-115172/-index.htm
  # okay this looks right! lets run a compare script real quick
  identical(filing.url, "https://www.sec.gov/Archives/edgar/data/314957/000110465920115172/0001104659-20-115172-index.htm")
  
  # TRUE!
  
  # okay lets stop here
  # Okay so I should be able to feed the generate url into the next step
  
# ============================== WORKSPACE ==============================   
    


    
    
    
    # Load keyset
    keyset <- as.data.table(read.csv(paste0(p_dir_in_base, "T-20-SECFilings - Sheet1.csv")))
    
    # Format keyset
      
      # remove unnecessary columns and empty rows
      keyset[University.Name != "" & University.Name != "U.S. News Ranking"]

    # load filelist from edgar
    
      
      
  html.chicago <- read_html("https://www.sec.gov/edgar/browse/?CIK=1728827")
    
  # set base page
  
  base.url <- "https://www.sec.gov/edgar/browse/?CIK="
    
  vec.cik <- keyset[CIK != "" & CIK != "-", str_remove(CIK, "CIK")]
  
  
  url.list <- list(names = vec.cik)
  
  # Loop through vector of CIK numbers
  for (cik in vec.cik) {
    
    cik.url <- paste0(base.url, cik)
    
    url.list[[cik]] <- cik.url
    
  }

  # Workspace with Chicago html page
  

  
  
  html.form13f.chicago <- "https://www.sec.gov/Archives/edgar/data/314957/000110465920115172/0001104659-20-115172-index.htm"
  
  
  
    x.table.chicago <- '//*[@id="filingsTable_wrapper"]/div[3]'
    
    tbl.chicago <- html.chicago %>% 
      html_node(xpath = x.table.chicago) %>% 
      html_table() %>%
      as_tibble() 
    
    tbl.chicago %>%
      mutate(
        
      ) %>% 
      add_column(
        "Listings" = NA
      ) %>% 
      select(
        -c("Location", "Technology", "Link")
      )
  
  
    
    