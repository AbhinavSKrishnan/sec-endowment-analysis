#############################################
# Title: Edgar SEC Form 13F Extraction
# Author: Abhinav Krishnan
# Date: 30 October
# Purpose: Scrape and clean endowment investment data from SEC's Edgar Database
# Inputs: Entity Landing Page Filing Tables, CIK numbers
# Outputs: 
#############################################

# =============================================================================
# ============================= Description ===================================
# =============================================================================

    

# =============================================================================
# ================================ Status =====================================
# =============================================================================

  # 11 November - Completed csv entry and reading; produced urls to 


# =============================================================================
# =============================== Notes ====================================
# =============================================================================

  # WAIT: I'm assuming that all 13F filings title their infotable the same way. This assumption might not hold
  # we'll deal with that in a bit

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

# =============================================================================
# =============================== Setup =====================================
# =============================================================================

  # clear working environment
    rm(list = ls())
  
  # ============== #
  # Load Libraries
  # ============== #
  
    library(data.table)
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

# =============================================================================
# ========================= Filing Table Processing ==============================
# =============================================================================

    # Unfortunately, I was unable to extract the filing tables from the entity pages automatically, so I've downloaded a
    # csv version of each filing table manually.
    # In the future, this step could be automated by using RSelenium or revisiting the html code
    
    # I can take the ascension numbers from that data table and continue with the other steps of this process!
    
    # Setup #
    
      # Create filing tables directory string
      dir_filing_tables <- paste0(p_dir_in_base, "Filing_Tables")
      
      # Create vector of filing table csvs in directory
      filing_tables <- as.vector(list.files(dir_filing_tables, full.names = FALSE))
      # NVM dont want to do this now
        # OKAY so I set full.names to FALSE because I want to string split the names so I can identify each university
        # I can generate full names using dir_filing_tables later
  
    
    # For Loop #
    
      # Create empty datatable to add to recursively
      dt_fltbls <- data.table()
      
      # Create empty datatable to add cik and entity names to
      dt_metadata <- data.table(current_cik = character(), current_entity_name = character())
    
      # create for loop to read in each filing table  
      for(i in 1:length(filing_tables)){
        
        # Extract name of ith csv
        current_filename <- as.vector(filing_tables[i])
        
        # Split current_filename into parts
        filename_parts <- as.vector(str_split(current_filename, "_"))
          
          # isolate each part as a separate vector
          current_cik <- str_replace(filename_parts[[1]][2], "CIK", "")
          current_entity_name <- str_replace(filename_parts[[1]][3], ".csv", "")
          
          # create vector of metadata
          current_metadata <- data.table(current_cik, current_entity_name)
          
          # add current metadata to dt_metadata
          dt_metadata <- rbind(dt_metadata, current_metadata)
        
        # create full.name filepath for ith filing table
        current_filepath <- paste0(dir_filing_tables, "/", current_filename)
        
        # Read csv for current filing table + assign to datatable object
        current_fltbl <- as.data.table(read.csv(current_filepath))
        
        # Add metadata to current_fltbl
        current_fltbl[, c("Entity.Name", "CIK.Number") := .(current_entity_name, current_cik)]
        
        # bind current_fltbl to existing dt_fltbls
        dt_fltbls <- rbind(dt_fltbls, current_fltbl)
        
      }
    
    # Post-Processing #
    
      # now we can extract ascension numbers and create urls in the same table
      # since we've created a datatable, we can avoid using a for loop, just need to referentially create a new column
      
      # create parameter for suffix
      p_filing_suffix <- "-index.htm"
      
      # filter datatable to create filing URL and filter to just 13F reports
      dt_13f <- dt_fltbls[, Filing.URL := paste(p_url_data, CIK.Number, gsub("-", "", x = Accession.number), paste0(Accession.number, p_filing_suffix), sep = "/")][Form.type %like% "13F-HR"]
    
    # OKAY YAY THIS WORKS!!

# =============================================================================
# ============================ Infotable Link  =================================
# =============================================================================
      
  # Now we need to obtain the URL for the information table. I noticed that 
    
  for (filing.url in dt_13f[, Filing.URL]) {
    
    # Set if condition for NA filing URLs
      # Question: why are there NA filing urls? Need to figure out this problem.
      
    if (is.na(filing.url)) {
      
      # Add inftbl url to dt_13f
      dt_13f[Filing.URL == filing.url, Infotable.URL := NA]
      
    } else {
    
      # Spoof user agent
      filing.spoof <- GET(filing.url, add_headers('user-agent' = 'SEC-13F-Scraper ([[abhinav.s.krishnan@vanderbilt.edu]])'))
     
      # Extract all hyperlinks from webpage
      filing.html <- filing.spoof %>% 
        read_html() %>% 
        html_elements('a') %>% # here, using the html class works better than trying to use the xpath
        html_attrs()
      
        # this code produces a list of attributes for each html element with tag 'a' on the website
        # the a tag, along with href =, is used to denote hyperlinks
      
      # Unlist list of hyperlinks obtained from filing webpage
      filing.links <- data.table(unlist(filing.html))
      
      # filter to hyperlink to Form 13F html infotable
      inftbl.url.partial <- filing.links[grepl("infotable.xml", V1) & grepl("Form13F", V1)]
        # NOTE: Here we're assuming that all infotables are marked as such. we may need to do this by reference instead
      
      # append base url to partial url
      inftbl.url <- inftbl.url.partial[, paste0(p_url_base, V1)] # yay this works too!
      
      # Add inftbl url to dt_13f
      dt_13f[Filing.URL == filing.url, Infotable.URL := inftbl.url]
      
      # Delay Execution to avoid Too Many Requests issue
      Sys.sleep(sample(1:2, 1))
   
    }
      
  }
    
    # Here's the issue with this method: I can only identify urls based on their contents.
    # since the url to the infotable is often named differently, this method is not robust.
    # Instead, I should try to extract the table on the webpage AND THEN use the "TYPE" column to identify the url I need
      
    # UPDATE: I just ran this for loop and it works without errors, but the problem above is still an issue.
      # We can address this in the next version
  
# =============================================================================
# =============================== Extracting Infotable ====================================
# =============================================================================

    # Appending infotables directly dt_13f will create a HUGE dataset that will be cumbersome to navigate
    # Instead, I'll try to create a list for each entity and each year.
      # then, I can unlist and stack each level together
    # Realistically, the infotable datasets don't need any extra documentation information
      
    # Create empty list with slots for every entity
      ls_entities <- vector(mode = 'list', length = length(dt_metadata[, current_entity_name]))
      
      ls_entities <- setNames(ls_entities, dt_metadata[, current_entity_name])
      
      # It would also be helpful to create a sublist of years to add each corresponding year's infotables to
      # For now, however, we'll just add infotables to the entity names
    
      # Set column names for modification in loop
        
        # Old and new column names
        old_inftbl_names <- paste0("X", seq(1, 12, 1))
        new_inftbl_names <- c("Name of Issuer", "Title of Class", "CUSIP", "Value", "Shares Amnt", "SH or PRN", "Put or Call", "Investment Discretion", "Other Manager", "Sole Voting Authority", "Shared Voting Authority", "None Voting Authority")
        
        # Set Header rows to be removed
        inftbl_remove_rows <- c("COLUMN 1", "", "NAME OF ISSUER")
      
    for (entity in dt_metadata[, current_entity_name]) {
      
      # Create empty datatable to store datatbles for each entity
      dt_entity <- data.table()
      
      for (inftbl.url in dt_13f[Entity.Name == entity, Infotable.URL]) {
        
        # Set if condition for NA filing URLs
        # Question: why are there NA filing urls? Need to figure out this problem.
        
        if (is.na(inftbl.url) | inftbl.url == p_url_base) {
          
          # Add inftbl url to dt_13f
          dt_13f[Infotable.URL == inftbl.url, Infotable.URL := NA]
          
        } else { 
          
          # Record the year of the filing
          current_year <- dt_13f[Infotable.URL == inftbl.url, str_sub(Reporting.date, 1, 4)]
          
          # Spoof user agent
          inftbl.spoof <- GET(inftbl.url, add_headers('user-agent' = 'SEC-13F-Scraper ([[abhinav.s.krishnan@vanderbilt.edu]])'))
          
          # Extract entire table of information
          current.inftbl.dt <- inftbl.spoof %>% 
            read_html() %>% 
            html_node(xpath = '/html/body/table[2]') %>% # this seems to be the xml path for all tables
            html_table()
          
          # Set new column names
          setnames(current.inftbl.dt, old = old_inftbl_names, new = new_inftbl_names)
          
          # Remove header rows
          # current.inftbl.dt[!grepl(pattern = inftbl_remove_rows, x = 'Name of Issuer')]
          
          # RBind current infotable datatable to collective infotable datatable
          dt_entity <- rbind(dt_entity, current.inftbl.dt)
          
        }
        
      }
      
      ls_entities[[entity]] <- dt_entity
      
    }  
      
    
      
# ============================== WORKSPACE: Company Search Page ==============================
  
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

# ==================== STAGE THREE: Filing Table Extract =======================
  
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

    