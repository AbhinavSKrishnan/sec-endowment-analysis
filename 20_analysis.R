#############################################
# Title: 20_analyze_data
# Author: Abhinav Krishnan
# Date: 16 November 2022
# Inputs: 
# Outputs: 
#############################################

# =============================================================================
# =============================== Description =================================
# =============================================================================

# okay so this script needs to do the following:
  # 1. identify which stocks contain fossil fuels/are related to fossil fuel companies
  # 2. cross-reference total endowment sizes with the universities' financial reports

# =============================================================================
# ================================= Setup =====================================
# =============================================================================

  # clear working environment
  rm(list = ls())
  
  # ============== #
  # Load Libraries
  # ============== #
  
    library(data.table)
    library(ggplot2)
    library(stringr)
    library(readxl)
    library(httr)
    library(jsonlite)
    library(tidyverse)
  
  # ============== #
  # Set Parameters
  # ============== #
  
    # set export toggle
    p_export <- F
    
    # set timestamp
    p_timestamp <- Sys.time()
    
    # set output directory
    p_dir_out <- ""
    
    # set input directory
    p_dir_in_base <- "/Output/"
  
  # ============== #
  # Load Data
  # ============== #

    # Create list of infotable datasets
    vec_datasets <- list.files(path = "Output/", full.names = T)
    
    # Create filepath of most recent file
    # filepath_dt_recent <- paste0(p_dir_in_base_vec_datasets[length(vec_datasets)])
    
    # Read most recent file since length(vec_datasets) returns the index of the most recent dataset
    in_dt_13f <- as.data.table(read.csv(file = vec_datasets[length(vec_datasets)]))
    
    # Import Fossil Free Funds Shareclass Dataset
    in_shareclass <- as.data.table(read_xlsx(path = "FFF-Company-Screens/Invest+Your+Values+shareclass+results+20221109.xlsx", sheet = 2))
    
    # IMport Fossil Free Funds Company Dataset
    in_company <- as.data.table(read_xlsx(path = "FFF-Company-Screens/Invest+Your+Values+company+screens+20221109.xlsx", sheet = 2))
    
    # Import CUSIP to Ticker dataset
    in_cusip_ticker <- as.data.table(read_xlsx(path = "snp100_cusip_database.xlsx"))
    
  # ============== #
  # Format Data
  # ============== #
  
    # in_dt_13f: Remove X and .id columns, change column order
      dt_13f <- in_dt_13f[, c("X", ".id") := NULL]
      setcolorder(dt_13f, c("Year", "Entity"))
    
    # in_cusip_ticker: Remove the extra Symbol...3 column
      dt_cusip_ticker <- in_cusip_ticker[, "Symbol...3" := NULL]
      setnames(dt_cusip_ticker, "Symbol...1", "ticker")
    
    # in_shareclass: 
      # create vector of columns to keep
      cols_shareclass <- grep("Fund profile:|Fossil Free Funds:", colnames(in_shareclass), value = TRUE)
      # subset dataset to keep_cols
      ff_shareclass <- in_shareclass[, .SD, .SDcols = cols_shareclass]
    
    # in_company: 
      # subset data to cols_company and 
      cols_company <- c("Company", "Country", "Tickers", 
                        grep("Fossil Free Funds: ", colnames(in_company), value = TRUE))
      company_select <- in_company[, .SD, .SDcols = cols_company]
      
      # Separate cells with multiple tickers
      ff_company <- as.data.table(separate_rows(company_select, Tickers, sep = ", "))
      
    
# =============================================================================
# =============================== Linking CUSIP Datasets ====================================
# =============================================================================
  
  # In order to assess the composition of these universities' endowments, I need to
    # identify which assets are fossil fuel emitters. 
  # I have obtained a dataset of shares with fossil fuel involvement data from Fossil Free Funds here:
      # https://www.asyousow.org/invest-your-values/spreadsheets (I'm using the Nov 9 Shareclass REsults spreadsheet)
    
  # The problem: the SEC dataset has CUSIP and Name of Issuer data, but the Shareclass Results dataset only has shareclass names, tickers, and asset manager names
    # I need some way to match CUSIP data to tickers so I can identify which stocks are fossil-fuel related
    
    # Solution 1: Manually search and link CUSIP numbers to Tickers
    # Solution 2: Ask Fossil Free Funds for data with CUSIP numbers or other identifiers - tried this!
    # Solution 3: access a database that can link the two
    
  # This becomes a LOT more challenging with mutual funds that change in composition over time. I may need to download several years of Shareclass Results from Fossil Free Funds in order to figure out how mutual funds have changed over time
    
    # ugh this gets more complicated every day lol.
    
  # ============== #
  # Method 1: Manually search CUSIP + Tickers
  # ============== #
    # I could solve this problem by manually looking up CUSIP numbers while I wait for a different dataset
    # I need to know the most common funds so I can manually look them up
    
    # Create frequency table of most frequent issuer and class titles and CUSIP, in descending order of frequency to prioritize which CUSIP ids to look up first
    frqtbl_shares <- in_dt_13f[, .N, by = c('Name.of.Issuer', 'Title.of.Class', 'CUSIP')][order(-N)]
      
    # Create frequency table of most frequent CUSIP numbers
    frqtbl_cusip <- in_dt_13f[, .N, by = CUSIP][order(-N)]
    
    # I should be able to use this to identify which CUSIP numbers to look up first
    
    head(in_dt_13f[, .N, by = CUSIP][order(-N)])
    
    # quick visualization for funzies
    barplot(frqtbl_shares$N, names.arg = frqtbl_shares$Name.of.Issuer)
  
  # ============== #
  # Method 2: Use free CUSIP dataset
  # ============== #
  
    # I found this free CUSIP dataset - we can merge it with any other datasets we find
  
    
  # ============== #
  # Method 3: use sec-api to generate ticker data using CUSIP numbers
  # ============== #
    
    # I have discovered an API that will map CUSIP to Ticker symbols, so now I'm learning about JSON and API calls! Wish me luck :)
    # api: https://sec-api.io/docs/mapping-api/map-cusip-to-company-details
    
      # okay this silly api limits me to 100 queries, so I need to make every single one count
      # I can only find the tickers for the first 100 share, so I'll prioritize the top shares in frqtbl_shares
    
    # specify testing cusip
    # test.cusip <- "607409109"
    
    # extract first 98 shares sorted in highest to lowest frequency
    top_98_shares <- head(frqtbl_shares, n = 98)
    
    # NOTE: frqtbl_shares, unlike frqtbl_cusip, lists the most frequent unique combinations of the Name.of.Issuer, Title.of.Class, and CUSIP. I'm not entirely sure how this will affect the frequency of the top_98_shares
    
    # specify base url for query
    base_url <- "https://api.sec-api.io/mapping"
    
    # set API key
    api.key <- "3e687bb9268f4e1eaff9be2b239bc2ea2c0cb90c4662695d51ce478f242c0f6d"
    
    # Create empty datatable to store datatables for each cusip
    dt_cusip <- data.table()
    
    # set query limit parameter
    p.query.limit <- FALSE
    
    # Make the for loop conditional on the query limit
    if (p.query.limit == TRUE) {
    
      for (current_cusip in top_98_shares[, CUSIP]) {
        
        # construct info url to query current_cusip
        current_info_url <- paste0("/cusip/", current_cusip)
        
        # construct full query url
        current_full_url <- paste0(base_url, current_info_url)
        
        # call using api.key as authorization
        api_call <- httr::GET(current_full_url, config = add_headers(Authorization = api.key))
        
        # set error code
        if(api_call$status_code != "200") {stop("API call FAILED")}
        # NOTE: you always want to check the status_code for a successful api call
        
        # convert hexadecimal raw content to JSON character
        api_char <- base::rawToChar(api_call$content)
        
        # convert json code to dataframe
        api_JSON <- as.data.table(jsonlite::fromJSON(api_char, flatten = TRUE))
        
        # RBind current infotable datatable to collective infotable datatable
        dt_cusip <- rbind(dt_cusip, api_JSON)
        
      }
      
    } else {
      
      # Import existing dt_cusip
      dt_cusip <- readRDS("cusip_ticker_mapping")
      
    }
    
    # YAY THIS WORKS! Note: this cannot be repeated since I've exhausted all of my free queries. I'm setting up an if statement that will prevent this from being rerun
  
# =============================================================================
# =============================== Merge CUSIP Datasets ====================================
# =============================================================================

  # Time to merge these datasets! We're going to re-merge the smaller, free ticker dataset AFTER using the dataset we generated with the API
    # not sure if this will make a significant difference in the resulting datasets
  
  # Rename dt_cusip "cusip" column to match in_dt_13f
  setnames(dt_cusip, "cusip", "CUSIP")
  
  # we're running into an issue with a one to many merge so let's try removing rows in dt_cusip that have multiple cusips listed. I doubt this will solve the problem but it's worth a shot
  
  # Remove exact duplicates from in_dt_13f
  no_dups_13f   <- unique(in_dt_13f)
  
  # some entries in dt_cusip are exactly the same, except for the currency, so I've removed duplicates using cik and cusip as key columns
  no_dups_cusip <- unique(dt_cusip, by = c("cik", "CUSIP"))
  
  # Separate rows with multiple cusip numbers in the same cell into several rows, each with one cusip number
  dt_cusip_api <- separate_rows(no_dups_cusip, CUSIP, sep = " ")
  
  # Okay actually, to avoid having to merge twice, and then join two ticker columns, we'll rbind dt_cusip_single and dt_cusip_sticker first, eliminate any duplicate tickers, and then join once with the no_dups_13f dataset
  
  # stack both cusip datasets
    setnames(dt_cusip_ticker, "Name", "name")
    
  # remove excess columns from dt_cusip_api so that the datasets stack correctly
  dt_cusip_api_limited <- dt_cusip_api[, c("name", "ticker", "CUSIP")]
    
  dt_cusip_stacked <- rbind(dt_cusip_api_limited, dt_cusip_ticker, fill = TRUE)
  
  dt_cusip_all <- unique(dt_cusip_stacked, by = "ticker")
  
  # Merge no_dups_13f and dt_cusip
  dt_cusip_merged <- merge(x = no_dups_13f, y = dt_cusip_all, by = "CUSIP", all.x = TRUE)
  
  # set colorder for easier viewing
  setcolorder(dt_cusip_merged, c("Year", "Entity", "Name.of.Issuer", "CUSIP", "ticker"))
  
# =============================================================================
# =============================== Merge FFF Dataset ====================================
# =============================================================================

  # Now that we have *some* securities with CUSIP data, let's add the Fossil Free Funds data

  # Rename tickers columns to match
  setnames(ff_shareclass, c("Fund profile: Shareclass name", "Fund profile: Ticker"), c("Name", "Ticker"))
  setnames(ff_company, c("Company", "Tickers"), c("Name", "Ticker"))
  
  # Stack ff_shareclass and ff_company
  ff_all <- rbind(ff_shareclass, ff_company, fill = TRUE)
  
  # merge dt_cusip_merged with ff_shareclass
  dt_13f_ff <- merge(dt_cusip_merged, ff_shareclass, by.x = "ticker", by.y = "Fund profile: Ticker", all.x = TRUE)
  
# =============================================================================
# =============================== Export =====================================
# =============================================================================

  if (p_export == TRUE) {
    
    saveRDS(in_dt_13f, file = "dt_13f")
    saveRDS(frqtbl_shares, file = "frqtbl_shares")
    
    # only saves new dt_cusip if query limit is set to true (implying that the dataset has been updated)
    if (p.query.limit == TRUE) {
      saveRDS(dt_cusip, file = paste0("cusip_ticker_mapping", p_timestamp))
    }
  
  }