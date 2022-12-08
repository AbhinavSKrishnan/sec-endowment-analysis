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
  rm(ls = list())
  
  # ============== #
  # Load Libraries
  # ============== #
  
    library(data.table)
    library(ggplot2)
    library(stringr)
    library(readxl)
  
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
    in_shareclass <- read_xlsx(path = "FFF-Company-Screens/Invest+Your+Values+shareclass+results+20221109.xlsx", sheet = 2)
    
    # Import CUSIP to Ticker dataset
    in_cusip_ticker <- read_xlsx(path = "snp100_cusip_database.xlsx")
    
# =============================================================================
# =============================== Linking Datasets ====================================
# =============================================================================
  
  # In order to assess the composition of these universities' endowments, I need to
    # identify which assets are fossil fuel emittors. 
  # I have obtained a dataset of shares with fossil fuel involvement data from Fossil Free Funds here:
      # https://www.asyousow.org/invest-your-values/spreadsheets (I'm using the Nov 9 Shareclass REsults spreadsheet)
    
  # The problem: the SEC dataset has CUSIP and Name of Issuer data, but the Shareclass Results dataset only has shareclass names, tickers, and asset manager names
    # I need some way to match CUSIP data to tickers so I can identify which stocks are fossil-fuel related
    
    # Solution 1: Ask Fossil Free Funds for data with CUSIP numbers or other identifiers
    # Solution 2: access a database that can link the two (harder)
    
  # This becomes a LOT more challenging with mutual funds that change in composition over time. I may need to download several years of Shareclass Results from Fossil Free Funds in order to figure out how mutual funds have changed over time
    
    # ugh this gets more complicated every day lol.
    
  # I could solve this problem by manually looking up CUSIP numbers while I wait for a different dataset
  # I need to know the most common funds so I can manually look them up
    
  # Create frequency table of most frequent issuer and class titles and CUSIP, in descending order of frequency
  frqtbl_shares <- in_dt_13f[, .N, by = c('Name.of.Issuer', 'Title.of.Class', 'CUSIP')][order(-N)]
    
  # Create frequency table of most frequent CUSIP numbers
  # frqtbl_cusip <- in_dt_13f[, .N, by = CUSIP][order(-N)]
  
  head(in_dt_13f[, .N, by = CUSIP][order(-N)])
  
  # quick visualization for funzies
  barplot(frqtbl_shares$N, names.arg = frqtbl_shares$Name.of.Issuer)
    