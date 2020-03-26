#' Get Pricing Report BVBG.086.01
#'
#' @description Downloads archive Pricing Report BVBG.086.01 from B3's website
#'
#' @param date "yyyy-mm-dd" in class Date format
#' @param last logical setting if we keep all files or choose the last one
#' @param directory a string for the directory where the files will be saved
#'
#' @import dplyr
#' @import utils
#'
#'
#' @return a file
#' @examples
#'\donttest{
#' b3_pricereport_download(as.Date("2020-01-31"))
#'}
#' @export

b3_pricereport_download <- function(date, last = TRUE, directory = tempdir()){

  first_wd <- getwd()
  on.exit(first_wd)
  setwd(directory)

  if(class(date) != "Date"){
    stop("date should be a Date class object!")
  }

  url <- paste0("http://www.b3.com.br/pesquisapregao/download?filelist=PR",
                format(date, "%y%m%d"),
                ".zip")

  temp_dir <- directory
  zip_file <- "pricereport.zip"
  utils::download.file(url = url, destfile = zip_file)
  unzipped_file <- unzip(zip_file)
  unzipped_file_again <- unzip(unzipped_file)

  # select last archive by time
  if(last == TRUE){
    which_file <- unzipped_file_again %>%
      substr(40, 49) %>%
      which.max()
  }else{
    which_file <- 1:length(unzipped_file_again)
  }

  # delete archives
  file.remove(zip_file)
  file.remove(unzipped_file)
  file.remove(unzipped_file_again[unzipped_file_again != unzipped_file_again[which_file]])

  paste0(directory, "/", unzipped_file_again[which_file])
}


#' Read and process Pricing Report BVBG.086.01
#'
#' @param file path to the file
#'
#' @import xml2
#' @import dplyr
#' @import purrr
#'
#' @return a data_frame with tidy data
#' @examples
#' \donttest{
#' b3_pricereport_read(b3_pricereport_download(as.Date("2020-01-31")))
#' }
#' @export

b3_pricereport_read <- function(file){

  BVBG.086.01 <- xml2::read_xml(file)
  message(paste0("Stripping node set from file ", file, ". It can take a long time..."))
  xml2::xml_ns_strip(BVBG.086.01)

  l <- list()

  # Amount of the initial value of the asset.
  l[["FirstPrice"]] <- dplyr::data_frame(
    Date = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/FrstPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/FrstPric/../../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/FrstPric") %>%
      xml2::xml_double(),
    Attribute = "FirstPrice"
  )

  # Maximum Price.
  l[["MaximumPrice"]]  <- dplyr::data_frame(
    Date = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/MaxPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/MaxPric/../../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value  = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/MaxPric") %>%
      xml2::xml_double(),
    Attribute = "MaximumPrice"
  )

  # Minimum Price.
  l[["MinimumPrice"]] <- dplyr::data_frame(
    Date = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/MinPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/MinPric/../../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value  = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/MinPric") %>%
      xml2::xml_double(),
    Attribute = "MinimumPrice"
  )

  # Last price.
  l[["LastPrice"]] <- dplyr::data_frame(
    Date = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/LastPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/LastPric/../../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value  = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/LastPric") %>%
      xml2::xml_double(),
    Attribute = "LastPrice"
  )

  # Financial volume traded (R$).
  l[["NationalFinancialVolume"]] <- dplyr::data_frame(
    Date = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/NtlFinVol/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/NtlFinVol/../../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value  = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/NtlFinVol") %>%
      xml2::xml_double(),
    Attribute = "NationalFinancialVolume"
  )

  # Quantity of financial instrument traded.
  l[["FinancialInstrumentQuantity"]] <- dplyr::data_frame(
    Date = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/FinInstrmQty/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/FinInstrmQty/../../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value  = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/FinInstrmQty") %>%
      xml2::xml_double(),
    Attribute = "FinancialInstrumentQuantity"
  )

  # Quantity of open contract.
  l[["OpenInterest"]] <- dplyr::data_frame(
    Date = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/OpnIntrst/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/OpnIntrst/../../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value  = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/OpnIntrst") %>%
      xml2::xml_double(),
    Attribute = "OpenInterest"
  )

  # Best Bid Price.
  l[["BestBidPrice"]] <- dplyr::data_frame(
    Date = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/BestBidPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/BestBidPric/../../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value  = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/BestBidPric") %>%
      xml2::xml_double(),
    Attribute = "BestBidPrice"
  )

  # Best Ask Price.
  l[["BestAskPrice"]] <- dplyr::data_frame(
    Date = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/BestAskPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/BestAskPric/../../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value  = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/BestAskPric") %>%
      xml2::xml_double(),
    Attribute = "BestAskPrice"
  )

  # Trade Average Price.
  l[["TradeAveragePrice"]] <- dplyr::data_frame(
    Date = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/TradAvrgPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/TradAvrgPric/../../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value  = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/TradAvrgPric") %>%
      xml2::xml_double(),
    Attribute = "TradeAveragePrice"
  )

  # Rate of oscillation.
  l[["OscillationPercentag"]] <- dplyr::data_frame(
    Date = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/OscnPctg/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/OscnPctg/../../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value  = BVBG.086.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/PricRpt/FinInstrmAttrbts/OscnPctg") %>%
      xml2::xml_double(),
    Attribute = "OscillationPercentag"
  )

  purrr::reduce(l, full_join, by = c("Date", "TickerSymbol", "Value", "Attribute"))
}



