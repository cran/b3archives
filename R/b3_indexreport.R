#' Get Index Report BVBG.087.01
#'
#' @description Downloads archive Index Report BVBG.087.01 from B3's website
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
#' b3_indexreport_download(as.Date("2020-01-31"))
#'}
#' @export

b3_indexreport_download <- function(date, last = TRUE, directory = tempdir()){

  first_wd <- getwd()
  on.exit(first_wd)
  setwd(directory)

  if(class(date) != "Date"){
    stop("date should be a Date class object!")
  }

  url <- paste0("http://www.b3.com.br/pesquisapregao/download?filelist=IR",
                format(date, "%y%m%d"),
                ".zip")

  temp_dir <- directory
  zip_file <- "indexreport.zip"
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

#' Read and process Index Report BVBG.087.01
#'
#' @param file path to the file
#'
#' @import xml2
#' @import dplyr
#' @import purrr
#'
#' @return a data_frame with tidy data
#' @examples
#'\donttest{
#' b3_indexreport_read(b3_indexreport_download(as.Date("2020-01-31")))
#'}
#' @export

b3_indexreport_read <- function(file){

  BVBG.087.01 <- xml2::read_xml(file)
  # message(paste0("Stripping node set from file ", file, ". It can take a long time..."))
  xml2::xml_ns_strip(BVBG.087.01)

  l <- list()

  # Opening price.
  l[["OpngPric"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/OpngPric/../../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/OpngPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/OpngPric") %>%
      xml2::xml_double(),
    Attribute = "OpngPric"
  )

  # Minimum price.
  l[["MinPric"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/MinPric/../../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/MinPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/MinPric") %>%
      xml2::xml_double(),
    Attribute = "MinPric"
  )

  # Maximum price.
  l[["MaxPric"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/MaxPric/../../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/MaxPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/MaxPric") %>%
      xml2::xml_double(),
    Attribute = "MaxPric"
  )

  # Trade Average Price.
  l[["TradAvrgPric"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/TradAvrgPric/../../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/TradAvrgPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/TradAvrgPric") %>%
      xml2::xml_double(),
    Attribute = "TradAvrgPric"
  )

  # Previous Day Closing Price.
  l[["PrvsDayClsgPric"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/PrvsDayClsgPric/../../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/PrvsDayClsgPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/PrvsDayClsgPric") %>%
      xml2::xml_double(),
    Attribute = "PrvsDayClsgPric"
  )

  # Closing Price.
  l[["ClsgPric"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/ClsgPric/../../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/ClsgPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/ClsgPric") %>%
      xml2::xml_double(),
    Attribute = "ClsgPric"
  )

  # Index Value.
  l[["IndxVal"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/IndxVal/../../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/IndxVal/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/IndxVal") %>%
      xml2::xml_double(),
    Attribute = "IndxVal"
  )

  # Oscillation Value.
  l[["OscnVal"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/OscnVal/../../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/OscnVal/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SctyInf/OscnVal") %>%
      xml2::xml_double(),
    Attribute = "OscnVal"
  )

  # xml paths changed! Be careful...
  # Settlement Value.
  l[["SttlmVal"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SttlmVal/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SttlmVal/../SctyInf/SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/SttlmVal") %>%
      xml2::xml_double(),
    Attribute = "SttlmVal"
  )

  # Number of rising shares from composition.
  l[["RsngShrsNb"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/RsngShrsNb/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/RsngShrsNb/../SctyInf/SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/RsngShrsNb") %>%
      xml2::xml_double(),
    Attribute = "RsngShrsNb"
  )

  # Number of falling shares from composition.
  l[["FlngShrsNb"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/FlngShrsNb/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/FlngShrsNb/../SctyInf/SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/FlngShrsNb") %>%
      xml2::xml_double(),
    Attribute = "FlngShrsNb"
  )

  # Number of stable shares from composition.
  l[["StblShrsNb"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/StblShrsNb/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/StblShrsNb/../SctyInf/SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IndxInf/StblShrsNb") %>%
      xml2::xml_double(),
    Attribute = "StblShrsNb"
  )

  # xml paths changed again! Be careful...
  # Opening Price.
  l[["OpngPric_IOPV"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/OpngPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/OpngPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/OpngPric") %>%
      xml2::xml_double(),
    Attribute = "OpngPric"
  )

  # Minimum Price.
  l[["MinPric_IOPV"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/MinPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/MinPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/MinPric") %>%
      xml2::xml_double(),
    Attribute = "MinPric"
  )

  # Maximum Price.
  l[["MaxPric_IOPV"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/MaxPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/MaxPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/MaxPric") %>%
      xml2::xml_double(),
    Attribute = "MaxPric"
  )

  # Trade Average Price.
  l[["TradAvrgPric_IOPV"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/TradAvrgPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/TradAvrgPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/TradAvrgPric") %>%
      xml2::xml_double(),
    Attribute = "TradAvrgPric"
  )

  # Previous Day Closing Price.
  l[["PrvsDayClsgPric_IOPV"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/PrvsDayClsgPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/PrvsDayClsgPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/PrvsDayClsgPric") %>%
      xml2::xml_double(),
    Attribute = "PrvsDayClsgPric"
  )

  # Closing Price.
  l[["ClsgPric_IOPV"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/ClsgPric/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/ClsgPric/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/ClsgPric") %>%
      xml2::xml_double(),
    Attribute = "ClsgPric"
  )

  # Index Value.
  l[["IndxVal_IOPV"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/IndxVal/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/IndxVal/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/IndxVal") %>%
      xml2::xml_double(),
    Attribute = "IndxVal"
  )

  # Oscillation Value.
  l[["OscnVal_IOPV"]] <- dplyr::data_frame(
    Date = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/OscnVal/../../TradDt/Dt") %>%
      xml2::xml_text() %>%
      as.Date(),
    TickerSymbol = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/OscnVal/../SctyId/TckrSymb") %>%
      xml2::xml_text(),
    Value = BVBG.087.01 %>%
      xml2::xml_find_all("/Document/BizFileHdr/Xchg/BizGrp/Document/IndxRpt/IOPVInf/OscnVal") %>%
      xml2::xml_double(),
    Attribute = "OscnVal"
  )

  purrr::reduce(l, full_join, by = c("Date", "TickerSymbol", "Value", "Attribute"))
}
