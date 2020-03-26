## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(b3archives)

archive <- b3_indexreport_download(as.Date("2019-09-18"))
df <- b3_indexreport_read(archive)
file.remove(archive)
dplyr::glimpse(df)


