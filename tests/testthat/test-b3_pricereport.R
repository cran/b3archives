test_that("link still the same", {
  archive <- b3archives::b3_indexreport_download(as.Date("2020-01-31"))
  file.remove(archive)
})
