# Function to pull data from the sosci API given a a query (ds_file).
# This also assumes a certain set of questions so needs to be adapted to the dataset that is being queried.

get_data_from_api <- function(ds_file) {
  
  # Reading in the data. 
  ds = read.table(
    file=ds_file, encoding="UTF-8",
    header = FALSE, sep = "\t", quote = "\"",
    dec = ".", #row.names = "CASE",
    col.names = c(
      "CASE","SERIAL","REF","QUESTNNR","MODE","LANGUAGE","STARTED","B001","B002",
      "B003_01","C001_01","C002_01","C003_01","C003_02","C003_03","C003_04","C004_01",
      "C005_01","C005_02","C005_03","C005_04","C005_05","C006_01","C006_02","C006_03",
      "C007_01","C007_02","C007_03","C007_04","C007_05","C008","D001","D002","D003",
      "D004_01","D004_02","D004_03","D004_04","D004_05","D005_01","D006_01","D007_01",
      "D008_01","E001_01","E002_01","E002_02","E003_01","E003_02","E003_03","E003_04",
      "E003_05","E003_06","E004","E005","E006","E007_01","E008_01","E008_02",
      "E008_03","E008_04","E008_05","E008_06","F001_01","F001_01a","F002_01",
      "F003_01","F003_01a","F003_02","F003_02a","F004","F005","F006","F007","F008",
      "F009","F010","F011","F012","F013","F014","F015","F016","F017","F018","F019",
      "F020","F021","F022_01","TIME001","TIME002","TIME003","TIME004","TIME005",
      "TIME006","TIME007","TIME008","TIME009","TIME010","TIME011","TIME012","TIME013",
      "TIME014","TIME015","TIME016","TIME017","TIME018","TIME019","TIME020","TIME021",
      "TIME022","TIME023","TIME024","TIME025","TIME026","TIME027","TIME028",
      "TIME_SUM","MAILSENT","LASTDATA","FINISHED","Q_VIEWER","LASTPAGE","MAXPAGE"
    ),
    as.is = TRUE,
    colClasses = c(
      CASE="numeric", SERIAL="character", REF="character", QUESTNNR="character",
      MODE="character", LANGUAGE="character", STARTED="POSIXct", B001="numeric",
      B002="numeric", B003_01="numeric", C001_01="numeric", C002_01="numeric",
      C003_01="numeric", C003_02="numeric", C003_03="numeric", C003_04="numeric",
      C004_01="numeric", C005_01="numeric", C005_02="numeric", C005_03="numeric",
      C005_04="numeric", C005_05="numeric", C006_01="numeric", C006_02="numeric",
      C006_03="numeric", C007_01="numeric", C007_02="numeric", C007_03="numeric",
      C007_04="numeric", C007_05="numeric", C008="numeric", D001="numeric",
      D002="numeric", D003="numeric", D004_01="numeric", D004_02="numeric",
      D004_03="numeric", D004_04="numeric", D004_05="numeric", D005_01="numeric",
      D006_01="numeric", D007_01="numeric", D008_01="numeric", E001_01="numeric",
      E002_01="numeric", E002_02="numeric", E003_01="numeric", E003_02="numeric",
      E003_03="numeric", E003_04="numeric", E003_05="numeric", E003_06="numeric",
      E004="numeric", E005="numeric", E006="numeric", E007_01="numeric",
      E008_01="numeric", E008_02="numeric", E008_03="numeric", E008_04="numeric",
      E008_05="numeric", E008_06="numeric", F001_01="numeric", F001_01a="logical",
      F002_01="numeric", F003_01="numeric", F003_01a="logical", F003_02="numeric",
      F003_02a="logical", F004="numeric", F005="numeric", F006="numeric",
      F007="numeric", F008="numeric", F009="numeric", F010="numeric",
      F011="numeric", F012="numeric", F013="numeric", F014="numeric",
      F015="numeric", F016="numeric", F017="numeric", F018="numeric",
      F019="numeric", F020="numeric", F021="character", F022_01="numeric",
      TIME001="integer", TIME002="integer", TIME003="integer", TIME004="integer",
      TIME005="integer", TIME006="integer", TIME007="integer", TIME008="integer",
      TIME009="integer", TIME010="integer", TIME011="integer", TIME012="integer",
      TIME013="integer", TIME014="integer", TIME015="integer", TIME016="integer",
      TIME017="integer", TIME018="integer", TIME019="integer", TIME020="integer",
      TIME021="integer", TIME022="integer", TIME023="integer", TIME024="integer",
      TIME025="integer", TIME026="integer", TIME027="integer", TIME028="integer",
      TIME_SUM="integer", MAILSENT="POSIXct", LASTDATA="POSIXct",
      FINISHED="logical", Q_VIEWER="logical", LASTPAGE="numeric",
      MAXPAGE="numeric"
    ),
    skip = 1,
    check.names = TRUE, fill = TRUE,
    strip.white = FALSE, blank.lines.skip = TRUE,
    comment.char = "",
    na.strings = ""
  )
  
  rm(ds_file)
  
  attr(ds, "project") = "eurofound"
  attr(ds, "description") = "Eurofound e-survey Living, working and COVID-19 "
  attr(ds, "date") = Sys.time()
  attr(ds, "server") = "https://s2survey.net"
  
  return(ds)
  
}