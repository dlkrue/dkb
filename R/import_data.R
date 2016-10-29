#' import_data
#'
#' @param last_file character file giving path to csv-input
#'
#' @return data.frame with imported and cleaned data
#'
#' @details if last_file is not provided, the last downloaded file is automatically chosen
#'
#' @export
#'
#' @examples
#' import_data()
import_data <- function(last_file = get_last_file()) {
  dd <- read.csv(last_file, skip = 8, sep = ";", stringsAsFactors = F)
  dim(dd)
  names(dd) <- c("Buchungstag",	"Wertstellung", "Buchungstext",	"AuftraggeberBegünstigter",
                 "Verwendungszweck",	"Kontonummer",	"BLZ",
                 "Betrag",	"Gläubiger-ID",	"Mandatsreferenz", 	"Kundenreferenz",
                 "12")
  dd$Betrag <- as.numeric(gsub(",", ".", gsub("\\.", "", dd$Betrag)))

  dd$day <- as.Date(dd$Buchungstag, format = "%d.%m.%Y")
  dd$week <- paste0(strftime(dd$day,format="%Y"), strftime(dd$day,format="%W"))
  dd$month <- paste0(strftime(dd$day,format="%Y"), strftime(dd$day,format="%m"))

  cat("\n\n# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #\n",
      "Import summary:-----------------------------------------------------\n",
      "account movements: ", nrow(dd), "\n",
      "max deposit: ", max(dd$Betrag, na.rm = T), " - ", substr(dd$Verwendungszweck[which.max(dd$Betrag)], 1, 30), "\n",
      "max payout: ", min(dd$Betrag, na.rm = T), " - ", substr(dd$Verwendungszweck[which.min(dd$Betrag)], 1, 30), "\n",
      "time range", as.character(range(dd$day)))

  return(dd)

}


# --------------------------------

get_ff_profile_dir <- function() {
  user_name <- shell(cmd = "echo %USERNAME%", intern = T)
  bdir <- paste0("C:/Users/", user_name, "/AppData/Roaming/Mozilla/Firefox/")

  cat("Hello", user_name, "\nGetting your ff profile directory")
  tmp <- readLines(paste0(bdir, "profiles.ini"))
  tmp <- grep("Path\\=Profiles\\/", tmp, value = T)
  tmp <- gsub("Path\\=Profiles\\/", "", tmp)

  ff_dir <- paste0(bdir, "Profiles/", tmp)
  if (!dir.exists(ff_dir)) {stop("couldnt finf ff profile dir")
  } else {return(ff_dir)}

}

# read last downloaded file
get_last_downloaded_filename <- function(ff_dir = get_ff_profile_dir()) {

  dbname <- paste0(ff_dir, "/places.sqlite")
  con = RMySQL::dbConnect(RSQLite::SQLite(), dbname=dbname)
  ii <- 2
  alltables <- RMySQL::dbListTables(con)
  tab <- alltables[ii]
  # get the populationtable as a data.frame
  p1 <- DBI::dbGetQuery(con, paste0('select * from ', tab))
  head(p1)

  last_file <- tail(grep("\\.csv", p1$content, value = T), 1)
}

get_download_dir <- function(ff_dir = get_ff_profile_dir()) {
  js_file <- paste0(ff_dir, "/prefs.js")

  tmp <- readLines(js_file)
  tmp <- grep("browser\\.download\\.dir", tmp, value = T)
  tmp <- stringr::str_split(string = tmp, pattern = "browser\\.download\\.dir")[[1]][2]

  regexpr(pattern = '\",', text = tmp)
  tmp <- gsub('");', "", gsub(pattern = '\", \"', "", tmp))
  return(tmp)
}


get_last_file <- function() {
  last_file <- paste(get_download_dir(), get_last_downloaded_filename(), sep="\\\\")
  file.exists(last_file)
  return(last_file)
}
