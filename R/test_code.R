bdir <- "C:/Users/dlkrue/Downloads/"



ff <- list.files(bdir)
ff2 <- ff[tools::file_ext(ff) == "csv"]

ff2
ii<- 1
dd3 <- list()
for (ii in 1:3){
  dd <- read.csv(paste0(bdir, ff2[ii]), skip = 8, sep = ";")
  dim(dd)
  names(dd) <- c("Buchungstag",	"Wertstellung", "Buchungstext",	"AuftraggeberBegünstigter",
                 "Verwendungszweck",	"Kontonummer",	"BLZ",
                 "Betrag",	"Gläubiger-ID",	"Mandatsreferenz", 	"Kundenreferenz",
                 "12")

  dd2<- dd[, c("Buchungstag", "Betrag")]
  dd3[[ii]] <- dd2
}

dd <- do.call(rbind, dd3)

dd$tag <- as.Date(dd$Buchungstag, format = "%d.%m.%Y")
dd$week <- paste0(strftime(dd$tag,format="%Y"), strftime(dd$tag,format="%W"))

dd$Betrag <- as.numeric(gsub(",", ".", as.character(dd$Betrag)))

dd2 <- aggregate(dd$Betrag, list(dd$week), sum)

mm <-mean(dd2$x[dd2$x < -30])

barplot(dd2$x, names.arg = dd2$Group.1, las = 2)
abline(mm, 0)
