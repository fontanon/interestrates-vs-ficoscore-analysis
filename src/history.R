library('testthat')
library('ProjectTemplate')
create.project("dataanalysys-project1")
setwd("/home/fontanon/Dropbox/Devel/dataanalysis-project1/")
savehistory("~/Dropbox/Devel/dataanalysis-project1/dataanalysys-project1/src/history.R")
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv", destfile="data/loansData.csv")
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda", destfile="data/loansData.rda")
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansCodebook.pdf", destfile="data/loansCodebook.pdf")
dateDownloaded <- date()
savehistory("~/Dropbox/Devel/dataanalysis-project1/dataanalysys-project1/src/history.R")
cleanLoansData$Interest.Rate <- gsub("\\%", "", cleanLoansData$Interest.Rate)
cleanLoansData$Debt.To.Income.Ratio <- gsub("\\%", "", cleanLoansData$Debt.To.Income.Ratio)
library('testthat')
library('ProjectTemplate')
cache()
cache("cleanLoansData")
cache(cleanLoansData)
cache(cleanLoansData)
save(cleanLoansData)
cleanLoansData$Loan.Length <- gsub(" months", "", cleanLoansData$Loan.Length)
View(cleanLoansData)
?save
wd
getwd
getwd()
save(cleanLoansData)
save(cleanLoansData, file="dat")
save(cleanLoansData, file="dataanalysys-project1/cache/")
View(loansData)
# Generate project
setwd("/home/fontanon/Dropbox/Devel/dataanalysis-project1/")
# Generate project
setwd("/home/fontanon/Dropbox/Devel/dataanalysis-project1/")
# Perform download of source files
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv", destfile="data/loansData.csv")
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda", destfile="data/loansData.rda")
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansCodebook.pdf", destfile="data/loansCodebook.pdf")
dateDownloaded <- date()
savehistory("~/Dropbox/Devel/dataanalysis-project1/dataanalysys-project1/src/history.R")
