# Initialice project
setwd("/home/fontanon/Dropbox/Devel/dataanalysis-project1/")

# Perform download of source files 
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv", destfile="data/loansData.csv")
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda", destfile="data/loansData.rda")
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansCodebook.pdf", destfile="data/loansCodebook.pdf")
dateDownloaded <- date()

# Clean
cleanLoansData <- loansData
cleanLoansData$Interest.Rate <- as.numeric(gsub('\\%', '', gsub(',','.',loansData$Interest.Rate)))
cleanLoansData$Debt.To.Income.Ratio <- as.numeric(gsub('\\%', '', gsub(',','.',loansData$Debt.To.Income.Ratio)))
cleanLoansData$Loan.Length <- as.numeric(gsub('\\ months', '', loansData$Loan.Length))
cleanLoansData$Employment.Length <- gsub('\\ years','',loansData$Employment.Length)
cleanLoansData$Employment.Length <- gsub('< 1 year','0',loansData$Employment.Length)
cleanLoansData$Employment.Length <- gsub('10+','11',loansData$Employment.Length)
cleanLoansData$Employment.Length <- as.numeric(loansData$Employment.Length)