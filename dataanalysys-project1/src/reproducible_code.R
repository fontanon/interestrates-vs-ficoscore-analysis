# Initialice project
setwd("/home/fontanon/Dropbox/Devel/dataanalysis-project1/dataanalysys-project1")

# Perform download of source files 
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv", destfile="data/loansData.csv")
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda", destfile="data/loansData.rda")
download.file("http://spark-public.s3.amazonaws.com/dataanalysis/loansCodebook.pdf", destfile="data/loansCodebook.pdf")
dateDownloaded <- date()
load("~/Dropbox/Devel/dataanalysis-project1/dataanalysys-project1/data/loansData.rda")

# Clean
cleanLoansData <- loansData
cleanLoansData$Interest.Rate <- as.numeric(gsub('\\%', '', gsub(',','.',loansData$Interest.Rate)))
cleanLoansData$Debt.To.Income.Ratio <- as.numeric(gsub('\\%', '', gsub(',','.',loansData$Debt.To.Income.Ratio)))
#cleanLoansData$Loan.Length <- as.numeric(gsub('\\ months', '', loansData$Loan.Length))
cleanLoansData$Employment.Length <- gsub('\\ years','',loansData$Employment.Length)
cleanLoansData$Employment.Length <- gsub('< 1 year','0',loansData$Employment.Length)
cleanLoansData$Employment.Length <- gsub('10+','11',loansData$Employment.Length)
cleanLoansData$Employment.Length <- as.numeric(loansData$Employment.Length)

# Exploratory Analysis
par(mfrow=c(1,2))

#Linear model with FICO Range as numeric
IRFR.lm <- lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$FICO.Range))

# Plot Interest Rate vs. FICO Range
plot(cleanLoansData$Interest.Rate ~ as.factor(cleanLoansData$FICO.Range), col="blue", varwidth=TRUE)
lines(as.numeric(cleanLoansData$FICO.Range), IRFR.lm$fitted, col="red", lwd=3)

# Plot residuals
plot(cleanLoansData$FICO.Range, IRFR.lm$residuals, pch=19, col="blue")
abline(c(0,0),col="red",lwd=3)

# ... At this point describe regression line and set there's correlation.....
IRFR.rsquared <- summary(IRFR.lm)$r.squared
IRFR.confint <- confint(IRFR.lm)