# Final Loan Analysis
# ===================

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
cleanLoansData$Loan.Length <- as.numeric(gsub('\\ months', '', loansData$Loan.Length))
cleanLoansData$Employment.Length <- gsub('\\ years','',loansData$Employment.Length)
cleanLoansData$Employment.Length <- gsub('< 1 year','0',loansData$Employment.Length)
cleanLoansData$Employment.Length <- gsub('10+','11',loansData$Employment.Length)
cleanLoansData$Employment.Length <- as.numeric(loansData$Employment.Length)

# Interest Rate vs. FICO-Range Analysis
IRFR.lm <- lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$FICO.Range))
IRFR.rsquared <- summary(IRFR.lm)$r.squared
IRFR.confint <- confint(IRFR.lm)

# Plot Interest Rate vs. FICO Range
par(mfrow=c(1,2))
plot(cleanLoansData$Interest.Rate ~ as.factor(cleanLoansData$FICO.Range), col="blue", varwidth=TRUE)
lines(as.numeric(cleanLoansData$FICO.Range), IRFR.lm$fitted, col="red", lwd=3)

# Plot residuals
plot(cleanLoansData$FICO.Range, IRFR.lm$residuals, pch=19, col="blue", varwith=TRUE)
abline(c(0,0),col="red",lwd=3)

# Exploration of r-squares
summary(lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$Employment.Length)))$r.squared
summary(lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$Home.Ownership)))$r.squared
summary(lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$Inquiries.in.the.Last.6.Months)))$r.squared
summary(lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$Revolving.CREDIT.Balance)))$r.squared
summary(lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$Open.CREDIT.Lines)))$r.squared
summary(lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$Monthly.Income)))$r.squared
summary(lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$Debt.To.Income.Ratio)))$r.squared
summary(lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$Loan.Length)))$r.squared
summary(lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$Loan.Purpose)))$r.squared
summary(lm(cleanLoansData$Interest.Rate ~ as.numeric(cleanLoansData$Amount.Requested)))$r.squared

# Interest Rate vs. Loan Length analysis
IRLL.lm <- lm(cleanLoansData$Interest.Rate ~ as.factor(cleanLoansData$Loan.Length))
IRLL.rsquared <- summary(lm(cleanLoansData$Interest.Rate ~ cleanLoansData$Loan.Length))$r.squared
IRLL.confint <- confint(IRLL.lm)

par(mfrow=c(1,2))
plot(cleanLoansData$Interest.Rate ~ as.factor(cleanLoansData$Loan.Length), varwidth=TRUE)

# Interest Rate vs. Amount Requested analysis
IRAR.lm <- lm(cleanLoansData$Interest.Rate ~ cleanLoansData$Amount.Requested)
IRAR.rsquared <- summary(lm(cleanLoansData$Interest.Rate ~ cleanLoansData$Amount.Requested))$r.squared
IRAR.confint <- confint(IRAR.lm)

smoothScatter(cleanLoansData$Amount.Requested, cleanLoansData$Interest.Rate, cex=5, col="blue")
lines(cleanLoansData$Amount.Requested, IRAR.lm$fitted.values, lwd=5, col="red")