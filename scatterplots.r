rents.raw <- read.csv("../Downloads/Neighborhood_MedianRentalPricePerSqft_Sfr.csv", header=TRUE)
prices.raw <- read.csv("../Downloads/Neighborhood_MedianListingPricePerSqft_AllHomes.csv", header=TRUE)
pce <- read.csv("../Downloads/PCEPILFE.csv", header=TRUE)
pce$month <- as.Date(pce$DATE)
data.raw <- merge(rents.raw, prices.raw, all=TRUE)

while (nrow(data.raw)>1) {
    current.region = data.raw[1:2,]
    if (current.region$RegionName[1]==current.region$RegionName[2] && current.region$State[1]==current.region$State[2] && current.region$City[1]==current.region$City[2] && current.region$Metro[1]==current.region$Metro[2] && current.region$CountyName[1]==current.region$CountyName[2]) {
        region.data <- t(subset(current.region, select=-c(RegionName, City, State, Metro, CountyName, SizeRank)))
        region.df <- as.data.frame(region.data)
        colnames(region.df) <- c("rent", "price")
        if (sum(!is.na(region.df$rent))>36 && sum(!is.na(region.df$price))>36) {
            region.df$month <- substring(rownames(region.df), 2)
            substring(region.df$month, 5) <- "-"
            region.df$month <- as.Date(paste(region.df$month, "-01", sep=""))
            region.df <- merge(region.df, pce, all.x=TRUE)
            region.df$adj.PCE <- region.df$PCEPILFE/100
            region.df$real.rent <- region.df$rent*region.df$adj.PCE
            region.df$real.price <- region.df$price*region.df$adj.PCE

            library(seasonal)
            deseas.rent <- seas(ts(region.df$real.rent, start=c(2010, 1), end=c(2019, 9), frequency=12), regression.aictest=NULL)
            deseas.rent.df <- subset(as.data.frame(deseas.rent), select=c(date, final))
            colnames(deseas.rent.df)[colnames(deseas.rent.df)=="date"] <- "month"
            colnames(deseas.rent.df)[colnames(deseas.rent.df)=="final"] <- "adj.rent"
            region.df <- merge(region.df, deseas.rent.df, all.x=TRUE)

            deseas.price <- seas(ts(region.df$real.price, start=c(2010, 1), end=c(2019, 9), frequency=12), regression.aictest=NULL)
            deseas.price.df <- subset(as.data.frame(deseas.price), select=c(date, final))
            colnames(deseas.price.df)[colnames(deseas.price.df)=="date"] <- "month"
            colnames(deseas.price.df)[colnames(deseas.price.df)=="final"] <- "adj.price"
            region.df <- merge(region.df, deseas.price.df, all.x=TRUE)

            scatterdata <- subset(region.df, select=c(adj.rent, adj.price, adj.PCE))
            outpath <- file.path("rentvsprice/plots/scatter", paste(current.region$RegionName[1], current.region$State[1], ".png", sep=""))

            png(file=outpath)
            pairs(scatterdata, main=paste(current.region$RegionName[1], current.region$State[1], sep=", "))
            dev.off()

            data.raw <- data.raw[-c(1, 2),]
        } else {
            data.raw <- data.raw[-c(1, 2),]
        }
    } else {
        data.raw <- data.raw[-c(1),]
    }
}