x <- read.table("C:\\Users/pietr/Documents/dnp/gnomad/1.tsv", skip = 1)
#col.names =c("locus","refs","alts","distance","snp1","snp2","ac1","ac2","ac_mnv","ac1_adj","ac2_adj","ac_mnv_adj")

z <- subset(x, x$V10!=1 & x$V11!=1) #elim. rari
z$V13 <- z$V10-z$V11
z$V14 <- (z$V10-z$V11)/z$V10

hist(z$V13)
hist(z$V14)

zz <- subset(z, z[,13]>=-10 & z[,13]<=10)
hist(zz[,13], xlab = "ac1-ac2")
hist(zz[,14], xlab = "ac1-ac2 %")
table(zz[,13])

zz <- subset(z, z[,13]>=-1 & z[,13]<=1)

finestre <- hist(zz[,10], xlab = "ac1-ac2 %", breaks = c(min(zz[,10]),1885,9223,22194,29486,max(zz[,10])))
finestre

------------------------------------------------------------

primo_subset <- subset(zz, zz[,10]>1885 & zz[,10]<9223) #8847
secondo_subset <- subset(zz, zz[,10]>9223 & zz[,10]<22194) #3696
terzo_subset <- subset(zz, zz[,10]>22194 & zz[,10]<29486) #873

primo_e_terzo <- rbind(primo_subset,terzo_subset)

