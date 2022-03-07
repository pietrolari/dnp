x <- read.table("C:\\Users/pietr/Documents/dnp/gnomad/1_chr_sep", skip = 1)
#col.names =c("locus","refs","alts","distance","snp1","snp2","ac1","ac2","ac_mnv","ac1_adj","ac2_adj","ac_mnv_adj")

z <- subset(x, x$V11!=1 & x$V12!=1) #elim. rari
z$V14 <- z$V11-z$V12
z$V15 <- (z$V11-z$V12)/z$V11

#zz <- subset(z, z[,14]>=-1 & z[,14]<=1) #con differenza tra -1 e 1
zzz <- subset(z, z[,14]>=0 & z[,14]<=0) #con differenza = 0

#finestre <- hist(zz[,11], xlab = "ac1-ac2 %", breaks = c(min(zz[,11]),1885,9223,22194,29486,max(zz[,11])))
finestre <- hist(zzz[,11], xlab = "ac1-ac2 %", breaks = c(min(zzz[,11]),1885,9223,22194,29486,max(zzz[,11])))
finestre

#primo_subset <- subset(zz, zz[,11]>1885 & zz[,11]<9223)
primo_subset <- subset(zzz, zzz[,11]>1885 & zzz[,11]<9223)

#secondo_subset <- subset(zz, zz[,11]>9223 & zz[,11]<22194)

#terzo_subset <- subset(zz, zz[,11]>22194 & zz[,11]<29486)
terzo_subset <- subset(zzz, zzz[,11]>22194 & zzz[,11]<29486)
finestre3 <- hist(terzo_subset[,11], xlab = "ac1-ac2 %")

primo_e_terzo <- rbind(primo_subset,terzo_subset)


#manhattan
c22 <- c("dodgerblue2", "#E31A1C", "green4","#6A3D9A", "#FF7F00", "black", "gold1","skyblue2", "#FB9A99","palegreen2","#CAB2D6","#FDBF6F","gray70","khaki2","maroon", "orchid1", "deeppink1", "blue1", "steelblue4","darkturquoise", "green1", "yellow4") #colori
par(mfrow=c(1,2))
plot(primo_subset[,11], main="DNP Perfetti-Allele alt nella finestra (4225)", cex= 0.5, col=c22[primo_subset[,1]], ylim=c(1800,12000)) 
plot(terzo_subset[,11], main="DNP Perfetti-Allele ref nella finestra (373)", cex= 0.5, col=c22[terzo_subset[,1]], ylim=c(22000,30000))

