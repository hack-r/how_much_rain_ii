iterations <- 5
# Matthew T. Boulanger
# University of Missouri Research Reactor

# Modifications by Jason D. Miller, hack-r.com / millerintllc.com

#Set no. of iterations
iter = iterations

#End user-defined variables
#-------------------------------->

rn <- paste( "ANID", seq(1:nrow(test)), sep = " ")
row.names(test) <- rn

#Create Vector of ANIDs
anid.list <- c(rep(row.names(test), iter))

#Log transform data
#data.log <- log10(test)

#Impute missing values
imputed <- amelia(test,p2s =2, m = iter, parallel = "snow", cl = parallel::makePSOCKcluster(20), ncpus = 20, idvars = c("Id")) #cl = cl,

#Write each imputed test to files
write.amelia(obj=imputed, file.stem = "imputed_")

#Create list of the imputed files
imputed.files = list.files(pattern = 'imputed_*')

#Read in all imputed files
imputed.list = lapply(imputed.files, read.csv, header=TRUE, row.names = "X")

#Compile all imputed data into one big data frame
imputed.ppm <- ldply(imputed.list, data.frame, row.names=NULL)

#Set variable y, which is the base-10 log concentration of each specimen
#y = big.list

#Use variable y to convert data back to ppm
#imputed.ppm <- 10^y

#Convert imputed data to data frame
#imputed.ppm <- data.frame(imputed.ppm, row.names=NULL)

#Insert ANIDs into imputed data data frame
imputed.ppm$ANID <- anid.list

#Calculate mean of all imputations based on ANID
final.imputed.ppm <- aggregate(imputed.ppm[,!(colnames(imputed.ppm) == "ANID")],list(imputed.ppm$ANID), mean)

return(final.imputed.ppm)
