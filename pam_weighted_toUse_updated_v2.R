library("TraMineR")
library("cluster")
library("WeightedCluster")
library("dplyr")
library("reshape2")
library("alluvial")
setwd("/Users/nikospatias/Desktop/work_np/PhD_Data/paper1/new_work/seq_analysis")
getwd()

# Read in the file including the cluster sequences
db <- read.csv("clusters_all.csv")
head(db)
nrow(db)


# If we want to use weights based on the frequency of its sequence
# Create a new column that keeps only the sequences
db$sequence <- paste (db$clusters71,db$clusters81, db$clusters91, db$clusters01, db$clusters11, sep="-")
# count the unique values
db1 <- aggregate(data.frame(count = db$sequence), list(value = db$sequence), length)
head(db1)
nrow(db1)



# Create a sequence object
# specify the colours to be consistent with the maps
seq.cl <- seqdef(db1$value, weights = db1$count, cnames = c("1971", "1981", "1991",
                                                            "2001", "2011"), cpal =c("Affluent" = "darkgreen",
                                                                                   "Blues collar families" = "midnightblue", 
                                                                                   "Families in council rent" = "#e70ca2", 
                                                                                   "Mixed workers suburban" = "goldenrod4", 
                                                                                   "Multicultural urban" = "#ed7b45", 
                                                                                   "Older striving" = "steelblue1", 
                                                                                   "Struggling" = "#da142b",
                                                                                   "Thriving suburban" = "burlywood"))
seq.cl

# Calculate transition rates
couts_t <- seqsubm(seq.cl, method = "TRATE", time.varying = TRUE)
tr_rates <- round(couts_t, 2)
tr_rates
# write.csv(tr_rates, "tr_rates.csv") 

# Calculate the unique distamce matrix
seq.OM <- seqdist(seq.cl, method = "DHD", sm = couts_t)
head(seq.OM[1:10,1:10])


# hierarchical clustering to initialise the medoids
wardCluster <- hclust(as.dist(seq.OM), method="ward.D2", members=db1$count)
# PAM clustering
pamwardclust <- wcKMedoids(seq.OM, k=7, weights=db1$count, initialclust=wardCluster)
unique(pamwardclust$clustering)
pamwardclust$stats
pamwardclust$ASW


# give names to clusters
pamwardclust$clustering <- factor(pamwardclust$clustering, levels=c(1, 
                                       327, 
                                       366, 
                                       510, 
                                       687, 
                                       785, 
                                       939),
                                  labels=c("Stable affluent",
                               "Ageing manual labour",
                               "Increasing socioeconomic diversity",
                               "Increasing struggling home-owners",
                               "Stable multicultural urban",
                               "Rejuvenating",
                               "Upwarding thriving"))

db1$cl <- pamwardclust$clustering


head(db1)

# Create a dataframe that includes cluster number, sequences and counts
db$sequence <- paste (db$clusters71,db$clusters81, db$clusters91, db$clusters01, db$clusters11, sep="-")

db_cl2 <- merge(x = db, y = db1, by.x = "sequence",by.y = "value", all.x = TRUE)

head(db_cl2)


#create a table with counts of grids by cluster by region
cl_count <- db_cl2 %>%
  group_by(Region) %>% 
  count(cl)

# I reshape the table form long to wide format
cl_count.reshape <- dcast(cl_count, Region~cl,sum)
head(cl_count.reshape)




### GRAPHS for sequence clusters

# for all the sequence clusters in one graph
# these are used for the paper
# calculate silouette score to use later in graphs
sil <- wcSilhouetteObs(seq.OM, pamwardclust$clustering, weights=db1$count, measure="ASWw")
# seqIplot(seq.cl, group=pamwardclust$clustering, sortv=sil)
# seqfplot(seq.cl, group=pamwardclust$clustering,border=NA, sortv=sil)
# seqdplot(seq.cl, group=pamwardclust$clustering, border=NA)
# seqmtplot(seq.cl, group = pamwardclust$clustering, title = "Mean time", border=NA)

seqlegend(seq.cl, ncol = 4)
mypath <- file.path("/Users/nikospatias/Desktop/work_np/PhD_Data/paper1/new_work/seq_analysis/",
                    paste("legend", ".jpg", sep = ""))
jpeg(file=mypath, width = 2200, height = 400, quality = 10)
seqlegend(seq.cl, ncol = 4, cex = 3.2)
dev.off()


for (i in unique(pamwardclust$clustering)) {
  mypath <- file.path("/Users/nikospatias/Desktop/work_np/PhD_Data/paper1/new_work/seq_analysis/",
                      paste("seqIplot3_", i, ".jpg", sep = ""))
  
  jpeg(file=mypath, width = 400, height = 900)
  seqIplot(seq.cl[pamwardclust$clustering == i,], border =NA, with.legend = FALSE, sortv=sil, cex.axis=1.5, cex.lab=1.5)
  
  dev.off()

} 

for (i in unique(pamwardclust$clustering)) {
  mypath <- file.path("/Users/nikospatias/Desktop/PhD_Data/paper1/new_work/seq_analysis",
                      paste("seqdplot_", i, ".jpg", sep = ""))
  
  jpeg(file=mypath, width = 400, height = 700)
  seqdplot(seq.cl[pamwardclust$clustering == i,], border =NA, with.legend = FALSE, sortv=sil, cex.axis=1.5, cex.lab=1.5)
  
  dev.off()
} 

for (i in unique(pamwardclust$clustering)) {
  mypath <- file.path("/Users/nikospatias/Desktop/PhD_Data/paper1/new_work/seq_analysis",
                      paste("seqmtplot_", i, ".jpg", sep = ""))
  
  jpeg(file=mypath, width = 1200, height = 700)
  seqmtplot(seq.cl[pamwardclust$clustering == i,], border =NA, with.legend = FALSE, sortv=sil)
  
  dev.off()
} 

#meantc4 <- by(LSAY.seq, LSAY$bf.pam4, seqmeant)
#meant<-matrix(as.numeric(unlist(meantc4)), nrow = 3, ncol = 4, byrow = F)
#colors <- c(“#7570B3”, “#D95F02" , “#1B9E77”)
#             clusters <- c(“C1",“C2”,“C3",“C4”)
#            areas <- c(“UA”,“RC”,“RA”)
#             png(file=“/Users/Franciscorowe 1/Dropbox/Francisco/research/in_progress/PSP/2017/Results/Assesing cluster solutions/RAs/Return/TimeSpent_ReturnMig_4C.png”, width = 12, height = 12, units=“cm”, res=600, pointsize=6.5)
#             barplot(meant, beside=F, horiz=TRUE, names.arg = clust.labels, border = NA, xlab = “No. of Years”, ps=6.5, col = colors)
#             dev.off()



# mena time spent graph as a barplot
m <- seqmeant(seq.cl)
m2 <- by(seq.cl, pamwardclust$clustering, seqmeant)
meant <- matrix(as.numeric(unlist(m2)), nrow = 8, ncol = 7, byrow = F)
#meant_df <- as.data.frame(meant)



colnames(meant) <- c("Stable affluent", "Ageing manual labour", 
                     "Increasing socioeconomic diversity", "Increasing struggling home-owners",
                     "Stable multicultural urban","Rejuvenating", "Upwarding thriving")
row.names(meant) <- c("Affluent", "Blue collar families", 
                      "Families in council rent", "Mixed workers suburban",
                      "Multicultural urban","Older striving","Struggling",
                      "Thriving suburban")



colors <- c("darkgreen", "midnightblue", "#e70ca2", "goldenrod4", "#ed7b45", "steelblue1", "#da142b", "burlywood")

mypath <- file.path("/Users/nikospatias/Desktop/work_np/PhD_Data/paper1/new_work/seq_analysis/",
                    paste("barplot_meantime2", ".jpg", sep = ""))

jpeg(file=mypath, width = 400, height = 700)
barplot(meant, beside=F, horiz=TRUE, names.arg = unique(pamwardclust$clustering), border = NA, xlab = 'No. of Years', ps=6.5,col = colors)

dev.off()


meant
mean(meant[,5])

