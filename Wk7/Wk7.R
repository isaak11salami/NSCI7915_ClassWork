#Start by importing the data files
library(readr)
carnivore<- read_csv("Wk7/data/raw/carnivore_matrix.csv")
View(carnivore)


N=read.csv("carnivore_matrix.csv",row.names=1)
getwd()
setwd("C:/Users/isaak/OneDrive/Documents/Uni/2025/Session 1/Scientific Analysis_NSCI7915/WorkshopsRepo/Wk7/data/raw")
N=read.csv("carnivore_matrix.csv",row.names=1)
N

#Let's do a cluster analysis and get a dendrogram
d<-dist(N)
plot(hclust(d),cex=0.5,ann=F)
plot(hclust(d,method="average"),cex=0.5,ann=F)

#Now let's do a different cluster analysis method called vegdist
install.packages("vegan")
library(vegan)

d=vegdist(N)
plot(hclust(d),cex=0.5,ann=F)
plot(hclust(d,method="average"),cex=0.5,ann=F)

#Our dendrogram structure has changed! This shows that HOW you set up 
#your dendrogram model really matters

#Now, let's do a PCoA

k=cmdscale(d)
plot(k,cex=0)
text(k,labels=rownames(N),cex=0.5)
#Awesome, we have some visible structure! Different species
#are clustered according to their region of origin

#Now, let's do it wrong. Let's plot "species" and "site" simultaneously
cc=cca(N)$CA
#cc$u will give us the coordinates for the species
plot(cc$u,cex=0)
text(cc$u,labels=rownames(N),cex=0.5)

#Okay. What are the two species sitting atop each other on the far left. We can
#do this by sorting the species names, which is column 1 of object u. Then, look at
#first two species listed
sort(cc$u[,1])
#These two species are Leopardus guina and Tremarctos ornatus. These two species
#are very common in Sth America, and so have much higher counts than the other species.
#CA recognises them as outliers, and so throws them out of the main group. This is an
#issue with CA, which is why we don't use it!
