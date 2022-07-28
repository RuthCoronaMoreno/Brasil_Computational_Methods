###############################################
#Introduction to biological diversity analyses
###############################################

#Data
comm <- read.csv("data/raw/cestes/comm.csv")
dim(comm)
head(comm[,1:6])

#Which are the 5 most abundant species overall in the dataset?
sort(colSums(comm), decreasing=TRUE)[1:6]

#How many species are there in each site? (Richness)
comm_binary=comm[,-1]
comm_binary[comm_binary > 0] <- 1
species_site=rowSums(comm_binary)

#Which are the most abundant species in each site?
apply(X=comm[,-1], MARGIN=1, FUN=which.max)

#Create code to calculate Shannon and Simpson diversity in comm dataset (leave in notes!)
P_matrix=comm/rowSums(comm)
P_matrix_ln=t(log(P_matrix))
P_matrix_ln[P_matrix_ln==-Inf]<-0
P_matrix %*% P_matrix_ln

