# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Topic: Introduction to biological diversity analyses
# --------------------------------------------------#

#Biodiversity is a complex multifaceted concept that includes scales in space and time, and entities s.a. species, trait and evolutionary units.
#Diveristy indexes have a richness and evenness components
#Must increases when more new elements are added to the sample
#Some indices are proportional to sample size: a mathematical artifact
#Must be maximal wen all the elements have the same frequency (eveness)

#Data
comm <- read.csv("data/raw/cestes/comm.csv")
dim(comm)
head(comm[,1:6])

#----Which are the 5 most abundant species overall in the dataset?----
sort(colSums(comm), decreasing=TRUE)[1:6]

#----How many species are there in each site? (Richness)-----
comm_binary=comm[,-1]
comm_binary[comm_binary > 0] <- 1
species_site=rowSums(comm_binary)

#----Which are the most abundant species in each site?-----
apply(X=comm[,-1], MARGIN=1, FUN=which.max)

#----Create code to calculate Shannon and Simpson diversity in comm dataset (leave in notes!)----
P_matrix=comm/rowSums(comm) #pi computation
P_matrix_ln=t(log(P_matrix)) #ln(pi) computation
P_matrix_ln[P_matrix_ln==-Inf]<-0 #Changing -Inf values to 0

#Convert dataframes into matrix
P_matrix <- data.matrix(P_matrix)
P_matrix_ln <- data.matrix(P_matrix_ln)

#Shannon diversity index by product of matrix
H_matrix <- -diag(P_matrix %*% P_matrix_ln)
H_matrix

#Simpson diversity index
S_matrix=1-rowSums(P_matrix*P_matrix)

#Inverse Simpson
Inv_S=1/rowSums(P_matrix*P_matrix)
