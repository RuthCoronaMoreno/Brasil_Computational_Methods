# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Topic: From taxonomical to functional and phylogenetic diversity in R
# --------------------------------------------------#

library(vegan)
library(cluster)
library(FD)
library(SYNCSA)
library(taxize)
library(dplyr)


comm <- read.csv("data/raw/cestes/comm.csv")
traits <- read.csv("data/raw/cestes/traits.csv")
splist <- read.csv("data/raw/cestes/splist.csv")

head(comm)#[,1:6]
head(traits)#[,1:6]

rownames(comm)[1:6]

#Change arrow names using elements of a column
rownames(comm) <- paste0("Site", comm[,1])
comm <- comm[,-1]
head(comm)[,1:6]



head(traits)[,1:6]

#Transform column traits$Sp into the rownames of the dataframe.
rownames(traits) <- paste0("Sp", traits[,1])
traits <- traits[,-1]
head(traits)[,1:6]


#############################
#-----Species richness-------
#############################
richness <- vegan::specnumber(comm) # Number of species in each site

shannon <- vegan::diversity(comm) #Shannon index
simpson <- vegan::diversity(comm, index = "simpson")


gow <- cluster::daisy(traits, metric = "gower")
gow2 <- FD::gowdis(traits)
#implementations in R vary and the literature reports extensions and modifications
identical(gow, gow2) #not the same but why?

class(gow)
class(gow2)

plot(gow, gow2, asp = 1) #same values


##########################################
#Rao’s quadratic entropy calculations in R
##########################################
#install.packages(SYNCSA)
tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)
#plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
#abline(a = 0, b = 1)

#---Calculating FD indices with package PD----
#install.packages("FD")
#we can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)
#the returned object has Villéger's indices and Rao calculation
names(FuncDiv1)
#We can also do the calculation using the traits matrix directly
FuncDiv <- dbFD(x = traits, a = comm, messages = F)





##################################################################
# -------Philogenetic diversity----------
##################################################################

classification_data <- classification(splist$TaxonName, db="ncbi") #required(taxize)
str(classification)
length(classification_data)

classification_data$`Arisarum vulgare`
classification_data[[1]]
classification_data[[4]]

tible_ex <- classification_data[[1]] %>% #required(dplyr)
  filter(rank == "family") %>%
  select(name) %>%
  pull()

extract_family <- function(x){
  if(!is.null(dim(x))){
    y <- x %>% #required(dplyr)
      filter(rank == "family") %>%
      #select(name) %>%
      pull(name) #it will give you a vector instead of df
    return(y)
  }

}

families <- c()
for(i in 1:length(classification_data)){
  f <- extract_family(classification_data[[i]])
  if (length(f)>0) families[i] <- f
}
families
