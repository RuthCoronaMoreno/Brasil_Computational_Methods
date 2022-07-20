library(bbmle)
library(ggplot2)


cuckoo <- read.csv("data/raw/valletta_cuckoo.csv")

#Translating hypotheses into models
h1 <- glm(Beg ~ Mass, data = cuckoo,
          family = poisson(link = log))

h2 <- glm(Beg ~ Mass + Species, data = cuckoo,
          family = poisson(link = log))

h3 <- glm(Beg ~ Mass * Species, data = cuckoo,
          family = poisson(link = log))

h0 <- glm(Beg ~ 0, data = cuckoo,
          family = poisson(link = log))

summary(h3)
summary(h2)

AIC(h3)
AIC(h1)
AIC(h0)

#Using AIC to confront simultaneosly multiple hypotheses
bbmle::AICtab(h0, h1, h2, h3, base = TRUE, weights = TRUE)


# Calculating the predicted values
newdata <- expand.grid(Mass = seq(min(cuckoo$Mass), max(cuckoo$Mass), length.out = 200),
                       Species = unique(cuckoo$Species))
newdata$Beg <- predict(h3, newdata, type = 'response')


## explore ?predict.glm

p <- ggplot(mapping = aes(x = Mass, y = Beg, colour = Species)) +
  geom_point(data = cuckoo) +  geom_line(data = newdata) +
  theme_classic()

p
