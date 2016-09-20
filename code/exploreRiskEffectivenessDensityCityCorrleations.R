d2 = as.data.frame(readRDS('data/derived/dataVImodel.RDS'))
library(ggplot2)
## I think it look justifiable to define a single joint distribution of 
## effectiveness and risk and draw from it for an assignment to each agent, 
## independent of city or housing density.

cowplot::plot_grid(
  ggplot(d2, aes(x = city, y = effectiveness)) + 
    geom_violin() +
    coord_flip()
  ,
  
  ggplot(d2, aes(x = city, y = risk)) + 
    geom_violin() +
    coord_flip()
  
)

count(d2, city)
m = lm(risk ~ city, d2) 
summary(m)
# El Cajon/Lakeside really does look different here. 
# Enough different to make a separate distribution, meh I hope not.

ggplot(d2, aes(effectiveness, risk)) + 
  geom_jitter(aes(color = city), width = .5, height = .5) +
  geom_smooth(method = "lm")
# This correlations is okay. Will be captured by the MVN.

# Let's see what those look like:
ggplot(d2, aes(x = logDensity, y = risk, color = city)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~city)

ggplot(d2, aes(x = logDensity, y = effectiveness, color = city)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~city)

# Don't want to see different intercepts or slopes here:
m = lm(effectiveness ~ risk * city, d2)
summary(m)
# Overall, pretty similar.