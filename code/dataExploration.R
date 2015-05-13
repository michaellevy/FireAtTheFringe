setwd(file.path('~', 'GitHub', 'FireAtTheFringe'))  # Set working directory
d = read.csv('data/cleanData.csv')  # Read the data in
library(ggplot2)
library(reshape2)
library(dplyr)

# Names of defensible space behavior questions
dsBehavior = names(d)[grepl("f2.1", names(d))]
melt(d[dsBehavior]) %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_violin()
# Most people their gutters; other DS behaviors are rarer
d$sumDSBehavior = apply(d[dsBehaviorPresent], 1, 
                           function(x) sum(x, na.rm = TRUE))
ggplot(d, aes(x = sumDSBehavior)) + geom_histogram()

dsEffectiveness = names(d)[grepl("^f1", names(d))]
melt(d[dsEffectiveness]) %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_violin()
# There's quite a bit of variance in how effective different DS behaviors are.
d$medianDSEffectiveness = apply(d[dsEffectiveness], 1, 
                           function(x) median(x, na.rm = TRUE))

mosaicplot(
  table(
    data.frame(
      medianEffectiveness = floor(d$medianDSEffectiveness), 
      sumDSBehavior = d$sumDSBehavior)),
  main = "Most people perceive defensible space behavior to be quite effective;
  even among those who think it's extremely effective, many haven't implemented much."
)

# Breakdown actual behavior vs. perceived effectiveness by individual actions
t1 = melt(d[dsBehavior], value.name = "behavior")
t2 = melt(d[dsEffectiveness], value.name = "effectiveness")
t = data.frame(question = substr(t1$variable, 3, 3),
               behavior = t1$behavior,
               effectiveness = t2$effectiveness)
levels(t$question) = 
  c("clean roof & gutters", "stack wood > 30' from house", 
    "non-flammable building materials", "fire resistant plants",
    "space plants > 15' apart", "prune low branches", "reduce tree density")
t$own = c("rent", "own")[d$a5]

likert5 =  c("Not all", "Slightly", "Neutral", "Quite", "Extremely")

t[complete.cases(t), ] %>%
  ggplot(aes(x = effectiveness, y = behavior, color = own)) +
    geom_point(position = "jitter", alpha = 1) + 
    facet_wrap(~question, ncol = 2) + 
    geom_smooth(method = "lm") +
    scale_y_continuous(breaks = 0:1, labels = c("no", "yes")) +
    scale_x_continuous(breaks = 1:5, labels = likert5) +
    theme_bw() +
    theme(axis.text.x=element_text(angle = -45, hjust = 0))
# Some ds behaviors renters can't implement. We may want to remove them
# from some analyses. They only make up 5% of the dataset:
table(t$own)

ggplot(d, aes(x = f5b, y = sumDSBehavior)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = 1:5, labels = likert5) +
  xlab("To what extent do you consider your own home to be at risk from wildland fire?") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))
# Greater perceived risk is not at all or slightly negatively correlated
# with defensible space behavior.
# Maybe because DS-adopters feel like they've ameliorated the risk.
# Check that by replacing their personal risk with community risk...

ggplot(d, aes(x = f5a, y = sumDSBehavior)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = 1:5, labels = likert5) +
  xlab("To what extent do you consider the community in which you live
       to be at risk from wildland fire?") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))
# Same pattern! Maybe they feel their whole community is protected. Or maybe 
# people are foolish.

# USFS is like me
fsAlike = names(d)[grepl("^g1", names(d))]
d$medianfsAlike = apply(d[fsAlike], 1, 
                                function(x) median(x, na.rm = TRUE))
ggplot(d, aes(x = medianfsAlike, y = sumDSBehavior)) +
  geom_point(position = 'jitter') +
  geom_smooth(method = 'lm')
# No obvious relationship between relating to USFS and adopting DS behavior

townTable = table(d$b3atwnct)
mainTowns = names(townTable)[townTable > 5]
# Assign "other" to towns for which we have 5 or fewer responses
d$town = as.character(rep(d$b3atwnct))
d$town[!(d$town %in% mainTowns)] = "other"

ggplot(d, aes(x = town, y = sumDSBehavior)) +
  geom_violin()
# Things are different in Alpine!


# Note some e1-Qs  (ds beliefs) are reverse coded
