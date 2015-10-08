neartab = read.csv("data/nearTableCleaned.csv")

# attach nearest neighbors to each row
# plot(density(neartab$NEAR_DIST))

neighborThresh = 500
nearNeighbors = neartab[neartab$NEAR_DIST < neighborThresh, 1:2]

getNeighborBehavior = 
  function(focal, q, neighbors = nearNeighbors, df = d)
  {
    neighbs = unique(neighbors$survey_id_near[neighbors$survey_id_in == focal])
    if(length(neighbs) == 0) 
      return(NA)
    neighbsDo = df[df$id %in% neighbs, q]
    sum(neighbsDo) / length(neighbsDo)
  }

dvs = names(d)[grep("^f2.1", names(d))]
neighborDS = 
  lapply(dvs, function(dv) {
    sapply(d$id, function(who)
      getNeighborBehavior(focal = who, q = dv)
    )
  })
neighborDS[[length(neighborDS) + 1]] = d$id
names(neighborDS) = c(paste0("neighbors_", dvs), "id")
neighDF = as.data.frame(do.call(cbind, neighborDS))
d = merge(d, neighDF, by = "id")
