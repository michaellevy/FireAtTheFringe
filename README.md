# Fire at the Fringe

We are a [SESYNC Graduate Pursuit](https://www.sesync.org/opportunities/grad-themes-2014) team building a coupled model of fire, vegetation, and behavior dynamics in the San Diego wildland-urban interface. This repository holds a statistical model conditioned on survey data that is used to generate agents in the model, and the manuscript describing that model and process. There is also a demonstration coupled fire/agent-based model developed in NetLogo and some structural equation modeling. @mvsana is leading the development of the fire simulation model, which is in [this repository](https://github.com/mvsaha/fire_sim).

There is a lot of exploratory code here. The pipeline is:

- refineData.csv -> dataCleaning.R -> cleanData.csv
- cleanData.csv -> imputing.R -> imputedData.RDS 
~~- mergedPoints.RDS (from arcGIS) -> densityEstimating.R -> structuresWithDensity.RDS~~
- imputedData.RDS + mergedPoints.csv -> modeling.R -> modelWithHousingDensity.RDS
