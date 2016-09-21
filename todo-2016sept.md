# To do

- Define WUI: Which of the 151k structures are included?
	- Survey definition (census tracts intersecting with a 1/2 mile buffer surrounding the NF)
	- SILVIS definition (interface & intermix, based on housing density and vegetation)
	- 1.5 mile national forest buffer
	- Combination
- Choose density estimation method
	- Appropriate scale/kernal
	- Or use political definition e.g. census blocks

- Confirm model strategy
	- Updating rules will be theory-defined. Something like DS behaviors decay stochastically at some rate, and at each timestep there is a probability of increasing number of behaviors, which is a function of 
		- effectiveness and risk beliefs
		- town and housing density
		- experience with fire and perhaps experience with DS behavior (is it stopping fire in the model run). These could affect the probability of increasing DS behavior directly, or could affect beliefs about risk and effectiveness
	- Agent instantiation: We need number of DS behaviors at each structure that respects the relationship of geographic variables (town and density) to DS tendency, as well as the relatioship between psychological variables and DS tendency. 
		- Model DS behavior as function of all four
		- For each structure, we know geographic variables. For psych variables, draw from a multivariate normal distribution defined by the survey data. I don't love that step, but don't see a way around it. It requires an assumption that risk, effectiveness, and the correlation between the two, and their relationships to housing density, are independent of town. Or modeling them as a function of town, but that kind of two-step model seems goofy. I explore the assumption of independence across town in exploreRiskEffectivenessDensityCityCorrleations.R; it looks close enough for me to be okay with it, but only slightly. 

- Execute the above agent instantiation

- Map the predicted and empirical structures with DS behavior

- Writing
	- Broad justification
	- Background on drivers of DS behavior, especially the variables in our model
	- Methods and results for drawing from predictor and parameter distributions to generate population
	- Discussion
		- Substance -- do model results comport with previous findings? Anything novel or interesting?
		- Modeling strategy -- we offer a new way to generate an empirically grounded population of agents, blah blah
		- Where we're headed (ABM updating rules, coupled model)

- References organized: Zotero shared library?
