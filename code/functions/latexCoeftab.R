latexCoeftab = function(model) {
  tab = precis(model, depth = 2, prob = .95)@output
  rownames(tab)[1:7] = paste0('\\alpha_{', sort(unique(d2$city)))
  betas = grepl("^b", rownames(tab))
  rownames(tab)[betas] = substr(rownames(tab)[betas], 1, 2)
  rownames(tab) = stringr::str_replace(rownames(tab), "^b", "\\\\beta_{")
  rownames(tab) = stringr::str_replace(rownames(tab), "sigma_cities", "\\\\sigma")
  noSubs = !grepl("\\{", rownames(tab))
  rownames(tab)[rownames(tab) == "a"] = "\\alpha"
  rownames(tab)[!noSubs] = paste0(rownames(tab)[!noSubs], "}")
  rownames(tab) = paste0("$", rownames(tab), "$")
  colnames(tab)[2:4] = c("SD", "Lower 95% CI", "Upper 95% CI")
  mdTab = round(tab[c(10:nrow(tab), 9, 8, 1:7), 1:4], 2)
  # separate = mdTab[1, ]
  # rownames(separate) = "Varying Intercepts"
  # separate[, 1:4] = rep("", 4)
  # mdTab = rbind(
  #   tab[1:6, ],
  #   separate,
  #   tab[7:13, ]
  # )
  # Copy and paste this into markdown writeup:
  return(coefTab = knitr::kable(mdTab, digits = 2, align = rep('c', 4)))
}