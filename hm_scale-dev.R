rm(list = ls()) #keep env cln

#it's a library, so shhh!
shhh = function(...){
    suppressWarnings(
        suppressPackageStartupMessages(base::library(...))
    )
}

#load libraries
shhh(tidyverse)
shhh(psych)

#read in the data, skip the second row
readFile = readLines("./data/dat_cln.csv")
datRaw = read.csv(textConnection(readFile[-2]), header = T, sep = ",")

#function to recode chr/fcts to nums
unfactorise <- function(x) {
      case_when(x %in% c("Strongly disagree") ~ 1,
                x %in% c("Somewhat disagree") ~ 2,
                x %in% c("Neither agree nor disagree") ~ 3,
                x %in% c("Somewhat agree") ~ 4,
                x %in% c("Strongly agree") ~ 5)
  }

#create df w/ unfactorised values
dat = data.frame(sapply(datRaw[, 1:32], unfactorise))
head(dat)

#re-score reverse-coded items (5-point Likert-type scale)
datFnl = mutate_at(dat, vars(contains("_R")), list(~ 6 - .)) #subtract 6

#list of construct dfs
datList = list(
  hum = select(datFnl, starts_with("HUM")), 
  sa = select(datFnl, starts_with("SA")), 
  ls = select(datFnl, starts_with("LS"))
  )

# Correlation Analysis
shhh(corrplot)

#list containing construct item correlations
corrList = lapply(datList, cor)

#all correlations
corrs = cor(cbind(datList[["hum"]], datList[["sa"]], datList[["ls"]]))

#corrplot only works with matrices!
corrplot(corrs, 
         method = "color", 
         type = "lower", 
         tl.col = "black", tl.srt = 15, tl.cex = .7,
         diag = T, 
         addCoef.col = "black", 
         number.cex = .42, 
         cl.cex = .8)

# Exploratory Factor Analysis

#parallel analysis of "hum" construct items
fa.parallel(datList[["hum"]], fm = "pa", fa = "both", 
            main = "Scree Plot of Human-Machine Items", 
            ylabel = "Eigenvalues of Factors")
  #suggestions: fa = 4 factors & pc = 3..."theoreticized" a 1-factor scale

#function to run multiple efa models
efa_mods_fun = function(x, n_models = NULL, ...){
    
    if (!is.data.frame(x))
        stop("x must be a dataframe with numeric values")
    
    efa_models = list()
    
    for (i in seq(n_models)){
        efa_models[[i]] = fa(x, nfactors = i, rotate = "oblimin", fm = "pa", 
                             use = "pairwise")
    }
    return(efa_models)
}

#run series of models 
mods = efa_mods_fun(datList[["hum"]], n_models = 5) #up to 5-factor solutions

#factor loadings of each model - loop
# loadings_l = list()
for (i in 1:length(mods)) { 
  mods[[i]]$loadings %>% 
    print(sort = F, cutoff = .4)
}

#recursion...
lapply(mods, function(x)
  print(x[["loadings"]][])
  )

#scale was theorized to be 1 factor...rm items with low loadings (i.e., < .4)
humNew = datList[["hum"]][, -c(4, 5, 12)]

#add new "hum" items to list of dfs
datList = append(datList, list("humNew" = humNew), after = 1)

#run EFA w/ 1-factor solution using df w/ redacted items
mod_humNew = efa_mods_fun(datList[["humNew"]], n_models = 1)
mod_humNew[[1]]$loadings #all loadings >= .40!

# Data Viz

#diagram of loadings
fa.diagram(mod_humNew[[1]], 
           main = "EFA 1-Factor Solution")

#df of loadings
loadings = as.data.frame(mod_humNew[[1]]$loadings[]) %>%
    round(3) %>%
    #rename(Factors = 1) %>%
    add_column(.before = 1, Item = names(datList[["humNew"]])) %>%
    mutate(Item = as.factor(Item))

#viz of factor loadings - all loadings are >= .4!
gather(loadings, key = "Factor", value = "Loadings", -Item) %>% 
  ggplot(aes(fct_inorder(Item), abs(Loadings), fill = Factor)) + 
    geom_bar(stat = "identity", width = .8, color = "gray") +
    coord_flip() +
    labs(
      title = "Loading Comparisons", 
      subtitle = "1-Factor Solution", 
      x = "Item",
      y = "Loading Strength"
      ) +
    theme_gray(base_size = 10) +
    theme(legend.position = "right")

# Reliability Analysis

#compute coefficient alphas
alphas = lapply(datList, psych::alpha)

#cronbach alpha estimates for each construct!
alphas[["hum"]]$total[1:2] #human-machine alpha: .83
alphas[["humNew"]]$total[1:2] #human-machine (dropped) alpha: .85!
alphas[["ls"]]$total[1:2] #science alpha: .88
alphas[["sa"]]$total[1:2] #satisfaction alpha: .89
