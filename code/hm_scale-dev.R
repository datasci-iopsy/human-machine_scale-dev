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
readFile = readLines("../data/dat_cln.csv")
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
shhh(GGally)

#list containing construct item correlations
corrList = list()

corrList[["pearson"]] = lapply(datList, stats::cor)
corrList[["poly"]] = lapply(datList, function(r) psych::polychoric(r)$rho)

corrList[["pearson"]][["all_vars"]] = cor(
  cbind.data.frame(datList[[1]], datList[[2]], datList[[3]])
  )

corrList[["poly"]][["all_vars"]] = polychoric(
  cbind.data.frame(datList[[1]], datList[[2]], datList[[3]])
  )$rho

# plot function for correlations
corrPlots = list()

corrPlots[["pearson"]] = lapply(
  corrList[["pearson"]], 
  function(df) {
    ggcorr(
      data = NULL,
      cor_matrix = df,
      #method = c("pairwise", "pearson"),
      size = 3,
      hjust = .75,
      nbreaks = 7,
      palette = "RdYlBu",
      label = TRUE, 
      label_color = "black",
      digits = 2,
      #label_alpha = .3, 
      label_round = 2, 
      label_size = 2
    ) + 
      theme(legend.position = "none")
  }
)

corrPlots[["poly"]] = lapply(
  corrList[["poly"]], 
  function(df) {
    ggcorr(
      data = NULL,
      cor_matrix = df,
      #method = c("pairwise", "pearson"),
      size = 3,
      hjust = .75,
      nbreaks = 7,
      palette = "RdYlBu",
      label = TRUE, 
      label_color = "black",
      digits = 2,
      #label_alpha = .3, 
      label_round = 2, 
      label_size = 2
    ) + 
      theme(legend.position = "none")
  }
)

# Exploratory Factor Analysis

#parallel analysis of "hum" construct items
fa.parallel(datList[["hum"]], fm = "wls", fa = "both", 
            main = "Scree Plot of Human-Machine Items", 
            ylabel = "Eigenvalues of Factors")
  #suggestions: fa = 4 factors & pc = 3..."theoreticized" a 1-factor scale

#function to run multiple efa models
efa_mods_fun = function(x, n_models = NULL, ...){
    
    if (!is.data.frame(x))
        stop("x must be a dataframe with numeric values")
    
    efa_models = list()
    
    for (i in seq(n_models)){
        efa_models[[i]] = fa(x, nfactors = i, rotate = "oblimin", fm = "wls", 
                             cor = "poly", max.iter = 5000)
    }
    return(efa_models)
}

#run series of models 
modsPolyWLS = efa_mods_fun(datList[["hum"]], n_models = 5) #up to 5-factor solutions

modsFit = list(
  hum = round(
    data.frame(
      a = c(modsPolyWLS[[1]]$STATISTIC, modsPolyWLS[[2]]$STATISTIC, 
            modsPolyWLS[[3]]$STATISTIC, modsPolyWLS[[4]]$STATISTIC, 
            modsPolyWLS[[5]]$STATISTIC), 
      
      b = c(modsPolyWLS[[1]]$TLI, modsPolyWLS[[2]]$TLI, 
            modsPolyWLS[[3]]$TLI, modsPolyWLS[[4]]$TLI, 
            modsPolyWLS[[5]]$TLI), 
      
      c = c(modsPolyWLS[[1]]$BIC, modsPolyWLS[[2]]$BIC, 
            modsPolyWLS[[3]]$BIC, modsPolyWLS[[4]]$BIC, 
            modsPolyWLS[[5]]$BIC), 
      
      d = c(modsPolyWLS[[1]]$RMSEA[1], modsPolyWLS[[2]]$RMSEA[1], 
            modsPolyWLS[[3]]$RMSEA[1], modsPolyWLS[[4]]$RMSEA[1], 
            modsPolyWLS[[5]]$RMSEA[1]), 
      
      e = c(modsPolyWLS[[1]]$Vaccounted[2], 
            modsPolyWLS[[2]]$Vaccounted[3, 2], 
            modsPolyWLS[[3]]$Vaccounted[3, 3],
            modsPolyWLS[[4]]$Vaccounted[3, 4], 
            modsPolyWLS[[5]]$Vaccounted[3, 5]), 
      
      row.names = c('Model 1', 'Model 2', 'Model 3', 'Model 4', 'Model 5')
      ), 
    2))
modsFit

library(kableExtra) #masks dplyr::group_rows
library(tibble)

modsFit[["hum"]] %>% 
  rownames_to_column() %>% 
  rename(
    'Model Solution(s)' = rowname, 
    'X\u00B2' = a,
    'TLI' = b, 
    'BIC' = c, 
    'RMSEA' = d, 
    'Var Explained' = e
  ) %>% 
  mutate(
    'Model Solution(s)' = c('1 Factor', '2 Factors', '3 Factors', 
                            '4 Factors', '5 Factors')
  ) %>% 
  kable('html', 
        booktabs = TRUE, 
        caption = 'EFA Model Fit Indices') %>% 
  kable_styling(bootstrap_options = c('striped', 'HOLD_position'),
                full_width = FALSE, 
                position = 'center') %>% 
  column_spec(1, width = '8cm') %>% 
  pack_rows(
    index = c('Human-Machine Preference' = 5),
    latex_gap_space = '.70em')

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
