rm(list = ls()) #keep env cln

#import custom functions
source("funs.R")

#load libraries
library(tidyverse)
library(psych)

#read in the data, skip the second row
readFile = readLines("../data/dat_cln.csv")
datRaw = read.csv(textConnection(readFile[-2]), header = T, sep = ",")

#call 'unfactorise' function to recode chr/fcts to nums
dat = data.frame(sapply(datRaw[, 1:32], unfactorise))

#re-score reverse-coded items (5-point Likert-type scale)
datVars = mutate_at(dat, vars(contains("_R")), list(~ 6 - .)) #subtract 6

#list of construct dfs
datList = list(
  hum = select(datVars, starts_with("HUM")), 
  sa = select(datVars, starts_with("SA")), 
  ls = select(datVars, starts_with("LS"))
  #demo = select(datRaw, .data$POL:.data$EDU)
  )

#keep env cln
rm(datRaw, dat, readFile)

#EDA - Viz
# Correlation Analysis
library(GGally)

#distributions
ggpairs(
  select_at(datVars, vars(matches("HUM"))),
  upper = "blank",
  diag = list(continuous = wrap("densityDiag")),
  lower = list(continuous = wrap(ggally_smooth_lm)),
  title = "Pairs Plot of Human-Machine Items"
  )

#list containing construct item correlations
corrList = list()

corrList[["spearman"]] = lapply(datList, 
                                function(r) stats::cor(r, method = "spearman"))
corrList[["poly"]] = lapply(datList, function(r) psych::polychoric(r)$rho)

corrList[["spearman"]][["all_vars"]] = cor(
  cbind.data.frame(datList[[1]], datList[[2]], datList[[3]]), 
  method = "spearman")

corrList[["poly"]][["all_vars"]] = polychoric(
  cbind.data.frame(datList[[1]], datList[[2]], datList[[3]])
  )$rho

# plot function for correlations
corrPlots = list()

corrPlots[["spearman"]] = lapply(
  corrList[["spearman"]], 
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
    })

# # export pearson corr plots
# for (i in 1:length(names(corrPlots[["spearman"]]))) {
# 
#   corrPlots[["spearman"]][i]
#   ggsave(corrPlots[["spearman"]][[i]],
#          file = paste0("../figs/corrPlots-spearman/",
#                        names(corrPlots[["spearman"]][i]),
#                        ".png")
#          )}

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
    })

# # export poly corr plots
# for (i in 1:length(names(corrPlots[["poly"]]))) {
#   
#   corrPlots[["poly"]][i]
#   ggsave(corrPlots[["poly"]][[i]],
#          file = paste0("../figs/corrPlots-poly/",
#                        names(corrPlots[["poly"]][i]),
#                        ".png")
#   )}

# Exploratory Factor Analysis

#parallel analysis of "hum" construct items
fa.parallel(corrList[["poly"]][["hum"]], 
            n.obs = nrow(datList[["hum"]]),
            fm = "wls", 
            fa = "fa", 
            main = "Scree Plot of Items - Poly & WLS", 
            ylabel = "Eigenvalues of Factors")
  #suggestions: fa = 4 factors & pc = 3..."theoreticized" a 1-factor scale

#function to run multiple efa models
efa_mods_fun = function(r, n_models = NULL, ...){
    
    if (!is.matrix(r))
        stop("r must be a matrix of covariances!")
    
    efa_models = list()
    
    for (i in seq(n_models)){
        efa_models[[i]] = fa(r, 
                             n.obs = nrow(datVars),
                             nfactors = i, 
                             rotate = "oblimin", 
                             # n.iter = 1000,
                             fm = "wls", 
                             max.iter = 5000)
    }
    return(efa_models)
}

#run series of models; 1:5-factor solutions
modsEFA_rnd1 = efa_mods_fun(corrList[["poly"]][["hum"]], n_models = 5)

#fit indices - round 1
modsFit_rnd1 = list(
  hum = round(
    data.frame(
      a = c(modsEFA_rnd1[[1]]$STATISTIC, modsEFA_rnd1[[2]]$STATISTIC,
            modsEFA_rnd1[[3]]$STATISTIC, modsEFA_rnd1[[4]]$STATISTIC,
            modsEFA_rnd1[[5]]$STATISTIC),

      b = c(modsEFA_rnd1[[1]]$TLI, modsEFA_rnd1[[2]]$TLI,
            modsEFA_rnd1[[3]]$TLI, modsEFA_rnd1[[4]]$TLI,
            modsEFA_rnd1[[5]]$TLI),

      c = c(modsEFA_rnd1[[1]]$BIC, modsEFA_rnd1[[2]]$BIC,
            modsEFA_rnd1[[3]]$BIC, modsEFA_rnd1[[4]]$BIC,
            modsEFA_rnd1[[5]]$BIC),

      d = c(modsEFA_rnd1[[1]]$RMSEA[1], modsEFA_rnd1[[2]]$RMSEA[1],
            modsEFA_rnd1[[3]]$RMSEA[1], modsEFA_rnd1[[4]]$RMSEA[1],
            modsEFA_rnd1[[5]]$RMSEA[1]),

      e = c(modsEFA_rnd1[[1]]$Vaccounted[2], modsEFA_rnd1[[2]]$Vaccounted[3, 2],
            modsEFA_rnd1[[3]]$Vaccounted[3, 3], modsEFA_rnd1[[4]]$Vaccounted[3, 4],
            modsEFA_rnd1[[5]]$Vaccounted[3, 5]),
      
      row.names = c('Model 1', 'Model 2', 'Model 3', 'Model 4', 'Model 5')
      ), 
    2))
modsFit_rnd1

#visualize table
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
        caption = 'EFA Model Fit Indices - Round 1') %>% 
  kable_styling(bootstrap_options = c('striped', 'HOLD_position'),
                full_width = FALSE, 
                position = 'center') %>% 
  column_spec(1, width = '8cm') %>% 
  pack_rows(
    index = c('Human-Machine Preference' = 5),
    latex_gap_space = '.70em')

#factor loadings of each model
modsEFA_loadings = list()

#loop
for (i in seq_along(modsEFA_rnd1)) { 
  modsEFA_loadings[[i]] = rownames_to_column(
    round(data.frame(
      modsEFA_rnd1[[i]][["loadings"]][]), 3),  
    var = "Item") %>% 
    gather(key = "Factor", value = "Loading", -1)
}

#faDiagram
fa.diagram(modsEFA_rnd1[[3]], 
           main = "WLS using Poly - Round 1", 
           digits = 3, 
           rsize = .6,
           esize = 3,
           size = 5,
           cex = 1,
           l.cex = .5,
           cut = .4, 
           marg = (c(.5, 2.5, 3, .5)))

datList[["humBest"]] = select(datList[["hum"]], HUM11_R, HUM2_R, HUM14_R, HUM9_R,
                              HUM13_R, HUM6_R, HUM8_R, HUM3_R, HUM16_R, HUM1)

corrList[["poly"]][["humBest"]] = polychoric(datList[["humBest"]])$rho

#second iteration of models
modsEFA_rnd2 = efa_mods_fun(corrList[["poly"]][["humBest"]], 
                                   n_models = 3)

modsFit_rnd2 = list(
  humBest = round(
    data.frame(
      a = c(modsEFA_rnd2[[1]]$STATISTIC, modsEFA_rnd2[[2]]$STATISTIC,
            modsEFA_rnd2[[3]]$STATISTIC),
      
      b = c(modsEFA_rnd2[[1]]$TLI, modsEFA_rnd2[[2]]$TLI,
            modsEFA_rnd2[[3]]$TLI),
      
      c = c(modsEFA_rnd2[[1]]$BIC, modsEFA_rnd2[[2]]$BIC,
            modsEFA_rnd2[[3]]$BIC),
      
      d = c(modsEFA_rnd2[[1]]$RMSEA[1], modsEFA_rnd2[[2]]$RMSEA[1],
            modsEFA_rnd2[[3]]$RMSEA[1]),
      
      e = c(modsEFA_rnd2[[1]]$Vaccounted[2], modsEFA_rnd2[[2]]$Vaccounted[3, 2],
            modsEFA_rnd2[[3]]$Vaccounted[3, 3]),
      
      row.names = c('Model 1', 'Model 2', 'Model 3')
    ), 
    2))
modsFit_rnd2

# Data Viz

#diagram of loadings
fa.diagram(modsEFA_rnd2[[3]], 
           main = "EFA 1-Factor Solution", 
           digits = 3,
           cut = .5)

#df of loadings
loadings = as.data.frame(modsEFA_rnd2[[3]]$loadings[]) %>%
    round(3) %>%
    #rename(Factors = 1) %>%
    add_column(.before = 1, Item = names(datList[["humBest"]])) %>%
    mutate(Item = as.factor(Item))

#viz of factor loadings - all loadings are >= .4!
gather(loadings, key = "Factor", value = "Loadings", -Item) %>% 
  ggplot(aes(fct_inorder(Item), 
             abs(Loadings), 
             fill = Factor)) + 
  geom_bar(stat = "identity", width = .8, color = "gray") + 
  coord_flip() + 
  facet_wrap(~ Factor) + 
  scale_x_discrete(limits = rev(unique(test[[1]]))) +
    labs(
      title = "Best Competing EFA Model", 
      subtitle = "3-Factor Solution", 
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
alphas[["humBest"]]$total[1:2] #human-machine (dropped) alpha: .82!
alphas[["ls"]]$total[1:2] #science alpha: .89
alphas[["sa"]]$total[1:2] #satisfaction alpha: .88