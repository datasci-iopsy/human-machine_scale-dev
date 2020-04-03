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

test = cor.test()

## test custom corr function
polyCorrs_func = function(data, mapping, ...) { #arg3: sizeRange = c(1, 5)
  x = eval(mapping$x, data)
  y = eval(mapping$y, data)
  
  poly = cor.test(x, y)
  
  r = unname(poly$estimate)
  rt = format(r, digits = 2)[1]
  
  # plot the cor value
  ggally_text(
    label = as.character(rt), 
    mapping = aes(),
    xP = 0.5, yP = 0.5, 
    # size = I(percent_of_range(cex * abs(r), sizeRange)),
    #color = color,
    ...
  ) + 
    # remove all the background stuff and wrap it with a dashed line
    theme_classic() + 
    theme(
      panel.background = element_rect(
        color = color, 
        linetype = "longdash"
      ), 
      axis.line = element_blank(), 
      axis.ticks = element_blank(), 
      axis.text.y = element_blank(), 
      axis.text.x = element_blank()
    )
}
polyCorrs_func(datList[["sa"]], aes(SA1, SA2))

#distributions
ggpairs(
  tibble(select_at(datVars, vars(matches("HUM")))),
  upper = list(continuous = wrap(polychoric, smooth = TRUE)),
  # diag = list(),
  # lower = list(),
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
modsEFA = efa_mods_fun(corrList[["spearman"]][["hum"]], n_models = 5)

modsFit = list(
  hum = round(
    data.frame(
      a = c(modsEFA[[1]]$STATISTIC, modsEFA[[2]]$STATISTIC,
            modsEFA[[3]]$STATISTIC, modsEFA[[4]]$STATISTIC,
            modsEFA[[5]]$STATISTIC),

      b = c(modsEFA[[1]]$TLI, modsEFA[[2]]$TLI,
            modsEFA[[3]]$TLI, modsEFA[[4]]$TLI,
            modsEFA[[5]]$TLI),

      c = c(modsEFA[[1]]$BIC, modsEFA[[2]]$BIC,
            modsEFA[[3]]$BIC, modsEFA[[4]]$BIC,
            modsEFA[[5]]$BIC),

      d = c(modsEFA[[1]]$RMSEA[1], modsEFA[[2]]$RMSEA[1],
            modsEFA[[3]]$RMSEA[1], modsEFA[[4]]$RMSEA[1],
            modsEFA[[5]]$RMSEA[1]),

      e = c(modsEFA[[1]]$Vaccounted[2], modsEFA[[2]]$Vaccounted[3, 2],
            modsEFA[[3]]$Vaccounted[3, 3], modsEFA[[4]]$Vaccounted[3, 4],
            modsEFA[[5]]$Vaccounted[3, 5]),
      
      row.names = c('Model 1', 'Model 2', 'Model 3', 'Model 4', 'Model 5')
      ), 
    2))
modsFit

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
        caption = 'EFA Model Fit Indices') %>% 
  kable_styling(bootstrap_options = c('striped', 'HOLD_position'),
                full_width = FALSE, 
                position = 'center') %>% 
  column_spec(1, width = '8cm') %>% 
  pack_rows(
    index = c('Human-Machine Preference' = 5),
    latex_gap_space = '.70em')

fa.diagram(modsEFA[[4]], main = "WLS using Poly", digits = 3, cut = 0)


datList[["humBest"]] = select_at(datList[["hum"]], 
                                 vars(matches("16_R$|^HUM3|_14")))

modsEFA[["round2"]] = efa_mods_fun(cor(datList[["humBest"]], method = "spearman"), 
                                   n_models = 3)


#factor loadings of each model
modsEFA_loadings = list()

#loop
for (i in seq_along(modsEFA)) { 
  modsEFA_loadings[[i]] = rownames_to_column(
    round(data.frame(
      modsEFA[[i]][["loadings"]][]), 3),  
    var = "Item") %>% 
    gather(key = "Factor", value = "Loading", -1)
}

#scale was theorized to be 1 factor...rm items with low loadings (i.e., < .4)
datList[["humBest"]] = select_at(datList[["hum"]], vars(matches("_R")))

# #add new "hum" items to list of dfs
# datList = append(datList, list("humNew" = humNew), after = 1)

#run EFA w/ 1-factor solution using df w/ redacted items
mod_humNew = efa_mods_fun(datList[["humNew"]], n_models = 1)
mod_humNew[[1]]$loadings #all loadings >= .40!

# Data Viz

#diagram of loadings
fa.diagram(modsEFA[["round2"]][[1]], 
           main = "EFA 1-Factor Solution", 
           digits = 3)

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
m