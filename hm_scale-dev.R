
#it's a library, so shhh!
shhh = function(...){
    suppressWarnings(
        suppressPackageStartupMessages(base::library(...))
    )
}

shhh(tidyverse)
shhh(psych)

#read in the data, skip the second row

readFile = readLines("./data/dat_cln.csv")
datRaw = read.csv(textConnection(readFile[-2]), header = T, sep = ",")


#func to recode 
unfactorise <- function(x) {
      case_when(x %in% c("Strongly disagree") ~ 1,
                x %in% c("Somewhat disagree") ~ 2,
                x %in% c("Neither agree nor disagree") ~ 3,
                x %in% c("Somewhat agree") ~ 4,
                x %in% c("Strongly agree") ~ 5)
}


datCln = data.frame(sapply(datRaw[, 1: 32], unfactorise))
head(datCln, 10)


#recode reverse-coded items
datFnl = datCln %>% 
    mutate_at(vars(contains("_R")), list(~6 - .))
head(datFnl, 10)

hum = datFnl %>% select(starts_with("HUM"))
sa = datFnl %>% select(starts_with("SA"))
ls = datFnl %>% select(starts_with("LS"))

shhh(corrplot)

corr = cor(cbind(hum, sa, ls))

corr %>% 
    #corrplot only works with matrices!
    corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 15, diag = T, 
             tl.cex = .7, addCoef.col = "black", number.cex = .42, cl.cex = .8)

fa.parallel(hum, fm = "pa", fa = "fa", main = "Scree Plot of Human-Machine Items", 
            ylabel = "Eigenvalues of Factors")

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

#check factor loadings for 1-factor solution
mods = efa_mods_fun(hum, n_models = 3)

mods[[1]]$loadings %>%
    print(sort = F, cutoff = .4)

humNew = hum[, -c(4, 5, 12)]

shhh(reshape2)

loadings = as.data.frame(mods[[1]]$loadings[]) %>%
    round(3) %>%
    rename(Factor1 = 1) %>%
    add_column(.before = 1, Item = names(hum)) %>%
    mutate(Item = as.factor(Item))

#viz of factor loadings
ggplot(melt(loadings), aes(fct_inorder(Item), abs(value), fill = variable)) +
    geom_bar(stat = "identity", width = .8, color = "gray") +
    coord_flip() +
    labs(title = "Loading Comparisons", 
         subtitle = "1-Factor Solution", 
         y = "Loading Strength") +
    theme_gray(base_size = 10) +
    theme(legend.position = "right")

alphas = lapply(list(humNew, sa, ls), psych::alpha)

alphas[[1]]$total #human-machine alpha: .85
alphas[[2]]$total #science alpha: .88
alphas[[3]]$total #satisfaction alpha: .89




