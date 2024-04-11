#setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/model/lauren/lieRecurseToMapp")
#load("Rdata/recurseToMfit.Rdata")

source("ToMModelFunctions.R")
source("models/recursiveToM_model.R")
source("models/noToM_model.R")
source("models/somePeopleLie_model.R")


bs.final <- read.csv("bsfinal_anon.csv")
humanLie <- bs.final %>%
  filter(roleCurrent == "bullshitter")
humanLieCounts <- humanLie %>%
  count(expt, probabilityRed, drawnRed, reportedDrawn) %>%
  complete(expt=c("expt4","expt5"), probabilityRed=c(0.2,0.5,0.8), drawnRed=0:10, reportedDrawn=0:10, fill = list(n = 0)) %>%
  pull(n) %>%
  matrix(nrow=121)

# Functions
logitToProb <- function(logit){
  exp(logit) / (1+exp(logit))
}

probToLogit <- function(prob){
  log(prob / (1 - prob))
}

eval.s <- function(matr, ns){ #ns = 121 x 6 matrix of counts for all conditions
  sum(log(matr)*ns)
}

getDiag <- function(arr){
  d = length(dim(arr))
  if(d == 3){
    apply(arr, MARGIN=d, FUN=diag)
  } else{
    diag(arr)
  }
}
select_all_but_diag <- function(x) {
  matrix(x[lower.tri(x, diag = F) | upper.tri(x, diag = F)], 
         nrow = nrow(x) - 1, 
         ncol = ncol(x))
}
getLies <- function(arr){
  d = length(dim(arr))
  if(d == 3){
    apply(arr, MARGIN=d, FUN=select_all_but_diag)
  } else{
    select_all_but_diag(arr)
  }
}

asTibble <- function(df){
  df %>%
    as_tibble() %>% 
    mutate(ksay = 0:10) %>% 
    pivot_longer(-ksay, names_to = 'k', values_to='probability') %>% 
    mutate(k = as.numeric(substr(k, 2, 10))-1,
           expt = ifelse(k < ceiling(max(k)/2), "red", "blue"),
           expt = factor(expt, levels=c("red","blue"))) %>%
    relocate(k, .before = ksay) %>%
    arrange(k, ksay) %>%
    mutate(p = rep(rep(c(0.2, 0.5, 0.8), each=121),2),
           p = as.factor(p),
           k = k %% 11,
           probTxt = paste0(round(probability*100),"%")) %>%
    relocate(c(expt,p), .before = k) %>%
    arrange(expt, p, k, ksay)
}



# Experiment Data

expt = bs.final %>%
  mutate(roleCurrent = ifelse(roleCurrent=="bullshitter", "sender", "receiver")) %>%
  rename(p = probabilityRed, k = drawnRed, ksay = reportedDrawn) %>%
  mutate(expt = factor(ifelse(expt=="expt4", "red", "blue"), levels=c("red","blue")),
         p = as.factor(p))

expt.S.full <- expt %>%
  filter(roleCurrent == "sender")

expt.S <- expt.S.full %>%
  group_by(expt, p, k, ksay) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(expt, p, k, ksay, fill=list(n=0)) %>%
  group_by(expt, p, k) %>%
  mutate(prob = n / sum(n),
         probTxt = paste0(round(prob*100), "%"))


