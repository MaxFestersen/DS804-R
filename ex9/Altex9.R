library(dplyr)
library(tidyr)
data_seeds = data.frame(read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt'))
names(data_seeds) = c('area','perimeter','compactness','length','width','coefficient','groove','types')

eudist = function(x1, x2){
  l = sqrt(sum((x1[1:7]-x2[1:7])^2))
  return(l)}




clus = function(x, p){
  index = apply(p, 1, eudist, x)
  group = which.min(unname(index))
  return(group)
}

pmat = data_seeds[1:3,]
newgroup = function(data_seeds, pmat){
  group_ind = apply(data_seeds, 1, clus, pmat)
  data_new = cbind(data_seeds, group_ind)
  p1 = data_new %>% filter(group_ind == 1) %>% dplyr::select(c(area,perimeter,compactness,length,width,coefficient,groove)) %>% apply(2,mean)
  p2 = data_new %>% filter(group_ind == 2) %>% dplyr::select(c(area,perimeter,compactness,length,width,coefficient,groove)) %>% apply(2,mean)
  p3 = data_new %>% filter(group_ind == 3) %>% dplyr::select(c(area,perimeter,compactness,length,width,coefficient,groove)) %>% apply(2,mean)
  pmat = rbind(p1,p2,p3)
  pmat = rbind(p1,p2,p3)
  return(pmat)
}
pmatnew = list()
for (i in 1:6){
  if (i == 1) {pmatnew[[i]] = newgroup(data_seeds, pmat)}
  else pmatnew[[i]] = newgroup(data_seeds, pmatnew[[i-1]])
}
group = apply(data_seeds, 1, clus, pmatnew[[6]])
seed_new = cbind(data_seeds, group) 
seeds = seed_new %>% group_by(types, group) %>% summarize(count=n()) %>% spread(types, count, fill=0)
knitr::kable(seeds, caption='Types Fitted')


fit = kmeans(data_seeds, algorithm = "Lloyd", 3)
seed_new = cbind(data_seeds, group = fit$cluster) %>% group_by(types, group) %>% summarize(count=n()) %>% spread(types, count, fill=0)
knitr::kable(seed_new, caption='Types Fitted through Kmeans Lloyd')

fit = kmeans(data_seeds, algorithm = "MacQueen", 3)
seed_new = cbind(data_seeds, group = fit$cluster) %>% group_by(types, group) %>% summarize(count=n()) %>% spread(types, count, fill=0)
knitr::kable(seed_new, caption='Types Fitted through Kmeans MacQueen')


fit = kmeans(data_seeds, algorithm = "Forgy", 3)
seed_new = cbind(data_seeds, group = fit$cluster) %>% group_by(types, group) %>% summarize(count=n()) %>% spread(types, count, fill=0)
knitr::kable(seed_new, caption='Types Fitted through Kmeans Forgy')


fit = kmeans(data_seeds, algorithm = "Hartigan-Wong", 3)
seed_new = cbind(data_seeds, group = fit$cluster) %>% group_by(types, group) %>% summarize(count=n()) %>% spread(types, count, fill=0)
knitr::kable(seed_new, caption='Types Fitted through Kmeans Hartigan-Wong')











