#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "11 Feb 2018"

rm(list=ls())
graphics.off()

require(igraph)
require(knitr)
require(kableExtra)

# This file reads in the csv created by the python script and analyses the sparow interaction data.
# It creates networks for all of the sampled days as well as the trip, combinations of days, and the
# total population recorded over 3 years.  It then uses igraph to find community subsets in the networks
# using three algorithms.  The plots are saved to be used in LaTeX.






###############################################################################
# Read in Data
###############################################################################

print('Read in data')

data <- read.csv('../Results/adj_data.csv', header=T)
# read data in

###############################################################################
# Data Formatting
###############################################################################


indv <- data.frame(data$individual1, data$individual2)
# make individual data into df

net <- graph.data.frame(indv, directed=F)
# network plot format for data

E(net)$weight <- 1
net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
# collapses multiple edges to weighted edges

###############################################################################
# Whole network functions
###############################################################################

avg_mod <- setNames(data.frame(matrix(ncol = 4, nrow = 9)), c("Timescale", "Algorithm", "Modularity", "Groups"))
# sets up df to populate later for average modularity

avg_den <- data.frame('Timescale'=rep(0,3), 'Nodes'=rep(0,3), 'Edges'=rep(0,3), 'Density'=rep(0,3), 'Average Degree' =rep(0,3), 'Cluster Coefficient'=rep(0,3))
# sets up df to populate later for network statistics

alg_stats_t <- function(name_algt, algt){
  com <- length(algt)
  # number of communities
  mod <- modularity(algt)
  # modularity of the plot
  avg_mod[] <- c('Timescale'= 'Total',  'Algorithm'= name_algt, 'Modularity'= mod, 'Groups' = com)
  
}
# function to save total population modularity information to created df

  
nodes_t <- vcount(net2)
# number of nodes in the total network

edges_t <- ecount(net2)
# number of edges in the total network

density_t <- edge_density(net2, loops=F)
# density of the total network 

avg_degree_t <- mean(degree(net2, mode="in"))
# average degree of the total network

clust_coeff_t <- transitivity(net2, type="global")
# cluster coefficient of the total network


avg_den[3,] <-c('Timescale'='Total','Nodes'=nodes_t, 'Edges'=edges_t, 'Density'=density_t, 'Average Degree' =avg_degree_t, 'Cluster Coefficient'=clust_coeff_t)
# populates average network stats table with total network information

###############################################################################
# Whole network
###############################################################################


#Greedy
print('Greedy Cluster for total data')

clust_greedy <- cluster_fast_greedy(as.undirected(net2)) # run the function on the network
avg_mod[8,] <- alg_stats_t('Greedy Cluster', clust_greedy) # populates the df for greedy information
pdf('../Results/fast_greedy_total.pdf') # saves pdf
plot(clust_greedy, as.undirected(net2), # plots algorithm results
     vertex.label = NA,
     #main= 'Greedy Cluster',
     vertex.size = degree(net2)*0.02,
     layout=layout.fruchterman.reingold)
#fastgreedy.community(net, weights = NULL)

invisible(dev.off()) # exit save pdf info



# lovain
print('louvain Cluster for total data')

clust_louv <- cluster_louvain(as.undirected(net2)) # same process as for greedy above
avg_mod[9,] <- alg_stats_t('Louvain Cluster', clust_louv)
pdf('../Results/louvain_total.pdf')

plot(clust_louv, as.undirected(net2),
     vertex.label = NA,
     #main= 'Louvain Cluster',
     vertex.size = degree(net2)*0.02,
     edge.width = E(net)$weight,
     layout=layout.fruchterman.reingold)

invisible(dev.off())

# edge-between
print('Edge Betweenness Cluster for total data - this takes around 4min on my computer')

clust_betw <- cluster_edge_betweenness(as.undirected(net2)) # same process as for greedy above
avg_mod[7,] <- alg_stats_t('Edge Betweenness Cluster', clust_betw)
pdf('../Results/edge_betw_total.pdf')

plot(clust_betw, as.undirected(net2),
     vertex.label = NA,
     #main= 'Edge-Betweenness Cluster',
     vertex.size = degree(net2)*0.02,
     layout=layout.fruchterman.reingold)

invisible(dev.off())


###############################################################################
# functions for trips
############################################################################### 

louvain_cluster_plots <- function(t){ # function for plotting louvain for trips
  name_alg <- 'Louvain Cluster' # sets name
  #print(paste('Running ', name_alg, 'Trip ',t, sep='')) # used to test 
  df <- subset(data, trip == trips[t,]) # chooses trip to analyse
  indv <- data.frame(df$individual1, df$individual2) # sets two interacting individuals
  net <- graph.data.frame(indv, directed=F) # the network
  E(net)$weight <- 1
  net2 <- simplify(net, edge.attr.comb = list(weight="sum")) # edge values assigned
  alg <- cluster_louvain(as.undirected(net2)) # defines algorithm to use
  pdf(file=paste('../Results/louvain_trip_',t,'.pdf', sep='')) # saving file
  plot(alg, as.undirected(net2), # plots per trip
       vertex.label = NA,
       #main= paste('Louvain Cluster Trip ',t, sep=''), # lables on plots
       #vertex.size = degree(net)*0.2, # vertex size contingent on degree
       vertex.size = 0.75,
       layout=layout.fruchterman.reingold)
  invisible(dev.off())
  alg_stats(name_alg, alg) # saves alg stats to put in table
}



greedy_cluster_plots <- function(t){ # same as for louvain above
  name_alg <- 'Greedy Cluster'
  #print(paste('Running ', name_alg, 'Trip ',t, sep=''))
  df <- subset(data, trip == trips[t,])
  indv <- data.frame(df$individual1, df$individual2)
  net <- graph.data.frame(indv, directed=F)
  E(net)$weight <- 1
  net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
  alg <- cluster_fast_greedy(as.undirected(net2))
  pdf(file=paste('../Results/greedy_trip_',t,'.pdf', sep=''))
  plot(alg, as.undirected(net2),
       vertex.label = NA,
       #main= paste('Greedy Cluster Trip ',t, sep=''),
       vertex.size = 0.75,
       layout=layout.fruchterman.reingold)
  invisible(dev.off())
  alg_stats(name_alg, alg)
}



edge.betw_cluster_plots <- function(t){ # same as for louvain above
  name_alg <- 'Edge Betweenness Cluster'
  #print(paste('Running ', name_alg, 'Trip ',t, sep=''))
  df <- subset(data, trip == trips[t,])
  indv <- data.frame(df$individual1, df$individual2)
  net <- graph.data.frame(indv, directed=F)
  E(net)$weight <- 1
  net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
  alg <- cluster_edge_betweenness(as.undirected(net2))
  pdf(file=paste('../Results/edge_betw_trip_',t,'.pdf', sep=''))
  plot(alg, as.undirected(net2),
       vertex.label = NA,
       #main= paste('Edge Betweenness Cluster Trip ',t,sep=''),
       vertex.size = 0.75,
       layout=layout.fruchterman.reingold)
  invisible(dev.off())
  alg_stats(name_alg, alg)
}

#fix append function and create df before called
sum_stats <- function(t){ # saves stats on created network for each trip
  df <- subset(data, trip == trips[t,]) 
  indv <- data.frame(df$individual1, df$individual2)
  net <- graph.data.frame(indv, directed=F)
  E(net)$weight <- 1
  net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
  nodes <- vcount(net2)
  edges <- ecount(net2)
  density <- edge_density(net2, loops=F)
  avg_degree <- mean(degree(net2, mode="in"))
  clust_coeff <- transitivity(net2, type="global")
  net_data[i,] <- c('Trip'=t, 'nodes'=nodes, 'edges'=edges, 'density'=density, 'avg_degree'=avg_degree, 'clust_coeff'=clust_coeff) # saves information


}


alg_stats <- function(name_alg2, alg2){ # saves modularity information for each model for each trip
com <- length(alg2)
# number of communities
mod <- modularity(alg2)
# modularity of the plot
alg_data[i,] <- c('trip'= i, 'Algorithm'= name_alg2, 'Modularity'= mod, 'Groups' = com)

}
###############################################################################
# loop to run functions for trips
###############################################################################

trips <- data.frame(unique(data$trip, incomparables = FALSE, MARGIN = 1,
                          fromLast = FALSE,))
# makes a df of list of unique recording days

dlen <- nrow(trips)
# number of days

alg_data <- data.frame('Trip'=rep(0,(dlen*3)), 'Algorithm'=rep(0,(dlen*3)), 'Modularity'=rep(0,(dlen*3)), 'Groups'=rep(0,(dlen*3)))
net_data <-data.frame('Trip'=rep(0,dlen), 'Nodes'=rep(0,dlen), 'Edges'=rep(0,dlen), 'Density'=rep(0,dlen), 'Average Degree' =rep(0,dlen), 'Cluster Coefficient'=rep(0,dlen))
# empty df's to be populated by loop

print('Analysis by trip')

for (i in 1:dlen){

  alg_data[i,] <- louvain_cluster_plots(i)
  alg_data[(i+7),] <- greedy_cluster_plots(i)
  alg_data[(i+14),] <- edge.betw_cluster_plots(i)
  net_data[i,]<- sum_stats(i)
  
}
# runs the 3 types of plot by day and populates the created df's


###############################################################################
# functions for days
############################################################################### 

louvain_cluster_plots_d <- function(day, name){ # same as for trips above but for days
  name_alg <- 'Louvain Cluster'
  df <- subset(data, date.ELO2 == days[day,])
  indv <- data.frame(df$individual1, df$individual2)
  net <- graph.data.frame(indv, directed=F)
  E(net)$weight <- 1
  net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
  alg <- cluster_louvain(as.undirected(net2))
  pdf(file=paste('../Results/louvain_',name,'.pdf', sep=''))
  plot(alg, as.undirected(net2),
       vertex.label = NA,
       #main= 'Louvain Cluster',
       vertex.size = 0.75,
       #vertex.size = degree(net)*0.2,
       layout=layout.fruchterman.reingold)
  invisible(dev.off())
  alg_stats_d(name_alg, alg)
}

greedy_cluster_plots_d <- function(day, name){ # same as for trips above but for days
  name_alg <- 'Greedy Cluster'
  df <- subset(data, date.ELO2 == days[day,])
  indv <- data.frame(df$individual1, df$individual2)
  net <- graph.data.frame(indv, directed=F)
  E(net)$weight <- 1
  net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
  alg <- cluster_fast_greedy(as.undirected(net2))
  pdf(file=paste('../Results/greedy_',name,'.pdf', sep=''))
  plot(alg, as.undirected(net2),
     vertex.label = NA,
     #main= 'Greedy Cluster',
     vertex.size = 0.75,
     layout=layout.fruchterman.reingold)

  invisible(dev.off())
  alg_stats_d(name_alg, alg)
}


edge.betw_cluster_plots_d <- function(day, name){ # same as for trips above but for days
  name_alg <- 'Edge Betweenness Cluster'
  df <- subset(data, date.ELO2 == days[day,])
  indv <- data.frame(df$individual1, df$individual2)
  net <- graph.data.frame(indv, directed=F)
  E(net)$weight <- 1
  net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
  alg <- cluster_edge_betweenness(as.undirected(net2))
  pdf(file=paste('../Results/edge_betw_',name,'.pdf', sep=''))
  p <- plot(alg, as.undirected(net2),
       vertex.label = NA,
       #main= 'Edge Betweenness Cluster',
       vertex.size = 0.75,
       layout=layout.fruchterman.reingold)

  invisible(dev.off())
  alg_stats_d(name_alg, alg)
}

#fix append function and create df before called
sum_stats_d <- function(day){   # same as for trips above but for days
  df <- subset(data, date.ELO2 == days[day,])
  indv <- data.frame(df$individual1, df$individual2)
  net <- graph.data.frame(indv, directed=F)
  E(net)$weight <- 1
  net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
  nodes <- vcount(net2)
  edges <- ecount(net2)
  density <- edge_density(net2, loops=F)
  avg_degree <- mean(degree(net2, mode="in"))
  clust_coeff <- transitivity(net2, type="global")
  net_data_d[i,] <- c('date'=days[day,], 'nodes'=nodes, 'edges'=edges, 'density'=density, 'avg_degree'=avg_degree, 'clust_coeff'=clust_coeff)

}



###############################################################################
# loop to run functions for days
###############################################################################

days <- data.matrix(unique(data$date.ELO2, incomparables = FALSE, MARGIN = 1, fromLast = FALSE,))
# makes a df of list of unique recording days

dlen_d <- nrow(days)
# number of days
alg_data_d <- data.frame('Date'=rep(0,(dlen_d*3)), 'Algorithm'=rep(0,(dlen_d*3)), 'Modularity'=rep(0,(dlen_d*3)), 'Groups'=rep(0,(dlen_d*3)))

net_data_d <-data.frame('Date'=rep(0,dlen_d), 'Nodes'=rep(0,dlen_d), 'Edges'=rep(0,dlen_d), 'Density'=rep(0,dlen_d), 'Average Degree'=rep(0,dlen_d), 'Cluster Coefficient'=rep(0,dlen_d))
# empty df to be populated by loop


alg_stats_d <- function(name_alg2, alg2){
  com <- length(alg2)
  # number of communities
  mod <- modularity(alg2)
  # modularity of the plot

  time <- days[i,]
  #print(time)
  alg_data_d[i,] <- c('Date'= time , 'Algorithm'= name_alg2, 'Modularity'= mod, 'Groups' = com)
  
} # same as for trips above but for days

print('Analysis by day')
for (i in 1:dlen_d){
  alg_data_d[i,] <- louvain_cluster_plots_d(i,days[i,])
  alg_data_d[(i+(dlen_d)),] <- greedy_cluster_plots_d(i,days[i,])
  alg_data_d[(i+(dlen_d*2)),] <- edge.betw_cluster_plots_d(i,days[i,])
  net_data_d[i,]<- sum_stats_d(i)
  
}
# 3 types of plot by day # same as for trips above but for days


###############################################################################
# Save tables 
###############################################################################
print('Making Tables')

####### for days ########

alg_data_d2 <- alg_data_d[order(alg_data_d$Date, alg_data_d$Algorithm),]
# sorts data

rownames(alg_data_d2)  <- NULL
# removes index values for table

alg_data <- alg_data[order(alg_data$Trip, alg_data$Algorithm),]
# orders df

rownames(alg_data) <- NULL
# removes index names

##############################################################################
# Make average Tables
###############################################################################

###################### Day ########################################
###################################################################

######### Modularity #############
mean_d <- function(func_n, colmn){
  mean(as.numeric(unlist((subset(alg_data_d2, alg_data_d2$Algorithm == func_n))[colmn])))  
}
# function to find the mean of the day network measurements


avg_mod[1,] <- c('Day','Edge Betweenness Cluster',mean_d('Edge Betweenness Cluster', 'Modularity'),mean_d('Edge Betweenness Cluster', 'Groups'))
avg_mod[2,] <- c('Day','Greedy Cluster',mean_d('Greedy Cluster', 'Modularity'),mean_d('Greedy Cluster', 'Groups'))
avg_mod[3,] <- c('Day','Louvain Cluster',mean_d('Louvain Cluster', 'Modularity'),mean_d('Louvain Cluster', 'Groups'))
# fills in the df's for mod by day

############ Groups #############


avg_den[1,] <- c('Timescale'='Day','Nodes'=mean(as.numeric(net_data_d$Nodes)), 'Edges'=mean(as.numeric(net_data_d$Edges)), 'Density'=mean(as.numeric(net_data_d$Density)), 'Average Degree' =mean(as.numeric(net_data_d$Average.Degree)), 'Cluster Coefficient'=mean(as.numeric(net_data_d$Cluster.Coefficient)))
# fills in the df's for den by day


###################### Trip ########################################
###################################################################

######### Modularity #############
mean_t <- function(func_n, colmn){
  mean(as.numeric(unlist((subset(alg_data, alg_data$Algorithm == func_n))[colmn])))  
}
# function to find the mean of the trip network measurements

avg_mod[4,] <- c('Trip','Edge Betweenness Cluster',mean_t('Edge Betweenness Cluster', 'Modularity'),mean_t('Edge Betweenness Cluster', 'Groups'))
avg_mod[5,] <- c('Trip','Greedy Cluster',mean_t('Greedy Cluster', 'Modularity'),mean_t('Greedy Cluster', 'Groups'))
avg_mod[6,] <- c('Trip','Louvain Cluster',mean_t('Louvain Cluster', 'Modularity'),mean_t('Louvain Cluster', 'Groups'))
# fills in the df's for mod by trip


############ Groups #############
avg_den[2,] <- c('Timescale'='Trip','Nodes'=mean(as.numeric(net_data$Nodes)), 'Edges'=mean(as.numeric(net_data$Edges)), 'Density'=mean(as.numeric(net_data$Density)), 'Average Degree' =mean(as.numeric(net_data$Average.Degree)), 'Cluster Coefficient'=mean(as.numeric(net_data$Cluster.Coefficient)))
# fills in the df's for den by day

##############################################################################
# reduce significant values for tables
###############################################################################

alg_data_d2$Modularity <- as.numeric(alg_data_d2$Modularity) # converts column into numbers
is.num <- sapply(alg_data_d2, is.numeric) # apply to all number
alg_data_d2[is.num] <- lapply(alg_data_d2[is.num], round, 3) # set significance after the decimal
# for alg_data_d2

net_data_d$Density <- as.numeric(net_data_d$Density)
net_data_d$Average.Degree <- as.numeric(net_data_d$Average.Degree)
net_data_d$Cluster.Coefficient <- as.numeric(net_data_d$Cluster.Coefficient)
is.num <- sapply(net_data_d, is.numeric)
net_data_d[is.num] <- lapply(net_data_d[is.num], round, 3)
# same for net_data_d

alg_data$Modularity <- as.numeric(alg_data$Modularity)
is.num <- sapply(alg_data, is.numeric)
alg_data[is.num] <- lapply(alg_data[is.num], round, 3)
# same for alg_data

net_data$Density <- as.numeric(net_data$Density)
net_data$Average.Degree <- as.numeric(net_data$Average.Degree)
net_data$Cluster.Coefficient <- as.numeric(net_data$Cluster.Coefficient)
is.num <- sapply(net_data, is.numeric)
net_data[is.num] <- lapply(net_data[is.num], round, 3)
# same for net_data

avg_den$Density <- as.numeric(avg_den$Density)
avg_den$Average.Degree <- as.numeric(avg_den$Average.Degree)
avg_den$Cluster.Coefficient <- as.numeric(avg_den$Cluster.Coefficient)
is.num <- sapply(avg_den, is.numeric)
avg_den[is.num] <- lapply(avg_den[is.num], round, 3)
# same for net_data

avg_mod$Modularity <- as.numeric(avg_mod$Modularity)
avg_mod$Groups <- as.numeric(avg_mod$Groups)
is.num <- sapply(avg_mod, is.numeric)
avg_mod[is.num] <- lapply(avg_mod[is.num], round, 3)
# same for alg_data

##############################################################################
# Used to make tables in LaTeX format
###############################################################################


mod_by_day <- kable(alg_data_d2, format = "latex", booktabs = T)%>%
  kable_styling(latex_options = "striped", position = "center")%>%
  row_spec(0, angle= 45)
# Modularity by day

den_by_day <- kable(net_data_d, format = "latex", booktabs = T)%>%
  kable_styling(latex_options = "striped", position = "center")%>%
  row_spec(0, angle= 45)
# Density by day

mod_by_trip <- kable(alg_data, format = "latex", booktabs = T)%>%
  kable_styling(latex_options = "striped", position = "center")%>%
  row_spec(0, angle= 45)
# Modularity by trip 

den_by_trip <- kable(net_data, format = "latex", booktabs = T)%>%
  kable_styling(latex_options = "striped", position = "center")%>%
  row_spec(0, angle= 45)
# Density by trip

avg_den_all <- kable(avg_den, format = "latex", booktabs = T)%>%
  kable_styling(latex_options = "striped", position = "center")%>%
  row_spec(0, angle= 45)
# avg den all

avg_mod_all <- kable(avg_mod, format = "latex", booktabs = T)%>%
  kable_styling(latex_options = "striped", position = "center")%>%
  row_spec(0, angle= 45)
# avg mod all
