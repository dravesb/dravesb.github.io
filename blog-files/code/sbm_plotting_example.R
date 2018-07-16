#------------------------------
#
#   SBM - Plotting Example 
#
#------------------------------

#source plot_matrix
source("~/Documents/Work/github/dravesb.github.io/blog-files/code/plot_matrix.R")

#load up igraph
require(igraph)

#set seed 
set.seed(2018)

#generate adjacency matrix from 10 block SBM

#set block probabilities
B = matrix(NA, nrow = 10, ncol = 10)
B[upper.tri(B)] = rbeta(45, 1, 20)
B[lower.tri(B)] = t(B)[lower.tri(B)]
diag(B) = rep(0.5, 10)

B
plot_matrix(B)

#sample SBM
G = sample_sbm(200, B, rep(20, 10))
A = as.matrix(as_adjacency_matrix(G))
plot_matrix(A)

