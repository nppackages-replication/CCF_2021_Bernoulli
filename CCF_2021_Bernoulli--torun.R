################################################################################
## Coverage Error Optimal Confidence Intervals for Local Polynomial Regression
## SEBASTIAN CALONICO, MATIAS D. CATTANEO and MAX H. FARRELL
## Last update: July 22, 2021
################################################################################
## SOFTWARE WEBSITE: https://nppackages.github.io/
## install.packages('nprobust')
################################################################################
rm(list=ls(all=TRUE))
library(Hmisc)
library(nprobust)
library(Deriv)
library(reshape2)

setwd("C:/Users/sebas/Dropbox/CCTF/Calonico-Cattaneo-Farrell/submissions/07 Bernoulli/round1/simuls")

sim = 5000
d.list = c(0,1)
k.list = c("epa","uni")
n.list = c(100,250,500,750,1000,2000)
h.list = c("mse","rbc","us")

# Generate Plots
for (deriv in d.list) {
  for (kernel in k.list) {
    source("replication-codes/CCF-plots.R")
  }
}

# Generate Tables
for (deriv in d.list) {
  for (kernel in k.list) {
    source("replication-codes/CCF-tables.R")
  }
}

# Generate Simulation Results
for (n in n.list) {
  for (bwsel in bw.list){
       source("replication-codes/CCF-simuls.R")
  }
}
