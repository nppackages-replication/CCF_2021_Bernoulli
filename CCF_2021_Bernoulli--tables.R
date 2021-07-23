################################################################################
## Coverage Error Optimal Confidence Intervals for Local Polynomial Regression
## SEBASTIAN CALONICO, MATIAS D. CATTANEO and MAX H. FARRELL
## Last update: July 22, 2021
################################################################################

p <- deriv+1
x.lb  <- -1
x.ub  <- 1
neval <- 6
eval  <- seq(x.lb, x.ub, length.out = neval)
n.list <- c(100,250,500,750,1000,2000)

m.fun = function(x) {sin(3*pi*x/2)/(1+18*x^2*(sign(x)+1))}

bwsel = "rbc"
data1.rbc <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[1],".csv",sep=""))
data2.rbc <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[2],".csv",sep=""))
data3.rbc <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[3],".csv",sep=""))
data4.rbc <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[4],".csv",sep=""))
data5.rbc <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[5],".csv",sep=""))
data6.rbc <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[6],".csv",sep=""))

bwsel = "us"
data1.us <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[1],".csv",sep=""))
data2.us <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[2],".csv",sep=""))
data3.us <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[3],".csv",sep=""))
data4.us <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[4],".csv",sep=""))
data5.us <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[5],".csv",sep=""))
data6.us <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[6],".csv",sep=""))

bwsel = "mse"
data1.mse <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[1],".csv",sep=""))
data2.mse <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[2],".csv",sep=""))
data3.mse <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[3],".csv",sep=""))
data4.mse <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[4],".csv",sep=""))
data5.mse <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[5],".csv",sep=""))
data6.mse <- read.csv(paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n.list[6],".csv",sep=""))


ec.hmse.rb = cbind(data1.mse$ec.rb, data2.mse$ec.rb, data3.mse$ec.rb, data4.mse$ec.rb, data5.mse$ec.rb, data6.mse$ec.rb) 
ec.hmse.us = cbind(data1.mse$ec.us, data2.mse$ec.us, data3.mse$ec.us, data4.mse$ec.us, data5.mse$ec.us, data6.mse$ec.us) 
ec.hrbc.rb = cbind(data1.rbc$ec.rb, data2.rbc$ec.rb, data3.rbc$ec.rb, data4.rbc$ec.rb, data5.rbc$ec.rb, data6.rbc$ec.rb) 
ec.hrbc.us = cbind(data1.rbc$ec.us, data2.rbc$ec.us, data3.rbc$ec.us, data4.rbc$ec.us, data5.rbc$ec.us, data6.rbc$ec.us) 
ec.hus.rb  = cbind(data1.us$ec.rb,  data2.us$ec.rb,  data3.us$ec.rb,  data4.us$ec.rb,  data5.us$ec.rb,  data6.us$ec.rb) 
ec.hus.us  = cbind(data1.us$ec.us,  data2.us$ec.us,  data3.us$ec.us,  data4.us$ec.us,  data5.us$ec.us,  data6.us$ec.us) 

il.hmse.rb = cbind(data1.mse$il.rb, data2.mse$il.rb, data3.mse$il.rb, data4.mse$il.rb, data5.mse$il.rb, data6.mse$il.rb) 
il.hmse.us = cbind(data1.mse$il.us, data2.mse$il.us, data3.mse$il.us, data4.mse$il.us, data5.mse$il.us, data6.mse$il.us) 
il.hrbc.rb = cbind(data1.rbc$il.rb, data2.rbc$il.rb, data3.rbc$il.rb, data4.rbc$il.rb, data5.rbc$il.rb, data6.rbc$il.rb) 
il.hrbc.us = cbind(data1.rbc$il.us, data2.rbc$il.us, data3.rbc$il.us, data4.rbc$il.us, data5.rbc$il.us, data6.rbc$il.us) 
il.hus.rb  = cbind(data1.us$il.rb,  data2.us$il.rb,  data3.us$il.rb,  data4.us$il.rb,  data5.us$il.rb,  data6.us$il.rb) 
il.hus.us  = cbind(data1.us$il.us,  data2.us$il.us,  data3.us$il.us,  data4.us$il.us,  data5.us$il.us,  data6.us$il.us) 


h.mse = cbind(data1.mse$h.hat, data2.mse$h.hat, data3.mse$h.hat, data4.mse$h.hat, data5.mse$h.hat, data6.mse$h.hat) 
h.rbc = cbind(data1.rbc$h.hat, data2.rbc$h.hat, data3.rbc$h.hat, data4.rbc$h.hat, data5.rbc$h.hat, data6.rbc$h.hat) 
h.us  = cbind(data1.us$h.hat,  data2.us$h.hat,  data3.us$h.hat,  data4.us$h.hat,  data5.us$h.hat,  data6.us$h.hat) 

colnames(ec.hrbc.rb)=colnames(ec.hrbc.us)=colnames(ec.hus.rb)=colnames(ec.hus.us)=colnames(ec.hmse.rb)=colnames(ec.hmse.us)=n.list
rownames(ec.hrbc.rb)=rownames(ec.hrbc.us)=rownames(ec.hus.rb)=rownames(ec.hus.us)=rownames(ec.hmse.rb)=rownames(ec.hmse.us)=eval
colnames(il.hrbc.rb)=colnames(il.hrbc.us)=colnames(il.hus.rb)=colnames(il.hus.us)=colnames(il.hmse.rb)=colnames(il.hmse.us)=n.list
rownames(il.hrbc.rb)=rownames(il.hrbc.us)=rownames(il.hus.rb)=rownames(il.hus.us)=rownames(il.hmse.rb)=rownames(il.hmse.us)=eval

colnames(h.mse)=colnames(h.rbc)=colnames(h.us)=n.list
rownames(h.mse)=rownames(h.rbc)=rownames(h.us)=eval

ec.hrbc.rb2 = melt(t(ec.hrbc.rb))
ec.hrbc.us2 = melt(t(ec.hrbc.us))
ec.hus.rb2  = melt(t(ec.hus.rb))
ec.hus.us2  = melt(t(ec.hus.us))
ec.hmse.rb2 = melt(t(ec.hmse.rb))
ec.hmse.us2 = melt(t(ec.hmse.us))

il.hrbc.rb2 = melt(t(il.hrbc.rb))
il.hrbc.us2 = melt(t(il.hrbc.us))
il.hus.rb2  = melt(t(il.hus.rb))
il.hus.us2  = melt(t(il.hus.us))
il.hmse.rb2 = melt(t(il.hmse.rb))
il.hmse.us2 = melt(t(il.hmse.us))

h.rbc2 = melt(t(h.rbc))
h.us2  = melt(t(h.us))
h.mse2 = melt(t(h.mse))

table1 = matrix(NA,36,9)
#table1[,1] = ec.hrbc.rb2[,1]

table1[,1] = h.rbc2[,3]
table1[,2] = ec.hrbc.rb2[,3]
table1[,3] = ec.hrbc.us2[,3]

table1[,4] = h.us2[,3]
table1[,5] = ec.hus.rb2[,3]
table1[,6] = ec.hus.us2[,3]

table1[,7]  = h.mse2[,3]
table1[,8]  = ec.hmse.rb2[,3]
table1[,9] = ec.hmse.us2[,3]

colnames(table1) = c("$h$","RBC","US", "$h$","RBC","US", "$h$","RBC","US")
rownames(table1) = rep(n.list,neval)
table1=formatC(table1,     format = "f", digits = 3)

table1_tex = latex(table1, file = paste("output/table_ec_k",kernel,"_p",p,"_d",deriv,".txt",sep=""), landscape=FALSE,
                   outer.size='scriptsize', col.just=rep('c',ncol(table1)), center='none', title='', table.env=FALSE,
                   n.cgroup=c(3,3,3), cgroup = c("$h_{\\texttt{RBC}}$","$h_{\\texttt{US}}$","$h_{\\texttt{MSE}}$"),
                   n.rgroup=c(6,6,6,6,6,6), rgroup = c(paste("$x=$",eval,sep="")))



table2 = matrix(NA,36,9)
#table1[,1] = ec.hrbc.rb2[,1]

table2[,1] = h.rbc2[,3]
table2[,2] = il.hrbc.rb2[,3]
table2[,3] = il.hrbc.us2[,3]

table2[,4] = h.us2[,3]
table2[,5] = il.hus.rb2[,3]
table2[,6] = il.hus.us2[,3]

table2[,7]  = h.mse2[,3]
table2[,8]  = il.hmse.rb2[,3]
table2[,9] = il.hmse.us2[,3]

colnames(table2) = c("$h$","RBC","US", "$h$","RBC","US", "$h$","RBC","US")
rownames(table2) = rep(n.list,neval)
table2=formatC(table2,     format = "f", digits = 3)

table1_tex = latex(table2, file = paste("output/table_il_k",kernel,"_p",p,"_d",deriv,".txt",sep=""), landscape=FALSE,
                   outer.size='scriptsize', col.just=rep('c',ncol(table1)), center='none', title='', table.env=FALSE,
                   n.cgroup=c(3,3,3), cgroup = c("$h_{\\texttt{RBC}}$","$h_{\\texttt{US}}$","$h_{\\texttt{MSE}}$"),
                   n.rgroup=c(6,6,6,6,6,6), rgroup = c(paste("$x=$",eval,sep="")))



