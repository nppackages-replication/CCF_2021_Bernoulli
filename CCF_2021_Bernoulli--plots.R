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

m.pob = m.fun(eval)
data = data.frame(x = eval, y = m.pob)
p1 <- ggplot(data, aes(x=x, y=y)) + stat_function(fun = m.fun, colour = "black") +
  theme(axis.title.y = element_blank(), 
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  xlab("x") + geom_point(aes(x = x, y = y))
p1  
ggsave(paste("output/dgp_m0",".pdf",sep=""))

theme(legend.position="none", panel.background = element_rect(fill = "white",colour = "white"),
      axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))

m1.fun = Deriv(m.fun)
m1.pob = m1.fun(eval)
data = data.frame(x = eval, y = m1.pob)
p1 <- ggplot(data, aes(x=x, y=y)) + stat_function(fun = m1.fun, colour = "black") +
  theme(axis.title.y = element_blank(), 
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.5, linetype = "solid", colour = "black")) +  xlab("x") + geom_point(aes(x = x, y = y))
p1  
ggsave(paste("output/dgp_m1",".pdf",sep=""))



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
h.list = cbind(h.rbc2, h.us2[,3], h.mse2[,3])
colnames(h.list)=c("n","x","RBC","US","MSE")
h.list2 = melt(h.list, id=c("n","x"))
h.df = data.frame(n=h.list2[,1], x=h.list2[,2], Bandwidth=h.list2[,3], h=h.list2[,4])



d.ec.hmse = data.frame(n=ec.hrbc.rb2$Var1, x=ec.hrbc.rb2$Var2, 
                       RBC = ec.hmse.rb2$value,
                       US  = ec.hmse.us2$value)

d.ec.hrbc = data.frame(n=ec.hrbc.rb2$Var1, x=ec.hrbc.rb2$Var2, 
                       RBC = ec.hrbc.rb2$value,
                       US  = ec.hrbc.us2$value)

d.ec.hus = data.frame(n=ec.hrbc.rb2$Var1, x=ec.hrbc.rb2$Var2, 
                      RBC = ec.hus.rb2$value,
                      US  = ec.hus.us2$value)

d.ec.hmse2 = melt(d.ec.hmse,id=c("x","n"))
d.ec.hrbc2 = melt(d.ec.hrbc,id=c("x","n"))
d.ec.hus2  = melt(d.ec.hus,id=c("x","n"))

y.lim.ec.hmse = c(min(d.ec.hmse2$value),1)
y.lim.ec.hrbc = c(min(d.ec.hrbc2$value),1)
y.lim.ec.hus = c(min(d.ec.hus2$value),1)

d.il.hmse = data.frame(n=il.hrbc.rb2$Var1, x=il.hrbc.rb2$Var2, 
                       RBC = il.hmse.rb2$value,
                       US  = il.hmse.us2$value)

d.il.hrbc = data.frame(n=il.hrbc.rb2$Var1, x=il.hrbc.rb2$Var2, 
                       RBC = il.hrbc.rb2$value,
                       US  = il.hrbc.us2$value)

d.il.hus = data.frame(n=il.hrbc.rb2$Var1, x=il.hrbc.rb2$Var2, 
                      RBC = il.hus.rb2$value,
                      US  = il.hus.us2$value)
d.il.h = data.frame(n=il.hrbc.rb2$Var1, x=il.hrbc.rb2$Var2, 
                      T_rbc_h_rbc = il.hrbc.rb2$value,
                      T_rbc_h_mse = il.hmse.rb2$value,
                      T_rbc_h_us  = il.hus.rb2$value,
                      T_us_h_us   = il.hus.us2$value)

d.il  = data.frame(n=il.hrbc.rb2$Var1, x=il.hrbc.rb2$Var2, 
                  il = il.hrbc.rb2$value/il.hus.us2$value)

d.il2 = data.frame(n=il.hrbc.rb2$Var1, x=il.hrbc.rb2$Var2, 
                  il = il.hmse.rb2$value/il.hmse.us2$value)

d.il.hmse2 = melt(d.il.hmse,id=c("x","n"))
d.il.hrbc2 = melt(d.il.hrbc,id=c("x","n"))
d.il.hus2  = melt(d.il.hus,id=c("x","n"))
d.il  = melt(d.il, id=c("x","n"))
d.il2 = melt(d.il2,id=c("x","n"))
d.il.h2 = melt(d.il.h,id=c("x","n"))

evals=unique(d.ec.hrbc2$x)

for (i in 1:neval) {
  p1 <- ggplot(subset(d.ec.hrbc2, x==evals[i]), 
               aes(x=n, y=value, group = variable, linetype=variable, color= variable))  + 
    labs(color = "") +
    geom_line(size=1) +  coord_cartesian(ylim = y.lim.ec.hrbc ) + labs(x = "n", y = "") +
    geom_hline(aes(yintercept=0.95) , linetype=2, color="black") +
    geom_point(aes(x = n, y = value, shape=variable)) +
    theme(legend.justification = 'left', legend.position=c(0,0.2), legend.background = element_blank(), legend.key = element_blank()) +
    scale_linetype_manual(name="", values = c("solid","dotdash"),labels=c("Robust Bias Correction", "Undersmoothing")) +
    scale_color_manual(name ="", values = c( "black", "red")) + 
    theme(legend.position="none", panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))
  p1
  ggsave(paste("output/ce_hrbc_k",kernel,"_p",p,"_d",deriv,"_x",i,".pdf",sep=""))
  
  p2 <- ggplot(subset(d.ec.hus2, x==evals[i]), 
               aes(x=n, y=value, group = variable, linetype=variable, color= variable))  + 
    labs(color = "") +
    geom_line(size=1) +  coord_cartesian(ylim = y.lim.ec.hus ) + labs(x = "n", y = "") +
    geom_hline(aes(yintercept=0.95) , linetype=2, color="black") +
    geom_point(aes(x = n, y = value, shape=variable)) +
    theme(legend.justification = 'left', legend.position=c(0,0.2), legend.background = element_blank(), legend.key = element_blank()) +
    scale_linetype_manual(name="", values = c("solid","dotdash"),labels=c("Robust Bias Correction", "Undersmoothing")) +
    scale_color_manual(name ="", values = c( "black", "red")) + 
    theme(legend.position="none", panel.background = element_rect(fill = "white",colour = "white"),
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))
  p2
  ggsave(paste("output/ce_hus_k",kernel,"_p",p,"_d",deriv,"_x",i,".pdf",sep=""))
  
  p3 <- ggplot(subset(d.ec.hmse2, x==evals[i]), 
               aes(x=n, y=value, group = variable, linetype=variable, color= variable))  + 
    labs(color = "") +
    geom_line(size=1) +  coord_cartesian(ylim = y.lim.ec.hmse ) + labs(x = "n", y = "") +
    geom_hline(aes(yintercept=0.95) , linetype=2, color="black") +
    geom_point(aes(x = n, y = value, shape=variable)) +
    theme(legend.justification = 'left', legend.position=c(0,0.2), legend.background = element_blank(), legend.key = element_blank()) +
    scale_linetype_manual(name="", values = c("solid","dotdash"),labels=c("Robust Bias Correction", "Undersmoothing")) +
    scale_color_manual(name ="", values = c( "black", "red")) + 
    theme(legend.position="none", panel.background = element_rect(fill = "white",colour = "white"),
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))
  p3
  ggsave(paste("output/ce_hmse_k",kernel,"_p",p,"_d",deriv,"_x",i,".pdf",sep=""))
  
}


evals=unique(d.il$x)

for (i in 1:neval) {
  p1 <- ggplot(subset(d.il.h2, x==evals[i]), 
               aes(x=n, y=value, group = variable, linetype=variable, color= variable)) + 
    labs(color = "") +
    geom_line(size = 1) +   labs(x = "n", y = "") +
    geom_point(aes(x = n, y = value, shape=variable)) +
    theme(legend.justification = 'left', legend.position=c(0,0.2), legend.background = element_blank(), legend.key = element_blank()) +
    scale_linetype_manual(name="", values = c("solid",  "dotted",  "dashed",      "dotdash"), labels=c("T1", "T2", "T3", "T4")) +
    scale_color_manual(name ="",   values = c("black",   "blue",   "darkgreen",   "red" ))  + 
    theme(legend.position="none", panel.background = element_rect(fill = "white",colour = "white"),
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))
  p1
  ggsave(paste("output/il_k",kernel,"_p",p,"_d",deriv,"_x",i,".pdf",sep="")) 
}

y.lim.h = c(min(h.df$h),max(h.df$h))
evals=unique(h.df$x)
for (i in 1:neval) {
  tmp <- subset(h.df, x==evals[i])
  
  p5 <- ggplot(data = tmp, aes(x=n, y=h, group = Bandwidth))  + 
    labs(x = "n", y = "h", color="Bandwidth") +
    coord_cartesian(ylim = y.lim.h)  +
    geom_line(size = 1, aes(linetype=Bandwidth, color= Bandwidth)) +
    geom_point(aes(shape=Bandwidth, color= Bandwidth)) +
    scale_linetype_manual(name="", values = c("solid","dotdash","dotted")) +
    scale_color_manual(name ="",   values = c( "black", "red",  "blue")) + 
    theme(legend.position="none", panel.background = element_rect(fill = "white",colour = "white"),
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))
  
    #theme(legend.justification = 'left', legend.position=c(0,0.9), legend.background = element_blank(), legend.key = element_blank()) 
  p5
  ggsave(paste("output/h_k",kernel,"_p",p,"_d",deriv,"_x",i,".pdf",sep=""))
  

}





