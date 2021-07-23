sigma <- 1
x.lb  <- -1
x.ub  <- 1
neval <- 6
eval  <- seq(x.lb, x.ub, length.out = neval)
vce   <- "nn"
scale <- 1
check <- 21
qz    <- qnorm(0.975)

rho.star=1
if (kernel=="epa") {
  if (deriv==0 & p==1) rho.star=0.8647918
  if (deriv==1 & p==2) rho.star=0.8953202
}

m.fun = function(x) {sin(3*pi*x/2)/(1+18*x^2*(sign(x)+1))}

m.pob <- m.fun(eval)
plot.fun = m.fun
if (deriv==1) {
  m1.fun = Deriv(m.fun)
  m.pob <- m1.fun(eval)
  plot.fun = m1.fun
}

h.hat = m.hat   = m.hat.bc   = se.hat.us = se.hat.rb = matrix(NA,neval,sim)
ec.rb = ec.us = il.rb = il.us = rep(0,neval)

# Loop
set.seed(2020)

showwhen = 1; showevery=100
for (i in 1:sim) {
  if (i==showwhen) {cat(paste("\nSimulations Completed:",i-1,"of",sim,"- n:",n,    " - ", Sys.time())); showwhen=showwhen+showevery}
  
  # Generate random data
  x = runif(n, min = x.lb, max = x.ub)
  y = m.fun(x) + rnorm(n,0,sigma)
  
  ### bwselect
  if (bwsel=="us")  bw.out = lpbwselect(y=y, x=x, p=p-1, deriv=0,     eval=eval, bwselect="ce-dpi",  kernel=kernel, vce=vce, bwcheck=check, bwregul=scale)
  if (bwsel=="rbc") bw.out = lpbwselect(y=y, x=x, p=p,   deriv=deriv, eval=eval, bwselect="ce-dpi",  kernel=kernel, vce=vce, bwcheck=check, bwregul=scale)
  if (bwsel=="mse") bw.out = lpbwselect(y=y, x=x, p=p,   deriv=deriv, eval=eval, bwselect="mse-dpi", kernel=kernel, vce=vce, bwcheck=check, bwregul=scale)
  h.hat[,i]  = bw.out$bws[,"h"]
    
  lp.out = lprobust(y=y, x=x, p=p, deriv=deriv, eval=eval, h=h.hat[,i], rho=rho.star, kernel=kernel, vce=vce, bwcheck=check, bwregul=scale)
    m.hat[,i]     = lp.out$Estimate[,"tau.us"]
    se.hat.us[,i] = lp.out$Estimate[,"se.us"]
    m.hat.bc[,i]  = lp.out$Estimate[,"tau.bc"]
    se.hat.rb[,i] = lp.out$Estimate[,"se.rb"]

  T.rb   = (m.hat.bc[,i] - m.pob) / se.hat.rb[,i]
  ec.rb  = ec.rb + 1*(abs(T.rb)<=qz)
  il.rb  = il.rb + 2*qz*se.hat.rb[,i]
  
  T.us   = (m.hat[,i] - m.pob) / se.hat.us[,i]
  ec.us  = ec.us + 1*(abs(T.us)<=qz)
  il.us  = il.us + 2*qz*se.hat.us[,i]
}

ec.rb = ec.rb/sim
ec.us = ec.us/sim
il.rb = il.rb/sim
il.us = il.us/sim

lb.rb = rowMeans(m.hat.bc - qz*se.hat.rb)
ub.rb = rowMeans(m.hat.bc + qz*se.hat.rb)

lb.us = rowMeans(m.hat - qz*se.hat.us)
ub.us = rowMeans(m.hat + qz*se.hat.us)

out = cbind(eval, m.pob, rowMeans(h.hat), ec.us, ec.rb, il.us, il.rb, lb.us, ub.us, lb.rb, ub.rb)
colnames(out) = c("eval", "m.pob", "h.hat", "ec.us", "ec.rb", "il.us", "il.rb", "lb.us", "ub.us", "lb.rb", "ub.rb")
write.csv(out, file=paste("output/lp_h",bwsel,"_k",kernel,"_p",p,"_d",deriv,"_n",n,".csv",sep=""))
