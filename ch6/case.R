# question 1

library(ncvreg)
data("heart")
x <- as.matrix(heart[,1:9])
y <- heart$chd

mod.lasso <- cv.ncvreg(x,y,family="binomial",penalty = "lasso" )
mod.MCP <- cv.ncvreg(x,y,family="binomial",penalty = "MCP" )
mod.SCAD <- cv.ncvreg(x,y,family="binomial",penalty = "SCAD" )
plot(mod.lasso)
coef(mod.lasso)
plot(mod.MCP)
coef(mod.MCP)
plot(mod.SCAD)
coef(mod.SCAD)

# question 2_________________________________________
n=100
p=10
mu=rep(0,p)
simdata <- function(n, p, ro, mu, type = "type1"){
  require(MASS)
  if(type == "type1"){
    sigma = matrix(NA,nrow = p,ncol = p)
    for (i in 1:p){
      for (j in 1:p){
        sigma[i,j] = ro^abs(i-j)
      }
    }
  }
  if(type == "type2"){
    sigma = matrix(ro,nrow = p,ncol = p)
    diag(sigma) <- 1
  }
  return(mvrnorm(n=n,mu=mu,Sigma=sigma))
}
sim1_1 <- simdatatype1(n,p,0.1,mu,"type1")
sim1_2 <- simdatatype1(n,p,0.5,mu,"type1")
sim1_3 <- simdatatype1(n,p,0.9,mu,"type1")
sim2_1 <- simdatatype2(n,p,0.1,mu,"type2")
sim2_2 <- simdatatype2(n,p,0.5,mu,"type2")
sim2_3 <- simdatatype2(n,p,0.9,mu,"type2")
