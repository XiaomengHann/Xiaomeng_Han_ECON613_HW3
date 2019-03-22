# Question 1
# We first install the package and download the data frames.
install.packages("bayesm")
library(bayesm)
data("margarine")
choiceprice <- as.data.frame(margarine$choicePrice)
demos <- as.data.frame(margarine$demos)
# Calculate the dispersion and average of product 1 to 10
price <- as.data.frame(choiceprice[,3:12])
dispersion <- var(price)
dispersion <- diag(dispersion)
print(dispersion)
average=apply(as.matrix(choiceprice[,3:12]),2,mean)
print(average)

# table the frequency of product 1 to 10
print(table(choiceprice[,2]))
# calculate the marketshares of product 1 to 10
ms_choice1 <- 1766/nrow(choiceprice)
ms_choice2 <- 699/nrow(choiceprice)
ms_choice3 <- 243/nrow(choiceprice)
ms_choice4 <- 593/nrow(choiceprice)
ms_choice5 <- 315/nrow(choiceprice)
ms_choice6 <- 74/nrow(choiceprice)
ms_choice7 <- 319/nrow(choiceprice)
ms_choice8 <- 203/nrow(choiceprice)
ms_choice9 <- 225/nrow(choiceprice)
ms_choice10 <- 33/nrow(choiceprice)
choice <- as.vector(c("1","2","3","4","5","6","7","8","9","10"))
ms_choice <- rbind(ms_choice1,ms_choice2,ms_choice3,ms_choice4,ms_choice5,ms_choice6,ms_choice7,ms_choice8,ms_choice9,ms_choice10)
product_ms <- cbind(choice, ms_choice)
colnames(product_ms) = c("product","marketshare")
print(product_ms)

# calculate the marketshares based on brand names
# generate the data of choices for brand PPk
choice_ppk1 <- subset(choiceprice, choiceprice[,2] == 1)
choice_ppk2 <- subset(choiceprice, choiceprice[,2] == 8)
choice_ppk <- rbind(choice_ppk1,choice_ppk2)
# calculate the marketshare of brank PPk
ms_ppk <- nrow(choice_ppk)/nrow(choiceprice)
# repeat the process above for other brands
choice_pbb <- subset(choiceprice, choiceprice[,2] == 2)
ms_pbb <- nrow(choice_pbb)/nrow(choiceprice)
choice_pfi1 <- subset(choiceprice, choiceprice[,2] == 3)
choice_pfi2 <- subset(choiceprice, choiceprice[,2] == 9)
choice_pfi <- rbind(choice_pfi1,choice_pfi2)
ms_pfi <- nrow(choice_pfi)/nrow(choiceprice)
choice_phse1 <- subset(choiceprice, choiceprice[,2] == 4)
choice_phse2 <- subset(choiceprice, choiceprice[,2] == 10)
choice_phse <- rbind(choice_phse1,choice_phse2)
ms_phse <- nrow(choice_phse)/nrow(choiceprice)
choice_pgen <- subset(choiceprice, choiceprice[,2] == 5)
ms_pgen <- nrow(choice_phse)/nrow(choiceprice)
choice_plmp <- subset(choiceprice, choiceprice[,2] == 6)
ms_plmp <- nrow(choice_plmp)/nrow(choiceprice)
choice_pss <- subset(choiceprice, choiceprice[,2] == 7)
ms_pss <- nrow(choice_pss)/nrow(choiceprice)
brand <- as.vector(c("PPk","PBB","PFI","PHse","PGen","Plmp","PSS"))
ms_brand <- rbind(ms_ppk,ms_pbb,ms_pfi,ms_phse,ms_pgen,ms_plmp,ms_pss)
brand_ms <- cbind(brand, ms_brand)
colnames(brand_ms) = c("brand","marketshare")
print(brand_ms)

# calculate the marketshares based on types
# generate the data of choices for stick
choice_stk1 <- subset(choiceprice, choiceprice[,2] == 1)
choice_stk2 <- subset(choiceprice, choiceprice[,2] == 2)
choice_stk3 <- subset(choiceprice, choiceprice[,2] == 3)
choice_stk4 <- subset(choiceprice, choiceprice[,2] == 4)
choice_stk5 <- subset(choiceprice, choiceprice[,2] == 5)
choice_stk6 <- subset(choiceprice, choiceprice[,2] == 6)
choice_stk <-rbind(choice_stk1,choice_stk2,choice_stk3,choice_stk4,choice_stk5,choice_stk6)
# calculate the marketshare of stick
ms_stk <- nrow(choice_stk)/nrow(choiceprice)
# repeat the process above for type tube
choice_tub1 <- subset(choiceprice, choiceprice[,2] == 7)
choice_tub2 <- subset(choiceprice, choiceprice[,2] == 8)
choice_tub3 <- subset(choiceprice, choiceprice[,2] == 9)
choice_tub4 <- subset(choiceprice, choiceprice[,2] == 10)
choice_tub <- rbind(choice_tub1,choice_tub2,choice_tub3,choice_tub4)
ms_tub <- nrow(choice_tub)/nrow(choiceprice)
type <- as.vector(c("stk","tub"))
ms_type <- rbind(ms_stk,ms_tub)
type_ms <-cbind(type,ms_type)
colnames(type_ms) = c("type","marketshare")
print(type_ms)

# caculate the marketshares based on individual's income
# generate the data of individual code and income from matrix demos
income <- as.data.frame(demos[,1:2])
# generate the data of individual code and choice from matrix choiceprice
org <- as.data.frame(choiceprice[,1:2])
# merge the two data frames to map income and choice
map <- merge(income, org, by.income=c(1), by.org=c(1), all.x=TRUE,sort=TRUE )
# table map to see different income levels
table(map)
# generate the data of income level 2.5
level1 <- subset(map,map[,2] == 2.5)
# calculate the marketshare of product 1 to 10 in income level 2.5
fre1_ms <- as.matrix(table(level1[,3])/nrow(level1))
# arrange the data
subset_1 <- as.vector(c("1","2","3","4","5","6","7","8","9","10"))
ms_level1 <- as.vector(c(0.38,0.08,0,0.04,0.12,0,0.32,0.02,0.04,0))
ms_level1 <- cbind(subset_1,ms_level1)
# repeat the process above for other income levels
level2 <- subset(map,map[,2] == 7.5)
fre2_ms <- as.matrix(table(level2[,3])/nrow(level2))
ms_level2 <- cbind(subset_1,fre2_ms)
colnames(ms_level2)=c("subset_1","ms_level2")
level3 <- subset(map,map[,2] == 12.5)
fre3_ms <- as.matrix(table(level3[,3])/nrow(level3))
ms_level3 <- cbind(subset_1,fre3_ms)
colnames(ms_level3)=c("subset_1","ms_level3")
level4 <- subset(map,map[,2] == 17.5)
fre4_ms <- as.matrix(table(level4[,3])/nrow(level4))
ms_level4 <- cbind(subset_1,fre4_ms)
colnames(ms_level4)=c("subset_1","ms_level4")
level5 <- subset(map,map[,2] == 22.5)
fre5_ms <- as.matrix(table(level5[,3])/nrow(level5))
ms_level5 <- cbind(subset_1,fre5_ms)
colnames(ms_level5)=c("subset_1","ms_level5")
level6 <- subset(map,map[,2] == 27.5)
fre6_ms <- as.matrix(table(level6[,3])/nrow(level6))
ms_level6 <- cbind(subset_1,fre6_ms)
colnames(ms_level6)=c("subset_1","ms_level6")
level7 <- subset(map,map[,2] == 32.5)
fre7_ms <- as.matrix(table(level7[,3])/nrow(level7))
ms_level7 <- cbind(subset_1,fre7_ms)
colnames(ms_level7)=c("subset_1","ms_level7")
level8 <- subset(map,map[,2] == 37.5)
fre8_ms <- as.matrix(table(level8[,3])/nrow(level8))
ms_level8 <- cbind(subset_1,fre8_ms)
colnames(ms_level8)=c("subset_1","ms_level8")
level9 <- subset(map,map[,2] == 42.5)
fre9_ms <- as.matrix(table(level9[,3])/nrow(level9))
ms_level9 <- cbind(subset_1,fre9_ms)
colnames(ms_level9)=c("subset_1","ms_level9")
level10 <- subset(map,map[,2] == 47.5)
fre10_ms <- as.matrix(table(level10[,3])/nrow(level10))
ms_level10 <- cbind(subset_1,fre10_ms)
colnames(ms_level10)=c("subset_1","ms_level10")
level11 <- subset(map,map[,2] == 55)
fre11_ms <- as.matrix(table(level11[,3])/nrow(level11))
fre11_ms <- as.matrix(c(fre11_ms,0))
ms_level11 <- cbind(subset_1,fre11_ms)
colnames(ms_level11)=c("subset_1","ms_level11")
level12 <- subset(map,map[,2] == 67.5)
fre12_ms <- as.matrix(table(level12[,3])/nrow(level12))
ms_level12 <- c(0.37254902,0.07843137,0.01960784,0.15686275,0.11764706,0.03921569,0.13725490,0.05882353,0,0.01960784)
ms_level12 <- cbind(subset_1,ms_level12)
colnames(ms_level12)=c("subset_1","ms_level12")
level13 <- subset(map,map[,2] == 87.5)
fre13_ms <- as.matrix(table(level13[,3])/nrow(level13))
fre13_ms <- as.matrix(c(0.24324324,0.27027027,0.08108108,0.02702703,0,0.02702703,0.02702703,0,0.32432432,0))
ms_level13 <- cbind(subset_1,fre13_ms)
colnames(ms_level13)=c("subset_1","ms_level13")
level14 <- subset(map,map[,2] == 130)
fre14_ms <- as.matrix(table(level14[,3])/nrow(level14))
fre14_ms <- as.matrix(c(0.19230769,0.03846154,0.11538462,0.30769231,0.07692308,0.07692308,0,0,0.19230769,0))
ms_level14 <- cbind(subset_1,fre14_ms)
colnames(ms_level14)=c("subset_1","ms_level14")
# merge all the infomation above to get a whole matrix
map2 <- merge(ms_level1, ms_level2, by.ms_level1=c(1), by.ms_level2=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level3, by.map=c(1), by.ms_level3=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level4, by.map=c(1), by.ms_level4=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level5, by.map=c(1), by.ms_level5=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level6, by.map=c(1), by.ms_level6=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level7, by.map=c(1), by.ms_level7=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level8, by.map=c(1), by.ms_level8=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level9, by.map=c(1), by.ms_level9=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level10, by.map=c(1), by.ms_level10=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level11, by.map=c(1), by.ms_level11=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level12, by.map=c(1), by.ms_level12=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level13, by.map=c(1), by.ms_level13=c(1), all.x=TRUE,sort=TRUE )
map2 <- merge(map2, ms_level14, by.map=c(1), by.ms_level14=c(1), all.x=TRUE,sort=TRUE )
map2 <- map2[order(as.numeric(as.character(map2$subset_1))),]
colnames(map2) = c("choice","income_2.5","income_7.5","income_12.5","income_17.5","income_22.5","income_27.5","income_32.5","income_37.5","income_42.5","income47.5","income_55","income_67.5","income_87.5","income_130")
print(map2)


# Question 2
# extract the data of demand as y
demand <- as.matrix(choiceprice[,2])
# extrace the data of prices as x
price2 <- choiceprice[,3:12]
c2 <- as.vector(price2[,2]-price2[,1])
c3 <- as.vector(price2[,3]-price2[,1])
c4 <- as.vector(price2[,4]-price2[,1])
c5 <- as.vector(price2[,5]-price2[,1])
c6 <- as.vector(price2[,6]-price2[,1])
c7 <- as.vector(price2[,7]-price2[,1])
c8 <- as.vector(price2[,8]-price2[,1])
c9 <- as.vector(price2[,9]-price2[,1])
c10 <- as.vector(price2[,10]-price2[,1])
c_all <- cbind(c2,c3,c4,c5,c6,c7,c8,c9,c10)
# choice product 1 as reference, generate a matrix of 4470*10 
price3 <- matrix(c(0),nrow = 4470,ncol = 1)
price4 <- cbind(price3,c_all)
dataset <- matrix(c(0),ncol=10,nrow=4470)
dataset <- cbind(demand,dataset)
# generate dij according to individuals' choices
for(i in 1:4470){
  for(r in 1:10){
    dataset[i,r+1] <- ifelse(dataset[i,1]==r,1,0)
  }
}
dij <- dataset[,2:11]

# write the log likelihood function, the parameters contain 9 intercepts for product 2 to 10 and one coefficient for the whole matrix of prices
loglike <- function(b,x,y){
  # generate the matrix aij of intercepts
  a <- b[2:10]
  a <- sapply(a,rep,4470)
  a2 <- matrix(c(0),nrow = 4470,ncol = 1)
  a <- cbind(a2,a)
  # generate the matrix vij, where vij = x*beta + aij
  x <- price4
  vij <- x*b[1]+a
  # calculate the matrix of pij
  total <- exp(vij[,1])+exp(vij[,2])+exp(vij[,3])+exp(vij[,4])+exp(vij[,5])+exp(vij[,6])+exp(vij[,7])+exp(vij[,8])+exp(vij[,9])+exp(vij[,10])
  pij <- exp(vij)/total
  # calculate the log likelihood equation and return the maximum value
  new_dataset <- dij*log(pij)
  log <- cbind(sum(new_dataset[,1]),sum(new_dataset[,2]),sum(new_dataset[,3]),sum(new_dataset[,4]),sum(new_dataset[,5]),sum(new_dataset[,6]),sum(new_dataset[,7]),sum(new_dataset[,8]),sum(new_dataset[,9]),sum(new_dataset[,10]))
  log <- sum(log[1,])
  return(-log)
}

# let the initial value be a vector of 0
start <- rep(0,10)
# optimize the parameters' value
max_loglike <- optim(start, loglike, x = price4, y = demand, method = "BFGS")$par
max_loglike <- as.matrix(max_loglike)
# Interpretation: 
# Since beta < 0, an increase in the price of one of the products 2 to 10 decreases the probability of choosing that product and increases the probability of choosing other alternatives.


# Question 3
# extract the data of income from demos
income2 <- as.matrix(map[,2])
# generate a 4470*10 matrix for income as y
income2 <- cbind(income2,income2,income2,income2,income2,income2,income2,income2,income2,income2)

# write the log likelihood function, the parameters contain 9 intercepts and 9 coefficients for product 2 to 10
loglike2 <- function(b,x,y){
  x <- income2
  # generate a matrix bij as coefficient matrix
  bij <- c(0,b[1:9])
  bij <- sapply(bij,rep,4470)
  # repeat the process to generate matrix aij
  a <- b[10:18]
  a <- sapply(a,rep,4470)
  a2 <- matrix(c(0),nrow = 4470,ncol = 1)
  a <- cbind(a2,a)
  # generate the matrix vij, where vij = x*bij + aij
  vij <- x*bij+a
  # calculate the matrix of pij
  total <- exp(vij[,1])+exp(vij[,2])+exp(vij[,3])+exp(vij[,4])+exp(vij[,5])+exp(vij[,6])+exp(vij[,7])+exp(vij[,8])+exp(vij[,9])+exp(vij[,10])
  pij <- exp(vij)/total
  # calculate the log likelihood equation and return the maximum value
  new_dataset2 <- dij*log(pij)
  log2 <- cbind(sum(new_dataset2[,1]),sum(new_dataset2[,2]),sum(new_dataset2[,3]),sum(new_dataset2[,4]),sum(new_dataset2[,5]),sum(new_dataset2[,6]),sum(new_dataset2[,7]),sum(new_dataset2[,8]),sum(new_dataset2[,9]),sum(new_dataset2[,10]))
  log2 <- sum(log2[1,])
  return(-log2)
}

# let the initial value be a vector of 0
start <- rep(0,18)
# optimize the parameters' value
max_loglike2 <- optim(start, loglike2, x = income2, y = demand, method = "BFGS")$par
max_loglike2 <- as.matrix(max_loglike2)
# Interpretation: 
# The coefficients for product 4 and 6 are negative, which means that compared to product 1, a higher income leads to reduced likelihood of choosing product 4 and 6.
# The coefficients for product 2, 3, 5, 7, 8, 9 and 10 are positive, which means that compared to product 1, a higher income leads to increased likelihood of choosing product 2, 3, 5, 7, 8, 9 and 10.


# Question 4
# use the coefficients generated from question 2 to calculate the pij matrix
a <- max_loglike[2:10]
a <- sapply(a,rep,4470)
a2 <- matrix(c(0),nrow = 4470,ncol = 1)
a <- cbind(a2,a)
x <- price4
vij <- x*max_loglike[1]+a
total <- exp(vij[,1])+exp(vij[,2])+exp(vij[,3])+exp(vij[,4])+exp(vij[,5])+exp(vij[,6])+exp(vij[,7])+exp(vij[,8])+exp(vij[,9])+exp(vij[,10])
pij <- exp(vij)/total

# use the equation of marginal effect of conditional logit
# generate a 4470*10 matrix which is the repeat of the fist column in pij
pij_1 <- as.matrix(cbind(pij[,1],pij[,1],pij[,1],pij[,1],pij[,1],pij[,1],pij[,1],pij[,1],pij[,1],pij[,1]))
me_1 <- pij_1*(dij-pij)*max_loglike[1,]
# calculate the means of 10 marginal effects for product 1
me_1 <- apply(me_1,2,mean)
# repeat the process above for product 2 to 10
pij_2 <- cbind(pij[,2],pij[,2],pij[,2],pij[,2],pij[,2],pij[,2],pij[,2],pij[,2],pij[,2],pij[,2])
me_2 <- pij_2*(dij-pij)*max_loglike[1,]
me_2 <- apply(me_2,2,mean)
pij_3 <- cbind(pij[,3],pij[,3],pij[,3],pij[,3],pij[,3],pij[,3],pij[,3],pij[,3],pij[,3],pij[,3])
me_3 <- pij_3*(dij-pij)*max_loglike[1,]
me_3 <- apply(me_3,2,mean)
pij_4 <- cbind(pij[,4],pij[,4],pij[,4],pij[,4],pij[,4],pij[,4],pij[,4],pij[,4],pij[,4],pij[,4])
me_4 <- pij_4*(dij-pij)*max_loglike[1,]
me_4 <- apply(me_4,2,mean)
pij_5 <- cbind(pij[,5],pij[,5],pij[,5],pij[,5],pij[,5],pij[,5],pij[,5],pij[,5],pij[,5],pij[,5])
me_5 <- pij_5*(dij-pij)*max_loglike[1,]
me_5 <- apply(me_5,2,mean)
pij_6 <- cbind(pij[,6],pij[,6],pij[,6],pij[,6],pij[,6],pij[,6],pij[,6],pij[,6],pij[,6],pij[,6])
me_6 <- pij_6*(dij-pij)*max_loglike[1,]
me_6 <- apply(me_6,2,mean)
pij_7 <- cbind(pij[,7],pij[,7],pij[,7],pij[,7],pij[,7],pij[,7],pij[,7],pij[,7],pij[,7],pij[,7])
me_7 <- pij_7*(dij-pij)*max_loglike[1,]
me_7 <- apply(me_7,2,mean)
pij_8 <- cbind(pij[,8],pij[,8],pij[,8],pij[,8],pij[,8],pij[,8],pij[,8],pij[,8],pij[,8],pij[,8])
me_8 <- pij_8*(dij-pij)*max_loglike[1,]
me_8 <- apply(me_8,2,mean)
pij_9 <- cbind(pij[,9],pij[,9],pij[,9],pij[,9],pij[,9],pij[,9],pij[,9],pij[,9],pij[,9],pij[,9])
me_9 <- pij_9*(dij-pij)*max_loglike[1,]
me_9 <- apply(me_9,2,mean)
pij_10 <- cbind(pij[,10],pij[,10],pij[,10],pij[,10],pij[,10],pij[,10],pij[,10],pij[,10],pij[,10],pij[,10])
me_10 <- pij_10*(dij-pij)*max_loglike[1,]
me_10 <- apply(me_10,2,mean)
me_all <- rbind(me_1,me_2,me_3,me_4,me_5,me_6,me_7,me_8,me_9,me_10)
# Interpretation:
# Take the first element in the first row in the marginal effect matrix as example, -0.005445123 means that by other prices are unchanged, one unit increase in the price of product 1 will decrease 0.005445123  in the probability to buy the product 1 on average.
# Take the second element in the first row in the marginal effect matrix as example, 0.0257883274 means that by other prices are unchanged, one unit increase in the price of product 2 will increase 0.0257883274 in the probability to buy the product 1 on average.

# generate the pij matrix based on the coefficients got from question 3
x2 <- income2
bij2 <- c(0,max_loglike2[1:9])
bij2 <- sapply(bij2,rep,4470)
a22 <- max_loglike2[10:18,]
a22 <- sapply(a22,rep,4470)
a222 <- matrix(c(0),nrow = 4470,ncol = 1)
a22 <- cbind(a222,a22)
vij2 <- x2*bij2+a22
total2 <- exp(vij2[,1])+exp(vij2[,2])+exp(vij2[,3])+exp(vij2[,4])+exp(vij2[,5])+exp(vij2[,6])+exp(vij2[,7])+exp(vij2[,8])+exp(vij2[,9])+exp(vij2[,10])
pij2 <- exp(vij2)/total2

# calculate the probability weighted average of the coefficients
betaba1 <- as.matrix(pij2*bij2)
betaba1 <- as.matrix((apply(betaba1,1,sum)))
betaba1 <- cbind(betaba1,betaba1,betaba1,betaba1,betaba1,betaba1,betaba1,betaba1,betaba1,betaba1)
# use the equation of marginal effect of multinomial logit
betaba1 <- as.matrix(bij2-betaba1)
me2_all <- as.matrix(pij2*betaba1)
me2_all <- as.matrix(apply(me2_all,2,mean))
# Interpretagtion:
# Take the first element in the marginal effect matrix as example, -0.0010504137 means that one unit increase in income will decrease -0.0010504137 in probability of choosing product 1 on average.


# Question 5
#(1)
# write the log likelihood function, the parameters contain 9 intercepts and 10 coefficients
x3 <- cbind(price4,income2)
mixed_likelihood <- function(b,x,y){
  # repeat the process before
  a <- b[11:19]
  a <- sapply(a,rep,4470)
  a2 <- matrix(c(0),nrow = 4470,ncol = 1)
  a <- cbind(a2,a)
  x <- x3[,1:10]
  vij <- x*b[1]
  bij <- c(0,b[2:10])
  bij <- sapply(bij,rep,4470)
  x2 <- x3[,11:20]
  vij2 <- x2*bij
  # generate the vij matrix, which is combined by vij from conditional and multinomial logit
  vij3 <- vij+vij2+a
  total3 <- exp(vij3[,1])+exp(vij3[,2])+exp(vij3[,3])+exp(vij3[,4])+exp(vij3[,5])+exp(vij3[,6])+exp(vij3[,7])+exp(vij3[,8])+exp(vij3[,9])+exp(vij3[,10])
  # generate the new pij matrix
  pij3 <- exp(vij3)/total3
  new_dataset3 <- dij*log(pij3)
  # calculate the log likelihood equation and return the maximum value
  log3 <- cbind(sum(new_dataset3[,1]),sum(new_dataset3[,2]),sum(new_dataset3[,3]),sum(new_dataset3[,4]),sum(new_dataset3[,5]),sum(new_dataset3[,6]),sum(new_dataset3[,7]),sum(new_dataset3[,8]),sum(new_dataset3[,9]),sum(new_dataset3[,10]))
  log3 <- sum(log3[1,])
  return(-log3)
}

# let the initial value be a vector of 0
start <- rep(0,19)
# optimize the parameters' value
max_loglike3 <- optim(start, mixed_likelihood, x = x3, y = demand, method = "BFGS")$par
max_loglike3 <- as.matrix(max_loglike3)

#(2)
# We remove the data of product 10 and repeat the process of (1).
mixed_likelihood2 <- function(b,x,y){
  a <- b[10:17]
  a <- sapply(a,rep,4470)
  a2 <- matrix(c(0),nrow = 4470,ncol = 1)
  a <- cbind(a2,a)
  x <- x3[,1:9]
  vij <- x*b[1]
  bij <- c(0,b[2:9])
  bij <- sapply(bij,rep,4470)
  x2 <- x3[,11:19]
  vij2 <- x2*bij
  vij3 <- vij+vij2+a
  total3 <- exp(vij3[,1])+exp(vij3[,2])+exp(vij3[,3])+exp(vij3[,4])+exp(vij3[,5])+exp(vij3[,6])+exp(vij3[,7])+exp(vij3[,8])+exp(vij3[,9])
  pij3 <- exp(vij3)/total3
  new_dataset3 <- dij[,1:9]*log(pij3)
  log4 <- cbind(sum(new_dataset3[,1]),sum(new_dataset3[,2]),sum(new_dataset3[,3]),sum(new_dataset3[,4]),sum(new_dataset3[,5]),sum(new_dataset3[,6]),sum(new_dataset3[,7]),sum(new_dataset3[,8]),sum(new_dataset3[,9]))
  log4 <- sum(log4[1,])
  return(-log4)
}
start <- rep(0,17)
max_loglike4 <- optim(start, mixed_likelihood2, x = x3, y = demand, method = "BFGS")$par
max_loglike4 <- as.matrix(max_loglike4)

#(3)
# We reduce the coefficient and intercept of product 10 in parameters got from (1). Then use this new vector and the log likelihood function in (2) to get the maximum value
d1 <- as.vector(max_loglike3[1:9,])
d2 <- as.vector(max_loglike3[11:18,])
max_loglike5 <- c(d1,d2)
max_result1 <- mixed_likelihood2(max_loglike5)
max_result2 <- mixed_likelihood2(max_loglike4)
# calculate the IIA test statistic
MTT <- -2*(max_result2-max_result1)
# Under the degree of freedom of 9, we can check the chart of chi-square at 5% siginificance level and get the critical value of 16.919. Then we fail to reject the null hypothesis that the two results in (1) and (2) are the same. 
# We get the result because that the market share of product 10 is only 7%, which means that it may effect very little on the whole market, so we can see very small difference after excluding product 10.


write.csv(product_ms, file = "product_ms.csv")
write.csv(brand_ms, file = "brand_ms.csv")
write.csv(type_ms, file = "type_ms.csv")
write.csv(map2, file = "map2.csv")