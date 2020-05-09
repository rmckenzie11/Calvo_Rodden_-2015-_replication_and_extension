
data <- read.dta("Uk_postwar_with three parties.dta") 
data2 <- read.dta("party aggregates_wide.dta") 
newdata2 <- data.frame(c("2015","2017", "2019"), 0,0,0,0,0,0,0,0)
names(newdata2) <- c("election", "seats1", "v1", "seats2", "v2", "seats3", "v3", "seats4", "v4")
data2 <- rbind(data2,newdata2)
data3 <- read.csv("uk_results.csv") %>%
  filter(election %in% c("2015", "2017", "2019")) %>%
  rename(
    year = election,
    total = total_votes,
    pev = electorate,
    votesl =  natSW_votes,
    nat = natSW_share,
    votess = lab_votes,
    votesc = con_votes,
    newothers = oth_votes,
    seatname = constituency,
    labor = lab_share,
    conservatives = con_share
  ) %>%
  mutate(year = as.numeric(levels(year)[year]),
         seatname = as.character(seatname),
         newothers = newothers + lib_votes) 

data3[is.na(data3)] <- 0

data3 <- na.omit(data3) %>%
  mutate(sumparties = votesl + votess + votesc)

newdata <- data %>%
  full_join(data3) %>%
  dplyr::select(year,total,pev,votesl,votess,votesc,seatname,sumparties,newothers,labor,conservatives,nat)

newdata[is.na(newdata)] <- 0 
  
dataElec <- split(newdata, newdata$year, drop = FALSE)

my.reps=19
my.gini = array(0, dim=c(my.reps,4))
my.sd = array(0, dim=c(my.reps,4))
my.mean = array(0, dim=c(my.reps,4))
my.median = array(0, dim=c(my.reps,4))
my.votes = array(0, dim=c(my.reps,4))
my.pvotes = array(0, dim=c(my.reps,4))
my.seats = array(0, dim=c(my.reps,4))
theo.cor = array(0, dim=c(my.reps,4,4))
my.cor = array(0, dim=c(my.reps,4,4))
CV = array(0, dim=c(my.reps,4,4))

for(i in 1:my.reps){
  party3 <- dataElec[[i]]$votesl/dataElec[[i]]$total
  party2 <- dataElec[[i]]$votess/dataElec[[i]]$total
  party1 <- dataElec[[i]]$votesc/dataElec[[i]]$total
  party4 <- dataElec[[i]]$newothers/dataElec[[i]]$total
  
  V<-as.matrix(cbind(party1,party2,party3,party4))
  S<-dhont(d=V,M=rep(1,length(party1)),U=1)
  pV<- V/rowSums(V) # percent vote by party and district
  my.mean[i,]<-apply(pV, 2, mean)
  my.median[i,]<-apply(pV, 2, median)
  my.sd[i,]<- apply(pV, 2, sd)
  CV<-my.sd/my.mean
  my.votes[i,]<-colSums(V) # sum of votes all districts
  my.pvotes[i,]<-my.votes[i,]/sum(my.votes[i,]) # percent vote by party and election
  
  my.seats[i,]<-colSums(S) 
  my.gini[i,]<- c(gini(party1),gini(party2),gini(party3),gini(party4))
  
} 

plot(my.gini,CV, ylab="Empirical Coefficient of Variation", xlab="GINI")


#####################################
##
## Winbugs models
##
#####################################

##
## GINI RESULTS USING WINBUGS
##

UK.mean.sim<- seats.bugs(votes=my.pvotes, seats=round(my.seats), gini= my.gini, model=0.2, n.burnin=12200, n.iter=15800)
UK.0.sim<- seats.bugs(votes=my.pvotes, seats=round(my.seats), gini=my.gini, model=0, n.burnin=1220, n.iter=1580)
UK.0bis.sim<- seats.bugs(votes=my.pvotes, seats=round(my.seats), gini=my.gini, model=1, n.burnin=1220, n.iter=1580)

quantile(UK.0.sim$median$rho, probs=c(.1,.5,.9))
quantile(UK.1.sim$median$rho, probs=c(.1,.5,.9))
quantile(UK.0bis.sim$median$rho, probs=c(.1,.5,.9))

mssMean <- sum((UK.mean.sim$median$p-my.seats/rowSums(my.seats))^2)
mssRho <- sum((UK.rho.sim$median$p-my.seats/rowSums(my.seats))^2)
mss0 <- sum((UK.0.sim$median$p-my.seats/rowSums(my.seats))^2)
mss0bis <- sum((UK.0bis.sim$median$p-my.seats/rowSums(my.seats))^2)
mss1 <- sum((UK.1.sim$median$p-my.seats/rowSums(my.seats))^2)

devianceMean <- quantile(UK.mean.sim$sims.matrix[,length(UK.mean.sim$sims.matrix[1,])], prob=c(.2,.5,.8))
devianceRho <- quantile(UK.rho.sim$sims.matrix[,length(UK.rho.sim$sims.matrix[1,])], prob=c(.2,.5,.8))
deviance0 <- quantile(UK.0.sim$sims.matrix[,length(UK.0.sim$sims.matrix[1,])], prob=c(.2,.5,.8))
deviance0bis <- quantile(UK.0bis.sim$sims.matrix[,length(UK.0bis.sim$sims.matrix[1,])], prob=c(.2,.5,.8))
deviance1 <- quantile(UK.1.sim$sims.matrix[,length(UK.1.sim$sims.matrix[1,])], prob=c(.2,.5,.8))

delta1 <- quantile(UK.0.sim$sims.matrix[,77], prob=c(.2,.5,.8))
delta2 <- quantile(UK.0.sim$sims.matrix[,78], prob=c(.2,.5,.8))

summary(lm(c(UK.rho.sim$median$p)~c(my.seats/rowSums(my.seats))))
summary(lm(c(UK.0.sim$median$p)~c(my.seats/rowSums(my.seats))))

summary(lm(c(UK.rho.sim$mean$p)~c(my.seats/rowSums(my.seats))))
summary(lm(c(UK.0.sim$mean$p)~c(my.seats/rowSums(my.seats))))

op<-par(mfrow = c(1, 2),ann=TRUE)
plot(my.seats/rowSums(my.seats)~UK.rho.sim$median$p, ylab="Observed Seat Shares", xlab="Predicted Seat Shares (Fixed Rho Model)")
abline(lm(c(my.seats/rowSums(my.seats))~c(UK.rho.sim$median$p)), col=2, lty=2)
text(.1, .5, paste("DIC ",devianceRho[2], sep=""))
plot(my.seats/rowSums(my.seats)~UK.0.sim$median$p, ylab="Observed Seat Shares", xlab="Predicted Seat Shares (Full Model)")
abline(lm(c(my.seats/rowSums(my.seats))~c(UK.0.sim$median$p)), col=2, lty=2)
text(.1, .5, paste("DIC ",deviance0[2], sep=""))
par(op)

op<-par(mfrow = c(2, 2),ann=TRUE)
plot(my.seats/rowSums(my.seats)~UK.rho.sim$mean$p)
abline(lm(c(my.seats/rowSums(my.seats))~c(UK.rho.sim$mean$p)), col=2, lty=2)
plot(my.seats/rowSums(my.seats)~UK.0.sim$mean$p)
abline(lm(c(my.seats/rowSums(my.seats))~c(UK.0.sim$mean$p)), col=2, lty=2)
plot(my.seats/rowSums(my.seats)~UK.mean.sim$mean$p)
abline(lm(c(my.seats/rowSums(my.seats))~c(UK.mean.sim$mean$p)), col=2, lty=2)
par(op)

load("UK.mean.sim")
load("UK.rho.sim")
load("UK.1.sim")
load("UK.mean.sim2")
load("UK.0.sim")
load("UK.0bis.sim")

my.rho<- UK.0.sim$mean$rho
my.sd.rho<- UK.0.sim$sd$rho
my.rho.t<- exp(sqrt((1-my.mean)/my.mean))

##
## FIGURE 5
##

plot(1:19, my.rho[,1], ylim=c(1,5.2), xlim=c(0,20), pch=15, cex=1.2, xlab="Election Year", ylab="Majoritarian Bias",  xaxt='n', main="GINI Model")
for(i in 1:19){
  segments(i,my.rho[i,1]+1.96*my.sd.rho[i,1],i,my.rho[i,1]-1.96*my.sd.rho[i,1])
  segments(i-.15,1.5,i-.15,3.7, lty=4, col="grey")
  text(i+.4,1.2, data2[i,1])
}
points((1:19)+.25,my.rho[,2], pch=16, col=2, cex=1.2)
for(i in 1:19){
  segments(i+.25,my.rho[i,2]+1.96*my.sd.rho[i,2],i+.25,my.rho[i,2]-1.96*my.sd.rho[i,2])
}
points((1:16)+.5,my.rho[1:16,3], pch=17, col=3, cex=1.2)
points((17:19)+.5,my.rho[17:19,3], pch=25, col="coral", bg = "coral", cex=1.2)
for(i in 1:19){
  segments(i+.5,my.rho[i,3]+1.96*my.sd.rho[i,3],i+.5,my.rho[i,3]-1.96*my.sd.rho[i,3])
}
points((1:19)+.75,my.rho[,4], pch=18, col=4, cex=1.2)
for(i in 1:19){
  segments(i+.75,my.rho[i,4]+1.96*my.sd.rho[i,4],i+.75,my.rho[i,4]-1.96*my.sd.rho[i,4])
}
legend("topright",col=c(1,2,3,"coral",4), pt.bg = c(1,2,3,"coral",4), legend=c("Conservatives","Labor","Liberals","SNP & PC", "Others"),pch =c(15,16,17,25,18), cex=.7)
segments(8-.15,1.5,8-.15,3.7, lty=4, col="black")
#text(1.2, 3.2, expression(paste(delta," = "," 0.81 ",sep=""), cex = 1))
text(1, 5.1, expression(alpha[1]), cex = 1)
text(1.8, 5.1, " = 3.69", cex = 1)
text(3, 5.1, expression(alpha[2]), cex = 1)
text(3.8, 5.1, " = -1.70", cex = 1)




#######
##
## FIGURE 7
##
#######

seat.hat.rm<- exp(rowMeans(UK.0.sim$median$rho)*log(my.pvotes))/rowSums(exp(rowMeans(UK.0.sim$median$rho)*log(my.pvotes)))
seat.hat<- exp(UK.0.sim$median$rho*log(my.pvotes))/rowSums(exp(UK.0.sim$median$rho*log(my.pvotes)))

#win.metafile(filename = "Actual vs observed rho.wmf", width = 10, height = 10, pointsize = 12, restoreConsole = TRUE)
plot(round(seat.hat.rm[,1],3)*100,round(seat.hat[,1],3)*100, pch=16, xlim=c(0,72), ylim=c(0,72), xlab="Expected Seats (Fixed Rho)", ylab="Expected Seats(Actual Rho)", cex=1)
points(round(seat.hat.rm[,2],3)*100,round(seat.hat[,2],3)*100, pch=17,col=2, cex=1)
points(round(seat.hat.rm[1:16,3],3)*100,round(seat.hat[1:16,3],3)*100, pch=18,col=4, cex=1)
points(round(seat.hat.rm[17:19,3],3)*100,round(seat.hat[17:19,3],3)*100, pch=25,col="coral", bg = "coral", cex=1)
legend("topleft",col=c(1,2,"coral", 3), pt.bg = c(1,2,"coral", 3), legend=c("Conservatives","Labor", "SNP & PC", "Liberals"),pch =c(16,17,25,18), cex=1)
abline(0,1)
#dev.off()


#######
##
## FIGURE 7 USING CV 
##
#######

seat.hat.rm<- exp(rowMeans(UK.1.sim$median$rho)*log(my.pvotes))/rowSums(exp(rowMeans(UK.1.sim$median$rho)*log(my.pvotes)))
seat.hat<- exp(UK.1.sim$median$rho*log(my.pvotes))/rowSums(exp(UK.1.sim$median$rho*log(my.pvotes)))

#win.metafile(filename = "Actual vs observed rho.wmf", width = 10, height = 10, pointsize = 12, restoreConsole = TRUE)
plot(round(seat.hat.rm[,1],3)*100,round(seat.hat[,1],3)*100, pch=16, xlim=c(0,72), ylim=c(0,72), xlab="Expected Seats (Fixed Rho)", ylab="Expected Seats(Actual Rho)", cex=1)
points(round(seat.hat.rm[,2],3)*100,round(seat.hat[,2],3)*100, pch=17,col=2, cex=1)
points(round(seat.hat.rm[,3],3)*100,round(seat.hat[,3],3)*100, pch=18,col=4, cex=1)
legend("topleft",col=c(1,2,4),legend=c("Conservatives","Labor", "Liberals"),pch =c(16,17,18), cex=1)
abline(0,1)
#dev.off()


##############
##############
##############
# END
##############
##############
##############

UK.mean.sim <- seats.bugs(votes=my.pvotes, seats=round(my.seats), gini= CV, model=0.2, n.burnin=1220, n.iter=1580)

UK.rho.sim <- seats.bugs(votes=my.pvotes, seats=round(my.seats), gini= CV, model=0.3, n.burnin=1220, n.iter=1580)

UK.1.sim<- seats.bugs(votes=my.pvotes, seats=round(my.seats), gini= CV, model=0, n.burnin=1220, n.iter=1580)

UK.mean.sim2<- seats.bugs(votes=my.pvotes, seats=round(my.seats), gini= my.gini, model=0.2, n.burnin=1220, n.iter=1580)

UK.0.sim<- seats.bugs(votes=my.pvotes, seats=round(my.seats), gini=my.gini, model=0, n.burnin=1220, n.iter=1580)

UK.0bis.sim<- seats.bugs(votes=my.pvotes, seats=round(my.seats), gini=my.gini, model=1, n.burnin=1220, n.iter=1580)

save(UK.mean.sim, file = "UK.mean.sim")
save(UK.rho.sim, file = "UK.rho.sim")
save(UK.1.sim, file = "UK.1.sim")
save(UK.mean.sim2, file = "UK.mean.sim2")
save(UK.0.sim, file = "UK.0.sim")
save(UK.0bis.sim, file = "UK.0bis.sim")