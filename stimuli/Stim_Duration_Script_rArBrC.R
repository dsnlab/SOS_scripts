# Generate SOS Video Block Order & Timing

# Load Libraries
library(crossdes)
library(tidyr)

# Williams Design
williams <- get.plan(trt = 3, k = 3, maxsub = 64)
                     2
                     1
                     
williams <- c(t(williams))

# MOLS (mutually orthogonal latin squares) Design
mols <- get.plan(trt = 3, k = 3, maxsub = 64)
                 3
                 1

mols <- c(t(mols))

#Generate Task Timings
set.seed(49928436)
dur1 <- rgamma(n = 18,shape = 16, rate = 1)
set.seed(40796324)
dur2 <- rgamma(n = 18,shape = 16, rate = 1)
set.seed(10823246)
dur3 <- rgamma(n = 18,shape = 16, rate = 1)
set.seed(96381362)
dur4 <- rgamma(n = 18,shape = 16, rate = 1)

#Generate Rest Timings
set.seed(24626349)
restA <- rgamma(n = 18, shape = 10, rate = 1)
set.seed(16437392)
restB <- rgamma(n = 18, shape = 10, rate = 1)
set.seed(81105672)
restC <- rgamma(n = 18, shape = 10, rate = 1)
set.seed(92744420)
restD <- rgamma(n = 18, shape = 10, rate = 1)

#Check that all runs are under 8 mins
sum(dur1+restA)/60
sum(dur2+restB)/60
sum(dur3+restC)/60
sum(dur4+restD)/60

#Inserting rest blocks in order vectors
order_A <- rep(0, length(williams)*2)
order_A[seq(2,36,by=2)] <- williams
order_A <- append(order_A, 0, after = length(order_A))

order_Am <- rep(0, length(mols)*2)
order_Am[seq(2,36,by=2)] <- mols
order_Am <- append(order_Am, 0, after = length(order_A))

order_B <- rep(0, length(williams)*2)
order_B[seq(2,36,by=2)] <- williams
order_B <- append(order_B, 0, after = length(order_B))

order_Bm <- rep(0, length(mols)*2)
order_Bm[seq(2,36,by=2)] <- mols
order_Bm <- append(order_Bm, 0, after = length(order_B))

order_C <- rep(0, length(williams)*2)
order_C[seq(2,36,by=2)] <- williams
order_C <- append(order_C, 0, after = length(order_C))

order_Cm <- rep(0, length(mols)*2)
order_Cm[seq(2,36,by=2)] <- mols
order_Cm <- append(order_Cm, 0, after = length(order_C))

order_D <- rep(0, length(williams)*2)
order_D[seq(2,36,by=2)] <- williams
order_D <- append(order_D, 0, after = length(order_D))

order_Dm <- rep(0, length(mols)*2)
order_Dm[seq(2,36,by=2)] <- mols
order_Dm <- append(order_Dm, 0, after = length(order_D))

#Inserting rest blocks in duration vectors
dur_A <- rep(0, length(order_A))
dur_A[seq(2,36,by=2)] <- dur1
dur_A[seq(1,35,by=2)] <- restA

dur_B <- rep(0, length(order_B))
dur_B[seq(2,36,by=2)] <- dur2
dur_B[seq(1,35,by=2)] <- restB

dur_C <- rep(0, length(order_C))
dur_C[seq(2,36,by=2)] <- dur3
dur_C[seq(1,35,by=2)] <- restC

dur_D <- rep(0, length(order_D))
dur_D[seq(2,36,by=2)] <- dur4
dur_D[seq(1,35,by=2)] <- restD

#Finalizing Baseline Padding at End of Run to Reflect 240 Volumes (= 8min run)
dur_A[37]=(480-sum(as.numeric(dur_A)))
dur_B[37]=(480-sum(as.numeric(dur_B)))
dur_C[37]=(480-sum(as.numeric(dur_C)))
dur_D[37]=(480-sum(as.numeric(dur_D)))

#Combining Orders & Durations
SetA <- data.frame(order_A, dur_A)
SetB <- data.frame(order_B, dur_B)
SetC <- data.frame(order_C, dur_C)
SetD <- data.frame(order_D, dur_D)

SetAm <- data.frame(order_Am, dur_A)
SetBm <- data.frame(order_Bm, dur_B)
SetCm <- data.frame(order_Cm, dur_C)
SetDm <- data.frame(order_Dm, dur_D)

#USE THIS ONE
#Set A has equal durations across conditions
sum(SetA$dur_A[2],SetA$dur_A[12],SetA$dur_A[16],SetA$dur_A[24],SetA$dur_A[26],SetA$dur_A[34])  
sum(SetA$dur_A[4],SetA$dur_A[8],SetA$dur_A[18],SetA$dur_A[22],SetA$dur_A[30],SetA$dur_A[32]) 
sum(SetA$dur_A[6],SetA$dur_A[10],SetA$dur_A[14],SetA$dur_A[20],SetA$dur_A[28],SetA$dur_A[36]) 

#Set Am has unequal durations across conditions
sum(SetAm$dur_A[2],SetAm$dur_A[12],SetAm$dur_A[16],SetAm$dur_A[20],SetAm$dur_A[28],SetAm$dur_A[36])  
sum(SetAm$dur_A[4],SetAm$dur_A[8],SetAm$dur_A[18],SetAm$dur_A[24],SetAm$dur_A[26],SetAm$dur_A[34]) 
sum(SetAm$dur_A[6],SetAm$dur_A[10],SetAm$dur_A[14],SetAm$dur_A[22],SetAm$dur_A[30],SetAm$dur_A[32]) 

#Set B has unequal durations across conditions
sum(SetB$dur_B[2],SetB$dur_B[12],SetB$dur_B[16],SetB$dur_B[24],SetB$dur_B[26],SetB$dur_B[34])  
sum(SetB$dur_B[4],SetB$dur_B[8],SetB$dur_B[18],SetB$dur_B[22],SetB$dur_B[30],SetB$dur_B[32]) 
sum(SetB$dur_B[6],SetB$dur_B[10],SetB$dur_B[14],SetB$dur_B[20],SetB$dur_B[28],SetB$dur_B[36]) 

#Set Bm has fairly equal durations across conditions
sum(SetBm$dur_B[2],SetBm$dur_B[12],SetBm$dur_B[16],SetBm$dur_B[20],SetBm$dur_B[28],SetBm$dur_B[36])  
sum(SetBm$dur_B[4],SetBm$dur_B[8],SetBm$dur_B[18],SetBm$dur_B[24],SetBm$dur_B[26],SetBm$dur_B[34]) 
sum(SetBm$dur_B[6],SetBm$dur_B[10],SetBm$dur_B[14],SetBm$dur_B[22],SetBm$dur_B[30],SetBm$dur_B[32]) 

#Set C has unequal durations across conditions
sum(SetC$dur_C[2],SetC$dur_C[12],SetC$dur_C[16],SetC$dur_C[24],SetC$dur_C[26],SetC$dur_C[34])  
sum(SetC$dur_C[4],SetC$dur_C[8],SetC$dur_C[18],SetC$dur_C[22],SetC$dur_C[30],SetC$dur_C[32]) 
sum(SetC$dur_C[6],SetC$dur_C[10],SetC$dur_C[14],SetC$dur_C[20],SetC$dur_C[28],SetC$dur_C[36]) 

#USE THIS ONE
#Set Cm has equal durations across conditions
sum(SetCm$dur_C[2],SetCm$dur_C[12],SetCm$dur_C[16],SetCm$dur_C[20],SetCm$dur_C[28],SetCm$dur_C[36])  
sum(SetCm$dur_C[4],SetCm$dur_C[8],SetCm$dur_C[18],SetCm$dur_C[24],SetCm$dur_C[26],SetCm$dur_C[34]) 
sum(SetCm$dur_C[6],SetCm$dur_C[10],SetCm$dur_C[14],SetCm$dur_C[22],SetCm$dur_C[30],SetCm$dur_C[32]) 

#Set D has unequal durations across conditions
sum(SetD$dur_D[2],SetD$dur_D[12],SetD$dur_D[16],SetD$dur_D[24],SetD$dur_D[26],SetD$dur_D[34])  
sum(SetD$dur_D[4],SetD$dur_D[8],SetD$dur_D[18],SetD$dur_D[22],SetD$dur_D[30],SetD$dur_D[32]) 
sum(SetD$dur_D[6],SetD$dur_D[10],SetD$dur_D[14],SetD$dur_D[20],SetD$dur_D[28],SetD$dur_D[36]) 

#Set Dm has unequal durations across conditions
sum(SetDm$dur_D[2],SetDm$dur_D[12],SetDm$dur_D[16],SetDm$dur_D[20],SetDm$dur_D[28],SetDm$dur_D[36])  
sum(SetDm$dur_D[4],SetDm$dur_D[8],SetDm$dur_D[18],SetDm$dur_D[24],SetDm$dur_D[26],SetDm$dur_D[34]) 
sum(SetDm$dur_D[6],SetDm$dur_D[10],SetDm$dur_D[14],SetDm$dur_D[22],SetDm$dur_D[30],SetDm$dur_D[32]) 
