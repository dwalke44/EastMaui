# build test data from dummy vars + 4 fish species habitat

insectTest.mat = habitatInput.mat[ ,c(1,2,12, 13:16)]

mosq = rep(0.5, 308)
damsel1 = rep(1.0, 309)
damsel2 = rep(1.5, 309)
damsel3 = rep(2.0, 309)

habFunInput.mat = cbind(mosq, damsel1, damsel2,damsel3)

habFunInput = habFunInput.mat
habInput = insectTest.mat
