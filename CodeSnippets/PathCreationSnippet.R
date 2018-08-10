
createdPath = paste0("Output - ", substring(timestamp(), 14, 19))

dir.create(path = createdPath)

x = matrix(data = rnorm(n = 200, mean = 4, sd = 2), nrow = 50, ncol = 4)

workType = "randomMatrix"

write.csv(x, file = paste0(createdPath,"/" , workType," - ", substring(timestamp(), 14, 22),".",substring(timestamp(), 24, 25),  ".csv"))

