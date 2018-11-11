## FUNCTION: 10-fold cross-validation
## returns a data frame of results, one line per iteration
tenFold <- function(df) {
	size <- nrow(df)
	tenth <- round(nrow(df) / 10, digits=0)
	ten <- 1:10
	results <- data.frame()
	for (i in ten) {
		print(paste("Fold:", i))
		test <- df[((i*tenth)-(tenth-1)):(i*tenth),]
		if (i==10 & size>(i*tenth)) {
			remainder <- df[(i*tenth+1):size,]
			test <- rbind(test, remainder)
		}
		if (i==1) {
			train <- df[(i*tenth+1):size,]
		} else if (i==10) {
			train <- df[1:((i-1)*tenth),]
		} else {
			train1 <- df[1:((i-1)*tenth),]
			train2 <- df[(i*tenth+1):size,]
			train <- rbind(train1, train2)
		}
		print(paste("Training data:", nrow(train), "rows; Test data:", nrow(test), "rows."))
		lineout <- model(i, test, train)
		results <- rbind(results, lineout)
	}
	results
}


## classifier
p <- c()
r <- c()
p <- append(p, eval[1])
r <- append(r, eval[2])
meanP <- mean(p)
meanR <- mean(r)
Fmeasure <- 2*((meanP*meanR)/(meanP+meanR))
