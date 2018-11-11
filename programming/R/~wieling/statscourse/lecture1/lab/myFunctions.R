# Custom functions for R lab session, 2013-12-02
# Martijn Wieling
library(lme4)
library(car)
library(lattice)

# Source of these functions: https://raw.github.com/aufrank/R-hacks/master/regression-utils.R (made by Florian Jaeger)
c. <- function (x) scale(x, scale = FALSE)
z. <- function (x) scale(x)

hasConverged = function(model) { 
	dd <- model@optinfo$derivs
	with(dd,max(abs(solve(Hessian,gradient)))<2e-3) # TRUE, so probably converged
}

# myCoefPlot2D wil plot the adjustments to the slopes of two predictors
# with respect to one random-effect factor. It will display by default
# 5 words at the outer points of the plot
#
# parameters: model: result of lmer
#             ranfact: name of the random effect factor between quotes
#             pred1: name of first predictor between quotes
#             pred2: name of second predictor between quotes
#             showGrey: show a grey box in a quadrant around the origin
#             nrWords: nr of printed words in the plots at the end points
myCoefPlot2D = function(model,ranfact,pred1,pred2,showGrey=F,nrWords=5) { 
	smry = summary(model)
    lmercoefsfixed = coef(smry)
	if (rownames(lmercoefsfixed)[1] == "(Intercept)") { 
		rownames(lmercoefsfixed)[1] = "Intercept"
	}
	a=coef(model)
	w=data.frame(a[ranfact])
	colnames(w) = gsub("\\.","",gsub(paste(ranfact,"\\.*",sep=""),"",colnames(w)))
	w=w[order(w[,pred1]),]
	w2=rbind(head(w,nrWords), tail(w,nrWords))
	rownames(w2) = gsub("\\*.*","",rownames(w2))

	if (!is.null(pred2)) { 
		
		plot(w[,c(pred1,pred2)], xlab="", ylab="", cex.axis=1.4,pch=20,type="n", xlim=c(min(w[,pred1])-abs(max(w[,pred1])-min(w[,pred1]))*0.1, max(w[,pred1])+abs(max(w[,pred1])-min(w[,pred1]))*0.1))

		if (lmercoefsfixed[pred1,"Estimate"] < 0) { 
			xmin = par()$usr[1]
			xmax = 0 
		} else { 
			xmin = 0 
			xmax = par()$usr[2]
		}
		if (lmercoefsfixed[pred2,"Estimate"] < 0) { 
			ymin = par()$usr[3]
			ymax = 0 
		} else { 
			ymin = 0 
			ymax = par()$usr[4]
		}
		if (showGrey) { 
			polygon(c(xmin,xmin,xmax,xmax),c(ymin,ymax,ymax,ymin),col="#EEEEEE",border=NA)
		}
		points(w[,c(pred1,pred2)], xlab="", ylab="", cex.axis=1.4,pch=20,xlim=c(min(w[,pred1])-abs(min(w[,pred1]))*0.2, max(w[,pred1])+abs(max(w[,pred1]))*0.2))
		abline(v=lmercoefsfixed[pred1,"Estimate"],h=lmercoefsfixed[pred2,"Estimate"],lty=2)
		abline(v=0,h=0,lty=1)
		text(w2[,c(pred1, pred2)], paste("", rownames(w2)), adj=0, cex=1.15)
		box()
		title(xlab=paste("Coefficient",pred1), ylab=paste("Coefficient",pred2),cex.lab=1.5)
	}
	else { 
		w=w[order(w[,pred1]),]
		rownames(w) = gsub("\\*.*","",rownames(w))
		plot(w[,pred1],xlab="",ylab="",xlim=c(-10,length(w[,pred1])+10))
		abline(h=lmercoefsfixed[pred1,"Estimate"],lty=2)
		abline(h=0,lty=1)

		for (i in 1:length(w[,pred1])) { 
			if (i <= nrWords | i > length(w[,pred1])-nrWords) { 
				if (i%%2 ==0) { text(x=i,y=w[,pred1][i], paste( rownames(w)[i],""), pos=2,cex=1)}
				else { text(x=i,y=w[,pred1][i], paste("", rownames(w)[i]), pos=4,cex=1)}
			}
		}
		title(xlab="Sorted index",ylab=paste("Coefficient",pred1),cex.lab=1.5)
	}
}


# myCoefPlot wil plot the adjustments to the slope of one predictor
# with respect to one random-effect factor. It will display by default
# 5 words at the outer points of the plot
#
# parameters: model: result of lmer
#             ranfact: name of the random effect factor between quotes
#             pred1: name of predictor between quotes
#             showGrey: show a grey box in a quadrant around the origin
#             nrWords: nr of printed words in the plots at the end points
myCoefPlot = function(model,ranfact,pred1,showGrey=F,nrWords=5) { 
	myCoefPlot2D(model,ranfact,pred1,NULL,showGrey,nrWords)
}





# myInterceptPlot will display the individual intercepts for 
# a random-effect factor
#
# parameters: dat: data set
#             dependent: dependent variable between quotes
#             pred: predictor variable between quotes
#             random: random-effect factor between quotes
#             model1: result of lmer (solid red line)
#             showFixedSlope: if the slope and intercepts of the general model should be displayed (dashed black line)
myInterceptPlot = function(dat,dependent,pred,random,model1,showFixedSlope=T) {
	# based on p.249 of Baayen (2008)

	xyplot(as.formula(paste(dependent,"~",pred,"|",random)), data=dat,
		panel = function(x,y,subscripts) { 
			panel.xyplot(x,y)
			itm = as.character(dat[subscripts[1],random])
			str = paste("coefs = as.numeric(unlist(coef(model1)$",random,"[itm,]))",sep="")
			eval(parse(text=str))
			panel.abline(coefs,col="red",lty=1,lwd=2)

			if (showFixedSlope) { 
				str = paste("coefs2 = fixef(model1)[c(\"(Intercept)\")]",sep="")
				eval(parse(text=str))
				panel.abline(coefs2,col="black",lty=2,lwd=2)
			}
		})
}


# myXYPlot will display the individual intercepts and slopes for 
# the random-effect factor for two different models
# the dependent has to occur in the fixed-effects
#
# parameters: dat: data set
#             dependent: dependent variable between quotes
#             pred: predictor variable between quotes
#             random: random-effect factor between quotes
#             model1: result of lmer (displayed with red solid line)
#             showFixedSlope: if the slope and intercepts of the general model should be displayed (dashed black line)

myXYPlot = function(dat,dependent,pred,random,model1,showFixedSlope=T) {
	# based on p.249 of Baayen (2008)

	xyplot(as.formula(paste(dependent,"~",pred,"|",random)), data=dat,
		panel = function(x,y,subscripts) { 
			panel.xyplot(x,y)
			itm = as.character(dat[subscripts[1],random])
			str = paste("coefs = as.numeric(unlist(coef(model1)$",random,"[itm,][c(\"(Intercept)\",\"",pred,"\")]))",sep="")
			eval(parse(text=str))
			panel.abline(coefs,col="red",lty=1,lwd=2)

			if (showFixedSlope) { 
				str = paste("coefs2 = fixef(model1)[c(\"(Intercept)\",\"",pred,"\")]",sep="")
				eval(parse(text=str))
				panel.abline(coefs2,col="black",lty=2,lwd=2)
			}
			#if (!is.null(model2)) { 
			#	str = paste("coefs2 = c(unlist(coef(model2)$",random,"[itm,][c(\"(Intercept)\",\"",pred,"\")]))",sep="")
			#	eval(parse(text=str))
			#	panel.abline(coefs2,col="blue",lty=3)
			#}
		})
}


