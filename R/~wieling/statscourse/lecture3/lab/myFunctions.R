# Custom functions for statistics course on mixed-effects regression and generalized additive modeling, 2014
# Last changed: 2013-12-09, Martijn Wieling
library(car)
library(mgcv)
library(lattice)


# Source of these functions: https://raw.github.com/aufrank/R-hacks/master/regression-utils.R (made by Florian Jaeger)
c. <- function (x) scale(x, scale = FALSE)
z. <- function (x) scale(x)


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


# myCoefPlot2.gam wil plot the adjustments to the slopes of two predictors
# with respect to one random-effect factor. It will display by default
# 5 words at the outer points of the plot
#
# parameters: model: result of gam/bam
#             ranfact: name of the random effect factor between quotes
#             pred1: name of first predictor between quotes
#             pred2: name of second predictor between quotes
#             nrWords: nr of printed words in the plots at the end points
# e.g. myCoefPlot2.gam(finalmodelC,"Word","SpEdu.z","SpIsMale")
myCoefPlot2.gam = function(model,ranfact,pred1,pred2,nrWords=5) { 
	
	mystr = paste('dat =',sub(".* data = (.*).", "\\1", model$call)[3])
	eval(parse(text=mystr))


	a = as.data.frame(coef(model))
	if (pred1 == "Intercept" | pred1 == "(Intercept)") { 
		splineName1 = paste("s(",ranfact,").",sep="")
	} else { 
		splineName1 = paste("s(",ranfact,",",pred1,").",sep="")
	}
	mydf = data.frame(a[regexpr(splineName1,rownames(a),fixed=T)>0,])
	colnames(mydf)[1] = pred1
	fixed1 = NA
	if (nrow(data.frame(a[regexpr(paste("^",pred1,sep=""),rownames(a),perl=T)>0,])) == 1) { 
		mydf[1] = mydf[1] + a[regexpr(paste("^",pred1,sep=""),rownames(a),perl=T)>0,] # add value of fixed effect if present
		fixed1 = a[regexpr(paste("^",pred1,sep=""),rownames(a),perl=T)>0,]
	} else if (nrow(data.frame(a[regexpr(paste("^",pred1,sep=""),rownames(a),perl=T)>0,])) > 1) {
		print("ERROR: 2 columns have the same prefix. The value is not added (pred1).")
	}
	
	if (!is.null(pred2)) { 
		if (pred2 == "Intercept" | pred2 == "(Intercept)") { 
			splineName2 = paste("s(",ranfact,").",sep="")
		} else { 
			splineName2 = paste("s(",ranfact,",",pred2,").",sep="")
		}
		col2 = data.frame(a[regexpr(splineName2,rownames(a),fixed=T)>0,])
		mydf = data.frame(mydf,col2)
		colnames(mydf)[2] = pred2
		fixed2 = NA
		if (nrow(data.frame(a[regexpr(paste("^",pred2,sep=""),rownames(a),perl=T)>0,])) == 1) { 
			mydf[2] = mydf[2] + a[regexpr(paste("^",pred2,sep=""),rownames(a),perl=T)>0,] # add value of fixed effect if present
			fixed2 = a[regexpr(paste("^",pred2,sep=""),rownames(a),perl=T)>0,]
		} else if (nrow(data.frame(a[regexpr(paste("^",pred2,sep=""),rownames(a),perl=T)>0,])) > 1) {
			print("ERROR: 2 columns have the same prefix. The value is not added (pred2).")
		}
	}

	#smry = summary(model)
    #lmercoefsfixed = smry@coefs
	#rownames(lmercoefsfixed)[1] = "Intercept"
	#a=ranef(model) # still need to add model estimates (http://r.789695.n4.nabble.com/Random-slopes-in-lmer-td2339656.html)
	#w=data.frame(a[ranfact])
	w = mydf



	#colnames(w) = gsub("\\.","",gsub(paste(ranfact,"\\.*",sep=""),"",colnames(w)))
	#pred1alt = gsub("\\.","",pred1)
	#pred2alt = gsub("\\.","",pred2)
	
	if (!is.null(dat)) { 
		rownames(w) = unique(dat[,c(ranfact)])
		rownames(w) = gsub("_"," ",rownames(w))
		rownames(w) = gsub("rotonda","rotondo",rownames(w))
	}

	w$r = rownames(w) # temporary, otherwise ordering loses column names...

	if (!is.null(pred2)) { 
		w=w[order(w[,pred2]),]
	} else { 
		w=data.frame(w[order(w[,pred1]),])
	}
	w$r = NULL
	


#	if (pred1 %in% rownames(lmercoefsfixed)) { 
#		w[,pred1alt] = w[,pred1alt] + lmercoefsfixed[pred1,"Estimate"]
#	}

	pred1alt = pred1
	pred2alt = pred2

	w2=rbind(head(w,nrWords), tail(w,nrWords))
	

	if (!is.null(pred2)) { 
		#if (pred2 %in% rownames(lmercoefsfixed)) { 
		#	w[,pred2alt] = w[,pred2alt] + lmercoefsfixed[pred2,"Estimate"]
		#}
		
		plot(w[,c(pred1alt,pred2alt)], xlab="", ylab="", cex.axis=1,pch=20,type="n", xlim=c(min(w[,pred1alt])-abs(min(w[,pred1alt]))*0.5, max(w[,pred1alt])+abs(max(w[,pred1alt]))*0.5))

		if (!is.na(fixed1)) { 
			if (fixed1 < 0) { 
				xmin = par()$usr[1]
				xmax = 0 
			} else { 
				xmin = 0 
				xmax = par()$usr[2]
			}
			vline = fixed1
		}
		else { 
			xmin = 0 
			xmax = par()$usr[2]
			vline = 0
		}

		if (!is.na(fixed2)) { 
			if (fixed2 < 0) { 
				ymin = par()$usr[3]
				ymax = 0 
			} else { 
				ymin = 0 
				ymax = par()$usr[4]
			}
			hline = fixed2
		}
		else { 
			ymin = 0 
			ymax = par()$usr[4]
			hline = 0
		}
		
		#polygon(c(xmin,xmin,xmax,xmax),c(ymin,ymax,ymax,ymin),col="#EEEEEE",border=NA)
		points(w[,c(pred1alt,pred2alt)], xlab="", ylab="", cex.axis=1.4,pch=20,xlim=c(min(w[,pred1alt])-abs(min(w[,pred1alt]))*0.2, max(w[,pred1alt])+abs(max(w[,pred1alt]))*0.2))
		
		abline(v=vline,h=hline,lty=2)
		abline(v=0,h=0,lty=1)
		#text(w2[,c(pred1alt, pred2alt)], rownames(w2), adj=0, cex=0.8)

		for (i in 1:length(w[,pred1alt])) { 
			if (i <= nrWords | i > length(w[,pred1alt])-nrWords) { 
				#if (i%%2 ==0) { text(x=i,y=w[,pred1alt][i], rownames(w)[i], pos=2,cex=1)}
				#else { text(x=i,y=w[,pred1alt][i], rownames(w)[i], pos=4,cex=1)}
				if (i > 0.5*length(w[,pred1alt])) { text(x=w[,pred1alt][i],y=w[,pred2alt][i], rownames(w)[i], pos=2,cex=1)}
				else { text(w[,pred1alt][i],y=w[,pred2alt][i], rownames(w)[i], pos=4,cex=1)}
			}
		}


		box()
		#title(xlab=xlabel, ylab=ylabel,cex.lab=1)
		title(xlab=paste("Coefficient",pred1), ylab=paste("Coefficient",pred2),cex.lab=1.5)
	}
	else { 
		#w=w[order(w[,pred1alt]),]
		rownames(w) = gsub("\\*.*","",rownames(w))
		plot(w[,pred1alt],xlab="",ylab="",xaxt='n',ann=F,xlim=c(0,length(w[,pred1alt])))

		if (!is.na(fixed1)) { 
			abline(h=fixed1,lty=2)
		}
		abline(h=0,lty=1)

		for (i in 1:length(w[,pred1alt])) { 
			if (i <= nrWords | i > length(w[,pred1alt])-nrWords) { 
				#if (i%%2 ==0) { text(x=i,y=w[,pred1alt][i], rownames(w)[i], pos=2,cex=1)}
				#else { text(x=i,y=w[,pred1alt][i], rownames(w)[i], pos=4,cex=1)}
				add = 0
				
				if (i > 0.5*length(w[,pred1alt])) {
					poss = 2
				} else { 
					poss = 4
				}
				text(x=i,y=w[,pred1alt][i], rownames(w)[i], pos=poss,cex=1)

				#if (i > 0.5*length(w[,pred1alt])) { }
				#else { text(x=i,y=w[,pred1alt][i], rownames(w)[i], pos=4,cex=1)}
			}
		}
		title(xlab="Sorted index",ylab=paste("Coefficient",pred1),cex.lab=1.5)
		#title(xlab=xlabel,ylab=ylabel,cex.lab=1)
	}
}


# myCoefPlot.gam wil plot the adjustments to the slope of one predictor
# with respect to one random-effect factor. It will display by default
# 5 words at the outer points of the plot
#
# parameters: model: result of gam/bam
#             ranfact: name of the random effect factor between quotes
#             pred1: name of predictor between quotes
#             nrWords: nr of printed words in the plots at the end points
myCoefPlot.gam = function(model,ranfact,pred1,nrWords=5,labelcolumn=NULL) { 
	myCoefPlot2.gam(model,ranfact,pred1,NULL,nrWords)
}

