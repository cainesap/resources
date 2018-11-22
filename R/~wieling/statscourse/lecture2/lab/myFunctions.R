# Custom functions for R lab session, 2013-12-09
# Martijn Wieling
library(lme4)
library(car)
library(gplots)
library(lattice)

# Source of these functions: https://raw.github.com/aufrank/R-hacks/master/regression-utils.R (made by Florian Jaeger)
c. <- function (x) scale(x, scale = FALSE)
z. <- function (x) scale(x)


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
			str = paste("coefs = as.numeric(unlist(coef(model1)$",random,"[1,][c(\"(Intercept)\",\"",pred,"\")]))",sep="")
			eval(parse(text=str))
			panel.abline(coefs,col="red",lty=1,lwd=2)

			if (showFixedSlope) { 
				str = paste("coefs2 = fixef(model1)[c(\"(Intercept)\",\"",pred,"\")]",sep="")
				eval(parse(text=str))
				panel.abline(coefs2,col="black",lty=2,lwd=2)
			}
		})
}


# myFocusPlot plots the average focus proportions for target (if plotTarget), 
# competitor (if plotComp) and distractors (if plotDist) 
# 
# the function assumes the following columnnames in the data file (dat):
# Target : 1 if looking at target, 0 otherwise
# Comp : 1 if looking at target, 0 otherwise
# Dist : 0.5 if looking at one of two distractors, 0 otherwise
# Definiteness: separates definite from indefinite
# TargetGender: specifies the target of the gender
# ColorComp: specifies the presence of a color competitor
# GenderComp: specifies the presence of a gender competitor
# Subject: ID of subject
# Item: ID of item
# TargetColor: specifies the color of the target
# TargetPlace: specifies the position of the target
# Time: the timing of every focus measurement
# TimeOnsetDet: the time when the determiner starts (only when plotMarkers==T)
# TimeOnsetAdj: the time when the adjective starts (only when plotMarkers==T)
# dat$TimeOnsetNoun: the time when the noun starts (only when plotMarkers==T)
# 
# the split-variables specify if the graphs are split based
# on those variables, or that the results are combined
# e.g., if splitItem == T and splitGender == T then you will get separate plots
# for every item for all genders separately
#
# parameters: dat: your data set having the aforementioned columnnames
#             pdfname: the name of the pdf which is generated with the plots (default: eyeplots)
#             splitGender: separate plots for common and neuter genders? (default: True)
#             splitGenderComp: separate plots for same gender and different gender between target and competitor? (default: True)
#             splitColorComp: separate plots for same color and different color between target and competitor? (default: True)
#             splitDefiniteness: separate plots for definite and indefinite? (default: False)
#             splitTargetColor: separate plots for the color of the target? (default: False)
#             splitTargetPlace: separate plots for the position of the target? (default: False)
#             splitSubject: separate plots per subject? (default: False)
#             splitItem: separate plots per item? (default: False)
#             plotMarkers: show markers for the average onset time of determiner, adjective and noun as well as noun+200 (default: True)
#             plotTarget: plot the target focus line (default: True)
#             plotComp: plot the competitor focus line (default: True)
#             plotDist: plot the average distractors focus line (default: True)
#             startTime: focus on a certain time window (starting time, default: 0)
#             endTime: focus on a certain time window (ending time, default: 2200)
myFocusPlot = function(dat,usepdf=T,pdfname='eyeplots',splitGender=T,splitGenderComp=T,splitColorComp=T,splitDefiniteness=F,splitTargetColor=F,splitTargetPlace=F,splitItem=F,splitSubject=F,plotMarkers=T,plotTarget=T,plotComp=T,plotDist=T,startTime=0,endTime=2200) {

library(gplots)

data = dat

eyeDefiniteness = unique(data$Definiteness)
eyeTargetGender = unique(data$TargetGender)
eyeGenderComp = unique(data$GenderComp)
eyeColorComp = unique(data$ColorComp)
eyeSubject = unique(data$Subject)
eyeItem = unique(data$Item)
eyeTargetColor = unique(data$TargetColor)
eyeTargetPlace = unique(data$TargetPlace)

if (usepdf) {
	pdf(paste(pdfname,".pdf",sep=""),width=12,height=7,useDingbats=F)
}
par(mfrow=c(1,2))
 
i = 1
conti = T
while (conti & (i <= length(eyeColorComp))) {
	data$useColorComp = data$ColorComp==eyeColorComp[i]
	labelColorComp = paste("Color",eyeColorComp[i],"- ")
	i = i+1
	if (!splitColorComp) { 
		data$useColorComp=data$useColorComp | T # all colors
		labelColorComp = ""
		conti = F # only one iteration
	}

	j = 1
	contj = T
	while (contj & (j <= length(eyeTargetGender))) {
		data$useTargetGender = data$TargetGender==eyeTargetGender[j]
		labelTargetGender = paste(eyeTargetGender[j],"- ")
		j = j+1
		if (!splitGender) { 
			data$useTargetGender=data$useTargetGender | T # all genders
			labelTargetGender = "" 
			contj = F
		}

		k = 1
		contk = T
		while (contk & (k <= length(eyeGenderComp))) {
			data$useGenderComp=data$GenderComp==eyeGenderComp[k]
			labelGenderComp = paste("Gender",eyeGenderComp[k],"- ")
			k = k+1
			if (!splitGenderComp) { 
				data$useGenderComp=data$useGenderComp | T # all genders
				labelGenderComp = "" 
				contk = F
			}

			l = 1
			contl = T
			while (contl & (l <= length(eyeDefiniteness))) {
				data$useDefiniteness = data$Definiteness==eyeDefiniteness[l]
				labelDefiniteness = paste(eyeDefiniteness[l],"- ")
				l = l+1
				if (!splitDefiniteness) { 
					data$useDefiniteness=data$useDefiniteness | T # all genders
					labelDefiniteness = "" 
					contl = F
				}
				m = 1
				contm = T
				while (contm & (m <= length(eyeSubject))) {
					data$useSubject = data$Subject==eyeSubject[m]
					labelSubject = paste(eyeSubject[m],"- ")
					m = m+1
					if (!splitSubject) { 
						data$useSubject = data$useSubject | T # all genders
						labelSubject = "" 
						contm = F
					}

					n = 1
					contn = T
					while (contn & (n <= length(eyeItem))) {
						data$useItem = data$Item==eyeItem[n]
						labelItem = paste(eyeItem[n],"- ")
						n = n+1
						if (!splitItem) { 
							data$useItem = data$useItem | T # all genders
							labelItem = "" 
							contn = F
						}


						o = 1
						conto = T
						while (conto & (o <= length(eyeTargetColor))) {
							data$useTargetColor = data$TargetColor==eyeTargetColor[o]
							labelTargetColor = paste("Target color",eyeTargetColor[o],"- ")
							o = o+1
							if (!splitTargetColor) { 
								data$useTargetColor = data$useTargetColor | T # all genders
								labelTargetColor = "" 
								conto = F
							}

							p = 1
							contp = T
							while (contp & (p <= length(eyeTargetPlace))) {
								data$useTargetPlace = data$TargetPlace==eyeTargetPlace[p]
								labelTargetPlace = paste("Target place",eyeTargetPlace[p],"- ")
								p = p+1
								if (!splitTargetPlace) { 
									data$useTargetPlace = data$useTargetPlace | T # all genders
									labelTargetPlace = "" 
									contp = F
								}


								dataT = data[data$useColorComp & data$useTargetGender & data$useGenderComp & data$useDefiniteness & data$useSubject & data$useItem & data$useTargetColor & data$useTargetPlace,]
								label = paste("Condition: ",labelColorComp,labelTargetGender,labelGenderComp,labelDefiniteness,labelTargetColor,labelTargetPlace,labelSubject,labelItem,sep="")
								if (label=="Condition: ") { 
									label = "Condition: all"
								}
								label = gsub(' - $','',label)
								singlePlot(dataT,label,plotMarkers,plotTarget,plotComp,plotDist,startTime,endTime)
							}
						}
					}
				}
			}
		}
	}
}
if (usepdf) { 
	dev.off()
}

}

# helper function for myFocusPlot, see that function for details
singlePlot = function(dat,label,plotMarkers=T,plotTarget=T,plotComp=T,plotDist=T,startTime=0,endTime=2200) {

	TimeOnsetDet=mean(dat$TimeOnsetDet)
	TimeOnsetAdj=mean(dat$TimeOnsetAdj)
	TimeOnsetNoun=mean(dat$TimeOnsetNoun)
	detstart=mean(dat$TimeOnsetDet)
	adjstart=mean(dat$TimeOnsetAdj)
	nounstart=mean(dat$TimeOnsetNoun)
	
	eval(parse(text=paste("startTime =", startTime)))
	eval(parse(text=paste("endTime = ", endTime)))
	
	dat = dat[dat$Time>=startTime & dat$Time <= endTime,]
	


	plotstart = F
	if (plotTarget) { 
		suppressWarnings(plotmeans(dat$Target ~dat$Time,barcol="green",ccol="green",n.label=F,main=label,cex.main=1,xlab="Time (ms)",ylab="Fixation proportion",ylim=c(0,1),xaxt="n"))
		plotstart = T
	}

	if (plotComp) {
		if (plotstart) { 
			suppressWarnings(plotmeans(dat$Comp ~dat$Time, barcol="red",ccol="red",n.label=F,add=T,xaxt="n"))
		}
		else { 
			suppressWarnings(plotmeans(dat$Comp ~dat$Time, barcol="red",ccol="red",n.label=F,main=label,cex.main=1,xlab="Time (ms)",ylab="Fixation proportion",ylim=c(0,0.5),xaxt="n"))
			plotstart = T
		}
	}

	if (plotDist) { 
		if (plotstart) { 
			suppressWarnings(plotmeans(dat$Dist ~dat$Time, barcol="orange",ccol="orange",n.label=F,add=T,xaxt="n"))
		}
		else { 
			suppressWarnings(plotmeans(dat$Dist ~dat$Time, barcol="orange",ccol="orange",n.label=F,main=label,cex.main=1,xlab="Time (ms)",ylab="Fixation proportion",ylim=c(0,0.5),xaxt="n"))
		}
	}

	if (plotTarget & plotComp & plotDist) { 
		legend("topleft", legend=c("Target","Competitor", "Distractors"), col=c("green","red","orange"), lwd=c(1,1,1), bty="n", cex=1)
	}
	else if (!plotTarget & plotComp & plotDist) { 
		legend("topleft", legend=c("Competitor", "Distractors"), col=c("red","orange"), lwd=c(1,1), bty="n", cex=1)
	}
	else if (plotTarget & !plotComp & plotDist) { 
		legend("topleft", legend=c("Target","Distractors"), col=c("green","orange"), lwd=c(1,1), bty="n", cex=1)
	}
	else if (plotTarget & plotComp & !plotDist) { 
		legend("topleft", legend=c("Target","Competitor"), col=c("green","red"), lwd=c(1,1), bty="n", cex=1)
	}
	if (plotTarget & !plotComp & !plotDist) { 
		legend("topleft", legend=c("Target"), col=c("green"), lwd=c(1), bty="n", cex=1)
	}
	if (!plotTarget & plotComp & !plotDist) { 
		legend("topleft", legend=c("Competitor"), col=c("red"), lwd=c(1), bty="n", cex=1)
	}
	if (!plotTarget & plotComp & !plotDist) { 
		legend("topleft", legend=c("Distractors"), col=c("orange"), lwd=c(1), bty="n", cex=1)
	}
	
	abline(v=(detstart-startTime)/16.67)
	#abline(v=(detstart+200)/16.67,lty=2)
	abline(v=(adjstart-startTime)/16.67)
	abline(v=(nounstart-startTime)/16.67)
	abline(v=(nounstart+200-startTime)/16.67,lty=2)

	axis(1,at=c(0,(detstart-startTime)/16.67,(adjstart-startTime)/16.67,(nounstart-startTime)/16.67,(nounstart+200-startTime)/16.67,endTime/16.67-(startTime/16.67)),labels=c(toString(round(startTime)),"D","A","N","N+200",toString(round(endTime))))
	#axis(1,at=c(0,(nounstart-startTime)/16.67,(nounstart+200-startTime)/16.67),labels=c("D+200 (= A)","N","N+200"))
}

