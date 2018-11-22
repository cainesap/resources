# MW: 2014-10-13, added vis.gam2 which has additional parameter dropRanef to drop random effects
# MW: 2014-10-07, changed to work with cbind(..,..) as dependent variable
# plots smooths of different levels of a by-variable, or only a single smooth if no by-variable was used
plotSmooths <- function(model, xvar, catvar=NULL, catlevels=NULL, eegAxis=T, ylim=NULL, useLegend=T, legendPos=NULL, legendScale=1, colors=NULL, ylab=NULL, main=NULL, dropRanef=NULL, cond=list(), method='lpmatrix',...) { 
	if (is.null(main)) { 
		main = paste('Model:',deparse(substitute(model)))
	}

	dat = model$model
	
	ignoreCols = c(as.character(model$formula[2]),xvar,catvar,dropRanef)


	if (is.null(ylab)) { 
		ylab = as.character(model$formula[2])
	}

	convert = 1
	if(eegAxis==T) convert = -1
			
	yvals <- sort(convert*ylim, decreasing=F)
	
	rngX = max(na.exclude(dat[,xvar])) - min(na.exclude(dat[,xvar]))
	
	np = 100

	if (!is.null(catvar)) { 
		if (is.null(catlevels)) {
			vals = sort(unique(dat[,catvar]))
		} else { 
			vals = catlevels
		}
	} else { 
		vals = NA
	}

	ymin = Inf
	ymax = -Inf

	#if (!is.null(catvar)) { 
	#	grp <- dat[which(is.na(dat[,catvar])), ] 
	#} else { 
	#	grp <- dat[which(is.na(dat[,xvar])),]
	#}
	grp = dat[1,]
	grp = do.call('rbind', replicate(np, grp, simplify=F))
	#grp[1:np,] = dat[1,]
	grp[,xvar] = seq(min(na.exclude(dat[,xvar])),max(na.exclude(dat[,xvar])), by=rngX/(np-1))
	
	# set column values to mean numerical value, or (the first) most frequent value otherwise
	for (cl in colnames(dat)) { 
		if (!cl %in% ignoreCols) { 
			if (is.numeric(dat[,cl])) { 
				grp[,cl] = mean(dat[,cl],na.rm=T)
			} else { 
				grp[,cl] = names(table(dat[,cl])[which(max(table((dat[,cl])))==table(dat[,cl]))])[1]
			}
		}
	}
	
	# overwrite values with pre-specified values
	if (length(cond) > 0) { 
		for (i in 1:length(cond)) {
			grp[,names(cond)[i]] = cond[[i]]
		}
	}

	
	datasets = list() 
	for (i in 1:length(vals)) {
		if (!is.null(catvar)) { 
			grp[,catvar] = vals[i]
		}

		if (method == 'lpmatrix') { 
			predval = predict.dropRanef(model,grp,dropRanef)
			newcoef <- coef(model)
			newvcov <- vcov(model)
			fv <- data.frame(fit = predval %*% newcoef)
			fv$se.fit <- sqrt(rowSums((predval%*%newvcov)*predval))
		} else if (method == 'response') { 
			fv = predict.gam(model,grp,type='response',se.fit=T)
		} else {
			fv = predict.gam(model,grp,type='link',se.fit=T)
		}

		grp$fit = fv$fit
		grp$ul = fv$fit + 1.96*fv$se.fit
		grp$ll = fv$fit - 1.96*fv$se.fit
		grp$ul99 = fv$fit + 2.58*fv$se.fit
		grp$ll99 = fv$fit - 2.58*fv$se.fit
		datasets[[length(datasets)+1]] = grp
		ymin = min(grp$ll,ymin)
		ymax = max(grp$ul,ymax)
	}

	ymin = convert*ymin
	ymax = convert*ymax

	if (is.null(ylim)) { 
		yvals = sort(c(ymin,ymax),decreasing=F)
	}

	if (!is.null(colors) & length(colors) >= length(datasets)) { 
		clr = colors
		clrs = unlist(lapply(clr,add.alpha)) # shaded colors
	} else { 
		clr = rainbow(length(vals))
		clrs = rainbow(length(vals),alpha=0.25)
	}

	if (is.null(catvar)) { 
		clr = '#222222'
		clrs = add.alpha(clr)
	}

	for (i in 1:length(datasets)) { 
		grp = datasets[[i]]
		if (i == 1) {
			plot(grp[,xvar], convert*grp$fit, ylab=ylab, xlab=xvar, ylim=yvals, type='l',axes=F, col=clr[i], main=main)
			box()
			axis(1)
			axis(2, at=axTicks(2), labels=convert*axTicks(2))
			polygon(c(grp[,xvar],rev(grp[,xvar])),c(convert*grp$ul,rev(convert*grp$ll)),
					col=clrs[i],border=NA) # shaded confidence interval
		} else { 
			lines(grp[,xvar], convert*grp$fit, col=clr[i])
			polygon(c(grp[,xvar],rev(grp[,xvar])),c(convert*grp$ul,rev(convert*grp$ll)),
					col=clrs[i],border=NA) # shaded confidence interval
		}
	}
	abline(h=0)

	if (useLegend & !is.null(catvar)) { 
		if (!is.null(legendPos)) { 
			lx = legendPos
		} else { 
			lx = "bottomleft"
		}
		legend(lx,legend=vals, col=clr,lwd=2,title=catvar,cex=legendScale)
	}
}


# plots difference curve
# set binary=T if you have a by-variable occurring multiple times 
plotDiff <- function(model, xvar, catvar, catlevels=NULL, eegAxis=T, ylim=NULL, ylab=NULL, main=NULL, dropRanef=NULL, ...) { 
	dat = model$model

	if (is.null(ylab)) { 
		ylab = as.character(model$formula[2])
	}

	convert <- 1
	if(eegAxis==T) convert <- -1
			
	yvals <- sort(convert*ylim, decreasing=F)
	
	rngX = max(na.exclude(dat[,xvar])) - min(na.exclude(dat[,xvar]))
	
	np = 100

	if (is.null(catlevels)) { 
		vals = sort(unique(dat[,catvar]))
	} else { 
		vals = catlevels
	}

	grp1 <- dat[which(is.na(dat[,catvar])), ] 
	grp1[1:np,] = dat[1,]
	grp1[,xvar] = seq(min(na.exclude(dat[,xvar])),max(na.exclude(dat[,xvar])), by=rngX/(np-1))
	
	grp1[,catvar] = vals[1]

	#pred1 = predict.onlyInclude(model,grp1,onlyInclude=c(xvar,paste(catvar,vals[1],sep='')))
	pred1 = predict.dropRanef(model,grp1,dropRanef)

	grp2 = grp1
	grp2[,catvar] = vals[2]

	#pred2 = predict.onlyInclude(model,grp2,onlyInclude=c(xvar,paste(catvar,vals[2],sep='')))
	pred2 = predict.dropRanef(model,grp2,dropRanef)

	res1 = grp1
	res1$Xp <- pred2 - pred1

	res1$diff <- res1$Xp%*%coef(model)
	

	res1$XXX = res1[,xvar]
		
	if (is.null(main)) {
		mn = paste('Difference between',vals[1],'and',vals[2])
	} else { 
		mn = main
	}

	res1 = res1[order(res1$XXX),]

	se.diff <- sqrt(rowSums((res1$Xp%*%vcov(model))*res1$Xp))
	res1$ul <- res1$diff + 1.96 * se.diff
	res1$ll <- res1$diff - 1.96 * se.diff
	res1$ul99 <- res1$diff + 2.58 * se.diff
	res1$ll99 <- res1$diff - 2.58 * se.diff

	if (is.null(ylim)) yvals <- sort(c(convert*min(res1$ll),convert*max(res1$ul)),decreasing=F)
	plot(res1$XXX,convert*res1$diff,type='l',xlab=xvar, main=mn, ylab=ylab, ylim=yvals, axes=F,...)
	box()
	axis(1)
	axis(2, at=axTicks(2), labels=convert*axTicks(2))

	polygon(c(res1$XXX,rev(res1$XXX)),c(convert*res1$ul,rev(convert*res1$ll)),
			col=rgb(0.25,0.25,0.25,alpha=.25),border=NA) # shaded confidence interval

	abline(h=0)
}


# plots smooths and difference curve
plots <- function(model, xvar, catvar, catlevels = NULL, eegAxis=T, ylim=NULL, useLegend=T, legendPos=NULL, colors=NULL, ylab=NULL, main=NULL, dropRanef=NULL, ...) { 
	par(mfrow=c(1,2))
	plotSmooths(model=model, xvar=xvar, catvar=catvar, catlevels = catlevels, eegAxis=eegAxis, ylim=ylim, useLegend=useLegend, legendPos=legendPos, colors=colors, ylab=ylab, main=paste("Model:",deparse(substitute(model))), ...)
	plotDiff(model=model, xvar=xvar, catvar=catvar, catlevels = catlevels, eegAxis=eegAxis, ylim=ylim, ylab=ylab, main=main, ...)
}



# plots differences in 2D plot
plotDiff2D <- function(model,xvar,yvar,catvar,catlevels=NULL,col='topo',plotCI=F,main=NULL,method='lpmatrix',dropRanef=NULL) { 
	dat = model$model
	
	nX = 100
	nY = 100
	rngX = max(na.exclude(dat[,xvar])) - min(na.exclude(dat[,xvar]))
	rngY = max(na.exclude(dat[,yvar])) - min(na.exclude(dat[,yvar]))
	
	np = nX * nY

	if (is.null(catlevels)) { 
		vals = sort(unique(dat[,catvar]))
	} else { 
		vals = catlevels
	}

	# datasets maken, waarbij je alleen de variabele van interest varieert, om verschilcurves te kunnen maken
	grp1 <- dat[which(is.na(dat[,catvar])), ] # get same columns
	grp1[1:np,] = dat[1,]
	grp1[,xvar] = rep(seq(min(na.exclude(dat[,xvar])),max(na.exclude(dat[,xvar])), by=rngX/(nX-1) ),nY)
	grp1$nr = as.numeric(rownames(grp1))-1
	grp1[,yvar] = min(na.exclude(dat[,yvar])) + floor(grp1$nr / nX)*(rngY / (nY-1))
	
	grp1[,catvar] = vals[1]

	#pred1 = predict.onlyInclude(model,grp1,onlyInclude=c(xvar,yvar,paste(catvar,vals[1],sep='')))
	pred1 = predict.dropRanef(model,grp1,dropRanef)

	grp2 = grp1
	grp2[,catvar] = vals[2]

	#pred2 = predict.onlyInclude(model,grp2,onlyInclude=c(xvar,yvar,paste(catvar,vals[2],sep='')))
	pred2 = predict.dropRanef(model,grp2,dropRanef)

	res1 = grp1
	res1$Xp <- pred2 - pred1
	res1$diff <- res1$Xp%*%coef(model)


	res1$XXX = res1[,xvar]
	res1$YYY = res1[,yvar]
	z = matrix(res1$diff, nX, nY)
	m1 = seq(min(na.exclude(dat[,xvar])), max(na.exclude(dat[,xvar])), length=nX)
	m2 = seq(min(na.exclude(dat[,yvar])), max(na.exclude(dat[,yvar])), length=nY)
	
	if (is.null(main)) {
		mn = paste('Difference between',vals[1],'and',vals[2])
	} else { 
		mn = main
	}

	image(m1,m2,z,col=topo.colors(100),main=mn,xlab=xvar,ylab=yvar)
	contour(m1,m2,z,col='black',nlevels=10,add=T)

	if (plotCI) { 
		se.diff <- sqrt(rowSums((res1$Xp%*%vcov(model))*res1$Xp))
		res1$ul <- res1$diff + 1.96 * se.diff
		res1$ll <- res1$diff - 1.96 * se.diff
		res1$ul99 <- res1$diff + 2.58 * se.diff
		res1$ll99 <- res1$diff - 2.58 * se.diff
		zu = matrix(res1$ul, nX, nY)
		contour(m1,m2,zu,col='red',nlevels=10,add=T,lty=2)
		zl = matrix(res1$ll, nX, nY)
		contour(m1,m2,zl,col='green',nlevels=10,add=T,lty=2)
	}
}


# plots effect of rho parameter in second model (rho=...)
acf.new <- function(model0,modelrho,timevar='Time') {
	source('compareML.r')
	# plotten of de ACF nu beter is (ja)
	dat = modelrho$model

	dat$res = resid(modelrho)
	dat$res.next = NA
	dat[1:nrow(dat)-1,]$res.next = dat[2:(nrow(dat)),]$res
	dat$res.new = dat$res.next - modelrho$AR1.rho*dat$res
	dat[dat[,timevar]==min(dat[,timevar]),]$res.new = NA
	par(mfrow=c(1,2))
	acfs = acf(resid(model0),plot=F)

	plot(acfs,main='Original acf')
	acf(na.exclude(dat$res.new), main=paste('New acf with rho:',round(modelrho$AR1.rho,2)))
	#compareML(model0,modelrho)

	#library(car)
	#qqp(dat$res.new)

}

# qqplot which takes into account the autocorrelation
qqplot.rho <- function(modelrho,timevar='Time',main='') {
	source('compareML.r')
	# plotten of de ACF nu beter is (ja)
	dat = modelrho$model

	if (!is.null(modelrho$AR1.rho) & modelrho$AR1.rho > 0) {
		dat$res = resid(modelrho)
		dat$res.next = NA
		dat[1:nrow(dat)-1,]$res.next = dat[2:(nrow(dat)),]$res
		dat$res.new = dat$res.next - modelrho$AR1.rho*dat$res
		dat[dat[,timevar]==min(dat[,timevar]),]$res.new = NA
		library(car)
		qqp(na.exclude(dat$res.new),ylab='residuals',main=main)
	} else { 
		library(car)
		qqp(resid(modelrho),main=main)
	}
}

# histogram which takes into aacount
hist.rho <- function(modelrho,timevar='Time') {
	source('compareML.r')
	# plotten of de ACF nu beter is (ja)
	dat = modelrho$model

	if (!is.null(modelrho$AR1.rho) & modelrho$AR1.rho > 0) {
		dat$res = resid(modelrho)
		dat$res.next = NA
		dat[1:nrow(dat)-1,]$res.next = dat[2:(nrow(dat)),]$res
		dat$res.new = dat$res.next - modelrho$AR1.rho*dat$res
		dat[dat[,timevar]==min(dat[,timevar]),]$res.new = NA
		library(car)
		hist(na.exclude(dat$res.new),xlab='residuals',main='')
	} else { 
		library(car)
		hist(resid(modelrho))
	}
}


# Changed by Martijn Wieling, 2013
# This modfication of vis.gam allows the user to drop Random effects from the effect surface 
# Use: 
# 1) view=c('Time','Trial') to specify which surface to plot, and 
# 2) cond=list(X=5) can be used to select a specific value of a continuous predictor 
# in a complex interaction, e.g. to specify the value of X in te(Time,Trial,X, by=Cond).
# Important: do not specify other predictors in cond that are not to be plotted.
vis.gam2 <- function (x, view = NULL, cond=list(), n.grid = 30, too.far = 0, 
    col = NA, color = "topo", contour.col = 'black', se = -1, type = "link", 
    plot.type = "contour", zlim = NULL, nCol = 50, dropRanef=NULL,...) 
{
    fac.seq <- function(fac, n.grid) {
        fn <- length(levels(fac))
        gn <- n.grid
        if (fn > gn) 
            mf <- factor(levels(fac))[1:gn]
        else {
            ln <- floor(gn/fn)
            mf <- rep(levels(fac)[fn], gn)
            mf[1:(ln * fn)] <- rep(levels(fac), rep(ln, fn))
            mf <- factor(mf, levels = levels(fac))
        }
        mf
    }
    dnm <- names(list(...))
    v.names <- names(x$var.summary)
    if (is.null(view)) {
        k <- 0
        view <- rep("", 2)
        for (i in 1:length(v.names)) {
            ok <- TRUE
            if (is.matrix(x$var.summary[[i]])) 
                ok <- FALSE
            else if (is.factor(x$var.summary[[i]])) {
                if (length(levels(x$var.summary[[i]])) <= 1) 
                  ok <- FALSE
            }
            else {
                if (length(unique(x$var.summary[[i]])) == 1) 
                  ok <- FALSE
            }
            if (ok) {
                k <- k + 1
                view[k] <- v.names[i]
            }
            if (k == 2) 
                break
        }
        if (k < 2) 
            stop("Model does not seem to have enough terms to do anything useful")
    }
    else {
        if (sum(view %in% v.names) != 2) {
            stop(paste(c("view variables must be one of", v.names), 
                collapse = ", ")) }
        for (i in 1:2) if (!inherits(x$var.summary[[view[i]]], 
            c("numeric", "factor"))) 
            stop("Don't know what to do with parametric terms that are not simple numeric or factor variables")
    }
    ok <- TRUE
    for (i in 1:2) if (is.factor(x$var.summary[[view[i]]])) {
        if (length(levels(x$var.summary[[view[i]]])) <= 1) 
            ok <- FALSE
    }
    else {
        if (length(unique(x$var.summary[[view[i]]])) <= 1) 
            ok <- FALSE
    }
    if (!ok) 
        stop(paste("View variables must contain more than one value. view = c(", 
            view[1], ",", view[2], ").", sep = ""))
    if (is.factor(x$var.summary[[view[1]]])) {
        m1 <- fac.seq(x$var.summary[[view[1]]], n.grid)
    }else {
        r1 <- range(x$var.summary[[view[1]]])
        m1 <- seq(r1[1], r1[2], length = n.grid)
    }
    if (is.factor(x$var.summary[[view[2]]])) {
        m2 <- fac.seq(x$var.summary[[view[2]]], n.grid)
    }else {
        r2 <- range(x$var.summary[[view[2]]])
        m2 <- seq(r2[1], r2[2], length = n.grid)
    }
    v1 <- rep(m1, n.grid)
    v2 <- rep(m2, rep(n.grid, n.grid))
    newd <- data.frame(matrix(0, n.grid * n.grid, 0))
    
 
    for (i in 1:length(x$var.summary)) {
        ma <- cond[[v.names[i]]]

        # if no value for this variable is specified in cond, then take mean
        if (is.null(ma)) {
            ma <- x$var.summary[[i]]
            if (is.numeric(ma)) 
                ma <- ma[2]
        }
        
        if (is.matrix(x$var.summary[[i]])) {
            newd[[i]] <- matrix(ma, n.grid * n.grid, ncol(x$var.summary[[i]]), 
                byrow = TRUE)
        } else {newd[[i]] <- rep(ma, n.grid * n.grid)}
    }
    names(newd) <- v.names
    newd[[view[1]]] <- v1
    newd[[view[2]]] <- v2
    if (type == "link"){ 
        zlab <- paste("linear predictor")
    }else if (type == "response") {
        zlab <- type
    }else stop("type must be \"link\" or \"response\"")
    
    #--------------------------------------------
    # NEW:
    predval = predict.dropRanef(x,newd,dropRanef)
	newcoef <- coef(x)
	newvcov <- vcov(x)
	fv <- data.frame(fit = predval %*% newcoef)
	fv$se.fit <- sqrt(rowSums((predval%*%newvcov)*predval))
    
    # END NEW
    #--------------------------------------------
    
    z <- fv$fit
    if (too.far > 0) {
        ex.tf <- exclude.too.far(v1, v2, x$model[, view[1]], 
            x$model[, view[2]], dist = too.far)
        fv$se.fit[ex.tf] <- fv$fit[ex.tf] <- NA
    }
    if (is.factor(m1)) {
        m1 <- as.numeric(m1)
        m1 <- seq(min(m1) - 0.5, max(m1) + 0.5, length = n.grid)
    }
    if (is.factor(m2)) {
        m2 <- as.numeric(m2)
        m2 <- seq(min(m1) - 0.5, max(m2) + 0.5, length = n.grid)
    }
    if (se <= 0) {
        old.warn <- options(warn = -1)
        av <- matrix(c(0.5, 0.5, rep(0, n.grid - 1)), n.grid, 
            n.grid - 1)
        options(old.warn)
        max.z <- max(z, na.rm = TRUE)
        z[is.na(z)] <- max.z * 10000
        z <- matrix(z, n.grid, n.grid)
        surf.col <- t(av) %*% z %*% av
        surf.col[surf.col > max.z * 2] <- NA
        if (!is.null(zlim)) {
            if (length(zlim) != 2 || zlim[1] >= zlim[2]) 
                stop("Something wrong with zlim")
            min.z <- zlim[1]
            max.z <- zlim[2]
        }
        else {
            min.z <- min(fv$fit, na.rm = TRUE)
            max.z <- max(fv$fit, na.rm = TRUE)
        }
        surf.col <- surf.col - min.z
        surf.col <- surf.col/(max.z - min.z)
        surf.col <- round(surf.col * nCol)
        con.col <- 1
        if (color == "heat") {
            pal <- heat.colors(nCol)
            con.col <- 3
        }
        else if (color == "topo") {
            pal <- topo.colors(nCol)
            con.col <- 2
        }
        else if (color == "cm") {
            pal <- cm.colors(nCol)
            con.col <- 1
        }
        else if (color == "terrain") {
            pal <- terrain.colors(nCol)
            con.col <- 2
        }
        else if (color == "gray" || color == "bw") {
            pal <- gray(seq(0.1, 0.9, length = nCol))
            con.col <- 1
        }
        else stop("color scheme not recognised")
        if (is.null(contour.col)) 
            contour.col <- con.col
        surf.col[surf.col < 1] <- 1
        surf.col[surf.col > nCol] <- nCol
        if (is.na(col)) 
            col <- pal[as.array(surf.col)]
        z <- matrix(fv$fit, n.grid, n.grid)
        if (plot.type == "contour") {
            stub <- paste(ifelse("xlab" %in% dnm, "", ",xlab=view[1]"), 
                ifelse("ylab" %in% dnm, "", ",ylab=view[2]"), 
                ifelse("main" %in% dnm, "", ",main=zlab"), ",...)", 
                sep = "")
            if (color != "bw") {
                txt <- paste("image(m1,m2,z,col=pal,zlim=c(min.z,max.z)", 
                  stub, sep = "")
                eval(parse(text = txt))
                txt <- paste("contour(m1,m2,z,col=contour.col,zlim=c(min.z,max.z)", 
                  ifelse("add" %in% dnm, "", ",add=TRUE"),
                  ",...)", 
                  sep = "")
                eval(parse(text = txt))
            }
            else {
                txt <- paste("contour(m1,m2,z,col=1,zlim=c(min.z,max.z)", 
                  stub, sep = "")
                eval(parse(text = txt))
            }
        }
    }
    else {
        if (color == "bw" || color == "gray") {
            subs <- paste("grey are +/-", se, "s.e.")
            lo.col <- "gray"
            hi.col <- "gray"
        }
        else {
            subs <- paste("red/green are +/-", se, "s.e.")
            lo.col <- "green"
            hi.col <- "red"
        }
        if (!is.null(zlim)) {
            if (length(zlim) != 2 || zlim[1] >= zlim[2]) 
                stop("Something wrong with zlim")
            min.z <- zlim[1]
            max.z <- zlim[2]
        }
        else {
            z.max <- max(fv$fit + fv$se.fit * se, na.rm = TRUE)
            z.min <- min(fv$fit - fv$se.fit * se, na.rm = TRUE)
        }
        zlim <- c(z.min, z.max)
        z <- fv$fit - fv$se.fit * se
        z <- matrix(z, n.grid, n.grid)
        if (plot.type == "contour") 
            warning("sorry no option for contouring with errors: try plot.gam")
        stub <- paste(ifelse("xlab" %in% dnm, "", ",xlab=view[1]"), 
            ifelse("ylab" %in% dnm, "", ",ylab=view[2]"), ifelse("zlab" %in% 
                dnm, "", ",zlab=zlab"), ifelse("sub" %in% dnm, 
                "", ",sub=subs"), ",...)", sep = "")
        txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
            dnm, "", ",border=lo.col"), stub, sep = "")
        eval(parse(text = txt))
        par(new = TRUE)
        z <- fv$fit
        z <- matrix(z, n.grid, n.grid)
        txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
            dnm, "", ",border=\"black\""), stub, sep = "")
        eval(parse(text = txt))
        par(new = TRUE)
        z <- fv$fit + se * fv$se.fit
        z <- matrix(z, n.grid, n.grid)
        txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
            dnm, "", ",border=hi.col"), stub, sep = "")
        eval(parse(text = txt))
    }
    invisible(list(fv=fv, m1=m1, m2=m2))
}





# Created by Jacolien van Rij, 2013
# This modfication of vis.gam allows the user to specify one condition to plot as partial
# effect surface. 
# Use: 
# 1) view=c('Time','Trial') to specify which surface to plot, and 
# 2) select=2 to select a specific smooth term (necessary for distinguishing between several 
# levels of a predictor, such as te(Time, Trial):Condition2 of the tensor te(Time, Trial,by=Cond). 
# 3) cond=list(X=5) can be used to select a specific value of a continuous predictor 
# in a complex interaction, e.g. to specify the value of X in te(Time,Trial,X, by=Cond).
# Important: do not specify other predictors in cond that are not to be plotted.
pvis.gam <- function (x, view = NULL, select=NULL, cond=list(), n.grid = 30, too.far = 0, 
    col = NA, color = "topo", contour.col = 'black', se = -1, type = "link", 
    plot.type = "contour", zlim = NULL, nCol = 50, ...) 
{
    fac.seq <- function(fac, n.grid) {
        fn <- length(levels(fac))
        gn <- n.grid
        if (fn > gn) 
            mf <- factor(levels(fac))[1:gn]
        else {
            ln <- floor(gn/fn)
            mf <- rep(levels(fac)[fn], gn)
            mf[1:(ln * fn)] <- rep(levels(fac), rep(ln, fn))
            mf <- factor(mf, levels = levels(fac))
        }
        mf
    }
    dnm <- names(list(...))
    v.names <- names(x$var.summary)
    if (is.null(view)) {
        k <- 0
        view <- rep("", 2)
        for (i in 1:length(v.names)) {
            ok <- TRUE
            if (is.matrix(x$var.summary[[i]])) 
                ok <- FALSE
            else if (is.factor(x$var.summary[[i]])) {
                if (length(levels(x$var.summary[[i]])) <= 1) 
                  ok <- FALSE
            }
            else {
                if (length(unique(x$var.summary[[i]])) == 1) 
                  ok <- FALSE
            }
            if (ok) {
                k <- k + 1
                view[k] <- v.names[i]
            }
            if (k == 2) 
                break
        }
        if (k < 2) 
            stop("Model does not seem to have enough terms to do anything useful")
    }
    else {
        if (sum(view %in% v.names) != 2) {
            stop(paste(c("view variables must be one of", v.names), 
                collapse = ", ")) }
        for (i in 1:2) if (!inherits(x$var.summary[[view[i]]], 
            c("numeric", "factor"))) 
            stop("Don't know what to do with parametric terms that are not simple numeric or factor variables")
    }
    ok <- TRUE
    for (i in 1:2) if (is.factor(x$var.summary[[view[i]]])) {
        if (length(levels(x$var.summary[[view[i]]])) <= 1) 
            ok <- FALSE
    }
    else {
        if (length(unique(x$var.summary[[view[i]]])) <= 1) 
            ok <- FALSE
    }
    if (!ok) 
        stop(paste("View variables must contain more than one value. view = c(", 
            view[1], ",", view[2], ").", sep = ""))
    if (is.factor(x$var.summary[[view[1]]])) {
        m1 <- fac.seq(x$var.summary[[view[1]]], n.grid)
    }else {
        r1 <- range(x$var.summary[[view[1]]])
        m1 <- seq(r1[1], r1[2], length = n.grid)
    }
    if (is.factor(x$var.summary[[view[2]]])) {
        m2 <- fac.seq(x$var.summary[[view[2]]], n.grid)
    }else {
        r2 <- range(x$var.summary[[view[2]]])
        m2 <- seq(r2[1], r2[2], length = n.grid)
    }
    v1 <- rep(m1, n.grid)
    v2 <- rep(m2, rep(n.grid, n.grid))
    newd <- data.frame(matrix(0, n.grid * n.grid, 0))
    
    # add factor to condition list
    if (is.numeric(select)){
		if(x$smooth[[select]]$by != "NA"){
			level <- x$smooth[[select]]$by.level
			if(is.null(level)){
				level <- 1
			}
			cond[[x$smooth[[select]]$by]] = level
		}
	}
    
    for (i in 1:length(x$var.summary)) {
        ma <- cond[[v.names[i]]]

        # if no value for this variable is specified in cond, then take mean
        if (is.null(ma)) {
            ma <- x$var.summary[[i]]
            if (is.numeric(ma)) 
                ma <- ma[2]
        }
        
        if (is.matrix(x$var.summary[[i]])) {
            newd[[i]] <- matrix(ma, n.grid * n.grid, ncol(x$var.summary[[i]]), 
                byrow = TRUE)
        } else {newd[[i]] <- rep(ma, n.grid * n.grid)}
    }
    names(newd) <- v.names
    newd[[view[1]]] <- v1
    newd[[view[2]]] <- v2
    if (type == "link"){ 
        zlab <- paste("linear predictor")
    }else if (type == "response") {
        zlab <- type
    }else stop("type must be \"link\" or \"response\"")
    
    #--------------------------------------------
    # NEW:
    
    X1 <- predict(x, newdata=newd, type='terms', se.fit=TRUE)
    
    fv <- NULL
    
    # determine select value
	n.linpred <- 0
	if( length(attr(x$pterms, 'term.labels')) > 0 ){
		n.linpred <- length(attr(x$pterms, 'term.labels'))
	}

    
   	if (is.numeric(select)) {
       		fv <- data.frame(fit =X1$fit[,select+n.linpred])
    		fv$se.fit <- X1$se.fit[,select+n.linpred]	
    		#print(paste('Tensor(s) to be plotted:', names(X1$fit)[select+n.linpred]))
    		
    } else {
     
		if (!is.na(view[1])) {
	
			colnamesX1 <- NA
		
			for(i in 1:length(view)){
				if(is.na(colnamesX1[1]))
					colnamesX1 <- colnames(X1$fit)[grepl(view[i],colnames(X1$fit))]
				else
					colnamesX1 <- colnamesX1[ colnamesX1 %in% colnames(X1$fit)[grepl(view[i],colnames(X1$fit))] ]
			}

			if(length(colnamesX1) > 1){
				if(length(cond) > 0){
					select = c()
					for (i in 1:length(cond)) {
						# check if cond is factor:
						if(!is.numeric(x$var.summary[[names(cond[i])]])){
							test <- strsplit(colnamesX1, names(cond[i]))
							for(j in 1:length(test)){
								if( (length(test[[j]]) > 1) & (grepl(test[[j]][2],as.character(cond[[i]])))){
									select=c(select,j)
								}
							}
							colnamesX1 <- colnamesX1[select]
						}
					}
			}} 
		}
		
    	#print(paste('Tensor(s) to be plotted:', paste(colnamesX1, collapse=' + ')))
    		
    	if(length(colnamesX1) == 1){
    		fv <- data.frame(fit =X1$fit[,colnamesX1])
    		fv$se.fit <- X1$se.fit[,colnamesX1]
    	}else{ 
    		stop("More than one level is selected for plotting. Use 'select=(number of smooth, found in summary). Note that the surface does not reflect a true average of their partial effects. Please consider specifying one of these conditions in the cond parameter.", immediate. = TRUE)
		}	
    }
    
    # END NEW
    #--------------------------------------------
    
    z <- fv$fit
    if (too.far > 0) {
        ex.tf <- exclude.too.far(v1, v2, x$model[, view[1]], 
            x$model[, view[2]], dist = too.far)
        fv$se.fit[ex.tf] <- fv$fit[ex.tf] <- NA
    }
    if (is.factor(m1)) {
        m1 <- as.numeric(m1)
        m1 <- seq(min(m1) - 0.5, max(m1) + 0.5, length = n.grid)
    }
    if (is.factor(m2)) {
        m2 <- as.numeric(m2)
        m2 <- seq(min(m1) - 0.5, max(m2) + 0.5, length = n.grid)
    }
    if (se <= 0) {
        old.warn <- options(warn = -1)
        av <- matrix(c(0.5, 0.5, rep(0, n.grid - 1)), n.grid, 
            n.grid - 1)
        options(old.warn)
        max.z <- max(z, na.rm = TRUE)
        z[is.na(z)] <- max.z * 10000
        z <- matrix(z, n.grid, n.grid)
        surf.col <- t(av) %*% z %*% av
        surf.col[surf.col > max.z * 2] <- NA
        if (!is.null(zlim)) {
            if (length(zlim) != 2 || zlim[1] >= zlim[2]) 
                stop("Something wrong with zlim")
            min.z <- zlim[1]
            max.z <- zlim[2]
        }
        else {
            min.z <- min(fv$fit, na.rm = TRUE)
            max.z <- max(fv$fit, na.rm = TRUE)
        }
        surf.col <- surf.col - min.z
        surf.col <- surf.col/(max.z - min.z)
        surf.col <- round(surf.col * nCol)
        con.col <- 1
        if (color == "heat") {
            pal <- heat.colors(nCol)
            con.col <- 3
        }
        else if (color == "topo") {
            pal <- topo.colors(nCol)
            con.col <- 2
        }
        else if (color == "cm") {
            pal <- cm.colors(nCol)
            con.col <- 1
        }
        else if (color == "terrain") {
            pal <- terrain.colors(nCol)
            con.col <- 2
        }
        else if (color == "gray" || color == "bw") {
            pal <- gray(seq(0.1, 0.9, length = nCol))
            con.col <- 1
        }
        else stop("color scheme not recognised")
        if (is.null(contour.col)) 
            contour.col <- con.col
        surf.col[surf.col < 1] <- 1
        surf.col[surf.col > nCol] <- nCol
        if (is.na(col)) 
            col <- pal[as.array(surf.col)]
        z <- matrix(fv$fit, n.grid, n.grid)
        if (plot.type == "contour") {
            stub <- paste(ifelse("xlab" %in% dnm, "", ",xlab=view[1]"), 
                ifelse("ylab" %in% dnm, "", ",ylab=view[2]"), 
                ifelse("main" %in% dnm, "", ",main=zlab"), ",...)", 
                sep = "")
            if (color != "bw") {
                txt <- paste("image(m1,m2,z,col=pal,zlim=c(min.z,max.z)", 
                  stub, sep = "")
                eval(parse(text = txt))
                txt <- paste("contour(m1,m2,z,col=contour.col,zlim=c(min.z,max.z)", 
                  ifelse("add" %in% dnm, "", ",add=TRUE"),
                  ",...)", 
                  sep = "")
                eval(parse(text = txt))
            }
            else {
                txt <- paste("contour(m1,m2,z,col=1,zlim=c(min.z,max.z)", 
                  stub, sep = "")
                eval(parse(text = txt))
            }
        }
    }
    else {
        if (color == "bw" || color == "gray") {
            subs <- paste("grey are +/-", se, "s.e.")
            lo.col <- "gray"
            hi.col <- "gray"
        }
        else {
            subs <- paste("red/green are +/-", se, "s.e.")
            lo.col <- "green"
            hi.col <- "red"
        }
        if (!is.null(zlim)) {
            if (length(zlim) != 2 || zlim[1] >= zlim[2]) 
                stop("Something wrong with zlim")
            min.z <- zlim[1]
            max.z <- zlim[2]
        }
        else {
            z.max <- max(fv$fit + fv$se.fit * se, na.rm = TRUE)
            z.min <- min(fv$fit - fv$se.fit * se, na.rm = TRUE)
        }
        zlim <- c(z.min, z.max)
        z <- fv$fit - fv$se.fit * se
        z <- matrix(z, n.grid, n.grid)
        if (plot.type == "contour") 
            warning("sorry no option for contouring with errors: try plot.gam")
        stub <- paste(ifelse("xlab" %in% dnm, "", ",xlab=view[1]"), 
            ifelse("ylab" %in% dnm, "", ",ylab=view[2]"), ifelse("zlab" %in% 
                dnm, "", ",zlab=zlab"), ifelse("sub" %in% dnm, 
                "", ",sub=subs"), ",...)", sep = "")
        txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
            dnm, "", ",border=lo.col"), stub, sep = "")
        eval(parse(text = txt))
        par(new = TRUE)
        z <- fv$fit
        z <- matrix(z, n.grid, n.grid)
        txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
            dnm, "", ",border=\"black\""), stub, sep = "")
        eval(parse(text = txt))
        par(new = TRUE)
        z <- fv$fit + se * fv$se.fit
        z <- matrix(z, n.grid, n.grid)
        txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
            dnm, "", ",border=hi.col"), stub, sep = "")
        eval(parse(text = txt))
    }
    invisible(list(fv=fv, m1=m1, m2=m2))
}

# shows the 1D plot from a 2D difference surface
plotDiffFrom2D <- function(model,xvar,yvar,catvar,yvals,catlevels=NULL,eegAxis=T,ylim=NULL,main=NULL,ylab=NULL, singlePage=F, dropRanef=NULL, ...) { 
	dat = model$model
	
	if (is.null(ylab)) { 
		ylab = as.character(model$formula[2])
	}


	nX = 100
	rngX = max(na.exclude(dat[,xvar])) - min(na.exclude(dat[,xvar]))
	
	np = nX

	if (is.null(catlevels)) { 
		vals = sort(unique(dat[,catvar]))
	} else { 
		vals = catlevels
	}

	convert <- 1
	if(eegAxis==T) convert <- -1
			
	yvalslist <- sort(convert*ylim, decreasing=F)

	if (singlePage) { 
		par(mfrow=c(1,length(yvals)))
	} else { 
		par(ask=T)
	}

	for (yval in yvals) { 
		# datasets maken, waarbij je alleen de variabele van interest varieert, om verschilcurves te kunnen maken
		grp1 <- dat[which(is.na(dat[,catvar])), ] # get same columns
		grp1[1:np,] = dat[1,]
		grp1[,xvar] = seq(min(na.exclude(dat[,xvar])),max(na.exclude(dat[,xvar])), by=rngX/(nX-1) )
		grp1[,yvar] = yval 
		grp1[,catvar] = vals[1]
		#pred1 = predict.onlyInclude(model,grp1,onlyInclude=c(xvar,yvar,paste(catvar,vals[1],sep='')))
		pred1 = predict.dropRanef(model,grp1,dropRanef)

		grp2 = grp1
		grp2[,catvar] = vals[2]
		#pred2 = predict.onlyInclude(model,grp2,onlyInclude=c(xvar,yvar,paste(catvar,vals[2],sep='')))
		pred2 = predict.dropRanef(model,grp2,dropRanef)

		res1 = grp1
		res1$Xp <- pred2 - pred1
		res1$diff <- res1$Xp%*%coef(model)
		se.diff <- sqrt(rowSums((res1$Xp%*%vcov(model))*res1$Xp))
		res1$ul <- res1$diff + 1.96 * se.diff
		res1$ll <- res1$diff - 1.96 * se.diff
		res1$ul99 <- res1$diff + 2.58 * se.diff
		res1$ll99 <- res1$diff - 2.58 * se.diff

		res1$XXX = res1[,xvar]
		res1 = res1[order(res1$XXX),]

		if (is.null(main)) {
			mn = paste('Difference between ',vals[1],' and ',vals[2],' (',yvar,': ',yval,')',sep='')
		} else { 
			mn = main
		}
		
		if (is.null(ylim)) yvalslist <- sort(c(convert*min(res1$ll),convert*max(res1$ul)),decreasing=F)
		plot(res1$XXX,convert*res1$diff,type='l',xlab=xvar, main=mn, ylab=ylab, ylim=yvalslist, axes=F,...)
		box()
		axis(1)
		axis(2, at=axTicks(2), labels=convert*axTicks(2))
		abline(h=0)

		polygon(c(res1$XXX,rev(res1$XXX)),c(convert*res1$ul,rev(convert*res1$ll)),
				col=rgb(0.25,0.25,0.25,alpha=.25),border=NA) # shaded confidence interval
	}
}



# excludes random effects curves from model
predict.dropRanef <- function(model,newdat,dropRanef=NULL,method='lpmatrix') {
	if (is.null(dropRanef)) { 
		return(predict.gam(model,newdata=newdat,type=method, se.fit=T))
	} else {
		predval <- predict.gam(model, newdata=newdat, type=method, se.fit=T)

		pattern = c()
		if (sum(is.na(dropRanef)) == 0) { 
			for(i in 1:length(dropRanef)){
				pattern = c(pattern,paste("s\\(",dropRanef[i],"\\)",sep=''),paste("s\\(",dropRanef[i],",",sep=''),paste(",",dropRanef[i],"\\)",sep=''))
			}
		}

		if(length(pattern)>0){
			for(i in 1:length(pattern)){
				predval[,colnames(predval)[grepl(pattern[i],colnames(predval))]] = 0
			}
		}
		
		return(predval)
	}
}

# only includes columns which match smooths/variables with the name onlyInclude
predict.onlyInclude <- function(model,newdat,onlyInclude=NULL) {
	if (is.null(onlyInclude)) { 
		return(predict.gam(model,newdata=newdat,type='lpmatrix', se.fit=T))
	} else {
		predval <- predict.gam(model, newdata=newdat, type='lpmatrix', se.fit=T)


		pattern = ""
		if (!is.null(onlyInclude)) { 
			for(i in 1:length(onlyInclude)){
				if (i == 1) { 
					pattern = paste("^",onlyInclude[i],"$|s\\(",onlyInclude[i],"\\)|s\\(",onlyInclude[i],",|,",onlyInclude[i],"\\)|s\\(.*,",onlyInclude[i],",.*\\)|te\\(",onlyInclude[i],"\\)|te\\(",onlyInclude[i],",|,",onlyInclude[i],"\\)|te\\(.*,",onlyInclude[i],",.*\\)|ti\\(",onlyInclude[i],"\\)|ti\\(",onlyInclude[i],",|,",onlyInclude[i],"\\)|ti\\(.*,",onlyInclude[i],",.*\\)",sep='')
				} else { 
					pattern = paste(pattern,"|^",onlyInclude[i],"$|s\\(",onlyInclude[i],"\\)|s\\(",onlyInclude[i],",|,",onlyInclude[i],"\\)|s\\(.*,",onlyInclude[i],",.*\\)|te\\(",onlyInclude[i],"\\)|te\\(",onlyInclude[i],",|,",onlyInclude[i],"\\)|te\\(.*,",onlyInclude[i],",.*\\)|ti\\(",onlyInclude[i],"\\)|ti\\(",onlyInclude[i],",|,",onlyInclude[i],"\\)|ti\\(.*,",onlyInclude[i],",.*\\)",sep='')
				}
			}
		}
		if (pattern != "") { 
			predval[,colnames(predval)[!grepl(pattern,colnames(predval))]] = 0
		}
		
		return(predval)
	}
}

# source: http://lamages.blogspot.de/2013/04/how-to-change-alpha-value-of-colours-in.html
add.alpha <- function(col, alpha=0.25){
	if(missing(col))
	stop("Please provide a vector of colours.")
	apply(sapply(col, col2rgb)/255, 2, 
						function(x) 
						rgb(x[1], x[2], x[3], alpha=alpha))  
}

