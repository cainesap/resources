# ------------------------------------------------------------------------
# Function for model comparison.
#
# Note that this is only a heuristic, the summary is often more reliable. 
# Also use plots and other evidence to determine the best-fitting model.
#
# Version 2 (March 20, 2014)
# 
# Changes: 
# - new way of calculating degrees of freedom to use for chisquare 
# comparison, may yield different results.
# - model names are added in the output (addition of Martijn)
# - different chisquare test is used for method GCV
#
# Questions, bugs: vanrij.jacolien@gmail.com
# ------------------------------------------------------------------------

compareML <- function(model1, model2){
    # check whether models are comparable:
    if( model1$method != model2$method){
        stop(sprintf('Models are incomparable: method model1 = %s, method model2 = %s', model1$method, model2$method))
    } 


    type <- model1$method

    ml1 <- model1$gcv.ubre[1]
    ml2 <- model2$gcv.ubre[1] 

    ### OLD METHOD, SIMON SAYS NOT OK! ###
    # edf1 <- sum(model1$edf)
    # edf2 <- sum(model2$edf)
    ### NEW METHOD: ###
    ndf1 <- length(model1$sp) + model1$nsdf + sum(sapply(model1$smooth, FUN=function(x){x$null.space.dim}, USE.NAMES = FALSE))
    ndf2 <- length(model2$sp) + model2$nsdf + sum(sapply(model2$smooth, FUN=function(x){x$null.space.dim}, USE.NAMES = FALSE))

    if(model1$method == "GCV"){
        type <- 'AIC'
        ml1 <- AIC(model1)
        ml2 <- AIC(model2)

        ndf1 <- sum(model1$edf1)
        ndf2 <- sum(model2$edf1)
    } 


    # pchisq(4, .5, lower.tail=F) # p < .1
    # pchisq(-4, .5, lower.tail=F) # p = 1
    # pchisq(4, -.5, lower.tail=F) # NaN

    # Situation 1: model 1 has lower score, but model 2 has lower df. Is it significantly better model than model 2?
    cat(sprintf('%s: ', deparse(substitute(model1)))) 
    cat(sprintf('%s\n', deparse(model1$formula)))
    cat(sprintf('\n%s: ', deparse(substitute(model2)))) 
    cat(sprintf('%s\n', deparse(model2$formula)))

    

    if (model1$method %in% c("GCV","fREML", "REML", "ML")){
        if (( ml1 < ml2 ) & (ndf2 <= ndf1)){  
            cat(sprintf('\nChi-square test of %s scores\n-----\n', type))

            # twice the amount of difference in likelihood
            h1 <- pchisq(2*(ml2-ml1), abs(ndf1-ndf2), lower.tail=F)

            print( data.frame('Model'=c(deparse(substitute(model2)), deparse(substitute(model1))),
                Score = c(ml2, ml1),
                Edf = c(ndf2, ndf1),
                Chisq = c('', sprintf('%.3f',ml2-ml1)),
                Df = c('', sprintf('%.3f', abs(ndf1-ndf2))),
                p.value = c('', ifelse(h1<2e-16,sprintf(' < 2e-16'), ifelse(h1<.001, sprintf('%.3e', h1), 
                    ifelse(h1<.01, sprintf('%.3f', h1),
                        ifelse(h1<.05, sprintf('%.3f', h1), sprintf('%.3f', h1))))) ),
                Sig. = c('', ifelse(h1<.001, sprintf('***', h1), 
                    ifelse(h1<.01, sprintf('** ', h1),
                        ifelse(h1<.05, sprintf('*  ', h1), sprintf('   ', h1)))) ) ) ) 

        # Situation 2: model 2 has lower score, but model 1 has lower df. Is it significantly better model than model 1?
        } else if (( ml2 < ml1 ) & (ndf1 <= ndf2)) {     

            cat(sprintf('\nChi-square test of %s scores\n-----\n', type))  

            h1 <- pchisq(2*(ml1-ml2), abs(ndf1-ndf2), lower.tail=F)

            print( data.frame('Model'=c(deparse(substitute(model1)), deparse(substitute(model2))),
                Score = c(ml1, ml2),
                Edf = c(ndf1, ndf2),
                Chisq = c('', sprintf('%.3f',ml1-ml2)),
                Df = c('', sprintf('%.3f', abs(ndf1-ndf2))),
                p.value = c('', ifelse(h1<2e-16,sprintf(' < 2e-16'), ifelse(h1<.001, sprintf('%.3e', h1), 
                    ifelse(h1<.01, sprintf('%.3f', h1),
                        ifelse(h1<.05, sprintf('%.3f', h1), sprintf('%.3f', h1))))) ),
                Sig. = c('', ifelse(h1<.001, sprintf('***', h1), 
                    ifelse(h1<.01, sprintf('** ', h1),
                        ifelse(h1<.05, sprintf('*  ', h1), sprintf('   ', h1)))) )  ) ) 

        # Situation 3: model 1 has lower score, and also lower df. 
        } else if (( ml1 < ml2 ) & (ndf1 <= ndf2)) {        
            cat(sprintf('\nModel %s preferred: lower %s score (%.3f), and lower df (%.3f).\n-----\n', deparse(substitute(model1)), type, ml2-ml1, ndf2-ndf1))
            print( data.frame('Model'=c(deparse(substitute(model2)), deparse(substitute(model1))),
                Score = c(ml2, ml1),
                Edf = c(ndf2, ndf1),
                Difference = c('', sprintf('%.3f',ml2-ml1)),
                Df = c('', sprintf('%.3f', abs(ndf1-ndf2))) ) )  
            
        # Situation 4: model 2 has lower score, and also lower df. 
        } else if (( ml2 < ml1 ) & (ndf2 <= ndf1)) {        
            cat(sprintf('\nModel %s preferred: lower %s score (%.3f), and lower df (%.3f).\n-----\n', deparse(substitute(model2)), type, ml1-ml2, ndf1-ndf2))
            print( data.frame('Model'=c(deparse(substitute(model1)), deparse(substitute(model2))),
                Score = c(ml1, ml2),
                Edf = c(ndf1, ndf2),
                Difference = c('', sprintf('%.3f',ml2-ml1)),
                Df = c('', sprintf('%.3f', abs(ndf1-ndf2))) ) )  

        } else {
            cat('No preference:\n-----\n')
            print( data.frame('Model'=c(deparse(substitute(model1)), deparse(substitute(model2))),
                Score = c(ml1, ml2),
                Edf = c(ndf1, ndf2),
                Difference = c('', sprintf('%.3f',ml2-ml1)),
                Df = c('', sprintf('%.3f', abs(ndf1-ndf2))) ) )  
        }

        cat('\n')

        if(type != "AIC"){
            if (is.null(model1$AR1.rho)) { 
                rho1 = 0
            } else { 
                rho1 = model1$AR1.rho
            }

            if (is.null(model2$AR1.rho)) { 
                rho2 = 0
            } else { 
                rho2 = model2$AR1.rho
            }

            if (rho1 == 0 & rho2 == 0) { # AIC is useless for models with rho
                if (AIC(model1) == AIC(model2)) { 
                    cat(sprintf('AIC difference: 0.\n\n'))
                } else { 
                    cat(sprintf('AIC difference: %.2f, model %s has lower AIC.\n\n', 
                        AIC(model1)-AIC(model2), 
                        ifelse(AIC(model1)>=AIC(model2),deparse(substitute(model2)),deparse(substitute(model1)))))
                }
            }else{
                warning(sprintf(' AIC is not reliable, because an AR1 model is included (rho1 = %f, rho2 = %f). ', rho1, rho2))
            }
        }
    } else {
        cat(sprintf('CompareML is not implemented for method %s.\n', model1$method))
    }

    if ( abs(ml1-ml2) <= 5) {
        warning(sprintf('Only small difference in %s...\n', type))
    } 

}