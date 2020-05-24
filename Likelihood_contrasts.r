# Likelihood contrasts for binary classification
# 17.1.2019
# Riku Klen, PhD
# Markku Karhunen, PhD

# Dependencies: requires nlme

# --------------------------------- Model fitting ----------------------------------------

lcont = function(X, dlt, form, id.name=NULL, random.formula=NULL){

	# X: data frame of covariates
	# dlt: labels of data points (0/1)
	# form: model formula to be applied on X, e.g. "myvar ~ x1 + x2"
  # id.name: name of the column which contains the grouping factor, default = "id"
  # random.formula: formula for random effect in LME, default = "~1|id"
	
	# The grouping factor
	X = as.data.frame(X)
	if( is.null(id.name) ){
	  id.name = "id"
	}
	if( is.null(random.formula) ){
	  random.formula = paste0("~1|",id.name)
	}
	require(nlme)
	
	# Fitting two LME models for two groups of data
	group0 = X[which(dlt==0),]
	group1 = X[which(dlt==1),]
	mod0 = lme(fixed=as.formula(form), random=as.formula(random.formula), data=group0)
	mod1 = lme(fixed=as.formula(form), random=as.formula(random.formula), data=group1)

	# The function returns both models, group labels and the model formula.
	# Unluckily, the models need to be re-fitted in predict.lcont.

	return(list(X=X, mod0=mod0, mod1=mod1, group0=group0, group1=group1, form=form))
}

# ------------------------ Predictions for a new data set --------------------------------	
	
predict.multiple = function(model, newx, id.name=NULL, random.formula=NULL){

	# model: output of function lcont
	# newx: new set of covariates (data frame)
  # id.name: name of the column which contains the grouping factor, default = "id"
  # random.formula: formula for random effect in LME, default = "~1|id"

	# The grouping factor
	newx = as.data.frame(newx)
	X = as.data.frame(model$X)
	if( is.null(id.name) ){
	  id.name = "id"
	}
	require(nlme)
	
	# Loop over individuals
	probs = NULL
	for( u in eval(parse(text=paste0('unique(newx$',id.name,')'))) ){
	  totake = eval(parse(text=paste0('which(newx$',id.name,'==u)')))
	  probs = c(probs, predict.lcont(model, newx[totake,], id.name=id.name, random.formula=random.formula))
	}
	
	return(probs) # probability scores!
}	
	
# --------------------------------- Generic prediction -----------------------------------	
# You should only access this function, if you know what you do.
	
predict.lcont = function(model, newx, id.name=NULL, random.formula=NULL, binary=F){

	# model: output of function lcont
	# newx: new set of covariates (data frame)
  # id.name: name of the column which contains the grouping factor, default = "id"
  # random.formula: formula for random effect in LME, default = "~1|id"
  # binary: use binary classification instead of probability score

	# The grouping factor
	newx = as.data.frame(newx)
	X = as.data.frame(model$X)
	if( is.null(id.name) ){
	  id.name = "id"
	}
	if( is.null(random.formula) ){
	  random.formula = paste0("~1|",id.name)
	}
	form = as.formula(model$form)
	require(nlme)
	
	# Initial log-likelihoods before adding the new observations
	like0_init = logLik(model$mod0)
	like1_init = logLik(model$mod1)
	
	# Trying to add the new observations to group 0
	group0 = rbind(model$group0, newx)
	mod0 = lme(fixed=form, random=as.formula(random.formula), data=group0)
	diff0 = logLik(mod0) - like0_init
	
	# Trying to add the new observations to group 1
	group1 = rbind(model$group1, newx)
	mod1 = lme(fixed=form, random=as.formula(random.formula), data=group1)
	diff1 = logLik(mod1) - like1_init

	# Estimated probability of group 1
	if(binary){
	  pred = ifelse(diff0<diff1,1,0)
	} else{
	  pred = exp(diff1) / (exp(diff0) + exp(diff1))
	}
	return(c(pred))
}

