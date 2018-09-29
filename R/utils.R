#' Print results of cv_auc
#' @export
#' @param x An object of class "cvauc"
#' @param ci_level Level of confidence interval to print. Defaults to 0.95. 
print.cvauc <- function(x, ci_level = 0.95, ...){
	cvtmle_ci <- if(x$se_cvtmle_type == "std"){
		c(x$est_cvtmle - stats::abs(qnorm((1-ci_level)/2))*x$se_cvtmle, x$est_cvtmle + stats::abs(qnorm((1-ci_level)/2))*x$se_cvtmle)
	}else{
		plogis(c(qlogis(x$est_cvtmle) - stats::abs(qnorm((1-ci_level)/2))*x$se_cvtmle, qlogis(x$est_cvtmle) + stats::abs(qnorm((1-ci_level)/2))*x$se_cvtmle))
	}
	cvtmle_rslt <- c(x$est_cvtmle, x$se_cvtmle, cvtmle_ci)

	esteq_ci <- if(x$se_esteq_type == "std"){
		c(x$est_esteq - stats::abs(qnorm((1-ci_level)/2))*x$se_esteq, x$est_esteq + stats::abs(qnorm((1-ci_level)/2))*x$se_esteq)
	}else{
		plogis(c(qlogis(x$est_esteq) - stats::abs(qnorm((1-ci_level)/2))*x$se_esteq, qlogis(x$est_esteq) + stats::abs(qnorm((1-ci_level)/2))*x$se_esteq))
	}
	esteq_rslt <- c(x$est_esteq, x$se_esteq, esteq_ci)	

	onestep_ci <- if(x$se_onestep_type == "std"){
		c(x$est_onestep - stats::abs(qnorm((1-ci_level)/2))*x$se_onestep, x$est_onestep + stats::abs(qnorm((1-ci_level)/2))*x$se_onestep)
	}else{
		plogis(c(qlogis(x$est_onestep) - stats::abs(qnorm((1-ci_level)/2))*x$se_onestep, qlogis(x$est_onestep) + stats::abs(qnorm((1-ci_level)/2))*x$se_onestep))
	}
	onestep_rslt <- c(x$est_onestep, x$se_onestep, onestep_ci)

	empirical_ci <- if(x$se_empirical_type == "std"){
		c(x$est_empirical - stats::abs(qnorm((1-ci_level)/2))*x$se_empirical, x$est_empirical + stats::abs(qnorm((1-ci_level)/2))*x$se_empirical)
	}else{
		plogis(c(qlogis(x$est_empirical) - stats::abs(qnorm((1-ci_level)/2))*x$se_empirical, qlogis(x$est_empirical) + stats::abs(qnorm((1-ci_level)/2))*x$se_empirical))
	}
	empirical_rslt <- c(x$est_empirical, x$se_empirical, empirical_ci)

	out <- data.frame(rbind(cvtmle_rslt, onestep_rslt, esteq_rslt, empirical_rslt))

  	colnames(out) <- c("est","se","cil","ciu")
  	row.names(out) <- c("cvtmle","onestep","esteq","standard")
  	print(out)
}

#' Print results of cv_scrnp
#' @export
#' @param x An object of class "cvauc"
#' @param ci_level Level of confidence interval to print. Defaults to 0.95. 
print.scrnp <- function(x, ci_level = 0.95, ...){
	cvtmle_ci <- if(x$se_cvtmle_type == "std"){
		c(x$est_cvtmle - stats::abs(qnorm((1-ci_level)/2))*x$se_cvtmle, x$est_cvtmle + stats::abs(qnorm((1-ci_level)/2))*x$se_cvtmle)
	}else{
		plogis(c(qlogis(x$est_cvtmle) - stats::abs(qnorm((1-ci_level)/2))*x$se_cvtmle, qlogis(x$est_cvtmle) + stats::abs(qnorm((1-ci_level)/2))*x$se_cvtmle))
	}
	cvtmle_rslt <- c(x$est_cvtmle, x$se_cvtmle, cvtmle_ci)

	esteq_ci <- if(x$se_esteq_type == "std"){
		c(x$est_esteq - stats::abs(qnorm((1-ci_level)/2))*x$se_esteq, x$est_esteq + stats::abs(qnorm((1-ci_level)/2))*x$se_esteq)
	}else{
		plogis(c(qlogis(x$est_esteq) - stats::abs(qnorm((1-ci_level)/2))*x$se_esteq, qlogis(x$est_esteq) + stats::abs(qnorm((1-ci_level)/2))*x$se_esteq))
	}
	esteq_rslt <- c(x$est_esteq, x$se_esteq, esteq_ci)	

	onestep_ci <- if(x$se_onestep_type == "std"){
		c(x$est_onestep - stats::abs(qnorm((1-ci_level)/2))*x$se_onestep, x$est_onestep + stats::abs(qnorm((1-ci_level)/2))*x$se_onestep)
	}else{
		plogis(c(qlogis(x$est_onestep) - stats::abs(qnorm((1-ci_level)/2))*x$se_onestep, qlogis(x$est_onestep) + stats::abs(qnorm((1-ci_level)/2))*x$se_onestep))
	}
	onestep_rslt <- c(x$est_onestep, x$se_onestep, onestep_ci)

	empirical_ci <- if(x$se_empirical_type == "std"){
		c(x$est_empirical - stats::abs(qnorm((1-ci_level)/2))*x$se_empirical, x$est_empirical + stats::abs(qnorm((1-ci_level)/2))*x$se_empirical)
	}else{
		plogis(c(qlogis(x$est_empirical) - stats::abs(qnorm((1-ci_level)/2))*x$se_empirical, qlogis(x$est_empirical) + stats::abs(qnorm((1-ci_level)/2))*x$se_empirical))
	}
	empirical_rslt <- c(x$est_empirical, x$se_empirical, empirical_ci)

	out <- data.frame(rbind(cvtmle_rslt, onestep_rslt, esteq_rslt, empirical_rslt))

  	colnames(out) <- c("est","se","cil","ciu")
  	row.names(out) <- c("cvtmle","onestep","esteq","standard")
  	print(out)
}