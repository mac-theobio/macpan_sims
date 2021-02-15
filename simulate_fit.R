library(McMasterPandemic)
library(tidyverse)
library(parallel)
source("testify_funs.R")
source("batchtools.R")
# source("makestuff/makeRfuns.R")

pp <- read_params("PHAC_testify.csv")
pp <- fix_pars(pp,target=c(R0=1.3,Gbar=6))

start <- as.Date("2021-01-01")
end <- as.Date("2022-01-01")

dateVec <- seq.Date(start,end,by=1)

## Some saturating function of testing intensity

#S <- 2
#F <- 0.5
# h <- 6
# sat <- (F-S)*m/(h+m) + S


satfun <- function(S,F,h,m){
	(F-S)*m/(h+m) + S
}

timevars_increaseT <- data.frame(Date= dateVec
	, Symbol = rep("testing_intensity"), each=length(dateVec)
	, Relative_value = satfun(S=1
			, F=5
			, h=length(dateVec)/2
			, m=seq(0,length(dateVec)-1,by=1)
	)
)

## Don't like these as global variables
use_ode <- FALSE
testing_time <- "report"
stoch_obs <- TRUE
keep_all <- FALSE

# pp["obs_disp"] <- 5
pp["mu"] <- 0.99

simtable <- expand.grid(seed=1:10
  , W_asymp = c(0.01, 0.1, 0.25, 0.5)
)

simcalib <- function(x){

seed <- simtable[x,"seed"]
set.seed(seed)
pp[["W_asymp"]] <- simtable[x,"W_asymp"]
pp[["obs_disp"]] <- 5

dd <- simtestify(pp, timevars_increaseT)

dat <- (dd %>% select(date, postest, death, H)
	%>% gather(key="var",value="value",-date)
	%>% mutate(value=round(value))
	)
	opt_pars <- with(as.list(pp)
						  , list(params=c(log_beta0 = log(beta0)
						  					 # , log_E0 = log(E0)
						  					 , logit_W_asymp = 0 # -4.5
						  )
						  )
	)
	testing_data <- (timevars_increaseT
						  %>% filter(Symbol == "testing_intensity")
						  %>% transmute(Date, intensity = Relative_value*pp[["testing_intensity"]])
						  # %>% filter(Date >= as.Date("2021-02-20"))
	)
	
	sim_args <- list(ratemat_args = list(testing_time=testing_time))
	mod <- do.call(calibrate_comb
						, c(nlist(params = pp
									 , use_DEoptim = FALSE
									 , use_spline = FALSE
									 , debug_plot = FALSE
									 , data = dat
									 , opt_pars = opt_pars
									 , sim_args = sim_args
									 , use_testing = TRUE
									 , testing_data = testing_data
									 , start_date_offset=0
									 , maxit = 1000
						)
						)
	)
	
	ll <- list(fit=mod,data=dat,testing_data=testing_data,simpars = simtable[x,])
	saveRDS(ll,file=paste0("cachestuff/simcalib.",x,".RDS"))
}

mclapply(1:nrow(simtable),simcalib,mc.cores = 3)
	