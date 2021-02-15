library(tidyverse)
library(bbmle)

flist <- list.files(path = "cachestuff/",".RDS")

collect_pars <- function(x){
  mod <- readRDS(paste0("cachestuff/",x))
  lgf <- function(x){log(x/(1-x))}
  
  truedat <- data.frame(trueval = c(log(mod$fit$forecast_args$base_params[["beta0"]])
    , lgf(mod$fit$forecast_args$base_params[["W_asymp"]]))
    , params = c("params.log_beta0","params.logit_W_asymp")
    , seed = mod$simpars$seed
    , W_asymp = mod$simpars$W_asymp
  )

  CIs <- confint(mod$fit$mle2,method="quad",levels=0.95)
  est <- coef(mod$fit$mle2)
  dd <- (data.frame(CIs)
    %>% mutate(est = est
               , params = rownames(CIs)
        )
    %>% rename(lwr = X2.5..
               , upr = X97.5..
        )
  )
  
  dd2 <- (left_join(truedat,dd)
    %>% rowwise()
    %>% mutate(inCI = between(trueval,lwr,upr))
  )
}


dd_list <- lapply(flist,collect_pars)

ddcombo <- bind_rows(dd_list)

gg <- (ggplot(ddcombo,aes(x=factor(seed)))
  + geom_point(aes(y=est))
  + geom_pointrange(aes(y=est,ymin=lwr,ymax=upr,color=inCI))
  + scale_color_manual(values=c("red","grey"))
  + geom_hline(aes(yintercept = trueval))
  + facet_grid(params~W_asymp,scale="free")
  + theme_bw()
)
gg

ggsave("testify_sim.png")