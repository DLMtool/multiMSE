
SS_import <- function(SSdir, silent = FALSE, ...) {
  if(!requireNamespace("r4ss", quietly = TRUE)) {
    stop("Download the r4ss package to use this function. It is recommended to install the Github version with: devtools::install_github(\"r4ss/r4ss\")", call. = FALSE)
  }

  dots <- list(dir = SSdir, ...)
  if(!any(names(dots) == "covar")) dots$covar <- FALSE
  if(!any(names(dots) == "forecast")) dots$forecast <- FALSE
  #if(!any(names(dots) == "ncols")) dots$ncols <- 1e3
  if(!any(names(dots) == "printstats")) dots$printstats <- FALSE
  if(!any(names(dots) == "verbose")) dots$verbose <- FALSE
  if(!any(names(dots) == "warn")) dots$warn <- FALSE

  if(!silent) {
    message(paste("-- Using function SS_output of package r4ss version", packageVersion("r4ss"), "to extract data from SS file structure --"))
    message(paste("Reading directory:", SSdir))
  }
  replist <- try(do.call(r4ss::SS_output, dots), silent = TRUE)
  if(is.character(replist)) stop("r4ss::SS_output function returned an error -\n", replist, call. = FALSE)
  if(!silent) message("-- End of r4ss operations --\n")

  return(replist)
}

SS_steepness <- function(replist, mainyrs) {

  ###### Steepness
  if(replist$SRRtype == 3 || replist$SRRtype == 6) { # Beverton-Holt SR
    SRrel <- 1L
    h <- replist$parameters[grepl("steep", rownames(replist$parameters)), ]$Value
  } else if(replist$SRRtype == 2) {
    SRrel <- 2L
    h <- replist$parameters[grepl("SR_Ricker", rownames(replist$parameters)), ]$Value
  } else if(replist$SRRtype == 7) {
    OM@SRrel <- 1L

    s_frac <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Sfrac"]
    Beta <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Beta"]

    s0 <- 1/SpR0
    z0 <- -log(s0)
    z_min <- z0 * (1 - s_frac)

    h <- 0.2 * exp(z0 * s_frac * (1 - 0.2 ^ Beta))

  } else {
    if(packageVersion("r4ss") == 1.24) {
      SR_ind <- match(mainyrs, replist$recruit$year)
      SSB <- replist$recruit$spawn_bio[SR_ind]
      SSB0 <- replist$derived_quants[replist$derived_quants$LABEL == "SPB_Virgin", 2]
    } else {
      SR_ind <- match(mainyrs, replist$recruit$Yr)
      SSB <- replist$recruit$SpawnBio[SR_ind]
      SSB0 <- replist$derived_quants[replist$derived_quants$Label == "SSB_Virgin", 2]
    }
    SRrel <- 1L

    rec <- replist$recruit$pred_recr[SR_ind] # recruits to age 0
    SpR0 <- SSB0/(R0 * ifelse(season_as_years, nseas, 1))

    h <- SRopt(1, SSB, rec, SpR0, plot = FALSE, type = ifelse(SR == 1, "BH", "Ricker"))
  }
  return(list(SRrel = SRrel, h = h))
}

# #' A function that samples multivariate normal (logspace) variables
# #'
# #' @param xmu The mean (normal space) of the first (x) variable
# #' @param ymu The mean (normal space) of the second (y) variable
# #' @param xcv The coefficient of variation (normal space, log normal sd) of the x variable
# #' @param nsim The number of random draws
# #' @param cor The off-diagonal (symmetrical) correlation among x and y
# #' @param ploty Whether a plot of the sampled variables should be produced
# #' @author T. Carruthers
# #' @export negcorlogspace
#' @importFrom mvtnorm rmvnorm
negcorlogspace<-function(xmu,ymu,xcv=0.1,nsim,cor=-0.9,ploty=FALSE){

  varcov=matrix(c(1,cor,cor,1),nrow=2)
  out<-mvtnorm::rmvnorm(nsim,c(0,0),sigma=varcov)
  out<-out/rep(apply(out,2,sd)/xcv,each=nsim)
  out<-exp(out)
  out<-out/rep(apply(out,2,mean),each=nsim)
  out[,1]<-out[,1]*xmu
  out[,2]<-out[,2]*ymu
  if(ploty)plot(out[,1],out[,2])
  out

}

# #' Predict recruitment and return fit to S-R observations
# #'
# #' @description Internal function to \link{optSR}
# #' @param pars an initial guess at model parameters steepness and R0
# #' @param SSB 'observations' of spawning biomass
# #' @param rec 'observations' (model predictions) of recruitment
# #' @param SSBpR spawning stock biomass per recruit at unfished conditions
# #' @param mode should fit (= 1) or recruitment deviations (not 1) be returned
# #' @param plot should a plot of the model fit be produced?#'
# #' @param type what type of stock recruitment curve is being fitted ("BH" = Beverton-Holt or "Ricker")
# #' @author T. Carruthers
# #' @export
getSR <- function(pars, SSB, rec, SSBpR, mode = 1, plot = FALSE, type = c("BH", "Ricker")){
  R0 <- exp(pars[2])
  if(type == "BH") {
    h <- 0.2 + 0.8 * ilogit(pars[1])
    recpred<-((0.8*R0*h*SSB)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB))
  }
  if(type == "Ricker") {
    h <- 0.2 + exp(pars[1])
    recpred <- SSB * (1/SSBpR) * (5*h)^(1.25*(1 - SSB/(R0*SSBpR)))
  }

  if(plot){
    ord <- order(SSB)
    plot(SSB[ord], rec[ord], ylim=c(0, max(rec, R0)), xlim=c(0, max(SSB, R0*SSBpR)), xlab="", ylab="")
    SSB2 <- seq(0, R0*SSBpR, length.out=500)
    if(type == "BH") recpred2 <- ((0.8*R0*h*SSB2)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB2))
    if(type == "Ricker") recpred2 <- SSB2 * (1/SSBpR) * (5*h)^(1.25*(1 - SSB2/(R0*SSBpR)))
    lines(SSB2, recpred2, col='blue')
    abline(v=c(0.2*R0*SSBpR, R0*SSBpR), lty=2, col='red')
    abline(h=c(R0, R0*h), lty=2, col='red')
    legend('topright', legend=c(paste0("h = ", round(h,3)), paste0("ln(R0) = ", round(log(R0),3))), bty='n')
  }

  if(mode==1){
    #return(sum(((recpred-rec)/10000)^2))
    sigmaR <- sqrt(sum((log(rec/recpred))^2)/length(recpred))
    return(-sum(dnorm(log(rec)-log(recpred),0,sigmaR,log=T)))
    #-dnorm(pars[1],0,6,log=T)) # add a vague prior on h = 0.6
    #return(-sum(dnorm(recpred,rec,rec*0.5,log=T)))
  }else{
    return(rec-recpred)
  }
}

# #' Wrapper for estimating stock recruitment parameters from resampled stock-recruitment data
# #'
# #' @param x position to accommodate lapply-type functions
# #' @param SSB 'observations' of spawning biomass
# #' @param rec 'observations' (model predictions) of recruitment
# #' @param SSBpR spawning stock biomass per recruit at unfished conditions
# #' @param pars an initial guess at model parameters steepness and R0
# #' @param frac the fraction of observations for resampling
# #' @param plot should a plot of model fit be produced?
# #' @param type what type of stock recruitment curve is being fitted ("BH" = Beverton-Holt or "Ricker")
# #' @return Estimated value of steepness.
# #' @author T. Carruthers
# #' @export
optSR<-function(x, SSB, rec, SSBpR, pars, frac = 0.5, plot = FALSE, type = c("BH", "Ricker")) {
  type <- match.arg(type)
  samp <- sample(1:length(SSB), size = ceiling(length(SSB) * frac), replace = FALSE)
  opt <- optim(pars, getSR, method = "BFGS", #lower = c(-6, pars[2]/50), upper = c(6, pars[2] * 50),
               SSB = SSB[samp], rec = rec[samp], SSBpR = SSBpR, mode = 1, plot = FALSE, type = type)
  if(plot) getSR(opt$par, SSB, rec, SSBpR, mode = 2, plot = plot, type = type)
  if(type == "BH") h <- 0.2 + 0.8 * ilogit(opt$par[1])
  if(type == "Ricker") h <- 0.2 + exp(opt$par[1])
  return(h)
}

# #' Function that returns a stochastic estimate of steepness given observed stock recruitment data
# #'
# #' @param nsim number of samples of steepness to generate
# #' @param SSB 'observations' of spawning biomass
# #' @param rec 'observations' (model predictions) of recruitment
# #' @param SSBpR spawning stock biomass per recruit at unfished conditions
# #' @param plot should plots of model fit be produced?
# #' @param type what type of stock recruitment curve is being fitted ("BH" = Beverton-Holt or "Ricker")
# #' @return Vector of length nsim with steepness values.
# #' @author T. Carruthers
# #' @export
SRopt <- function(nsim, SSB, rec, SSBpR, plot = FALSE, type = c("BH", "Ricker")) {
  type <- match.arg(type)
  R0temp <- rec[1] # have a guess at R0 for initializing nlm
  pars <- c(0, log(R0temp))
  #SSBpR=SSB[1]/rec[1]
  vapply(1:nsim, optSR, numeric(1), SSB = SSB, rec = rec, SSBpR = SSBpR, pars = pars, frac = 0.8,
         plot = plot, type = type)
}


# #' Extracts growth parameters from a SS3 r4ss replist
# #'
# #' @param replist the list output of the r4ss SS_output function (a list of assessment inputs / outputs)
# #' @param seas The reference season for the growth (not actually sure what this does yet)
# #' @author T. Carruthers
# #' @export getGpars
getGpars <- function(replist, seas = 1) { # This is a rip-off of SSPlotBiology

  if(packageVersion("r4ss") == 1.24) {
    res <- getGpars_r4ss_124(replist, seas)
  } else res <- getGpars_r4ss_134(replist, seas)
  #if(nrow(res) == 0) warning("No growth parameters were retrieved from r4ss output.")
  return(res)
}

getGpars_r4ss_124 <- function(replist, seas = 1) {

  nseasons <- replist$nseasons
  growdat <- replist$endgrowth[replist$endgrowth$Seas == seas, ]
  growdat$CV_Beg <- growdat$SD_Beg/growdat$Len_Beg
  growthCVtype <- replist$growthCVtype
  biology <- replist$biology
  startyr <- replist$startyr
  FecType <- replist$FecType
  FecPar1name <- replist$FecPar1name
  FecPar2name <- replist$FecPar2name
  FecPar1 <- replist$FecPar1
  FecPar2 <- replist$FecPar2
  parameters <- replist$parameters
  nsexes <- replist$nsexes
  mainmorphs <- replist$mainmorphs
  accuage <- replist$accuage
  startyr <- replist$startyr
  endyr <- replist$endyr
  growthvaries <- replist$growthvaries
  growthseries <- replist$growthseries
  ageselex <- replist$ageselex
  MGparmAdj <- replist$MGparmAdj
  wtatage <- replist$wtatage
  Growth_Parameters <- replist$Growth_Parameters
  Grow_std <- replist$derived_quants[grep("Grow_std_", replist$derived_quants$LABEL), ]
  if (nrow(Grow_std) == 0) {
    Grow_std <- NULL
  }  else {
    Grow_std$pattern <- NA
    Grow_std$sex_char <- NA
    Grow_std$sex <- NA
    Grow_std$age <- NA
    for (irow in 1:nrow(Grow_std)) {
      tmp <- strsplit(Grow_std$LABEL[irow], split = "_")[[1]]
      Grow_std$pattern[irow] <- as.numeric(tmp[3])
      Grow_std$sex_char[irow] <- tmp[4]
      Grow_std$age[irow] <- as.numeric(tmp[6])
    }
    Grow_std$sex[Grow_std$sex_char == "Fem"] <- 1
    Grow_std$sex[Grow_std$sex_char == "Mal"] <- 2
  }
  if (!is.null(replist$wtatage_switch)) {
    wtatage_switch <- replist$wtatage_switch
  } else{ stop("SSplotBiology function doesn't match SS_output function. Update one or both functions.")
  }
  if (wtatage_switch)
    cat("Note: this model uses the empirical weight-at-age input.\n",
        "     Therefore many of the parametric biology quantities which are plotted\n",
        "     are not used in the model.\n")
  if (!seas %in% 1:nseasons)
    stop("'seas' input should be within 1:nseasons")

  if (length(mainmorphs) > nsexes) {
    cat("!Error with morph indexing in SSplotBiology function.\n",
        " Code is not set up to handle multiple growth patterns or birth seasons.\n")
  }
  #if (FecType == 1) {
  #  fec_ylab <- "Eggs per kg"
  #  FecX <- biology$Wt_len_F
  #  FecY <- FecPar1 + FecPar2 * FecX
  #}

  growdatF <- growdat[growdat$Gender == 1 & growdat$Morph ==
                        mainmorphs[1], ]
  growdatF$Sd_Size <- growdatF$SD_Beg

  if (growthCVtype == "logSD=f(A)") {
    growdatF$high <- qlnorm(0.975, meanlog = log(growdatF$Len_Beg),
                            sdlog = growdatF$Sd_Size)
    growdatF$low <- qlnorm(0.025, meanlog = log(growdatF$Len_Beg),
                           sdlog = growdatF$Sd_Size)
  }  else {
    growdatF$high <- qnorm(0.975, mean = growdatF$Len_Beg,
                           sd = growdatF$Sd_Size)
    growdatF$low <- qnorm(0.025, mean = growdatF$Len_Beg,
                          sd = growdatF$Sd_Size)
  }
  if (nsexes > 1) {
    growdatM <- growdat[growdat$Gender == 2 & growdat$Morph ==
                          mainmorphs[2], ]
    xm <- growdatM$Age_Beg
    growdatM$Sd_Size <- growdatM$SD_Beg
    if (growthCVtype == "logSD=f(A)") {
      growdatM$high <- qlnorm(0.975, meanlog = log(growdatM$Len_Beg),
                              sdlog = growdatM$Sd_Size)
      growdatM$low <- qlnorm(0.025, meanlog = log(growdatM$Len_Beg),
                             sdlog = growdatM$Sd_Size)
    }    else {
      growdatM$high <- qnorm(0.975, mean = growdatM$Len_Beg,
                             sd = growdatM$Sd_Size)
      growdatM$low <- qnorm(0.025, mean = growdatM$Len_Beg,
                            sd = growdatM$Sd_Size)
    }
  } else growdatM <- NULL

  list(Female = growdatF, Male = growdatM)

}

getGpars_r4ss_134 <- function(replist, seas = 1) {
  nseasons <- replist$nseasons
  growdat <- replist$endgrowth[replist$endgrowth$Seas == seas,
  ]
  growdat$CV_Beg <- growdat$SD_Beg/growdat$Len_Beg
  growthCVtype <- replist$growthCVtype
  biology <- replist$biology
  startyr <- replist$startyr
  FecType <- replist$FecType
  FecPar1name <- replist$FecPar1name
  FecPar2name <- replist$FecPar2name
  FecPar1 <- replist$FecPar1
  FecPar2 <- replist$FecPar2
  parameters <- replist$parameters
  nsexes <- replist$nsexes
  accuage <- replist$accuage
  startyr <- replist$startyr
  endyr <- replist$endyr
  growthvaries <- replist$growthvaries
  growthseries <- replist$growthseries
  ageselex <- replist$ageselex
  MGparmAdj <- replist$MGparmAdj
  wtatage <- replist$wtatage
  if ("comment" %in% names(wtatage)) {
    wtatage <- wtatage[, -grep("comment", names(wtatage))]
  }
  M_at_age <- replist$M_at_age
  Growth_Parameters <- replist$Growth_Parameters
  #if (is.null(morphs)) {
  morphs <- replist$mainmorphs
  #}
  Grow_std <- replist$derived_quants[grep("Grow_std_", replist$derived_quants$Label), ]
  if (nrow(Grow_std) == 0) {
    Grow_std <- NULL
  } else {
    Grow_std$pattern <- NA
    Grow_std$sex_char <- NA
    Grow_std$sex <- NA
    Grow_std$age <- NA
    for (irow in 1:nrow(Grow_std)) {
      tmp <- strsplit(Grow_std$Label[irow], split = "_")[[1]]
      Grow_std$pattern[irow] <- as.numeric(tmp[3])
      Grow_std$sex_char[irow] <- tmp[4]
      Grow_std$age[irow] <- as.numeric(tmp[6])
    }
    Grow_std$sex[Grow_std$sex_char == "Fem"] <- 1
    Grow_std$sex[Grow_std$sex_char == "Mal"] <- 2
  }
  if (!is.null(replist$wtatage_switch)) {
    wtatage_switch <- replist$wtatage_switch
  } else {
    stop("SSplotBiology function doesn't match SS_output function.",
         "Update one or both functions.")
  }
  #if (wtatage_switch) {
  #  cat("Note: this model uses the empirical weight-at-age input.\n",
  #      "      Plots of many quantities related to growth are skipped.\n")
  #}
  if (!seas %in% 1:nseasons)
    stop("'seas' input should be within 1:nseasons")
  #if (nseasons > 1) {
  #  labels[6] <- gsub("beginning of the year", paste("beginning of season",
  #                                                   seas), labels[6])
  #}

  if (length(morphs) > nsexes) {
    cat("!Error with morph indexing in SSplotBiology function.\n",
        " Code is not set up to handle multiple growth patterns or birth seasons.\n")
  }
  #if (FecType == 1) {
  #  fec_ylab <- "Eggs per kg"
  #  fec_xlab <- labels[8]
  #  FecX <- biology$Wt_len_F
  #  FecY <- FecPar1 + FecPar2 * FecX
  #}
  #if (labels[11] != "Default fecundity label")
  #  fec_ylab <- labels[11]
  growdatF <- growdat[growdat$Sex == 1 & growdat$Morph == morphs[1], ]
  growdatF$Sd_Size <- growdatF$SD_Beg
  if (growthCVtype == "logSD=f(A)") {
    growdatF$high <- qlnorm(0.975, meanlog = log(growdatF$Len_Beg),
                            sdlog = growdatF$Sd_Size)
    growdatF$low <- qlnorm(0.025, meanlog = log(growdatF$Len_Beg),
                           sdlog = growdatF$Sd_Size)
  } else {
    growdatF$high <- qnorm(0.975, mean = growdatF$Len_Beg,
                           sd = growdatF$Sd_Size)
    growdatF$low <- qnorm(0.025, mean = growdatF$Len_Beg,
                          sd = growdatF$Sd_Size)
  }
  if (nsexes > 1) {
    growdatM <- growdat[growdat$Sex == 2 & growdat$Morph == morphs[2], ]
    xm <- growdatM$Age_Beg
    growdatM$Sd_Size <- growdatM$SD_Beg
    if (growthCVtype == "logSD=f(A)") {
      growdatM$high <- qlnorm(0.975, meanlog = log(growdatM$Len_Beg),
                              sdlog = growdatM$Sd_Size)
      growdatM$low <- qlnorm(0.025, meanlog = log(growdatM$Len_Beg),
                             sdlog = growdatM$Sd_Size)
    } else {
      growdatM$high <- qnorm(0.975, mean = growdatM$Len_Beg,
                             sd = growdatM$Sd_Size)
      growdatM$low <- qnorm(0.025, mean = growdatM$Len_Beg,
                            sd = growdatM$Sd_Size)
    }
  } else growdatM <- NULL

  list(Female = growdatF, Male = growdatM)

}

# i = 1 (female)
# i = 2 (male)
SS2MOM_stock <- function(i, replist, mainyrs, nyears, MOM) {
  allyears <- nyears + MOM@proyears

  Stock <- new("Stock")
  Stock@Name <- ifelse(i == 1, "Female", "Male")
  if(nrow(replist$movement) > 0) warning("Movement detected in SS model but not imported right now.")
  Stock@Size_area_1 <- Stock@Frac_area_1 <- Stock@Prob_staying <- rep(0.5, 2)

  cpars_bio <- list()

  ###### maxage
  N_at_age <- replist$natage[replist$natage$Sex == i, ]
  Stock@maxage <- suppressWarnings(colnames(N_at_age) %>% as.numeric()) %>% max(na.rm = TRUE)

  ###### R0
  R0_row <- N_at_age$`Beg/Mid` == "B" & N_at_age$Era == "VIRG"
  R0_col <- parse(text = paste0("N_at_age$`", 1, "`")) %>% eval() # recruit to age-1
  Stock@R0 <- R0_col[R0_row]

  ###### Biological parameters
  endgrowth <- replist$endgrowth[replist$endgrowth$Sex == i, ]

  # M
  Stock@M <- Stock@M2 <- endgrowth$M[-1]

  # Steepness
  SR_par <- SS_steepness(replist, mainyrs)
  Stock@SRrel <- SR_par[[1]]
  Stock@h <- rep(SR_par[[2]], 2)

  Stock@Perr <- rep(replist$sigma_R_in, 2)

  # Perr_y
  Rec_main <- replist$recruit[vapply(mainyrs, match, numeric(1), table = replist$recruit$Yr, nomatch = 0), ]
  Rdev <- Rec_main$pred_recr/Rec_main$exp_recr

  # Rec_early <- replist$recruit[vapply(c((min(mainyrs)-(Stock@maxage-1)):(min(mainyrs)-1)), match, numeric(1), table = replist$recruit$Yr, nomatch = NA), ] # original

  Rec_early <- replist$recruit[vapply(c((min(mainyrs-1)-(Stock@maxage-1)):(min(mainyrs-1))), match, numeric(1), table = replist$recruit$Yr, nomatch = NA), ]

  Rdev_early <- Rec_early$pred_recr/Rec_early$exp_recr
  Rdev_early[is.na(Rdev_early)] <- 1

  cpars_bio$Perr_y <- c(Rdev_early, Rdev) %>% matrix(nrow = MOM@nsim, ncol = Stock@maxage + nyears, byrow = TRUE)

  # AC
  Stock@AC <- log(cpars_bio$Perr_y[1, ]) %>% acf(lag.max = 1, plot = FALSE) %>% getElement("acf") %>%
    getElement(2) %>% rep(2)

  # Length at age - not found for terminal year
  Len_age_df <- replist$growthseries[replist$growthseries$Morph == i, ]
  Len_age_df <- Len_age_df[vapply(mainyrs, match, numeric(1), table = Len_age_df$Yr, nomatch = 0), ]
  Len_age <- do.call(rbind, lapply(1:Stock@maxage, function(x) parse(text = paste0("Len_age_df$`", x, "`")) %>% eval()))
  if(ncol(Len_age) == nyears - 1) Len_age <- cbind(Len_age, endgrowth$Len_Beg[-1])
  Len_age_pro <- matrix(Len_age[, nyears], Stock@maxage, MOM@proyears)

  cpars_bio$Len_age <- cbind(Len_age, Len_age_pro) %>% array(c(Stock@maxage, allyears, MOM@nsim)) %>%
    aperm(c(3, 1, 2))

  Stock@LenCV <- mean(endgrowth$SD_Beg[-1]/endgrowth$Len_Beg[-1]) %>% rep(2)

  # Weight at age
  Wt_age_df <- replist$mean_body_wt[replist$mean_body_wt$Morph == i, ]
  Wt_age_df <- Wt_age_df[vapply(mainyrs, match, numeric(1), table = Wt_age_df$Yr, nomatch = 0), ]
  Wt_age <- do.call(rbind, lapply(1:Stock@maxage, function(x) parse(text = paste0("Wt_age_df$`", x, "`")) %>% eval()))
  if(ncol(Wt_age) == nyears - 1) Wt_age <- cbind(Wt_age, endgrowth$Wt_Beg[-1])
  Wt_age_pro <- matrix(Wt_age[, nyears], Stock@maxage, MOM@proyears)

  cpars_bio$Wt_age <- cbind(Wt_age, Wt_age_pro) %>% array(c(Stock@maxage, allyears, MOM@nsim)) %>%
    aperm(c(3, 1, 2))

  cpars_bio$plusgroup <- rep(1, MOM@nsim)

  # numbers-at-age in initial year
  ns <- replist$natage %>% dplyr::filter(Sex==i, Yr==mainyrs[1], `Beg/Mid`=='B')
  ind <- which(names(ns)=='1')
  cpars_bio$N_at_age_initial <- ns[1,ind:length(ns)] %>% as.numeric()

  # Maturity at age - technically fecundity = 0 for males
  Mat_age <- endgrowth$Len_Mat[-1] * endgrowth$Age_Mat[-1]
  cpars_bio$Mat_age <- array(Mat_age, c(Stock@maxage, allyears, MOM@nsim)) %>% aperm(c(3, 1, 2))

  # Depletion
  if(i == 1) {
    Stock@D <- rep(replist$recruit$SpawnBio[match(max(mainyrs), replist$recruit$Yr)]/replist$recruit$SpawnBio[1], 2)
  } else {
    # Calculate 'SSB' for males, in SS, SSB is always 0 for males
    N_virg <- N_at_age[N_at_age$Era == "VIRG" & N_at_age$`Beg/Mid` == "B", ]
    N_virg2 <- vapply(1:Stock@maxage, function(x) N_virg[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))

    N_now <- N_at_age[N_at_age$Yr == max(mainyrs) & N_at_age$`Beg/Mid` == "B", ]
    N_now2 <- vapply(1:Stock@maxage, function(x) N_now[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))

    Stock@D <- rep(sum(N_now2 * Mat_age * Wt_age[, nyears])/sum(N_virg2 * Mat_age * Wt_age[, 1]), 2)
  }

  # Get Fdisc from fleets

  # not used - vB pars estimated from Len_age internally
  Stock@Ksd <- Stock@Kgrad <- Stock@Linfsd <- Stock@Linfgrad <- Stock@Msd <- Stock@Mgrad <- c(0, 0)
  Stock@K <- Stock@Linf <- Stock@t0 <- c(0, 0)
  Stock@L50 <- Stock@L50_95 <- c(0, 0)

  fleet_output <- lapply(seq_len(replist$nfleets)[replist$IsFishFleet], SS2MOM_fleet, i = i, replist = replist,
                         Stock = Stock, mainyrs = mainyrs, nyears = nyears, MOM = MOM)

  Fleet <- lapply(fleet_output, getElement, "Fleet")
  cpars <- lapply(fleet_output, function(x) c(cpars_bio, x$cpars_fleet))

  return(list(Stock = Stock, Fleet = Fleet, cpars = cpars))
}

SS2MOM_fleet <- function(ff, i, replist, Stock, mainyrs, nyears, MOM) {
  allyears <- nyears + MOM@proyears

  #### Selectivity (Asel2 incorporates time-varying length selectivity, Asel age-based assumed constant)
  #V <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
  #                        replist$ageselex$Factor == "sel_nums", ]
  #V2 <- vapply(1:Stock@maxage, function(x) V[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))
  Asel2 <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
                              replist$ageselex$Factor == "Asel2", ]
  V2 <- vapply(1:Stock@maxage, function(x) Asel2[match(mainyrs, Asel2$Yr), parse(text = paste0("\"", x, "\"")) %>% eval()],
               numeric(length(mainyrs))) %>% t()
  Asel <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
                             replist$ageselex$Factor == "Asel", ]
  V <- vapply(1:Stock@maxage, function(x) Asel[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))

  V2 <- V2 * V
  V2_proj <- matrix(V2[, nyears], nrow(V2), MOM@proyears)

  #### Retention-at-age (not currently used in SampleFleetPars)
  #VR <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
  #                         replist$ageselex$Factor == "sel*ret_nums", ]
  #VR2 <- vapply(1:Stock@maxage, function(x) VR[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))
  #retA <- VR2/V2

  #### Retention-at-length - assumed to be constant over time (need to update for time-varying retention)
  retL <- replist$sizeselex[replist$sizeselex$Fleet == ff & replist$sizeselex$Sex == i &
                              replist$sizeselex$Factor == "Ret" & replist$sizeselex$Yr == max(mainyrs), -c(1:5)] %>% unlist()

  #### Discard mortality
  disc_mort <- replist$sizeselex[replist$sizeselex$Fleet == ff & replist$sizeselex$Sex == i &
                                   replist$sizeselex$Factor == "Mort" & replist$sizeselex$Yr == max(mainyrs), -c(1:5)] %>% unlist() %>%
    as.numeric() %>% unique()
  if(length(disc_mort) > 1) warning("Discard mortality at age not supported.")

  #### Apical F
  FF <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
                           replist$ageselex$Factor == "F", ]
  FF <- FF[match(mainyrs, FF$Yr), ]
  F2 <- vapply(1:Stock@maxage, function(x) FF[, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))
  Find <- apply(F2, 1, max)
  # These F's are slighty lower than in replist$exploitation?

  #### Sex-specific catches: predicted retained catch for fleet ff for stock (sex) i
  ALK <- replist$ALK[, , paste0("Seas: 1 Sub_Seas: 2 Morph: ", i)]
  ALK <- ALK[match(replist$lbins, dimnames(ALK)$Length), match(1:nrow(V2), dimnames(ALK)$TrueAge)]
  retA <- colSums(retL * ALK)

  wt <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
                           replist$ageselex$Factor == "bodywt", ]
  wt2 <- vapply(1:Stock@maxage, function(x) wt[match(mainyrs, wt$Yr), parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))

  meanN <- replist$natage[replist$natage$Sex == i & replist$natage$`Beg/Mid` == "M", ]
  meanN <- meanN[match(mainyrs, meanN$Yr), ]
  meanN2 <- vapply(1:Stock@maxage, function(x) meanN[, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))
  Cat <- colSums(t(meanN2 * Find * wt2) * retA * V2)

  #### Fleet object
  Fleet <- new("Fleet")
  Fleet@Name <- replist$FleetNames[ff]
  Fleet@nyears <- length(mainyrs)
  Fleet@Spat_targ <- rep(1, 2)
  Fleet@EffYears <- 1:nyears
  Fleet@EffLower <- Fleet@EffUpper <- Find
  Fleet@Esd <- Fleet@qinc <- Fleet@qcv <- rep(0, 2)
  Fleet@L5 <- Fleet@LFS <- Fleet@Vmaxlen <- rep(0, 2)
  Fleet@isRel <- "FALSE"
  Fleet@CurrentYr <- max(mainyrs)

  #### cpars
  cpars_fleet <- list()
  cpars_fleet$binWidth <- replist$lbins[2] - replist$lbins[1]
  cpars_fleet$CAL_bins <- replist$lbins %>% c(max(replist$lbins) + cpars_fleet$binWidth)
  cpars_fleet$Fdisc <- rep(mean(disc_mort), MOM@nsim)
  cpars_fleet$V <- cbind(V2, V2_proj) %>% array(c(Stock@maxage, allyears, MOM@nsim)) %>% aperm(c(3, 1, 2))
  #cpars_fleet$retA <- retA %>% array(c(Stock@maxage, allyears, MOM@nsim)) %>% aperm(c(3, 1, 2))
  cpars_fleet$retL <- retL %>% array(c(length(retL), allyears, MOM@nsim)) %>% aperm(c(3, 1, 2))
  cpars_fleet$DR <- rep(0, MOM@nsim)
  cpars_fleet$Find <- Find %>% matrix(MOM@nsim, length(mainyrs), byrow = TRUE)
  cpars_fleet$Data <- new("Data")
  cpars_fleet$Data@Cat <- matrix(Cat, nrow = 1)

  return(list(Fleet = Fleet, cpars_fleet = cpars_fleet))
}

sample_recruitment <- function(Perr_hist, proyears, procsd, AC, seed) {
  set.seed(seed)
  nsim <- nrow(Perr_hist)
  procmu <- -0.5 * procsd^2 * (1 - AC/sqrt(1 - AC^2)) # adjusted log normal mean with AC
  Perr_proj <- rnorm(proyears * nsim, rep(procmu, each = nsim), rep(procsd, each = nsim)) %>%
    matrix(nrow = nsim, ncol = proyears) # Sample recruitment for projection

  # Add autocorrelation to projection recruitment
  Perr_proj[, 1] <- AC * Perr_hist[, ncol(Perr_hist)] + Perr_proj[, 1] * sqrt(1 - AC^2)
  for(y in 2:ncol(Perr_proj)) Perr_proj[, y] <- AC * Perr_proj[, y-1] + Perr_proj[, y] * sqrt(1 - AC^2)

  return(Perr_proj)
}

#Asel <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
#                           replist$ageselex$Factor == "Asel" & replist$ageselex$Yr == max(mainyrs), ]
#VA <- vapply(1:Stock@maxage, function(x) Asel[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))
#Asel2 <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
#                            replist$ageselex$Factor == "Asel2"  & replist$ageselex$Yr == max(mainyrs), ]
#VA2 <- vapply(1:Stock@maxage, function(x) Asel2[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))
#Vprod <- VA * VA2
