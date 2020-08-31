library(multiMSE)
library(DLMtool)
library(dplyr)

# ---- Test model runs under all scenarios ----

# OK = doesn't crash (doesn't mean the model dynamics are working correctly!)

### ****************** ####
# Check dimensions of MPrefs throughout code - not sure if indexing is correct everywhere

# ------ DEBUG SECTION ----

CheckMPs = FALSE; timelimit = 1; Hist=FALSE; ntrials=50; fracD=0.05; CalcBlow=FALSE;
HZN=2; Bfrac=0.5; AnnualMSY=TRUE; silent=FALSE; PPD=FALSE; parallel=FALSE;
save_name=NULL; checks=FALSE; control=NULL

devtools::load_all()

###########################


MPs <- c('AvC', 'curE', 'FMSYref')

Stock1 <- DLMtool::Albacore
Stock2 <- DLMtool::Blue_shark
Stock3 <- DLMtool::Bluefin_tuna

Stock1@Msd <- Stock1@Ksd <- Stock1@Linfsd <- c(0,0)
Stock2@Msd <- Stock2@Ksd <- Stock2@Linfsd <- c(0,0)
Stock3@Msd <- Stock3@Ksd <- Stock3@Linfsd <- c(0,0)

Fleet1 <- DLMtool::IncE_HDom
Fleet2 <- DLMtool::DecE_NDom
Fleet3 <- DLMtool::FlatE_Dom

Stock1@Linf
Stock2@Linf
Stock3@Linf

Fleet1@L5 <- c(45, 50)
Fleet1@LFS <- c(55, 60)
Fleet1@Vmaxlen <- c(0.7, 0.8)
Fleet1@isRel <- FALSE

Fleet2@L5 <- c(15, 25)
Fleet2@LFS <- c(35, 40)
Fleet2@Vmaxlen <- c(0.3, 0.4)
Fleet2@isRel <- FALSE

Fleet3@L5 <- c(88, 95)
Fleet3@LFS <- c(100, 105)
Fleet3@Vmaxlen <- c(1, 1)
Fleet3@isRel <- FALSE

Obs <- DLMtool::Precise_Unbiased
Imp <- DLMtool::Perfect_Imp

nsim <- 10



# ---- 1 Stock, 1 Fleet --- compare DLMtool ----
# Compare with DLMtool runMSE

MPs <- c('AvC', 'curE', 'FMSYref')

Stock2@M <- c(0.1,0.1)

MOM <- new("MOM", Stocks = list(Stock2),
           Fleets = list(
             list(Fleet1)),
           Obs = list(list(Obs)),
           Imps = list(list(Imp)),
           CatchFrac = NULL,
           nsim = nsim)

setup()
MOM@interval <- 1
MMSE <- multiMSE(MOM, MPs=list(list(MPs))) #OK
matplot(t(MMSE@F_FMSY[,1,1,3,]), type='l')

MMSE@F_FMSY[1,1,1,3,]

OM <- new('OM', Stock1, Fleet1, Obs, Imp)
OM@interval <- 1
OM@nsim <- nsim
OM@seed <- MOM@seed
MSE <- runMSE(OM, MPs)

MSE@F_FMSY[1,3,]
MSE@FM[1,3,]

Stocks = list(Stock2)
MPs=list(list(MPs))
Fleets = list(
  list(Fleet1))
Obs = list(list(Obs))
Imps = list(list(Imp))



par(mfrow=c(1,2))
mm <- 1
matplot(t(MMSE@C[,1,1,mm,]), type="l")
matplot(t(MSE@C[,mm,]), type="l")



# ---- Complex MPS ----
# ---- 1 Stock, 2 Fleets ----
CatchFrac <- list(matrix(rep(c(0.3, 0.7), each = nsim), nrow = nsim))
MOM <- new("MOM", Stocks = list(Stock1),
           Fleets = list(
             list(Fleet1, Fleet2)),
           Obs = list(list(Obs,Obs)),
           Imps = list(list(Imp, Imp)),
           CatchFrac = CatchFrac,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) # OK


# CatchFrac
# A list nstock long, of matrices nsim x nfleet representing the fraction of
# current catches of the various fleets to each stock (each matrix is nsim by
# nfleet long and rows sum to 1 for each stock)

# ---- 3 Stocks, 1 Fleets ----

MOM <- new("MOM", Stocks = list(Stock1, Stock2, Stock3),
           Fleets = list(
             list(Fleet1),
             list(Fleet1),
             list(Fleet1)),
           Obs = list(list(Obs),
                      list(Obs),
                      list(Obs)),
           Imps = list(list(Imp),
                       list(Imp),
                       list(Imp)),
           CatchFrac = NULL,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs)


# ---- 3 Stocks, 3 Fleets -----

# CatchFrac
# A list nstock long, of matrices nsim x nfleet representing the fraction of
# current catches of the various fleets to each stock (each matrix is nsim by
# nfleet long and rows sum to 1 for each stock)

CatchFrac <- list(
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 1
  matrix(rep(c(0.1, 0.6, 0.3), each = nsim), nrow = nsim), # stock 2
  matrix(rep(c(0.7, 0.1, 0.2), each = nsim), nrow = nsim) # stock 3
  )

MOM <- new("MOM", Stocks = list(Stock1, Stock2, Stock3),
           Fleets = list(
             list(Fleet1, Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3),
             list(Fleet1, Fleet2, Fleet3)),
           Obs = list(list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs)),
           Imps = list(list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp)),
           CatchFrac = CatchFrac,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) # OK


# ---- 2-sex Stock, 1 Fleet -----
SexPars <- list()
# Stock (Row) spawn from SSB contributed by Stock (Column)
SexPars$SSBfrom <- matrix(c(1,1,0,0),nrow=2)
SexPars$SSBfrom

MOM <- new("MOM", Stocks = list(Stock1, Stock1),
           Fleets = list(
             list(Fleet1),
             list(Fleet1)),
           Obs = list(list(Obs),
                      list(Obs)
                      ),
           Imps = list(list(Imp),
                       list(Imp)
                       ),
           CatchFrac = NULL,
           SexPars=SexPars,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) # OK

# ---- 2-sex Stock, 3 Fleet -----

CatchFrac <- list(
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 1
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim) # stock 2
  )

SexPars <- list()
# Stock (Row) spawn from SSB contributed by Stock (Column)
SexPars$SSBfrom <- matrix(c(1,1,0,0),nrow=2)
SexPars$SSBfrom

MOM <- new("MOM", Stocks = list(Stock1, Stock1),
           Fleets = list(
             list(Fleet1, Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3)
             ),
           Obs = list(list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs)
                      ),
           Imps = list(list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp)
                       ),
           CatchFrac = CatchFrac,
           SexPars=SexPars,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) # OK

# ---- 2-sex + 1 Stock, 3 Fleet ----

CatchFrac <- list(
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 1
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 2
  matrix(rep(c(0.6, 0.1, 0.3), each = nsim), nrow = nsim) # stock 3
)

SexPars <- list()
# Stock (Row) spawn from SSB contributed by Stock (Column)
SexPars$SSBfrom <- matrix(c(1,1,0,
                            0,0,0,
                            0,0,1),nrow=3)
SexPars$SSBfrom

MOM <- new("MOM", Stocks = list(Stock1, Stock1, Stock2),
           Fleets = list(
             list(Fleet1, Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3)
           ),
           Obs = list(list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs)
           ),
           Imps = list(list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp)
           ),
           CatchFrac = CatchFrac,
           SexPars=SexPars,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) #


# ---- Stock-Specific MPS ----
# ---- 3 Stocks, 1 Fleets ----

MPs <- list(c("AvC","DCAC"), # stock 1
            c("FMSYref","curE"), # stock 2
            c("FMSYref50","HDAAC")) # stock 3

MOM <- new("MOM", Stocks = list(Stock1, Stock2, Stock3),
           Fleets = list(
             list(Fleet1),
             list(Fleet1),
             list(Fleet1)),
           Obs = list(list(Obs),
                      list(Obs),
                      list(Obs)),
           Imps = list(list(Imp),
                       list(Imp),
                       list(Imp)),
           CatchFrac = NULL,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) # OK

# ---- 3 Stocks, 3 Fleets -----

# CatchFrac
# A list nstock long, of matrices nsim x nfleet representing the fraction of
# current catches of the various fleets to each stock (each matrix is nsim by
# nfleet long and rows sum to 1 for each stock)

MPs <- list(c("AvC","DCAC"), # stock 1
            c("FMSYref","curE"), # stock 2
            c("FMSYref50","HDAAC")) # stock 3

CatchFrac <- list(
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 1
  matrix(rep(c(0.1, 0.6, 0.3), each = nsim), nrow = nsim), # stock 2
  matrix(rep(c(0.7, 0.1, 0.2), each = nsim), nrow = nsim) # stock 3
)

MOM <- new("MOM", Stocks = list(Stock1, Stock2, Stock3),
           Fleets = list(
             list(Fleet1, Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3),
             list(Fleet1, Fleet2, Fleet3)),
           Obs = list(list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs)),
           Imps = list(list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp)),
           CatchFrac = CatchFrac,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) # OK


# ---- 2-sex Stock, 1 Fleet -----

MPs <- list(c("AvC","DCAC"), # stock 1
            c("NFref","curE") # stock 2
)

SexPars <- list()
# Stock (Row) spawn from SSB contributed by Stock (Column)
SexPars$SSBfrom <- matrix(c(1,1,0,0),nrow=2)
SexPars$SSBfrom

MOM <- new("MOM", Stocks = list(Stock1, Stock2),
           Fleets = list(
             list(Fleet1),
             list(Fleet1)),
           Obs = list(list(Obs),
                      list(Obs)
           ),
           Imps = list(list(Imp),
                       list(Imp)
           ),
           CatchFrac = NULL,
           SexPars=SexPars,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) # OK

# ---- 2-sex Stock, 3 Fleet -----

MPs <- list(c("AvC","DCAC"), # stock 1
            c("NFref","curE") # stock 2
)

CatchFrac <- list(
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 1
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim) # stock 2
)

SexPars <- list()
# Stock (Row) spawn from SSB contributed by Stock (Column)
SexPars$SSBfrom <- matrix(c(1,1,0,0),nrow=2)
SexPars$SSBfrom

MOM <- new("MOM", Stocks = list(Stock1, Stock1),
           Fleets = list(
             list(Fleet1, Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3)
           ),
           Obs = list(list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs)
           ),
           Imps = list(list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp)
           ),
           CatchFrac = CatchFrac,
           SexPars=SexPars,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) # ok


# ---- 2-sex + 1 Stock, 3 Fleet ----

MPs <- list(c("AvC","DCAC"), # stock 1
            c("NFref","curE"),# stock 2
            c('FMSYref', 'FMSYref75') # stock 3
)

CatchFrac <- list(
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 1
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 2
  matrix(rep(c(0.6, 0.1, 0.3), each = nsim), nrow = nsim) # stock 3
)

SexPars <- list()
# Stock (Row) spawn from SSB contributed by Stock (Column)
SexPars$SSBfrom <- matrix(c(1,1,0,
                            0,0,0,
                            0,0,1),nrow=3)
SexPars$SSBfrom

Stock1a <- Stock1
Stock1a@Name <- 'Albacore_Male'
MOM <- new("MOM", Stocks = list(Stock1, Stock1a, Stock2),
           Fleets = list(
             list(Fleet1, Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3)
           ),
           Obs = list(list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs)
           ),
           Imps = list(list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp)
           ),
           CatchFrac = CatchFrac,
           SexPars=SexPars,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) # ok


# ---- Stock & Fleet Specific MPS ----


# ---- 3 Stocks, 3 Fleets -----

# CatchFrac
# A list nstock long, of matrices nsim x nfleet representing the fraction of
# current catches of the various fleets to each stock (each matrix is nsim by
# nfleet long and rows sum to 1 for each stock)

MPs <- list(list(c("AvC","DCAC"), c('curE', 'FMSYref'), c('FMSYref50', "HDAAC")), # stock 1
            list(c("DCAC","curE"), c('AvC', 'HDAAC'), c('FMSYref', "FMSYref50")), # stock 2
            list(c("FMSYref50","curE"), c('HDAAC', 'curE'), c('AvC', "DCAC"))) # stock 3

CatchFrac <- list(
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 1
  matrix(rep(c(0.1, 0.6, 0.3), each = nsim), nrow = nsim), # stock 2
  matrix(rep(c(0.7, 0.1, 0.2), each = nsim), nrow = nsim) # stock 3
)

MOM <- new("MOM", Stocks = list(Stock1, Stock2, Stock3),
           Fleets = list(
             list(Fleet1, Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3),
             list(Fleet1, Fleet2, Fleet3)),
           Obs = list(list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs)),
           Imps = list(list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp)),
           CatchFrac = CatchFrac,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) #  ok

# ---- 2-sex Stock, 3 Fleet -----

MPs <- list(list(c("AvC","DCAC"), c('curE', 'FMSYref'), c('FMSYref50', "HDAAC")), # stock 1
            list(c("DCAC","curE"), c('AvC', 'HDAAC'), c('FMSYref', "FMSYref50")) # stock 2
        )


CatchFrac <- list(
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 1
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim) # stock 2
)

SexPars <- list()
# Stock (Row) spawn from SSB contributed by Stock (Column)
SexPars$SSBfrom <- matrix(c(1,1,0,0),nrow=2)
SexPars$SSBfrom

MOM <- new("MOM", Stocks = list(Stock1, Stock1),
           Fleets = list(
             list(Fleet1, Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3)
           ),
           Obs = list(list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs)
           ),
           Imps = list(list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp)
           ),
           CatchFrac = CatchFrac,
           SexPars=SexPars,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) # ok


# ---- 2-sex + 1 Stock, 3 Fleet ----

MPs <- list(list(c("AvC","DCAC"), c('curE', 'FMSYref'), c('FMSYref50', "HDAAC")), # stock 1
            list(c("DCAC","curE"), c('AvC', 'HDAAC'), c('FMSYref', "FMSYref50")), # stock 2
            list(c("FMSYref50","curE"), c('HDAAC', 'curE'), c('AvC', "DCAC"))) # stock 3

CatchFrac <- list(
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 1
  matrix(rep(c(0.3, 0.3, 0.4), each = nsim), nrow = nsim), # stock 2
  matrix(rep(c(0.6, 0.1, 0.3), each = nsim), nrow = nsim) # stock 3
)

SexPars <- list()
# Stock (Row) spawn from SSB contributed by Stock (Column)
SexPars$SSBfrom <- matrix(c(1,1,0,
                            0,0,0,
                            0,0,1),nrow=3)
SexPars$SSBfrom

Stock1a <- Stock1
Stock1a@Name <- 'Albacore_Male'
MOM <- new("MOM", Stocks = list(Stock1, Stock1a, Stock2),
           Fleets = list(
             list(Fleet1, Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3),
             list(Fleet1,Fleet2, Fleet3)
           ),
           Obs = list(list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs),
                      list(Obs, Obs, Obs)
           ),
           Imps = list(list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp),
                       list(Imp, Imp, Imp)
           ),
           CatchFrac = CatchFrac,
           SexPars=SexPars,
           nsim = nsim)

setup()
MMSE <- multiMSE(MOM, MPs=MPs) # ok

