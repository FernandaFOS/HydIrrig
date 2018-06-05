#'Establish the pressure at the start of a lateral line in a subunit in function flow rater required of emissors
#'
#'Establish the pressure at the start of a lateral line in a subunit
#'
#'
#' @param DLL Diameter lateral line in meters
#' @param K Emission coefficient
#' @param x Emission exponent
#' @param DLD Diameter manifold in meters
#' @param SeLL Emitter spacing in meters
#' @param SeLD Lateral line spacing in meters
#' @param nLL Number of lateral lines in meters
#' @param ne Number of emitters in meters
#' @param dec Slope on the lateral line
#' @param qreq Flow rate required at the emitters in cubic meters per second
#' @param imax Maximum number of iteration
#' @param toler Maximum tolerance
#' @param HfimLDa Maximum pressure head at the end of the manifold in meters
#' @param HfimLDb Minimum pressure head at the end of the manifold in meters

#'@return HfimLD pressure head at the end of the manifold, qsistema system flow rate, iter number iterations, tempo time
#'
#'@export
#'
#'@examples
#'subunih(DLL = 0.025, K = 1.053e-6, x = 0.5, DLD = 0.04,
#' SeLL = 1, SeLD = 4, nLL = 10, ne = 100, dec = 0.0,
#'qreq = 3.333333e-6, imax = 100, toler = 1e-10, HfimLDa = 100, HfimLDb = 0)
#'subunih(DLL=0.025, K = 1.053e-6, x = 0.5, DLD = 0.04,
#' SeLL = 1, SeLD = 4, nLL = 10, ne = 100, dec = 0.01,
#' qreq = 3.333333e-6, imax = 100, toler = 1e-10, HfimLDa = 100, HfimLDb = 0)
#'
#Function subunih
subunih <-
  function(DLL,
           K,
           x,
           DLD,
           SeLL,
           SeLD,
           nLL,
           ne,
           dec,
           qreq,
           imax = 100,
           toler = 1e-10,
           HfimLDa = 100,
           HfimLDb = 0) {

    # Time
    ptm <- proc.time()

    # Initial step
    res.a <-
      subuni(
        HfimLD = HfimLDa,
        K = K,
        x = x,
        DLL = DLL,
        DLD = DLD,
        SeLL = SeLL,
        SeLD = SeLD,
        nLL = nLL,
        ne = ne,
        dec = dec
      )
    fHfimLDa <- (qreq - mean(res.a$q))  #calculate f(a)

    #parar se não confinar a raiz, fDLLa tem que ter valor positivo
    if (fHfimLDa > 0) {
      stop(
        paste(
          "Não é possível resolver está condição:",
          "vazao inicial =",
          qreq - fHfimLDa ,
          "vazao desejado =",
          qreq
        ),
        call. = FALSE
      )

    }

    i = 0

    # Bisection method
    repeat {
      #x = (a+b)/2
      HfimLDx <- (HfimLDa + HfimLDb) / 2
      #calculate f(x)
      res.x <-
        subuni(
          HfimLD = HfimLDx,
          K = K,
          x = x,
          DLL = DLL,
          DLD = DLD,
          SeLL = SeLL,
          SeLD = SeLD,
          nLL = nLL,
          ne = ne,
          dec = dec
        )
      fHfimLDx <-
        (qreq - mean(res.x$q)) #f(x)


      # Determine the side
      if (fHfimLDa * fHfimLDx < 0) {
        HfimLDb<- HfimLDx
        fHfimLDb <- fHfimLDx
      } else{
        HfimLDa <- HfimLDx
        fHfimLDa <- fHfimLDx
      }

      i = i + 1 #Increase

      # Stop maximum loterance
      if (abs(fHfimLDx) < toler) {
        break
      }

      # Stop number maximum iterations
      if (i > imax) {
        break
      }
    }

    return (list(
      Hfim = HfimLDx,
      qsis = abs(qreq - fHfimLDx),
      iter = i,
      tempo = (proc.time() - ptm)[3]
    ))
  }
