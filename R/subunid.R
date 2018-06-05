#' Establish the diameter of a lateral line as a function of flow variation the a subunit.
#'
#' Set the diameter of a lateral line
#'
#' @param HfimLD Pressure at the end of the manifold in mca
#' @param DLL Diameter lateral line in meters
#' @param K Emission coefficient
#' @param x Emission exponent
#' @param SeLL Emitter spacing in meters
#' @param SeLD Lateral line spacing in meters
#' @param nLL Number of lateral lines in meters
#' @param ne Number of emitters in meters
#' @param dec Slope on the lateral line
#' @param qvr Emitter flow variation
#' @param imax Maximum number of iteration
#' @param toler Maximum tolerance
#' @param DLDa Maximum diameter in meters
#' @param DLDb Minimum diameter in meters
#'
#' @return D diameter lateral line,
#'  qv emitter flow variation,
#'  iter number iterations, tempo time
#'
#' @export
#' @examples
#' subunid(HfimLD = 10, DLL = 0.025, K = 1.053e-6, x = 0.5,
#' SeLL = 1, SeLD = 4, nLL = 10, ne = 100, qvr = 10, dec = 0.00,
#' imax = 100, toler = 1e-6, DLDa = 1, DLDb = 0)
#' subunid(HfimLD = 10, DLL = 0.025, K = 1.053e-6, x = 0.5,
#' SeLL = 1, SeLD = 4, nLL = 10, ne = 100, qvr = 10, dec = -0.01,
#' imax = 100, toler = 1e-6, DLDa = 1, DLDb = 0)
#' subunid(HfimLD = 20, DLL = 0.025, K = 1.053e-6, x = 0.5,
#' SeLL = 1, SeLD = 4, nLL = 10, ne = 100, qvr = 10, dec = -0.01,
#' imax = 100, toler = 1e-6, DLDa = 1, DLDb = 0)


# Function subunid
subunid <-
  function(HfimLD,
           DLL,
           K,
           x,
           SeLL,
           SeLD,
           nLL,
           ne,
           dec,
           qvr = 10,
           imax = 100,
           toler = 1e-6,
           DLDa = 1,
           DLDb = 0) {

    # Time
    ptm <- proc.time()

    # Initial step
    res.a <-
      subuni(
        HfimLD,
        DLL = DLL,
        K = K,
        x = x,
        DLD = DLDa,
        SeLL = SeLL,
        SeLD = SeLD,
        nLL = nLL,
        ne = ne,
        dec =dec
      )
    fDLDa <-
      (qvr - (max(res.a$q) - min(res.a$q)) / max(res.a$q) * 100) #calculate f(a)

    #parar se não confinar a raiz, fDLLa tem que ter valor positivo
    if (fDLDa < 0) {
      stop(
        paste(
          "Não foi possível resolver esta condição:",
          "qv inicial =",
          qvr - fDLDa ,
          "qv desejado =",
          qvr
        ),
        call. = FALSE
      )

    }

    i = 0

    # Bisection method
    repeat {
      # x = (a+b)/2
      DLDx <- (DLDa + DLDb) / 2
      # Calculate f(x)
      res.x <-
        subuni(
          HfimLD,
          DLL= DLL,
          K = K,
          x = x,
          DLD = DLDx,
          SeLL = SeLL,
          SeLD = SeLD,
          nLL = nLL,
          ne = ne,
          dec = dec
        )
      fDLDx <-
        (qvr - (max(res.x$q) - min(res.x$q)) / max(res.x$q) * 100) #f(x)

      # #mostra
      # print(
      #   paste(
      #     "i=",
      #     i,
      #     "DLLa=",
      #     DLLa,
      #     "DLLb=",
      #     DLLb,
      #     "DLLx=",
      #     DLLx,
      #     "fDDLa=",
      #     fDLLa,
      #     "fDDLx=",
      #     fDLLx,
      #     "qv=",
      #     qvr - fDLLx
      #   )
      # )


      # Determine the side
      if (fDLDa * fDLDx < 0) {
        DLDb <- DLDx
        fDLDb <- fDLDx
      } else{
        DLDa <- DLDx
        fDLDa <- fDLDx
      }

      i = i + 1 #Increase

      # Stop maximum loterance
      if (abs(fDLDx) < toler) {
        break
      }

      # Stop number maximum iterations
      if (i > imax) {
        break
      }
    }

    return (list(
      D = DLDx,
      qv = (qvr - fDLDx),
      iter = i,
      tempo = (proc.time() - ptm)[3]
    ))
  }
