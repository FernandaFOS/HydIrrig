#' Adjust the emitter flow variation to change the diameter
#'
#' Set the diameter of a lateral line
#'
#' @param Hfim Pressure at the end of the line in mca
#' @param K Emission coefficient
#' @param x Emission exponent
#' @param Se Emitter spacing in meters
#' @param ne Number of emitters
#' @param dec Slope on the lateral line
#' @param qvr Emitter flow variation
#' @param imax Maximum number of iteration
#' @param toler Maximum tolerance
#' @param DLLa Maximum diameter in meters
#' @param DLLb Minimum diameter in meters
#'
#' @return D diameter, qv emitter flow variation, iter number iterations, tempo time
#' @export
#' @examples
#' lld(Hfim = 10, K = 1.053e-6, x = 0.5, Se = 1, ne = 100,
#' dec =  0.00, qvr = 10, imax = 100, toler = 1e-6, DLLa = 1, DLLb = 0)
#' lld(Hfim = 10, K = 1.053e-6, x = 0.5, Se = 1, ne = 100,
#' dec =  0.01, qvr = 10, imax = 100, toler = 1e-6, DLLa = 1, DLLb = 0)
#' lld(Hfim = 10, K = 1.053e-6, x = 0.5, Se = 1, ne = 100,
#' dec = -0.01, qvr = 10, imax = 100, toler = 1e-6, DLLa = 1, DLLb = 0)

#Function lld
lld <-
  function(Hfim,
           K,
           x,
           Se,
           ne,
           dec,
           qvr,
           imax = 100,
           toler = 1e-6,
           DLLa = 1,
           DLLb = 0) {

    # Time
    ptm <- proc.time()

    # Initial step
    res.a <-
      ll(
        Hfim = Hfim,
        K = K,
        x = x,
        D = DLLa,
        Se = Se,
        ne = ne,
        dec = dec
      )
    fDLLa <-
      (qvr - (max(res.a$q) - min(res.a$q)) / max(res.a$q) * 100) #calculate f(a)


    if (fDLLa < 0) {
      stop(
        paste(
          "Não é possível resolver está condição:",
          "qv inicial =",
          qvr - fDLLa ,
          "qv desejado =",
          qvr
        ),
        call. = FALSE
      )

    }

    i = 0

    # Bisection method
    repeat {
      #x=(a+b)/2
      DLLx <- (DLLa + DLLb) / 2
      # Calculate f(x)
      res.x <-
        ll(
          Hfim = Hfim,
          K = K,
          x = x,
          D = DLLx,
          Se = Se,
          ne = ne,
          dec = dec
        )
      fDLLx <-
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
      if (fDLLa * fDLLx < 0) {
        DLLb <- DLLx
        fDLLb <- fDLLx
      } else{
        DLLa <- DLLx
        fDLLa <- fDLLx
      }

      i = i + 1 # Increase

      # Stop maximum loterance
      if (abs(fDLLx) < toler) {
        break
      }

      # Stop number maximum iterations
      if (i > imax) {
        break
      }
    }

    return (list(
      D = DLLx,
      qv = (qvr - fDLLx),
      iter = i,
      tempo = (proc.time() - ptm)[3]
    ))
  }
