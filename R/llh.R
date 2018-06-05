#' Establish the pressure at the start of a lateral line
#'
#' Determine the pressure at the start of the lateral line
#'
#' @param D Diameter in meters
#' @param K Emission coefficient
#' @param x Emission exponent
#' @param Se Emitter spacing in meters
#' @param ne Number of emitters
#' @param dec Slope on the lateral line
#' @param qreq Flow rate required at the emitters in cubic meters per second
#' @param imax Maximum number of iteration
#' @param toler Maximum tolerance
#' @param Hfima Maximum pressure head at the end of the lateral line in meters
#' @param Hfimb Minimum pressure head at the end of the lateral line in meters
#'
#' @return Hfim pressure head at the end of the lateral line, qsistema system flow rate, iter number iterations,tempo time
#'
#' @export
#'
#' @examples
#' llh( D = 0.025, K = 1.053e-6, x = 0.5, Se = 1, ne = 10,
#' dec = 0.0, qreq = 3.333333e-6, imax = 100, toler = 1e-10,
#' Hfima = 30, Hfimb = 0)
#' llh( D = 0.025, K = 1.053e-6, x = 0.5, Se = 1, ne = 10,
#' dec = 0.04, qreq = 3.333333e-6, imax = 100, toler = 1e-10,
#' Hfima = 30, Hfimb = 0)
#' llh( D = 0.025, K = 1.053e-6, x = 0.5, Se = 1, ne = 10,
#'  dec = -0.04, qreq = 3.333333e-6, imax = 100, toler = 1e-10,
#'  Hfima = 30, Hfimb = 0)

#Function llh
llh <-
  function(D,
           K,
           x,
           Se,
           ne,
           dec,
           qreq,
           imax = 100,
           toler = 1e-10,
           Hfima = 100,
           Hfimb = 0) {

    # Time
    ptm <- proc.time()

    #Initial step
    res.a <-
      ll(
        Hfim = Hfima,
        K = K,
        x = x,
        D = D,
        Se = Se,
        ne = ne,
        dec = dec
      )

    fHfima <- (qreq - mean(res.a$q))  #calcule f(a)


    if (fHfima > 0) {
      stop(
        paste(
          "Não foi possível resolver esta condição:",
          "vazão inicial =",
          qreq - fHfima ,
          "vazão desejado =",
          qreq
        ),
        call. = FALSE
      )

    }

    i = 0

    # Bisection method
    repeat {
      # x = (a+b)/2
      Hfimx <- (Hfima + Hfimb) / 2

      # Calculate f(x)
      res.x <-
        ll(
          Hfim = Hfimx,
          K = K,
          x = x,
          D = D,
          Se = Se,
          ne = ne,
          dec = dec
        )
      fHfimx <-
        (qreq - mean(res.x$q)) #f(x)
      #print(fHfimx)

      # #mostra
      #print(
      #paste(
      #"i=",
      #i,
      #"Hfima=",
      #Hfima,
      #"Hfimb=",
      #Hfimb,
      #"Hfimx=",
      #Hfimx,
      #"fHfima=",
      #fHfima,
      #"fHfimx=",
      #fHfimx,
      #"vazao=",
      #qreq - fHfimx
      #)
      #)


      # Determine the side
      if (fHfima * fHfimx < 0) {
        Hfimb<- Hfimx
        fHfimb <- fHfimx
      } else{
        Hfima <- Hfimx
        fHfima <- fHfimx
      }

      i = i + 1 # Increase

      # Stop maximum loterance
      if (abs(fHfimx) < toler) {
        break
      }

      # Stop number maximum iterations
      if (i > imax) {
        break
      }
    }

    return (list(
      Hfim = Hfimx,
      qsistema = abs(qreq - fHfimx),
      iter = i,
      tempo = (proc.time() - ptm)[3]
    ))
  }
