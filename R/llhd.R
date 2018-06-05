#' Established to set the diameter and pressure required at the start of a lateral line
#'
#' Determine a linha lateral e o diâmetro de pressão necessários no início de uma linha lateral
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
#' @param qvr Emitter flow variation
#'
#' @return D diameter, Hfim pressure head at the end of the lateral line, iter number of iterations, tempo time
#' @export
#'
#' @examples
#' llhd( D = 0.25, K = 1.053e-6, x = 0.5, Se = 1, ne = 10,
#'  dec = 0.00, qreq = 3.33333e-6, imax = 100, toler = 1e-10, qvr = 10)
#' llhd( D = 0.25, K = 1.053e-6, x = 0.5, Se = 1, ne = 10,
#'  dec = 0.04, qreq = 3.33333e-6, imax = 100, toler = 1e-10, qvr = 10)
#' llhd( D = 0.25, K = 1.053e-6, x = 0.5, Se = 1, ne = 10,
#'  dec = 0.04, qreq = 3.33333e-6, imax = 100, toler = 1e-10, qvr = 10)

#Function llhd
  llhd <-
    function( D,
              K,
              x,
              Se,
              ne,
              dec,
              qreq,
              imax = 100,
              toler = 1e-6,
              qvr
    ){
      # Time
      ptm <- proc.time()
      i = 0
      a = 0
      c = 0

      repeat{
        resb.llh <- llh(
          D = D,
          K = K,
          x = x,
          Se = Se,
          ne = ne,
          dec = dec,
          qreq = qreq,
          imax = 100,
          toler = 1e-10,
          Hfima = 100,
          Hfimb = 0
        )
        b <- resb.llh$iter
        a = a + b

        resb.lld <- lld(
          Hfim = resb.llh$Hfim,
          K = K,
          x = x,
          Se = Se,
          ne = ne,
          dec = dec,
          qvr = qvr,
          imax = 100,
          toler = 1e-6,
          DLLa = 1,
          DLLb = 0
        )
        d <- resb.llh$iter
        c = c + d

        AD <- abs ( D - resb.lld$D )
        #print(AD)
        D <- resb.lld$D
        #print(D)

        # Stop maximum loterance
        if( AD < toler){
          break
        }


        i = i + 1


        if (i > imax) {
          break
        }
      }
      return(list(
        D = D,
        Hfim = resb.llh$Hfim,
        iter = a + c,
        tempo = (proc.time() - ptm)[3]))
    }
