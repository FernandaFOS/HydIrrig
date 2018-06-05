#' Established to set the diameter lateral line and pressure required at the start a subunit
#'
#' Set the diameter lateral line and pressure required at the start  a subunit
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
#' @param qvr Emitter flow variation
#'
#' @return D diameter lateral line, pressure head at the end, iter number of iterations, tempo time
#'
#' @export
#' @examples
#' subunihd(DLL = 0.025,K = 1.053e-6,x = 0.5,DLD = 0.04,
#' SeLL = 4, SeLD = 1, nLL = 100, ne = 10, dec = 0.00,
#' qreq = 3.33333e-6, imax = 100,toler = 1e-6, qvr = 10)

#Function subunihd

subunihd <-
  function( DLL,
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
            toler = 1e-6,
            qvr
  ){

    # Time
    ptm <- proc.time()
    i = 0
    a = 0
    c = 0

    repeat{
      resb.subunih <- subunih(
        DLL=DLL,
        K = K,
        x = x,
        DLD = DLL,
        SeLL = SeLL,
        SeLD = SeLD,
        nLL = nLL,
        ne = ne,
        dec = dec,
        qreq = qreq,
        imax = 100,
        toler = 1e-10,
        HfimLDa = 100,
        HfimLDb = 0
      )

      #print(resb.llh$iter)
      b <- resb.subunih$iter
      a = a + b
      #print(a)


      resb.subunid <- subunid(
        HfimLD = resb.subunih$Hfim,
        DLL = DLL,
        K = K,
        x = x,
        SeLL = SeLL,
        SeLD = SeLD,
        nLL = nLL,
        ne = ne,
        qvr = qvr,
        dec = dec,
        imax = 100,
        toler = 1e-6,
        DLDa = 1,
        DLDb = 0
      )

      #print(resb.subunid$iter)
      d <- resb.subunid$iter
      c = c + d
      #print(c)


      AD <- abs(DLL - resb.subunid$D)
      #print(AD)
      DLL <- resb.subunid$D
      #print(DLL)

      # Stop maximum loterance
      if( AD < toler){
        break
      }

      i = i + 1

      # Stop number maximum iterations
      if (i > imax) {
        break
      }
    }
    return(list(
      D = DLL,
      Hfim = resb.subunih$Hfim,
      iter = a + c,
      tempo = (proc.time() - ptm)[3]))
  }
