#' Define an irrigation subunit with lateral line and the manifold
#'
#' calculate of the pressure head and flow rate profile a subunit
#'
#' @param HfimLD Pressure at the end of the manifold in mca
#' @param K Emission coefficient
#' @param x Emission exponent
#' @param DLL Diameter lateral line in meters
#' @param DLD Diameter manifold in meters
#' @param SeLD Lateral line spacing in meters
#' @param SeLL Emitter spacing in meters
#' @param neLL Number of lateral lines in meters
#' @param ne Number of emitters in meters
#' @param dec Slope on the lateral line
#'
#' @return h Pressure profile and q Flow rate Profile
#'
#' @export
#'
#' @examples
#' subuni( HfimLD = 15, K = 1.053e-6, x = 0.5, DLL = 0.025, DLD = 0.040, SeLD = 4, SeLL = 1, nLL = 10, ne = 100, dec = 0.0)

#Function subuni
subuni <-
  function (
    HfimLD,
    K,
    x,
    DLL,
    DLD,
    SeLD,
    SeLL,
    nLL,
    ne,
    dec) {

  # Time
  ptm <- proc.time()

  # Calculate the coefficients of the lateral line
  cc <- coefic(
    K = K,
    x = x,
    D = DLL,
    Se = SeLL,
    ne = ne,
    dec = dec
  )

  # Calculate manifold
  LD <-
    ll(
      Hfim = HfimLD,
      K = cc$a,
      x = cc$b,
      D = DLD,
      Se = SeLL,
      ne = nLL,
      dec = dec
    )

  # Calculate lines lateral
  LL <-
    sapply(1:nLL, function(y) {
      ll(
        Hfim = (cc$c * LD$H[y]),
        K = K,
        x = x,
        D = DLL,
        Se = SeLL,
        n = ne,
        dec = dec
      )
    })
  qq <- matrix(unlist(LL[4, ]), ncol = nLL)
  hh <- matrix(unlist(LL[3, ]), ncol = nLL)

  # Qini <- unlist(LL[2, ])
  # Qini
  # LD$q
  # LD$q - Qini
  # (LD$q - Qini) / LD$q * 100
  #
  # Hini <- unlist(LL[1, ])
  # Hini
  # LD$H
  # LD$H - Hini
  # (LD$H - Hini) / LD$H * 100

  return(list(
    "q" = qq,
    "h" = hh,
    tempo = (proc.time() - ptm)[3]
  ))
}
