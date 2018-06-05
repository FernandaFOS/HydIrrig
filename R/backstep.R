#' Design a lateral line
#'
#' Calculate the pressure head and flow rate profile along a lateral line
#'
#' @param Hfim Pressure at the end of the line in mca
#' @param K Emission coefficient
#' @param x Emission exponent
#' @param D Diameter in meters
#' @param Se Emitter spacing in meters
#' @param ne Number of emitters
#' @param dec Slope on the lateral line
#'
#' @return Hini pressure head at inlet of lateral, Qini flow rate at inlet of the lateral, H pressure head in the emitter,  q flow rate in the emitter,
#' Q flow rate in lateral line, hf head loss
#'
#' @export
#' @examples
#' ll(Hfim = 10, K = 1.053e-6, x = 0.5, D = 0.025, Se = 1,
#'  ne = 10, dec = 0.00)
#' ll(Hfim = 10, K = 1.053e-6, x = 0.5, D = 0.025, Se = 1,
#' ne = 10, dec = 0.01)
#' ll(Hfim = 10, K = 1.053e-6, x = 0.5, D = 0.025, Se = 1,
#'  ne = 10, dec = -0.01)
#' ll(Hfim = 10, K = 1.053e-6, x = 0.5, D = 0.025, Se = 1,
#'  ne = 100, dec = 0.00)
#' ll(Hfim = 10, K = 1.053e-6, x = 0.5, D = 0.025, Se = 1,
#'  ne = 100, dec = 0.01)
#' ll(Hfim = 10, K = 1.053e-6, x = 0.5, D = 0.025, Se = 1,
#' ne = 100, dec = -0.01)

#Function lateral line
ll <- function (Hfim, K, x, D, Se, ne, dec) {

  #time
  ptm <- proc.time()

  #Vectors
  H <- c()
  q <- c()
  Q <- c()
  Hf <- c()


  # Last emitter / i=ne
  H[ne] <- Hfim
  q[ne] <- K * H[ne] ^ x #
  #Q[ne] <- q[ne]
  Q[ne] <- 0


  #Loop i=1
  for (i in ne:2)  {
    Q[i - 1] <- Q[i] + q[i]
    Hf[i - 1] <- hf(D = D, Q = Q[i - 1], L = Se)$hf
    H[i - 1] <- H[i] + (dec*Se) + Hf[i - 1]

    q[i - 1] <- K * H[i - 1] ^ x

}

  #First section
  Q <- c(Q[1] + q[1], Q[1:(ne - 1)])
  Hf <- c(hf(D = D, Q = Q[1], L = Se)$hf, Hf)
  Hini <- H[1] + (dec*Se)+ Hf[1]

  return (list(
    "Hini" = Hini,
    "Qini" = sum(q),
    "H" = H,
    "q" = q,
    "Q" = Q,
    "Hf" = Hf,
    tempo = (proc.time() - ptm)[3]
  ))
}




