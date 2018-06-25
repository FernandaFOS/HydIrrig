#' Head loss in meters overs the length of pipe
#'
#' Calculate the head loss by fricition in pipes
#'
#' @param D Diameter in meters
#' @param Q Flow rate in cubic meters per second
#' @param L Length of pipe in meters
#' @param v Kinematic viscosity of fluiyd in square meters per second.
#' @param E Roughness coefficient in meters
#' @param x1 Initial parameter of f for Newthon-Raphson
#' @param g Gravitational acceleration \code{g=9.81} meters per square second.
#'
#' @return  regime flow laminar or turbulent, hf Head loss in meters, iterações iterations end tempo time
#' @export
#' @examples
#' hf(D = 0.025, Q = 0.001,  L = 100)
#' hf(D = 0.080, Q = 0.0001, L = 500)

#Function hf
hf <- function (D,
                Q,
                L,
                E = 1e-4,
                v = 1.01e-6,
                x1 = 4,
                g = 9.81) {
  #time
  ptm <- proc.time()

  i = 0
  A = ((pi * (D ^ 2)) / 4)
  V = Q / A
  Re = V * D / v

  if (Re < 2000)
    #regime laminar
  {
    regime = "regime laminar"
    fi = 64 / Re
    hf = fi * (L / D) * ((V ^ 2) / (2 * g))
    #    return(hf)
  }
  else
  {
    regime <-  "regime turbulento"
    dif <-  1
    x2 <-  x1 + 1
    while (dif >= 1e-12) {
      i <- i + 1
      w <-  (E / (3.7 * D)) + ((2.51 * x1) / Re)
      h <-  (2.18 / (((E * Re) / (3.7 * D)) + (2.51 * x1)))

      x2 <-  x1 - (((x1 + (2 * log10(
        w
      ))) / (1 + h)))
      dif <-  abs(x2 - x1)
      x1  <-  x2
    }
    fi = 1 / x1 ^ 2
    hf = fi * (L / D) * ((V ^ 2) / (2 * g))

  }
  return(list(
    "regime" = regime,
    "hf" = hf,
    "iteração" = i,
    tempo = (proc.time() - ptm)[3]
  ))
}
