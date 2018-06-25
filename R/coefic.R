#' Coefficient
#'
#' Calculate the coefficients of lateral line
#'
#' @param K Emission coefficient
#' @param x Emission exponent
#' @param D Diameter in meters
#' @param Se Emitter spacing in meters
#' @param ne Number of emitters
#' @param dec Slope on the lateral line
#' @param HL presures
#'
#' @return coefficient a, b e c
#'
#' @export
#'
#' @examples
#' coefic( K = 1.053e-6, x = 0.5, D = 0.015, Se = 1, ne = 100, dec = 0.00, HL = c(1, 100, 0.5))
#' coefic( K = 1.053e-6, x = 0.5, D = 0.015, Se = 1, ne = 100, dec = -0.01, HL = c(1, 100, 0.5))

#Function coefficcients lateral line
coefic <- function (K, x, D, Se, ne, dec, HL ) {

  #time
  ptm <- proc.time()

  HL1 = HL - 10
  HL2 = HL + 10
  HL3 = 0.5

  if (HL1<= 0){
    HL1 <- 1
  }

  HL = c(HL1, HL2, HL3)


  # Defines the simulations
  Hfim <- seq(HL[1], HL[2], HL[3])

  #Simularion vectors
  HQ <-
    sapply(Hfim, function(y) {
      ll(
        Hfim = y,
        K = K,
        x = x,
        D = D,
        Se = Se,
        ne = ne,
        dec = dec*Se
      )
    })
  Hini <- unlist(HQ[1, ])
  Qini <- unlist(HQ[2, ])

  options(warn = -1) #evita warnings

  # Power equation: Qini=a*Hini^b
  eq1 <- lm(log10(Qini) ~ log10(Hini))
  a = 10 ^ summary(eq1)$coefficients[1]
  b = summary(eq1)$coefficients[2]

  # Linear equation: Hfim=c*Hini
  eq2 <- lm(Hfim ~ 0 + Hini)
  c = summary(eq2)$coefficients[1]

  options(warn = 0) #reativa warnings

  # Plot graphics
  par(mfrow = c(1, 2))
  plot(Qini ~ Hini)
  curve(a * x ^ b, add = TRUE, col = "red")
  plot(Hfim ~ Hini)
  curve(c * x, add = TRUE, col = "red")
  return(list(
    "a" = a,
    "b" = b,
    "c" = c,
    tempo = (proc.time() - ptm)[3]
  ))
}
