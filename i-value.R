# Classify a test statistic relative to its descriptive reference and calculate
# its null smallness probability (the proposed i-value).
#
# type: "F", "t" (two-sided; uses |t|), or "chisq" (Pearson chi-square).
# df1 is numerator df for F and the degrees of freedom for t/chi-square.
# df2 is required only for F. This helper does not test model assumptions.
check_statistic <- function(statistic, type = c("F", "t", "chisq"),
                            df1, df2 = NULL, alpha_si = 0.05) {
  type <- match.arg(type)

  scalar_finite <- function(x) is.numeric(x) && length(x) == 1L &&
    !is.na(x) && is.finite(x)
  if (!scalar_finite(statistic)) {
    stop("Statistic must be one finite number.", call. = FALSE)
  }
  if (!scalar_finite(df1) || df1 <= 0) {
    stop("Degrees of freedom must be one finite positive number.",
         call. = FALSE)
  }
  if (!scalar_finite(alpha_si) || alpha_si <= 0 || alpha_si >= 1) {
    stop("alpha_si must be greater than 0 and less than 1.", call. = FALSE)
  }

  if (type == "F") {
    if (!is.null(df2) && (!scalar_finite(df2) || df2 <= 0)) {
      stop("Denominator df must be one finite positive number.",
           call. = FALSE)
    }
    if (is.null(df2)) stop("F requires numerator and denominator df.",
                            call. = FALSE)
    if (statistic < 0) stop("F statistic cannot be negative.", call. = FALSE)
    reference <- 1
    value <- statistic
    i_value <- stats::pf(value, df1 = df1, df2 = df2)
  } else if (type == "t") {
    reference <- 1
    value <- abs(statistic)
    i_value <- 2 * stats::pt(value, df = df1) - 1
  } else {
    if (statistic < 0) {
      stop("Chi-square statistic cannot be negative.", call. = FALSE)
    }
    reference <- df1
    value <- statistic
    i_value <- stats::pchisq(value, df = df1)
  }

  below_reference <- value < reference
  classification <- if (!below_reference) {
    "report_full"
  } else if (i_value < alpha_si) {
    "significantly_insignificant"
  } else {
    "below_reference"
  }

  list(
    type = type,
    statistic = statistic,
    magnitude = value,
    reference = reference,
    df1 = df1,
    df2 = if (type == "F") df2 else NULL,
    i_value = i_value,
    alpha_si = alpha_si,
    classification = classification
  )
}
