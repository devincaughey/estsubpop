\name{calc_n_dirichlet}
\alias{calc_n_dirichlet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculate implied sample size of a Dirichlet distribution
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Given a proportion and its sampling variance, \code{calc_n_dirichlet} calculates the implied sample size of the corresponding Dirichlet distribution. That is, if a simplex is distributed Dirichlet(\eqn{p \times N}{p * N}) and one element \eqn{p} equals \code{prop} and has sampling variance \code{prop_variance}, \code{calc_n_dirichlet} calculates the implied value of \eqn{N} using the formula \code{(prop * (1 - prop) / prop_variance) - 1}.
}
\usage{
calc_n_dirichlet(prop, prop_variance)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prop}{
    stipulated proportion
}
  \item{prop_variance}{
    stipulated sampling variance
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The implied ``sample size'' of the Dirichlet distribution.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Devin Caughey
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## proportion of 0.5 with standard error of 0.01
calc_n_dirichlet(0.5, 0.01 ^ 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
