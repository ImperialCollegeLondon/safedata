\name{getSAFE}
\alias{getSAFE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getSAFE(record_ids, dir = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{record_ids}{
%%     ~~Describe \code{record_ids} here~~
}
  \item{dir}{
%%     ~~Describe \code{dir} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (record_ids, dir = ".") 
{
    api <- "https://sandbox.zenodo.org/api/records/\%s"
    for (rec in record_ids) {
        record <- fromJSON(url(sprintf(api, record_id)))
        bucket <- fromJSON(record$links$bucket)
        fname <- file.path(dir, basename(bucket$contents$links$self))
        download.file(bucket$contents$links$self, destfile = fname)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
