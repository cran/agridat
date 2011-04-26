\name{agridat}
\alias{agridat}
\docType{package}
\title{Datasets from agricultural experiments}
\description{
  This package contains datasets from published papers and books
  relating to agriculture, especially to field experiments.
}
\details{
\tabular{lllllll}{
name                             \tab gen \tab loc\tab reps\tab years \tab trt \tab other \cr
\link{allcroft.lodging}          \tab 32  \tab 7  \tab     \tab    \tab     \tab nonnormal\cr
\link{australia.soybean}         \tab 58  \tab 4  \tab     \tab 2  \tab     \tab 6 traits \cr
\link{batchelor.apple}           \tab     \tab    \tab     \tab    \tab     \tab xy, uni \cr 
\link{batchelor.lemon}           \tab     \tab    \tab     \tab    \tab     \tab xy, uni \cr 
\link{batchelor.navel1}          \tab     \tab    \tab     \tab    \tab     \tab xy, uni \cr 
\link{batchelor.navel2}          \tab     \tab    \tab     \tab    \tab     \tab xy, uni \cr 
\link{batchelor.valencia}        \tab     \tab    \tab     \tab    \tab     \tab xy, uni \cr 
\link{batchelor.walnut}          \tab     \tab    \tab     \tab    \tab     \tab xy, uni \cr 
\link{besag.elbatan}             \tab 50  \tab    \tab 3   \tab    \tab     \tab       \cr 
\link{besag.met}                 \tab 64  \tab 6  \tab 3   \tab    \tab     \tab xy, incblock\cr
\link{cochran.bib}               \tab 13  \tab 13 \tab     \tab    \tab     \tab BIB\cr  
\link{corsten.interaction}       \tab 20  \tab 7  \tab     \tab    \tab     \tab \cr  
\link{crowder.germination}       \tab 2   \tab    \tab 21  \tab    \tab 2   \tab \cr  
\link{denis.missing}             \tab 5   \tab 26 \tab     \tab    \tab     \tab \cr  
\link{durban.competition}        \tab 36  \tab    \tab 3   \tab    \tab     \tab xy, competition \cr  
\link{durban.rowcol}             \tab 272 \tab    \tab 2   \tab    \tab     \tab xy \cr  
\link{durban.splitplot}          \tab 70  \tab    \tab 4   \tab    \tab 2   \tab xy \cr  
\link{federer.tobacco}           \tab     \tab    \tab 8   \tab    \tab 7   \tab xy \cr  
\link{gilmour.serpentine}        \tab 108 \tab    \tab 3   \tab    \tab     \tab xy, serpentine\cr  
\link{gomez.fractionalfactorial} \tab     \tab    \tab 2   \tab    \tab 6   \tab xy \cr  
\link{gomez.groupsplit}          \tab 45  \tab    \tab 3   \tab    \tab 2   \tab xy, 3 gen groups \cr  
\link{gomez.multilocsplitplot}   \tab 2   \tab 3  \tab 3   \tab    \tab     \tab nitro \cr  
\link{gomez.splitsplit}          \tab 3   \tab    \tab 3   \tab    \tab     \tab xy, nitro, mgmt \cr  
\link{gomez.stripplot}           \tab 6   \tab    \tab 3   \tab    \tab     \tab xy, nitro \cr  
\link{gomez.stripsplitplot}      \tab 6   \tab    \tab 3   \tab    \tab     \tab xy, nitro\cr  
\link{gomez.uniformity}          \tab     \tab    \tab     \tab    \tab     \tab xy, uni \cr  
\link{hughes.grapes}             \tab     \tab    \tab 3   \tab    \tab 6   \tab binomial \cr  
\link{kempton.rowcol}            \tab 35  \tab    \tab 2   \tab    \tab     \tab uni, row-col \cr
\link{kempton.uniformity}        \tab     \tab    \tab     \tab    \tab     \tab uni \cr
\link{mead.strawberry}           \tab 8   \tab    \tab 4   \tab    \tab     \tab \cr
\link{mercer.wheat}              \tab     \tab    \tab     \tab    \tab     \tab xy, uni \cr
\link{rothamsted.brussels}       \tab     \tab    \tab 4   \tab    \tab 6   \tab \cr
\link{shafii.rapeseed}           \tab 6   \tab 14 \tab 3   \tab 3  \tab     \tab \cr  
\link{smith.uniformity3}         \tab     \tab    \tab 4   \tab 3  \tab     \tab xy, uni \cr  
\link{stroup.nin}                \tab 56  \tab    \tab 4   \tab    \tab     \tab xy \cr  
\link{stroup.splitplot}          \tab     \tab    \tab 4   \tab    \tab     \tab \cr  
\link{student.barley}            \tab 2   \tab 51 \tab     \tab 6  \tab     \tab \cr
\link{talbot.potato}             \tab 9   \tab 12 \tab     \tab 6  \tab     \tab pls GxE GxT \cr  
\link{theobald.covariate}        \tab 10  \tab 7  \tab 5   \tab    \tab     \tab cov \cr
\link{thompson.cornsoy}          \tab     \tab 5  \tab 33  \tab    \tab     \tab corn/soy, repeated measures \cr
\link{vargas.wheat1}             \tab 7   \tab    \tab     \tab 6  \tab     \tab pls GxY~GxT YxG~YxE\cr  
\link{vargas.wheat2}             \tab 8   \tab 7  \tab     \tab    \tab     \tab pls ExG~ExT, 13 covs\cr  
\link{verbyla.lupin}             \tab 9   \tab 8  \tab     \tab 2  \tab     \tab xy, density\cr  
\link{williams.trees}            \tab 37  \tab 6  \tab     \tab    \tab 2   \tab \cr  
\link{wiebe.wheat}               \tab     \tab    \tab     \tab    \tab     \tab xy, uni\cr  
\link{yan.winterwheat}           \tab 18  \tab 9  \tab     \tab    \tab     \tab \cr  
\link{yates.missing}             \tab     \tab    \tab 10  \tab    \tab 3^2 \tab factorial \cr  
\link{yates.oats}                \tab 3   \tab    \tab 6   \tab    \tab     \tab xy, nitro\cr  
}

  The original sources for these data use several different words to refer to
  genetics including \emph{line}, \emph{cultivar}, \emph{hybrid},
  \emph{variety}, and \emph{genotype}. For consistency, these datasets
  all use \code{gen} (genotype).

  Box (1957) said, "I had hoped that we had seen the end of the obscene
  tribal habit practiced by statisticians of continually exhuming and
  massaging dead data sets after their purpose in life has long since been
  forgotten and there was no possibility of doing anything useful as a
  result of this treatment."

Clearly, massaging these 'dead' data sets will not lead to any of the
genetics being release for commercial use.  The value of these data is,
however, multifold:
1. Validating published analyses (reproducible research).
2. Providing data for testing new analysis methods.
3. Illustrating the use of R.
4. Learning from history so as not to repeat it.
}
\author{
  Kevin Wright, kw.stat@gmail.com
  
  The author is grateful to the many people who granted permission to
  include their data in this package.
  If you use these data, please consider citing this package
  and the original source of the data.  
}
\references{
  Box G. E. P. (1957), Integration of Techniques in Process Development,
  \emph{Transactions of the American Society for Quality Control}.
}

