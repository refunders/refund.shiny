# `refund.shiny`
[![](https://travis-ci.org/refunders/refund.shiny.svg?branch=master)](https://travis-ci.org/refunders/refund.shiny)

## Interactive plots of functional data analyses.

The primary function is `plot.shiny`, which calls other functions depending on the class of the object. The supported classes are "fpca" for functional principal components analyses, "mfpca" for multilevel fpca, "lfpca" for longitudinal fpca, and "fosr" for function on scalar regression.

---------------

### Installation

To install the latest version directly from Github, please use:
<pre><code>install.packages("devtools")
devtools::install_github("refunders/refund.shiny", ref = "jw-devel")
</code></pre>
