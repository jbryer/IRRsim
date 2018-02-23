### Simulating Inter-Class Correlation (ICC)

##### Author: Jason Bryer, Ph.D. jason@bryer.org

This shiny app is included in the `IRRsim` R package. You can get more information a Github repository for the package: https://github.com/jbryer/IRRsim

The intraclass correlations (ICC) are caluclated using the `ICC` function in the  [`DescTools`](https://cran.r-project.org/web/packages/DescTools/index.html) package. For more information about ICC, see [Shrout and Fleiss](https://cran.r-project.org/web/packages/DescTools/index.html) (1979).

#### Options

* **Number of Levels** - The number of scoring levels to simulate.
* **Number of Raters** - The number of raters to use in each simulated dataset.
* **Inter-class Correlation** - Which ICC to visualize, or all six ICC statistics in a faceted plot.
* **Number of Ratings** - The number of ratings per simulated dataset to generate.
* **Number of Samples** - The total number of samples to generate.
