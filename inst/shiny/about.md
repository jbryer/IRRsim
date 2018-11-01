### Simulating Inter-Class Correlation (ICC)

##### Author: Jason Bryer, Ph.D. jason@bryer.org

This shiny app is included in the `IRRsim` R package. You can get more information a Github repository for the package: https://github.com/jbryer/IRRsim

The intraclass correlations (ICC) are caluclated using the `ICC` function in the  [`DescTools`](https://cran.r-project.org/web/packages/DescTools/index.html) package. For more information about ICC, see [Shrout and Fleiss](https://cran.r-project.org/web/packages/DescTools/index.html) (1979).

#### Options

* **Number of Scoring Levels** - The number of scoring levels for each event (e.g. number of levels in the rubric).
* **Number of Raters** - The total number of raters available for each scoring event.
* **Number of Raters per Event** - The number of raters who scored each event.
* **Inter-class Correlation** - Which ICC to visualize, or all six ICC statistics in a faceted plot.
* **Number of Scoring Events per Matrix** - The number of scoring events within each simulated dataset.
* **Number of Scoring Matrices** - The number of scoring matrices.
* **Simulate data in parallel** - Whether multiple processes should be used to simulated the data.
