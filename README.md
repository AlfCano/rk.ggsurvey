![Version](https://img.shields.io/badge/Version-0.1.4-green.svg)

An RKWard plugin package to create a wide range of publication-quality visualizations from complex survey data, using the powerful `{ggsurvey}` and `{ggplot2}` packages.

This package provides a user-friendly graphical interface for several `ggsurvey` functions, allowing for easy generation of weighted plots from `survey.design` objects and post-stratification tables from `svyby` objects.

## Features

This package provides six powerful and highly customizable plotting plugins:

1.  **Line Graph from svyby Object**:
    *   Ideal for visualizing trends over time from pre-calculated `svyby` results.
    *   Supports error bars based on user-defined confidence levels.
    *   Allows for faceting by an additional variable.

2.  **Means Graph from svyby Object**:
    *   Visualizes point estimates and confidence intervals using a clean dot plot style.
    *   A preferred alternative to bar charts for displaying means with uncertainty.
    *   Supports coordinate flipping (useful for long category labels) and faceting.

3.  **Bar Diagram**:
    *   Intelligently switches between functions based on the number of variables provided:
        *   **1 Variable**: Creates a weighted bar plot (`ggbarweight_svy`).
        *   **2 Variables**: Creates a crosstab bar plot (`ggbarcrosstabs_svy`) with `fill` or `dodge` positions.
        *   **3 Variables**: Creates a faceted 3D crosstab bar plot (`ggbarcrosstabs3d_svy`).
    *   Supports coordinate flipping for horizontal bars.

4.  **Box Plot**:
    *   Creates weighted boxplots for visualizing distributions.
    *   Switches between `ggboxweight_svy` (1D), `ggboxweight2d_svy` (2D), and `ggboxweight3d_svy` (3D with facets) based on input.

5.  **Hexbin Plot**:
    *   Creates weighted 2D density plots using hexagonal bins, ideal for large datasets.
    *   Switches between `gghexweight_svy`, `gghexweight2d_svy`, and `gghexweight3d_svy`.
    *   Allows customization of bin size and `viridis` color palettes.

6.  **Histogram**:
    *   Creates weighted histograms to explore variable distributions.
    *   Switches between `gghistweight_svy`, `gghistweight2d_svy`, and `gghistweight3d_svy`.
    *   Allows customization of bin count.

### Universal Features

All plotting plugins share a consistent, tabbed interface with extensive options for:
-   **Labels**: Full control over title, subtitle, axes, legend, and caption text.
-   **Style & Layout**: Options for color palettes (`ColorBrewer` or `viridis`), faceting layout, and other plot-specific tweaks.
-   **Output Device**: Fine-grained control over the output graph, including device type (PNG, JPG, SVG), dimensions, resolution, and background color.
-   **Live Preview**: All plotting dialogs include a preview pane that updates as you change options.

## Installation

This plugin is not yet on CRAN. To install it, you will need the `{remotes}` package.

1.  **Install `{remotes}`**:
    If you don't have it, open the R console in RKWard and run:
    ```R
    install.packages("remotes")
    ```

2.  **Install the Plugin**:
    Run the following command in the R console:
    ```R
    remotes::install_github("AlfCano/rk.ggsurvey")
    ```

3.  **Activate the Plugin**:
    Restart RKWard. The new menu items will be available automatically.

## Usage

Once installed, the plugins will be available in the RKWard menu under:

**`Survey` -> `Graphs` -> `ggGraphs`**

You will see the six plotting options:
-   Line Graph
-   Means Graph
-   Bar Diagram
-   Box Plot
-   Hexbin Plot
-   Histogram

Select the desired plot type, choose your data objects and variables in the dialog, customize the appearance using the tabs, and click "Submit" to generate the final high-quality graph.

## Dependencies

This plugin requires the following R packages to be installed:
-   `ggsurvey`
-   `survey`
-   `ggplot2`
-   `dplyr`
-   `tidyr`
-   `forcats`
-   `stringr`
-   `RColorBrewer`

## Author

Alfonso Cano
<alfonso.cano@correo.buap.mx>

*Plugin structure and JavaScript logic developed with assistance from Gemini, a large language model from Google.*

## License

GPL (>= 3)
