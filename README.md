# rk.ggsurvey: Survey Visualization Tools for RKWard

![Version](https://img.shields.io/badge/Version-0.1.10-blue.svg)
![License](https://img.shields.io/badge/License-GPL--3-green.svg)
[![R Linter](https://github.com/AlfCano/rk.ggsurvey/actions/workflows/lintr.yml/badge.svg)](https://github.com/AlfCano/rk.ggsurvey/actions/workflows/lintr.yml)

An RKWard plugin package to create a wide range of publication-quality visualizations from complex survey data, using the powerful `{ggsurvey}` and `{ggplot2}` packages.

This package provides a user-friendly graphical interface for several `ggsurvey` functions, allowing for easy generation of weighted plots from `survey.design` objects and post-stratification tables from `svyby` objects.

## What's New in Version 0.1.10?

### 🐛 Bug Fixes
* **Critical UI Fix for Bivariate Plots:** Resolved a copy-paste bug where the **Box Plot**, **Hexbin Plot**, and **Histogram** components were incorrectly inheriting the Bar Diagram's UI and JavaScript logic. 
* **Missing Y-Variable Slots Added:** Box Plots and Hexbin Plots now correctly display input slots for both **X and Y variables** (previously, they only allowed an X variable).
* **Correct `ggsurvey` Function Calls:** The JavaScript generator was updated to call the appropriate underlying functions for each plot type instead of reusing `ggbarweight_svy()`:
  * Box Plot now calls `ggboxweight_svy(design, x, y)`
  * Hexbin Plot now calls `gghexweight_svy(design, x, y)`
  * Histogram now calls `gghistweight_svy(design, x)`

### ✨ Enhancements
* **Independent Previews:** Each plot component now has a unique RKWard preview ID (`plot_preview_bar`, `plot_preview_hex`, etc.). This prevents the internal RKWard HTML engine from crashing or freezing when switching between different types of graphs in the same session.
* **Component Restructuring:** The source code was refactored to separate "1-Variable Graphs" (Bar, Histogram) from "2-Variable Graphs" (Box, Hexbin), making future maintenance and scaling much easier.


## What's New in Version 0.1.9

*   **Cleaner Legends:** A new option in the **Labels** tab allows you to remove specific text patterns from legend labels in the line graph dialog. This is essential for cleaning up `svyby` output where variable names are often concatenated with factor levels (e.g., converting "VariableNameLevel" to just "Level").

## What's New in Version 0.1.8

This version focused on accessibility and internationalization. The entire plugin suite has been fully localized.

*   **Multilingual Support:** The interface is now available in:
    *   🇺🇸 English (Default)
    *   🇪🇸 Spanish (`es`)
    *   🇫🇷 French (`fr`)
    *   🇩🇪 German (`de`)
    *   🇧🇷 Portuguese (Brazil) (`pt_BR`)

## Features

This package provides six powerful and highly customizable plotting plugins:

1.  **Line Graph from svyby Object**:
    *   Ideal for visualizing trends over time from pre-calculated `svyby` results.
    *   Supports error bars based on user-defined confidence levels.
    *   Allows for faceting by an additional variable.
    *   **Visual Aesthetics:** Map grouping variables to Color, Shape, or both.
    *   **Label Cleaning:** built-in option to strip variable name prefixes from the legend.

2.  **Means Graph from svyby Object**:
    *   Visualizes point estimates and confidence intervals using a clean dot plot style (point + error bar).
    *   A superior alternative to bar charts for displaying means with uncertainty.
    *   Robust handling of single-variable or multi-variable inputs.
    *   Supports coordinate flipping (great for long category labels) and faceting.

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
-   **Labels**: Full control over title, subtitle, axes, legend, and caption text, with text wrapping options.
-   **Style & Layout**: Options for color palettes (`ColorBrewer` or `viridis`), faceting layout, and themes.
-   **Output Device**: Fine-grained control over the output graph, including device type (PNG, JPG, SVG), dimensions, resolution, and background color.
-   **Live Preview**: All plotting dialogs include a preview pane that updates as you change options.

## Installation

This plugin is not yet on CRAN. To install it, you will need the `{remotes}` or `{devtools}` package.

1.  **Open RKWard**.
2.  **Run the following command** in the R console:

    ```R
    # If you don't have devtools installed:
    # install.packages("devtools")
    
    local({
      require(devtools)
      install_github("AlfCano/rk.ggsurvey", force = TRUE)
    })
    ```

3.  **Restart RKWard**. The new menu items will be available automatically.

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
-   `scales`

## Author

Alfonso Cano
<alfonso.cano@correo.buap.mx>

*Plugin structure and JavaScript logic developed with assistance from Gemini, a large language model from Google.*

## License

GPL (>= 3)
