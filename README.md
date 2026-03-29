# Assembly-Based Product Composition Calculator

# Check out the live app: https://tpettit3.shinyapps.io/product-composition-calculator/

## Overview

This application is a **Shiny-based Product Composition Calculator** designed to estimate the material breakdown of a configurable, modular product. It provides users with a clear view of how different product configurations influence overall material composition, supporting more informed decision-making—particularly in sustainability and end-of-life planning contexts.

This is a **sanitized demo version** built using simulated data and generalized product structures.

---

## Concept & Model Structure

The calculator is built around a product architecture consisting of a **matrix of modular subunits**:

* The product is defined by a grid (e.g., *Modular Units High × Modular Units Wide*)
* This matrix determines the quantity of a core “Modular Assembly”
* Additional assemblies are then calculated based on:

  * The size of the modular matrix
  * The selected product variant
  * Customer-selected configuration options

### Product Variants

The app supports multiple product types (Product A–G), which represent:

* Products with **similar overall design**
* But **different assemblies and quantity logic**

Each product follows its own calculation pathway (“branch logic”) to reflect these differences.

---

## Configuration Logic

### Assembly Quantities

Assembly quantities are determined through a combination of:

* Modular grid size (primary driver)
* Conditional logic based on:

  * Product type
  * Selected assembly options
  * Market or configuration context

### Customer-Selectable Options

Some assemblies include **configurable options** (e.g., Assembly 1A vs. 1B vs. 1C):

* These represent real-world design choices available to customers
* Selecting an option may:

  * Change the quantity of that assembly
  * Influence quantities of other dependent assemblies
  * Or have no effect on unrelated components

This reflects how real products often behave:

> Not all configuration choices impact all parts equally.

---

## Material Composition Calculation

Each assembly includes:

* A defined **weight**
* A **percentage breakdown of materials** (e.g., Material1, Material2, etc.)

The app:

1. Calculates total weight contribution by assembly
2. Converts percentage material data into absolute mass (kg)
3. Aggregates across all assemblies
4. Outputs:

   * Total material weights
   * Percent composition
   * Visualization via pie chart

An **“Unevaluated Material”** category captures any difference between:

* User-defined total product weight
* Calculated known assembly weight

---

## Purpose & Use Case

The primary purpose of this tool is to:

> **Provide product material composition data to customers to support end-of-life decision making.**

### Why this matters

Understanding material composition is increasingly important for:

* ♻️ **Recycling and disposal planning**
* 🌍 **Environmental impact awareness**
* 📊 **Sustainability reporting**
* 🏢 **Regulatory compliance**
* 🤝 **Customer transparency**

### Stakeholders who benefit

* Customers evaluating products for sustainability
* Procurement teams with environmental requirements
* Sustainability / ESG teams
* Manufacturers tracking product-level impacts
* Regulators and compliance bodies

---

## Assembly-Level vs. Part-Level Modeling

This calculator operates at the **assembly level**, not the individual part level.

### Ideal scenario

The most accurate approach would be:

> **Part-level composition modeling**

This would involve:

* Detailed bill of materials (BOM)
* Material composition for each individual component

### Why assembly-level is used here

In practice, many organizations—especially those with:

* Smaller sustainability teams
* Limited data infrastructure
* Early-stage ESG programs

…do not have access to complete part-level data.

### Tradeoff

| Approach       | Accuracy | Feasibility |
| -------------- | -------- | ----------- |
| Part-level     | High     | Low–Medium  |
| Assembly-level | Medium   | High        |

This tool demonstrates that:

> **Assembly-level modeling is a practical and scalable starting point**
> for generating meaningful composition insights.

---

## Key Features

* Modular product configuration
* Multiple product logic pathways
* Conditional assembly behavior
* Dynamic material composition calculation
* Pie chart visualization
* Downloadable data outputs
* Reactive UI with user input controls

---

## Disclaimer

This is a **demonstration tool**:

* All data is simulated
* Product names and structures are generalized
* No proprietary or confidential information is included

---

## Future Enhancements

Potential extensions of this framework include:

* Transition to part-level modeling
* Integration with real BOM systems
* Lifecycle assessment (LCA) integration
* Carbon footprint calculations
* Cost modeling
* Scenario comparison tools

---

## Getting Started

To run locally:

```r
shiny::runApp()
```

Ensure required packages are installed:

* shiny
* ggplot2
* dplyr
* tidyr
* shinyjs
* and others listed in `app.R`

---

## Author

Trevor Pettit
March 2026