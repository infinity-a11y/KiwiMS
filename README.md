<img src="KiwiMS_App/resources/KiwiMS.png" align="right" width="20%"/>

KiwiMS is an interactive pipeline with graphical user interface to perform proteomics mass spectrometry data analysis. It implements UniDec's (https://github.com/michaelmarty/UniDec) bayesian mass spectra deconvolution method and provides downstream analyses for protein binding studies.

![Windows](https://img.shields.io/badge/Windows-339033?style=flat&logo=windows&logoColor=white) [![Version](https://img.shields.io/badge/Version-0.3.1-E8CB98)](https://github.com/infinity-a11y/KiwiMS/releases/tag/0.3.1) [![License: GPLv3](https://img.shields.io/badge/License-GPLv3-659DA3.svg)](https://www.gnu.org/licenses/gpl-3.0) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16575977.svg)](https://doi.org/10.5281/zenodo.16575976) [![KiwiMS Installation Validation](https://github.com/infinity-a11y/KiwiMS/actions/workflows/test-installer.yml/badge.svg)](https://github.com/infinity-a11y/KiwiMS/actions/workflows/test-installer.yml)

<sup>*KiwiMS is supposed to be used for research and academic purposes only.*</sup>

<br><br><hr><br>

### Current Version: KiwiMS 0.3.1

<i> 2026-02-09 </i> <br> <b>KiwiMS 0.3.1</b> <br> <https://github.com/infinity-a11y/KiwiMS/releases/tag/0.3.1>

<br><hr><br>

### Installation

1.  Download the KiwiMS installer from the latest release: <br>

| OS | Version | Download | SHA256 checksum |
|------------------|------------------|------------------|------------------|
| Windows 10/11 | 0.3.1 | [KiwiMS_0.3.1-Windows-x86_64.exe](https://github.com/infinity-a11y/KiwiMS/releases/download/0.3.1/KiwiMS_0.3.1-Windows-x86_64.exe) | <sub><sup>E53055B1D90CA6863D3D7B50E950206A5D03173BF66104614E59EED837674508</sup></sub> |

2.  Run the installer to set KiwiMS up.

> [!NOTE]
> <i>You may see a security warning from Windows Defender SmartScreen. This is a standard notification that appears because the installer is new and does not yet have a widely recognized digital signature.</i>

3.  Launching the installer, a blue window titled "Windows protected your PC" may appear.
4.  On this window, click the small text link that says "More info."
5.  The window will expand to show more details. You will see that the "Publisher" is listed as "Unknown publisher."
6.  Below this information, a new button will appear. Click the "Run anyway" button to start the installation.

<br><hr><br>

### System Requirements

-   **Operating System**: Windows 10/11.
-   **Browser**: KiwiMS is running in the default browser.
-   **Administrative Privileges**: Required for setup and updates. A UAC prompt will appear.
-   **Internet Connection**: Required to download Miniconda, updates, and packages.

<br><hr><br>

### Citation

>Please cite both **KiwiMS** and **UniDec** if you used this software in your work.

Marian Freisleben. (2026). infinity-a11y/KiwiMS: KiwiMS 0.3.1. Zenodo. DOI: https://doi.org/10.5281/zenodo.18552188
```      
@software{marian_freisleben_2026_18552188,
  author       = {Marian Freisleben},
  title        = {infinity-a11y/KiwiMS: KiwiMS 0.3.1},
  month        = feb,
  year         = 2026,
  publisher    = {Zenodo},
  version      = {0.3.1},
  doi          = {10.5281/zenodo.18552188},
  url          = {https://doi.org/10.5281/zenodo.18552188},
}
```

Marty, M. T.; Baldwin, A. J.; Marklund, E. G.; Hochberg, G. K.; Benesch, J. L.; Robinson, C. V. Bayesian deconvolution of mass and ion mobility spectra: from binary interactions to polydisperse ensembles. Analytical chemistry 2015, 87 (8), 4370– 6,  DOI: https://doi.org/10.1021/acs.analchem.5b00140
```
@article{marty_bayesian_2015,
	title = {Bayesian {Deconvolution} of {Mass} and {Ion} {Mobility} {Spectra}: {From} {Binary} {Interactions} to {Polydisperse} {Ensembles}},
	volume = {87},
	issn = {0003-2700},
	shorttitle = {Bayesian {Deconvolution} of {Mass} and {Ion} {Mobility} {Spectra}},
	url = {https://doi.org/10.1021/acs.analchem.5b00140},
	doi = {10.1021/acs.analchem.5b00140},
	abstract = {Interpretation of mass spectra is challenging because they report a ratio of two physical quantities, mass and charge, which may each have multiple components that overlap in m/z. Previous approaches to disentangling the two have focused on peak assignment or fitting. However, the former struggle with complex spectra, and the latter are generally computationally intensive and may require substantial manual intervention. We propose a new data analysis approach that employs a Bayesian framework to separate the mass and charge dimensions. On the basis of this approach, we developed UniDec (Universal Deconvolution), software that provides a rapid, robust, and flexible deconvolution of mass spectra and ion mobility-mass spectra with minimal user intervention. Incorporation of the charge-state distribution in the Bayesian prior probabilities provides separation of the m/z spectrum into its physical mass and charge components. We have evaluated our approach using systems of increasing complexity, enabling us to deduce lipid binding to membrane proteins, to probe the dynamics of subunit exchange reactions, and to characterize polydispersity in both protein assemblies and lipoprotein Nanodiscs. The general utility of our approach will greatly facilitate analysis of ion mobility and mass spectra.},
	number = {8},
	urldate = {2026-02-12},
	journal = {Analytical Chemistry},
	publisher = {American Chemical Society},
	author = {Marty, Michael T. and Baldwin, Andrew J. and Marklund, Erik G. and Hochberg, Georg K. A. and Benesch, Justin L. P. and Robinson, Carol V.},
	month = apr,
	year = {2015},
	pages = {4370--4376}
}
```

<br><hr><br>

<p align="center">
  <a href="https://liora-bioinformatics.com/">
    <picture>
      <source media="(prefers-color-scheme: dark)" srcset="media/Liora_rect_white.png">
      <img alt="Liora Bioinformatics" src="media/Liora_Rect.png" height="130px">
    </picture>
  </a>
  &nbsp&nbsp
  <a href="https://github.com/michaelmarty/UniDec">
    <img alt="UniDec Logo" src="media/unidec.png" height="130px">
  </a>
  &nbsp&nbsp
  <a href="https://www.hs-furtwangen.de/en/">
    <img alt="HFU Logo" src="media/hfu_logo.png" height="130px">
  </a>
</p>

<br><hr>

Developed by Marian Freisleben. <br>
