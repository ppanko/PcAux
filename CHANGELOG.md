# Change Log
All notable changes to the **PcAux** project will be documented in this file.

The format is based on [Keep a Changelog][kacl], and this project adheres to
[Semantic Versioning][sv].

NOTE: Changes prior to version 0.0.0.9006 were documented retrospectively and 
should, therefore, be viewed with an appropriate degree of skepticism.

## 0.0.0.9007 - 2017-06-12

### Added
- @ppanko contributed documentation for his timing/benchmarking utilities
- Included tutorial document in the "documentation" directory

## 0.0.0.9006 - 2017-04-13

### Changed
- Improved the README.md file

### Added
- Added this Change Log
- Added URL and BugReports links to DESCRIPTION file

## 0.0.0.9005 - 2017-04-07

### Added
- @ppanko (Pavel Panko) contributed utilities for timing/benchmarking

## 0.0.0.9004 - 2017-03-27

### Changed
- Tightened up the dependencies by moving all but **mice** into the *imports* 
  section and using explicit namespace resolution, where feasible
 
## 0.0.0.9003 - 2017-03-26

### Fixed
- Fixed bug that was causing all PcAux scores to be used as predictors, 
  regardless of what value the user set for the *nComps* argument.

## 0.0.0.9002 - 2017-03-24

### Changed
- Adopted semantic versioning scheme suggested by [Hadley Wickham][hw]

### Fixed
- Fixed a bug that occurred while casting nominal variables that was causing 
  missing rows to be excluded from the dummy-coded representations

## 0.0.1 - 2017-03-23

### Changed
- More documentation clean-up after the package name change

## 0.0.0 - 2017-03-23

### Changed
- Changed the name of the package from **quark** to **PcAux**
- Updated README.md
- Cleaned up documentation

## <0.0.0 (i.e., Change history for **quark**) - <2017-03-23

At the same time as changing the name of this package from **quark** to 
**PcAux**, many dramatic changes were made to the package internals and to key 
aspects of the user-interface. For those early-adopters who have become 
familiar with the UI of **quark** please take some time to familiarize yourself 
with the new features of **PcAux**.

The biggest differences that experienced users are likely to notice are in:

1. The ways that the number of PcAux scores are selected (see documentation for 
   `createPcAux` and `miWithPcAux` for details) 
2. The ways that interactions are incorporated into the PcAux scores (the 
   **quark**-era approach is still available by setting `interactType = 3`; see 
   the documentation for `createPcAux` for details)
3. Several places were redundant function arguments were removed (e.g., to 
   exclude polynomial terms you now set `maxPolyPow = 1` without any dedicated 
   logical switch).

Code that ran with **quark** will almost certainly fail with **PcAux**. The 
PcAux development team apologizes for any inconvenience that this lack of 
backward compatibility may cause. 

Please recognize, as this project moves forward, that **PcAux** is a rapidly 
developing piece of beta software, and the development team is, currently, 
making no significant effort to maintain backward compatibility. To do so would 
only stagnate development. Once we release a feature-complete version of 
**PcAux** we will begin to seriously consider backward compatibility.

### Previous Contribution Credits
The orginal source code for **quark** was adapted from an un-packaged 
implementation written by Dr. Steven Chesnut.

During **quark**-era development:
- @IMMAPbjung (Byungkwan Jung) contributed a routine to check for collinearity 
  using parallel processing
- @vibhutittu (Vibhuti Gupta) contributed an improved algorithm for selecting 
  which variables to exclude when dissolving collinear variable pairs

[kacl]: http://keepachangelog.com/
[sv]:   http://semver.org/
[hw]:   http://r-pkgs.had.co.nz/
