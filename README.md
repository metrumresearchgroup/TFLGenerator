# TFL package documentation

TFL R package documentation can be found [here](https://metrumresearchgroup.github.io/TFL/).

# Installation

The [deliv](https://github.com/metrumresearchgroup/TFLGenerator/tree/master/deliv) folder contains the executable.  Follow the instructions outlined in the README.md there on an Envision workflow to run the GUI.  

# Development

When developing, use the Makefile to manage the deployment process after updating the GUI and TFL R packages (which exist as submodules in this repository).  Typically, you'll use:

`make data update_documentation build install update_server signal_restart`

When ready to update the deliverable:

`make update_deliv`

