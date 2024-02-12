---
title: Develope Online Experiments through Psychopy
author: Yen Chun Chen
date: '2024-01-31'
slug: psychopy
categories:
  - Experiment
tags:
  - Python
  - Experimental Design
  - Data Management
  - Other
subtitle: ''
summary: 'I created online experiments with Psychopy, PsychoJS, and Pavlovia. Which enable researchers to collect data on a web page.'
authors: []
lastmod: '2024-01-31T17:09:04+08:00'
featured: no
image:
  caption: 'Photo credit by psychopy.org'
  focal_point: 'smart'
  preview_only: no
projects: []
links:
- icon: github
  icon_pack: fab
  name: Materials
  url: https://github.com/yenchunnnn/psychopy-experiment
---

I built online experiments with [Open Science Tools](https://opensciencetools.org/) when I was working as a research assistant.

The tools include:

-   PsychoPy

    One of the most popular open-source packages for running experiments in psychology, neuroscience, and related behavioral sciences.[^1] Here's their [github](https://github.com/psychopy) and [official website](https://www.psychopy.org/).

    *(You can also randomly assign participants to different treatment/control groups by setting additional code components.)*

-   PsychoJS

    The JavaScript port of the PsychoPy library. Its purpose is to make PsychoPy experiments available online via a web page. Here's their [github](https://github.com/psychopy/psychojs).

-   Pavlovia

    A place for the wide community of researchers in the behavioral sciences to run, share, and explore experiments online. Here's their [official website](https://pavlovia.org/docs/home/about).

[^1]: Peirce, J. W., Gray, J. R., Simpson, S., MacAskill, M. R., Höchenberger, R., Sogo, H., Kastman, E., Lindeløv, J. (2019). [PsychoPy2: experiments in behavior made easy.](https://link.springer.com/article/10.3758/s13428-018-01193-y) *Behavior Research Methods.* 10.3758/s13428-018-01193-y

An example of the experiment's flow:

``` mermaid
graph LR
Aid1([Survey]) -->B(Pretest)
    B -- Random Assign --> C[Manipulate IV_1]
    C -->|Group1| D[Manipulate IV_2]
    C -->|Group2| D[Manipulate IV_2]
    D -->|Group1| E(Measure DV)
    D -->|Group2| E(Measure DV)
    D -->|Group3| E(Measure DV)
    E --> F(Posttest)
```

What I've utilized in this project:

-   {{< icon name="python" pack="fab" >}} Python

-   {{< icon name="r-project" pack="fab" >}} R

    -   rmarkdown, [OCTA shiny app](https://elinevg.shinyapps.io/OCTA_toolbox/)[^2]

-   {{< icon name="square-js" pack="fab" >}} JavaScript

-   {{< icon name="html5" pack="fab" >}} Html

-   {{< icon name="square-gitlab" pack="fab" >}} Gitlab

[^2]: Van Geert, E., Bossens, C., & Wagemans, J. (2022). The Order & Complexity Toolbox for Aesthetics (OCTA): A systematic approach to study the relations between order, complexity, and aesthetic appreciation. <https://doi.org/10.31234/osf.io/2bu8a>

Thanks to all contributors in open science for helping researchers easily conduct experiments on online platforms without a lot of cost. Which is efficient and good for the science community!
