---
title: Develope online experiments through Psychopy
author: Yen Chun Chen
date: '2024-01-31'
slug: psychopy
categories:
  - Experiment
tags:
  - Python
  - Experimental design
  - Data management
  - Other
subtitle: ''
summary: 'I created online experiments with Psychopy, PsychoJS and Pavlovia. Which enable researchers to collect data on a web page.'
authors: []
lastmod: '2024-01-31T17:09:04+08:00'
featured: no
image:
  caption: 'Photo from: psychopy.org'
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

    One of the most popular open-source packages for running experiments in psychology, neuroscience and related behavioral sciences.[^1] Here's their [github](https://github.com/psychopy) and [offical website](https://www.psychopy.org/).
    
    _(You can also random assign participants to different treatment/control groups by setting additional code components.)_
    
-   PsychoJS

    The JavaScript port of the PsychoPy library. Its purpose is to make PsychoPy experiments available online, from a web page. Here's their [github](https://github.com/psychopy/psychojs).

-   Pavlovia

    A place for the wide community of researchers in the behavioural sciences to run, share, and explore experiments online. Here's their [offical website](https://pavlovia.org/docs/home/about).

[^1]: Peirce, J. W., Gray, J. R., Simpson, S., MacAskill, M. R., Höchenberger, R., Sogo, H., Kastman, E., Lindeløv, J. (2019). [PsychoPy2: experiments in behavior made easy.](https://link.springer.com/article/10.3758/s13428-018-01193-y) *Behavior Research Methods.* 10.3758/s13428-018-01193-y

What I've utilized in this project:

-   {{< icon name="python" pack="fab" >}} Python

-   {{< icon name="r-project" pack="fab" >}} R 

    - rmarkdown, [OCTA shiny app](https://elinevg.shinyapps.io/OCTA_toolbox/)[^2]

-   {{< icon name="square-js" pack="fab" >}} JavaScript

-   {{< icon name="html5" pack="fab" >}} Html

-   {{< icon name="square-gitlab" pack="fab" >}} Gitlab

[^2]: Van Geert, E., Bossens, C., & Wagemans, J. (2022). The Order & Complexity Toolbox for Aesthetics (OCTA): A systematic approach to study the relations between order, complexity, and aesthetic appreciation. https://doi.org/10.31234/osf.io/2bu8a

Thanks for all contributors in the open science, helping researchers could easily lunch experiments on online platforms without lots of cost. Which is efficient, and good for science community!
