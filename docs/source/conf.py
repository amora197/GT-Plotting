# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# 'GT-Plotting using R' documentation build configuration file, created by
# sphinx-quickstart on Thu Oct 5 21:04:22 2017.
#
# This file is execfile()d with the current directory set to its
# containing dir.
# 
# Note that not all possible configuration values are present in this
# autogenerated file.
#
# All configuration values have a default; values that are commented out
# serve to show the default.

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

project = 'GT-Plotting'
copyright = '2020, Anza Ghaffar, Anibal Morales'
author = 'Anza Ghaffar, Anibal Morales'

# Specify date for LaTex pdf creation
today = '24 November 2020'


# The full version, including alpha/beta/rc tags
release = ''


# -- General configuration ---------------------------------------------------

# install the needed theme with:
import sphinx_rtd_theme

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = ['sphinx.ext.autodoc',
   'sphinx.ext.doctest',
   'sphinx.ext.intersphinx',
   'sphinx.ext.todo',
   'sphinx.ext.coverage',
   'sphinx.ext.mathjax',
   'sphinx.ext.ifconfig',
   'sphinx.ext.viewcode',
   'sphinx.ext.githubpages'
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']
source_suffix = '.rst'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
pygments_style = 'sphinx'
master_doc = 'index'

html_theme = "sphinx_rtd_theme"
html_add_permalinks = ""

html_theme_options = {
    'sticky_navigation': True,
    'collapse_navigation': False,
}


# -- Options for LaTeX output ---------------------------------------------

latex_engine = 'pdflatex'
latex_theme = 'howto'
latex_toplevel_sectioning = 'part'


# Configuration of Title Page
latex_maketitle = r'''
        \pagenumbering{Roman} %%% to avoid page 1 conflict with actual page 

        \begin{titlepage}

            \vspace*{10mm} %%% * is used to give space from top
            \flushright\textbf{\Huge {GT-Plotting Documentation}}

            \vspace{0mm} %%% * is used to give space from top
            \textbf{\huge {A Step-by-Step Tutorial - V1.0}}

            \vspace{50mm}
            \textbf{\Large {Anza Ghaffar, Anibal Morales}}

            \vspace{10mm}
            \textbf{\Large {Plant Breeding and Genetics Laboratory}}

            \vspace{0mm}
            \textbf{\Large {Seibersdorf, Austria}}

	    \vspace{10mm}
            \normalsize Created on : July, 2020

            \vspace*{0mm}
            \normalsize  Last updated : November, 2020


            %% \vfill adds at the bottom
            \vfill
            \small\flushleft {{\textbf {Please note:}} \textit {This is not an official IAEA publication but is made available as working material. The material has not undergone an official review by the IAEA. The views
expressed do not necessarily reflect those of the International Atomic Energy Agency or its Member States and remain the responsibility of the contributors. The use of particular designations of countries or territories does not imply any judgement by the publisher, the IAEA, as to the legal status of such countries or territories, of their authorities and institutions or of the delimitation of their boundaries. The mention of names of specific companies or products (whether or not indicated as registered) does not imply any intention to infringe proprietary rights, nor should it be construed as an endorsement or recommendation on the part of the IAEA.}}

        \end{titlepage}
        \pagenumbering{arabic}
'''
latex_elements = {
   'releasename': 'Version 1.0',
   'maketitle': latex_maketitle,
}
