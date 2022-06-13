\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{homework}[2021/01/01 Homework]
\DeclareOption{handout}{\PassOptionsToClass{\CurrentOption}{amsart}}
\ProcessOptions\relax
\LoadClass[12pt, oneside]{amsart}
\RequirePackage[margin=0.5in]{geometry} % For margins
\RequirePackage{lmodern, enumitem, needspace} % Needspace for dynamic line breaking
\RequirePackage{graphicx, wrapfig, pgffor, ifthen} % For images, wrappings and if statements
\RequirePackage[normalem]{ulem} % For better underlining and org-mode
\RequirePackage[font=small]{caption} % For caption resizing with wrapfig
\RequirePackage[caption=false]{subfig}
\RequirePackage{hyperref} % For pdf bookmarks, other information and org-mode
\RequirePackage{hwsymb, hwlst} % By Tashfeen
\hypersetup {
  pdfstartview=FitH,
  colorlinks=true,
}

% Bonus question environment
\theoremstyle{definition}
\newtheorem{bonus}{\@unitname{} (Bonus)}

\usepackage{graphbox,graphicx}
\usepackage{mathtools}
\usepackage{xcolor}
\usepackage{tikz}
\usepackage[most]{tcolorbox}
\usepackage{listings}


% For right aligned figure
\newcommand{\rightfig}[4][0.4] {
  \begin{wrapfigure}{r}{#1\textwidth}
    \centering
    \vspace{-3ex}
    \includegraphics[width=#1\textwidth]{#2}
    \vspace{-3ex}
    \caption{#3}
    \vspace{-1ex}
    \label{#4} % has to stay after caption
  \end{wrapfigure}
}

% Just one or a list of figures
\newcommand{\fig}[4][0.4]{
  \begin{figure}[hbtp]
    \foreach \elem in {#2} {
      \begin{minipage}{#1\textwidth}\centering
        \subfloat {
          \includegraphics[width=\linewidth]{\elem}
        }
      \end{minipage} % minipage for easy centring
    }
    \caption{#3}\label{#4}
  \end{figure}
}

% This handles the info tags at the beginning
\makeatletter
  \gdef\@author{\empty}
  \gdef\@class{\empty}
  \newcommand{\class}[1]{\gdef\@class{#1}}
  \renewcommand{\author}[1]{\gdef\@author{#1}}
  \renewcommand{\maketitle} {\bgroup\setlength{\parindent}{0em}
    \begin{flushleft}
      \ifthenelse{\equal{\@author}{\empty}}{}{\@author\\}
      \ifthenelse{\equal{\@class}{\empty}}{}{\@class\\}
      \@date
    \end{flushleft}\egroup
    \begin{center}
      \textbf{\@title} \\
    \end{center}
    \bigskip
  }
\makeatother
