<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<!-- 2019-04-01 Mon 17:15 -->
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>My Emacs Initialisation File, Written in <code>Org-mode</code></title>
<meta name="generator" content="Org mode" />
<meta name="author" content="Musa Al-hassy" />
<meta name="description" content="Configuration file for Emacs usage."
 />
<style type="text/css">
 <!--/*--><![CDATA[/*><!--*/
  .title  { text-align: center;
             margin-bottom: .2em; }
  .subtitle { text-align: center;
              font-size: medium;
              font-weight: bold;
              margin-top:0; }
  .todo   { font-family: monospace; color: red; }
  .done   { font-family: monospace; color: green; }
  .priority { font-family: monospace; color: orange; }
  .tag    { background-color: #eee; font-family: monospace;
            padding: 2px; font-size: 80%; font-weight: normal; }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .org-right  { margin-left: auto; margin-right: 0px;  text-align: right; }
  .org-left   { margin-left: 0px;  margin-right: auto; text-align: left; }
  .org-center { margin-left: auto; margin-right: auto; text-align: center; }
  .underline { text-decoration: underline; }
  #postamble p, #preamble p { font-size: 90%; margin: .2em; }
  p.verse { margin-left: 3%; }
  pre {
    border: 1px solid #ccc;
    box-shadow: 3px 3px 3px #eee;
    padding: 8pt;
    font-family: monospace;
    overflow: auto;
    margin: 1.2em;
  }
  pre.src {
    position: relative;
    overflow: visible;
    padding-top: 1.2em;
  }
  pre.src:before {
    display: none;
    position: absolute;
    background-color: white;
    top: -10px;
    right: 10px;
    padding: 3px;
    border: 1px solid black;
  }
  pre.src:hover:before { display: inline;}
  /* Languages per Org manual */
  pre.src-asymptote:before { content: 'Asymptote'; }
  pre.src-awk:before { content: 'Awk'; }
  pre.src-C:before { content: 'C'; }
  /* pre.src-C++ doesn't work in CSS */
  pre.src-clojure:before { content: 'Clojure'; }
  pre.src-css:before { content: 'CSS'; }
  pre.src-D:before { content: 'D'; }
  pre.src-ditaa:before { content: 'ditaa'; }
  pre.src-dot:before { content: 'Graphviz'; }
  pre.src-calc:before { content: 'Emacs Calc'; }
  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }
  pre.src-fortran:before { content: 'Fortran'; }
  pre.src-gnuplot:before { content: 'gnuplot'; }
  pre.src-haskell:before { content: 'Haskell'; }
  pre.src-hledger:before { content: 'hledger'; }
  pre.src-java:before { content: 'Java'; }
  pre.src-js:before { content: 'Javascript'; }
  pre.src-latex:before { content: 'LaTeX'; }
  pre.src-ledger:before { content: 'Ledger'; }
  pre.src-lisp:before { content: 'Lisp'; }
  pre.src-lilypond:before { content: 'Lilypond'; }
  pre.src-lua:before { content: 'Lua'; }
  pre.src-matlab:before { content: 'MATLAB'; }
  pre.src-mscgen:before { content: 'Mscgen'; }
  pre.src-ocaml:before { content: 'Objective Caml'; }
  pre.src-octave:before { content: 'Octave'; }
  pre.src-org:before { content: 'Org mode'; }
  pre.src-oz:before { content: 'OZ'; }
  pre.src-plantuml:before { content: 'Plantuml'; }
  pre.src-processing:before { content: 'Processing.js'; }
  pre.src-python:before { content: 'Python'; }
  pre.src-R:before { content: 'R'; }
  pre.src-ruby:before { content: 'Ruby'; }
  pre.src-sass:before { content: 'Sass'; }
  pre.src-scheme:before { content: 'Scheme'; }
  pre.src-screen:before { content: 'Gnu Screen'; }
  pre.src-sed:before { content: 'Sed'; }
  pre.src-sh:before { content: 'shell'; }
  pre.src-sql:before { content: 'SQL'; }
  pre.src-sqlite:before { content: 'SQLite'; }
  /* additional languages in org.el's org-babel-load-languages alist */
  pre.src-forth:before { content: 'Forth'; }
  pre.src-io:before { content: 'IO'; }
  pre.src-J:before { content: 'J'; }
  pre.src-makefile:before { content: 'Makefile'; }
  pre.src-maxima:before { content: 'Maxima'; }
  pre.src-perl:before { content: 'Perl'; }
  pre.src-picolisp:before { content: 'Pico Lisp'; }
  pre.src-scala:before { content: 'Scala'; }
  pre.src-shell:before { content: 'Shell Script'; }
  pre.src-ebnf2ps:before { content: 'ebfn2ps'; }
  /* additional language identifiers per "defun org-babel-execute"
       in ob-*.el */
  pre.src-cpp:before  { content: 'C++'; }
  pre.src-abc:before  { content: 'ABC'; }
  pre.src-coq:before  { content: 'Coq'; }
  pre.src-groovy:before  { content: 'Groovy'; }
  /* additional language identifiers from org-babel-shell-names in
     ob-shell.el: ob-shell is the only babel language using a lambda to put
     the execution function name together. */
  pre.src-bash:before  { content: 'bash'; }
  pre.src-csh:before  { content: 'csh'; }
  pre.src-ash:before  { content: 'ash'; }
  pre.src-dash:before  { content: 'dash'; }
  pre.src-ksh:before  { content: 'ksh'; }
  pre.src-mksh:before  { content: 'mksh'; }
  pre.src-posh:before  { content: 'posh'; }
  /* Additional Emacs modes also supported by the LaTeX listings package */
  pre.src-ada:before { content: 'Ada'; }
  pre.src-asm:before { content: 'Assembler'; }
  pre.src-caml:before { content: 'Caml'; }
  pre.src-delphi:before { content: 'Delphi'; }
  pre.src-html:before { content: 'HTML'; }
  pre.src-idl:before { content: 'IDL'; }
  pre.src-mercury:before { content: 'Mercury'; }
  pre.src-metapost:before { content: 'MetaPost'; }
  pre.src-modula-2:before { content: 'Modula-2'; }
  pre.src-pascal:before { content: 'Pascal'; }
  pre.src-ps:before { content: 'PostScript'; }
  pre.src-prolog:before { content: 'Prolog'; }
  pre.src-simula:before { content: 'Simula'; }
  pre.src-tcl:before { content: 'tcl'; }
  pre.src-tex:before { content: 'TeX'; }
  pre.src-plain-tex:before { content: 'Plain TeX'; }
  pre.src-verilog:before { content: 'Verilog'; }
  pre.src-vhdl:before { content: 'VHDL'; }
  pre.src-xml:before { content: 'XML'; }
  pre.src-nxml:before { content: 'XML'; }
  /* add a generic configuration mode; LaTeX export needs an additional
     (add-to-list 'org-latex-listings-langs '(conf " ")) in .emacs */
  pre.src-conf:before { content: 'Configuration File'; }

  table { border-collapse:collapse; }
  caption.t-above { caption-side: top; }
  caption.t-bottom { caption-side: bottom; }
  td, th { vertical-align:top;  }
  th.org-right  { text-align: center;  }
  th.org-left   { text-align: center;   }
  th.org-center { text-align: center; }
  td.org-right  { text-align: right;  }
  td.org-left   { text-align: left;   }
  td.org-center { text-align: center; }
  dt { font-weight: bold; }
  .footpara { display: inline; }
  .footdef  { margin-bottom: 1em; }
  .figure { padding: 1em; }
  .figure p { text-align: center; }
  .inlinetask {
    padding: 10px;
    border: 2px solid gray;
    margin: 10px;
    background: #ffffcc;
  }
  #org-div-home-and-up
   { text-align: right; font-size: 70%; white-space: nowrap; }
  textarea { overflow-x: auto; }
  .linenr { font-size: smaller }
  .code-highlighted { background-color: #ffff00; }
  .org-info-js_info-navigation { border-style: none; }
  #org-info-js_console-label
    { font-size: 10px; font-weight: bold; white-space: nowrap; }
  .org-info-js_search-highlight
    { background-color: #ffff00; color: #000000; font-weight: bold; }
  .org-svg { width: 90%; }
  /*]]>*/-->
</style>
<script type="text/javascript">
/*
@licstart  The following is the entire license notice for the
JavaScript code in this tag.

Copyright (C) 2012-2018 Free Software Foundation, Inc.

The JavaScript code in this tag is free software: you can
redistribute it and/or modify it under the terms of the GNU
General Public License (GNU GPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option)
any later version.  The code is distributed WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

As additional permission under GNU GPL version 3 section 7, you
may distribute non-source (e.g., minimized or compacted) forms of
that code without the copy of the GNU GPL normally required by
section 4, provided you include this license notice and a URL
through which recipients can access the Corresponding Source.


@licend  The above is the entire license notice
for the JavaScript code in this tag.
*/
<!--/*--><![CDATA[/*><!--*/
 function CodeHighlightOn(elem, id)
 {
   var target = document.getElementById(id);
   if(null != target) {
     elem.cacheClassElem = elem.className;
     elem.cacheClassTarget = target.className;
     target.className = "code-highlighted";
     elem.className   = "code-highlighted";
   }
 }
 function CodeHighlightOff(elem, id)
 {
   var target = document.getElementById(id);
   if(elem.cacheClassElem)
     elem.className = elem.cacheClassElem;
   if(elem.cacheClassTarget)
     target.className = elem.cacheClassTarget;
 }
/*]]>*///-->
</script>
</head>
<body>
<div id="content">
<h1 class="title">My Emacs Initialisation File, Written in <code>Org-mode</code></h1>
<h1> A Life Configuring Emacs </h1>
<h3> My Literate Setup </h3>
<p>
I enjoy reading others' <i>literate</i> configuration files and incorporating what I learn
into my own. The result is a sufficiently well-documented and accessible read that yields
a stylish and functional system (•̀ᴗ•́)و
</p>

<p>
The contents below could also be read in blog format
<a href="https://alhassy.github.io/init/">here</a>. Enjoy :smile:
</p>

<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#org30435f6">1. Abstract&#xa0;&#xa0;&#xa0;<span class="tag"><span class="ignore">ignore</span></span></a></li>
<li><a href="#org09fc239">2. Why Emacs?</a></li>
<li><a href="#org3646ebc">3. <span class="todo TODO">TODO</span> Booting Up</a>
<ul>
<li><a href="#org49a418a">3.1. <code>~/.emacs</code> vs. <code>init.org</code></a></li>
<li><a href="#org6317939">3.2. Elementary Version Control</a></li>
<li><a href="#orga5535eb">3.3. What's in, or at the top of, my <code>~/.emacs</code></a></li>
<li><a href="#orgdf58af4">3.4. <code>use-package</code> &#x2013;The start of <code>init.el</code></a></li>
<li><a href="#org1d30ff8">3.5. <code>magit</code> &#x2013;Emacs' porcelain interface to git</a></li>
<li><a href="#org810c497">3.6. Fix spelling as you type &#x2013;and a thesaurus too!</a></li>
<li><a href="#orga38b3bc">3.7. Unicode Input via Agda Input</a></li>
<li><a href="#org56f882e">3.8. <span class="todo TODO">TODO</span> Locally <code>toggle</code> a variable</a></li>
<li><a href="#orgee89ec7">3.9. <span class="todo TODO">TODO</span> Altering PATH</a></li>
<li><a href="#orgc3acbbb">3.10. Who am I?</a></li>
</ul>
</li>
<li><a href="#orgc8bcc8e">4. Cosmetics</a>
<ul>
<li><a href="#org23509c1">4.1. Startup message: Emacs &amp; Org versions</a></li>
<li><a href="#org212b90b">4.2. Spaceline: A sleek mode line</a></li>
<li><a href="#org44242f8">4.3. Mouse Editing Support</a></li>
<li><a href="#org2f6201b">4.4. Having a workspace manager in Emacs</a></li>
<li><a href="#org9dc0071">4.5. Flashing when something goes wrong</a></li>
<li><a href="#org14f5a41">4.6. My to-do list: The initial buffer when Emacs opens up</a></li>
<li><a href="#org728d828">4.7. Showing date, time, and battery life</a></li>
<li><a href="#org56e9863">4.8. Hiding Scrollbar, tool bar, and menu</a></li>
<li><a href="#orge78d6db">4.9. Increase/decrease text size and word wrapping</a></li>
<li><a href="#orgc60a02e">4.10. Delete Selection mode</a></li>
<li><a href="#org25b2774">4.11. Highlight &amp; complete parenthesis pair when cursor is near ;-</a></li>
<li><a href="#orgffa49ad">4.12. Minibuffer should display line and column numbers</a></li>
<li><a href="#orgc1e7ff8">4.13. Completion Frameworks</a></li>
<li><a href="#org7ed54bf">4.14. Neotree: Directory Tree Listing</a></li>
<li><a href="#orgf3cb3b0">4.15. Window resizing using the golden ratio&#xa0;&#xa0;&#xa0;<span class="tag"><span class="Disabled">Disabled</span></span></a></li>
<li><a href="#org6d025c5">4.16. Jump between windows using Cmd+Arrow</a></li>
</ul>
</li>
<li><a href="#org363e928">5. General Config, “Interior”&#xa0;&#xa0;&#xa0;<span class="tag"><span class="Bad_name">Bad_name</span></span></a>
<ul>
<li><a href="#orge32fe41">5.1. Backups</a></li>
</ul>
</li>
<li><a href="#org2caf379">6. Helpful Functions &amp; Shortcuts</a>
<ul>
<li><a href="#org440c857">6.1. Bind <code>recompile</code> to <code>C-c C-m</code> &#x2013; “m” for “m”ake</a></li>
<li><a href="#orga4573ad">6.2. Reload buffer with <code>f5</code></a></li>
<li><a href="#orgc03da51">6.3. Kill to start of line</a></li>
<li><a href="#org3211870">6.4. <code>file-as-list</code> and <code>file-as-string</code></a></li>
<li><a href="#org4014ba0">6.5. <code>kill-other-buffers</code></a></li>
<li><a href="#org9a59387">6.6. <code>create-scratch-buffer</code></a></li>
<li><a href="#orgb216a31">6.7. Switching from 2 horizontal windows to 2 vertical windows</a></li>
<li><a href="#orgaf8ad78">6.8. <code>re-replace-in-file</code></a></li>
<li><a href="#org2735a41">6.9. Obtaining Values of <code>#+KEYWORD</code> Annotations</a></li>
<li><a href="#orgb27c52f">6.10. Quickly pop-up a terminal, run a command, close it</a></li>
<li><a href="#orge0f5623">6.11. <code>C-x k</code> kills current buffer</a></li>
</ul>
</li>
<li><a href="#org582256d">7. Life within Org-mode</a>
<ul>
<li><a href="#org7df22ce">7.1. Org Speed Keys</a></li>
<li><a href="#org098ec75">7.2. Using org-mode as a Day Planner</a></li>
<li><a href="#WorkflowStates">7.3. Workflow States</a></li>
<li><a href="#orgd01d535">7.4. Clocking Work Time</a></li>
<li><a href="#org7bba67d">7.5. Coloured LaTeX using Minted</a></li>
<li><a href="#org66b0af1">7.6. Editing &amp; Special Key Handling</a></li>
<li><a href="#org3489f03">7.7. Executing code from <code>src</code> blocks</a></li>
<li><a href="#org70b330c">7.8. Hiding Emphasise Markers &amp; Inlining Images</a></li>
<li><a href="#orgeb33d31">7.9. Jumping without hassle</a></li>
<li><a href="#orgbb6f4be">7.10. Folding within a subtree</a></li>
<li><a href="#org7286c5e">7.11. Making then opening html's from org's</a></li>
<li><a href="#orge433dbe">7.12. Making then opening pdf's from org's</a></li>
<li><a href="#org839c7a4">7.13. Interpret the Haskell source blocks in a file</a></li>
</ul>
</li>
<li><a href="#org5bd4969">8. Summary of Utilities Provided</a></li>
</ul>
</div>
</div>

<div id="outline-container-org30435f6" class="outline-2">
<h2 id="org30435f6"><span class="section-number-2">1</span> Abstract&#xa0;&#xa0;&#xa0;<span class="tag"><span class="ignore">ignore</span></span></h2>
<div class="outline-text-2" id="text-1">
<div class="org-center">
<p>
<b>Abstract</b>
</p>
</div>

<p>
Herein I document the configurations I utilise with Emacs.
</p>

<p>
As a <a href="https://www.offerzen.com/blog/literate-programming-empower-your-writing-with-emacs-org-mode">literate program</a> file with <a href="http://orgmode.org/">Org-mode</a>, I am ensured optimal navigation
through my ever growing configuration files, ease of usability and reference
for peers, and, most importantly, better maintainability for myself!
</p>

<p>
Dear reader, when encountering a foregin command <code>X</code> I encourage you to execute <code>(describe-symbol 'X)</code>.
An elementary Elisp Cheat Sheet can be found <a href="https://github.com/alhassy/ElispCheatSheet">here.</a>
</p>
</div>
</div>

<div id="outline-container-org09fc239" class="outline-2">
<h2 id="org09fc239"><span class="section-number-2">2</span> Why Emacs?</h2>
<div class="outline-text-2" id="text-2">
<p>
<i>Emacs is a flexible platform for developing end-user applications</i> &#x2013;unfortunately it is generally perceived as
merely a text editor. Some people use it specifically for one or two applications.
</p>

<p>
For example, <a href="https://www.youtube.com/watch?v=FtieBc3KptU">writers</a> use it as an interface for Org-mode and others use it as an interface for version
control with Magit. <a href="https://orgmode.org/index.html#sec-4">Org</a> is an organisation tool that can be used for typesetting which subsumes LaTeX, generating many different
formats &#x2013;html, latex, pdf, etc&#x2013; from a single source, keeping track of <a href="https://orgmode.org/worg/org-tutorials/index.html#orgff7b885">schedules</a> &amp; task management, blogging, habit tracking, personal information management tool, and <a href="http://orgmode.org/worg/org-contrib/">much more</a>.
Moreover, its syntax is so <a href="https://karl-voit.at/2017/09/23/orgmode-as-markup-only/">natural</a> that most people use it without even knowing!
For me, Org allows me to do literate programming: I can program and document at the same time,
with no need to seperate the two tasks and with the ability to generate multiple formats and files from a single file.
</p>

<blockquote>
<p>
If you are a professional writer…Emacs outshines all other editing software 
in approximately the same way that the noonday sun does the stars. 
It is not just bigger and brighter; it simply makes everything else vanish.
—<a href="https://so.nwalsh.com/2019/03/01/emacs">Neal Stephenson</a>
</p>
</blockquote>

<p>
Of course Emacs comes with the basic features of a text editor, but it is much more;
for example, it comes with a powerful notion of ‘undo’: Basic text editors have a single stream of undo,
yet in Emacs, we have a tree &#x2013;when we undo and make new edits, we branch off in our editing stream
as if our text was being version controlled as we type! &#x2013;We can even switch between such branches!
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Allow tree-semantics for undo operations.</span>
<span style="color: #3a81c3;">(</span>package-install 'undo-tree<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>global-undo-tree-mode<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>diminish 'undo-tree-mode<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Execute (undo-tree-visualize) then navigate along the tree to witness</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">changes being made to your file live!</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Each node in the undo tree should have a timestamp.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> undo-tree-visualizer-timestamps t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Show a diff window displaying changes between undo nodes.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> undo-tree-visualizer-diff t<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
<i>Emacs is an extensible editor: You can make it into the editor of your dreams!</i>
You can make it suited to your personal needs.
If there's a feature you would like, a behaviour your desire, you can simply code that into Emacs with
a bit of Lisp. As a programming language enthusiast, for me Emacs is my default Lisp interpreter
and a customisable IDE that I use for other programming languages 
&#x2013;such as C, Haskell, Agda, Racket, and Prolog.
Moreover, being a Lisp interpreter, we can alter the look and feel of Emacs live, without having
to restart it &#x2013;e.g., press <code>C-x C-e</code> after the final parenthesis of <code>(scroll-bar-mode 0)</code>
to run the code that removes the scroll-bar.
</p>

<blockquote>
<p>
<i>I use Emacs every day. I rarely notice it. But when I do, it usually brings me joy.</i>
─<a href="https://so.nwalsh.com/2019/03/01/emacs">Norman Walsh</a>
</p>
</blockquote>

<p>
I have used Emacs as an interface for developing cheat sheets, for making my blog, and as an application 
for ‘interactively learning C’. If anything Emacs is more like an OS than just a text editor
&#x2013;“living within Emacs” provides an abstraction over whatever operating system my machine has:
<a href="https://www.fugue.co/blog/2015-11-11-guide-to-emacs.html">It's so easy to take everything with me.</a> Moreover, the desire to mould Emacs to my needs has made me
a better programmer: I am now a more literate programmer and, due to Elisp's documentation-oriented nature, I actually take the time
and effort to make meaningful documentation &#x2013;even when the project is private and will likely only be seen by me.
</p>

<blockquote>
<p>
<i>Seeing Emacs as an editor is like seeing a car as a seating-accommodation.</i> &#x2013; <a href="https://karl-voit.at/2015/10/23/Emacs-is-not-just-an-editor/">Karl Voit</a>
</p>
</blockquote>

<p>
Consider reading ⋯ 
</p>
<ul class="org-ul">
<li><a href="https://www.fugue.co/blog/2015-11-11-guide-to-emacs.html">A CEO's Guide to Emacs</a> &#x2013;A non-programmer introduction to Emacs (•̀ᴗ•́)و</li>
<li><a href="https://www.reddit.com/r/emacs/comments/6fytr5/when_did_you_start_using_emacs/">“When did you start using Emacs” discussion on Reddit</a></li>
<li>The <a href="http://tuhdo.github.io/emacs-tutor.html#orgheadline63">Emacs Mini Manual</a>, or</li>
<li><a href="https://david.rothlis.net/emacs/howtolearn.html">“How to Learn Emacs”</a></li>
<li><a href="https://orgmode.org/index.html#sec-4">The Org-mode Reference Manual</a> or <a href="https://orgmode.org/worg/">Worg: Community-Written Docs</a> which includes a <a href="https://orgmode.org/worg/org-tutorials/index.html">meta-tutorial</a>.</li>
</ul>
</div>
</div>

<div id="outline-container-org3646ebc" class="outline-2">
<h2 id="org3646ebc"><span class="section-number-2">3</span> <span class="todo TODO">TODO</span> Booting Up</h2>
<div class="outline-text-2" id="text-3">
</div>
<div id="outline-container-org49a418a" class="outline-3">
<h3 id="org49a418a"><span class="section-number-3">3.1</span> <code>~/.emacs</code> vs. <code>init.org</code></h3>
<div class="outline-text-3" id="text-3-1">
<p>
Why not keep Emac's configurations in the <code>~/.emacs</code> file?
This is because the Emacs system may explicitly add, or alter, code
in it.
</p>

<p>
For example, execute the following
</p>
<ol class="org-ol">
<li><code>M-x customize-variable RET line-number-mode RET</code></li>
<li>Then press: <code>toggle</code>, <code>state</code>, then <code>1</code>.</li>
<li>Now take a look: <code>(find-file "~/.emacs")</code></li>
</ol>
<p>
Notice how additions to the file have been created by `custom'.
</p>

<p>
As such, I've chosen to write my Emacs' initialisation configurations
in a file named <code>~/.emacs.d/init.org</code>: I have a literate configuration which
is then loaded using org-mode's tangling feature.
Read more about Emacs' initialisation configurations <a href="http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html#Init-File">here.</a>
</p>

<p>
Off topic, I love tiling window managers and had been using <a href="https://xmonad.org">xmonad</a>
until recently when I obtained a mac machine and now use 
<a href="https://ianyh.com/amethyst/">Amethyst</a> &#x2013; “Tiling window manager for macOS along the lines of xmonad.”
</p>
</div>
</div>

<div id="outline-container-org6317939" class="outline-3">
<h3 id="org6317939"><span class="section-number-3">3.2</span> Elementary Version Control</h3>
<div class="outline-text-3" id="text-3-2">
<p>
<a href="http://www.linfo.org/hard_link.html">Soft links</a> are pointers to other filenames, whereas hardlinks
are pointers to memory location of a given filename!
Soft links are preferable since they defer to the orginal filename
and can work across servers.
</p>

<p>
We can declare them as follows,
</p>
<pre class="example">
ln -s source_file myfile
</pre>

<p>
If <code>repo</code> refers to a directory under version control
&#x2013;or Dropbox&#x2013; we move our init file and emacs directory to it,
then make soft links to these locations so that whenever <code>~/.emacs</code> 
is accessed it will refer to <code>repo/.emacs</code> and likewise for <code>.emacs.d</code> :-)
</p>

<p>
On a new machine, copy-paste any existing emacs configs we want
to the <code>repo</code> folder then <code>rm -rf ~~/.emacs*</code> and then make the soft
links only.
</p>

<pre class="example">
repo=~/Dropbox     ## or my git repository: ~/dotfiles

cd ~

mv .emacs $repo/
ln -s $repo/.emacs .emacs

mv .emacs.elc $repo/
ln -s $repo/.emacs.elc .emacs.elc
          
mv .emacs.d/ $repo/
ln -s $repo/.emacs.d/ .emacs.d
</pre>
<p>
Note the extra <code>/</code> after <code>.emacs.d</code>!
</p>

<p>
You may need to unlink soft links if you already have them;
e.g., <code>unlink .emacs.d</code>.
</p>

<p>
To make another softlink to a file, say in a blogging directory,
we <code>cd</code> to the location of interest then execute, say:
<code>ln -s $repo/.emacs.d/init.org init.org</code>
</p>

<p>
While we're at it, let's make this super-duper file (and another) easily
accessible &#x2013;since we'll be altering it often&#x2013;:
</p>
<pre class="example">
cd ~

ln -s dotfiles/.emacs.d/init.org init.org
ln -s alhassy.github.io/content/AlBasmala.org AlBasmala.org
</pre>

<p>
Below I'll equip us with an Emacs ‘porcelain’ interface to git
&#x2013;it makes working with version control tremendously convenient.
Moreover, I add a little pop-up so that I don't forget to commit often!
</p>
</div>
</div>

<div id="outline-container-orga5535eb" class="outline-3">
<h3 id="orga5535eb"><span class="section-number-3">3.3</span> What's in, or at the top of, my <code>~/.emacs</code></h3>
<div class="outline-text-3" id="text-3-3">
<p>
We evaluate every piece of emacs-lisp code available here when
Emacs starts up by placing the following at the top of our <code>.emacs</code> file:
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">&#10218;March 7, 2019&#10219; For some reason, I need these here or my org-mode defaults to an older version.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">require</span> '<span style="color: #4e3163;">package</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'package-archives '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"org"</span> . <span style="color: #2d9574;">"http://orgmode.org/elpa/"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>package-initialize<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">require</span> '<span style="color: #4e3163;">org-tempo</span><span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span>org-babel-load-file <span style="color: #2d9574;">"~/.emacs.d/init.org"</span><span style="color: #3a81c3;">)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;;</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">My Emacs settings: (find-file "~/.emacs.d/init.org")</span>
</pre>
</div>
<p>
( I do not generate my <code>.emacs</code> file from this source code in-fear of
   overriding functionality inserted by <code>custom</code>. )
</p>

<p>
Our <code>.emacs</code> should be byte-compiled so that when we start Emacs it will
automatically determine if the <code>init.org</code> file has changed and if so it
would tangle it producing the <code>init.el</code> file which will then be loaded immediately.
</p>
</div>
</div>

<div id="outline-container-orgdf58af4" class="outline-3">
<h3 id="orgdf58af4"><span class="section-number-3">3.4</span> <code>use-package</code> &#x2013;The start of <code>init.el</code></h3>
<div class="outline-text-3" id="text-3-4">
<p>
There are a few ways to install packages
&#x2013;run <code>C-h C-e</code> for a short overview.
The easiest, for a beginner, is to use the command <code>package-list-packages</code>
then find the desired package, press <code>i</code> to mark it for installation, then
install all marked packages by pressing <code>x</code>.
</p>

<p>
Alternatively, one uses the declarative configuration tool <a href="https://github.com/jwiegley/use-package/">use-package</a> 
&#x2013;a meta-package that manages other packages and the way they interact.
</p>

<p>
Background:
Recently I switched to mac &#x2013;first time trying the OS.
I had to do a few <code>package-install</code>'s and it was annoying.
I'm looking for the best way to package my Emacs installation 
&#x2013;inlcuding my installed pacakages and configuration&#x2013;
so that I can quickly install it anywhere, say if I go to another machine.
It seems <code>use-package</code> allows me to configure and auto install packages. 
On a new machine, when I clone my <code>.emacs.d</code> and start emacs,
on the first start it should automatically install and compile 
all of my packages through <code>use-package</code> when it detects they're missing.
</p>

<p>
First we need the basic <code>package</code> module which not only allows us to obtain <code>use-package</code> but
acts as its kernel.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Make all commands of the &#8220;package&#8221; module present.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">require</span> '<span style="color: #4e3163;">package</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Speef up start up by not loading any packages at startup.</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(setq package-enable-at-startup nil)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Look at the *Messages* buffer before setting this to nil, then after.</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Internet repositories for new packages.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> package-archives '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">(</span><span style="color: #2d9574;">"org"</span>       . <span style="color: #2d9574;">"https://orgmode.org/elpa/"</span><span style="color: #2d9574;">)</span>
                         <span style="color: #2d9574;">(</span><span style="color: #2d9574;">"gnu"</span>       . <span style="color: #2d9574;">"https://elpa.gnu.org/packages/"</span><span style="color: #2d9574;">)</span>
                         <span style="color: #2d9574;">(</span><span style="color: #2d9574;">"melpa"</span>     . <span style="color: #2d9574;">"https://melpa.org/packages/"</span><span style="color: #2d9574;">)</span>
                         <span style="color: #2d9574;">(</span><span style="color: #2d9574;">"melpa-stable"</span> . <span style="color: #2d9574;">"https://stable.melpa.org/packages/"</span><span style="color: #2d9574;">)</span>     
                         <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Maintainer is AWOL.</span>
                         <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">("marmalade" . "https://marmalade-repo.org/packages/")</span>
                         <span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Actually get &#8220;package&#8221; to work.</span>
<span style="color: #3a81c3;">(</span>package-initialize<span style="color: #3a81c3;">)</span> 
</pre>
</div>

<p>
We can now:
</p>
<ul class="org-ul">
<li><code>M-x list-packages</code> to see all melpa packages that can install
<ul class="org-ul">
<li>Not in alphabetical order, so maybe search with <code>C-s</code>.</li>
</ul></li>
<li>For example to download the haskell mode: <code>M-x package-install RET haskell-mode RET</code>.
<ul class="org-ul">
<li>Or maybe to install <code>unicode-fonts</code> ;-)</li>
</ul></li>
<li>Read more at <a href="http://ergoemacs.org/emacs/emacs_package_system.html">http://ergoemacs.org/emacs/emacs_package_system.html</a> or 
at <a href="https://github.com/milkypostman/melpa">https://github.com/milkypostman/melpa</a></li>
</ul>

<p>
We now bootstrap <code>use-package</code>,
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Unless it's already installed, update the packages archives,</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">then install the most recent version of &#8220;use-package&#8221;.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">unless</span> <span style="color: #6c3163;">(</span>package-installed-p 'use-package<span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span>package-refresh-contents<span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span>package-install 'use-package<span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">require</span> '<span style="color: #4e3163;">use-package</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
We can now invoke <code>(use-package XYZ :ensure t)</code>
which should check for the <code>XYZ</code> package and make sure it is accessible. 
If not, the <code>:ensure t</code> part tells <code>use-package</code> to download it 
&#x2013;using <code>package.el</code>&#x2013;
and place it somewhere accessible, in <code>~/.emacs.d/elpa/</code> by default.
</p>

<p>
Here's an example use of <code>use-package</code>.
Below I have my “show recent files pop-up” command set to <code>C-x C-r</code>;
but what if I forget? This mode shows me all key completions when I type <code>C-x</code>, for example.
Moreover, I will be shown other commands I did not know about! Neato :-)
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Making it easier to discover Emacs key presses.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">which-key</span> 
 <span style="color: #3a81c3;">:ensure</span> t
 <span style="color: #3a81c3;">:diminish</span> which-key-mode
 <span style="color: #3a81c3;">:init</span> <span style="color: #6c3163;">(</span>which-key-mode<span style="color: #6c3163;">)</span>
 <span style="color: #3a81c3;">:config</span> <span style="color: #6c3163;">(</span>which-key-setup-side-window-bottom<span style="color: #6c3163;">)</span>
         <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> which-key-idle-delay 0.05<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
The <code>:diminish</code> keyword indicates that we do not want the mode's name to be
shown to us in the modeline &#x2013;the area near the bottom of Emacs.
It does so by using the <code>diminish</code> package, so let's install that.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">diminish</span>
  <span style="color: #3a81c3;">:ensure</span> t
<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Here are other packages that I want to be installed onto my machine.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(package-refresh-contents)     ;; Always stay up to date.</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Nice looking theme ^_^</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">this gives me an error for some reason</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(use-package spacemacs-theme :ensure t)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">&#8220;C-x&#8221; t to toggle between light and dark themes.</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">my/toggle-theme</span> <span style="color: #6c3163;">()</span> <span style="color: #da8b55;">"Toggle between dark and light themes."</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #6c3163;">)</span>
  <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Load dark if light is top-most enabled theme, else load light.</span>
  <span style="color: #6c3163;">(</span>load-theme 
  <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">if</span> <span style="color: #67b11d;">(</span>equal <span style="color: #b1951d;">(</span>car custom-enabled-themes<span style="color: #b1951d;">)</span> 'spacemacs-light<span style="color: #67b11d;">)</span> 
      'spacemacs-dark
      'spacemacs-light
  <span style="color: #2d9574;">)</span> <span style="color: #dc752f; background-color: #fbf8ef;">t </span><span style="color: #6c3163; background-color: #fbf8ef;">)</span>

  <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">The dark theme's modeline separator is ugly.</span>
  <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Keep reading below regarding &#8220;powerline&#8221;.</span>
  <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(setq powerline-default-separator 'arrow)</span>
  <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(spaceline-spacemacs-theme)</span>
<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>global-set-key <span style="color: #2d9574;">"\C-x\ t"</span> 'my/toggle-theme<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
The <a href="https://github.com/hlissner/emacs-doom-themes/tree/screenshots">Doom Themes</a> also look rather appealing.
A showcase of many themes can be found <a href="https://emacsthemes.com/">here</a>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Efficient version control.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">magit</span>
  <span style="color: #3a81c3;">:ensure</span> t
  <span style="color: #3a81c3;">:config</span> <span style="color: #6c3163;">(</span>global-set-key <span style="color: #2d9574;">(</span>kbd <span style="color: #2d9574;">"C-x g"</span><span style="color: #2d9574;">)</span> 'magit-status<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">htmlize</span> <span style="color: #3a81c3;">:ensure</span><span style="color: #3a81c3;">)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Main use: Org produced htmls are coloured.</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Can be used to export a file into a coloured html.</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">biblio</span>  <span style="color: #3a81c3;">:ensure</span><span style="color: #3a81c3;">)</span>     <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Quick BibTeX references, sometimes.</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Get org-headers to look pretty! E.g., * &#8594; &#8857;, ** &#8614; &#9711;, *** &#8614; &#9733;</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">https://github.com/emacsorphanage/org-bullets</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">org-bullets</span> <span style="color: #3a81c3;">:ensure</span> t<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-hook 'org-mode-hook 'org-bullets-mode<span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">haskell-mode</span> <span style="color: #3a81c3;">:ensure</span><span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">dash</span> <span style="color: #3a81c3;">:ensure</span><span style="color: #3a81c3;">)</span>    <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">&#8220;A modern list library for Emacs&#8221;</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">s</span>    <span style="color: #3a81c3;">:ensure</span><span style="color: #3a81c3;">)</span>    <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">&#8220;The long lost Emacs string manipulation library&#8221;.</span>
</pre>
</div>

<p>
Note:
</p>
<ul class="org-ul">
<li><a href="https://github.com/magnars/dash.el">dash</a>: “A modern list library for Emacs”
<ul class="org-ul">
<li>E.g., <code>(--filter (&gt; it 10) (list 8 9 10 11 12))</code></li>
</ul></li>
<li><a href="https://github.com/magnars/s.el">s</a>: “The long lost Emacs string manipulation library”.
<ul class="org-ul">
<li>E.g., <code>s-trim, s-replace, s-join</code>.</li>
</ul></li>
</ul>

<p>
Finally, since I've symlinked my <code>.emacs</code>:
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Don't ask for confirmation when opening symlinked files.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> vc-follow-symlinks t<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-org1d30ff8" class="outline-3">
<h3 id="org1d30ff8"><span class="section-number-3">3.5</span> <code>magit</code> &#x2013;Emacs' porcelain interface to git</h3>
<div class="outline-text-3" id="text-3-5">
<p>
Why use <code>magit</code> as the interface to the git version control system?
In a magit buffer nearly everything can be acted upon:
Press <code>return,</code> or <code>space</code>, to see details and <code>tab</code> to see children items, usually.
</p>

<p>
Below is my personal quick guide to working with magit.
A quick magit tutorial can be found on <a href="http://jr0cket.co.uk/2012/12/driving-git-with-emacs-pure-magic-with.html.html">jr0cket's blog</a>
</p>

<dl class="org-dl">
<dt><code>magit-init</code></dt><dd>Put a project under version control. 
The mini-buffer will prompt you for the top level folder version.
A <code>.git</code> folder will be created there.</dd>

<dt><code>magit-status</code> , <code>C-x g</code></dt><dd><p>
See status in another buffer. Press <code>?</code> to see options,
including:
</p>
<dl class="org-dl">
<dt><code>q</code></dt><dd>Quit magit, or go to previous magit screen.</dd>
<dt><code>s</code></dt><dd>Stage, i.e., add, a file to version control.
Add all untracked files by selecting the <i>Untracked files</i> title.</dd>
<dt><code>k</code></dt><dd>Kill, i.e., delete a file locally.</dd>
<dt><code>K</code></dt><dd>This' <code>(magit-file-untrack)</code> which does <code>git rm --cached</code>.</dd>
<dt><code>i</code></dt><dd>Add a file to the project <code>.gitignore</code> file. Nice stuff =)</dd>
<dt><code>u</code></dt><dd>Unstage a specfif staged change highlighed by cursor.
<code>C-u s</code> stages everything &#x2013;tracked or not.</dd>
<dt><code>c</code></dt><dd>Commit a change.
<ul class="org-ul">
<li>A new buffer for the commit message appears, you write it then
commit with <code>C-c C-c</code> or otherwise cancel with <code>C-c C-k</code>.
These commands are mentioned to you in the minibuffer when you go to commit.</li>
<li>You can provide a commit to <i>each</i> altered chunk of text! 
This is super neat, you make a series of local such commits rather
than one nebulous global commit for the file. The <code>magit</code> interface
makes this far more accessible than a standard terminal approach!</li>
<li>You can look at the unstaged changes, select a <i>region</i>, using <code>C-SPC</code> as usual, 
and commit only that if you want!</li>
<li>When looking over a commit, <code>M-p/n</code> to efficiently go to previous or next altered sections.</li>
<li>Amend a commit by pressing <code>a</code> on <code>HEAD</code>.</li>
</ul></dd>

<dt><code>d</code></dt><dd>Show differences, another <code>d</code> or another option.
<ul class="org-ul">
<li>This is magit! Each hunk can be acted upon; e.g., <code>s</code> or <code>c</code> or <code>k</code> ;-)</li>
<li><a href="https://softwareengineering.stackexchange.com/a/119807/185815">The staging area is akin to a pet store; commiting is taking the pet home.</a></li>
</ul></dd>
<dt><code>v</code></dt><dd>Revert a commit.</dd>
<dt><code>x</code></dt><dd>Undo last commit. Tantamount to <code>git reset HEAD~</code> when cursor is on most recent
commit; otherwise resets to whatever commit is under the cursor.</dd>
<dt><code>l</code></dt><dd>Show the log, another <code>l</code> for current branch; other options will be displayed.
<ul class="org-ul">
<li>Here <code>space</code> shows details in another buffer while cursour remains in current
buffer and, moreover, continuing to press <code>space</code> scrolls through the other buffer!
Neato.</li>
</ul></dd>
<dt><code>P</code></dt><dd>Push.</dd>
<dt><code>F</code></dt><dd>Pull.</dd>
<dt><code>:</code></dt><dd>Execute a raw git command; e.g., enter <code>whatchanged</code>.</dd>
</dl>

<p>
The status buffer may be refereshed using <code>g</code>, and all magit buffer by <code>G</code>.
</p>

<p>
Press <code>tab</code> to see collapsed items, such as what text has been changed.
</p></dd>
</dl>

<p>
Notice that every time you press one of these commands, a ‘pop-up’ of realted git options
appears! Thus not only is there no need to memorize many of them, but this approach makes
discovering other commands easier.
</p>

<p>
Use <code>M-x (magit-list-repositories) RET</code> to list local repositories:
</p>

<p>
Below are the git repos I'd like to clone
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Do not ask about this variable when cloning.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> magit-clone-set-remote.pushDefault t<span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">maybe-clone</span> <span style="color: #6c3163;">(</span>remote local<span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"Clone a &#8216;</span><span style="color: #4e3163;">remote</span><span style="color: #da8b55;">&#8217; repository if the &#8216;</span><span style="color: #4e3163;">local</span><span style="color: #da8b55;">&#8217; directory does not exist.</span>
<span style="color: #da8b55;">    Yields &#8216;</span><span style="color: #4e3163;">nil</span><span style="color: #da8b55;">&#8217; when no cloning transpires, otherwise yields &#8220;cloned-repo&#8221;.</span>
<span style="color: #da8b55;">  "</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">unless</span> <span style="color: #2d9574;">(</span>file-directory-p local<span style="color: #2d9574;">)</span> 
     <span style="color: #2d9574;">(</span>magit-clone remote local<span style="color: #2d9574;">)</span> 
     <span style="color: #2d9574;">(</span>add-to-list 'magit-repository-directories `<span style="color: #67b11d;">(</span>,local   . 0<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
     'cloned-repo<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Set variable without asking.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> magit-clone-set-remote.pushDefault 't<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Public repos</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://github.com/alhassy/dotfiles"</span> <span style="color: #2d9574;">"~/dotfiles"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://github.com/alhassy/alhassy.github.io"</span> <span style="color: #2d9574;">"~/alhassy.github.io"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://github.com/alhassy/CheatSheet"</span> <span style="color: #2d9574;">"~/CheatSheet"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://github.com/alhassy/ElispCheatSheet"</span> <span style="color: #2d9574;">"~/ElispCheatSheet"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://github.com/alhassy/MyUnicodeSymbols"</span> <span style="color: #2d9574;">"~/MyUnicodeSymbols"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://github.com/alhassy/interactive-way-to-c"</span> <span style="color: #2d9574;">"~/interactive-way-to-c"</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Private repos</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(maybe-clone "https://gitlab.cas.mcmaster.ca/carette/cs3fp3.git" "~/3fp3") ;; cat adventures</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(maybe-clone "https://gitlab.cas.mcmaster.ca/RATH/RATH-Agda"     "~/RATH-Agda")</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://gitlab.cas.mcmaster.ca/3ea3-winter2019/assignment-distribution.git"</span> <span style="color: #2d9574;">"~/3ea3/assignment-distribution"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://gitlab.cas.mcmaster.ca/3ea3-winter2019/notes.git"</span> <span style="color: #2d9574;">"~/3ea3/notes"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://gitlab.cas.mcmaster.ca/3ea3-winter2019/assignment-development.git"</span> <span style="color: #2d9574;">"~/3ea3/assignment-development"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://gitlab.cas.mcmaster.ca/3ea3-winter2019/kandeeps.git"</span> <span style="color: #2d9574;">"~/3ea3/sujan"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://gitlab.cas.mcmaster.ca/3ea3-winter2019/horsmane.git"</span> <span style="color: #2d9574;">"~/3ea3/emily"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>maybe-clone <span style="color: #2d9574;">"https://gitlab.cas.mcmaster.ca/3ea3-winter2019/anderj12.git"</span> <span style="color: #2d9574;">"~/3ea3/jacob"</span><span style="color: #3a81c3;">)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(maybe-clone "https://gitlab.cas.mcmaster.ca/alhassm/3EA3.git" "~/3ea3/_2018")  </span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(maybe-clone "https://gitlab.cas.mcmaster.ca/2DM3/LectureNotes.git" "~/2dm3")</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Likely want to put a hook when closing emacs, or at some given time,</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">to show me this buffer so that I can &#8216;</span><span style="color: #4e3163; background-color: #ecf3ec;">push</span><span style="color: #2aa1ae; background-color: #ecf3ec;">&#8217; if I haven't already!</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(magit-list-repositories)</span>
</pre>
</div>

<p>
Let's always notify ourselves of a file that has <a href="https://tpapp.github.io/post/check-uncommitted/">uncommited changes</a>
&#x2013;we might have had to step away from the computer and forgotten to commit.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">require</span> '<span style="color: #4e3163;">magit-git</span><span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">my/magit-check-file-and-popup</span> <span style="color: #6c3163;">()</span>
  <span style="color: #da8b55;">"If the file is version controlled with git </span>
<span style="color: #da8b55;">  and has uncommitted changes, open the magit status popup."</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">let</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>file <span style="color: #b1951d;">(</span>buffer-file-name<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
    <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">when</span> <span style="color: #67b11d;">(</span><span style="color: #3a81c3; font-weight: bold;">and</span> file <span style="color: #b1951d;">(</span>magit-anything-modified-p t file<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span>
      <span style="color: #67b11d;">(</span>message <span style="color: #2d9574;">"This file has uncommited changes!"</span><span style="color: #67b11d;">)</span>
      <span style="color: #67b11d;">(</span><span style="color: #3a81c3; font-weight: bold;">when</span> nil <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Became annyoying after some time.</span>
      <span style="color: #b1951d;">(</span>split-window-below<span style="color: #b1951d;">)</span>
      <span style="color: #b1951d;">(</span>other-window 1<span style="color: #b1951d;">)</span>
      <span style="color: #b1951d;">(</span>magit-status<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">I usually have local variables, so I want the message to show</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">after the locals have been loaded.</span>
<span style="color: #3a81c3;">(</span>add-hook 'find-file-hook 
  '<span style="color: #6c3163;">(</span>lambda <span style="color: #2d9574;">()</span> 
      <span style="color: #2d9574;">(</span>add-hook 'hack-local-variables-hook 'my/magit-check-file-and-popup<span style="color: #2d9574;">)</span>
   <span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>
<p>
Let's try this out:
</p>
<pre class="example">
(progn (eshell-command "echo change-here &gt;&gt; ~/dotfiles/.emacs")
       (find-file "~/dotfiles/.emacs")
)
</pre>

<p>
In doubt, execute <code>C-h e</code> to jump to the <code>*Messages*</code> buffer.
</p>
</div>
</div>

<div id="outline-container-org810c497" class="outline-3">
<h3 id="org810c497"><span class="section-number-3">3.6</span> Fix spelling as you type &#x2013;and a thesaurus too!</h3>
<div class="outline-text-3" id="text-3-6">
<p>
I would like to check spelling by default.
</p>
<dl class="org-dl">
<dt><code>C-;</code></dt><dd>Cycle through corrections for word at point.</dd>
<dt><code>M-$</code></dt><dd>Check and correct spelling of the word at point</dd>
<dt><code>M-x ispell-change-dictionary RET TAB</code></dt><dd>To see what dictionaries are available.</dd>
</dl>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">flyspell</span>
  <span style="color: #3a81c3;">:hook</span> <span style="color: #6c3163;">(</span>
           <span style="color: #2d9574;">(</span>prog-mode . flyspell-prog-mode<span style="color: #2d9574;">)</span>
           <span style="color: #2d9574;">(</span>text-mode . flyspell-mode<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Flyspell needs a spell checking tool, which is not included in Emacs. 
We install <code>aspell</code> spell checker using, say, homebrew via <code>brew install aspell</code>.
Note that Emacs' <code>ispell</code> is the interface to such a command line spelling utility.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> ispell-program-name <span style="color: #2d9574;">"/usr/local/bin/aspell"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> ispell-dictionary <span style="color: #2d9574;">"en_GB"</span><span style="color: #3a81c3;">)</span> <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">set the default dictionary</span>

<span style="color: #3a81c3;">(</span>diminish 'flyspell-mode<span style="color: #3a81c3;">)</span> <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Don't show it in the modeline.</span>
</pre>
</div>

<p>
Ecnabling fly-spell for text-mode enables it for org and latex modes since they
derive from text-mode.
</p>

<p>
Let us select a correct spelling merely by clicking on a word.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>eval-after-load <span style="color: #2d9574;">"flyspell"</span>
  ' <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">progn</span>
     <span style="color: #2d9574;">(</span>define-key flyspell-mouse-map <span style="color: #67b11d;">[</span>down-mouse-3<span style="color: #67b11d;">]</span> #'flyspell-correct-word<span style="color: #2d9574;">)</span>
     <span style="color: #2d9574;">(</span>define-key flyspell-mouse-map <span style="color: #67b11d;">[</span>mouse-3<span style="color: #67b11d;">]</span> #'undefined<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Colour incorrect works; default is an underline.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>global-font-lock-mode t<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>custom-set-faces '<span style="color: #6c3163;">(</span>flyspell-incorrect <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>t <span style="color: #b1951d;">(</span><span style="color: #3a81c3;">:inverse-video</span> t<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Finally, save to user dictionary without asking:
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> ispell-silently-savep t<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Nowadays, I very rarely write non-literate programs, but if I do
I'd like to check spelling only in comments/strings. E.g.,
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>add-hook          'c-mode-hook 'flyspell-prog-mode<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Use the thesaurus Emacs frontend <a href="https://github.com/hpdeifel/synosaurus">Synosaurus</a> to avoid unwarranted repetition.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">synosaurus</span> 
  <span style="color: #3a81c3;">:ensure</span> t
  <span style="color: #3a81c3;">:diminish</span> synosaurus-mode
  <span style="color: #3a81c3;">:init</span>    <span style="color: #6c3163;">(</span>synosaurus-mode<span style="color: #6c3163;">)</span>
  <span style="color: #3a81c3;">:config</span>  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> synosaurus-choose-method 'popup<span style="color: #6c3163;">)</span> <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">'ido is default.</span>
           <span style="color: #6c3163;">(</span>global-set-key <span style="color: #2d9574;">(</span>kbd <span style="color: #2d9574;">"M-#"</span><span style="color: #2d9574;">)</span> 'synosaurus-choose-and-replace<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>
<p>
The thesaurus is powered by the Wordnet <code>wn</code> tool, which can be invoked without an internet connection!
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(shell-command "brew cask install xquartz &amp;") ;; Dependency</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(shell-command "brew install wordnet &amp;")</span>
</pre>
</div>

<p>
Use this game to help you learn to spell words that you're having trouble with;
see <code>~/Dropbox/spelling.txt</code>.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>autoload 'typing-of-emacs <span style="color: #2d9574;">"~/.emacs.d/typing.el"</span> <span style="color: #da8b55;">"The Typing Of Emacs, a game."</span> t<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Practice touch typing using <a href="https://github.com/hagleitn/speed-type">speed-type</a>.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">speed-type</span> <span style="color: #3a81c3;">:ensure</span> t<span style="color: #3a81c3;">)</span>
</pre>
</div>
<p>
Running <code>M-x speed-type-region</code> on a region of text, or <code>M-x speed-type-buffer</code> on a 
whole buffer, or just <code>M-x speed-type-text</code> will produce the selected region, buffer,
or random text for practice. The timer begins when the first key is pressed
and stats are shown when the last letter is entered.
</p>
</div>
</div>

<div id="outline-container-orga38b3bc" class="outline-3">
<h3 id="orga38b3bc"><span class="section-number-3">3.7</span> Unicode Input via Agda Input</h3>
<div class="outline-text-3" id="text-3-7">
<p>
<a href="https://mazzo.li/posts/AgdaSort.html">Agda</a> is one of my favourite languages, it's like Haskell on steroids.
Let's set it up.
</p>

<p>
Executing <code>agda-mode setup</code> appends the following text to the <code>.emacs</code> file.
Let's put it here ourselves.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>load-file <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">let</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>coding-system-for-read 'utf-8<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
                <span style="color: #2d9574;">(</span>shell-command-to-string <span style="color: #2d9574;">"/usr/local/bin/agda-mode locate"</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
I almost always want the <code>agda-mode</code> input method.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">require</span> '<span style="color: #4e3163;">agda-input</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-hook 'text-mode-hook <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">lambda</span> <span style="color: #2d9574;">()</span> <span style="color: #2d9574;">(</span>set-input-method <span style="color: #2d9574;">"Agda"</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-hook 'org-mode-hook <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">lambda</span> <span style="color: #2d9574;">()</span> <span style="color: #2d9574;">(</span>set-input-method <span style="color: #2d9574;">"Agda"</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Below are my personal Agda input symbol translations;
e.g., <code>\set → 𝒮ℯ𝓉</code>. Note that we could give a symbol new Agda TeX binding
interactively: <code>M-x customize-variable agda-input-user-translations</code> then
<code>INS</code> then for key sequence type <code>set</code> then <code>INS</code> and for string paste <code>𝒮ℯ𝓉</code>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">category theory</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"set"</span> <span style="color: #2d9574;">"&#119982;&#8495;&#120009;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"alg"</span> <span style="color: #2d9574;">"&#119964;&#120001;&#8458;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"split"</span> <span style="color: #2d9574;">"&#9653;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"join"</span> <span style="color: #2d9574;">"&#9663;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"adj"</span> <span style="color: #2d9574;">"&#8867;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">";;"</span> <span style="color: #2d9574;">"&#65108;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">";;"</span> <span style="color: #2d9574;">"&#10814;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">";;"</span> <span style="color: #2d9574;">"&#8728;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">lattices</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"meet"</span> <span style="color: #2d9574;">"&#8851;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"join"</span> <span style="color: #2d9574;">"&#8852;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">residuals</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"syq"</span>  <span style="color: #2d9574;">"&#9587;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"over"</span> <span style="color: #2d9574;">"&#9585;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"under"</span> <span style="color: #2d9574;">"&#9586;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
        <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Maybe &#8220;\\&#8221; shortcut?</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Z-quantification range notation, e.g., &#8220;&#8704; x &#10073; R &#8226; P&#8221;</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"|"</span> <span style="color: #2d9574;">"&#10073;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">adjunction isomorphism pair</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"floor"</span>  <span style="color: #2d9574;">"&#8970;&#8971;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"lower"</span>  <span style="color: #2d9574;">"&#8970;&#8971;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"lad"</span>    <span style="color: #2d9574;">"&#8970;&#8971;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"ceil"</span>   <span style="color: #2d9574;">"&#8968;&#8969;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"raise"</span>  <span style="color: #2d9574;">"&#8968;&#8969;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"rad"</span>    <span style="color: #2d9574;">"&#8968;&#8969;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">silly stuff</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;;</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">angry, cry, why-you-no</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations 
   '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"whyme"</span> <span style="color: #2d9574;">"&#4314;(&#3232;&#30410;&#3232;)&#4314;"</span> <span style="color: #2d9574;">"&#12541;&#3900;&#3234;_&#3234;&#3901;&#65417;&#9730;"</span> <span style="color: #2d9574;">"&#1097;(&#12444;&#12525;&#12444;&#1097;)"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span> 
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">confused, disapprove, dead, shrug</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations 
   '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"what"</span> <span style="color: #2d9574;">"&#12300;(&#176;&#12504;&#176;)"</span> <span style="color: #2d9574;">"(&#3232;_&#3232;)"</span> <span style="color: #2d9574;">"(&#10006;&#9581;&#9582;&#10006;)"</span> <span style="color: #2d9574;">"&#175;\\_(&#12484;)_/&#175;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">dance, csi</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations 
   '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"cool"</span> <span style="color: #2d9574;">"&#9487;(-_-)&#9491;&#9487;(-_-)&#9499;&#9495;(-_-&#65279; )&#9491;"</span> <span style="color: #2d9574;">"&#8226;_&#8226;)</span>
<span style="color: #2d9574;">( &#8226;_&#8226;)&gt;&#8976;&#9632;-&#9632;</span>
<span style="color: #2d9574;">(&#8976;&#9632;_&#9632;)</span>
<span style="color: #2d9574;">"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">love, pleased, success, yesss</span>
<span style="color: #3a81c3;">(</span>add-to-list 'agda-input-user-translations 
   '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"smile"</span> <span style="color: #2d9574;">"&#9829;&#8255;&#9829;"</span> <span style="color: #2d9574;">"(&#9472;&#8255;&#8255;&#9472;)"</span> <span style="color: #2d9574;">"(&#8226;&#768;&#7447;&#8226;&#769;)&#1608;"</span> <span style="color: #2d9574;">"(&#3591;&#3232;_&#3232;)&#3591;"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Finally let's effect such translations.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">activate translations</span>
<span style="color: #3a81c3;">(</span>agda-input-setup<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Note that the effect of <a href="http://ergoemacs.org/emacs/emacs_n_unicode.html">Emacs unicode input</a> could be approximated using
<code>abbrev-mode</code>.
</p>
</div>
</div>

<div id="outline-container-org56f882e" class="outline-3">
<h3 id="org56f882e"><span class="section-number-3">3.8</span> <span class="todo TODO">TODO</span> Locally <code>toggle</code> a variable</h3>
<div class="outline-text-3" id="text-3-8">
<p>
<b>todo</b> body of toggle should be a progn?
</p>

<p>
It is dangerous to load a file with local variables;
instead we should load files without evaluating locals,
read the locals to ensure they are safe &#x2013;e.g., there's nothing
malicious like <code>eval: (delete-file your-important-file.txt)</code>&#x2013;
then revert the buffer to load the locals.
</p>

<p>
However, when preprocessing my own files I sometimes wish
to accept all locals without being queried and so have the following
combinator.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defmacro</span> <span style="color: #6c3163; font-weight: bold;">toggle</span> <span style="color: #6c3163;">(</span>variable value code<span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"Locally set the value of &#8216;</span><span style="color: #4e3163;">variable</span><span style="color: #da8b55;">&#8217; to be &#8216;</span><span style="color: #4e3163;">value</span><span style="color: #da8b55;">&#8217; in the scope of &#8216;</span><span style="color: #4e3163;">code</span><span style="color: #da8b55;">&#8217;.</span>
<span style="color: #da8b55;">   In particular, the value of &#8216;</span><span style="color: #4e3163;">variable</span><span style="color: #da8b55;">&#8217;, if any, *is* affected</span>
<span style="color: #da8b55;">   to produce useful sideffects. It retains its orginal value outside this call.</span>

<span style="color: #da8b55;">   Example uses include terse replacements for one-off let-statements,</span>
<span style="color: #da8b55;">   or, more likely, of temporarily toggeling important values, such as </span>
<span style="color: #da8b55;">   &#8216;</span><span style="color: #4e3163;">kill-buffer-query-functions</span><span style="color: #da8b55;">&#8217; for killing a process buffer without confirmation.</span>

<span style="color: #da8b55;">   Another example: &#8216;(toggle enable-local-variables :all &#8943;)&#8217; to preprocess files</span>
<span style="color: #da8b55;">   without being queried about possibly dangerous local variables.</span>
<span style="color: #da8b55;">  "</span>
  `<span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">let</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>_initial_value_ ,variable<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
        <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> ,variable ,value<span style="color: #2d9574;">)</span>
        ,code
        <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> ,variable _initial_value_<span style="color: #2d9574;">)</span>
  <span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Since emacs-lisp interprets definitions sequentially, I define <code>toggle</code> here
since I employ it in the next section.
</p>
</div>
</div>

<div id="outline-container-orgee89ec7" class="outline-3">
<h3 id="orgee89ec7"><span class="section-number-3">3.9</span> <span class="todo TODO">TODO</span> Altering PATH</h3>
<div class="outline-text-3" id="text-3-9">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">https://emacs.stackexchange.com/questions/4090/org-mode-cannot-find-pdflatex-using-mac-os</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">set-exec-path-from-shell-PATH</span> <span style="color: #6c3163;">()</span>
  <span style="color: #da8b55;">"Sets the exec-path to the same value used by the user shell"</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">let</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>path-from-shell
         <span style="color: #b1951d;">(</span>replace-regexp-in-string
          <span style="color: #2d9574;">"[[:space:]\n]*$"</span> <span style="color: #2d9574;">""</span>
          <span style="color: #3a81c3;">(</span>shell-command-to-string <span style="color: #2d9574;">"$SHELL -l -c 'echo $PATH'"</span><span style="color: #3a81c3;">)</span><span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
    <span style="color: #2d9574;">(</span>setenv <span style="color: #2d9574;">"PATH"</span> path-from-shell<span style="color: #2d9574;">)</span>
    <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> exec-path <span style="color: #67b11d;">(</span>split-string path-from-shell path-separator<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">call function now</span>
<span style="color: #3a81c3;">(</span>set-exec-path-from-shell-PATH<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-orgc3acbbb" class="outline-3">
<h3 id="orgc3acbbb"><span class="section-number-3">3.10</span> Who am I?</h3>
<div class="outline-text-3" id="text-3-10">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> user-full-name    <span style="color: #2d9574;">"Musa Al-hassy"</span>
      user-mail-address <span style="color: #2d9574;">"alhassy@gmail.com"</span><span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>
</div>
<div id="outline-container-orgc8bcc8e" class="outline-2">
<h2 id="orgc8bcc8e"><span class="section-number-2">4</span> Cosmetics</h2>
<div class="outline-text-2" id="text-4">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Make it very easy to see the line with the cursor.</span>
<span style="color: #3a81c3;">(</span>global-hl-line-mode t<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>

<div id="outline-container-org23509c1" class="outline-3">
<h3 id="org23509c1"><span class="section-number-3">4.1</span> Startup message: Emacs &amp; Org versions</h3>
<div class="outline-text-3" id="text-4-1">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Silence the usual message: Get more info using the about page via C-h C-a.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> inhibit-startup-message t<span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">display-startup-echo-area-message</span> <span style="color: #6c3163;">()</span>
  <span style="color: #6c3163;">(</span>message
      <span style="color: #2d9574;">(</span>concat <span style="color: #2d9574;">"Welcome "</span>      user-full-name
              <span style="color: #2d9574;">"! Emacs "</span>      emacs-version
              <span style="color: #2d9574;">"; Org-mode "</span>   org-version
              <span style="color: #2d9574;">"; System "</span>    <span style="color: #67b11d;">(</span>system-name<span style="color: #67b11d;">)</span>
      <span style="color: #2d9574;">)</span>
  <span style="color: #6c3163;">)</span>     
<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(setq initial-scratch-message "Welcome! This&#8217; the scratch buffer" )</span>
</pre>
</div>
<p>
Now my startup message is,
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Welcome Musa Al-hassy! Emacs 26.1; Org-mode 9.2.2; System alhassy-air.local</span>
</pre>
</div>

<p>
For some fun, run this cute method.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>animate-birthday-present user-full-name<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Moreover, since I end up using org-mode most of the time, let's make that the default mode.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> initial-major-mode 'org-mode<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-org212b90b" class="outline-3">
<h3 id="org212b90b"><span class="section-number-3">4.2</span> Spaceline: A sleek mode line</h3>
<div class="outline-text-3" id="text-4-2">
<p>
I may not use spacemacs, since I do not like evil-mode and find spacemacs 
to “hide things” from me &#x2013;whereas Emacs “”encourages” me to learn more&#x2013;,
however it is a configuration and I enjoy reading Emacs configs in order to 
improve my own setup. From Spacemacs I've adopted Helm for list completion,
its sleek light &amp; dark themes, and its modified powerline setup.
</p>

<p>
The ‘modeline’ is a part near the bottom of Emacs that gives information
about the current mode, as well as other matters &#x2013;such as time &amp; date, for example.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">spaceline</span>
  <span style="color: #3a81c3;">:ensure</span> t
  <span style="color: #3a81c3;">:config</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">require</span> '<span style="color: #4e3163;">spaceline-config</span><span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> spaceline-buffer-encoding-abbrev-p nil<span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> spaceline-line-column-p nil<span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> spaceline-line-p nil<span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> powerline-default-separator 'arrow<span style="color: #6c3163;">)</span>
  <span style="color: #3a81c3;">:init</span>
 <span style="color: #6c3163;">(</span>spaceline-helm-mode<span style="color: #6c3163;">)</span> <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">When using helm, mode line looks prettier.</span>
 <span style="color: #6c3163;">(</span>spaceline-spacemacs-theme<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>
<p>
Other separators I've considered include <code>'brace</code> instead of an arrow,
and <code>'contour, 'chamfer, 'wave, 'zigzag</code> which look like browser tabs
that are curved, boxed, wavy, or in the style of driftwood.
</p>
</div>
</div>

<div id="outline-container-org44242f8" class="outline-3">
<h3 id="org44242f8"><span class="section-number-3">4.3</span> Mouse Editing Support</h3>
<div class="outline-text-3" id="text-4-3">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Text selected with the mouse is automatically copied to clipboard.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> mouse-drag-copy-region t<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-org2f6201b" class="outline-3">
<h3 id="org2f6201b"><span class="section-number-3">4.4</span> Having a workspace manager in Emacs</h3>
<div class="outline-text-3" id="text-4-4">
<p>
I've loved using XMonad as a window tiling manager.
I've enjoyed the ability to segregate my tasks
according to what ‘project’ I'm working on;
such as research, marking, Emacs play, etc.
With <a href="https://github.com/nex3/perspective-el">perspective</a>, I can do the same thing :-)
</p>

<p>
That is, I can have a million buffers, but only those
that belong to a workspace will be visible when I'm switching between buffers, for example.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">perspective</span> <span style="color: #3a81c3;">:ensure</span> t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Activate it.</span>
<span style="color: #3a81c3;">(</span>persp-mode<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">In the modeline, tell me which workspace I'm in.</span>
<span style="color: #3a81c3;">(</span>persp-turn-on-modestring<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
All commands are prefixed by <code>C-x x</code>; main commands:
</p>
<dl class="org-dl">
<dt><code>s, n/→, p/←</code></dt><dd>‘S’elect a workspace to go to or create it, or go to ‘n’ext one, or go to ‘p’revious one.</dd>
<dt><code>c</code></dt><dd>Query a perspective to kill.</dd>
<dt><code>r</code></dt><dd>Rename a perspective.</dd>
<dt><code>A</code></dt><dd>Add buffer to current perspective &amp; remove it from all others.</dd>
</dl>

<p>
As always, since we've installed <code>which-key</code>, it suffices to press
<code>C-x x</code> then look at the resulting menu 😃
</p>
</div>
</div>

<div id="outline-container-org9dc0071" class="outline-3">
<h3 id="org9dc0071"><span class="section-number-3">4.5</span> Flashing when something goes wrong</h3>
<div class="outline-text-3" id="text-4-5">
<p>
Make top and bottom of screen flash when something unexpected happens thereby observing a warning message in the minibuffer. E.g., C-g, or calling an unbound key sequence, or misspelling a word.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> visible-bell 1<span style="color: #3a81c3;">)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Enable flashing mode-line on errors</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">On MacOS, this shows a caution symbol ^_^</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-org14f5a41" class="outline-3">
<h3 id="org14f5a41"><span class="section-number-3">4.6</span> My to-do list: The initial buffer when Emacs opens up</h3>
<div class="outline-text-3" id="text-4-6">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>find-file <span style="color: #2d9574;">"~/Dropbox/todo.org"</span><span style="color: #3a81c3;">)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(setq initial-buffer-choice "~/Dropbox/todo.org")</span>

<span style="color: #3a81c3;">(</span>split-window-right<span style="color: #3a81c3;">)</span>                      <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x 3</span>
<span style="color: #3a81c3;">(</span>other-window 1<span style="color: #3a81c3;">)</span>                                  <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x 0</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">toggle</span> enable-local-variables 'all           <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Load *all* locals.</span>
  <span style="color: #6c3163;">(</span>find-file <span style="color: #2d9574;">"~/.emacs.d/init.org"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
(describe-symbol 'enable-local-variables)
</p>
</div>
</div>
<div id="outline-container-org728d828" class="outline-3">
<h3 id="org728d828"><span class="section-number-3">4.7</span> Showing date, time, and battery life</h3>
<div class="outline-text-3" id="text-4-7">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> display-time-day-and-date t<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>display-time<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(display-battery-mode 1)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Nope; let's use a fancy indicator &#8230; </span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">fancy-battery</span>
  <span style="color: #3a81c3;">:diminish</span>
  <span style="color: #3a81c3;">:ensure</span> t
  <span style="color: #3a81c3;">:config</span>
    <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> fancy-battery-show-percentage t<span style="color: #6c3163;">)</span>
    <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> battery-update-interval 15<span style="color: #6c3163;">)</span>
    <span style="color: #6c3163;">(</span>fancy-battery-mode<span style="color: #6c3163;">)</span>
    <span style="color: #6c3163;">(</span>display-battery-mode<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
This will show remaining battery life, coloured green if charging
and coloured yellow otherwise. It is important to note that
this package is no longer maintained. It works on my machine.
</p>
</div>
</div>

<div id="outline-container-org56e9863" class="outline-3">
<h3 id="org56e9863"><span class="section-number-3">4.8</span> Hiding Scrollbar, tool bar, and menu</h3>
<div class="outline-text-3" id="text-4-8">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>tool-bar-mode -1<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>scroll-bar-mode -1<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>menu-bar-mode -1<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-orge78d6db" class="outline-3">
<h3 id="orge78d6db"><span class="section-number-3">4.9</span> Increase/decrease text size and word wrapping</h3>
<div class="outline-text-3" id="text-4-9">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>global-set-key <span style="color: #6c3163;">(</span>kbd <span style="color: #2d9574;">"C-+"</span><span style="color: #6c3163;">)</span> 'text-scale-increase<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>global-set-key <span style="color: #6c3163;">(</span>kbd <span style="color: #2d9574;">"C--"</span><span style="color: #6c3163;">)</span> 'text-scale-decrease<span style="color: #3a81c3;">)</span>
  <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x C-0 restores the default font size</span>

<span style="color: #3a81c3;">(</span>add-hook 'text-mode-hook
            '<span style="color: #6c3163;">(</span>lambda <span style="color: #2d9574;">()</span>
               <span style="color: #2d9574;">(</span>visual-line-mode 1<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-orgc60a02e" class="outline-3">
<h3 id="orgc60a02e"><span class="section-number-3">4.10</span> Delete Selection mode</h3>
<div class="outline-text-3" id="text-4-10">
<p>
Delete Selection mode lets you treat an Emacs region much like a typical text
selection outside of Emacs: You can replace the active region.
We can delete selected text just by hitting the backspace key.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>delete-selection-mode 1<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-org25b2774" class="outline-3">
<h3 id="org25b2774"><span class="section-number-3">4.11</span> Highlight &amp; complete parenthesis pair when cursor is near ;-</h3>
<div class="outline-text-3" id="text-4-11">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Highlight expression within matching parens when near one of them.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> show-paren-delay 0<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> blink-matching-paren nil<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> show-paren-style 'expression<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>show-paren-mode<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Colour parens, and other delimiters, depending on their depth.</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Very useful for parens heavy languages like Lisp.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">rainbow-delimiters</span>
  <span style="color: #3a81c3;">:ensure</span> t
<span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span>add-hook 'org-mode-hook
  '<span style="color: #6c3163;">(</span>lambda <span style="color: #2d9574;">()</span> <span style="color: #2d9574;">(</span>rainbow-delimiters-mode 1<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-hook 'prog-mode-hook
  '<span style="color: #6c3163;">(</span>lambda <span style="color: #2d9574;">()</span> <span style="color: #2d9574;">(</span>rainbow-delimiters-mode 1<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
For example,
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>blue <span style="color: #6c3163;">(</span>purple <span style="color: #2d9574;">(</span>forest <span style="color: #67b11d;">(</span>green <span style="color: #b1951d;">(</span>yellow <span style="color: #3a81c3;">(</span>blue<span style="color: #3a81c3;">)</span><span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
There is a powerful package called ‘smartparens’ for working with pair-able
characters, but I've found it to be too much for my uses. Instead I'll utilise
the lightweight package <code>electric</code>, which provided by Emacs out of the box.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>electric-pair-mode 1<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
It supports, by default, ACSI pairs <code>{}, [], ()</code> and Unicode <code>‘’, “”, ⟪⟫, ⟨⟩</code>.
</p>
</div>
</div>

<div id="outline-container-orgffa49ad" class="outline-3">
<h3 id="orgffa49ad"><span class="section-number-3">4.12</span> Minibuffer should display line and column numbers</h3>
<div class="outline-text-3" id="text-4-12">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>global-display-line-numbers-mode t<span style="color: #3a81c3;">)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(line-number-mode t)</span>
<span style="color: #3a81c3;">(</span>column-number-mode t<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-orgc1e7ff8" class="outline-3">
<h3 id="orgc1e7ff8"><span class="section-number-3">4.13</span> Completion Frameworks</h3>
<div class="outline-text-3" id="text-4-13">
<p>
<a href="http://tuhdo.github.io/helm-intro.html">Helm</a> provides possible completions and also shows recently executed commands when pressing <code>M-x</code>.
</p>

<p>
Extremely helpful for when switching between buffers, <code>C-x b</code>,
and discovering &amp; learning about other commands!
E.g., press <code>M-x</code> to see recently executed commands and other possible commands!
</p>

<p>
Try and be grateful.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">helm</span> 
 <span style="color: #3a81c3;">:ensure</span> t
 <span style="color: #3a81c3;">:diminish</span>
 <span style="color: #3a81c3;">:init</span> <span style="color: #6c3163;">(</span>helm-mode t<span style="color: #6c3163;">)</span>
 <span style="color: #3a81c3;">:bind</span>
  <span style="color: #6c3163;">(</span><span style="color: #2d9574;">"C-x C-r"</span> . helm-recentf<span style="color: #6c3163;">)</span>      <span style="color: #2aa1ae; background-color: #ecf3ec;">; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">search for recently edited  </span>

  <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Helm provides generic functions for completions to replace </span>
  <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">tab-completion in Emacs with no loss of functionality. </span>
  <span style="color: #6c3163;">(</span><span style="color: #2d9574;">"M-x"</span> . 'helm-M-x<span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span><span style="color: #2d9574;">"C-x b"</span>. 'helm-buffers-list<span style="color: #6c3163;">)</span> <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Avoid seeing all those *helm&#8943;* mini buffers!</span>
  <span style="color: #6c3163;">(</span><span style="color: #2d9574;">"C-x r b"</span> .'helm-filtered-bookmarks<span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span><span style="color: #2d9574;">"C-x C-f"</span> . 'helm-find-files<span style="color: #6c3163;">)</span>

   <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Show all meaningful Lisp symbols whose names match a given pattern.</span>
   <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Helpful for looking up commands.</span>
   <span style="color: #6c3163;">(</span><span style="color: #2d9574;">"C-h a"</span> . helm-apropos<span style="color: #6c3163;">)</span>

   <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Look at what was cut recently &amp; paste it in.</span>
   <span style="color: #6c3163;">(</span><span style="color: #2d9574;">"M-y"</span> . helm-show-kill-ring<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(global-set-key (kbd "M-x") 'execute-extended-command) ;; Default &#8220;M-x&#8221;</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Yet, let's keep tab-completetion anyhow.</span>
<span style="color: #3a81c3;">(</span>define-key helm-map <span style="color: #6c3163;">(</span>kbd <span style="color: #2d9574;">"TAB"</span><span style="color: #6c3163;">)</span> #'helm-execute-persistent-action<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>define-key helm-map <span style="color: #6c3163;">(</span>kbd <span style="color: #2d9574;">"&lt;tab&gt;"</span><span style="color: #6c3163;">)</span> #'helm-execute-persistent-action<span style="color: #3a81c3;">)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">We can list &#8216;</span><span style="color: #4e3163; background-color: #ecf3ec;">actions</span><span style="color: #2aa1ae; background-color: #ecf3ec;">&#8217; on the currently selected item by C-z.</span>
<span style="color: #3a81c3;">(</span>define-key helm-map <span style="color: #6c3163;">(</span>kbd <span style="color: #2d9574;">"C-z"</span><span style="color: #6c3163;">)</span>  'helm-select-action<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
When <code>helm-mode</code> is enabled, even help commands make use of it.
E.g., <code>C-h o</code> runs <code>describe-symbol</code> for the symbol at point,
and <code>C-h w</code> runs <code>where-is</code> to find the key binding of the symbol at point.
Both show a pop-up of other possible commands.
</p>

<p>
Incidentally, helm even provides an <a href="http://tuhdo.github.io/helm-intro.html#orgheadline24">interface</a> for the top program via
<code>helm-top</code>. It also serves as an interface to popular search engines
and over 100 websites such as <code>google, stackoverflow</code>, and <code>arxive</code>.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(shell-command "brew install surfraw &amp;")</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Invoke helm-surfraw</span>
</pre>
</div>
<p>
If we want to perform a google search, with interactive suggestions,
then invoke <code>helm-google-suggest</code> &#x2013;which can be acted for other serves,
such as Wikipedia or Youtube by <code>C-z</code>. For more google specific options,
there is the <code>google-this</code> package.
</p>

<p>
Let's switch to a powerful searching mechanism &#x2013; <a href="https://github.com/ShingoFukuyama/helm-swoop">helm-swoop</a>.
It allows us to not only search the current buffer but also
the other buffers and to make live edits by pressing <code>C-c C-e</code>
when a search buffer exists. Incidentally, executing <code>C-s</code> on a word, region,
will search for that particular word, region; then apply changes by <code>C-x C-s</code>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">helm-swoop</span>
  <span style="color: #3a81c3;">:ensure</span> t
  <span style="color: #3a81c3;">:bind</span>
  <span style="color: #6c3163;">(</span>
   <span style="color: #2d9574;">(</span><span style="color: #2d9574;">"C-s"</span>     . 'helm-swoop<span style="color: #2d9574;">)</span>           <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">search current buffer</span>
   <span style="color: #2d9574;">(</span><span style="color: #2d9574;">"C-M-s"</span>   . 'helm-multi-swoop-all<span style="color: #2d9574;">)</span> <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Search all buffer</span>
   <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Go back to last position where &#8216;</span><span style="color: #4e3163; background-color: #ecf3ec;">helm-swoop</span><span style="color: #2aa1ae; background-color: #ecf3ec;">&#8217; was called</span>
   <span style="color: #2d9574;">(</span><span style="color: #2d9574;">"C-S-s"</span> . 'helm-swoop-back-to-last-point<span style="color: #2d9574;">)</span>
  <span style="color: #6c3163;">)</span>
 <span style="color: #3a81c3;">:config</span>
   <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Give up colour for speed.</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> helm-swoop-speed-or-color nil<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>
<p>
Press <code>M-i</code> after a search has executed to enable it for all buffers.
</p>

<p>
We can also limit our search to org files, or buffers of the same mode,
or buffers belonging to the same project!
</p>

<p>
Finally, let's enable “complete anything” mode
&#x2013;it ought to start in half a second and only need two characters to get going,
which means word suggestions are provided and so I need only type partial words
then tab to get the full word!
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">company</span>
  <span style="color: #3a81c3;">:ensure</span>
  <span style="color: #3a81c3;">:diminish</span>
  <span style="color: #3a81c3;">:config</span>
    <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> company-idle-delay 0<span style="color: #6c3163;">)</span>
    <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> company-minimum-prefix-length 2<span style="color: #6c3163;">)</span>
    <span style="color: #6c3163;">(</span>add-hook 'after-init-hook 'global-company-mode<span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">So fast that we don't need this.</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(global-set-key (kbd "C-c h") 'company-complete)</span>
</pre>
</div>
<p>
Note that <code>Meta-/</code> goes through a sequence of completions.
</p>

<p>
Note that besides the arrow keys, we can also use <code>C-</code> or <code>M-</code> with <code>n, p</code> to
navigate the options.
</p>

<p>
Besides boring word completition, let's add support for <a href="https://github.com/dunn/company-emoji">emojis</a>.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">company-emoji</span> <span style="color: #3a81c3;">:ensure</span> t<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>add-to-list 'company-backends 'company-emoji<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
For example: 🥞 💻 🐵 ✉️😉 🐬 🌵.
</p>

<p>
➡️On a new line, write <code>:</code> then any letter to have a tool-tip appear.
All emoji names are lowercase. ◀
</p>

<p>
The libraries <code>emojify, emojify-logos</code> provides cool items like :haskell: :emacs: :org: :ruby: :python:.
Unfortunately they do not easily export to html with org-mode, so I'm not using
them.
</p>
</div>
</div>

<div id="outline-container-org7ed54bf" class="outline-3">
<h3 id="org7ed54bf"><span class="section-number-3">4.14</span> Neotree: Directory Tree Listing</h3>
<div class="outline-text-3" id="text-4-14">
<p>
We open a nifty file manager upon startup.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">neotree --sidebar for project file navigation</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">neotree</span> <span style="color: #3a81c3;">:ensure</span> t
  <span style="color: #3a81c3;">:config</span> <span style="color: #6c3163;">(</span>global-set-key <span style="color: #2d9574;">"\C-x\ d"</span> 'neotree-toggle<span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(use-package all-the-icons :ensure t)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Only do this once: (all-the-icons-install-fonts)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> neo-theme 'icons<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>neotree-refresh<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Open it up upon startup.</span>
<span style="color: #3a81c3;">(</span>neotree-toggle<span style="color: #3a81c3;">)</span>
</pre>
</div>
<p>
By default <code>C-x d</code> invokes <code>dired</code>, but I prefer <code>neotree</code> for file management.
</p>

<p>
Useful navigational commands include
</p>
<ul class="org-ul">
<li><code>U</code> to go up a directory.</li>
<li><code>C-c C-c</code> to change directory focus; <code>C-C c</code> to type the directory out.</li>
<li><code>?</code> or <code>h</code> to get help and <code>q</code> to quit.</li>
</ul>

<p>
As always, to go to the neotree pane when it's the only other window,
execute <code>C-x o</code>.
</p>

<p>
I <i>rarely</i> make use of this feature; company mode &amp; Helm together quickly provide
an automatic replacement for nearly all of my uses.
</p>
</div>
</div>

<div id="outline-container-orgf3cb3b0" class="outline-3">
<h3 id="orgf3cb3b0"><span class="section-number-3">4.15</span> Window resizing using the golden ratio&#xa0;&#xa0;&#xa0;<span class="tag"><span class="Disabled">Disabled</span></span></h3>
<div class="outline-text-3" id="text-4-15">
<p>
Let's load the following package, which automatically resizes windows so that
the window containing the cursor is the largest, according to the golden ratio.
Consequently, the window we're working with is nice and large yet the other windows
are still readable.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">golden-ratio</span>
  <span style="color: #3a81c3;">:ensure</span> t
  <span style="color: #3a81c3;">:diminish</span> golden-ratio-mode
  <span style="color: #3a81c3;">:init</span> <span style="color: #6c3163;">(</span>golden-ratio-mode 1<span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
After some time this got a bit annoying and I'm no longer  using this.
</p>
</div>
</div>

<div id="outline-container-org6d025c5" class="outline-3">
<h3 id="org6d025c5"><span class="section-number-3">4.16</span> Jump between windows using Cmd+Arrow</h3>
<div class="outline-text-3" id="text-4-16">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">windmove</span>
  <span style="color: #3a81c3;">:ensure</span> t
  <span style="color: #3a81c3;">:config</span>
  <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">use command key on Mac</span>
  <span style="color: #6c3163;">(</span>windmove-default-keybindings 'super<span style="color: #6c3163;">)</span>
  <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">wrap around at edges</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> windmove-wrap-around t<span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-org363e928" class="outline-2">
<h2 id="org363e928"><span class="section-number-2">5</span> General Config, “Interior”&#xa0;&#xa0;&#xa0;<span class="tag"><span class="Bad_name">Bad_name</span></span></h2>
<div class="outline-text-2" id="text-5">
<p>
Configurations that affect Emacs, but not the look.
</p>
</div>

<div id="outline-container-orge32fe41" class="outline-3">
<h3 id="orge32fe41"><span class="section-number-3">5.1</span> Backups</h3>
<div class="outline-text-3" id="text-5-1">
<p>
By default, Emacs saves backup files &#x2013; those ending in ~ &#x2013; in the current directory, thereby cluttering it up. Let's place them in <code>~/.emacs.d/backups</code>, in case we need to look for a backup; moreover,
let's keep old versions since there's disk space to go around
&#x2013;what am I going to do with 500gigs when nearly all my ‘software’ is
textfiles interpreted within Emacs 😼
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">New location for backups.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> backup-directory-alist '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">(</span><span style="color: #2d9574;">"."</span> . <span style="color: #2d9574;">"~/.emacs.d/backups"</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Never silently delete old backups.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> delete-old-versions -1<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Use version numbers for backup files.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> version-control t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Even version controlled files get to be backed up.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> vc-make-backup-files t<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Why backups? Sometimes I may forget to submit a file, or edit, to my
version control system, and it'd be nice to be able to see a local
automatic backup. Whenever ‘I need space,’ then I simply empty
the backup directory, if ever.
</p>

<p>
Like package installations, my backups are not kept in any version control
system, like git; only locally.
</p>
</div>
</div>
</div>

<div id="outline-container-org2caf379" class="outline-2">
<h2 id="org2caf379"><span class="section-number-2">6</span> Helpful Functions &amp; Shortcuts</h2>
<div class="outline-text-2" id="text-6">
<p>
Here is a collection of Emacs-lisp functions that I have come to use in other files.
</p>

<p>
Let's save a few precious seconds,
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">change all prompts to y or n</span>
<span style="color: #3a81c3;">(</span>fset 'yes-or-no-p 'y-or-n-p<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>

<div id="outline-container-org440c857" class="outline-3">
<h3 id="org440c857"><span class="section-number-3">6.1</span> Bind <code>recompile</code> to <code>C-c C-m</code> &#x2013; “m” for “m”ake</h3>
<div class="outline-text-3" id="text-6-1">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defvar</span> <span style="color: #715ab1;">my-keys-minor-mode-map</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">let</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>map <span style="color: #b1951d;">(</span>make-sparse-keymap<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
    <span style="color: #2d9574;">(</span>define-key map <span style="color: #67b11d;">(</span>kbd <span style="color: #2d9574;">"C-c C-m"</span><span style="color: #67b11d;">)</span> 'recompile<span style="color: #2d9574;">)</span>
    map<span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"my-keys-minor-mode keymap."</span><span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">define-minor-mode</span> <span style="color: #6c3163; font-weight: bold;">my-keys-minor-mode</span>
  <span style="color: #da8b55;">"A minor mode so that my key settings override annoying major modes."</span>
  <span style="color: #3a81c3;">:init-value</span> t
  <span style="color: #3a81c3;">:lighter</span> <span style="color: #2d9574;">" my-keys"</span><span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span>my-keys-minor-mode<span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span>diminish 'my-keys-minor-mode<span style="color: #3a81c3;">)</span> <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Don't show it in the modeline.</span>
</pre>
</div>
</div>
</div>
<div id="outline-container-orga4573ad" class="outline-3">
<h3 id="orga4573ad"><span class="section-number-3">6.2</span> Reload buffer with <code>f5</code></h3>
<div class="outline-text-3" id="text-6-2">
<p>
I do this so often it's not even funny.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>global-set-key <span style="color: #6c3163;">[</span>f5<span style="color: #6c3163;">]</span> '<span style="color: #6c3163;">(</span>lambda <span style="color: #2d9574;">()</span> <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #2d9574;">)</span> <span style="color: #2d9574;">(</span>revert-buffer nil t nil<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
In Mac OS, one uses <code>Cmd-r</code> to reload a page and spacemacs binds buffer reversion to <code>Cmd-u</code>
&#x2013;in Emacs, Mac's <code>Cmd</code> is referred to as the ‘super key’ and denoted <code>s</code>.
</p>

<p>
Moreover, since I use Org-mode to generate code blocks and occasionally
inspect them, it would be nice if they automatically reverted when they
were regenerated &#x2013;Emacs should also prompt me if I make any changes!
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Auto update buffers that change on disk.</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Will be prompted if there are changes that could be lost.</span>
<span style="color: #3a81c3;">(</span>global-auto-revert-mode 1<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-orgc03da51" class="outline-3">
<h3 id="orgc03da51"><span class="section-number-3">6.3</span> Kill to start of line</h3>
<div class="outline-text-3" id="text-6-3">
<p>
Dual to <code>C-k</code>,
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">M-k kills to the left</span>
<span style="color: #3a81c3;">(</span>global-set-key <span style="color: #2d9574;">"\M-k"</span> '<span style="color: #6c3163;">(</span>lambda <span style="color: #2d9574;">()</span> <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #2d9574;">)</span> <span style="color: #2d9574;">(</span>kill-line 0<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span> <span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>
<div id="outline-container-org3211870" class="outline-3">
<h3 id="org3211870"><span class="section-number-3">6.4</span> <code>file-as-list</code> and <code>file-as-string</code></h3>
<div class="outline-text-3" id="text-6-4">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">file-as-list</span> <span style="color: #6c3163;">(</span>filename<span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"Return the contents of FILENAME as a list of lines"</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">with-temp-buffer</span>
    <span style="color: #2d9574;">(</span>insert-file-contents filename<span style="color: #2d9574;">)</span>
    <span style="color: #2d9574;">(</span>split-string <span style="color: #67b11d;">(</span>buffer-string<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">file-as-string</span> <span style="color: #6c3163;">(</span>filename<span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"Return the contents of FILENAME as a list of lines"</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">with-temp-buffer</span>
    <span style="color: #2d9574;">(</span>insert-file-contents filename<span style="color: #2d9574;">)</span>
    <span style="color: #2d9574;">(</span>buffer-string<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-org4014ba0" class="outline-3">
<h3 id="org4014ba0"><span class="section-number-3">6.5</span> <code>kill-other-buffers</code></h3>
<div class="outline-text-3" id="text-6-5">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">kill-other-buffers</span> <span style="color: #6c3163;">()</span>
  <span style="color: #da8b55;">"Kill all other buffers."</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span>mapc 'kill-buffer <span style="color: #2d9574;">(</span>delq <span style="color: #67b11d;">(</span>current-buffer<span style="color: #67b11d;">)</span> <span style="color: #67b11d;">(</span>buffer-list<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-org9a59387" class="outline-3">
<h3 id="org9a59387"><span class="section-number-3">6.6</span> <code>create-scratch-buffer</code></h3>
<div class="outline-text-3" id="text-6-6">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">A very simple function to recreate the scratch buffer:</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">( http://emacswiki.org/emacs/RecreateScratchBuffer )</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">create-scratch-buffer</span> nil
   <span style="color: #da8b55;">"create a scratch buffer"</span>
   <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #6c3163;">)</span>
   <span style="color: #6c3163;">(</span>switch-to-buffer <span style="color: #2d9574;">(</span>get-buffer-create <span style="color: #2d9574;">"*scratch*"</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span>
   <span style="color: #6c3163;">(</span>insert initial-scratch-message<span style="color: #6c3163;">)</span>
   <span style="color: #6c3163;">(</span>lisp-interaction-mode<span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>   
</pre>
</div>
</div>
</div>
<div id="outline-container-orgb216a31" class="outline-3">
<h3 id="orgb216a31"><span class="section-number-3">6.7</span> Switching from 2 horizontal windows to 2 vertical windows</h3>
<div class="outline-text-3" id="text-6-7">
<p>
I often find myself switching from a horizontal view of two windows in Emacs to a
vertical view. This requires a variation of <code>C-x 1 RET C - x 3 RET C-x o X-x b RET</code>. 
Instead I now only need to type <code>C-|</code> to make this switch.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">ensure-two-vertical-windows</span> <span style="color: #6c3163;">()</span> 
  <span style="color: #da8b55;">"hello"</span>
 <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #6c3163;">)</span>
 <span style="color: #6c3163;">(</span>other-window 1<span style="color: #6c3163;">)</span>                       <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x 0</span>
 <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">let</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>otherBuffer <span style="color: #b1951d;">(</span>buffer-name<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span> 
   <span style="color: #2d9574;">(</span>delete-window<span style="color: #2d9574;">)</span>                      <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x 0</span>
   <span style="color: #2d9574;">(</span>split-window-right<span style="color: #2d9574;">)</span>                 <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x 3</span>
   <span style="color: #2d9574;">(</span>other-window 1<span style="color: #2d9574;">)</span>                     <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x 0</span>
   <span style="color: #2d9574;">(</span>switch-to-buffer otherBuffer<span style="color: #2d9574;">)</span>       <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x b RET</span>
 <span style="color: #6c3163;">)</span>
 <span style="color: #6c3163;">(</span>other-window 1<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>global-set-key <span style="color: #6c3163;">(</span>kbd <span style="color: #2d9574;">"C-|"</span><span style="color: #6c3163;">)</span> 'ensure-two-vertical-windows<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>
<div id="outline-container-orgaf8ad78" class="outline-3">
<h3 id="orgaf8ad78"><span class="section-number-3">6.8</span> <code>re-replace-in-file</code></h3>
<div class="outline-text-3" id="text-6-8">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">re-replace-in-file</span> <span style="color: #6c3163;">(</span>file regex whatDo<span style="color: #6c3163;">)</span> 
   <span style="color: #da8b55;">"Find and replace a regular expression in-place in a file.</span>

<span style="color: #da8b55;">   Terrible function &#8230; before I took the time to learn any Elisp!</span>
<span style="color: #da8b55;">   "</span>

    <span style="color: #6c3163;">(</span>find-file file<span style="color: #6c3163;">)</span>
    <span style="color: #6c3163;">(</span>goto-char 0<span style="color: #6c3163;">)</span>
    <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">let</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>altered <span style="color: #b1951d;">(</span>replace-regexp-in-string regex whatDo <span style="color: #3a81c3;">(</span>buffer-string<span style="color: #3a81c3;">)</span><span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
      <span style="color: #2d9574;">(</span>erase-buffer<span style="color: #2d9574;">)</span>
      <span style="color: #2d9574;">(</span>insert altered<span style="color: #2d9574;">)</span>
      <span style="color: #2d9574;">(</span>save-buffer<span style="color: #2d9574;">)</span>
      <span style="color: #2d9574;">(</span>kill-buffer<span style="color: #2d9574;">)</span>
   <span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Example usage:
</p>
<pre class="example">
;; Within mysite.html we rewrite: &lt;h1.*h1&gt;   ↦   &lt;h1.*h1&gt;\n NICE
;; I.e., we add a line break after the first heading and a new word, “NICE”.
(re-replace-in-file "mysite.html"
                    "&lt;h1.*h1&gt;"
                    (lambda (x) (concat x "\n NICE")))
</pre>
</div>

<div id="outline-container-org0b473f8" class="outline-4">
<h4 id="org0b473f8"><span class="section-number-4">6.8.1</span> <code>mapsto</code>: Simple rewriting for current buffer</h4>
<div class="outline-text-4" id="text-6-8-1">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">mapsto</span> <span style="color: #6c3163;">(</span>this that<span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"In the current buffer make the regular expression rewrite: this &#8614; that."</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">let*</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>current-location <span style="color: #b1951d;">(</span>point<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span>
       <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Do not alter the case of the &lt;replacement text&gt;.</span>
       <span style="color: #67b11d;">(</span>altered <span style="color: #b1951d;">(</span>replace-regexp-in-string this <span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">lambda</span> <span style="color: #6c3163;">(</span>x<span style="color: #6c3163;">)</span> that<span style="color: #3a81c3;">)</span> <span style="color: #3a81c3;">(</span>buffer-string<span style="color: #3a81c3;">)</span> 'no-fixed-case<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span>
       <span style="color: #2d9574;">)</span>
      <span style="color: #2d9574;">(</span>erase-buffer<span style="color: #2d9574;">)</span>
      <span style="color: #2d9574;">(</span>insert altered<span style="color: #2d9574;">)</span>
      <span style="color: #2d9574;">(</span>save-buffer<span style="color: #2d9574;">)</span>
      <span style="color: #2d9574;">(</span>goto-char current-location<span style="color: #2d9574;">)</span>
  <span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-org2735a41" class="outline-3">
<h3 id="org2735a41"><span class="section-number-3">6.9</span> Obtaining Values of <code>#+KEYWORD</code> Annotations</h3>
<div class="outline-text-3" id="text-6-9">
<p>
Org-mode settings are, for the most part, in the form <code>#+KEYWORD: VALUE</code>. Of notable interest
are the <code>TITLE</code> and <code>NAME</code> keywords. We use the following <code>org-keywords</code> function to obtain
the values of arbitrary <code>#+THIS : THAT</code> pairs, which may not necessarily be supported by native
Org-mode &#x2013;we do so for the case, for example, of the <code>CATEGORIES</code> and <code>IMAGE</code> tags associated with an article.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Src: http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">org-keywords</span> <span style="color: #6c3163;">()</span>
  <span style="color: #da8b55;">"Parse the buffer and return a cons list of (property . value) from lines like: #+PROPERTY: value"</span>
  <span style="color: #6c3163;">(</span>org-element-map <span style="color: #2d9574;">(</span>org-element-parse-buffer 'element<span style="color: #2d9574;">)</span> 'keyword
                   <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">lambda</span> <span style="color: #67b11d;">(</span>keyword<span style="color: #67b11d;">)</span> <span style="color: #67b11d;">(</span>cons <span style="color: #b1951d;">(</span>org-element-property <span style="color: #3a81c3;">:key</span> keyword<span style="color: #b1951d;">)</span>
                                           <span style="color: #b1951d;">(</span>org-element-property <span style="color: #3a81c3;">:value</span> keyword<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">org-keyword</span> <span style="color: #6c3163;">(</span>KEYWORD<span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"Get the value of a KEYWORD in the form of #+KEYWORD: value"</span>
  <span style="color: #6c3163;">(</span>cdr <span style="color: #2d9574;">(</span>assoc KEYWORD <span style="color: #67b11d;">(</span>org-keywords<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Note that capitalisation in a ”#+KeyWord” is irrelevant.
</p>

<p>
See <a href="https://orgmode.org/manual/Org-syntax.html">here</a> on how to see the abstract syntax tree of an org file
and how to manipulate it.
</p>
</div>
</div>

<div id="outline-container-orgb27c52f" class="outline-3">
<h3 id="orgb27c52f"><span class="section-number-3">6.10</span> Quickly pop-up a terminal, run a command, close it</h3>
<div class="outline-text-3" id="text-6-10">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">cl-defun</span> <span style="color: #6c3163; font-weight: bold;">toggle-terminal</span> <span style="color: #6c3163;">(</span><span style="color: #ba2f59; font-weight: bold;">&amp;optional</span> <span style="color: #2d9574;">(</span>name <span style="color: #2d9574;">"*eshell-pop-up*"</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span>
   <span style="color: #da8b55;">"Pop up a terminal, do some work, then close it using the same command.</span>

<span style="color: #da8b55;">   The toggle behaviour is tied into the existence of the pop-up buffer.</span>
<span style="color: #da8b55;">   If the buffer exists, kill it; else create it.</span>
<span style="color: #da8b55;">   "</span>
   <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #6c3163;">)</span>
   <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">cond</span> 
     <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">when the terminal buffer is alive, kill it.</span>
     <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>get-buffer name<span style="color: #67b11d;">)</span>  <span style="color: #67b11d;">(</span>kill-buffer name<span style="color: #67b11d;">)</span> 
                         <span style="color: #67b11d;">(</span><span style="color: #3a81c3; font-weight: bold;">ignore-errors</span> <span style="color: #b1951d;">(</span>delete-window<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
     <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">otherwise, set value to refer to a new eshell buffer.</span>
     <span style="color: #2d9574;">(</span>t                  <span style="color: #67b11d;">(</span>split-window-right<span style="color: #67b11d;">)</span>
                         <span style="color: #67b11d;">(</span>other-window 1<span style="color: #67b11d;">)</span>
                         <span style="color: #67b11d;">(</span>eshell<span style="color: #67b11d;">)</span>
                         <span style="color: #67b11d;">(</span>rename-buffer name<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
   <span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span>global-set-key <span style="color: #2d9574;">"\C-t"</span> 'toggle-terminal<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-orge0f5623" class="outline-3">
<h3 id="orge0f5623"><span class="section-number-3">6.11</span> <code>C-x k</code> kills current buffer</h3>
<div class="outline-text-3" id="text-6-11">
<p>
By default <code>C-x k</code> prompts to select which buffer
should be selected. I almost always want to kill
the current buffer, so let's not waste time making
such a tedious decision.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Kill current buffer; prompt only if</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">there are unsaved changes.</span>
<span style="color: #3a81c3;">(</span>global-set-key <span style="color: #6c3163;">(</span>kbd <span style="color: #2d9574;">"C-x k"</span><span style="color: #6c3163;">)</span>
  '<span style="color: #6c3163;">(</span>lambda <span style="color: #2d9574;">()</span> <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #2d9574;">)</span> <span style="color: #2d9574;">(</span>kill-buffer <span style="color: #67b11d;">(</span>current-buffer<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-org582256d" class="outline-2">
<h2 id="org582256d"><span class="section-number-2">7</span> Life within Org-mode</h2>
<div class="outline-text-2" id="text-7">
<p>
<a href="http://notesyoujustmightwanttosave.blogspot.com/">Here is useful Org-Mode Table Editing Cheatsheet.</a>
</p>

<p>
First off, let's replace the content marker, “⋯”, with a nice
unicode arrow.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-ellipsis <span style="color: #2d9574;">" &#10549;"</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Also, fold all source blocks on startup.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-hide-block-startup t<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>

<div id="outline-container-org7df22ce" class="outline-3">
<h3 id="org7df22ce"><span class="section-number-3">7.1</span> Org Speed Keys</h3>
<div class="outline-text-3" id="text-7-1">
<p>
Let's enable the <a href="http://notesyoujustmightwanttosave.blogspot.com/2011/12/org-speed-keys.html">Org Speed Keys</a> so that when the cursor is at the beginning of 
a headline, we can perform fast manipulation &amp; navigation using the standard Emacs movement
controls, such as 
</p>
<ul class="org-ul">
<li><code>#</code> toggle <code>COMMENT</code>-ing for an org-header.</li>
<li><p>
<code>s</code> toggles “narrowing” to a subtree; i.e., hide the rest of the document.
</p>

<p>
If you narrow to a subtree then any export, <code>C-c C-e</code>, will only consider
the narrowed detail.
</p></li>

<li><code>I/O</code> clock In/Out to the task defined by the current heading. 
<ul class="org-ul">
<li>Keep track of your work times!</li>
<li><code>v</code> view agenda.</li>
</ul></li>
<li><code>u</code> for jumping upwards to the parent heading.</li>
<li><code>c</code> for cycling structure below current heading, or <code>C</code> for cycling global structure.</li>
<li><code>i</code> insert a new same-level heading below current heading.</li>
<li><code>w</code> refile current heading; options list pops-up to select which heading to move it to. Neato!</li>
<li><code>t</code> cycle through the available TODO states.</li>
<li><code>^</code> sort children of current subtree; brings up a list of sorting options.</li>
<li><code>n/p</code> for next/previous <i>visible</i> heading.</li>
<li><code>f/b</code> for jumping forward/backward to the next/previous <i>same-level</i> heading.</li>
<li><code>D/U</code> move a heading down/up.</li>
<li><code>L/R</code> recursively promote (move leftwards) or demote (more rightwards) a heading.</li>
<li><code>1,2,3</code> to mark a heading with priority, highest to lowest.</li>
</ul>

<p>
We can add our own speed keys by altering the <code>org-speed-commands-user</code> variable.
</p>

<p>
Finally, <code>?</code> to see a complete list of keys available.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-use-speed-commands t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Add more speed commands by adding to this association list.</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(describe-symbol 'org-speed-commands-user)</span>
</pre>
</div>

<p>
PS. <code>C-RET, C-S-RET</code> make a new heading where the latter marks it as a <code>TODO</code>.
</p>
</div>
</div>

<div id="outline-container-org098ec75" class="outline-3">
<h3 id="org098ec75"><span class="section-number-3">7.2</span> Using org-mode as a Day Planner</h3>
<div class="outline-text-3" id="text-7-2">
<p>
⟪ This section is based on a dated, yet delightful, tutorial
  of the same title by <a href="http://newartisans.com/2007/08/using-org-mode-as-a-day-planner/">John Wiegley</a>. ⟫
</p>

<p>
We want a day-planner with the following use:
</p>
<ol class="org-ol">
<li>“Mindlessly” &amp; rapidly create new tasks.</li>
<li>Schedule and archive tasks at the end, or start, of the work day.</li>
<li>Glance at a week's tasks, shuffle if need be.</li>
<li>Prioritise the day's tasks. Aim for ≤15 tasks.</li>
<li>Progress towards <code>A</code> tasks completion by documenting work completed.</li>
<li>Repeat! During the day, if anything comes up, capture it and intentionally
forget about it.</li>
</ol>

<p>
<a href="https://orgmode.org/org.html#Setting-up-capture">Capture</a> lets me quickly make notes &amp; capture ideas, with associated reference material,
without any interruption to the current work flow. Without losing focus on what you're doing,
quickly jot down a note of something important that just came up.
</p>

<p>
E.g., I have a task, or something I wish to note down, rather than opening
some file, then making a heading, then writing it; instead, I press
<code>C-c c t</code> and a pop-up appears, I make my note, and it disappears with my
notes file(s) now being altered! Moreover, by default it provide a timestamp
and a link to the file location where I made the note &#x2013;helpful for tasks, tickets,
to be tackled later on.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-default-notes-file <span style="color: #2d9574;">"~/Dropbox/todo.org"</span><span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span>define-key global-map <span style="color: #2d9574;">"\C-cc"</span> 'org-capture<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
By default we only get a ‘tasks’ form of capture, let's add some more.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">cl-defun</span> <span style="color: #6c3163; font-weight: bold;">my/make/org-capture-template</span> 
   <span style="color: #6c3163;">(</span>shortcut heading <span style="color: #ba2f59; font-weight: bold;">&amp;optional</span> <span style="color: #2d9574;">(</span>no-todo nil<span style="color: #2d9574;">)</span> <span style="color: #2d9574;">(</span>description heading<span style="color: #2d9574;">)</span> <span style="color: #2d9574;">(</span>category heading<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"Quickly produce an org-capture-template.</span>

<span style="color: #da8b55;">  After adding the result of this function to &#8216;</span><span style="color: #4e3163;">org-capture-templates</span><span style="color: #da8b55;">&#8217;,</span>
<span style="color: #da8b55;">  we will be able perform a capture with &#8220;C-c c &#8216;</span><span style="color: #4e3163;">shortcut</span><span style="color: #da8b55;">&#8217;&#8221;</span>
<span style="color: #da8b55;">  which will have description &#8216;</span><span style="color: #4e3163;">description</span><span style="color: #da8b55;">&#8217;.</span>
<span style="color: #da8b55;">  It will be added to the tasks file under heading &#8216;</span><span style="color: #4e3163;">heading</span><span style="color: #da8b55;">&#8217;</span>
<span style="color: #da8b55;">  and be marked with category  &#8216;</span><span style="color: #4e3163;">category</span><span style="color: #da8b55;">&#8217;.</span>

<span style="color: #da8b55;">  &#8216;</span><span style="color: #4e3163;">no-todo</span><span style="color: #da8b55;">&#8217; omits the &#8216;</span><span style="color: #4e3163;">TODO</span><span style="color: #da8b55;">&#8217; tag from the resulting item; e.g.,</span>
<span style="color: #da8b55;">  when it's merely an interesting note that needn't be acted upon.</span>
<span style="color: #da8b55;">  &#9472;Probably a bad idea&#9472;</span>

<span style="color: #da8b55;">  Defaults for &#8216;</span><span style="color: #4e3163;">description</span><span style="color: #da8b55;">&#8217; and &#8216;</span><span style="color: #4e3163;">category</span><span style="color: #da8b55;">&#8217; are set to the same as</span>
<span style="color: #da8b55;">  the &#8216;</span><span style="color: #4e3163;">heading</span><span style="color: #da8b55;">&#8217;. Default for &#8216;</span><span style="color: #4e3163;">no-todo</span><span style="color: #da8b55;">&#8217; is &#8216;</span><span style="color: #4e3163;">nil</span><span style="color: #da8b55;">&#8217;.</span>
<span style="color: #da8b55;">  "</span>
  `<span style="color: #6c3163;">(</span>,shortcut ,description entry
      <span style="color: #2d9574;">(</span>file+headline org-default-notes-file 
         ,<span style="color: #67b11d;">(</span>concat heading <span style="color: #2d9574;">"\n#+CATEGORY: "</span> category<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
      , <span style="color: #2d9574;">(</span>concat <span style="color: #2d9574;">"*"</span> <span style="color: #67b11d;">(</span><span style="color: #3a81c3; font-weight: bold;">unless</span> no-todo <span style="color: #2d9574;">" TODO"</span><span style="color: #67b11d;">)</span> <span style="color: #2d9574;">" %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"</span><span style="color: #2d9574;">)</span>
      <span style="color: #3a81c3;">:empty-lines</span> 1<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-capture-templates
  `<span style="color: #6c3163;">(</span>
     ,<span style="color: #2d9574;">(</span>my/make/org-capture-template <span style="color: #2d9574;">"t"</span> <span style="color: #2d9574;">"Tasks, Getting Things Done"</span><span style="color: #2d9574;">)</span>
     ,<span style="color: #2d9574;">(</span>my/make/org-capture-template <span style="color: #2d9574;">"r"</span> <span style="color: #2d9574;">"Research"</span><span style="color: #2d9574;">)</span>
     ,<span style="color: #2d9574;">(</span>my/make/org-capture-template <span style="color: #2d9574;">"m"</span> <span style="color: #2d9574;">"Email"</span><span style="color: #2d9574;">)</span>
     ,<span style="color: #2d9574;">(</span>my/make/org-capture-template <span style="color: #2d9574;">"e"</span> <span style="color: #2d9574;">"Emacs (&#8226;&#768;&#7447;&#8226;&#769;)&#1608;"</span><span style="color: #2d9574;">)</span>
     ,<span style="color: #2d9574;">(</span>my/make/org-capture-template <span style="color: #2d9574;">"b"</span> <span style="color: #2d9574;">"Blog"</span><span style="color: #2d9574;">)</span>
     ,<span style="color: #2d9574;">(</span>my/make/org-capture-template <span style="color: #2d9574;">"a"</span> <span style="color: #2d9574;">"Arbitrary Reading and Learning"</span><span style="color: #2d9574;">)</span>
     ,<span style="color: #2d9574;">(</span>my/make/org-capture-template <span style="color: #2d9574;">"p"</span> <span style="color: #2d9574;">"Personal Matters"</span><span style="color: #2d9574;">)</span>
<span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
For now I capture everything into a single file.
One would ideally keep separate client, project, information in its own org file.
The <code>#+CATEGORY</code> appears alongside each task in the agenda view &#x2013;keep reading.
</p>

<p>
<b>Where am I currently capturing?</b>
</p>
<ul class="org-ul">
<li>During meetings, when a nifty idea pops into my mind, I quickly capture it.
<ul class="org-ul">
<li>I've found taking my laptop to meetings makes me an active listener
and I get much more out of my meetings since I'm taking notes.</li>
</ul></li>
<li>Through out the day, as I browse the web, read, and work; random ideas pop-up, and I capture them indiscriminately.</li>
<li>I envision that for a phone call, I would open up a capture to make note of what the call entailed so I can review it later.</li>
<li><p>
Anywhere you simply want to make a note, for the current heading, just press
<code>C-c C-z</code>. The notes are just your remarks along with a timestamp; they are
collected at the top of the tree, under the heading.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Ensure notes are stored at the top of a tree.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-reverse-note-order nil<span style="color: #3a81c3;">)</span>
</pre>
</div></li>
</ul>

<p>
Anyhow…
</p>

<p>
Step 1: When new tasks come up
Isn't it great that we can squirrel away info into some default location
then immediately return to what we were doing before &#x2013;with speed &amp; minimal distraction! ♥‿♥
Indeed, if our system for task management were slow then we may not produce tasks and so forget them altogether! щ(゜ロ゜щ)
</p>
<ul class="org-ul">
<li><p>
Entering tasks is a desirably impulsive act; 
do not make any further scheduling considerations.
</p>

<p>
The next step, the review stage occurring at the end or the start of
the workday, is for processing.
</p></li>
</ul>

<blockquote>
<p>
<i>The reason for this is that entering new tasks should be impulsive, not reasoned./</i> 
<i>Your reasoning skills are required for the task at hand, not every new tidbit./</i> 
<i>You may even find that during the few hours that transpire between creating a</i>
<i>task and categorizing it, you’ve either already done it or discovered it doesn’t</i>
<i>need to be done at all!</i> &#x2013; <a href="http://newartisans.com/2007/08/using-org-mode-as-a-day-planner/">John Wiegley</a>
</p>
</blockquote>

<p>
When my computer isn't handy, make a note on my phone then transfer it later.
</p>

<p>
<b>Step 2: Filing your tasks</b>
At a later time, a time of reflection, we go to our tasks list and actually schedule time to get them done
by <code>C-c C-s</code> then pick a date by entering a number in the form <code>+n</code> to mean that task is due <code>n</code> days from now.
</p>
<ul class="org-ul">
<li>Tasks with no due date are ones that “could happen anytime”, most likely no time at all.</li>
<li>At least schedule tasks reasonably far off in the future, then reassess when the time comes.</li>
<li><p>
An uncompleted task is by default rescheduled to the current day, each day, along with how overdue it is.
</p>
<ul class="org-ul">
<li>Aim to consciously reschedule such tasks!</li>
</ul>

<p>
With time, it will become clear what is an unreasonable day
verses what is an achievable day.
</p></li>
</ul>

<p>
<b>Step 3: Quickly review the upcoming week</b>
The next day we begin our work, we press <code>C-c a a</code> to see the 
scheduled tasks for this week --<code>C-c C-s</code> to re-schedule the 
task under the cursor and <code>r</code> to refresh the agenda.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>define-key global-map <span style="color: #2d9574;">"\C-ca"</span> 'org-agenda<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
<b>Step 4: Getting ready for the day</b>
After having seen our tasks for the week, we press <code>d</code> to enter daily view
for the current day. Now we decide whether the items for today are 
<code>A</code>: of high urgency &amp; important; <code>B</code>: of moderate urgency &amp; importance; or
<code>C</code>: Pretty much optional, or very quick or fun to do.
</p>
<ul class="org-ul">
<li><code>A</code> tasks should be both important <i>and</i> urgently done on the day they were scheduled.
<ul class="org-ul">
<li>Such tasks should be relatively rare!</li>
<li>If you have too many, you're anxious about priorities and rendering
priorities useless.</li>
</ul></li>
<li><code>C</code> tasks can always be scheduled for another day without much worry.
<ul class="org-ul">
<li>Act! If the thought of rescheduling causes you to worry, upgrade it to a
<code>B</code> or <code>A</code>.</li>
</ul></li>
<li>As such, most tasks will generally be priority <code>B</code>: 
Tasks that need to be done, but the exact day isn't as critical as with an
<code>A</code> task. These are the “bread and butter” tasks that make up your day to day
life.</li>
</ul>

<p>
On a task item, press <code>,</code> then one of <code>A, B, C</code> to set its priority.
Then <code>r</code> to refresh.
</p>

<p>
<b>Step 5: Doing the work</b>
Since <code>A</code> tasks are the important and urgent ones, if you do all of the <code>A</code> tasks and
nothing else today, no one would suffer. It's a good day (─‿‿─).
</p>

<p>
There should be no scheduling nor prioritising at this stage.
You should not be touching your tasks file until your next review session:
Either at the end of the day or the start of the next.
</p>

<ul class="org-ul">
<li>Leverage priorities! E.g., When a full day has several <code>C</code> tasks, reschedule
them for later in the week without a second thought.
<ul class="org-ul">
<li>You've already provided consideration when assigning priorities.</li>
</ul></li>
</ul>

<p>
<b>Step 6: Moving a task toward completion</b>
My workflow states are described in the section
<a href="#WorkflowStates">7.3</a> and contain states: <code>TODO, STARTED, WAITING, ON_HOLD, CANCELLED, DONE</code>.
</p>
<ul class="org-ul">
<li>Tasks marked <code>WAITING</code> are ones for which we are awaiting some event, like someone
to reply to our query. As such, these tasks can be rescheduled until I give up
or the awaited event happens &#x2013;in which case I go to <code>STARTED</code> and document
the reply to my query.</li>
<li>The task may be put off indefinitely with <code>ON_HOLD</code>, or I may choose never to do it
with <code>CANCELLED</code>. Along with <code>DONE</code>, these three mark a task as completed
and so it needn't appear in any agenda view.</li>
</ul>

<p>
I personally clock-in and clock-out of tasks &#x2013;keep reading&#x2013;,
where upon clocking-out I'm prompted for a note about what I've accomplished
so far.
Entering a comment about what I've done, even if it's very little,
feels like I'm getting something done. It's an explicit marker of progress.
</p>

<p>
In the past, I would make a “captain's log” at the end of the day, but that's
like commenting code after it's written, I didn't always feel like doing it and
it wasn't that important after the fact. The continuous approach of noting after
every clock-out is much more practical, for me at least.
</p>

<p>
<b>Step 7: Archiving Tasks</b>
During the review state,
when a task is completed, ‘archive’ it with <code>C-c C-x C-s</code>: This marks it as done, adds a time stamp,
and moves it to a local <code>*.org_archive</code> file. This was our ‘to do’ list becomes a ‘ta da’ list showcasing
all we have done (•̀ᴗ•́)و
</p>

<p>
Archiving keeps task lists clutter free, but unlike deletion it allows
us, possibly rarely, to look up details of a task or what tasks were completed
in a certain time frame &#x2013;which may be a motivational act, to see that you have
actually completed more than you thought, provided you make and archive tasks
regularly. We can use <code>(org-search-view)</code> to search an org file <i>and</i> the
archive file too, if we enable it so.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Include agenda archive files when searching for things</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-agenda-text-search-extra-files <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">quote</span> <span style="color: #2d9574;">(</span>agenda-archives<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Invoing the agenda command shows the agenda and enables</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">the org-agenda variables.</span>
<span style="color: #3a81c3;">(</span>org-agenda <span style="color: #2d9574;">"a"</span> <span style="color: #2d9574;">"a"</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Let's install some helpful views for our agenda.
</p>
<ul class="org-ul">
<li><p>
<code>C-c a c</code>: See completed tasks at the end of the day and archive them.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Pressing &#8216;c&#8217; in the org-agenda view shows all completed tasks,</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">which should be archived.</span>
<span style="color: #3a81c3;">(</span>add-to-list 'org-agenda-custom-commands 
  '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"c"</span> todo <span style="color: #2d9574;">"DONE|ON_HOLD|CANCELLED"</span> nil<span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div></li>
<li><p>
<code>C-c a u</code>: See unscheduled, undeadlined, and undated tasks in my todo files.
Which should then be scheduled or archived.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>add-to-list 'org-agenda-custom-commands 
  '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"u"</span> alltodo <span style="color: #2d9574;">""</span> 
     <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>org-agenda-skip-function
        <span style="color: #b1951d;">(</span><span style="color: #3a81c3; font-weight: bold;">lambda</span> <span style="color: #3a81c3;">()</span>
              <span style="color: #3a81c3;">(</span>org-agenda-skip-entry-if 'scheduled 'deadline 'regexp  <span style="color: #2d9574;">"\n]+&gt;"</span><span style="color: #3a81c3;">)</span><span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span>
              <span style="color: #67b11d;">(</span>org-agenda-overriding-header <span style="color: #2d9574;">"Unscheduled TODO entries: "</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div></li>
</ul>
</div>
</div>
<div id="outline-container-orgca9c7a6" class="outline-3">
<h3 id="WorkflowStates"><a id="orgca9c7a6"></a><span class="section-number-3">7.3</span> Workflow States</h3>
<div class="outline-text-3" id="text-WorkflowStates">
<p>
Here are some of my common workflow states, &#x2013;the ‘!’ indicates a timestamp should be generated&#x2013;
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-todo-keywords
      <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">quote</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>sequence <span style="color: #2d9574;">"TODO(t)"</span> <span style="color: #2d9574;">"STARTED(s@/!)"</span> <span style="color: #2d9574;">"|"</span> <span style="color: #2d9574;">"DONE(d/!)"</span><span style="color: #67b11d;">)</span>
              <span style="color: #67b11d;">(</span>sequence <span style="color: #2d9574;">"WAITING(w@/!)"</span> <span style="color: #2d9574;">"ON_HOLD(h@/!)"</span> <span style="color: #2d9574;">"|"</span> <span style="color: #2d9574;">"CANCELLED(c@/!)"</span><span style="color: #67b11d;">)</span>
             <span style="color: #2d9574;">)</span>
      <span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
The <code>@</code> brings up a pop-up to make a local note about why the state changed.
<b>Super cool stuff!</b>
In particular, we transition from <code>TODO</code> to <code>STARTED</code> once 15 minutes, or a
reasonable amount, of work has transpired.
Since all but one state are marked for logging, we could use the 
<code>lognotestate</code> logging facility of org-mode, which prompts for a note 
every time a task’s state is changed.
</p>

<p>
Entering a comment about what I've done, even if it's very little,
feels like I'm getting something done. It's an explicit marker of progress
and motivates me to want to change my task's states more often until I see
it marked <code>DONE</code>.
</p>

<p>
Here's how they are coloured,
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-todo-keyword-faces
      <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">quote</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span><span style="color: #2d9574;">"TODO"</span> <span style="color: #3a81c3;">:foreground</span> <span style="color: #2d9574;">"red"</span> <span style="color: #3a81c3;">:weight</span> bold<span style="color: #67b11d;">)</span>
              <span style="color: #67b11d;">(</span><span style="color: #2d9574;">"STARTED"</span> <span style="color: #3a81c3;">:foreground</span> <span style="color: #2d9574;">"blue"</span> <span style="color: #3a81c3;">:weight</span> bold<span style="color: #67b11d;">)</span>
              <span style="color: #67b11d;">(</span><span style="color: #2d9574;">"DONE"</span> <span style="color: #3a81c3;">:foreground</span> <span style="color: #2d9574;">"forest green"</span> <span style="color: #3a81c3;">:weight</span> bold<span style="color: #67b11d;">)</span>
              <span style="color: #67b11d;">(</span><span style="color: #2d9574;">"WAITING"</span> <span style="color: #3a81c3;">:foreground</span> <span style="color: #2d9574;">"orange"</span> <span style="color: #3a81c3;">:weight</span> bold<span style="color: #67b11d;">)</span>
              <span style="color: #67b11d;">(</span><span style="color: #2d9574;">"ON_HOLD"</span> <span style="color: #3a81c3;">:foreground</span> <span style="color: #2d9574;">"magenta"</span> <span style="color: #3a81c3;">:weight</span> bold<span style="color: #67b11d;">)</span>
              <span style="color: #67b11d;">(</span><span style="color: #2d9574;">"CANCELLED"</span> <span style="color: #3a81c3;">:foreground</span> <span style="color: #2d9574;">"forest green"</span> <span style="color: #3a81c3;">:weight</span> bold<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Now we press <code>C-c C-t</code> then the letter shortcut to actually make the state of an org heading.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-use-fast-todo-selection t<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
We can also change through states using Shift- left, or right.
</p>

<p>
Let's draw a state diagram to show what such a workflow looks like.
</p>

<p>
<a href="http://plantuml.com/index">PlantUML</a> supports drawing diagrams in a tremendously simple format
&#x2013;it even supports Graphviz/DOT directly and many other formats.
Super simple setup instructions can be found <a href="http://eschulte.github.io/babel-dev/DONE-integrate-plantuml-support.html">here</a>; below are a bit more
involved instructions. Read the manual <a href="http://plantuml.com/guide">here</a>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Install the tool</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(async-shell-command "brew cask install java") ;; Dependency</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(async-shell-command "brew install plantuml")</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Tell emacs where it is.</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">E.g., (async-shell-command "find / -name plantuml.jar")</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-plantuml-jar-path
      <span style="color: #6c3163;">(</span>expand-file-name <span style="color: #2d9574;">"/usr/local/./Cellar/plantuml/1.2019.3/libexec/plantuml.jar"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Enable C-c C-c to generate diagrams from plantuml src blocks.</span>
<span style="color: #3a81c3;">(</span>add-to-list 'org-babel-load-languages '<span style="color: #6c3163;">(</span>plantuml . t<span style="color: #6c3163;">)</span> <span style="color: #3a81c3;">)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(require 'ob-plantuml)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Use fundamental mode when editing plantuml blocks with C-c '</span>
<span style="color: #3a81c3;">(</span>add-to-list 'org-src-lang-modes <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">quote</span> <span style="color: #2d9574;">(</span><span style="color: #2d9574;">"plantuml"</span> . fundamental<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Let's use this! 
</p>

<div class="org-src-container">
<pre class="src src-plantuml">skinparam defaultTextAlignment center  /' Text alignment '/

skinparam titleBorderRoundCorner 15
skinparam titleBorderThickness 2
skinparam titleBorderColor red
skinparam titleBackgroundColor Aqua-CadetBlue
title My Personal Task States

[*] -&gt; Todo  /' This is my starting state '/
Done -right-&gt; [*]  /' This is an end state '/
Cancelled -up-&gt; [*]  /' This is an end state '/

/'A task is &#8220;Todo&#8221;, then it's &#8220;started&#8221;, then finally it's &#8220;done&#8221;. '/
Todo    -right-&gt; Started
Started -down-&gt;  Waiting
Waiting -up-&gt;    Started
Started -right-&gt; Done

/'Along the way, I may put pause the task for some reason then
  return to it. This may be since I'm &#8220;Blocked&#8221; since I need 
  something, or the task has been put on &#8220;hold&#8221; since it may not
  be important right now, and it may be &#8220;cancelled&#8221; eventually.  
'/

Todo    -down-&gt; Waiting
Waiting -up-&gt; Todo
Waiting -up-&gt; Done

Todo -down-&gt; On_Hold
On_Hold -&gt; Todo

On_Hold -down-&gt; Cancelled
Waiting -down-&gt; Cancelled
Todo    -down-&gt; Cancelled

/' The Org-mode shortcuts for these states are as follows. '/
Todo      : t
On_Hold   : h
Started   : s
Waiting   : w
Cancelled : c
Done      : d

/' If a task is paused, we should document why this is the case. '/
note right of Waiting: Note what is\nblocking us.
note right of Cancelled: Note reason\nfor cancellation.
note bottom of On_Hold: Note reason\nfor reduced priority.

center footer  &#9829;&#8255;&#9829; Org-mode is so cool (&#8226;&#768;&#7447;&#8226;&#769;)&#1608;
/' Note that we could omit the &#8220;center, left, right&#8221; if we wished, 
   or used a &#8220;header&#8221; instead.'/
</pre>
</div>

<img src="../assets/img/workflow.png" alt="My Personal Task States">

<p>
Of note:
</p>
<ul class="org-ul">
<li>Multiline comments are with <code>/' comment here '/</code>, single quote starts a one-line comment.</li>
<li>Nodes don't need to be declared, whose names may contain spaces if they are enclosed in double-quotes.</li>
<li><p>
One forms an arrow between two nodes by writing a line with <code>x -&gt;[label here] y</code>
or <code>y &lt;- x</code>; or using <code>--&gt;</code> and <code>&lt;--</code> for dashed lines. The label is optional.
</p>

<p>
To enforce a particular layout, use <code>-X-&gt;</code> where <code>X ∈ {up, down, right, left}</code>.
</p></li>

<li>To declare that a node <code>x</code> has fields <code>d, f</code> we make two new lines having
<code>x : f</code> and <code>x : d</code>.</li>
<li>One adds a note by a node <code>x</code> as follows: <code>note right of x: words then newline\nthen more words</code>.
Likewise for notes on the <code>left, top, bottom</code>.

<ul class="org-ul">
<li>Interesting sprites and many other things can be done with PlantUML. Read the docs.</li>
</ul></li>
</ul>

<p>
This particular workflow is inspired by <a href="http://doc.norang.ca/org-mode.html">Bernt Hansen</a>
&#x2013;while quickly searching through the PlantUML <a href="http://plantuml.com/guide">manual</a>:
The above is known as an “activity diagram” and it's covered in §4.
</p>
</div>
</div>

<div id="outline-container-orgd01d535" class="outline-3">
<h3 id="orgd01d535"><span class="section-number-3">7.4</span> Clocking Work Time</h3>
<div class="outline-text-3" id="text-7-4">
<p>
Let's keep track of the time we spend working on tasks that we may have captured for ourselves the previous day.
Such statistics provides a good idea of how long it actually takes me to accomplish a certain task in the future
and it lets me know where my time has gone.
</p>

<dl class="org-dl">
<dt>Clock in</dt><dd>on a heading with <code>I</code>, or in the subtree with <code>C-c C-x C-i</code>.</dd>
<dt>Clock out</dt><dd>of a heading with <code>O</code>, or in the subtree with <code>C-c C-x C-o</code>.</dd>
<dt>Clock report</dt><dd>See clocked times with <code>C-c C-x C-r</code>.</dd>
</dl>

<p>
After clocking out, the start and end times, as well as the elapsed time, are added to a drawer
to the heading. We can punch in and out of tasks as many times as desired, say we took a break or
switched to another task, and they will all be recorded into the drawer.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Record a note on what was acciomplished when clocking out of an item.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-log-note-clock-out t<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
To get started, we could estimate how long a task will take and clock-in;
then clock-out and see how long it actually took.
</p>

<p>
Moreover, we can overlay due dates and priorities to tasks in a non-intrusive way that is
easy to edit by hand.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">List of all the files where todo items can be found. Only one for now.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-agenda-files '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"~/Dropbox/todo.org"</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">How many days ahead the default agenda view should look</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-agenda-ndays 7<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">How many days early a deadline item will begin showing up in your agenda list.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-deadline-warning-days 14<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">In the agenda view, days that have no associated tasks will still have a line showing the date. </span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-agenda-show-all-dates t<span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-agenda-skip-deadline-if-done t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Scheduled items marked as complete will not show up in your agenda view.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-agenda-skip-scheduled-if-done t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">The agenda view &#8211; even in the 7-days-at-a-time view &#8211; will always begin on the current day. </span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">This is important, since while using org-mode as a day planner, you never want to think of </span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">days gone past. That&#8217;s something you do in other ways, such as when reviewing completed tasks.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-agenda-start-on-weekday nil<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Sometimes, at the beginning at least, I would accidentally invoke the transposed
command <code>C-x C-c</code>, which saves all buffers and quits Emacs. So here's a helpful
way to ensure I don't quit Emacs accidentally.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span>global-set-key <span style="color: #6c3163;">(</span>kbd <span style="color: #2d9574;">"C-x C-c"</span><span style="color: #6c3163;">)</span> '<span style="color: #6c3163;">(</span>lambda <span style="color: #2d9574;">()</span> <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #2d9574;">)</span> 
  <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">when</span> <span style="color: #67b11d;">(</span>yes-or-no-p <span style="color: #2d9574;">"Do you really want to quit Emacs? "</span><span style="color: #67b11d;">)</span> 
        <span style="color: #67b11d;">(</span>save-buffers-kill-terminal<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
  <span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Resume clocking task when emacs is restarted</span>
<span style="color: #3a81c3;">(</span>org-clock-persistence-insinuate<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Show lot of clocking history</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-clock-history-length 23<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Resume clocking task on clock-in if the clock is open</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-clock-in-resume t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-clock-out-remove-zero-time-clocks t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Clock out when moving task to a done state</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-clock-out-when-done t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Save the running clock and all clock history when exiting Emacs, load it on startup</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-clock-persist t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Do not prompt to resume an active clock</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(setq org-clock-persist-query-resume nil)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Include current clocking task in clock reports</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-clock-report-include-clocking-task t<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
<b>Finding tasks to clock in</b>
Use one of the following options, with the top-most being the first to be tried.
</p>
<ul class="org-ul">
<li>From anywhere, <code>C-u C-c C-x C-i</code> yields a pop-up for recently clocked in tasks.</li>
<li>Pick something off today's agenda scheduled items.</li>
<li>Pick a <code>Started</code> task from the agenda view, work on this unfinished task.</li>
<li>Pick something from the <code>TODO</code> tasks list in the agenda view.</li>
</ul>

<ul class="org-ul">
<li><code>C-c C-x C-d</code> also provides a quick summary of clocked time for the current org file.</li>
</ul>

<p>
<b>Estimates versus actual time</b>
Before clocking into a task, add to the properties drawer <code>:Effort: 1:25</code> or <code>C-c C-x C-e</code>, for a task
that you estimate will take an hour an twenty-five minutes, for example. Now the modeline
will have will mention the time elapsed alongside the task name.
</p>
<ul class="org-ul">
<li><p>
This is also useful when you simply want to put a time limit on a task that wont be
completed anytime soon, say writing a thesis or a long article, but you still want
to work on it for an hour a day and be warned when you exceed such a time constraint.
</p>

<p>
When you've gone above your estimate time, the modeline shows it to be red.
</p></li>
</ul>
</div>
</div>

<div id="outline-container-org7bba67d" class="outline-3">
<h3 id="org7bba67d"><span class="section-number-3">7.5</span> Coloured LaTeX using Minted</h3>
<div class="outline-text-3" id="text-7-5">
<p>
Execute the following for bib ref as well as minted
Org-mode uses the Minted package for source code highlighting in PDF/LaTeX
&#x2013;which in turn requires the pygmentize system tool.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-latex-listings 'minted
      org-latex-packages-alist '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">(</span><span style="color: #2d9574;">""</span> <span style="color: #2d9574;">"minted"</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span>
      org-latex-pdf-process
      '<span style="color: #6c3163;">(</span><span style="color: #2d9574;">"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"</span>
        <span style="color: #2d9574;">"biber %b"</span>
        <span style="color: #2d9574;">"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"</span>
        <span style="color: #2d9574;">"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"</span><span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
For faster pdf generation, may consider invoking:
</p>
<pre class="example">
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"))
</pre>
</div>
</div>

<div id="outline-container-org66b0af1" class="outline-3">
<h3 id="org66b0af1"><span class="section-number-3">7.6</span> Editing &amp; Special Key Handling</h3>
<div class="outline-text-3" id="text-7-6">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">On an org-heading, C-a goes to after the star, heading markers.</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">To use speed keys, run C-a C-a to get to the star markers.</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;;</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-e goes to the end of the heading, not including the tags.</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;;</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-special-ctrl-a/e t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-k no longer removes tags, if activated in the middle of a heading's name.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-special-ctrl-k t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">When you yank a subtree and paste it alongside a subtree of depth &#8216;d&#8217;,</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">then the yanked tree's depth is adjusted to become depth &#8216;d&#8217; as well.</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">If you don't want this, then refile instead of copy pasting.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-yank-adjusted-subtrees t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">adds alphabetical lists like</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;;  </span><span style="color: #2aa1ae; background-color: #ecf3ec;">a. item one</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;;  </span><span style="color: #2aa1ae; background-color: #ecf3ec;">b. item two</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-alphabetical-lists t<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>
<div id="outline-container-org3489f03" class="outline-3">
<h3 id="org3489f03"><span class="section-number-3">7.7</span> Executing code from <code>src</code> blocks</h3>
<div class="outline-text-3" id="text-7-7">
<p>
For example, to execute a shell command in emacs,
write a <code>src</code> with a shell command, then <code>C-c c-c</code> to see the results.
Emacs will generally query you to ensure you're sure about executing the
(possibly dangerous) code block; let's stop that:
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Seamless use of babel: No confirmation upon execution.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-confirm-babel-evaluate nil<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
A worked out example can be obtained as follows: <code>&lt;g TAB</code> then <code>C-c C-C</code> to make a nice
simple graph &#x2013;the code for this is in the next section.
</p>

<p>
Some initial languages we want org-babel to support:
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"> <span style="color: #3a81c3;">(</span>org-babel-do-load-languages
   'org-babel-load-languages
   '<span style="color: #6c3163;">(</span>
     <span style="color: #2d9574;">(</span>emacs-lisp . t<span style="color: #2d9574;">)</span>
     <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(shell   . t)</span>
     <span style="color: #2d9574;">(</span>python . t<span style="color: #2d9574;">)</span>
     <span style="color: #2d9574;">(</span>haskell . t<span style="color: #2d9574;">)</span>
     <span style="color: #2d9574;">(</span>ruby       . t<span style="color: #2d9574;">)</span>
     <span style="color: #2d9574;">(</span>ocaml      . t<span style="color: #2d9574;">)</span>
     <span style="color: #2d9574;">(</span>dot        . t<span style="color: #2d9574;">)</span>
     <span style="color: #2d9574;">(</span>latex      . t<span style="color: #2d9574;">)</span>
     <span style="color: #2d9574;">(</span>org        . t<span style="color: #2d9574;">)</span>
     <span style="color: #2d9574;">(</span>makefile   . t<span style="color: #2d9574;">)</span>
     <span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-src-preserve-indentation t<span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
More languages can be added using <code>add-to-list</code>.
</p>
</div>
</div>

<div id="outline-container-org70b330c" class="outline-3">
<h3 id="org70b330c"><span class="section-number-3">7.8</span> Hiding Emphasise Markers &amp; Inlining Images</h3>
<div class="outline-text-3" id="text-7-8">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">org-mode math is now highlighted ;-)</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-highlight-latex-and-related '<span style="color: #6c3163;">(</span>latex<span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Hide the *,=,/ markers</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-hide-emphasis-markers t<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(setq org-pretty-entities t) </span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Let's set inline images.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-display-inline-images t<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-redisplay-inline-images t<span style="color: #3a81c3;">)</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-startup-with-inline-images <span style="color: #2d9574;">"inlineimages"</span><span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Automatically convert LaTeX fragments to inline images.</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> org-startup-with-latex-preview t<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-orgeb33d31" class="outline-3">
<h3 id="orgeb33d31"><span class="section-number-3">7.9</span> Jumping without hassle</h3>
<div class="outline-text-3" id="text-7-9">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">org-goto-line</span> <span style="color: #6c3163;">(</span>line<span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"Go to the indicated line, unfolding the parent Org header.</span>

<span style="color: #da8b55;">   Implementation: Go to the line, then look at the 1st previous</span>
<span style="color: #da8b55;">   org header, now we can unfold it whence we do so, then we go</span>
<span style="color: #da8b55;">   back to the line we want to be at.</span>
<span style="color: #da8b55;">  "</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span>goto-line line<span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span>org-previous-visible-heading 1<span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span>org-cycle<span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span>goto-line line<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-orgbb6f4be" class="outline-3">
<h3 id="orgbb6f4be"><span class="section-number-3">7.10</span> Folding within a subtree</h3>
<div class="outline-text-3" id="text-7-10">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #2aa1ae; background-color: #ecf3ec;">; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">https://orgmode.org/manual/Structure-editing.html</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">(describe-symbol 'save-excursion)</span>
<span style="color: #2aa1ae; background-color: #ecf3ec;">;</span>
<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">org-fold-current-subtree-anywhere-in-it</span> <span style="color: #6c3163;">()</span>
  <span style="color: #da8b55;">"Hide the current heading, while being anywhere inside it."</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #6c3163;">)</span>
  <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">save-excursion</span>
    <span style="color: #2d9574;">(</span>org-narrow-to-subtree<span style="color: #2d9574;">)</span>
    <span style="color: #2d9574;">(</span>org-shifttab<span style="color: #2d9574;">)</span>
    <span style="color: #2d9574;">(</span>widen<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">FIXME: Make this buffer specfic!</span>
<span style="color: #3a81c3;">(</span>global-set-key <span style="color: #6c3163;">(</span>kbd <span style="color: #2d9574;">"C-c C-h"</span><span style="color: #6c3163;">)</span> 'org-fold-current-subtree-anywhere-in-it<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-org7286c5e" class="outline-3">
<h3 id="org7286c5e"><span class="section-number-3">7.11</span> Making then opening html's from org's</h3>
<div class="outline-text-3" id="text-7-11">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">cl-defun</span> <span style="color: #6c3163; font-weight: bold;">my/org-html-export-to-html</span> <span style="color: #6c3163;">(</span><span style="color: #ba2f59; font-weight: bold;">&amp;optional</span> <span style="color: #2d9574;">(</span>filename <span style="color: #67b11d;">(</span>buffer-name<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"Produce an HTML from the given &#8216;</span><span style="color: #4e3163;">filename</span><span style="color: #da8b55;">&#8217;, or otherwise current buffer,</span>
<span style="color: #da8b55;">   then open it in my default brower.</span>
<span style="color: #da8b55;">  "</span>
 <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #6c3163;">)</span>
 <span style="color: #6c3163;">(</span>org-html-export-to-html<span style="color: #6c3163;">)</span>
 <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">let</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>it <span style="color: #b1951d;">(</span>concat <span style="color: #3a81c3;">(</span>file-name-sans-extension buffer-file-name<span style="color: #3a81c3;">)</span> <span style="color: #2d9574;">".html"</span><span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
   <span style="color: #2d9574;">(</span>browse-url it<span style="color: #2d9574;">)</span>
   <span style="color: #2d9574;">(</span>message <span style="color: #67b11d;">(</span>concat it <span style="color: #2d9574;">" has been opened in Chromium."</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
   'success <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">otherwise we obtain a "compiler error".</span>
 <span style="color: #6c3163;">)</span> 
<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-orge433dbe" class="outline-3">
<h3 id="orge433dbe"><span class="section-number-3">7.12</span> Making then opening pdf's from org's</h3>
<div class="outline-text-3" id="text-7-12">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">cl-defun</span> <span style="color: #6c3163; font-weight: bold;">my/org-latex-export-to-pdf</span> <span style="color: #6c3163;">(</span><span style="color: #ba2f59; font-weight: bold;">&amp;optional</span> <span style="color: #2d9574;">(</span>filename <span style="color: #67b11d;">(</span>buffer-name<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"Produce a PDF from the given &#8216;</span><span style="color: #4e3163;">filename</span><span style="color: #da8b55;">&#8217;, or otherwise current buffer,</span>
<span style="color: #da8b55;">   then open it in my default viewer.</span>
<span style="color: #da8b55;">  "</span>
 <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">interactive</span><span style="color: #6c3163;">)</span>
 <span style="color: #6c3163;">(</span>org-latex-export-to-pdf<span style="color: #6c3163;">)</span>
 <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">let</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>it <span style="color: #b1951d;">(</span>concat <span style="color: #3a81c3;">(</span>file-name-sans-extension filename<span style="color: #3a81c3;">)</span> <span style="color: #2d9574;">".pdf"</span><span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
   <span style="color: #2d9574;">(</span>eshell-command <span style="color: #67b11d;">(</span>concat <span style="color: #2d9574;">"open "</span> it  <span style="color: #2d9574;">" &amp; "</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span>
   <span style="color: #6c3163;">(</span>message <span style="color: #2d9574;">(</span>concat it <span style="color: #2d9574;">" has been opened in your PDF viewer."</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span>
   'success <span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">otherwise we obtain a "compiler error".</span>
<span style="color: #3a81c3;">)</span>
</pre>
</div>
</div>
</div>

<div id="outline-container-org839c7a4" class="outline-3">
<h3 id="org839c7a4"><span class="section-number-3">7.13</span> Interpret the Haskell source blocks in a file</h3>
<div class="outline-text-3" id="text-7-13">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defvar</span> <span style="color: #715ab1;">*current-module*</span> <span style="color: #2d9574;">"NoModuleNameSpecified"</span> 
  <span style="color: #da8b55;">"The name of the module, file, that source blocks are </span>
<span style="color: #da8b55;">   currently being tangled to.</span>

<span style="color: #da8b55;">   This technique is insipired by &#8220;Interactive Way to C&#8221;;</span>
<span style="color: #da8b55;">   see https://alhassy.github.io/InteractiveWayToC/.</span>
<span style="color: #da8b55;">  "</span><span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">current-module</span> <span style="color: #6c3163;">()</span> 
  <span style="color: #da8b55;">"Returns the current module under focus."</span> 
  *current-module*<span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">defun</span> <span style="color: #6c3163; font-weight: bold;">set-module</span> <span style="color: #6c3163;">(</span>name<span style="color: #6c3163;">)</span>
   <span style="color: #da8b55;">"Set the name of the module currently under focus.</span>

<span style="color: #da8b55;">    Usage: When a module is declared, i.e., a new file has begun,</span>
<span style="color: #da8b55;">    then that source blocks header should be &#8220;:tangle (set-module &#8221;name-here&#8221;)&#8221;.</span>
<span style="color: #da8b55;">    succeeding source blocks now inherit this name and so are tangled</span>
<span style="color: #da8b55;">    to the same module file. How? By placing the following line at the top</span>
<span style="color: #da8b55;">    of your Org file: &#8220;&#8216;#+PROPERTY: header-args :tangle (current-module))&#8217;.</span>

<span style="color: #da8b55;">    This technique structures &#8220;Interactive Way to C&#8221;.</span>
<span style="color: #da8b55;">   "</span>
   <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">setq</span> *current-module* name<span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>

<span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">cl-defun</span> <span style="color: #6c3163; font-weight: bold;">my/org-run-haskell</span> <span style="color: #6c3163;">(</span><span style="color: #ba2f59; font-weight: bold;">&amp;optional</span> target <span style="color: #2d9574;">(</span>filename <span style="color: #67b11d;">(</span>buffer-name<span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span>
  <span style="color: #da8b55;">"Tangle Haskell source blocks of given &#8216;</span><span style="color: #4e3163;">filename</span><span style="color: #da8b55;">&#8217;, or otherwise current buffer,</span>
<span style="color: #da8b55;">   and load the resulting &#8216;</span><span style="color: #4e3163;">target</span><span style="color: #da8b55;">&#8217; file into a ghci buffer.</span>

<span style="color: #da8b55;">   If no name is provided for the &#8216;</span><span style="color: #4e3163;">target</span><span style="color: #da8b55;">&#8217; file that is generated from the</span>
<span style="color: #da8b55;">   tangeling process, it is assumed to be the buffer's name with a &#8216;</span><span style="color: #4e3163;">hs</span><span style="color: #da8b55;">&#8217; extension.</span>

<span style="color: #da8b55;">   Note that this only loads the blocks tangled to &#8216;</span><span style="color: #4e3163;">target</span><span style="color: #da8b55;">&#8217;.</span>

<span style="color: #da8b55;">   For example, file &#8216;</span><span style="color: #4e3163;">X.org</span><span style="color: #da8b55;">&#8217; may have haskell blocks that tangle to files</span>
<span style="color: #da8b55;">   &#8216;</span><span style="color: #4e3163;">X.hs</span><span style="color: #da8b55;">&#8217;, &#8216;</span><span style="color: #4e3163;">Y.hs</span><span style="color: #da8b55;">&#8217; and &#8216;</span><span style="color: #4e3163;">Z.hs</span><span style="color: #da8b55;">&#8217;. If no target name is supplied, we tangle all blocks</span>
<span style="color: #da8b55;">   but only load &#8216;</span><span style="color: #4e3163;">X.hs</span><span style="color: #da8b55;">&#8217; into the ghci buffer. A helpful technique to load the</span>
<span style="color: #da8b55;">   last, bottom most, defined haskell module, is to have the module declaration's</span>
<span style="color: #da8b55;">   source block be &#8216;:tangle (setq CODE &#8220;Y.hs&#8221;)&#8217;, for example; then the following</span>
<span style="color: #da8b55;">   code blocks will inherit this location provided our Org file has at the top</span>
<span style="color: #da8b55;">   &#8216;#+PROPERTY: header-args :tangle (current-module))&#8217;.</span>
<span style="color: #da8b55;">   Finally, our &#8216;</span><span style="color: #4e3163;">compile-command</span><span style="color: #da8b55;">&#8217; suffices to be &#8216;(my/org-run-haskell CODE)&#8217;.</span>
<span style="color: #da8b55;">   &#9472;</span>
<span style="color: #da8b55;">   This technique structures &#8220;Interactive Way to C&#8221;.</span>
<span style="color: #da8b55;">  "</span>
   <span style="color: #6c3163;">(</span><span style="color: #3a81c3; font-weight: bold;">let*</span> <span style="color: #2d9574;">(</span><span style="color: #67b11d;">(</span>it  <span style="color: #b1951d;">(</span><span style="color: #3a81c3; font-weight: bold;">if</span> target target <span style="color: #3a81c3;">(</span>concat <span style="color: #6c3163;">(</span>file-name-sans-extension filename<span style="color: #6c3163;">)</span> <span style="color: #2d9574;">".hs"</span><span style="color: #3a81c3;">)</span><span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span>
         <span style="color: #67b11d;">(</span>buf <span style="color: #b1951d;">(</span>concat <span style="color: #2d9574;">"*GHCI* "</span> it<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>

     <span style="color: #2d9574;">(</span><span style="color: #3a81c3; font-weight: bold;">toggle</span> kill-buffer-query-functions nil <span style="color: #67b11d;">(</span><span style="color: #3a81c3; font-weight: bold;">ignore-errors</span> <span style="color: #b1951d;">(</span>kill-buffer buf<span style="color: #b1951d;">)</span><span style="color: #67b11d;">)</span><span style="color: #2d9574;">)</span>
     <span style="color: #2d9574;">(</span>org-babel-tangle it <span style="color: #2d9574;">"haskell"</span><span style="color: #2d9574;">)</span>
     <span style="color: #2d9574;">(</span>async-shell-command <span style="color: #67b11d;">(</span>concat <span style="color: #2d9574;">"ghci "</span> it<span style="color: #67b11d;">)</span> buf<span style="color: #2d9574;">)</span> 
     <span style="color: #2d9574;">(</span>switch-to-buffer-other-window buf<span style="color: #2d9574;">)</span>
     <span style="color: #2d9574;">(</span>end-of-buffer<span style="color: #2d9574;">)</span>
   <span style="color: #6c3163;">)</span>
<span style="color: #3a81c3;">)</span>

<span style="color: #2aa1ae; background-color: #ecf3ec;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Set this as the &#8216;</span><span style="color: #4e3163; background-color: #ecf3ec;">compile-command</span><span style="color: #2aa1ae; background-color: #ecf3ec;">&#8217; in &#8216;Local Variables&#8217;, for example.</span>
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-org5bd4969" class="outline-2">
<h2 id="org5bd4969"><span class="section-number-2">8</span> Summary of Utilities Provided</h2>
<div class="outline-text-2" id="text-8">
<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><span class="underline">Command</span></td>
<td class="org-left"><span class="underline">Action</span></td>
</tr>

<tr>
<td class="org-left"><code>C-c C-m</code></td>
<td class="org-left">recompile file</td>
</tr>

<tr>
<td class="org-left"><code>&lt;f5&gt;</code></td>
<td class="org-left">revert buffer</td>
</tr>

<tr>
<td class="org-left"><code>M-x k</code></td>
<td class="org-left">kill to start of line</td>
</tr>

<tr>
<td class="org-left"><code>C-∣</code></td>
<td class="org-left">toggle 2 windows from horizontal to vertical view</td>
</tr>

<tr>
<td class="org-left"><code>(file-as-list   pathHere)</code></td>
<td class="org-left">construe a file as a list of lines</td>
</tr>

<tr>
<td class="org-left"><code>(file-as-string pathHere)</code></td>
<td class="org-left">construe a file as a string</td>
</tr>

<tr>
<td class="org-left">(<code>re-replace-in-file file regex whatDo)</code></td>
<td class="org-left">perform an in-file regular expression rewrite</td>
</tr>

<tr>
<td class="org-left"><code>(mapsto this that)</code></td>
<td class="org-left">regex rewrite in current buffer: this ↦ that</td>
</tr>

<tr>
<td class="org-left"><code>M-x create-scratch-buffer</code></td>
<td class="org-left">&#x2013;self evident--</td>
</tr>

<tr>
<td class="org-left"><code>M-x kill-other-buffers</code></td>
<td class="org-left">&#x2013;self evident--</td>
</tr>

<tr>
<td class="org-left"><code>M-$</code></td>
<td class="org-left">check spelling of word at point</td>
</tr>

<tr>
<td class="org-left"><code>M-#</code></td>
<td class="org-left">thesaurus look-up word at point</td>
</tr>

<tr>
<td class="org-left"><code>(toggle name val ⋯)</code></td>
<td class="org-left"><i>Effectfully</i> set <code>name</code> to <code>val</code> only for scope <code>⋯</code>.</td>
</tr>

<tr>
<td class="org-left"><code>(my/org-run-haskell &amp;optional file)</code></td>
<td class="org-left">Interpret the Haskell org-blocks from a file into ghci.</td>
</tr>

<tr>
<td class="org-left"><code>C-+/-</code></td>
<td class="org-left">increase/decrease text size</td>
</tr>

<tr>
<td class="org-left"><code>M-x my-org-html-export-to-html</code></td>
<td class="org-left">make then open html from an org file</td>
</tr>

<tr>
<td class="org-left"><code>C-c C-c</code></td>
<td class="org-left">execute code in an org <code>src</code> block</td>
</tr>

<tr>
<td class="org-left"><code>&lt;E</code></td>
<td class="org-left">produce an emacs-lisp <code>src</code> block</td>
</tr>

<tr>
<td class="org-left"><code>&lt;g</code></td>
<td class="org-left">produce a graph template <code>src</code> block</td>
</tr>

<tr>
<td class="org-left"><code>C-x t</code></td>
<td class="org-left">open a new untitled org template file</td>
</tr>

<tr>
<td class="org-left"><code>(org-keywords)</code></td>
<td class="org-left">get <code>#+Property: Value</code> pairs from an org file</td>
</tr>

<tr>
<td class="org-left"><code>(org-keyword property)</code></td>
<td class="org-left">get the <code>value</code> of a given org <code>#+property</code></td>
</tr>
</tbody>
</table>

<p>
Since I'm using <code>use-package</code>, I can invoke <code>M-x describe-personal-keybindings</code> to see what key bindings I've defined.
Since not all my bindings are via <code>use-package</code>, it does not yet cover all of my bindings.
</p>

<p>
We could run <code>C-h b</code> to see our bindings:
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #3a81c3;">(</span><span style="color: #3a81c3; font-weight: bold;">use-package</span> <span style="color: #4e3163;">helm-descbinds</span>
  <span style="color: #3a81c3;">:ensure</span> t
  <span style="color: #3a81c3;">:defer</span> t
  <span style="color: #3a81c3;">:bind</span> <span style="color: #6c3163;">(</span><span style="color: #2d9574;">(</span><span style="color: #2d9574;">"C-h b"</span> . helm-descbinds<span style="color: #2d9574;">)</span><span style="color: #6c3163;">)</span><span style="color: #3a81c3;">)</span>
</pre>
</div>

<p>
Some possibly interesting reads:
</p>
<ul class="org-ul">
<li><a href="https://to1ne.gitlab.io/literate-dotfiles/">Toon's Literate Dotfiles</a></li>
<li><a href="https://github.com/emacs-tw/awesome-emacs">Awesome Emacs</a>: A community driven list of useful Emacs packages, libraries and others.</li>
<li><a href="https://github.com/caisah/emacs.dz">A list of people's nice emacs config files</a></li>
<li><a href="https://zzamboni.org/post/my-emacs-configuration-with-commentary/">zzamboni's configuration file with commentary</a></li>
<li>Karl Voit's article <a href="https://karl-voit.at/2017/06/03/emacs-org/">My Emacs Configuration In Org-mode</a>; his init file can be found <a href="https://github.com/novoid/dot-emacs">here</a>.</li>
<li>Holger Schuri's article <a href="http://www.holgerschurig.de/en/emacs-init-tangle/">Efficient Emacs .org ➞ .el tangling</a> 
; his init file can be found <a href="https://bitbucket.org/holgerschurig/emacsconf/src/b06a0f394b9f20cd4e00cfc5a24f7f59db4ba376/config.org?at=master&amp;fileviewer=file-view-default">here</a>.</li>
<li>Arnaud Legrand's article <a href="http://mescal.imag.fr/membres/arnaud.legrand/misc/init.php">Emacs init file written in org-mode</a></li>
<li><a href="https://emacs.stackexchange.com/questions/3143/can-i-use-org-mode-to-structure-my-emacs-or-other-el-configuration-file">Stackexchange: Using org-mode to structure config files</a></li>
<li><a href="https://github.com/erikriverson/org-mode-R-tutorial/blob/master/org-mode-R-tutorial.org">A tutorial on evaluating code within <code>src</code> blocks</a></li>
</ul>
</div>
</div>
</div>
</body>
</html>
