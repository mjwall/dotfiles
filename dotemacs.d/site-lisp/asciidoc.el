


<!DOCTYPE html>
<html>
  <head prefix="og: http://ogp.me/ns# fb: http://ogp.me/ns/fb# githubog: http://ogp.me/ns/fb/githubog#">
    <meta charset='utf-8'>
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <title>asciidoc-el/asciidoc.el at master · metaperl/asciidoc-el · GitHub</title>
    <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub" />
    <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub" />
    <link rel="apple-touch-icon" sizes="57x57" href="/apple-touch-icon-114.png" />
    <link rel="apple-touch-icon" sizes="114x114" href="/apple-touch-icon-114.png" />
    <link rel="apple-touch-icon" sizes="72x72" href="/apple-touch-icon-144.png" />
    <link rel="apple-touch-icon" sizes="144x144" href="/apple-touch-icon-144.png" />
    <link rel="logo" type="image/svg" href="https://github-media-downloads.s3.amazonaws.com/github-logo.svg" />
    <meta property="og:image" content="https://github.global.ssl.fastly.net/images/modules/logos_page/Octocat.png">
    <meta name="hostname" content="fe17.rs.github.com">
    <meta name="ruby" content="ruby 1.9.3p194-tcs-github-tcmalloc (2012-05-25, TCS patched 2012-05-27, GitHub v1.0.32) [x86_64-linux]">
    <link rel="assets" href="https://github.global.ssl.fastly.net/">
    <link rel="xhr-socket" href="/_sockets" />
    
    


    <meta name="msapplication-TileImage" content="/windows-tile.png" />
    <meta name="msapplication-TileColor" content="#ffffff" />
    <meta name="selected-link" value="repo_source" data-pjax-transient />
    <meta content="collector.githubapp.com" name="octolytics-host" /><meta content="github" name="octolytics-app-id" />

    
    
    <link rel="icon" type="image/x-icon" href="/favicon.ico" />

    <meta content="authenticity_token" name="csrf-param" />
<meta content="PAXHU3mSMqI7eoVZ1/MV/Rc9b0eRIjPW+WaZX86Mad0=" name="csrf-token" />

    <link href="https://github.global.ssl.fastly.net/assets/github-2ecec547a18fa787eb84f55fb19e9ba1c121d4f9.css" media="all" rel="stylesheet" type="text/css" />
    <link href="https://github.global.ssl.fastly.net/assets/github2-37e86e426b1b017fee3061a69ac6d5eb2bb1a119.css" media="all" rel="stylesheet" type="text/css" />
    


      <script src="https://github.global.ssl.fastly.net/assets/frameworks-8c90145ced3264909647872c925b3f983056d3d1.js" type="text/javascript"></script>
      <script src="https://github.global.ssl.fastly.net/assets/github-ff4311ad8b825bfecbee3649e06911dcb14cf0d8.js" type="text/javascript"></script>
      
      <meta http-equiv="x-pjax-version" content="98a319bfd734e97b16d19760ac72f33b">

        <link data-pjax-transient rel='permalink' href='/metaperl/asciidoc-el/blob/183d4ad2cb961a01c7b4daaccffef353929fff91/asciidoc.el'>
  <meta property="og:title" content="asciidoc-el"/>
  <meta property="og:type" content="githubog:gitrepository"/>
  <meta property="og:url" content="https://github.com/metaperl/asciidoc-el"/>
  <meta property="og:image" content="https://github.global.ssl.fastly.net/images/gravatars/gravatar-user-420.png"/>
  <meta property="og:site_name" content="GitHub"/>
  <meta property="og:description" content="asciidoc-el - Emacs support for writing Asciidoc"/>

  <meta name="description" content="asciidoc-el - Emacs support for writing Asciidoc" />

  <meta content="21293" name="octolytics-dimension-user_id" /><meta content="metaperl" name="octolytics-dimension-user_login" /><meta content="339698" name="octolytics-dimension-repository_id" /><meta content="metaperl/asciidoc-el" name="octolytics-dimension-repository_nwo" /><meta content="true" name="octolytics-dimension-repository_public" /><meta content="false" name="octolytics-dimension-repository_is_fork" /><meta content="339698" name="octolytics-dimension-repository_network_root_id" /><meta content="metaperl/asciidoc-el" name="octolytics-dimension-repository_network_root_nwo" />
  <link href="https://github.com/metaperl/asciidoc-el/commits/master.atom" rel="alternate" title="Recent Commits to asciidoc-el:master" type="application/atom+xml" />

  </head>


  <body class="logged_out page-blob  vis-public env-production ">

    <div class="wrapper">
      
      
      


      
      <div class="header header-logged-out">
  <div class="container clearfix">

    <a class="header-logo-wordmark" href="https://github.com/">
      <span class="mega-octicon octicon-logo-github"></span>
    </a>

    <div class="header-actions">
        <a class="button primary" href="/signup">Sign up</a>
      <a class="button" href="/login?return_to=%2Fmetaperl%2Fasciidoc-el%2Fblob%2Fmaster%2Fasciidoc.el">Sign in</a>
    </div>

    <div class="command-bar js-command-bar  in-repository">


      <ul class="top-nav">
          <li class="explore"><a href="/explore">Explore</a></li>
        <li class="features"><a href="/features">Features</a></li>
          <li class="enterprise"><a href="https://enterprise.github.com/">Enterprise</a></li>
          <li class="blog"><a href="/blog">Blog</a></li>
      </ul>
        <form accept-charset="UTF-8" action="/search" class="command-bar-form" id="top_search_form" method="get">

<input type="text" data-hotkey="/ s" name="q" id="js-command-bar-field" placeholder="Search or type a command" tabindex="1" autocapitalize="off"
    
    
      data-repo="metaperl/asciidoc-el"
      data-branch="master"
      data-sha="d0ee6709e800c4273104f352416770b3d5710db9"
  >

    <input type="hidden" name="nwo" value="metaperl/asciidoc-el" />

    <div class="select-menu js-menu-container js-select-menu search-context-select-menu">
      <span class="minibutton select-menu-button js-menu-target">
        <span class="js-select-button">This repository</span>
      </span>

      <div class="select-menu-modal-holder js-menu-content js-navigation-container">
        <div class="select-menu-modal">

          <div class="select-menu-item js-navigation-item js-this-repository-navigation-item selected">
            <span class="select-menu-item-icon octicon octicon-check"></span>
            <input type="radio" class="js-search-this-repository" name="search_target" value="repository" checked="checked" />
            <div class="select-menu-item-text js-select-button-text">This repository</div>
          </div> <!-- /.select-menu-item -->

          <div class="select-menu-item js-navigation-item js-all-repositories-navigation-item">
            <span class="select-menu-item-icon octicon octicon-check"></span>
            <input type="radio" name="search_target" value="global" />
            <div class="select-menu-item-text js-select-button-text">All repositories</div>
          </div> <!-- /.select-menu-item -->

        </div>
      </div>
    </div>

  <span class="octicon help tooltipped downwards" title="Show command bar help">
    <span class="octicon octicon-question"></span>
  </span>


  <input type="hidden" name="ref" value="cmdform">

</form>
    </div>

  </div>
</div>


      


          <div class="site" itemscope itemtype="http://schema.org/WebPage">
    
    <div class="pagehead repohead instapaper_ignore readability-menu">
      <div class="container">
        

<ul class="pagehead-actions">


  <li>
  <a href="/login?return_to=%2Fmetaperl%2Fasciidoc-el"
  class="minibutton with-count js-toggler-target star-button entice tooltipped upwards"
  title="You must be signed in to use this feature" rel="nofollow">
  <span class="octicon octicon-star"></span>Star
</a>
<a class="social-count js-social-count" href="/metaperl/asciidoc-el/stargazers">
  6
</a>

  </li>

    <li>
      <a href="/login?return_to=%2Fmetaperl%2Fasciidoc-el"
        class="minibutton with-count js-toggler-target fork-button entice tooltipped upwards"
        title="You must be signed in to fork a repository" rel="nofollow">
        <span class="octicon octicon-git-branch"></span>Fork
      </a>
      <a href="/metaperl/asciidoc-el/network" class="social-count">
        3
      </a>
    </li>
</ul>

        <h1 itemscope itemtype="http://data-vocabulary.org/Breadcrumb" class="entry-title public">
          <span class="repo-label"><span>public</span></span>
          <span class="mega-octicon octicon-repo"></span>
          <span class="author">
            <a href="/metaperl" class="url fn" itemprop="url" rel="author"><span itemprop="title">metaperl</span></a></span
          ><span class="repohead-name-divider">/</span><strong
          ><a href="/metaperl/asciidoc-el" class="js-current-repository js-repo-home-link">asciidoc-el</a></strong>

          <span class="page-context-loader">
            <img alt="Octocat-spinner-32" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
          </span>

        </h1>
      </div><!-- /.container -->
    </div><!-- /.repohead -->

    <div class="container">

      <div class="repository-with-sidebar repo-container ">

        <div class="repository-sidebar">
            

<div class="repo-nav repo-nav-full js-repository-container-pjax js-octicon-loaders">
  <div class="repo-nav-contents">
    <ul class="repo-menu">
      <li class="tooltipped leftwards" title="Code">
        <a href="/metaperl/asciidoc-el" aria-label="Code" class="js-selected-navigation-item selected" data-gotokey="c" data-pjax="true" data-selected-links="repo_source repo_downloads repo_commits repo_tags repo_branches /metaperl/asciidoc-el">
          <span class="octicon octicon-code"></span> <span class="full-word">Code</span>
          <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>

        <li class="tooltipped leftwards" title="Issues">
          <a href="/metaperl/asciidoc-el/issues" aria-label="Issues" class="js-selected-navigation-item js-disable-pjax" data-gotokey="i" data-selected-links="repo_issues /metaperl/asciidoc-el/issues">
            <span class="octicon octicon-issue-opened"></span> <span class="full-word">Issues</span>
            <span class='counter'>1</span>
            <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>        </li>

      <li class="tooltipped leftwards" title="Pull Requests"><a href="/metaperl/asciidoc-el/pulls" aria-label="Pull Requests" class="js-selected-navigation-item js-disable-pjax" data-gotokey="p" data-selected-links="repo_pulls /metaperl/asciidoc-el/pulls">
            <span class="octicon octicon-git-pull-request"></span> <span class="full-word">Pull Requests</span>
            <span class='counter'>0</span>
            <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>


    </ul>
    <div class="repo-menu-separator"></div>
    <ul class="repo-menu">

      <li class="tooltipped leftwards" title="Pulse">
        <a href="/metaperl/asciidoc-el/pulse" aria-label="Pulse" class="js-selected-navigation-item " data-pjax="true" data-selected-links="pulse /metaperl/asciidoc-el/pulse">
          <span class="octicon octicon-pulse"></span> <span class="full-word">Pulse</span>
          <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>

      <li class="tooltipped leftwards" title="Graphs">
        <a href="/metaperl/asciidoc-el/graphs" aria-label="Graphs" class="js-selected-navigation-item " data-pjax="true" data-selected-links="repo_graphs repo_contributors /metaperl/asciidoc-el/graphs">
          <span class="octicon octicon-graph"></span> <span class="full-word">Graphs</span>
          <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>

      <li class="tooltipped leftwards" title="Network">
        <a href="/metaperl/asciidoc-el/network" aria-label="Network" class="js-selected-navigation-item js-disable-pjax" data-selected-links="repo_network /metaperl/asciidoc-el/network">
          <span class="octicon octicon-git-branch"></span> <span class="full-word">Network</span>
          <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>

    </ul>

  </div>
</div>

            <div class="only-with-full-nav">
              

  

<div class="clone-url open"
  data-protocol-type="http"
  data-url="/users/set_protocol?protocol_selector=http&amp;protocol_type=clone">
  <h3><strong>HTTPS</strong> clone URL</h3>

  <input type="text" class="clone js-url-field"
         value="https://github.com/metaperl/asciidoc-el.git" readonly="readonly">

  <span class="js-zeroclipboard url-box-clippy minibutton zeroclipboard-button" data-clipboard-text="https://github.com/metaperl/asciidoc-el.git" data-copied-hint="copied!" title="copy to clipboard"><span class="octicon octicon-clippy"></span></span>
</div>

  

<div class="clone-url "
  data-protocol-type="subversion"
  data-url="/users/set_protocol?protocol_selector=subversion&amp;protocol_type=clone">
  <h3><strong>Subversion</strong> checkout URL</h3>

  <input type="text" class="clone js-url-field"
         value="https://github.com/metaperl/asciidoc-el" readonly="readonly">

  <span class="js-zeroclipboard url-box-clippy minibutton zeroclipboard-button" data-clipboard-text="https://github.com/metaperl/asciidoc-el" data-copied-hint="copied!" title="copy to clipboard"><span class="octicon octicon-clippy"></span></span>
</div>



<p class="clone-options">You can clone with
    <a href="#" class="js-clone-selector" data-protocol="http">HTTPS</a>,
    <a href="#" class="js-clone-selector" data-protocol="subversion">Subversion</a>,
  and <a href="https://help.github.com/articles/which-remote-url-should-i-use">other methods.</a>
</p>



                <a href="/metaperl/asciidoc-el/archive/master.zip"
                   class="minibutton sidebar-button"
                   title="Download this repository as a zip file"
                   rel="nofollow">
                  <span class="octicon octicon-cloud-download"></span>
                  Download ZIP
                </a>
            </div>
        </div><!-- /.repository-sidebar -->

        <div id="js-repo-pjax-container" class="repository-content context-loader-container" data-pjax-container>
          


<!-- blob contrib key: blob_contributors:v21:a145df355cc8eefeca527f21ee81ef63 -->
<!-- blob contrib frag key: views10/v8/blob_contributors:v21:a145df355cc8eefeca527f21ee81ef63 -->

<p title="This is a placeholder element" class="js-history-link-replace hidden"></p>

<a href="/metaperl/asciidoc-el/find/master" data-pjax data-hotkey="t" style="display:none">Show File Finder</a>

<div class="file-navigation">
  


<div class="select-menu js-menu-container js-select-menu" >
  <span class="minibutton select-menu-button js-menu-target" data-hotkey="w"
    data-master-branch="master"
    data-ref="master">
    <span class="octicon octicon-git-branch"></span>
    <i>branch:</i>
    <span class="js-select-button">master</span>
  </span>

  <div class="select-menu-modal-holder js-menu-content js-navigation-container" data-pjax>

    <div class="select-menu-modal">
      <div class="select-menu-header">
        <span class="select-menu-title">Switch branches/tags</span>
        <span class="octicon octicon-remove-close js-menu-close"></span>
      </div> <!-- /.select-menu-header -->

      <div class="select-menu-filters">
        <div class="select-menu-text-filter">
          <input type="text" id="context-commitish-filter-field" class="js-filterable-field js-navigation-enable" placeholder="Filter branches/tags">
        </div>
        <div class="select-menu-tabs">
          <ul>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="branches" class="js-select-menu-tab">Branches</a>
            </li>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="tags" class="js-select-menu-tab">Tags</a>
            </li>
          </ul>
        </div><!-- /.select-menu-tabs -->
      </div><!-- /.select-menu-filters -->

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="branches">

        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <div class="select-menu-item js-navigation-item selected">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/metaperl/asciidoc-el/blob/master/asciidoc.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="master" data-skip-pjax="true" rel="nofollow" title="master">master</a>
            </div> <!-- /.select-menu-item -->
        </div>

          <div class="select-menu-no-results">Nothing to show</div>
      </div> <!-- /.select-menu-list -->

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="tags">
        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


        </div>

        <div class="select-menu-no-results">Nothing to show</div>
      </div> <!-- /.select-menu-list -->

    </div> <!-- /.select-menu-modal -->
  </div> <!-- /.select-menu-modal-holder -->
</div> <!-- /.select-menu -->

  <div class="breadcrumb">
    <span class='repo-root js-repo-root'><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/metaperl/asciidoc-el" data-branch="master" data-direction="back" data-pjax="true" itemscope="url"><span itemprop="title">asciidoc-el</span></a></span></span><span class="separator"> / </span><strong class="final-path">asciidoc.el</strong> <span class="js-zeroclipboard minibutton zeroclipboard-button" data-clipboard-text="asciidoc.el" data-copied-hint="copied!" title="copy to clipboard"><span class="octicon octicon-clippy"></span></span>
  </div>
</div>


  
  <div class="commit file-history-tease">
    <img class="main-avatar" height="24" src="https://secure.gravatar.com/avatar/f0508037c26d9bee6d3ab35a515c7e00?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="24" />
    <span class="author"><span rel="author">Terrence Brannon</span></span>
    <time class="js-relative-date" datetime="2012-04-12T07:29:26-07:00" title="2012-04-12 07:29:26">April 12, 2012</time>
    <div class="commit-title">
        <a href="/metaperl/asciidoc-el/commit/183d4ad2cb961a01c7b4daaccffef353929fff91" class="message" data-pjax="true" title="remove debug-on-error t">remove debug-on-error t</a>
    </div>

    <div class="participation">
      <p class="quickstat"><a href="#blob_contributors_box" rel="facebox"><strong>1</strong> contributor</a></p>
      
    </div>
    <div id="blob_contributors_box" style="display:none">
      <h2 class="facebox-header">Users who have contributed to this file</h2>
      <ul class="facebox-user-list">
        <li class="facebox-user-list-item">
          <img height="24" src="https://secure.gravatar.com/avatar/7618242c9d6ceb32124c9800904c8aa7?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="24" />
          <a href="/metaperl">metaperl</a>
        </li>
      </ul>
    </div>
  </div>


<div id="files" class="bubble">
  <div class="file">
    <div class="meta">
      <div class="info">
        <span class="icon"><b class="octicon octicon-file-text"></b></span>
        <span class="mode" title="File Mode">file</span>
          <span>415 lines (334 sloc)</span>
        <span>12.761 kb</span>
      </div>
      <div class="actions">
        <div class="button-group">
              <a class="minibutton js-entice" href=""
                 data-entice="You must be signed in to make or propose changes">Edit</a>
          <a href="/metaperl/asciidoc-el/raw/master/asciidoc.el" class="button minibutton " id="raw-url">Raw</a>
            <a href="/metaperl/asciidoc-el/blame/master/asciidoc.el" class="button minibutton ">Blame</a>
          <a href="/metaperl/asciidoc-el/commits/master/asciidoc.el" class="button minibutton " rel="nofollow">History</a>
        </div><!-- /.button-group -->
            <a class="minibutton danger empty-icon js-entice" href=""
               data-entice="You must be signed in and on a branch to make or propose changes">
            Delete
          </a>
      </div><!-- /.actions -->

    </div>
        <div class="blob-wrapper data type-emacs-lisp js-blob-data">
      <table class="file-code file-diff">
        <tr class="file-code-line">
          <td class="blob-line-nums">
            <span id="L1" rel="#L1">1</span>
<span id="L2" rel="#L2">2</span>
<span id="L3" rel="#L3">3</span>
<span id="L4" rel="#L4">4</span>
<span id="L5" rel="#L5">5</span>
<span id="L6" rel="#L6">6</span>
<span id="L7" rel="#L7">7</span>
<span id="L8" rel="#L8">8</span>
<span id="L9" rel="#L9">9</span>
<span id="L10" rel="#L10">10</span>
<span id="L11" rel="#L11">11</span>
<span id="L12" rel="#L12">12</span>
<span id="L13" rel="#L13">13</span>
<span id="L14" rel="#L14">14</span>
<span id="L15" rel="#L15">15</span>
<span id="L16" rel="#L16">16</span>
<span id="L17" rel="#L17">17</span>
<span id="L18" rel="#L18">18</span>
<span id="L19" rel="#L19">19</span>
<span id="L20" rel="#L20">20</span>
<span id="L21" rel="#L21">21</span>
<span id="L22" rel="#L22">22</span>
<span id="L23" rel="#L23">23</span>
<span id="L24" rel="#L24">24</span>
<span id="L25" rel="#L25">25</span>
<span id="L26" rel="#L26">26</span>
<span id="L27" rel="#L27">27</span>
<span id="L28" rel="#L28">28</span>
<span id="L29" rel="#L29">29</span>
<span id="L30" rel="#L30">30</span>
<span id="L31" rel="#L31">31</span>
<span id="L32" rel="#L32">32</span>
<span id="L33" rel="#L33">33</span>
<span id="L34" rel="#L34">34</span>
<span id="L35" rel="#L35">35</span>
<span id="L36" rel="#L36">36</span>
<span id="L37" rel="#L37">37</span>
<span id="L38" rel="#L38">38</span>
<span id="L39" rel="#L39">39</span>
<span id="L40" rel="#L40">40</span>
<span id="L41" rel="#L41">41</span>
<span id="L42" rel="#L42">42</span>
<span id="L43" rel="#L43">43</span>
<span id="L44" rel="#L44">44</span>
<span id="L45" rel="#L45">45</span>
<span id="L46" rel="#L46">46</span>
<span id="L47" rel="#L47">47</span>
<span id="L48" rel="#L48">48</span>
<span id="L49" rel="#L49">49</span>
<span id="L50" rel="#L50">50</span>
<span id="L51" rel="#L51">51</span>
<span id="L52" rel="#L52">52</span>
<span id="L53" rel="#L53">53</span>
<span id="L54" rel="#L54">54</span>
<span id="L55" rel="#L55">55</span>
<span id="L56" rel="#L56">56</span>
<span id="L57" rel="#L57">57</span>
<span id="L58" rel="#L58">58</span>
<span id="L59" rel="#L59">59</span>
<span id="L60" rel="#L60">60</span>
<span id="L61" rel="#L61">61</span>
<span id="L62" rel="#L62">62</span>
<span id="L63" rel="#L63">63</span>
<span id="L64" rel="#L64">64</span>
<span id="L65" rel="#L65">65</span>
<span id="L66" rel="#L66">66</span>
<span id="L67" rel="#L67">67</span>
<span id="L68" rel="#L68">68</span>
<span id="L69" rel="#L69">69</span>
<span id="L70" rel="#L70">70</span>
<span id="L71" rel="#L71">71</span>
<span id="L72" rel="#L72">72</span>
<span id="L73" rel="#L73">73</span>
<span id="L74" rel="#L74">74</span>
<span id="L75" rel="#L75">75</span>
<span id="L76" rel="#L76">76</span>
<span id="L77" rel="#L77">77</span>
<span id="L78" rel="#L78">78</span>
<span id="L79" rel="#L79">79</span>
<span id="L80" rel="#L80">80</span>
<span id="L81" rel="#L81">81</span>
<span id="L82" rel="#L82">82</span>
<span id="L83" rel="#L83">83</span>
<span id="L84" rel="#L84">84</span>
<span id="L85" rel="#L85">85</span>
<span id="L86" rel="#L86">86</span>
<span id="L87" rel="#L87">87</span>
<span id="L88" rel="#L88">88</span>
<span id="L89" rel="#L89">89</span>
<span id="L90" rel="#L90">90</span>
<span id="L91" rel="#L91">91</span>
<span id="L92" rel="#L92">92</span>
<span id="L93" rel="#L93">93</span>
<span id="L94" rel="#L94">94</span>
<span id="L95" rel="#L95">95</span>
<span id="L96" rel="#L96">96</span>
<span id="L97" rel="#L97">97</span>
<span id="L98" rel="#L98">98</span>
<span id="L99" rel="#L99">99</span>
<span id="L100" rel="#L100">100</span>
<span id="L101" rel="#L101">101</span>
<span id="L102" rel="#L102">102</span>
<span id="L103" rel="#L103">103</span>
<span id="L104" rel="#L104">104</span>
<span id="L105" rel="#L105">105</span>
<span id="L106" rel="#L106">106</span>
<span id="L107" rel="#L107">107</span>
<span id="L108" rel="#L108">108</span>
<span id="L109" rel="#L109">109</span>
<span id="L110" rel="#L110">110</span>
<span id="L111" rel="#L111">111</span>
<span id="L112" rel="#L112">112</span>
<span id="L113" rel="#L113">113</span>
<span id="L114" rel="#L114">114</span>
<span id="L115" rel="#L115">115</span>
<span id="L116" rel="#L116">116</span>
<span id="L117" rel="#L117">117</span>
<span id="L118" rel="#L118">118</span>
<span id="L119" rel="#L119">119</span>
<span id="L120" rel="#L120">120</span>
<span id="L121" rel="#L121">121</span>
<span id="L122" rel="#L122">122</span>
<span id="L123" rel="#L123">123</span>
<span id="L124" rel="#L124">124</span>
<span id="L125" rel="#L125">125</span>
<span id="L126" rel="#L126">126</span>
<span id="L127" rel="#L127">127</span>
<span id="L128" rel="#L128">128</span>
<span id="L129" rel="#L129">129</span>
<span id="L130" rel="#L130">130</span>
<span id="L131" rel="#L131">131</span>
<span id="L132" rel="#L132">132</span>
<span id="L133" rel="#L133">133</span>
<span id="L134" rel="#L134">134</span>
<span id="L135" rel="#L135">135</span>
<span id="L136" rel="#L136">136</span>
<span id="L137" rel="#L137">137</span>
<span id="L138" rel="#L138">138</span>
<span id="L139" rel="#L139">139</span>
<span id="L140" rel="#L140">140</span>
<span id="L141" rel="#L141">141</span>
<span id="L142" rel="#L142">142</span>
<span id="L143" rel="#L143">143</span>
<span id="L144" rel="#L144">144</span>
<span id="L145" rel="#L145">145</span>
<span id="L146" rel="#L146">146</span>
<span id="L147" rel="#L147">147</span>
<span id="L148" rel="#L148">148</span>
<span id="L149" rel="#L149">149</span>
<span id="L150" rel="#L150">150</span>
<span id="L151" rel="#L151">151</span>
<span id="L152" rel="#L152">152</span>
<span id="L153" rel="#L153">153</span>
<span id="L154" rel="#L154">154</span>
<span id="L155" rel="#L155">155</span>
<span id="L156" rel="#L156">156</span>
<span id="L157" rel="#L157">157</span>
<span id="L158" rel="#L158">158</span>
<span id="L159" rel="#L159">159</span>
<span id="L160" rel="#L160">160</span>
<span id="L161" rel="#L161">161</span>
<span id="L162" rel="#L162">162</span>
<span id="L163" rel="#L163">163</span>
<span id="L164" rel="#L164">164</span>
<span id="L165" rel="#L165">165</span>
<span id="L166" rel="#L166">166</span>
<span id="L167" rel="#L167">167</span>
<span id="L168" rel="#L168">168</span>
<span id="L169" rel="#L169">169</span>
<span id="L170" rel="#L170">170</span>
<span id="L171" rel="#L171">171</span>
<span id="L172" rel="#L172">172</span>
<span id="L173" rel="#L173">173</span>
<span id="L174" rel="#L174">174</span>
<span id="L175" rel="#L175">175</span>
<span id="L176" rel="#L176">176</span>
<span id="L177" rel="#L177">177</span>
<span id="L178" rel="#L178">178</span>
<span id="L179" rel="#L179">179</span>
<span id="L180" rel="#L180">180</span>
<span id="L181" rel="#L181">181</span>
<span id="L182" rel="#L182">182</span>
<span id="L183" rel="#L183">183</span>
<span id="L184" rel="#L184">184</span>
<span id="L185" rel="#L185">185</span>
<span id="L186" rel="#L186">186</span>
<span id="L187" rel="#L187">187</span>
<span id="L188" rel="#L188">188</span>
<span id="L189" rel="#L189">189</span>
<span id="L190" rel="#L190">190</span>
<span id="L191" rel="#L191">191</span>
<span id="L192" rel="#L192">192</span>
<span id="L193" rel="#L193">193</span>
<span id="L194" rel="#L194">194</span>
<span id="L195" rel="#L195">195</span>
<span id="L196" rel="#L196">196</span>
<span id="L197" rel="#L197">197</span>
<span id="L198" rel="#L198">198</span>
<span id="L199" rel="#L199">199</span>
<span id="L200" rel="#L200">200</span>
<span id="L201" rel="#L201">201</span>
<span id="L202" rel="#L202">202</span>
<span id="L203" rel="#L203">203</span>
<span id="L204" rel="#L204">204</span>
<span id="L205" rel="#L205">205</span>
<span id="L206" rel="#L206">206</span>
<span id="L207" rel="#L207">207</span>
<span id="L208" rel="#L208">208</span>
<span id="L209" rel="#L209">209</span>
<span id="L210" rel="#L210">210</span>
<span id="L211" rel="#L211">211</span>
<span id="L212" rel="#L212">212</span>
<span id="L213" rel="#L213">213</span>
<span id="L214" rel="#L214">214</span>
<span id="L215" rel="#L215">215</span>
<span id="L216" rel="#L216">216</span>
<span id="L217" rel="#L217">217</span>
<span id="L218" rel="#L218">218</span>
<span id="L219" rel="#L219">219</span>
<span id="L220" rel="#L220">220</span>
<span id="L221" rel="#L221">221</span>
<span id="L222" rel="#L222">222</span>
<span id="L223" rel="#L223">223</span>
<span id="L224" rel="#L224">224</span>
<span id="L225" rel="#L225">225</span>
<span id="L226" rel="#L226">226</span>
<span id="L227" rel="#L227">227</span>
<span id="L228" rel="#L228">228</span>
<span id="L229" rel="#L229">229</span>
<span id="L230" rel="#L230">230</span>
<span id="L231" rel="#L231">231</span>
<span id="L232" rel="#L232">232</span>
<span id="L233" rel="#L233">233</span>
<span id="L234" rel="#L234">234</span>
<span id="L235" rel="#L235">235</span>
<span id="L236" rel="#L236">236</span>
<span id="L237" rel="#L237">237</span>
<span id="L238" rel="#L238">238</span>
<span id="L239" rel="#L239">239</span>
<span id="L240" rel="#L240">240</span>
<span id="L241" rel="#L241">241</span>
<span id="L242" rel="#L242">242</span>
<span id="L243" rel="#L243">243</span>
<span id="L244" rel="#L244">244</span>
<span id="L245" rel="#L245">245</span>
<span id="L246" rel="#L246">246</span>
<span id="L247" rel="#L247">247</span>
<span id="L248" rel="#L248">248</span>
<span id="L249" rel="#L249">249</span>
<span id="L250" rel="#L250">250</span>
<span id="L251" rel="#L251">251</span>
<span id="L252" rel="#L252">252</span>
<span id="L253" rel="#L253">253</span>
<span id="L254" rel="#L254">254</span>
<span id="L255" rel="#L255">255</span>
<span id="L256" rel="#L256">256</span>
<span id="L257" rel="#L257">257</span>
<span id="L258" rel="#L258">258</span>
<span id="L259" rel="#L259">259</span>
<span id="L260" rel="#L260">260</span>
<span id="L261" rel="#L261">261</span>
<span id="L262" rel="#L262">262</span>
<span id="L263" rel="#L263">263</span>
<span id="L264" rel="#L264">264</span>
<span id="L265" rel="#L265">265</span>
<span id="L266" rel="#L266">266</span>
<span id="L267" rel="#L267">267</span>
<span id="L268" rel="#L268">268</span>
<span id="L269" rel="#L269">269</span>
<span id="L270" rel="#L270">270</span>
<span id="L271" rel="#L271">271</span>
<span id="L272" rel="#L272">272</span>
<span id="L273" rel="#L273">273</span>
<span id="L274" rel="#L274">274</span>
<span id="L275" rel="#L275">275</span>
<span id="L276" rel="#L276">276</span>
<span id="L277" rel="#L277">277</span>
<span id="L278" rel="#L278">278</span>
<span id="L279" rel="#L279">279</span>
<span id="L280" rel="#L280">280</span>
<span id="L281" rel="#L281">281</span>
<span id="L282" rel="#L282">282</span>
<span id="L283" rel="#L283">283</span>
<span id="L284" rel="#L284">284</span>
<span id="L285" rel="#L285">285</span>
<span id="L286" rel="#L286">286</span>
<span id="L287" rel="#L287">287</span>
<span id="L288" rel="#L288">288</span>
<span id="L289" rel="#L289">289</span>
<span id="L290" rel="#L290">290</span>
<span id="L291" rel="#L291">291</span>
<span id="L292" rel="#L292">292</span>
<span id="L293" rel="#L293">293</span>
<span id="L294" rel="#L294">294</span>
<span id="L295" rel="#L295">295</span>
<span id="L296" rel="#L296">296</span>
<span id="L297" rel="#L297">297</span>
<span id="L298" rel="#L298">298</span>
<span id="L299" rel="#L299">299</span>
<span id="L300" rel="#L300">300</span>
<span id="L301" rel="#L301">301</span>
<span id="L302" rel="#L302">302</span>
<span id="L303" rel="#L303">303</span>
<span id="L304" rel="#L304">304</span>
<span id="L305" rel="#L305">305</span>
<span id="L306" rel="#L306">306</span>
<span id="L307" rel="#L307">307</span>
<span id="L308" rel="#L308">308</span>
<span id="L309" rel="#L309">309</span>
<span id="L310" rel="#L310">310</span>
<span id="L311" rel="#L311">311</span>
<span id="L312" rel="#L312">312</span>
<span id="L313" rel="#L313">313</span>
<span id="L314" rel="#L314">314</span>
<span id="L315" rel="#L315">315</span>
<span id="L316" rel="#L316">316</span>
<span id="L317" rel="#L317">317</span>
<span id="L318" rel="#L318">318</span>
<span id="L319" rel="#L319">319</span>
<span id="L320" rel="#L320">320</span>
<span id="L321" rel="#L321">321</span>
<span id="L322" rel="#L322">322</span>
<span id="L323" rel="#L323">323</span>
<span id="L324" rel="#L324">324</span>
<span id="L325" rel="#L325">325</span>
<span id="L326" rel="#L326">326</span>
<span id="L327" rel="#L327">327</span>
<span id="L328" rel="#L328">328</span>
<span id="L329" rel="#L329">329</span>
<span id="L330" rel="#L330">330</span>
<span id="L331" rel="#L331">331</span>
<span id="L332" rel="#L332">332</span>
<span id="L333" rel="#L333">333</span>
<span id="L334" rel="#L334">334</span>
<span id="L335" rel="#L335">335</span>
<span id="L336" rel="#L336">336</span>
<span id="L337" rel="#L337">337</span>
<span id="L338" rel="#L338">338</span>
<span id="L339" rel="#L339">339</span>
<span id="L340" rel="#L340">340</span>
<span id="L341" rel="#L341">341</span>
<span id="L342" rel="#L342">342</span>
<span id="L343" rel="#L343">343</span>
<span id="L344" rel="#L344">344</span>
<span id="L345" rel="#L345">345</span>
<span id="L346" rel="#L346">346</span>
<span id="L347" rel="#L347">347</span>
<span id="L348" rel="#L348">348</span>
<span id="L349" rel="#L349">349</span>
<span id="L350" rel="#L350">350</span>
<span id="L351" rel="#L351">351</span>
<span id="L352" rel="#L352">352</span>
<span id="L353" rel="#L353">353</span>
<span id="L354" rel="#L354">354</span>
<span id="L355" rel="#L355">355</span>
<span id="L356" rel="#L356">356</span>
<span id="L357" rel="#L357">357</span>
<span id="L358" rel="#L358">358</span>
<span id="L359" rel="#L359">359</span>
<span id="L360" rel="#L360">360</span>
<span id="L361" rel="#L361">361</span>
<span id="L362" rel="#L362">362</span>
<span id="L363" rel="#L363">363</span>
<span id="L364" rel="#L364">364</span>
<span id="L365" rel="#L365">365</span>
<span id="L366" rel="#L366">366</span>
<span id="L367" rel="#L367">367</span>
<span id="L368" rel="#L368">368</span>
<span id="L369" rel="#L369">369</span>
<span id="L370" rel="#L370">370</span>
<span id="L371" rel="#L371">371</span>
<span id="L372" rel="#L372">372</span>
<span id="L373" rel="#L373">373</span>
<span id="L374" rel="#L374">374</span>
<span id="L375" rel="#L375">375</span>
<span id="L376" rel="#L376">376</span>
<span id="L377" rel="#L377">377</span>
<span id="L378" rel="#L378">378</span>
<span id="L379" rel="#L379">379</span>
<span id="L380" rel="#L380">380</span>
<span id="L381" rel="#L381">381</span>
<span id="L382" rel="#L382">382</span>
<span id="L383" rel="#L383">383</span>
<span id="L384" rel="#L384">384</span>
<span id="L385" rel="#L385">385</span>
<span id="L386" rel="#L386">386</span>
<span id="L387" rel="#L387">387</span>
<span id="L388" rel="#L388">388</span>
<span id="L389" rel="#L389">389</span>
<span id="L390" rel="#L390">390</span>
<span id="L391" rel="#L391">391</span>
<span id="L392" rel="#L392">392</span>
<span id="L393" rel="#L393">393</span>
<span id="L394" rel="#L394">394</span>
<span id="L395" rel="#L395">395</span>
<span id="L396" rel="#L396">396</span>
<span id="L397" rel="#L397">397</span>
<span id="L398" rel="#L398">398</span>
<span id="L399" rel="#L399">399</span>
<span id="L400" rel="#L400">400</span>
<span id="L401" rel="#L401">401</span>
<span id="L402" rel="#L402">402</span>
<span id="L403" rel="#L403">403</span>
<span id="L404" rel="#L404">404</span>
<span id="L405" rel="#L405">405</span>
<span id="L406" rel="#L406">406</span>
<span id="L407" rel="#L407">407</span>
<span id="L408" rel="#L408">408</span>
<span id="L409" rel="#L409">409</span>
<span id="L410" rel="#L410">410</span>
<span id="L411" rel="#L411">411</span>
<span id="L412" rel="#L412">412</span>
<span id="L413" rel="#L413">413</span>
<span id="L414" rel="#L414">414</span>

          </td>
          <td class="blob-line-code">
                  <div class="highlight"><pre><div class='line' id='LC1'><span class="c1">;;; asciidoc.el --- asciidoc text file development support</span></div><div class='line' id='LC2'><br/></div><div class='line' id='LC3'><span class="c1">;; Copyright (C) 2011 Terrence Brannon &lt;metaperl@gmail.com&gt;</span></div><div class='line' id='LC4'><br/></div><div class='line' id='LC5'><span class="c1">;; Author: Terrence Brannon &lt;bauhaus@metaperl.com&gt;</span></div><div class='line' id='LC6'><span class="c1">;; Created: 21 Sept 2007</span></div><div class='line' id='LC7'><span class="c1">;; Version: 0.2</span></div><div class='line' id='LC8'><span class="c1">;; Keywords: text-formatting</span></div><div class='line' id='LC9'><br/></div><div class='line' id='LC10'><span class="c1">;; This file is not (yet) part of GNU Emacs.</span></div><div class='line' id='LC11'><span class="c1">;; However, it is distributed under the same license.</span></div><div class='line' id='LC12'><br/></div><div class='line' id='LC13'><span class="c1">;; GNU Emacs is free software; you can redistribute it and/or modify</span></div><div class='line' id='LC14'><span class="c1">;; it under the terms of the GNU General Public License as published by</span></div><div class='line' id='LC15'><span class="c1">;; the Free Software Foundation; either version 2, or (at your option)</span></div><div class='line' id='LC16'><span class="c1">;; any later version.</span></div><div class='line' id='LC17'><br/></div><div class='line' id='LC18'><span class="c1">;; GNU Emacs is distributed in the hope that it will be useful,</span></div><div class='line' id='LC19'><span class="c1">;; but WITHOUT ANY WARRANTY; without even the implied warranty of</span></div><div class='line' id='LC20'><span class="c1">;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span></div><div class='line' id='LC21'><span class="c1">;; GNU General Public License for more details.</span></div><div class='line' id='LC22'><br/></div><div class='line' id='LC23'><span class="c1">;; You should have received a copy of the GNU General Public License</span></div><div class='line' id='LC24'><span class="c1">;; along with GNU Emacs; see the file COPYING.  If not, write to the</span></div><div class='line' id='LC25'><span class="c1">;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,</span></div><div class='line' id='LC26'><span class="c1">;; Boston, MA 02110-1301, USA.</span></div><div class='line' id='LC27'><br/></div><div class='line' id='LC28'><br/></div><div class='line' id='LC29'><span class="c1">;;; Commentary:</span></div><div class='line' id='LC30'><span class="c1">;;</span></div><div class='line' id='LC31'><br/></div><div class='line' id='LC32'><span class="c1">;; Suggested add-ons:</span></div><div class='line' id='LC33'><span class="c1">;; doc-mode: font-locking for asciidoc buffers</span></div><div class='line' id='LC34'><span class="c1">;; http://xpt.sourceforge.net/tools/doc-mode/</span></div><div class='line' id='LC35'><span class="c1">;; - connect asciidoc.el to doc-mode as follows:</span></div><div class='line' id='LC36'><span class="c1">;;   (autoload &#39;doc-mode &quot;doc-mode&quot; nil t)</span></div><div class='line' id='LC37'><span class="c1">;;   (add-to-list &#39;auto-mode-alist &#39;(&quot;\\.adoc$&quot; . doc-mode))</span></div><div class='line' id='LC38'><span class="c1">;;   (add-hook &#39;doc-mode-hook</span></div><div class='line' id='LC39'><span class="c1">;;	  &#39;(lambda ()</span></div><div class='line' id='LC40'><span class="c1">;;	     (turn-on-auto-fill)</span></div><div class='line' id='LC41'><span class="c1">;;	     (require &#39;asciidoc)))</span></div><div class='line' id='LC42'><br/></div><div class='line' id='LC43'><span class="c1">;; Author extends thanks to:</span></div><div class='line' id='LC44'><span class="c1">;; Steve Youngs (JackaLX on irc://irc.freenode.net/xemacs)</span></div><div class='line' id='LC45'><span class="c1">;; bpalmer, twb</span></div><div class='line' id='LC46'><span class="c1">;; jari aalto for turning me on the M-x checkdoc-current-buffer, etc</span></div><div class='line' id='LC47'><br/></div><div class='line' id='LC48'><span class="c1">;; Version control:</span></div><div class='line' id='LC49'><span class="c1">;; This software is under git version control and maybe retrieved at:</span></div><div class='line' id='LC50'><span class="c1">;; https://github.com/metaperl/asciidoc-el</span></div><div class='line' id='LC51'><br/></div><div class='line' id='LC52'><br/></div><div class='line' id='LC53'><span class="c1">;;; Code:</span></div><div class='line' id='LC54'><br/></div><div class='line' id='LC55'><span class="c1">;(setq debug-on-error t)</span></div><div class='line' id='LC56'><br/></div><div class='line' id='LC57'><br/></div><div class='line' id='LC58'><span class="p">(</span><span class="nf">require</span> <span class="ss">&#39;easymenu</span><span class="p">)</span></div><div class='line' id='LC59'><span class="p">(</span><span class="nf">require</span> <span class="ss">&#39;cl</span><span class="p">)</span></div><div class='line' id='LC60'><span class="p">(</span><span class="nf">require</span> <span class="ss">&#39;apropos</span><span class="p">)</span></div><div class='line' id='LC61'><br/></div><div class='line' id='LC62'><br/></div><div class='line' id='LC63'><span class="p">(</span><span class="nf">defvar</span> <span class="nv">*asciidoc-indent-level*</span> <span class="mi">2</span> <span class="s">&quot;Number of spaces to indent per level.&quot;</span><span class="p">)</span></div><div class='line' id='LC64'><br/></div><div class='line' id='LC65'><br/></div><div class='line' id='LC66'><span class="p">(</span><span class="nf">defvar</span> <span class="nv">*delimiter-length*</span> <span class="mi">70</span></div><div class='line' id='LC67'>&nbsp;&nbsp;<span class="s">&quot;How many characters to use when building a delimited block string.  4 min.&quot;</span><span class="p">)</span></div><div class='line' id='LC68'><br/></div><div class='line' id='LC69'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-header</span> <span class="p">(</span><span class="nf">title</span> <span class="nv">author</span> <span class="nv">revision</span><span class="p">)</span></div><div class='line' id='LC70'>&nbsp;&nbsp;<span class="s">&quot;Insert asciidoc header consisting of TITLE and optional AUTHOR and REVISION.&quot;</span></div><div class='line' id='LC71'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sHeader title: \nsHeader author (return if none): \nsHeader revision (return if none):&quot;</span><span class="p">)</span></div><div class='line' id='LC72'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC73'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="nv">title</span>    <span class="s">&quot;\n&quot;</span></div><div class='line' id='LC74'>	   <span class="p">(</span><span class="nb">make-string </span><span class="p">(</span><span class="nb">length </span><span class="nv">title</span><span class="p">)</span> <span class="nv">?=</span><span class="p">)</span> <span class="s">&quot;\n&quot;</span></div><div class='line' id='LC75'>	   <span class="nv">author</span>   <span class="s">&quot;\n&quot;</span></div><div class='line' id='LC76'>	   <span class="nv">revision</span> <span class="s">&quot;\n&quot;</span></div><div class='line' id='LC77'>	   <span class="s">&quot;\n&quot;</span></div><div class='line' id='LC78'>	   <span class="p">)))</span></div><div class='line' id='LC79'><br/></div><div class='line' id='LC80'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-get-started</span> <span class="p">()</span></div><div class='line' id='LC81'>&nbsp;&nbsp;<span class="s">&quot;Start the document.&quot;</span></div><div class='line' id='LC82'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC83'>&nbsp;&nbsp;<span class="p">(</span><span class="k">let </span><span class="p">((</span><span class="nf">date</span> <span class="p">(</span><span class="nf">format-time-string</span> <span class="s">&quot;%D&quot;</span> <span class="p">(</span><span class="nf">current-time</span><span class="p">))))</span></div><div class='line' id='LC84'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">asciidoc-header</span> <span class="s">&quot;Document Title&quot;</span></div><div class='line' id='LC85'>		     <span class="s">&quot;Terrence Brannon &lt;bauhaus@metaperl.com&gt;&quot;</span></div><div class='line' id='LC86'>		     <span class="nv">date</span><span class="p">)))</span></div><div class='line' id='LC87'><br/></div><div class='line' id='LC88'><br/></div><div class='line' id='LC89'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-emphasized</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC90'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with asciidoc emphasis formatting.&quot;</span></div><div class='line' id='LC91'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText to be emphasized: &quot;</span><span class="p">)</span></div><div class='line' id='LC92'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC93'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;_&quot;</span> <span class="nv">text</span> <span class="s">&quot;_&quot;</span><span class="p">)))</span></div><div class='line' id='LC94'><br/></div><div class='line' id='LC95'><br/></div><div class='line' id='LC96'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-strong</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC97'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with asciidoc strong formatting.&quot;</span></div><div class='line' id='LC98'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText to be strong-formatted: &quot;</span><span class="p">)</span></div><div class='line' id='LC99'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC100'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;*&quot;</span> <span class="nv">text</span> <span class="s">&quot;*&quot;</span><span class="p">)))</span></div><div class='line' id='LC101'><br/></div><div class='line' id='LC102'><br/></div><div class='line' id='LC103'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-monospace</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC104'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with asciidoc monospace formatting.&quot;</span></div><div class='line' id='LC105'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText to be monospace formatted: &quot;</span><span class="p">)</span></div><div class='line' id='LC106'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC107'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;`&quot;</span> <span class="nv">text</span> <span class="s">&quot;`&quot;</span><span class="p">)))</span></div><div class='line' id='LC108'><br/></div><div class='line' id='LC109'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-quoted</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC110'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with asciidoc quoted-text formatting.&quot;</span></div><div class='line' id='LC111'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText to be enclosed in quotation marks: &quot;</span><span class="p">)</span></div><div class='line' id='LC112'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC113'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;``&quot;</span> <span class="nv">text</span> <span class="s">&quot;&#39;&#39;&quot;</span><span class="p">)))</span></div><div class='line' id='LC114'><br/></div><div class='line' id='LC115'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-unquoted</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC116'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with asciidoc unquoted text formatting.&quot;</span></div><div class='line' id='LC117'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText to be non-quoted: &quot;</span><span class="p">)</span></div><div class='line' id='LC118'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC119'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;#&quot;</span> <span class="nv">text</span> <span class="s">&quot;#&quot;</span><span class="p">)))</span></div><div class='line' id='LC120'><br/></div><div class='line' id='LC121'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-passthru-triple-plus</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC122'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with asciidoc triple plus passthrough formatting.&quot;</span></div><div class='line' id='LC123'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText to be formatted for no change: &quot;</span><span class="p">)</span></div><div class='line' id='LC124'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC125'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;+++&quot;</span> <span class="nv">text</span> <span class="s">&quot;+++&quot;</span><span class="p">)))</span></div><div class='line' id='LC126'><br/></div><div class='line' id='LC127'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-passthru-double-dollar</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC128'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with asciidoc double-dollar formatting.&quot;</span></div><div class='line' id='LC129'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText to be formatted for no change except escaping special characters: &quot;</span><span class="p">)</span></div><div class='line' id='LC130'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC131'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;$$&quot;</span> <span class="nv">text</span> <span class="s">&quot;$$&quot;</span><span class="p">)))</span></div><div class='line' id='LC132'><br/></div><div class='line' id='LC133'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-superscript</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC134'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with asciidoc superscript formatting.&quot;</span></div><div class='line' id='LC135'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText to be formatted for superscripting: &quot;</span><span class="p">)</span></div><div class='line' id='LC136'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC137'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;^&quot;</span> <span class="nv">text</span> <span class="s">&quot;^&quot;</span><span class="p">)))</span></div><div class='line' id='LC138'><br/></div><div class='line' id='LC139'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-subscript</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC140'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with asciidoc subscript formatting.&quot;</span></div><div class='line' id='LC141'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText to be formatted for subscripting: &quot;</span><span class="p">)</span></div><div class='line' id='LC142'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC143'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;~&quot;</span> <span class="nv">text</span> <span class="s">&quot;~&quot;</span><span class="p">)))</span></div><div class='line' id='LC144'><br/></div><div class='line' id='LC145'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-line-break</span> <span class="p">()</span></div><div class='line' id='LC146'>&nbsp;&nbsp;<span class="s">&quot;Insert asciidoc forced line break.&quot;</span></div><div class='line' id='LC147'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC148'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC149'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot; +\n&quot;</span><span class="p">)))</span></div><div class='line' id='LC150'><br/></div><div class='line' id='LC151'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-horizontal-rule</span> <span class="p">()</span></div><div class='line' id='LC152'>&nbsp;&nbsp;<span class="s">&quot;Insert asciidoc &lt;hr /&gt; tag for HTML only.&quot;</span></div><div class='line' id='LC153'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC154'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC155'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;&#39;&#39;&#39;\n&quot;</span><span class="p">)))</span></div><div class='line' id='LC156'><br/></div><div class='line' id='LC157'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-copyright</span> <span class="p">()</span></div><div class='line' id='LC158'>&nbsp;&nbsp;<span class="s">&quot;Insert asciidoc copyright replacement.&quot;</span></div><div class='line' id='LC159'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC160'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span> <span class="s">&quot;(C) &quot;</span><span class="p">))</span></div><div class='line' id='LC161'><br/></div><div class='line' id='LC162'><br/></div><div class='line' id='LC163'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-trademark</span> <span class="p">()</span></div><div class='line' id='LC164'>&nbsp;&nbsp;<span class="s">&quot;Insert asciidoc copyright replacement.&quot;</span></div><div class='line' id='LC165'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC166'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span> <span class="s">&quot;(TM) &quot;</span><span class="p">))</span></div><div class='line' id='LC167'><br/></div><div class='line' id='LC168'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-registered-trademark</span> <span class="p">()</span></div><div class='line' id='LC169'>&nbsp;&nbsp;<span class="s">&quot;Insert asciidoc registered copyright replacement.&quot;</span></div><div class='line' id='LC170'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC171'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span> <span class="s">&quot;(R) &quot;</span><span class="p">))</span></div><div class='line' id='LC172'><br/></div><div class='line' id='LC173'><br/></div><div class='line' id='LC174'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-section-title</span> <span class="p">(</span><span class="nf">section-level</span> <span class="nv">title</span><span class="p">)</span></div><div class='line' id='LC175'>&nbsp;&nbsp;<span class="s">&quot;Insert asciidoc one-line title syntax consisting of SECTION-LEVEL number of asterisks and TITLE text.&quot;</span></div><div class='line' id='LC176'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;NNumber of equals signs (2-4):  \nsSection title:  &quot;</span><span class="p">)</span></div><div class='line' id='LC177'>&nbsp;&nbsp;<span class="c1">; &quot; &quot; equals-signs</span></div><div class='line' id='LC178'>&nbsp;&nbsp;<span class="p">(</span><span class="k">let </span><span class="p">((</span><span class="nf">equals-signs</span>     <span class="p">(</span><span class="nb">make-string </span><span class="p">(</span><span class="mi">1</span><span class="nv">+</span> <span class="nv">section-level</span><span class="p">)</span> <span class="nv">?=</span><span class="p">)))</span></div><div class='line' id='LC179'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC180'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span></div><div class='line' id='LC181'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s">&quot;\n&quot;</span></div><div class='line' id='LC182'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">equals-signs</span> <span class="s">&quot; &quot;</span> <span class="nv">title</span></div><div class='line' id='LC183'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s">&quot;\n\n&quot;</span><span class="p">))))</span></div><div class='line' id='LC184'><br/></div><div class='line' id='LC185'><br/></div><div class='line' id='LC186'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-block-title</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC187'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with asciidoc block title formatting.&quot;</span></div><div class='line' id='LC188'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText to be formatted as block title: &quot;</span><span class="p">)</span></div><div class='line' id='LC189'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC190'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;.&quot;</span> <span class="nv">text</span> <span class="s">&quot;\n&quot;</span><span class="p">)))</span></div><div class='line' id='LC191'><br/></div><div class='line' id='LC192'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-block-id-element</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC193'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with asciidoc BlockId Element formatting.&quot;</span></div><div class='line' id='LC194'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText to be formatted as block-id: &quot;</span><span class="p">)</span></div><div class='line' id='LC195'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC196'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;[[&quot;</span> <span class="nv">text</span> <span class="s">&quot;]]&quot;</span> <span class="s">&quot;\n&quot;</span><span class="p">)))</span></div><div class='line' id='LC197'><br/></div><div class='line' id='LC198'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-block-reference</span> <span class="p">(</span><span class="nf">block-id</span> <span class="nv">descriptive-text</span><span class="p">)</span></div><div class='line' id='LC199'>&nbsp;&nbsp;<span class="s">&quot;Insert asciidoc reference to a block consisting of BLOCK-ID and DESCRIPTIVE-TEXT.&quot;</span></div><div class='line' id='LC200'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sBlockId: \nsDescriptive text: &quot;</span><span class="p">)</span></div><div class='line' id='LC201'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC202'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;&lt;&lt;&quot;</span> <span class="nv">block-id</span> <span class="s">&quot;,&quot;</span> <span class="nv">descriptive-text</span> <span class="s">&quot;&gt;&gt;&quot;</span></div><div class='line' id='LC203'><span class="p">)))</span></div><div class='line' id='LC204'><br/></div><div class='line' id='LC205'><br/></div><div class='line' id='LC206'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-verse-paragraph</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC207'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with verse paragraph formatting.&quot;</span></div><div class='line' id='LC208'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC209'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span> <span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;[verse]&quot;</span> <span class="s">&quot;\n&quot;</span><span class="p">)))</span></div><div class='line' id='LC210'><br/></div><div class='line' id='LC211'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-literal-paragraph</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC212'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with literal paragraph formatting.&quot;</span></div><div class='line' id='LC213'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC214'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span> <span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;  &quot;</span> <span class="s">&quot;\n&quot;</span><span class="p">)))</span></div><div class='line' id='LC215'><br/></div><div class='line' id='LC216'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-admonition-paragraph</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC217'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT with admonition paragraph formatting.&quot;</span></div><div class='line' id='LC218'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC219'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span> <span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;[NOTE]&quot;</span> <span class="s">&quot;\n&quot;</span><span class="p">)))</span></div><div class='line' id='LC220'><br/></div><div class='line' id='LC221'><br/></div><div class='line' id='LC222'><br/></div><div class='line' id='LC223'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-delimited-block</span> <span class="p">(</span><span class="nf">delimiter</span> <span class="nv">text</span><span class="p">)</span></div><div class='line' id='LC224'>&nbsp;&nbsp;<span class="s">&quot;Make a string consisting of DELIMITER and TEXT.&quot;</span></div><div class='line' id='LC225'>&nbsp;&nbsp;<span class="p">(</span><span class="k">let </span><span class="p">((</span><span class="nf">str</span> <span class="p">(</span><span class="nb">make-string </span><span class="nv">*delimiter-length*</span> <span class="nv">delimiter</span><span class="p">)))</span></div><div class='line' id='LC226'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span> <span class="p">(</span><span class="nf">concat</span> <span class="nv">str</span> <span class="s">&quot;\n&quot;</span> <span class="nv">text</span> <span class="s">&quot;\n&quot;</span> <span class="nv">str</span> <span class="s">&quot;\n\n&quot;</span><span class="p">))))</span></div><div class='line' id='LC227'><br/></div><div class='line' id='LC228'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-comment-block</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC229'>&nbsp;&nbsp;<span class="s">&quot;Create an asciidoc CommentBlock consisting of TEXT.&quot;</span></div><div class='line' id='LC230'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText for comment block? &quot;</span><span class="p">)</span></div><div class='line' id='LC231'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">asciidoc-delimited-block</span> <span class="nv">?/</span> <span class="nv">text</span><span class="p">))</span></div><div class='line' id='LC232'><br/></div><div class='line' id='LC233'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-passthru-block</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC234'>&nbsp;&nbsp;<span class="s">&quot;Create an asciidoc PassthroughBlock consisting of TEXT.&quot;</span></div><div class='line' id='LC235'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText for passthru block? &quot;</span><span class="p">)</span></div><div class='line' id='LC236'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">asciidoc-delimited-block</span> <span class="nv">?+</span> <span class="nv">text</span><span class="p">))</span></div><div class='line' id='LC237'><br/></div><div class='line' id='LC238'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-listing-block</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC239'>&nbsp;&nbsp;<span class="s">&quot;Create an asciidoc ListingBlock consisting of TEXT.&quot;</span></div><div class='line' id='LC240'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText for listing block? &quot;</span><span class="p">)</span></div><div class='line' id='LC241'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">asciidoc-delimited-block</span> <span class="nv">?-</span> <span class="nv">text</span><span class="p">))</span></div><div class='line' id='LC242'><br/></div><div class='line' id='LC243'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-literal-block</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC244'>&nbsp;&nbsp;<span class="s">&quot;Create an asciidoc LiteralBlock consisting of TEXT.&quot;</span></div><div class='line' id='LC245'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText for literal block? &quot;</span><span class="p">)</span></div><div class='line' id='LC246'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">asciidoc-delimited-block</span> <span class="nv">?</span><span class="o">.</span> <span class="nv">text</span><span class="p">))</span></div><div class='line' id='LC247'><br/></div><div class='line' id='LC248'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-sidebar-block</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC249'>&nbsp;&nbsp;<span class="s">&quot;Create an asciidoc SidebarBlock consisting of TEXT.&quot;</span></div><div class='line' id='LC250'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText for sidebar block? &quot;</span><span class="p">)</span></div><div class='line' id='LC251'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">asciidoc-delimited-block</span> <span class="nv">?*</span> <span class="nv">text</span><span class="p">))</span></div><div class='line' id='LC252'><br/></div><div class='line' id='LC253'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-example-block</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC254'>&nbsp;&nbsp;<span class="s">&quot;Create an asciidoc ExampleBlock, using TEXT and optionally modifying the default EXAMPLE-LABEL and EXAMPLE-DESCRIPTION.&quot;</span></div><div class='line' id='LC255'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText for example block? &quot;</span><span class="p">)</span></div><div class='line' id='LC256'>&nbsp;&nbsp;<span class="p">(</span><span class="k">let </span><span class="p">((</span><span class="nf">example-label</span> <span class="p">(</span><span class="nf">read-string</span> <span class="s">&quot;Example label? (it needs a space at the end) &quot;</span> <span class="s">&quot;Example: &quot;</span><span class="p">))</span></div><div class='line' id='LC257'>	<span class="p">(</span><span class="nf">example-description</span> <span class="p">(</span><span class="nf">read-string</span> <span class="s">&quot;Example description? &quot;</span> <span class="s">&quot;An example&quot;</span><span class="p">)))</span></div><div class='line' id='LC258'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if </span><span class="p">(</span><span class="nb">not </span><span class="p">(</span><span class="nf">string=</span> <span class="s">&quot;&quot;</span> <span class="nv">example-label</span><span class="p">))</span></div><div class='line' id='LC259'>	<span class="p">(</span><span class="nf">insert</span> <span class="s">&quot;[caption=&quot;</span> <span class="s">&quot;\&quot;&quot;</span> <span class="nv">example-label</span> <span class="s">&quot;\&quot;&quot;</span> <span class="s">&quot;]&quot;</span> <span class="s">&quot;\n&quot;</span><span class="p">))</span></div><div class='line' id='LC260'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if </span><span class="p">(</span><span class="nb">not </span><span class="p">(</span><span class="nf">string=</span> <span class="s">&quot;&quot;</span> <span class="nv">example-description</span><span class="p">))</span></div><div class='line' id='LC261'>	<span class="p">(</span><span class="nf">insert</span> <span class="s">&quot;.&quot;</span> <span class="nv">example-description</span> <span class="s">&quot;\n&quot;</span><span class="p">))</span></div><div class='line' id='LC262'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">asciidoc-delimited-block</span> <span class="nv">?=</span> <span class="nv">text</span><span class="p">)))</span></div><div class='line' id='LC263'><br/></div><div class='line' id='LC264'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-quotation-block</span> <span class="p">(</span><span class="nf">text</span> <span class="nv">author</span> <span class="nv">source</span><span class="p">)</span></div><div class='line' id='LC265'>&nbsp;&nbsp;<span class="s">&quot;Given TEXT, AUTHOR, and SOURCE, create an asciidoc QuoteBlock.&quot;</span></div><div class='line' id='LC266'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sText of quotation? \nsAuthor of quotation? \nsWhere did this quotation come from? &quot;</span><span class="p">)</span></div><div class='line' id='LC267'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC268'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="s">&quot;[&quot;</span></div><div class='line' id='LC269'>	   <span class="s">&quot;attribution=&quot;</span> <span class="s">&quot;\&quot;&quot;</span> <span class="nv">author</span> <span class="s">&quot;\&quot;&quot;</span></div><div class='line' id='LC270'>	   <span class="s">&quot;,&quot;</span></div><div class='line' id='LC271'>	   <span class="s">&quot;citetitle=&quot;</span>   <span class="s">&quot;\&quot;&quot;</span> <span class="nv">source</span> <span class="s">&quot;\&quot;&quot;</span></div><div class='line' id='LC272'>	   <span class="s">&quot;]&quot;</span> <span class="s">&quot;\n&quot;</span><span class="p">))</span></div><div class='line' id='LC273'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">asciidoc-delimited-block</span> <span class="nv">?_</span> <span class="nv">text</span><span class="p">))</span></div><div class='line' id='LC274'><br/></div><div class='line' id='LC275'><br/></div><div class='line' id='LC276'><br/></div><div class='line' id='LC277'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-compile</span> <span class="p">()</span></div><div class='line' id='LC278'>&nbsp;&nbsp;<span class="s">&quot;Create an asciidoc document.&quot;</span></div><div class='line' id='LC279'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC280'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">compile-command</span></div><div class='line' id='LC281'>	<span class="p">(</span><span class="nf">concat</span></div><div class='line' id='LC282'>	 <span class="s">&quot;asciidoc -a numbered -a toc -a toclevels=4&quot;</span> <span class="s">&quot; &quot;</span></div><div class='line' id='LC283'>	 <span class="p">(</span><span class="nf">file-name-nondirectory</span> <span class="p">(</span><span class="nf">buffer-file-name</span><span class="p">))</span></div><div class='line' id='LC284'>	 <span class="p">))</span></div><div class='line' id='LC285'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">call-interactively</span> <span class="ss">&#39;compile</span><span class="p">))</span></div><div class='line' id='LC286'><br/></div><div class='line' id='LC287'><span class="p">(</span><span class="nf">defvar</span> <span class="nv">*asciidoc-bullet*</span> <span class="o">&#39;</span><span class="p">(</span><span class="s">&quot;-&quot;</span> <span class="s">&quot;*&quot;</span><span class="p">)</span></div><div class='line' id='LC288'>&nbsp;&nbsp;<span class="s">&quot;Strings representing each of the two bullet levels offered by Asciidoc.&quot;</span><span class="p">)</span></div><div class='line' id='LC289'><br/></div><div class='line' id='LC290'><br/></div><div class='line' id='LC291'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-bullet-item</span> <span class="p">(</span><span class="nf">bullet-level</span> <span class="nv">text</span><span class="p">)</span></div><div class='line' id='LC292'>&nbsp;&nbsp;<span class="s">&quot;At BULLET-LEVEL (1 or 2), insert TEXT.&quot;</span></div><div class='line' id='LC293'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;NBullet level (1 or 2):  \nsText for bullet:  &quot;</span><span class="p">)</span></div><div class='line' id='LC294'>&nbsp;&nbsp;<span class="p">(</span><span class="k">let* </span><span class="p">((</span><span class="nf">level</span>  <span class="p">(</span><span class="k">if </span><span class="p">(</span><span class="nb">= </span><span class="nv">bullet-level</span> <span class="mi">2</span><span class="p">)</span> <span class="mi">1</span> <span class="mi">0</span><span class="p">))</span></div><div class='line' id='LC295'>	 <span class="p">(</span><span class="nf">bullet</span> <span class="p">(</span><span class="nf">nth</span> <span class="nv">level</span> <span class="nv">*asciidoc-bullet*</span><span class="p">))</span></div><div class='line' id='LC296'>	 <span class="p">(</span><span class="nf">tab-space</span> <span class="p">(</span><span class="nb">make-string </span><span class="p">(</span><span class="nb">* </span><span class="nv">level</span> <span class="mi">4</span><span class="p">)</span> <span class="nv">?</span><span class="err">\</span><span class="nv">s</span><span class="p">)))</span></div><div class='line' id='LC297'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC298'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span></div><div class='line' id='LC299'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">tab-space</span> <span class="nv">bullet</span> <span class="s">&quot; &quot;</span> <span class="nv">text</span> <span class="s">&quot;\n&quot;</span><span class="p">))))</span></div><div class='line' id='LC300'><br/></div><div class='line' id='LC301'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-numbered-list-item</span> <span class="p">(</span><span class="nf">item-level</span> <span class="nv">text</span><span class="p">)</span></div><div class='line' id='LC302'>&nbsp;&nbsp;<span class="s">&quot;At ITEM-LEVEL, insert TEXT.&quot;</span></div><div class='line' id='LC303'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;NItem level (1 or 2):  \nsText for bullet:  &quot;</span><span class="p">)</span></div><div class='line' id='LC304'>&nbsp;&nbsp;<span class="p">(</span><span class="k">let* </span><span class="p">((</span><span class="nf">level</span>  <span class="p">(</span><span class="k">if </span><span class="p">(</span><span class="nb">= </span><span class="nv">item-level</span> <span class="mi">2</span><span class="p">)</span> <span class="mi">2</span> <span class="mi">1</span><span class="p">))</span></div><div class='line' id='LC305'>	 <span class="p">(</span><span class="nf">bullet</span> <span class="p">(</span><span class="nb">make-string </span><span class="nv">level</span> <span class="nv">?</span><span class="o">.</span><span class="p">))</span></div><div class='line' id='LC306'>	 <span class="p">(</span><span class="nf">tab-space</span> <span class="p">(</span><span class="nb">make-string </span><span class="p">(</span><span class="nb">* </span><span class="p">(</span><span class="nb">- </span><span class="nv">level</span> <span class="mi">1</span><span class="p">)</span> <span class="mi">4</span><span class="p">)</span> <span class="nv">?</span><span class="err">\</span><span class="nv">s</span><span class="p">)))</span></div><div class='line' id='LC307'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC308'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span></div><div class='line' id='LC309'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">tab-space</span> <span class="nv">bullet</span> <span class="s">&quot; &quot;</span> <span class="nv">text</span> <span class="s">&quot;\n&quot;</span><span class="p">))))</span></div><div class='line' id='LC310'><br/></div><div class='line' id='LC311'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-labelled-list-item</span> <span class="p">(</span><span class="nf">text</span><span class="p">)</span></div><div class='line' id='LC312'>&nbsp;&nbsp;<span class="s">&quot;Insert TEXT.&quot;</span></div><div class='line' id='LC313'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sLabel for list item: &quot;</span><span class="p">)</span></div><div class='line' id='LC314'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC315'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span></div><div class='line' id='LC316'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">text</span> <span class="s">&quot;::&quot;</span> <span class="s">&quot;\n    &quot;</span><span class="p">)))</span></div><div class='line' id='LC317'><br/></div><div class='line' id='LC318'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-bibliography-item</span> <span class="p">(</span><span class="nf">ref-label</span> <span class="nv">ref-text</span><span class="p">)</span></div><div class='line' id='LC319'>&nbsp;&nbsp;<span class="s">&quot;Insert bibliography item consisting of REF-LABEL and REF-TEXT.&quot;</span></div><div class='line' id='LC320'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sLabel for bib item: \nsText of bibitem: &quot;</span><span class="p">)</span></div><div class='line' id='LC321'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC322'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span></div><div class='line' id='LC323'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="s">&quot;+&quot;</span> <span class="s">&quot; &quot;</span>  <span class="s">&quot;[[[&quot;</span> <span class="nv">ref-label</span> <span class="s">&quot;]]]&quot;</span> <span class="s">&quot; &quot;</span> <span class="nv">ref-text</span> <span class="s">&quot;\n&quot;</span><span class="p">)))</span></div><div class='line' id='LC324'><br/></div><div class='line' id='LC325'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-href</span> <span class="p">(</span><span class="nf">url</span> <span class="nv">link-text</span><span class="p">)</span></div><div class='line' id='LC326'>&nbsp;&nbsp;<span class="s">&quot;Insert hyperlink consisting of URL and LINK-TEXT.&quot;</span></div><div class='line' id='LC327'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sURL: \nsText describing URL: &quot;</span><span class="p">)</span></div><div class='line' id='LC328'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC329'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span></div><div class='line' id='LC330'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">url</span> <span class="s">&quot;[&quot;</span> <span class="nv">link-text</span> <span class="s">&quot;]&quot;</span> <span class="s">&quot;\n&quot;</span></div><div class='line' id='LC331'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">)))</span></div><div class='line' id='LC332'><br/></div><div class='line' id='LC333'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-relative-href</span> <span class="p">(</span><span class="nf">url</span> <span class="nv">link-text</span><span class="p">)</span></div><div class='line' id='LC334'>&nbsp;&nbsp;<span class="s">&quot;Insert hyperlink consisting of URL and LINK-TEXT.&quot;</span></div><div class='line' id='LC335'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sRelative path to file (anchors allowed): \nsText describing link: &quot;</span><span class="p">)</span></div><div class='line' id='LC336'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC337'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span></div><div class='line' id='LC338'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="s">&quot;link:&quot;</span></div><div class='line' id='LC339'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">url</span> <span class="s">&quot;[&quot;</span> <span class="nv">link-text</span> <span class="s">&quot;]&quot;</span> <span class="s">&quot;\n&quot;</span></div><div class='line' id='LC340'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">)))</span></div><div class='line' id='LC341'><br/></div><div class='line' id='LC342'><span class="p">(</span><span class="nf">defun</span> <span class="nv">asciidoc-image-href</span> <span class="p">(</span><span class="nf">url</span> <span class="nv">link-text</span><span class="p">)</span></div><div class='line' id='LC343'>&nbsp;&nbsp;<span class="s">&quot;Insert hyperlink consisting of URL and LINK-TEXT.&quot;</span></div><div class='line' id='LC344'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;sURL to image file: \nsText describing image (only displayed when image unavailable): &quot;</span><span class="p">)</span></div><div class='line' id='LC345'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span></div><div class='line' id='LC346'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span></div><div class='line' id='LC347'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="s">&quot;image:&quot;</span></div><div class='line' id='LC348'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">url</span> <span class="s">&quot;[&quot;</span> <span class="s">&quot;\&quot;&quot;</span> <span class="nv">link-text</span> <span class="s">&quot;\&quot;&quot;</span> <span class="s">&quot;]&quot;</span> <span class="s">&quot;\n&quot;</span></div><div class='line' id='LC349'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">)))</span></div><div class='line' id='LC350'><br/></div><div class='line' id='LC351'><br/></div><div class='line' id='LC352'><span class="p">(</span><span class="k">let </span><span class="p">((</span><span class="nf">s</span> <span class="s">&quot;admonition-paragraph&quot;</span><span class="p">))</span></div><div class='line' id='LC353'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">split-string</span> <span class="nv">s</span> <span class="s">&quot;asciidoc&quot;</span><span class="p">))</span></div><div class='line' id='LC354'><br/></div><div class='line' id='LC355'><span class="p">(</span><span class="nf">setq</span> <span class="nv">asciidoc-global-menuspec</span></div><div class='line' id='LC356'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">list </span><span class="s">&quot;Doc&quot;</span></div><div class='line' id='LC357'>	    <span class="p">(</span><span class="nf">list</span></div><div class='line' id='LC358'>	     <span class="s">&quot;Links and refs&quot;</span></div><div class='line' id='LC359'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Href&quot;</span> <span class="ss">&#39;asciidoc-href</span><span class="p">)</span></div><div class='line' id='LC360'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Image&quot;</span> <span class="ss">&#39;asciidoc-image-href</span><span class="p">)</span></div><div class='line' id='LC361'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Relative url&quot;</span> <span class="ss">&#39;asciidoc-relative-href</span><span class="p">)</span></div><div class='line' id='LC362'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Internal document references&quot;</span> <span class="ss">&#39;asciidoc-block-reference</span><span class="p">)</span></div><div class='line' id='LC363'>	      <span class="p">)</span></div><div class='line' id='LC364'>	    <span class="p">(</span><span class="nf">list</span></div><div class='line' id='LC365'>	     <span class="s">&quot;Reference items&quot;</span></div><div class='line' id='LC366'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Bibliography listing&quot;</span> <span class="ss">&#39;asciidoc-bibliography-item</span><span class="p">)</span></div><div class='line' id='LC367'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;BlockId element&quot;</span> <span class="ss">&#39;asciidoc-block-id-element</span><span class="p">))</span></div><div class='line' id='LC368'>	    <span class="p">(</span><span class="nf">list</span></div><div class='line' id='LC369'>	     <span class="s">&quot;Bullets and lists&quot;</span></div><div class='line' id='LC370'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Bulleted list&quot;</span> <span class="ss">&#39;asciidoc-bullet-item</span><span class="p">)</span></div><div class='line' id='LC371'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Numbered list&quot;</span> <span class="ss">&#39;asciidoc-numbered-list-item</span><span class="p">)</span></div><div class='line' id='LC372'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Labelled list&quot;</span> <span class="ss">&#39;asciidoc-labelled-list-item</span><span class="p">)</span></div><div class='line' id='LC373'>	      <span class="p">)</span></div><div class='line' id='LC374'>	    <span class="p">(</span><span class="nf">list</span></div><div class='line' id='LC375'>	     <span class="s">&quot;Text formatting&quot;</span></div><div class='line' id='LC376'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Emphasis&quot;</span> <span class="ss">&#39;asciidoc-emphasized</span><span class="p">)</span></div><div class='line' id='LC377'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Strong (bold)&quot;</span> <span class="ss">&#39;asciidoc-strong</span><span class="p">)</span></div><div class='line' id='LC378'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Monospaced&quot;</span> <span class="ss">&#39;asciidoc-monospace</span><span class="p">)</span></div><div class='line' id='LC379'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Quotation marks around text&quot;</span> <span class="ss">&#39;asciidoc-quoted</span><span class="p">)</span></div><div class='line' id='LC380'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Superscript&quot;</span> <span class="ss">&#39;asciidoc-superscript</span><span class="p">)</span></div><div class='line' id='LC381'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Subscript&quot;</span> <span class="ss">&#39;asciidoc-subscript</span><span class="p">)</span></div><div class='line' id='LC382'>	      <span class="p">)</span></div><div class='line' id='LC383'>	    <span class="p">(</span><span class="nf">list</span></div><div class='line' id='LC384'>	     <span class="s">&quot;Special symbols&quot;</span></div><div class='line' id='LC385'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Copyright&quot;</span> <span class="ss">&#39;asciidoc-copyright</span><span class="p">)</span></div><div class='line' id='LC386'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Trademark&quot;</span> <span class="ss">&#39;asciidoc-trademark</span><span class="p">)</span></div><div class='line' id='LC387'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Registered trademark&quot;</span> <span class="ss">&#39;asciidoc-registered-trademark</span><span class="p">)</span></div><div class='line' id='LC388'>	      <span class="p">)</span></div><div class='line' id='LC389'>	    <span class="p">(</span><span class="nf">list</span></div><div class='line' id='LC390'>	     <span class="s">&quot;Blocks&quot;</span></div><div class='line' id='LC391'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Example block&quot;</span> <span class="ss">&#39;asciidoc-example-block</span><span class="p">)</span></div><div class='line' id='LC392'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Listing block&quot;</span> <span class="ss">&#39;asciidoc-listing-block</span><span class="p">)</span></div><div class='line' id='LC393'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Quotation block&quot;</span> <span class="ss">&#39;asciidoc-quotation-block</span><span class="p">)</span></div><div class='line' id='LC394'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Literal block&quot;</span> <span class="ss">&#39;asciidoc-literal-block</span><span class="p">)</span></div><div class='line' id='LC395'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Sidebar block&quot;</span> <span class="ss">&#39;asciidoc-sidebar-block</span><span class="p">)</span></div><div class='line' id='LC396'><br/></div><div class='line' id='LC397'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Comment block&quot;</span> <span class="ss">&#39;asciidoc-comment-block</span><span class="p">)</span></div><div class='line' id='LC398'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Pass-through block&quot;</span> <span class="ss">&#39;asciidoc-passthru-triple-plus</span><span class="p">)</span></div><div class='line' id='LC399'>	      <span class="p">)</span></div><div class='line' id='LC400'>	    <span class="p">(</span><span class="nf">list</span></div><div class='line' id='LC401'>	     <span class="s">&quot;Run Asciidoc&quot;</span></div><div class='line' id='LC402'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Compile&quot;</span>   <span class="ss">&#39;asciidoc-compile</span><span class="p">)</span></div><div class='line' id='LC403'>	      <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Recompile&quot;</span> <span class="ss">&#39;recompile</span><span class="p">)</span></div><div class='line' id='LC404'>	      <span class="p">)</span></div><div class='line' id='LC405'>	    <span class="p">(</span><span class="nb">vector </span><span class="s">&quot;Start document&quot;</span> <span class="ss">&#39;asciidoc-get-started</span><span class="p">)</span></div><div class='line' id='LC406'><span class="p">))</span></div><div class='line' id='LC407'><br/></div><div class='line' id='LC408'><br/></div><div class='line' id='LC409'><span class="p">(</span><span class="nf">easy-menu-define</span></div><div class='line' id='LC410'>&nbsp;&nbsp;<span class="nv">asciidoc-global-menu</span> <span class="nv">global-map</span> <span class="s">&quot;&quot;</span> <span class="nv">asciidoc-global-menuspec</span><span class="p">)</span></div><div class='line' id='LC411'><br/></div><div class='line' id='LC412'><span class="p">(</span><span class="nf">provide</span> <span class="ss">&#39;asciidoc</span><span class="p">)</span></div><div class='line' id='LC413'><br/></div><div class='line' id='LC414'><span class="c1">;;; asciidoc.el ends here</span></div></pre></div>
          </td>
        </tr>
      </table>
  </div>

  </div>
</div>

<a href="#jump-to-line" rel="facebox[.linejump]" data-hotkey="l" class="js-jump-to-line" style="display:none">Jump to Line</a>
<div id="jump-to-line" style="display:none">
  <form accept-charset="UTF-8" class="js-jump-to-line-form">
    <input class="linejump-input js-jump-to-line-field" type="text" placeholder="Jump to line&hellip;" autofocus>
    <button type="submit" class="button">Go</button>
  </form>
</div>

        </div>

      </div><!-- /.repo-container -->
      <div class="modal-backdrop"></div>
    </div><!-- /.container -->
  </div><!-- /.site -->


    </div><!-- /.wrapper -->

      <div class="container">
  <div class="site-footer">
    <ul class="site-footer-links right">
      <li><a href="https://status.github.com/">Status</a></li>
      <li><a href="http://developer.github.com">API</a></li>
      <li><a href="http://training.github.com">Training</a></li>
      <li><a href="http://shop.github.com">Shop</a></li>
      <li><a href="/blog">Blog</a></li>
      <li><a href="/about">About</a></li>

    </ul>

    <a href="/">
      <span class="mega-octicon octicon-mark-github"></span>
    </a>

    <ul class="site-footer-links">
      <li>&copy; 2013 <span title="0.03930s from fe17.rs.github.com">GitHub</span>, Inc.</li>
        <li><a href="/site/terms">Terms</a></li>
        <li><a href="/site/privacy">Privacy</a></li>
        <li><a href="/security">Security</a></li>
        <li><a href="/contact">Contact</a></li>
    </ul>
  </div><!-- /.site-footer -->
</div><!-- /.container -->


    <div class="fullscreen-overlay js-fullscreen-overlay" id="fullscreen_overlay">
  <div class="fullscreen-container js-fullscreen-container">
    <div class="textarea-wrap">
      <textarea name="fullscreen-contents" id="fullscreen-contents" class="js-fullscreen-contents" placeholder="" data-suggester="fullscreen_suggester"></textarea>
          <div class="suggester-container">
              <div class="suggester fullscreen-suggester js-navigation-container" id="fullscreen_suggester"
                 data-url="/metaperl/asciidoc-el/suggestions/commit">
              </div>
          </div>
    </div>
  </div>
  <div class="fullscreen-sidebar">
    <a href="#" class="exit-fullscreen js-exit-fullscreen tooltipped leftwards" title="Exit Zen Mode">
      <span class="mega-octicon octicon-screen-normal"></span>
    </a>
    <a href="#" class="theme-switcher js-theme-switcher tooltipped leftwards"
      title="Switch themes">
      <span class="octicon octicon-color-mode"></span>
    </a>
  </div>
</div>



    <div id="ajax-error-message" class="flash flash-error">
      <span class="octicon octicon-alert"></span>
      <a href="#" class="octicon octicon-remove-close close ajax-error-dismiss"></a>
      Something went wrong with that request. Please try again.
    </div>

    
  </body>
</html>

