/* global $,Mustache,document,hljs */
/* vim: set ft=javascript: */

var TOPBAR_H = 24 * 3;

// Accordian made into an object
function Accordian(sel, delay) {
  this.delay = delay || 400;
  this.sel = sel || '.side-nav';
  this.sliderCon = 'dt + div';
  this.sliderHeader = 'dt';
  var self = this;

  // Toggle TOC nav
  this.collapseAll(0)
  this.attach_click();

}

Accordian.prototype.attach_click = function() {
  var self = this;
  $(this.sel +' ' + this.sliderHeader).click(function(e){
    self.collapseAll();
    $(this).next(this.sliderCon).stop().slideToggle(self.delay);
  });
};

Accordian.prototype.setTo = function (id){
  var $a = $(this.sel + ' dd a[href=' + id + ']');
  var $con = $a.parents('dt + div')

  this.collapseAll(undefined, $con);

  if ($con.css('display') === 'none' ) {
    $a.parents('dt + div').slideDown();
  } // else it is already down

}

Accordian.prototype.collapseAll = function(delay, ignore) {
  if (delay === undefined) { delay = this.delay }

  if(ignore) {
    $(this.sel +' dt + div').not(ignore).slideUp(delay);
  } else {
    $(this.sel +' dt + div').slideUp(delay);
  }

}

// Imperative ....

function init_code_bar() {
  $('#code-bar dd a').click(function(e){
    e.preventDefault();

    var lang = $(this).parent().attr('data-lang');
    show_lang(lang);
  });

  // Show the first language we have...
  if (window.code) {
    var lang;
    // get first property...bit of a hack
    for(lang in window.code) { break;}
    show_lang(lang);
  }

}

function show_lang(lang) {
  // Show sections that are interested...
  $('#wrapper').attr('data-show-lang',lang);

  // Toggle active state on code-bar item
  $('#code-bar [data-lang]').removeClass('active');
  $('#code-bar [data-lang="'+lang+'"').addClass('active');
}

function inject_sample_into_markup(el,lang,sections) {
  var $container = $(el);
  var $lang_con = $('<div class="lang lang-'+lang+'"></div>');

  $.each(sections, function(i, sec){

    // Build a template
    var template = '';
    template += '<section>';
    template += '<h6 class="section-header"> '+ sec.header + '</h6>';
    template += '<pre><code class="'+sec.syntax+'">'+ sec.content + '</code></pre>';
    template += '</section>';

    $lang_con.append($(template));
  });

  $container.append($lang_con);
}

function inject_code_samples() {
  if (!window.code) {
    console.log('we have no code samples... :(');
    return;
  }

  // Merge all the samples into a single object, indexed by <api classname> .. <operationId> ..
  window.all_code = {};
  var _langs = [];
  for (var lang in window.code) {
    _langs.push(lang);
    $.extend(true,window.all_code, window.code[lang]);
  }

  console.log('found '+ _langs.length + ' langs: ' + _langs.join(','));

  // NOTE: These loops are long in the tooth to read.... is there a better OOP approach?
  // Iterate over each classname/operationId/language
  $.each(window.all_code, function(classname,ops){
    $.each(ops, function(opid, langs){
      $.each(langs, function(lang, sections){
        var id = '#code-samples-'+classname+'-'+opid;
        inject_sample_into_markup(id, lang, sections);
      })
    });

  });

}

function attach_toc_listeners() {

  window.navAccordian = new Accordian('dl.side-nav');

  // Handle flyout TOC
  $('.mobile-only .menu-icon').click(function(){
    $('#wrapper').addClass('show-nav-section');
  });
  // NOTE: is this efficient, having a click event for escaping the fly-out toc?
  $('a.side-nav-item, #main-wrapper').click(function(){
    $('#wrapper').removeClass('show-nav-section');
  });
  $(document).keyup(function(e) {
    if (e.keyCode === 27) { // esc
      $('#wrapper').removeClass('show-nav-section');
    }
  });


  // Add highlight to nav items when we scroll into their domain
  add_scroll_highlighter();
}

function get_list_of_toc_hrefs () {

  // Get all the hrefs of the <a> tags, in the toc's side nav
  return $('.toc dd > a').map(function() {
    var href = $(this).attr('href');

    // Only return internal links
    if (href.indexOf('#') === 0) { return href; }

  }).get(); // #get -> convert to array from jQuery Obj
}


function add_scroll_highlighter() {


  var list_of_hrefs = get_list_of_toc_hrefs();

  $(window).scroll(function(){

    var $win = $(window);
    var winTop = $win.scrollTop()// - TOPBAR_H;
    var winH = $win.height();
    var docH = $(document).height();
    var topbarH = $('#top-bar').outerHeight(true);
    var top = winTop + topbarH;

    $.each(list_of_hrefs, function(i, href) {

      var $href = $(href);
      var hrefTop = $href.offset().top; // get the offset of the div from the top of page
      var hrefH= $href.parents('.api-section').outerHeight(true); // get the height of the div in question

      if (top >= hrefTop && top < (hrefTop + hrefH)) {
        window.navAccordian.setTo(href);
        $("a[href='" + href + "']").addClass("nav-active");
      } else {
        $("a[href='" + href + "']").removeClass("nav-active");
      }

    });
  });

}

// // #scrollTo Plugin...http://lions-mark.com/jquery/scrollTo/
// $.fn.scrollTo = function( target, options, callback ){
//   if(typeof options == 'function' && arguments.length == 2){ callback = options; options = target; }
//   var settings = $.extend({
//     scrollTarget  : target,
//     offsetTop     : 0,
//     duration      : 500,
//     easing        : 'swing'
//   }, options);
//   return this.each(function(){
//     var scrollPane = $(this);
//     var scrollTarget = (typeof settings.scrollTarget == "number") ? settings.scrollTarget : $(settings.scrollTarget);
//     var scrollY = (typeof scrollTarget == "number") ? scrollTarget : scrollTarget.offset().top + scrollPane.scrollTop() - parseInt(settings.offsetTop);
//     scrollPane.animate({scrollTop : scrollY }, parseInt(settings.duration), settings.easing, function(){
//       if (typeof callback == 'function') { callback.call(this); }
//     });
//   });
// }

// --------------- Entry point...
// ...after we load-in and render our mustache templates

function app_setup() {

  // TOC
  attach_toc_listeners();
  // Code Samples
  inject_code_samples();
  init_code_bar();


  // Highlight.js (code syntax highlighting)
  hljs.configure({
    classPrefix: ''
  });
  $('pre>code').each(function(i, block) {
    hljs.highlightBlock(block);
  });

}
