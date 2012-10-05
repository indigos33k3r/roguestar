
var roguestar_preferences = {
    animation_thread_interval : 100,
    strong_magic_colors : ['FFFF00', '00FFFF', 'FF00FF'],
    weak_magic_colors : ['AAAA55', '55AAAA', 'AA55AA'],
    water_colors : ['0000FF', '1122FF', '2244FF', '3366FF', '4488FF', '3366FF', '2244FF', '1122FF' ]
  }

$('document').ready(function() {
  //Let the server know we have javascript
  $.cookie('javascript', 'enabled', { expires: 1, path: '/play' });

  //Form buttons that need to be automatically submitted, possibly after a pause while we do an animation.
  $('.autosubmit').hide();
  $('.autosubmit-after-pause').hide();

  setTimeout(
    function() {
      $('.autosubmit').submit();
    },
    100);
  setTimeout(
    function() {
      $('.autosubmit-after-pause').submit();
    },
    calculateDelayTime());

  runAnimationThread();
});

function calculateDelayTime() {
  if( $('.warpin').length )
    return 1500;

  return 500;
}

function animateViaCSS( css_class, css_property, color_gradient, date ) {
  var n = date.getSeconds() + date.getMinutes()*60 + date.getHours()*24;
  var color_1 = color_gradient[n%color_gradient.length];
  var color_2 = color_gradient[(n+1)%color_gradient.length];
  $(css_class).css(css_property,
                   $.xcolor.gradientlevel(color_1, color_2, date.getMilliseconds(), 1000));
}

function runAnimationThread() {
  var date = new Date();

  animateViaCSS( '.A', 'color', roguestar_preferences.strong_magic_colors, date );
  animateViaCSS( '.a', 'color', roguestar_preferences.weak_magic_colors, date );
  animateViaCSS( '.w', 'color', roguestar_preferences.water_colors, date );
  animateViaCSS( '.warpin', 'background-color', roguestar_preferences.strong_magic_colors, date );

  setTimeout( runAnimationThread, roguestar_preferences.animation_thread_interval );
}

