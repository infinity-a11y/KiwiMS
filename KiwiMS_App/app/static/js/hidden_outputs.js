// Spinner-wrapped HTML outputs that are hidden when they are built (a tab not
// selected yet, a result interface in the background) must learn they are
// shown once their part of the page opens.
//
// Shiny 1.14 refreshes an output's hidden state only from a ResizeObserver on
// the output and an IntersectionObserver on its nearest parent that is not an
// HTML output. Around a uiOutput, withSpinner() puts a container that is
// display: contents here (main.scss), so it has no box and never intersects,
// and an empty uiOutput is 0 x 0, so it never resizes: the output would stay
// suspended and its spinner turn forever. Plots and tables have a size and are
// not affected.
//
// recheck() runs Shiny's own refresh of those outputs (the callback its
// observers call, debounced); an output already up to date sends nothing.
(function () {
  var SELECTOR =
    '.shiny-spinner-output-container > .shiny-html-output.shiny-bound-output';

  function recheck() {
    $(SELECTOR).each(function () {
      var refresh = $(this).data('shiny-intersection-observer-callback');
      if (typeof refresh === 'function') refresh();
    });
  }

  window.kiwimsRecheckOutputs = recheck;

  $(document).on(
    'shown.bs.tab shown.bs.collapse shown.bs.modal shown.bs.offcanvas',
    recheck
  );
})();
