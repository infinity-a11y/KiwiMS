// Deconvolution spinner guard
// Keeps the deconv-pre-init class (which hides the shinycssloaders spinner)
// until the user first interacts with the deconvolution main panel or sidebar.
// This prevents a spinner flash on initial page load.
document.addEventListener('DOMContentLoaded', function () {
  var cid = 'app-deconvolution_main-deconvolution_ui_container';

  function activate() {
    var el = document.getElementById(cid);
    if (el) el.classList.remove('deconv-pre-init');
    document.body.removeEventListener('change', h, true);
    document.body.removeEventListener('click', h, true);
  }

  function h(e) {
    var c = document.getElementById(cid);
    var s = document.querySelector('.deconvolution-sidebar');
    if ((c && c.contains(e.target)) || (s && s.contains(e.target))) activate();
  }

  document.body.addEventListener('change', h, true);
  document.body.addEventListener('click', h, true);
});
