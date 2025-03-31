const resizeSelectOptions = () => {
  const options = document.querySelectorAll('.selectize-dropdown-content .option');
  const dropdown = document.querySelector('.selectize-dropdown-content');

  if (!dropdown || !options.length) return;

  const dropdownWidth = dropdown.offsetWidth;

  options.forEach(option => {
    let scaleFactor = 1;
    option.style.setProperty('--scale-factor', '1');

    while (option.scrollWidth > dropdownWidth && scaleFactor > 0.5) {
      scaleFactor -= 0.05;
      option.style.setProperty('--scale-factor', scaleFactor.toString());
    }
  });
};

$(document).ready(function() {
  // Initialize observer for dropdown opening
  const observer = new MutationObserver((mutations) => {
    mutations.forEach((mutation) => {
      if (mutation.target.classList.contains('selectize-dropdown-content')) {
        resizeSelectOptions();
      }
    });
  });

  // Start observing when selectize is initialized
  Shiny.addCustomMessageHandler('selectize-init', function(id) {
    const dropdown = document.querySelector(`#${id} .selectize-dropdown-content`);
    if (dropdown) {
      observer.observe(dropdown, {
        attributes: true,
        childList: true,
        subtree: true
      });
    }
  });
});

$(document).on('shiny:value', function(event) {
        if (event.name === 'app-deconvolution_process-decon_rep_logtext') {
            var preElement = document.getElementById('app-deconvolution_process-decon_rep_logtext');
            if (preElement) {
                var isAtBottom = preElement.scrollHeight - preElement.scrollTop <= preElement.clientHeight + 5;

                if (isAtBottom) {
                    preElement.scrollTop = preElement.scrollHeight;
                }

                preElement.addEventListener('scroll', function() {
                    var isUserScrollingUp = preElement.scrollHeight - preElement.scrollTop > preElement.clientHeight + 5;
                    preElement.dataset.userScroll = isUserScrollingUp ? 'true' : 'false';
                });

                if (preElement.dataset.userScroll !== 'true') {
                    preElement.scrollTop = preElement.scrollHeight;
                }
            }
        }
    });

