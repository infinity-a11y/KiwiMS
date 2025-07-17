const resizeSelectOptions = () => {
  const options = document.querySelectorAll('.selectize-dropdown-content .option');
  const dropdown = document.querySelector('.selectize-dropdown-content');

  if (!dropdown || !options.length) {
    return;
  }

  const dropdownWidth = dropdown.offsetWidth;

  options.forEach((option) => {
    let scaleFactor = 1;
    option.style.setProperty('--scale-factor', '1');

    while (option.scrollWidth > dropdownWidth && scaleFactor > 0.5) {
      scaleFactor -= 0.05;
      option.style.setProperty('--scale-factor', scaleFactor.toString());
    }
  });
};

$(document).ready(() => {
  // Initialize observer for dropdown opening
  const observer = new MutationObserver((mutations) => {
    mutations.forEach((mutation) => {
      if (mutation.target.classList.contains('selectize-dropdown-content')) {
        resizeSelectOptions();
      }
    });
  });

  // Start observing when selectize is initialized
  Shiny.addCustomMessageHandler('selectize-init', (id) => {
    const dropdown = document.querySelector(`#${id} .selectize-dropdown-content`);
    if (dropdown) {
      observer.observe(dropdown, {
        attributes: true,
        childList: true,
        subtree: true,
      });
    }
  });
});

$(document).on('shown.bs.modal', () => {
  const preElement = document.getElementById('app-deconvolution_process-logtext');
  if (!preElement) {
    return;
  }

  let shouldAutoScroll = true;

  // Update scroll state when user scrolls
  preElement.addEventListener('scroll', () => {
    const atBottom = (preElement.scrollHeight - preElement.scrollTop
    - preElement.clientHeight) <= 10;
    shouldAutoScroll = atBottom;
  });

  // When Shiny updates the output, decide whether to scroll
  $(document).on('shiny:value', (event) => {
    if (event.name === 'app-deconvolution_process-logtext') {
      setTimeout(() => {
        if (shouldAutoScroll) {
          preElement.scrollTo({ top: preElement.scrollHeight, behavior: 'smooth' });
        }
      }, 50);
    }
  });
});

$(document).on('shown.bs.modal', () => {
  const button = document.getElementById('app-deconvolution_process-make_deconvolution_report');
  if (!button) {
    return;
  }

  button.addEventListener('click', () => {
    const modalContent = document.querySelector('.modal-content');
    if (!modalContent) {
      return;
    }

    // Check if the element already exists
    const preElement = document.getElementById('app-deconvolution_process-decon_rep_logtext');
    if (preElement) {
      smartScroll('app-deconvolution_process-decon_rep_logtext');
      return;
    }

    // Set up MutationObserver to watch for the element being added
    const observer = new MutationObserver((mutations, obs) => {
      const preElement = document.getElementById('app-deconvolution_process-decon_rep_logtext');
      if (preElement) {
        smartScroll('app-deconvolution_process-decon_rep_logtext');
        obs.disconnect(); // Stop observing once the element is found
      }
    });

    observer.observe(modalContent, {
      childList: true,
      subtree: true
    });

    // Fallback: Retry after a short delay if the element isn't found immediately
    setTimeout(() => {
      const preElement = document.getElementById('app-deconvolution_process-decon_rep_logtext');
      if (preElement) {
        smartScroll('app-deconvolution_process-decon_rep_logtext');
        observer.disconnect();
      }
    }, 500);
  });
});

export function smartScroll(elementID) {
  const preElement = document.getElementById(elementID);
  if (!preElement) {
    return;
  }

  let shouldAutoScroll = true;

  preElement.scrollTo({ top: preElement.scrollHeight, behavior: 'smooth' });

  // Update scroll state when user scrolls
  preElement.addEventListener('scroll', () => {
    const atBottom = (preElement.scrollHeight - preElement.scrollTop
    - preElement.clientHeight) <= 10;
    shouldAutoScroll = atBottom;
  });

  // When Shiny updates the output, decide whether to scroll
  $(document).on('shiny:value', (event) => {
    if (event.name === elementID) {
      setTimeout(() => {
        if (shouldAutoScroll) {
          preElement.scrollTo({ top: preElement.scrollHeight, behavior: 'smooth' });
        }
      }, 50);
    }
  });
}

export function disableDismiss() {
  const button = document.querySelector('[data-dismiss="modal"]');
  if (button) {
    button.style.cursor = 'not-allowed';
    button.style.pointerEvents = 'none';
    button.style.opacity = '0.65';
  }
}

export function enableDismiss() {
  const button = document.querySelector('[data-dismiss="modal"]');
  if (button) {
    button.style.cursor = 'pointer';
    button.style.pointerEvents = 'all';
    button.style.opacity = '1';
  }
}