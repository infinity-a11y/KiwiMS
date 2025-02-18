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

// Add to your Shiny app
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
