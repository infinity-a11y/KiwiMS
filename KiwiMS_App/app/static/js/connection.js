// Connection of the page to the KiwiMS server (see `connections` in main.R and
// session_restore.R).
//
// - The tab has a token (sessionStorage, and ?tab= in the address so the
//   server reads it with the page's first message). A page reloaded in the
//   same tab - after a dropped connection, or F5 - keeps it, and the server
//   hands it the state its last session parked: the work comes back.
// - A page that is closed or reloaded says so before it goes
//   (kiwims_page_closing); only then does the server stop, after a grace
//   period a reload comes back within.
// - A connection the browser dropped on its own (a tab put to sleep, standby)
//   leaves the server running. The page then shows a notice, checks whether
//   the server answers and reloads once it does. Reloaded that way less than
//   a minute ago, it waits for a click on Reload instead, so a session that
//   ends at once (an error) cannot reload in a loop.
// - Against dropping in the first place: a Web Lock, which keeps Chrome and
//   Edge from freezing the tab, and the server's heartbeat, answered.
// - What happened to the connection is noted (tab hidden, frozen, paused,
//   socket closed and how) and sent to the server's log once the page is
//   connected again (kiwims_connection_report).
(function () {
  var AUTO_RELOAD_PAUSE = 60000;
  var POLL_MS = 3000;
  var GONE_AFTER_MS = 10000;
  // A timer or heartbeat late by more than this means the page did not run
  var PAUSE_MS = 120000;
  var NOTES_MAX = 40;

  var TOKEN = 'kiwims-tab-token';
  // When the page last reloaded itself, and whether it is yet to say so
  var RELOADED_AT = 'kiwims-reconnect-at';
  var RELOADED_NOTE = 'kiwims-reconnect-note';
  var NOTES = 'kiwims-connection-notes';

  var closing = false;
  var notice = null;
  var pollTimer = null;

  function store(key, value) {
    try {
      if (value === null) {
        window.sessionStorage.removeItem(key);
      } else {
        window.sessionStorage.setItem(key, value);
      }
    } catch (e) {}
  }

  function stored(key) {
    try {
      return window.sessionStorage.getItem(key);
    } catch (e) {
      return null;
    }
  }

  // Token of the tab. Kept by a reload (and by a tab the browser discarded
  // and loads again); a new or duplicated tab gets its own, so it does not
  // take the work of another.
  var token = (function () {
    var nav = window.performance && performance.getEntriesByType ?
      performance.getEntriesByType('navigation')[0] : null;
    var reloaded = (nav && nav.type === 'reload') || document.wasDiscarded;
    var kept = reloaded ? stored(TOKEN) : null;
    if (kept && /^[A-Za-z0-9-]{8,64}$/.test(kept)) return kept;
    var fresh = (window.crypto && crypto.randomUUID) ? crypto.randomUUID() :
      Date.now().toString(36) + '-' + Math.random().toString(36).slice(2, 12);
    store(TOKEN, fresh);
    return fresh;
  })();

  try {
    var url = new URL(window.location.href);
    if (url.searchParams.get('tab') !== token) {
      url.searchParams.set('tab', token);
      window.history.replaceState(window.history.state, '', url.toString());
    }
  } catch (e) {}

  // Held for the life of the page
  try {
    if (navigator.locks && navigator.locks.request) {
      navigator.locks.request('kiwims-tab-' + token, function () {
        return new Promise(function () {});
      });
    }
  } catch (e) {}

  function note(text) {
    var notes;
    try {
      notes = JSON.parse(stored(NOTES) || '[]');
    } catch (e) {
      notes = [];
    }
    notes.push(new Date().toISOString() + '  ' + text);
    if (notes.length > NOTES_MAX) notes = notes.slice(-NOTES_MAX);
    store(NOTES, JSON.stringify(notes));
  }

  function showNotice(title, text, withButton) {
    if (!notice) {
      notice = document.createElement('div');
      notice.id = 'kiwims-connection-notice';
      notice.setAttribute('role', 'alert');
      notice.style.cssText = [
        'position: fixed',
        'top: 50%',
        'left: 50%',
        'transform: translate(-50%, -50%)',
        'z-index: 100000',
        'max-width: min(440px, calc(100vw - 32px))',
        'padding: 18px 22px',
        'border-radius: 8px',
        'background: var(--bs-body-bg, #ffffff)',
        'color: var(--bs-body-color, #212529)',
        'box-shadow: 0 8px 28px rgba(0, 0, 0, 0.35)',
        'font-size: 15px',
        'line-height: 1.45'
      ].join(';');
      document.body.appendChild(notice);
    }
    notice.innerHTML = '';
    var head = document.createElement('div');
    head.style.cssText = 'font-weight: 600; margin-bottom: 6px;';
    head.textContent = title;
    var body = document.createElement('div');
    body.textContent = text;
    notice.appendChild(head);
    notice.appendChild(body);
    if (withButton) {
      var button = document.createElement('button');
      button.type = 'button';
      button.className = 'btn btn-primary btn-sm';
      button.style.marginTop = '12px';
      button.textContent = 'Reload';
      button.addEventListener('click', reload);
      notice.appendChild(button);
    }
    notice.style.display = 'block';
  }

  function hideNotice() {
    if (notice) notice.style.display = 'none';
  }

  function reload() {
    store(RELOADED_AT, String(Date.now()));
    store(RELOADED_NOTE, '1');
    window.location.reload();
  }

  function serverAnswers() {
    return fetch('static/js/connection.js?ping=' + Date.now(), {
      cache: 'no-store'
    }).then(
      function (response) {
        return response.ok;
      },
      function () {
        return false;
      }
    );
  }

  function watchServer() {
    var since = Date.now();
    var last = parseInt(stored(RELOADED_AT), 10);
    var mayReload = !(last && Date.now() - last < AUTO_RELOAD_PAUSE);

    showNotice(
      'Connection to KiwiMS interrupted',
      'The browser lost its connection to KiwiMS, e.g. after the tab was ' +
        'inactive for a while. Reconnecting…',
      false
    );

    function poll() {
      serverAnswers().then(function (ok) {
        if (ok) {
          if (mayReload) {
            note('server answers again, page reloads');
            reload();
            return;
          }
          showNotice(
            'Connection to KiwiMS interrupted',
            'KiwiMS is running. Reload the page to continue where you were.',
            true
          );
        } else if (Date.now() - since > GONE_AFTER_MS) {
          showNotice(
            'KiwiMS is not running',
            'The KiwiMS window was closed or the program stopped. Start ' +
              'KiwiMS again to continue.',
            false
          );
        }
        pollTimer = window.setTimeout(poll, POLL_MS);
      });
    }
    poll();
  }

  // Sent straight through the socket: Shiny.setInputValue() batches on a
  // timer, which no longer runs while the page goes
  window.addEventListener('pagehide', function () {
    closing = true;
    try {
      if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()) {
        Shiny.shinyapp.sendInput({ kiwims_page_closing: Date.now() });
      }
    } catch (e) {}
  });

  // Back from the browser's page cache: the old session is gone
  window.addEventListener('pageshow', function (event) {
    if (event.persisted) window.location.reload();
  });

  // What the page goes through while it is open
  document.addEventListener('visibilitychange', function () {
    note('page ' + document.visibilityState);
  });
  document.addEventListener('freeze', function () {
    note('page frozen by the browser');
  });
  document.addEventListener('resume', function () {
    note('page resumed by the browser');
  });
  window.addEventListener('offline', function () {
    note('browser offline');
  });
  window.addEventListener('online', function () {
    note('browser online');
  });

  // Timers stop while the computer sleeps or the browser freezes the page;
  // a hidden page's timers are only slowed down to once a minute
  var lastTick = Date.now();
  window.setInterval(function () {
    var now = Date.now();
    if (now - lastTick > PAUSE_MS) {
      note('page did not run for ' + Math.round((now - lastTick) / 1000) +
        ' s (standby or frozen tab)');
    }
    lastTick = now;
  }, 15000);

  var lastBeat = null;

  $(document).on('shiny:connected', function () {
    lastBeat = Date.now();

    // For the log and the support package
    Shiny.setInputValue('kiwims_browser', {
      userAgent: navigator.userAgent,
      language: navigator.language,
      screen: window.screen.width + 'x' + window.screen.height,
      window: window.innerWidth + 'x' + window.innerHeight,
      pixelRatio: window.devicePixelRatio
    });
    // Shiny takes only handlers of one argument
    Shiny.addCustomMessageHandler('kiwims_heartbeat', function (message) {
      var now = Date.now();
      if (lastBeat && now - lastBeat > PAUSE_MS) {
        note('no heartbeat from the server for ' +
          Math.round((now - lastBeat) / 1000) + ' s');
      }
      lastBeat = now;
      Shiny.setInputValue('kiwims_heartbeat', now);
    });

    // How the socket closes, when it does
    try {
      var socket = Shiny.shinyapp.$socket;
      if (socket && socket.addEventListener) {
        socket.addEventListener('close', function (event) {
          if (closing) return;
          note('connection closed: code ' + event.code +
            (event.reason ? ' (' + event.reason + ')' : '') +
            ', clean ' + event.wasClean +
            ', browser ' + (navigator.onLine ? 'online' : 'offline') +
            ', page ' + document.visibilityState);
        });
      }
    } catch (e) {}

    // A page reloaded after a dropped connection says what happened, here
    // and in the server's log
    var notes = stored(NOTES);
    if (notes && notes.indexOf('connection closed') !== -1) {
      try {
        Shiny.setInputValue(
          'kiwims_connection_report',
          JSON.parse(notes).join('\n'),
          { priority: 'event' }
        );
      } catch (e) {}
    }
    store(NOTES, null);

    if (!stored(RELOADED_NOTE)) return;
    store(RELOADED_NOTE, null);
    showNotice(
      'KiwiMS reconnected',
      'The page was reloaded after the connection was interrupted and ' +
        'continues where you were.',
      false
    );
    window.setTimeout(hideNotice, 6000);
  });

  $(document).on('shiny:disconnected', function () {
    if (closing || pollTimer !== null) return;
    watchServer();
  });
})();
