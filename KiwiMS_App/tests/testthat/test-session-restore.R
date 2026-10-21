# Unit tests for keeping the work of a browser tab across a dropped
# connection (app/logic/session_restore.R)

box::use(
  app/logic/session_restore[
    park_state,
    register_live,
    tab_token,
    take_state,
    unregister_live,
  ],
)

# fake_session(): a session with the given page address and a close() that
# says it was called
fake_session <- function(search = "") {
  closed <- FALSE
  list(
    clientData = shiny::reactiveValues(url_search = search),
    close = function() closed <<- TRUE,
    was_closed = function() closed
  )
}

test_that("a tab takes back the state parked under its token, once", {
  park_state("tab-aaaaaaaa", list(answer = 42))
  expect_equal(take_state("tab-aaaaaaaa")$answer, 42)
  expect_null(take_state("tab-aaaaaaaa"))
  # Another tab gets nothing
  park_state("tab-bbbbbbbb", list(answer = 1))
  expect_null(take_state("tab-cccccccc"))
  expect_equal(take_state("tab-bbbbbbbb")$answer, 1)
  expect_null(take_state(NULL))
})

test_that("a lost tab's state goes to the only page open, the latest one", {
  park_state("tab-old-1111", list(n = 1))
  Sys.sleep(0.05)
  park_state("tab-old-2222", list(n = 2))
  expect_null(take_state("tab-new-3333"))
  expect_equal(take_state("tab-new-3333", orphan = TRUE)$n, 2)
  expect_equal(take_state("tab-new-4444", orphan = TRUE)$n, 1)
  expect_null(take_state("tab-new-5555", orphan = TRUE))
})

test_that("a page replacing a still connected one of its tab takes over its state", {
  old_session <- fake_session()
  handle <- register_live("tab-live-1111", old_session, function() list(n = 7))
  state <- take_state("tab-live-1111")
  expect_equal(state$n, 7)
  expect_true(old_session$was_closed())
  # Retired: the old session does not park its state again as it ends
  expect_true(handle$retired)
  expect_null(take_state("tab-live-1111"))
})

test_that("a session only unregisters itself", {
  first <- register_live("tab-live-2222", fake_session(), function() list(n = 1))
  second <- register_live("tab-live-2222", fake_session(), function() list(n = 2))
  unregister_live("tab-live-2222", first)
  # The second is still taken over by a new page of the tab
  expect_equal(take_state("tab-live-2222")$n, 2)
})

test_that("the tab token is read from the page address, if it looks like one", {
  token <- function(search) shiny::isolate(tab_token(fake_session(search)))
  expect_equal(token("?tab=0f9c2a6e-1b2c-4d3e-8f90-123456789abc"), "0f9c2a6e-1b2c-4d3e-8f90-123456789abc")
  expect_null(token(""))
  expect_null(token("?tab=short"))
  expect_null(token("?tab=../../etc"))
})
