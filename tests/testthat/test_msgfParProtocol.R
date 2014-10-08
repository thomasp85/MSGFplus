context('msgfParProtocol: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParProtocol(0:3))
    expect_error(msgfParProtocol(10))
    expect_error(msgfParProtocol('test'))
    expect_is(msgfParProtocol(0), 'msgfParProtocol')
    expect_is(msgfParProtocol('Phosphorylation'), 'msgfParProtocol')
})

test_that('length works', {
    expect_equal(length(msgfParProtocol()), 0)
    expect_equal(length(msgfParProtocol(0)), 1)
})