library(MSGFplus)

context('msgfParModification: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParModification(name='test', composition='G3F2', residues='*', type='opt', position='any'))
    expect_error(msgfParModification(name='test', composition='C2H3N1O1', residues='X', type='opt', position='any'))
    expect_error(msgfParModification(name='test', composition='C2H3N1O1', residues='*', type='fixed(should be fix)', position='any'))
    expect_error(msgfParModification(name='test', composition='C2H3N1O1', residues='*', type='opt', position='s-term'))
    expect_error(msgfParModification(name='test', residues='*', type='opt', position='any'))
    expect_error(msgfParModification())
    expect_is(msgfParModification(name='test', composition='C2H3N1O1', residues='*', type='opt', position='any'), 'msgfParModification')
    expect_is(msgfParModification(name='test', mass=150.3333, residues='*', type='opt', position='any'), 'msgfParModification')
    expect_is(msgfParModification(name='test', mass=150.3333, residues='STY', type='opt', position='any'), 'msgfParModification')
    expect_is(msgfParModification(name='test', composition='CH-3', residues='*', type='opt', position='any'), 'msgfParModification')
})