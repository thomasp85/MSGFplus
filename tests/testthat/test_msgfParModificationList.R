context('msgfParModificationList: Construction and standard properties')

mod1 <- msgfParModification(
    name='Carbamidomethyl',
    composition='C2H3N1O1',
    residues='C',
    type='fix',
    position='any'
)
mod2 <- msgfParModification(
    name='Oxidation',
    mass=15.994915,
    residues='M',
    type='opt',
    position='any'
)

test_that('constructor works', {
    expect_error(msgfParModificationList(nMod=1.5))
    expect_error(msgfParModificationList(nMod=1:4))
    expect_error(msgfParModificationList(nMod='test'))
    expect_error(msgfParModificationList(nMod=2, modifications=list(1, 2, 3, 'test')))
    expect_is(msgfParModificationList(nMod=2, modifications=list(mod1, mod2)), 'msgfParModificationList')
})

test_that('length works', {
    expect_equal(length(msgfParModificationList()), 0)
    expect_equal(length(msgfParModificationList(nMod=2, modifications=list(mod1, mod2))), 2)
})

test_that('indexing works', {
    modList <- msgfParModificationList(nMod=2, modifications=list(mod1, mod2))
    expect_error(modList[2])
    expect_equal(modList[[2]], mod2)
    modList[[3]] <- mod1
    modList[[2]] <- mod1
    expect_equal(modList[[3]], mod1)
    expect_equal(modList[[2]], mod1)
})