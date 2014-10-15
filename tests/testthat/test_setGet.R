context('Getters and setters for msgfPar')

testPar <- msgfPar(
    database=system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'),
    tolerance='20 ppm',
    isotopeError=c(0,2),
    tda=TRUE,
    fragmentation='CID',
    instrument='TOF',
    enzyme='Lys-C',
    protocol='No protocol',
    ntt=2,
    modification=list(
        nMod=2,
        modifications=list(
            list(name='Carbamidomethyl',
                 composition='C2H3N1O1',
                 residues='C',
                 type='fix',
                 position='any'
            ),
            list(name='Oxidation',
                 mass=15.994915,
                 residues='M',
                 type='opt',
                 position='any'
            )
        )
    ),
    lengthRange=c(6,40),
    chargeRange=c(2,7),
    matches=1
)

test_that('database can be set and get', {
    db(testPar) <- 'testfile.fasta'
    expect_equal(db(testPar), 'testfile.fasta')
})

test_that('tolerance can be set and get', {
    tolerance(testPar) <- c('30 ppm', '30 ppm')
    expect_equal(tolerance(testPar), c('30 ppm', '30 ppm'))
    toleranceUnit(testPar) <- 'Da'
    expect_equal(tolerance(testPar), c('30 Da', '30 Da'))
    toleranceRange(testPar) <- c(1, 1.5)
    expect_equal(tolerance(testPar), c('1 Da', '1.5 Da'))
})

test_that('isotope error can be set and get', {
    isotopeError(testPar) <- c(2, 3)
    expect_equal(isotopeError(testPar), c(2, 3))
})

test_that('tda can be set and get', {
    tda(testPar) <- FALSE
    expect_false(tda(testPar))
})

test_that('fragmentation can be set and get', {
    fragmentation(testPar) <- 0
    expect_equal(fragmentation(testPar), c('As written in the spectrum or CID if no info'=0))
    fragmentation(testPar) <- 1
    expect_equal(fragmentation(testPar), c(CID=1))
    expect_error(fragmentation(testPar) <- 5, 'Unknown fragmentation method')
    expect_error(fragmentation(testPar) <- 'test', 'Unknown fragmentation method')
})

test_that('instrument can be set and get', {
    instrument(testPar) <- 0
    expect_equal(instrument(testPar), c(LowRes=0))
    instrument(testPar) <- 1
    expect_equal(instrument(testPar), c(HighRes=1))
    expect_error(instrument(testPar) <- 4, 'Unknown instrument')
    expect_error(instrument(testPar) <- 'test', 'Unknown instrument')
})

test_that('enzyme can be set and get', {
    enzyme(testPar) <- 0
    expect_equal(enzyme(testPar), c('unspecific cleavage'=0))
    enzyme(testPar) <- 1
    expect_equal(enzyme(testPar), c(Trypsin=1))
    expect_error(enzyme(testPar) <- 10, 'Unknown enzyme')
    expect_error(enzyme(testPar) <- 'test', 'Unknown enzyme')
})

test_that('protocol can be set and get', {
    protocol(testPar) <- 0
    expect_equal(protocol(testPar), c('No protocol'=0))
    protocol(testPar) <- 1
    expect_equal(protocol(testPar), c(Phosphorylation=1))
    expect_error(protocol(testPar) <- 4, 'Unknown protocol')
    expect_error(protocol(testPar) <- 'test', 'Unknown protocol')
})

test_that('ntt can be set and get', {
    ntt(testPar) <- 1
    expect_equal(ntt(testPar), 1)
    expect_error(ntt(testPar) <- 3, 'value must be between 0 and 2')
    expect_error(ntt(testPar) <- -1, 'value must be between 0 and 2')
})

test_that('modifications can be set and get', {
    nMod(testPar) <- 4
    expect_equal(nMod(testPar), 4)
    tempMod <- mods(testPar)
    mods(testPar) <- msgfParModificationList()
    expect_equal(mods(testPar), msgfParModificationList())
    mods(testPar) <- tempMod
    expect_equal(mods(testPar), tempMod)
})

test_that('length ranges can be set and get', {
    lengthRange(testPar) <- c(3, 10)
    expect_equal(lengthRange(testPar), c(min=3, max=10))
    expect_error(lengthRange(testPar) <- 3, 'value must be of length 2')
    expect_error(lengthRange(testPar) <- c(10, 3), 'lower bound must be less than upper bound')
})

test_that('charge ranges can be set and get', {
    chargeRange(testPar) <- c(2, 9)
    expect_equal(chargeRange(testPar), c(min=2, max=9))
    expect_error(chargeRange(testPar) <- 2, 'value must be of length 2')
    expect_error(chargeRange(testPar) <- c(9, 2), 'lower bound must be less than upper bound')
})

test_that('number of matches can be set and get', {
    matches(testPar) <- 2
    expect_equal(matches(testPar), 2)
    expect_error(matches(testPar) <- 2:4, 'value can only be of length 1')
})