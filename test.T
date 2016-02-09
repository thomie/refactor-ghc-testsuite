
test(0,0,0,0)
# ^^^ should reformat the first test as well!!!

testFile = 'nothing'
# ^^^ is asis.

test(1,0,0,0)
# ^^^ is a test.

test(2,0,0,0)
'''^^^ is a test.'''

test(3,
# this is asis.
     0, 0, 0)

test(4,

# this is asis. Tricky!

     0, 0, 0)

test  (5,0,0,0)
# ^^^ is a test. Spaces before opening paren is ok.

test(6,0,0,0)
test(7,0,0,0)
test(8,0,0,0)
# ^^^ These are 3 tests.

test(9,0,0,0)

test(10,0,0,0)
   
test(11,0,0,0)
# ^^^ These are 3 more tests. There are trailing spaces between 10 and 11.

test(12, 1 + f(2) + 3, 0, 0)
# ^^^ Don't reduce expressions.

test(13, "long \
          string", 0, 0)
# ^^^ is a test.

test(14, [ ], 0, 0)
# ^^^ has a space between brackets (empty list).

test('test15', ["setup1", "setup2", "setup3"], compile, ['-v0'])
# ^^^ Comfortably fits on a single line.

test('test16', ["setup1", "setup2", "setup3", "setup4"], compile, ['-v0'])
# ^^^ Still fits on a single line.

test('test17', ["setup1", "setup2", "setup3", "setup4"], compile, ['-v0123456'])
# ^^^ Barely fits on a single line of 80 characters.

test('test18', ["setup1", "setup2", "setup3", "setup4", "setup5"],
     compile, ['-v0'])
# ^^^ Don't put /only/ the last argument on the second line? Doesn't seem
# possible with `pretty`, while also getting test15 to stay on a single line.

test('test19', ["setup1", "setup2", "setup3", "setup4", "setup5"],
     compile,
     ['very long                                                   string'])
# ^^^ Do put this very long string on its own line.

test('test20________________',
     ["setup1", "setup2", "setup3", "setup4", "setup5"],
     compile, ['-v0'])
# ^^^ setups don't fit on first line anymore

test('test21',
     ["setup1", "setup2", "setup3", "setup4", "setup5", "setup6", "setup7", "setup8"],
     compile, ['-v0'])
# ^^^ setups don't fit on a single line anymore

test('test22', normal, compile, ['-v0'])
# ^^^ Don't put brackets around "normal".

test('TH_Roles2',
     normalise_version('array', 'base', 'deepseq', 'ghc-prim', 'ghc-boot', 'integer-gmp', 'pretty', 'template-haskell', 'binary', 'bytestring', 'containers'),
     compile, ['-v0 -ddump-tc'])
# ^^^ also pretty print inner list nicely

# vvv last test ends with newline.
test(99,0,0,0)
