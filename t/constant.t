use Test::More 'no_plan';
use Acme::Pythonic debug => 0;

use constant FOO => 0
use constant BAR => 1

use constant {BAZ => 2,
              MOO => 3,
              ZOO => 4,}

$foo = 5

is FOO, 0
is BAR, 1
is BAZ, 2
is MOO, 3
is ZOO, 4
is $foo, 5
