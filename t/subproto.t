# -*- Mode: Python -*-

use warnings;

package Bar;
sub bar (&);
sub foo (&);
sub twice (&);

use Test::More 'no_plan';
use Acme::Pythonic debug => 0;

# ----------------------------------------------------------------------

sub mygrep (&@):
    my $code = shift
    my @result
    foreach @_:
        push @result, $_ if &$code
    return @result

my @array = mygrep { $_ % 2 } 0..5
is_deeply \@array, [1, 3, 5]

@array = mygrep:
    my $aux = $_
    $aux *= 3
    $aux += 1
    $aux % 2
reverse 0..5
is_deeply \@array, [4, 2, 0]

# ----------------------------------------------------------------------

$n = 1
sub foo (&):
    my $code = shift
    if defined $code:
        return 5
    else:
        return 7

$n = foo:
    pass
is $n, 5

# ----------------------------------------------------------------------

$n = 1
sub bar (&):
    my $code = shift
    $code->()

$n = bar:
    2*3
Test::More::is $n, 6

# ----------------------------------------------------------------------

sub twice (&):
    my $code = shift
    $code->()
    $code->()

$n = "foo"
twice:
    $n .= "bar"
    $n .= "baz"

is $n, "foobarbazbarbaz"
