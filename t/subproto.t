# -*- Mode: Python -*-

use warnings;

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

# Inspired by Acme::Don't
sub ignore (&):
    pass

$n = 1
if 1:
    ignore:
        $n = 2
is $n, 1
