# -*- Mode: Python -*-

use strict;
use warnings;

use Test::More 'no_plan';
use Acme::Pythonic;

# ----------------------------------------------------------------------

sub foo:
    my $i = 7
    return $i

is foo, 7

# ----------------------------------------------------------------------

sub mygrep (&@):
    my $code = shift
    my @result
    foreach @_:
        push @result, $_ if &$code;
    return @result;

my @array = mygrep { $_ % 2 } 0..5;
is_deeply \@array, [1, 3, 5]

@array = mygrep:
             my $aux = $_
             $aux *= 3
             $aux += 1
             $aux % 2
         reverse 0..5
is_deeply \@array, [4, 2, 0]

# ----------------------------------------------------------------------

my $coderef = sub:
    my $n = shift
    $n *= 3

is $coderef->(3), 9

# ----------------------------------------------------------------------

my $fib
$fib = sub:
    my $n = shift
    die if $n < 0
    $n < 2 ? $n : $fib->($n - 1) + $fib->($n - 2)

is $fib->(5), 5

# ----------------------------------------------------------------------

sub count_3n_plus_1_steps:
    my $n = shift
    my $steps = 0
    while $n != 1:
        $steps++
        if $n % 2:
            $n = 3*$n + 1
        else:
            $n /= 2
    $steps

is count_3n_plus_1_steps(1), 0
is count_3n_plus_1_steps(2), 1
is count_3n_plus_1_steps(5), 5
