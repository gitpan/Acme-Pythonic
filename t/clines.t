# -*- Mode: Python -*-

use strict;
use warnings;

use Test::More 'no_plan';
use Acme::Pythonic;

# ----------------------------------------------------------------------

sub foo:
    my $i = \
       7
    return $i

is foo, 7

# ----------------------------------------------------------------------

sub mygrep (&@):
    my $code = shift
    my @result
    foreach @_:
        push @result, \
             $_ \
             if \
             &$code;
    return @result;

my @array = mygrep { $_ % 2 } 0..5;
is_deeply \@array, [1, 3, 5]

# ----------------------------------------------------------------------

my $coderef = sub:
    my $n = \
       shift
    $n *= \
       3

is $coderef->(3), 9

# ----------------------------------------------------------------------

my $fib
$fib = sub:
    my $n = shift
    die if $n < 0
    $n < 2 ? \
       $n : \
       $fib->($n - 1) \
       + $fib->\
       ($n - 2)

is $fib->(5), 5

# ----------------------------------------------------------------------

my $a = [1,
         2,
     3,

   4,
         ]

is_deeply $a, [1,2,3,4]

# ----------------------------------------------------------------------

my %n = (foo =>
   "heh",
                       bar      =>
                "moo")

ok exists $n{bar}
