# -*- Mode: Python -*-

use strict;
use warnings;

use Test::More 'no_plan';
use Acme::Pythonic;

# ----------------------------------------------------------------------

my $sgn = 1
for my $i = 0; $i < 3; ++$i:
    $sgn *= -1

ok $sgn, -1

# ----------------------------------------------------------------------

BLOCK_TO_DISABLE_STRICTNESS_LOCALLY:
    no strict
    $sgn = 1
    for $i = 0; $i < 3; ++$i:
        $sgn *= -1

ok $sgn, -1


# ----------------------------------------------------------------------

my @foo = 1..1000
my $n = @foo
for ; @foo; pop @foo:
    --$n
    $n += 0

is $n, 0

# ----------------------------------------------------------------------

for do {@foo = 1..1000; $n = 0}; @foo; pop @foo:
    ++$n
    $n += 0

is $n, 1000

# ----------------------------------------------------------------------

@foo = 1..1000
$n = 0
for @foo:
    $n += $_
    $n += 0

is $n, 500500

# ----------------------------------------------------------------------

@foo = 1..1000
$n = 0
for in @foo:
    $n += $_
    $n += 0

is $n, 500500

# ----------------------------------------------------------------------

@foo = 1..1000
$n = 0
my $elt
for $elt @foo:
    $n += $elt
    $n += 0

is $n, 500500

# ----------------------------------------------------------------------

@foo = 1..1000
$n = 0
for $elt in @foo:
    $n += $elt
    $n += 0

is $n, 500500

# ----------------------------------------------------------------------

@foo = 1..1000
$n = 0
for my $foo @foo:
    ++$n
    $n += 0

is $n, scalar @foo

# ----------------------------------------------------------------------

@foo = 1..1000
$n = 0
for my $moo in @foo:
    ++$n
    $n += 0

is $n, scalar @foo

# ----------------------------------------------------------------------

$n = 0
for my $x in do { reverse 1..1000 }:
    $n += $x
    $n += 0

is $n, 500500

# ----------------------------------------------------------------------

my @array = qw(foo ofo oof)
for my $perm in @array:
    $perm .= $perm
continue:
    $perm =~ s/f//g

is_deeply \@array, [('oooo') x 3]

# ----------------------------------------------------------------------

package foo
our $bar = 3
my @vars = ()
push @vars, $_ for keys %foo::
Test::More::is_deeply(\@vars, ['bar'])