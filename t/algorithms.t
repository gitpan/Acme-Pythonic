# -*- Mode: Python -*-

use strict;
use warnings;

use Test::More 'no_plan';
use Acme::Pythonic;

use integer

# ----------------------------------------------------------------------

# $i ** $j (mod $n)
sub exp_mod:
    my ($i, $j, $n) = @_
    my $r = 1
    while $j:
        if $j % 2:
            $r = $i*$r % $n
        $j >>= 1
        $i = $i**2 % $n
    return $r

is exp_mod(3, 2, 7), 2
is exp_mod(7, 2, 43), 6
is exp_mod(9, 4, 6561), 0

# ----------------------------------------------------------------------

sub gcd:
    my ($a, $b) = @_
    my $r
    do:
        ($a, $b) = ($b, $a % $b)
    while $b
    return $a

is gcd(12, 1), 1
is gcd(1, 12), 1
is gcd(21, 12), 3
is gcd(49, 91), 7

# ----------------------------------------------------------------------

sub is_prime:
    my $n = shift
    return 1 if $n <= 3
    return 0 unless $n % 2

    my $a = int(rand($n - 3)) + 2
    return 0 if gcd($a, $n) > 1

    # find the greatest odd divisor of $n - 1
    # $n - 1 = 2^$k*$x is an invariant
    my $x = $n - 1 # even
    my $k = 0
    do:
        $x /= 2
        ++$k
    until $x % 2

    my $b = exp_mod($a, $x, $n)
    return 1 if $b == 1

    my $c = $b
    for 1..$k:
        $b = exp_mod($b, 2, $n)
        if $b == 1:
            last
        else:
            $c = $b

    return 0 if $b != 1

    my $d = gcd($c + 1, $n)
    return $d == 1 || $d == $n ? 1 : 0


is is_prime(2), 1
is is_prime(3), 1
is is_prime(16), 0
is is_prime(99), 0
is is_prime(647), 1
is is_prime(4900), 0
is is_prime(7919), 1


# ----------------------------------------------------------------------

sub bubblesort:
    my $array = shift
    for my $i = $#$array; $i; $i--:
        for my $j = 1; $j <= $i; $j++:
            if $array->[$j-1] > $array->[$j]:
                @$array[$j, $j-1] = @$array[$j-1, $j]

my @a = 1..10
bubblesort(\@a)
is_deeply \@a, [sort { $a <=> $b } @a]

@a = (1,2,3,2,-1)
bubblesort(\@a)
is_deeply \@a, [sort { $a <=> $b } @a]

@a = reverse 50.100
bubblesort(\@a)
is_deeply \@a, [sort { $a <=> $b } @a]
