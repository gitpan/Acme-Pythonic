package Acme::Pythonic;

# Please, if you tested it in some earlier version of Perl and works let
# me know! The versions of Filter::Simple and Test::More would be useful
# as well, since they are put in Makefile.PL.
use 5.008002;
use strict;
use warnings;

use vars qw($VERSION $DEBUG);
$VERSION = '0.02';

use Text::Tabs;

sub import {
    my ($package, %cfg) = @_;
    $DEBUG = $cfg{debug};
}

use Filter::Simple;
FILTER_ONLY code => sub {
    # The order of execution matters.
    normalize_newlines();
    join_continuation_lines();
    remove_comments();
    join_lines_ending_in_commas();

    my $lines = [ split /\n/ ];
    uncolonize_and_parenthesize($lines);
    semicolonize_and_bracketize($lines);
    move_semicolons($lines);

    $_ = join "\n", @$lines;
    if ($DEBUG) {
        print "$_\n";
        $_ = "1;\n";
    }
};


# This regexp matches a 7-bit ASCII identifier. We use atomic grouping
# because an identifier cannot be backtracked.
my $id = qr/(?>[_a-zA-Z](?:[_a-zA-Z0-9']|::)*)/;

# In the trials I've done seems like the Python interpreter understands
# any of the three conventions, even if they are not the ones in the
# platform, and even if they are mixed in the same file.
sub normalize_newlines {
    tr/\015/\n/;
    tr/\012/\n/;
    s/\n+/\n/g;
}


# We remove continuation lines in one shot. This subroutine have to be
# called _before_ comments are removed, see remove_comments.
sub join_continuation_lines {
    s/\\\n\s*//g;
}


# Tries its best at removing trailing Pythonic colons, and putting
# Perlish parens around EXPRs, LISTs, etc.
#
# Modifies @$lines in place.
sub uncolonize_and_parenthesize {
    my $lines = shift;

    foreach my $line (@$lines) {
        # We check the parity of the number of ending colons to try to
        # avoid breaking
        #
        #    print for keys %main::
        #
        next unless $line =~ /(:+)\s*$/ && length($1) % 2;

        if ($line =~ /^\s*($id)\s*:\s*$/o) {
            # Labels cannot have lower-case letters in Acme::Pythonic.
            $line =~ s/:\s*$// if $1 =~ /[[:lower:]]/;
            next;
        }

        # we can safely remove the ending colon now
        $line =~ s/:\s*$//;

        # subroutines are ok this way
        next if $line =~ /^\s*sub\b/;

        # these need parens after the keyword
        next if $line =~ s/^(\s* if    ) (.*)$/$1 ($2)/x;
        next if $line =~ s/^(\s* elsif ) (.*)$/$1 ($2)/x;
        next if $line =~ s/^(\s* unless) (.*)$/$1 ($2)/x;

        # these may be preceded by a label
        next if $line =~ s/^(\s* (?:$id\s*:)? \s* while) (.*)$/$1 ($2)/ox;
        next if $line =~ s/^(\s* (?:$id\s*:)? \s* until) (.*)$/$1 ($2)/ox;

        # try your best with fors
        next if $line =~ s/^(\s* (?:$id\s*:\s*)? for(?:each)?) (.*)$/fortype_guesser($1,$2)/oxe;
    }
}


# Tries its best at guessing a for(each) type or, at least, how to put
# parens around what it has been matched to the right of the keyword.
#
# Returns the string to be substituted.
sub fortype_guesser {
    my ($for, $rest) = @_;
    my $guess = "";

    # Try to match "for VAR in LIST", and "for VAR LIST"
    if ($rest =~ m/^(\s* (?:my|our)? \s* \$ $id) \s+in\b ((?: (?:\s*[\$\@%&,\\]) | (?:\s+ $id) ) .*)$/ox ||
        $rest =~ m/^(\s* (?:my|our)? \s* \$ $id)         ((?: (?:\s*[\$\@%&,\\]) | (?:\s+ $id) ) .*)$/ox) {
        $guess = "$for $1 ($2)";
    } else {
        # We are not sure whether this is a for or a foreach, but it is
        # very likely that putting parens around gets it right.
        $rest =~ s/^\s*in\b//; # fixes "foreach in LIST"
        $guess = "$for ($rest)";
    }

    return $guess;
}


# Tries its best at figuring out blocks, and putting semicolons.
#
# Modifies @$lines in place.
#
# Language::Pythonesque was the basis of this subroutine. We keep its
# way to put semicolons by now (they are prepended to the _next_ line),
#
# They won't be in their correct place in general, however, because
# whilst
#
#    my $a = 3
#    ;print $a
#
# is valid, that does not work with POD, for instance:
#
#    my $a = 3
#
#    =pod
#
#    POD section
#
#    =cut
#
#    ;print $a
#
# does not compile.
#
# Thus, after this subroutine some cleanup has to be done. That's the
# job of move_semicolons().
sub semicolonize_and_bracketize {
    my $lines = shift;

    my @stack = ();
    my $prev_line;
    foreach my $line (@$lines) {
        next if $line =~ /^[\s$;]*$/; # skip blank lines

        my $orig_line = $line;
        my ($indent) = $line =~ /^(\s*)/;
        $indent = length(expand($indent));
        my $prev_indent = @stack ? $stack[-1]{indent} : 0;
        if ($prev_indent < $indent) {
            my ($prev_word) = $prev_line =~ /(\w+)\s*$/;
            $line =~ s/^(\s+)pass\s*$/$1/;
            my $spc = ' ' x $prev_indent;
            push @stack, {indent => $indent, prev_word => $prev_word};
            $line =~ s/^/$spc\{\n/;
        } elsif ($prev_indent > $indent) {
            my $close = '';
            while ($prev_indent > $indent) {
                my $prev_word = $stack[-1]{prev_word};
                pop @stack;
                $prev_indent = @stack ? $stack[-1]{indent} : 0;
                my $spc = ' ' x $prev_indent;
                $close .= "$spc}";
                $close .= ";" if defined $prev_word && $prev_word =~ /^(?:sub|do|eval)$/;
                $close .= "\n";
            }
            $line =~ s/^/$close/;
        } elsif (defined $prev_line && $prev_line !~ /^\s*$id:\s*$/o) {
            # putting the colon at the left of the first non-blank
            # character improves readability in debug mode
            $line =~ s/^/;/;
        }
        $prev_line = $orig_line;
    }

    # Close dangling blocks
    my $close;
    while (@stack) {
        my $prev_word = $stack[-1]{prev_word};
        pop @stack;
        my $prev_indent = @stack ? $stack[-1]{indent} : 0;
        my $spc = ' ' x $prev_indent;
        $close .= "$spc}";
        $close .= ";" if defined $prev_word && $prev_word =~ /^(?:sub|do|eval)$/;
        $close .= "\n";
    }
    push @$lines, $close if defined $close;
}


# See semicolonize_and_bracketize().
#
# This has finally become a bit dirty. Both subroutines should be
# refactored.
sub move_semicolons {
    my $lines = shift;

    my $current_line_ref;
    my $i;
    for ($i = 0; $i < @$lines; ++$i) {
        next if $lines->[$i] =~ /^[\s$;]*$/; # skip blank lines
        $current_line_ref = \$lines->[$i];
        last;
    }

    for (++$i; $i < @$lines; ++$i) {
        my $next_line_ref = \$lines->[$i];
        next if $$next_line_ref =~ /^[\s$;]*$/; # skip blank lines
        $$current_line_ref .= ";" if $$next_line_ref =~ s/^;//;
        # this fixes do {}; modifier -> do {} modifier
        # note that @$lines does no correspond with physical lines
        $$current_line_ref =~ s/};(?=\s*(?:if|unless|while|until|for(?:each)?)\b.*;$)/}/;
        $current_line_ref = $next_line_ref;
    }
}


# Removes comments and any whitespace to their left.  This subroutine
# has to be called after the one that removes continuation lines,
sub remove_comments {
    s/[ \t]*(?<!\$)#.*//g;
}


# Python forgives continuation lines in list constructors and friends.
sub join_lines_ending_in_commas {
    s/,\n\s*/,/g;
    s/=>\n\s*/=>/g;
}

1;

__END__

=head1 NAME

Acme::Pythonic - Python whitespace conventions for Perl

=head1 SYNOPSIS

 use Acme::Pythonic; # this semicolon yet needed

 for my $n in 1..1000:
     while $n != 1:
         if $n % 2:
             $n = 3*$n + 1
         else:
             $n /= 2

=head1 DESCRIPTION

Acme::Pythonic is a source filter that brings Python whitespace
conventions to Perl.

This module is thought for those who embrace contradictions. It is an
aid for those who are in an intermediate level, walking the Whitespace
Matters Way in their pursuit of highest realization, only attained with
L<SuperPython>.

=head2 Labels

Labels are supported. This is the Acme::Pythonic version of the example
in perlsyn:

    OUTER: for my $wid in @ary1:
        INNER: for my $jet in @ary2:
            next OUTER if $wid > $jet
            $wid += $jet

Labeled blocks work as well:

    my $k = 7
    FOO:
        --$k
        last FOO if $k < 0
        redo FOO

Note that if we put a label in the line before in a control structure
indentation matters. This would be a non-equivalent reformat of the
example above:

    OUTER:
        for my $wid in @ary1:               # NOT WHAT WE WANT
            INNER:
            for my $jet in @ary2:           # GOOD, ALIGNED
                next OUTER if $wid > $jet
                $wid += $jet

Since the first C<for> is indented with respect to C<OUTER:> we get a
labeled block containing a C<for> loop, instead of a labeled C<for>.

Labels can be composed just of upper-case letters. That was decided so
that list operators can be chained:

    my @st = map:
                 $_->[0]
             sort:
                 $a->[1] <=> $b->[1]
             map:
                 [$_, $foo{$_}]
             keys %foo

and &-prototyped subroutines can be used like this:

    use Error

    # ...
    try:
        $server->send_data($data)
    catch Error::IO with:
        my $E = shift
        # handle the exception

=head2 C<do/while>-like constructs

Acme::Pythonic tries to detect loop modifiers after a do BLOCK. Thus

    do:
        do_something()
        do_something_else()
    while $condition

is seen as a do/while, whereas

    do:
        do_something()
        do_something_else()
    while $condition:
        handle_some_stuff()

is not.

=head2 New Keywords

=head3 pass

To be able to have an empty block we provide C<pass>:

    sub abstract_method:
        pass

=head3 in

This works:

    foreach my $foo @array:
        do_something_with $foo

However C<in> is supported in case you find the following more readable

    foreach my $foo in @array:
        do_something_with $foo

This keyword can be used if there's no variable to its left too, which
means we are dealing with C<$_> as usual:

    foreach in @array:
        s/foo/bar/

but can't be used when the loop acts as a modifier:

    print foreach in @array # ERROR

=head2 Continuation lines

As in Python, you can break a logical line in several physical lines
using a backslash at the end:

    my $total = total_products() + \
                total_delivery() + \
                total_taxes()

and in that case the indentation of those additional lines is irrelevant.

=head2 Ending commas

If a line ends in a comma or arrow (C<< => >>) it is joined with the
following. This way we support

    my %authors = (Perl   => "Larry Wall",
                   Python => "Guido van Rossum")

which works in Python as well.

=head1 LIMITATIONS

Keywords followed by code in the same line are not supported. This would
be valid in Python:

    if $n % 2: $n = 3*$n + 1
    else: $n /= 2

but it does not work in Acme::Pythonic.

=head1 DEBUG

You can pass a C<debug> flag to Acme::Pythonic like this:

    use Acme::Pythonic debug => 1;

In debug mode the module prints to standard output the code it has
generated and substitutes everything with a dummy C<1;>, so nothing gets
executed. This way the resulting source can be inspected.

This happens I<before> L<Filter::Simple> undoes the blanking out of
PODs, strings, and regexps. Thus, those parts will be seen as C<$;>s in
a row (C<$;> is C<\034> by default.)

=head1 BUGS

This module uses a regexp approach and the superb help of
Filter::Simple. The regexp part of this means it is broken from the
start, though I've tried hard to make it as robust as I could. Bug
reports will be very welcome, just drop me a line!

=head1 THANKS

Damian Conway gave his full blessing if I wanted to write a module like
this based on his unpublished Language::Pythonesque. The code that
handles indentation is inspired by his.

Also, Dr. Conway is the author of L<Filter::Simple>, which aids a lot
blanking out PODs, strings, etc. so you can munge the source with
certain confidence. Without Filter::Simple this module would be
infinitely more broken.

=head1 SEE ALSO

L<perlfilter>, L<Filter::Simple>, L<SuperPython>.

=head1 AUTHOR

Xavier Noria (FXN), E<lt>fxn@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2004 by Xavier Noria

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=cut
