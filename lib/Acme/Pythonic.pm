package Acme::Pythonic;

# Please, if you tested it in some earlier version of Perl and works let
# me know! The versions of Filter::Simple, Text::Tabs, and Test::More
# would be useful as well.
use 5.008002;
use strict;
use warnings;

use vars qw($VERSION $DEBUG %SM);
$VERSION = '0.13';

use Text::Tabs;

sub import {
    my ($package, %cfg) = @_;
    $DEBUG = $cfg{debug};
    # Undocumented, just an idea I'm exploring.
    if (exists $cfg{'(&)'}) {
        $cfg{'(&)'} = [$cfg{'(&)'}] unless ref $cfg{'(&)'} eq 'ARRAY';
    } else {
        $cfg{'(&)'} = [];
    }
    %SM = map { $_ => 1 } ('do', 'sub', 'eval', @{$cfg{'(&)'}});
}


use Filter::Simple;
FILTER_ONLY code => sub {
    normalize_newlines();

    my $lines = [ split /\n/ ];
    uncolonize_and_parenthesize($lines);
    semicolonize_and_bracketize($lines);

    $_ = join "\n", @$lines;
    fix_modifiers();
    fix_dangling_else_and_friends();

    if ($DEBUG) {
        print "$_\n";
        $_ = "1;\n";
    }
};


# This regexp matches a 7-bit ASCII identifier. We use atomic grouping
# because an identifier cannot be backtracked.
my $id = qr/(?>[_a-zA-Z](?:[_a-zA-Z0-9']|::)*)/;

# Shorthand to add a possible trailing comment to some regexps.
my $tc = qr/(?<!\$)#.*/;


# In the trials I've done seems like the Python interpreter understands
# any of the three conventions, even if they are not the ones in the
# platform, and even if they are mixed in the same file.
sub normalize_newlines {
    s/\015\012/\n/g;
    tr/\015/\n/;
    tr/\012/\n/;
}


# Tries its best at removing trailing Pythonic colons, and putting
# Perlish parens around EXPRs, LISTs, etc.
#
# Modifies @$lines in place.
sub uncolonize_and_parenthesize {
    my $lines = shift;

    foreach (@$lines) {
        # We check the parity of the number of ending colons to try to
        # avoid breaking
        #
        #    print for keys %main::
        #
        next unless /(:+) (\s* $tc?) $/ox && length($1) % 2;
        next if /^\s*#/; # skip comments

        if (/^\s*($id)\s*:\s*$/o) {
            # Labels cannot have lower-case letters in Acme::Pythonic.
            s/:\s*$// if $1 =~ /[[:lower:]]/;
            next;
        }

        # We can safely remove the ending colon now.
        s/: (\s* $tc?) $/$1/ox;

        # Subroutine definitions are ok this way.
        next if /^\s*sub\b/;

        # These need parens after the keyword.
        next if s/^(\s* \b(?:if|elsif|unless)\b \s*) (.*?) (\s* $tc?) $/$1($2)$3/x;

        # These may be preceded by a label.
        next if s/^(\s* (?:$id\s*:)? \s* \b(?:while|until)\b \s*) (.*) (\s* $tc?) $/$1($2)$3/ox;

        # Try your best with fors.
        next if s/^(\s* (?:$id\s*:\s*)? \bfor(?:each)?\b \s*) (.*) (\s* $tc?) $/fortype_guesser($1,$2).$3/oxe;
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
    if ($rest =~ m/^((?:my|our)? \s* \$ $id\s+) in\s* ((?: (?:[\$\@%&\\]) | (?:\b\w) ) .*)$/ox ||
        $rest =~ m/^((?:my|our)? \s* \$ $id\s*)       ((?: (?:[\$\@%&\\]) | (?:\b\w) ) .*)$/ox) {
        $guess = "$for$1($2)";
    } else {
        # We are not sure whether this is a for or a foreach, but it is
        # very likely that putting parens around gets it right.
        $rest =~ s/^\s*in\b//; # fixes "foreach in LIST"
        $guess = "$for($rest)";
    }

    return $guess;
}


# Tries its best at putting semicolons and figuring out blocks.
# Language::Pythonesque was the basis of this subroutine.
#
# Modifies @$lines in place.
sub semicolonize_and_bracketize {
    my $lines = shift;
    return unless @$lines;

    my $prev_indent;
    my $prev_line;
    my @stack = ();

    # If unsure about the ending indentation level, add an extra
    # non-indented line to ensure the stack gets emptied.
    push @$lines, '1; # added by Acme::Pythonic' if $lines->[-1] =~ /^(?:\s|\s*#)/;
    foreach my $line (@$lines) {
        next unless $line =~ /\S/; # skip blank lines
        next if $line =~ /^\s*#/;  # skip comments

        if (defined $prev_line &&
            (($$prev_line =~ m'(?:,|=>)\s*(?<!$)#' || $$prev_line =~ m'(?:,|=>)\s*$') ||
             ($$prev_line !~ m'(?<!$)#' && $$prev_line =~ s/\\$//))) {
            # This is done this way because comments are allowed in implicit line joining, see
            # http://www.python.org/doc/2.3.3/ref/implicit-joining.html
            $prev_line = \$line;
            next;
        }

        my ($indent) = $line =~ /^(\s*)/;
        $indent = length(expand($indent));
        my $current_indent = @stack ? $stack[-1]{indent} : 0;
        if ($current_indent < $indent) {
            my ($label) = $$prev_line =~ /(?<![\$\@%&])($id)\s*$/;
            $line =~ s/^\s*pass\s*$//;
            push @stack, {indent => $indent, label => $label};
            $$prev_line .= " {" unless $$prev_line =~ s/(?=\s*(?<!\$)#)/ {/;
        } elsif ($current_indent > $indent) {
            my $close = '';
            while ($current_indent > $indent) {
                my $label = $stack[-1]{label};
                pop @stack;
                $current_indent = @stack ? $stack[-1]{indent} : 0;
                $close .= "\n" . (' ' x $current_indent) . "}";
                $close .= ";" if defined $label && exists $SM{$label};
            }
            $$prev_line .= $close;
        } elsif (defined $prev_line && $$prev_line !~ /$id:\s*$/o) {
            # Put a semicolon at the right of the previous line but be
            # careful with comments.
            $$prev_line .= ";" unless $$prev_line =~ s/(?=\s*(?<!\$)#)/;/;
        }
        $prev_line = \$line;
        $prev_indent = $indent;
    }
}


# Removes the semicolon and newline in do {}; if EXPR; and friends.
sub fix_modifiers {
    my $comments = qr'(?:(?<!$)#.*\n\s*)*';
    s/};\s*(?=$comments(?:if|unless|while|until|for(?:each)?)\b.*;$)/} /mog;
}


# We follow perlstyle here, as we did until now.
sub fix_dangling_else_and_friends {
    s/(?<=})\s*(?=elsif|else|continue)/ /g;
}

1;

__END__

=head1 NAME

Acme::Pythonic - Python whitespace conventions for Perl

=head1 SYNOPSIS

 use Acme::Pythonic; # this semicolon yet needed

 sub delete_edges:
     my $G = shift
     while my ($u, $v) = splice(@_, 0, 2):
         if defined $v:
             $G->delete_edge($u, $v)
         else:
             my @e = $G->edges($u)
             while ($u, $v) = splice(@e, 0, 2):
                 $G->delete_edge($u, $v)


=head1 DESCRIPTION

Acme::Pythonic is a source filter that brings Python whitespace
conventions to Perl.

This module is thought for those who embrace contradictions. A humble
aid for walkers of the Whitespace Matters Way in their pursuit of
highest realization, only attained with L<SuperPython>.

=head1 DETAILS

=head2 Labels

This is the Acme::Pythonic version of the example in L<perlsyn>:

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

and C<&>-prototyped subroutines can be used like this:

    sub mygrep (&@):
        my $code = shift
        my @result
        foreach @_:
            push @result, $_ if &$code
        return @result

    @array = mygrep:
        my $aux = $_
        $aux *= 3
        $aux += 1
        $aux % 2
    reverse 0..5

Nevertheless support for this has to be improved, see L</LIMITATIONS>.

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

A backslash in a line with a comment won't be removed though:

    my $foo = 1 + \  # comment (ERROR)

In Python that's a syntax error.

=head2 Ending commas

If a line ends in a comma or arrow (C<< => >>) it is conceptually joined
with the following:

    my %authors = (Perl   => "Larry Wall",
                   Python => "Guido van Rossum")

As in Python, comments can be intermixed there:

    my %hello = (Catalan => 'Hola',  # my mother tongue
                 English => 'Hello)

=head1 LIMITATIONS

Keywords followed by code in the same line are C<not> supported. This would
be valid in Python:

    if $n % 2: $n = 3*$n + 1
    else: $n /= 2

but it does not work in Acme::Pythonic. The reason for this is that it
would be hard to identify the colon that closes the expression without
parsing Perl, consider for instance:

    if keys %foo::bar ? keys %main:: : keys %foo::: print "foo\n"

On the other hand, to use a subroutine with prototype C<&> you need to
add the trailing semicolon in its own line by now:

    sub foo (&):
        pass

    foo:
        pass
    ;

That's a pity, improving this is in the TODO list.

=head1 DEBUG

You can pass a C<debug> flag to Acme::Pythonic like this:

    use Acme::Pythonic debug => 1;

In debug mode the module prints to standard output the code it has
generated and substitutes everything with a dummy C<1;>, so nothing gets
executed. This way the resulting source can be inspected.

The module tries to generate human readable code following L<perlstyle>.
Blank lines and comments are preserved.

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
