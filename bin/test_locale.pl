#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use POSIX qw(locale_h);
use locale;

# query and save the old locale
my $old_locale = setlocale(LC_CTYPE);

setlocale(LC_CTYPE, "POSIX");

# Get a reference to a hash of locale-dependent info
my $locale_values = localeconv();
# Output sorted list of the values
for my $key (sort keys %$locale_values) {
  printf "key is $key " . $locale_values->{$key} . "\n";
}
