#!/usr/bin/perl
use strict;                # 'strict' insists that all variables be declared
use diagnostics;           # 'diagnostics' expands the cryptic warnings

my ($pos, $spos, $epos, $beg, $newright, $string, $sbeg, $ebeg, $i);
$string=join "", <>;
$spos=7;
my @chars=split('', $string);
my %lpairs=("{"=>"}", "["=>"]", "("=>")", "|"=>"|");
my %pairs=("{"=>"}",  "["=>"]", "("=>")", "|"=>"|", "}"=>"{", "]"=>"[", ")"=>"(");

match_par(\$spos, \$epos, \@chars, \%lpairs); # pass data by reference
$pos=$spos; find_token (\$pos, \$beg, \@chars, \%pairs); $sbeg=$beg;
$pos=$epos; find_token (\$pos, \$beg, \@chars, \%pairs); $ebeg=$beg;
$newright= join "", @chars[$sbeg..($spos-1)];
$newright=$newright . $chars[$epos];
$newright =~ s/left/right/g;
for ($i=$ebeg ; $i < $epos ; $i++){
   $chars[$i]="";
  }
$chars[$epos]=$newright;
$string=join "", @chars;
print "$string";

# -- functions ---------------------------------------------
sub find_token {  # if given (, look to the left of it, to see if it has the form \big( or so.
  my ($ppos, $pbeg, $pchars, $ppairs, $npos)=@_;
  my ($i, $ch);
  if (!exists $$ppairs{$$pchars[$$ppos]}){
    print "Not a paranthesis\n";
    exit(0);
  }
  if ($$ppos==0 ||  $$pchars[$$ppos-1] !~ /[a-zA-Z\\]/ ){ # no chance, stop here
    $$pbeg=$$ppos;
  }elsif ($ppos==1){ # the special case when we are almost at the beg of string
    if ($$pchars[$$ppos-1] eq "\\" ){
      $$pbeg=$$ppos-1; # this is acceptable
    }else{
      $$pbeg=$$ppos; # this is not
    }
  }else{
    if ($$pchars[$$ppos-1] eq "\\"){ # a special exception for the cases \left\{, \}, \[, \(, etc
      $npos=$$ppos-1;
    }else{
     $npos=$$ppos; 
    }
    for ($i=$npos-1 ; $i >= 0  ; $i--){
      $ch=$$pchars[$i];
      if ($ch !~ /[a-zA-Z\\]/){
	$$pbeg=$npos;
	last;
      }elsif ($ch eq "\\"){
	$$pbeg=$i;
	last;
      }
    }
  }
}


sub match_par {
  my ($pspos, $pepos, $pchars, $plpairs)=@_;
  my ($i, $ch, $lpar, $rpar, $count);
  $lpar=$$pchars[$$pspos];

  if (! exists $$plpairs{$lpar} ){
    print "Not a left paranthesis!\n";
    exit(0);
  }else{
    $rpar=$$plpairs{$lpar};
  }

  $count=1;
  for ($i=$$pspos+1 ; $i <= $#$pchars ; $i++) {
    $ch=$$pchars[$i];
    if ($ch eq $rpar) {
      $count--;
    }elsif ($ch eq $lpar){
      $count++;
    }
    if ($count == 0){
      $$pepos=$i;
      last;
    }
  }
}
