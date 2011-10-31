#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

my $sep = " xa--z ";
my $sep2 = "qsq aflo3 abxze ";
MAIN: {

  my (%Done, $fun);
  my @files;
  my ($file, $text, @lines, $line, $sub_text, @subs_text, %subs, %subs_sort);
  my (%subs_inv_sort_hash, $done, %Almost_done);
  my ($sub_name, $arg, %sub_args, $args_list, $repl_text, $new_line, $write_flag);

  # function calls we dealt with already
  open(FILE, "<Done.txt"); $done = <FILE>; close(FILE);
  open(FILE, "<System_done.txt"); $done = $done . "\n" . <FILE>; close(FILE);
  foreach $fun (split ("\n", $done)){
    next if ($fun =~ /^\s*$/);
    $fun =~ s/^(.*?)\s*\!.*?$/$1/g;
    $Done{lc($fun)} = 1;
    #print "$fun\n";
  }

  # Look into a set of files where the functions we need are defined
  #@files = <../../../libftutil/src/*.f90>;
  #@files = (<../*.f90>);
  @files = (<*.f90>);
  #@files = ("delme.f90");
  &extract_arg_info(\@files, \%subs, \%subs_sort, \%subs_inv_sort_hash, \%sub_args);

  # files in which to use the gained info to do replacements
  @files = <*.f90>;
  #@files=("testJac.f90");
  foreach $file (@files){

    print "Will examine $file\n";
    
    open(FILE, "<$file"); $text = <FILE>; close(FILE);
    
    $repl_text = "";
    $write_flag = 0;
    
    @lines = split ("\n", $text);
    &extract_multiline_subdefs(\@lines, \%sub_args);

    foreach $line (@lines){

      # rm the line below if &extract_multiline_subdefs is removed!!!
      if ($line =~ /\bcall\b.*?\&/i){
        $line =~ s/\!.*?\n/\n/g; $line =~ s/\!.*?$//g;
        $line =~ s/\&.*?\n//g;
      }
      
      $new_line = &fix_line($file, $line, \%subs, \%subs_sort, \%subs_inv_sort_hash,
                            \%sub_args, \%Done, \%Almost_done);
      

      if ($new_line ne $sep2){
        $repl_text = $repl_text . $new_line . "\n";
      }

      $write_flag = 1 if ($new_line ne $line);
      $write_flag = 0; # never write!!!, until the &extract_multiline_subdefs is rmvd!
    }

    if ($write_flag == 1){
      print "Writing to $file\n";
      open(FILE, ">$file");
      print FILE "$repl_text";
      close(FILE);
    }
    
  }

  open(FILE, ">Done.txt");
  foreach $line (keys %Done){
    print FILE "$line\n";
  }
  foreach $line (keys %Almost_done){
    print FILE "$line\n";
  }
  close(FILE);
  
}

sub extract_multiline_subdefs{

  my ($lines, $line, $add_to, $do_add, $counter, $nlines, $cur_line, $sub_args);
  my ($sub_name, $sub_name_lc, $print_position);
  
  $lines = shift; $sub_args = shift;

  $nlines = scalar (@$lines);

  $cur_line = 0;
  $do_add = 0;
  $add_to = 0;
  $print_position = 0;
  while ($cur_line < $nlines){

    $line = $lines->[$cur_line];

    if ($do_add == 0 && $line !~ /\bcall\b.*?\&/i){
      $cur_line++;
      next;
      
    }elsif ($line =~ /\bcall\b.*?\&/i){
      $do_add = 1;
      $add_to = $cur_line;
      $cur_line++;
      next;
      
    }else{
      if ($do_add != 1){
        print "Monster error!!!\n";
        exit(0);
      }

      if ($lines->[$cur_line] !~ /\&/) { # ampersand must be before ! for this to work
        $do_add = 0;
      }
      
      $lines->[$add_to] .= "\n" . $lines->[$cur_line];
      $lines->[$cur_line] = $sep2;
      
      $cur_line++;
    
      
      if ($do_add == 0){

        if ( $lines->[$add_to] !~ /\bcall\b\s+([_\w]+)\s*\(/i ){
          print "Error! Can't find sub name!!!\n";
          exit(0);
        }

        $sub_name = $1; $sub_name_lc = lc($sub_name);

        $print_position++;
        if (!exists $sub_args->{$sub_name_lc} ){
          #print "Can't find function $sub_name!!!\n";
          
        }else{
          next; 
          print "\n---Position: $print_position----------\n";
          print "Original: " . $sub_name . '(' . $sub_args->{$sub_name_lc} . ")\n";
          print "\nCurrent:\n" . "$lines->[$add_to]\n-------\n\n";
        }
      }
    }
    
  }
}

sub extract_arg_info {

  my ($file, $text, @lines, $line, $sub_text, @subs_text);
  my ($sub_name, $arg, $args_list, $repl_text);
  my ($files, $subs, $subs_sort, $subs_inv_sort_hash, $sub_args);
  my @interfaces;
  
  $files = shift;
  $subs=shift; $subs_sort=shift; $subs_inv_sort_hash=shift; $sub_args=shift;

  foreach $file (@$files){

    open(FILE, "<$file"); $text = <FILE>; close(FILE);

    # debug: look at these lines, especially the interface!!!
    $text =~ s/\!.*?\n/\n/g;
    $text =~ s/\&[ \t\r]*\n//g;

    # Delete any interface text, it just confuses the script.
    #@interfaces = ($text =~ /interface\b(.*?)end\s+interface/sig);
    $text =~ s/\binterface\b(.*?)\bend\s+interface\b//sig;

    @subs_text = ($text =~ /(subroutine\s+.*?\s*\(.*?end\s+subroutine)/sig);
    #@subs_text = (@subs_text, @interfaces);
    
    foreach $sub_text (@subs_text){
      &parse_subs($sub_text, $subs, $subs_sort, $subs_inv_sort_hash);
    }
  }

  foreach $sub_name (keys %$subs){

    $args_list = "";
    foreach $arg (sort { $subs_sort->{$sub_name}->{$a} <=> $subs_sort->{$sub_name}->{$b} }
                  keys %{$subs->{$sub_name}} ){
      
      $args_list = $args_list . "$arg$subs->{$sub_name}->{$arg}, ";
    }

    if (exists $sub_args->{$sub_name}){
      print "Error! Duplicated $sub_name\n";
      exit(0);
    }
    
    $args_list =~ s/,\s*$//g;
    $sub_args->{$sub_name} = $args_list;

    #print "$sub_name\($args_list\)\n\n";
  }
}

sub fix_line {

  my ($file, $line, $line_new, $before, $sub_name, $tmp, $arg, $args, $args_new, $after);
  my ($ps, $pe, $arg_name, $arg_params, $subs, $subs_sort, $subs_inv_sort_hash, $sub_args);
  my ($count, $sub_name_lc, $arg_def_name, $arg_val_name, $action, $change_flag, $Done);
  my ($Almost_done);
  
  $file = shift; $line = shift;
  $subs=shift; $subs_sort=shift; $subs_inv_sort_hash=shift; $sub_args=shift;
  $Done = shift;
  $Almost_done = shift;
  
  return $line if ($line =~ /^\s*\!/);
  return $line if ($line =~ /\&/); # this must be fixed!
  return $line unless ($line =~ /^(.*?\bcall\s+)([_\w]+)(\s*\(.*?)$/i);

  $before   = $1;
  $sub_name = $2;
  $tmp      = $3;

  $sub_name_lc = lc ($sub_name);


  if (exists $Done->{$sub_name_lc}){
    return $line;
  }

  if (!exists $subs->{$sub_name_lc}){
    print "$sub_name not existent!\n";
    return $line;
  }

  
  if ($tmp =~ /^(.*?)(\s*[;!].*?)$/){
    $args = $1; $after = $2;
  }else{
    $args = $tmp; $after = ""; 
  }

  if ($args !~ /^(\s*\(\s*)(.*?)(\s*\)\s*)$/){
    print "Error in $args!\n";
    exit(0);
  }
  $ps = $1; $args = $2; $pe = $3;


  $args =~ s/(\(.*?\))/&mmenc($1)/eg;
  
  my @args_array = split (",", $args);

  $args_new = "";
  $count = -1;

  $action = 0;
  $change_flag = 0;

  foreach $arg (@args_array){

    $arg =~ s/$sep/,/g;
    $count++;
    
    #print "$arg ---";
    
    if ($arg =~ /^\s*$/ || $arg =~ /\'/){
      $args_new = $args_new . $arg . ",";
      next;
    }

    #if (!exists $subs_inv_sort_hash->{$sub_name_lc}->{$count}){
      #print "Error! $subs_inv_sort_hash of $sub_name_lc $count does no exist!\n";
    #}
    
    $arg_def_name = $subs_inv_sort_hash->{$sub_name_lc}->{$count};
    $arg_val_name = $subs->{$sub_name_lc}->{$arg_def_name};

    if (!exists $subs->{$sub_name_lc}->{$arg_def_name}){
      print "Error at $subs $sub_name_lc at $arg_def_name\n";
      print "Info: subroutine: $sub_name_lc\n";
      print "Info: line: $line\n";
      print "File: $file\n";
      exit(0);
    }

    $arg =~ s/^\s*//g;
    my $leftp; # worry about arguments of the form gModel(modelnum)%mu(jj)
    if ($arg =~ /^(.*?\%)(.*?)$/){
      $leftp=$1; $arg=$2;
      print "--------------->$leftp$arg\n";
    }else{
      $leftp="";
    }
    if ($arg =~ /^([_\w]+)(\(.*?)$/){
      $arg_name = $1; $arg_params = $2;
    }else{
      $arg_name = $arg; $arg_params = "";
    }
    $arg      = $leftp . $arg;
    $arg_name = $leftp . $arg_name;
    
    my $repl_val;
    if ($arg_val_name =~ /\(/ && $arg_params !~ /\(/){
      $action = 1;

      if ($arg_val_name =~ /,.*?,/){
        $repl_val = "(1,1,1)";
      }elsif ($arg_val_name =~ /,/){
        $repl_val = "(1,1)";
      }else{
        $repl_val="(1)";
          
      }
      print "$file: Original: " . $sub_name . '(' . $sub_args->{$sub_name_lc} . ")\n";
      print "$file: Current:  " . $sub_name . '(' . $args_new . $arg . "," . "\n";
      print "$file: Suggest:  " . $sub_name . '(' . $args_new . $arg . "$repl_val," . "\n";

      $/ = "\n";
      print "Accept? ";
      my $answer = <>;
      if ($answer =~ /y/i){
        $arg = $arg . "$repl_val";
        $change_flag = 1;
        print "Accepted!\n\n";
      }
      undef $/;
    }
    #print "$arg_name -- $arg_params -- $arg_def_name -- $arg_val_name\n";
    
    $args_new = $args_new . $arg . ",";
  }
  $args_new =~ s/,$//g;


  $line_new = $before . $sub_name . $ps . $args_new . $pe . $after;
  if ($line ne $line_new && $change_flag == 1){
    print "Before:\'$line\'\n";
    print "After: \'$line_new\'\n";
    print "Next? ";
    $/ = "\n";
    my $answer = <>;
    undef $/;
    print "\n";


  }

  #print "$line\n";
  $Almost_done->{$sub_name_lc} = 1;
  return $line_new;
}

sub parse_subs {

  my ($sub_text, $name, $arg_list, $body, $arg, @args_array, $line, @lines, %args, $list);
  my ($arg_name, $arg_params, $dummy, %args_sort, $count);
  my($subs_hash, $subs_sort_hash, $subs_inv_sort_hash);
  
  $sub_text = shift; $subs_hash = shift; $subs_sort_hash = shift;
  $subs_inv_sort_hash = shift; 
  

  if ($sub_text =~ /subroutine\s+(.*?)\s*\((.*?)\)(.*?)$/si){
    $name = lc($1);
    $arg_list = $2;
    $body = $3;
  }else{
    print "Bad sub!!! $sub_text\n";
    exit(0);
  }

  $dummy = " dummy_oleg ";
  $count = 0;
  @args_array = split (",", $arg_list);
  foreach $arg (@args_array){
    next if ($arg =~ /^\s*$/);
    $arg =~ s/^\s*(.*?)\s*$/$1/g;

    $args{$arg} = $dummy;
    
    $args_sort{$arg} = $count;
    $count++;
    #  print "$arg, ";
  }

  #print "$name ^^ ";
  @lines = split ("\n", $body);
  foreach $line (@lines){
    next unless ($line =~ /intent\s*\([\s\w]*\).*?::[\s,]*(.*?)$/i);
    $list = $1;
    my $list_new = $list;
    $list_new =~ s/(\(.*?\))/&mmenc($1)/eg; # to make split(",") below work well
    # print "$list_new\n";

    @args_array = split (",", $list_new);
    foreach $arg (@args_array){
      next if ($arg =~ /^\s*$/);
      $arg =~ s/$sep/,/g;
      $arg =~ s/\s*//g;

      if ($arg =~ /^\s*([_\w]+)\s*(\(.*\))[;\s*]*$/){ # here must have matching parens!
        $arg_name = $1; $arg_params = $2;
      }elsif ($arg =~ /^\s*([_\w]+)\s*$/){
        $arg_name = $arg; $arg_params = ""; 
      }else{
        print "Error in $arg!!!\n";
        exit(0);
      }

      {
        my @opens = ($arg_params =~ /(\()/g); my $nopens = $#opens;
        my @closd = ($arg_params =~ /(\))/g); my $nclosd = $#closd;

        if ($nopens != $nclosd){
          print "----Error in $arg_name$arg_params, originally: $list!\n";
          #exit(0);
        }
      }
      
      if (! exists $args{$arg_name}){
        print "Error! $arg_name does not exist in $name!!!\n";
        exit(0);
      }
      $args{$arg_name} = $arg_params;
      #print "$arg_name -- $arg_params ++ ";
    }
  }
  
  # foreach $arg (keys %args){
  #   if ($args{$arg} eq $dummy){
  #     print "Error with $arg!\n";
  #   }
  # }

  if (exists $subs_hash->{$name}){
    print "$name is duplicated!\n";
    exit(0);
  }


  #print "$name : ";
  foreach $arg ( sort { $args_sort{$a} <=> $args_sort{$b} } keys %args_sort){

    $subs_hash->{$name}->{$arg}      = $args{$arg};
    $subs_sort_hash->{$name}->{$arg} = $args_sort{$arg};
    $subs_inv_sort_hash->{$name}->{$args_sort{$arg}} = $arg;
    #print "$arg$args{$arg}, ";
  }

#  print "\n";
#  print " END\n";
}

sub mmenc {

  my ($text) = @_;
  $text =~ s/,/$sep/g;
  return $text;
}
