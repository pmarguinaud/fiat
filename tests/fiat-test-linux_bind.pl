#!/usr/bin/perl -w

use FileHandle;
use POSIX qw (ceil floor);
use Data::Dumper;

use strict;

sub bind : method
{
  use Cwd;
  use File::Spec;

  my ($class, %opts) = @_;

  my @line = $class->bind_node (%opts);

  return unless (@line);

  my $cpuinfo  = $opts{cpuinfo};

  my $nthreads = $cpuinfo->{ncpus} * $cpuinfo->{nthreadpercore};

  my $fh = 'FileHandle'->new (($opts{append} ? '>>' : '>') . 'linux_bind.txt');

  $ENV{EC_LINUX_BIND} = 'File::Spec'->join (&Cwd::cwd (), 'linux_bind.txt');

  if ($opts{distribution} eq 'cyclic')
    {

      my $rank = 1;
      for my $task (1 .. $opts{tasks})
        {
          for my $node (1 .. $opts{nodes})
            {
              my $line = $line[$task-1];
              $fh->print ($line);
              $rank++;
            }
        }

    }
  else
    {

      my $rank = 1;
      for my $node (1 .. $opts{nodes})
        {
          for my $task (1 .. $opts{tasks})
            {
              my $line = $line[$task-1];
              $fh->print ($line);
              $rank++;
            }
        }

    }
  
  
  $fh->close ();

}

# Returns hyperthreading level

sub ht_level
{
  my ($class, %opts) = @_;
  my $cpuinfo = $class->cpuinfo (%opts);
  
  my $level = ($opts{tasks} * $opts{openmp}) / $cpuinfo->{ncpus};

  return $level
    if (int ($level) == $level);

  return 0;
}

sub bind_node
{
  my ($class, %opts) = @_;

  my $cpuinfo = $opts{cpuinfo};

  die ("Could not retrieve cpuinfo\n")
    unless ($cpuinfo);


  my $cpuspertask  = $cpuinfo->{ncpus} / $opts{tasks};
  my $nthreads     = $cpuinfo->{ncpus} * $cpuinfo->{nthreadpercore};

  if (($cpuspertask * $cpuinfo->{nthreadpercore} == 1) && ($opts{openmp} == 1))
    {
      my @line;
      for my $task (1 .. $opts{tasks})
        {
          my @x = ('0') x ($cpuinfo->{ncpus} * $cpuinfo->{nthreadpercore});
          $x[$task-1] = '1';
          push @line, join ('', @x) . "\n";
        }
      return @line;
    }


  if ($cpuspertask * $cpuinfo->{nthreadpercore} < $opts{openmp})
    {
      return $class->nobind (%opts, cpuinfo => $cpuinfo);
    }

  my @line;

  my @w = (0) x $nthreads;

  for my $task (1 .. $opts{tasks})
    {
      my @thr = map { 
                      my $htoff = ($_-1) * ($cpuinfo->{ncpus});
                      ($htoff + &floor (($task-1) * $cpuspertask) .. $htoff + &ceil ($task * $cpuspertask - 1))
                    } (1 .. $cpuinfo->{nthreadpercore});

      my $ithr = 0;
      push @line, join (':', map 
                               {
                                 my $openmp = $_;
                                 my @x = ('0') x $nthreads;


                                 while ($w[$thr[$ithr]] > 0)
                                   {
                                     die if ($ithr > $#thr);
                                     die if ($thr[$ithr] > $#w);
                                     $ithr++;
                                   }

                                 my $thr = $thr[$ithr];
                                 $x[$thr] = '1';
                                 $w[$thr]++;

                                 join ('', @x)
                               }
                             (0 .. $opts{openmp}-1)) . "\n";
    }

  return @line;
}


my $ncpus = shift;

my $cpuinfo = 
{
  nthreadpercore =>      1,
  ncpus          => $ncpus,
};


'main'->bind (distribution => '', cpuinfo => $cpuinfo, @ARGV);

