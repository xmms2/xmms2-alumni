package Test::XMMS;

use strict;
use warnings;
use Carp qw/croak/;
use POSIX qw/SIGINT SIG_BLOCK SIG_UNBLOCK/;
use Audio::XMMSClient;
use File::Path qw/mkpath/;
use File::Basename qw/basename/;
use File::Spec::Functions qw/splitpath splitdir catdir catfile/;

my $xmms_client;
my $xmms_pid;

sub do_fork {
    my $sigset = POSIX::SigSet->new(SIGINT);
    POSIX::sigprocmask(SIG_BLOCK, $sigset)
        or croak("Can't block SIGINT for fork: `$!'");

    my $pid = fork();

    if (!defined $pid) {
        croak("Couldn't fork: `$!'");
    }

    $SIG{INT} = 'DEFAULT';

    POSIX::sigprocmask(SIG_UNBLOCK, $sigset)
        or croak("Can't unblock SIGINT for fork: `$!'");

    return $pid;
}

sub create_goof_plugin_dir {
    my ($from, $to) = @_;

    $to = catfile($to, 'plugins');
    mkpath([$to]);

    for my $plugin (glob $from . "/*/libxmms_*.so") {
        symlink $plugin, catfile($to, basename($plugin));
    }

    return $to;
}

sub import {
    my ($self, @args) = @_;

    my $top_dir = '/home/rafl/projects/c/xmms2/xmms2-rafl/'; #FIXME
    my $build_dir = catfile($top_dir, qw/_build_ default/);
    my $exec_path = catfile($build_dir, qw/src xmms xmms2d/);
    my $goof_dir = catfile($top_dir, qw/t goofy/);
    my $plugin_path;
    my @extra_args;

    while (defined (my $arg = shift @args)) {
        if ($arg eq '-nostart') {
            return;
        }
        elsif ($arg eq '-exec') {
            $exec_path = shift @args;
        }
        elsif ($arg eq '-plugin_path') {
            $plugin_path = shift @args;
        }
        else {
            push @extra_args, $arg;
        }
    }

    if (!defined $plugin_path) {
        $plugin_path = create_goof_plugin_dir(catdir($build_dir, qw/src plugins/), $goof_dir);
    }

    my $pid = do_fork();

    if (!$pid) {
        open STDIN,  '</dev/null' or croak("Can't open STDIN from /dev/null: `$!'");
        open STDOUT, '>/dev/null' or croak("Can't open STDOUT to /dev/null: `$!'");
        open STDERR, '>&STDOUT'   or croak("Can't open STDERR to STDOUT: `$!'");

        $ENV{XDG_CONFIG_HOME} = catdir($goof_dir, qw/home xmms2d .config/);
        $ENV{XDG_CACHE_HOME}  = catdir($goof_dir, qw/home xmms2d .cache/);

        chdir '/';

        POSIX::setsid();

        exec $exec_path, '-p', $plugin_path, '-o', 'null', @extra_args;
    }

    sleep 1; #wait for the server socket to become ready

    my $c = Audio::XMMSClient->new('Test_XMMS');
    $c->connect or croak($c->get_last_error);

    #TODO: handle dying xmms2d (xmms broadcasts? SIGCHLD?)

    $xmms_client = $c;
    $xmms_pid = $pid;
}

sub tear_down {
    $xmms_client->quit->wait;
}

END {
    tear_down();

    waitpid $xmms_pid, 0;
}

1;
