package Test::XMMS;

use strict;
use warnings;
use base qw/Test::Builder::Module Exporter/;
use Carp qw/croak/;
use Scalar::Util qw/blessed/;
use POSIX qw/SIGINT SIG_BLOCK SIG_UNBLOCK/;
use File::Path qw/mkpath/;
use File::Basename qw/basename/;
use File::Spec::Functions qw/splitpath splitdir catdir catfile rel2abs/;

my $xmms_client;
my $xmms_pid;

our @EXPORT = qw/coll_ok/;

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

my ($top_dir, $build_dir);

BEGIN {
    my $package = __PACKAGE__;
    (my $path = $package) =~ s{::}{/}g;
    $path .= '.pm';

    my $module_path = rel2abs($INC{$path});

    my (undef, $directories, undef) = splitpath($module_path);

    $top_dir = catdir($directories, (('..') x 4));
    $build_dir = catfile($top_dir, qw/_build_ default/);

    $ENV{LD_LIBRARY_PATH} = catdir($build_dir, qw/ src clients lib xmmsclient/);

    eval "require Audio::XMMSClient";
    Audio::XMMSClient->import;
}

sub import {
    my ($self, @args) = @_;

    $self->export_to_level( 1, $self, $_ ) foreach @EXPORT;

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

{
    my $Tester = __PACKAGE__->builder;

    sub coll_ok {
        my ($coll, $expected_coll) = @_;
        my $diag;

        if (!defined $coll) {
            $diag = 'The object is not defined';
        }
        elsif (!ref $coll) {
            $diag = 'The object is not a reference';
        }
        elsif (!blessed($coll)) {
            $diag = 'The object is not blessed';
        }
        elsif (!$coll->isa('Audio::XMMSClient::Collection')) {
            $diag = 'The object is a ' . blessed($coll) . ', not an Audio::XMMSClient::Collection instance';
        }

        if ($diag) {
            $Tester->ok(0, 'Not a collection');
            $Tester->diag($diag);
            return;
        }
        else {
            $Tester->ok(1, 'isa collection');
        }

        if (defined (my $type = $expected_coll->{type})) {
            $Tester->is_eq($coll->get_type, $type, 'collection type');
        }

        {
            my ($got, $expected, $name) = ({ $coll->attribute_list }, $expected_coll->{attributes} || {}, 'correct attributes');

            if( !ref $got and !ref $expected ) {
                $Tester->is_eq($got, $expected, $name);
            }
            elsif (!ref $got xor !ref $expected) {
                $Tester->ok(0, $name);
                $Tester->diag( Test::More::_format_stack({ vals => [ $got, $expected ] }) );
            }
            else {
                local @Test::More::Data_Stack = ();
                if (Test::More::_deep_check($got, $expected)) {
                    $Tester->ok(1, $name);
                }
                else {
                    $Tester->ok(0, $name);
                    $Tester->diag(Test::More::_format_stack(@Test::More::Data_Stack));
                }
            }
        }

        my @operands = $coll->operand_list;
        $Tester->is_num(scalar @operands, scalar @{ $expected_coll->{operands} || [] }, 'correct number of operands');

        for (my $i = 0; $i < @operands; $i++) {
            coll_ok($operands[$i], $expected_coll->{operands}->[$i]);
        }
    }
}

END {
    if (defined $xmms_pid) {
        tear_down();
        waitpid $xmms_pid, 0;
    }
}

1;
