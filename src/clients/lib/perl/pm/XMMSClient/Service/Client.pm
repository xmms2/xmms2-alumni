package Audio::XMMSClient::Service::Client;

use strict;
use warnings;
use Carp;
use Sub::Name;
use Audio::XMMSClient::Glib;

sub import {
    my ($class) = @_;
    my $package = (caller())[0];

    {
        no strict 'refs';
        *{ "${package}::service_method" } = \&service_method;
        unshift @{ "${package}::ISA" }, __PACKAGE__;
    }

    return;
}

{
    my %class_meths;

    sub service_method {
        my ($data, $func) = @_;

        subname $data->{name}, $func;

        my $meth = Audio::XMMSClient::Service::Method->new(
                $data->{name},
                $data->{description},
                _wrap_method($func, $data),
        ) or die;

        for my $arg (@{ $data->{args} || [] }) {
            $meth->arg_type_add(
                    $arg->{name},
                    $arg->{type},
                    ($arg->{optional} || 0),
            );
        }

        for my $ret (@{ $data->{return} || [] }) {
            $meth->ret_type_add(
                    $ret->{name},
                    $ret->{type},
                    ($ret->{optional} || 0),
            );
        }

        my $package = (caller())[0];

        $class_meths{$package} ||= [];
        push @{ $class_meths{$package} }, $meth;

        return;
    }

    sub _registered {
        my ($self, $result) = @_;

        if ($result->iserror) {
            croak $result->get_error;
        }

        for my $meth (@{ $class_meths{ ref $self } || [] }) {
            $self->conn->service_method_register(
                    $self->{args}->{name},
                    $meth,
            );
        }

        return;
    }
}

sub _wrap_method {
    my ($func, $data) = @_;

    my $wrapper = sub {
        my ($conn, $result, $method) = @_;

        if ($result->iserror) {
            warn $result->get_error;
            return;
        }

        my $args = $result->value;

        my @ret;
        eval {
            @ret = $func->($conn, $args);
        };

        use Data::Dump qw/dump/;
        dump $args;
        dump \@ret;

        if (my $error = $@) {
            $method->error_set($error);
        }
        else {
            my $i = 0;
            for my $ret (@{ $data->{ret} || [] }) {
                my $type     = $ret->{type};
                my $ret_func = "ret_add_${type}";

                last if $i > $#ret;

                my $value = $ret[$i++];

                warn "$ret_func $ret->{name} $value";

                $method->$ret_func($ret->{name}, $value)
                    if defined $value;
            }
        }

        $conn->service_return($result, $method);

        return;
    };

    return $wrapper;
}

sub new {
    my ($class, $args) = @_;

    my $self = bless {
        args => $args,
        conn => Audio::XMMSClient->new(($args->{client_name}|| ())),
    }, $class;

    return $self;
}

sub conn {
    return $_[0]->{conn};
}

sub _init {
    my ($self) = @_;

    $self->conn->connect(($self->{args}->{ipc_path} || ()))
        or croak $self->conn->get_last_error;

    return;
}

sub _register {
    my ($self) = @_;

    my ($major, $minor) = ($self->{args}->{major}, $self->{args}->{minor});

    if (!defined $major || !defined $minor) {
        my $pkg_version = $self->VERSION;

        ($major, $minor) = split /\./, $pkg_version;
    }

    $self->conn->service_register(
            $self->{args}->{name},
            $self->{args}->{description},
            $major,
            $minor,
    )->notifier_set(sub { $self->_registered(@_) });

    return;
}

sub _loop {
    my ($self) = @_;

    $self->conn->loop;

    return;
}

sub run {
    my ($class, $args) = @_;

    my $self = $class->new($args);

    $self->_init;
    $self->_register;
    $self->_loop;

    return;
}

1;
