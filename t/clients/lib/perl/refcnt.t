#!perl

use strict;
use warnings;
use Test::More;
use Audio::XMMSClient;

eval "use Devel::Peek";
plan skip_all => 'Devel::Peek required' if $@;

plan tests => 3;

{
    my $c = Audio::XMMSClient->new('foo');
    is(Devel::Peek::SvREFCNT($c), 1, 'object created with refcnt 1');
}

{
    my $c = Audio::XMMSClient::Collection->new('match');
    is(Devel::Peek::SvREFCNT($c), 1, 'object created with refcnt 1');
}

{
    my $c = Audio::XMMSClient::Collection->parse('a:b');
    is(Devel::Peek::SvREFCNT($c), 1, 'object created with refcnt 1');
}
