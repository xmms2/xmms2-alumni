#!perl

use strict;
use warnings;
use Test::More;
use Test::XMMS;

eval 'use Test::Exception';
plan skip_all => 'Test::Exception required' if $@;

plan tests => 7;

my $c = Audio::XMMSClient->new('foo');
$c->connect or BAIL_OUT($c->get_last_error);

isa_ok($c, 'Audio::XMMSClient');

{
    my $res = $c->main_stats;
    isa_ok($res, 'Audio::XMMSClient::Result');

    is($res->get_class, 'default', 'get_class');

    throws_ok(sub {
            $res->disconnect;
    }, qr/calling disconnect on a result that's neither a signal nor a broadcast/, 'disconnect on non-{broadcast,signals}');

    throws_ok(sub {
            $res->restart;
    }, qr/trying to restart a result that's not a signal/, 'restart a non-signal');

    $res->notifier_set(sub {
            my ($cb_res) = @_;

            isa_ok($cb_res, 'Audio::XMMSClient::Result');
            is($cb_res, $res, 'notifiers get the right result');

            $c->quit_loop;
    });

    $c->loop;
}
