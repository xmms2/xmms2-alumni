#!perl

use strict;
use warnings;
use Test::More tests => 14;
use Test::XMMS;
use Audio::XMMSClient;

my $c = Audio::XMMSClient->new('test-bindata');
$c->connect or BAIL_OUT($c->get_last_error);

my $hash;
my $data = 'foo';

{ # add some bindata
    my $res = $c->bindata_add($data);
    isa_ok($res, 'Audio::XMMSClient::Result');
    $res->wait;
    ok(!$res->iserror, 'bindata_add');
    like($res->value, qr/[a-z0-9]{32}/i, 'hash looks sane');

    $hash = $res->value;
}

#TODO: test if the server does md5 right? is it specced to use md5?

{ # same data -> same hash?
    my $res = $c->bindata_add($data);
    isa_ok($res, 'Audio::XMMSClient::Result');
    $res->wait;
    ok(!$res->iserror, 'bindata_retrieve');
    is($res->value, $hash, 'same data yields same hashes');
}

{ # different data -> different hash?
    my $res = $c->bindata_add($data . 'korv');
    isa_ok($res, 'Audio::XMMSClient::Result');
    $res->wait;
    ok(!$res->iserror, 'bindata_retrieve');
    isnt($res->value, $hash, 'different data yields different hashes');
}

{ # retrieve
    my $res = $c->bindata_retrieve($hash);
    isa_ok($res, 'Audio::XMMSClient::Result');
    $res->wait;
    ok(!$res->iserror, 'bindata_retrieve');
    is($res->value, $data, 'data looks sane');
}

{ # remove
    my $res = $c->bindata_remove($hash);
    isa_ok($res, 'Audio::XMMSClient::Result');
    $res->wait;
    ok(!$res->iserror, 'bindata_remove');
}
