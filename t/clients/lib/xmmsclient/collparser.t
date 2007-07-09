#!perl

use strict;
use warnings;
use Test::More qw/no_plan/;
use Test::XMMS qw/-nostart/;
use Audio::XMMSClient;

my $coll_ns = 'Audio::XMMSClient::Collection';

my $universe = {
    type       => 'reference',
    attributes => {
        reference => 'All Media',
    },
};

{
    my @coll_autofilter = qw/artist album title/;

    my ($expected_foo, $expected_bar, $expected_baz) = map { my $value = $_; +{
            type       => 'union',
            operands   => [ map { +{
                    type       => 'equals',
                    attributes => {
                        field  => $_,
                        value  => $value,
                    },
                    operands   => [ $universe ],
                } } @coll_autofilter,
            ],
    } } qw/Foo Bar Baz/;

    my $coll = $coll_ns->parse('Foo');
    coll_ok($coll, $expected_foo);

    $coll = $coll_ns->parse('Foo Bar');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [
                $expected_foo,
                $expected_bar,
            ],
    });

    $coll = $coll_ns->parse('Foo AND Bar');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [
                $expected_foo,
                $expected_bar,
            ],
    });

    $coll = $coll_ns->parse('Foo OR Bar');
    coll_ok($coll, {
            type     => 'union',
            operands => [
                $expected_foo,
                $expected_bar,
            ],
    });

    $coll = $coll_ns->parse('Foo Bar Baz');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [
                $expected_foo,
                $expected_bar,
                $expected_baz,
            ],
    });
}

{
    my %short_prop_map = (
            a => 'artist',
            l => 'album',
            t => 'title',
            n => 'tracknr',
            y => 'year',
            g => 'genre',
            u => 'url',
    );

    while (my ($short_prop, $long_prop) = each %short_prop_map) {
        my $coll = $coll_ns->parse($short_prop . ':Foo');
        coll_ok($coll, {
                type       => 'equals',
                attributes => {
                    field  => $long_prop,
                    value  => 'Foo',
                },
                operands   => [ $universe ],
        });
    }
}

{
    my $expected_ref_all_media = {
        type       => 'reference',
        attributes => {
            reference => 'All Media',
            namespace => 'Collections',
        },
    };

    my $expected_title_foo = {
        type       => 'equals',
        attributes => {
            field => 'title',
            value => 'Foo',
        },
        operands => [ $universe ],
    };

    my $coll = $coll_ns->parse('in:"All Media"');
    coll_ok($coll, $expected_ref_all_media);

    $coll = $coll_ns->parse('title:Foo');
    coll_ok($coll, $expected_title_foo);

    $coll = $coll_ns->parse('title:Foo in:"All Media"');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [ $expected_title_foo, $expected_ref_all_media ],
    });

    $coll = $coll_ns->parse('in:"All Media" title:Foo');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [ $expected_ref_all_media, $expected_title_foo ],
    });

    $coll = $coll_ns->parse('in:"All Media" AND title:Foo');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [ $expected_ref_all_media, $expected_title_foo ],
    });
}
