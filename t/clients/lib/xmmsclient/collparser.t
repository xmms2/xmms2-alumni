#!perl

use strict;
use warnings;
use Test::More qw/no_plan/;

use FindBin;
use lib "$FindBin::Bin/../../../helpers/lib";

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

{
    my $coll = $coll_ns->parse('::');
    ok(!defined $coll);

    $coll = $coll_ns->parse('in:pocket ::');
    ok(!defined $coll);

    $coll = $coll_ns->parse('in:pocket OR ::');
    ok(!defined $coll);
}

{
    my $expected_ref_pocket = {
        type       => 'reference',
        attributes => {
            reference => 'pocket',
            namespace => 'Collections',
        },
    };

    my $expected_artist_air = {
        type       => 'equals',
        attributes => {
            field => 'artist',
            value => 'Air',
        },
        operands   => [ $universe ],
    };

    my $expected_ref_all_media = {
        type       => 'reference',
        attributes => {
            reference => 'All Media',
            namespace => 'Collections',
        },
    };

    my $coll = $coll_ns->parse('in:pocket OR artist:Air');
    coll_ok($coll, {
            type     => 'union',
            operands => [ $expected_ref_pocket, $expected_artist_air ],
    });

    $coll = $coll_ns->parse('artist:Air OR in:pocket');
    coll_ok($coll, {
            type     => 'union',
            operands => [ $expected_artist_air, $expected_ref_pocket ],
    });

    $coll = $coll_ns->parse('in:pocket AND artist:Air');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [ $expected_ref_pocket, $expected_artist_air ],
    });

    $coll = $coll_ns->parse('artist:Air AND in:pocket');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [ $expected_artist_air, $expected_ref_pocket ],
    });

    $coll = $coll_ns->parse('in:pocket artist:Air');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [ $expected_ref_pocket, $expected_artist_air ],
    });

    $coll = $coll_ns->parse('artist:Air in:pocket');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [ $expected_artist_air, $expected_ref_pocket ],
    });

    $coll = $coll_ns->parse('in:"All Media" OR artist:Air');
    coll_ok($coll, {
            type => 'union',
            operands => [ $expected_ref_all_media, $expected_artist_air ],
    });
}

{
    my ($expected_ref_a, $expected_ref_b) = map {
        +{
            type => 'reference',
            attributes => {
                reference => $_,
                namespace => 'Collections',
            },
        }
    } qw/A B/;

    my $coll = $coll_ns->parse('in:A OR in:B');
    coll_ok($coll, {
            type     => 'union',
            operands => [ $expected_ref_a, $expected_ref_b ],
    });
}

{
    my $coll = $coll_ns->parse('artist:"Foo: Bar"');
    coll_ok($coll, {
            type       => 'equals',
            attributes => {
                field => 'artist',
                value => 'Foo: Bar',
            },
            operands   => [ $universe ],
    });
}

{
    my $coll = $coll_ns->parse('artist:"Foo (Bar)"');
    coll_ok($coll, {
            type       => 'equals',
            attributes => {
                field => 'artist',
                value => 'Foo (Bar)',
            },
            operands   => [ $universe ],
    });
}

{
    my $coll = $coll_ns->parse('artist:Foo\:\ Bar');
    coll_ok($coll, {
            type       => 'equals',
            attributes => {
                field => 'artist',
                value => 'Foo: Bar',
            },
            operands   => [ $universe ],
    });
}

{
    my $coll = $coll_ns->parse('artist:Foo\ \(Bar\)');
    coll_ok($coll, {
            type       => 'equals',
            attributes => {
                field => 'artist',
                value => 'Foo (Bar)',
            },
            operands => [ $universe ],
    });
}

{
    my $expected_artist_air = {
        type       => 'equals',
        attributes => {
            field => 'artist',
            value => 'Air',
        },
        operands => [ $universe ],
    };

    my $expected_album_moon_safari = {
        type       => 'equals',
        attributes => {
            field => 'album',
            value => 'Moon Safari',
        },
        operands => [ $universe ],
    };

    my $coll = $coll_ns->parse('(artist:Air)');
    coll_ok($coll, $expected_artist_air);

    $coll = $coll_ns->parse('(artist:Air album:"Moon Safari")');
    coll_ok($coll, {
            type => 'intersection',
            operands => [ $expected_artist_air, $expected_album_moon_safari ],
    });

    $coll = $coll_ns->parse('(artist:Air AND album:"Moon Safari")');
    coll_ok($coll, {
            type => 'intersection',
            operands => [ $expected_artist_air, $expected_album_moon_safari ],
    });

    $coll = $coll_ns->parse('(artist:Air OR album:"Moon Safari")');
    coll_ok($coll, {
            type => 'union',
            operands => [ $expected_artist_air, $expected_album_moon_safari ],
    });

    $coll = $coll_ns->parse('(artist:Air NOT album:"Moon Safari")');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [
                $expected_artist_air,
                {
                    type     => 'complement',
                    operands => [ $expected_album_moon_safari ],
                },
            ],
    });

    $coll = $coll_ns->parse('(artist:Air AND (album:"Moon Safari" OR title:Radian))');
    coll_ok($coll, {
            type     => 'intersection',
            operands => [
                $expected_artist_air,
                {
                    type     => 'union',
                    operands => [
                        $expected_album_moon_safari,
                        {
                            type       => 'equals',
                            attributes => {
                                field => 'title',
                                value => 'Radian',
                            },
                            operands   => [ $universe ],
                        }
                    ],
                },
            ],
    });
}

{
    my @ops = (
            [ equals  => 42 ],
            [ smaller => 42 ],
            [ greater => 42 ],
            [ smaller => 43 ],
            [ greater => 41 ],
    );

    my ($expected_coll_eq,
        $expected_coll_lt,
        $expected_coll_gt,
        $expected_coll_lteq,
        $expected_coll_gteq) = map {
        +{
            type       => $_->[0],
            attributes => {
                field => 'tracknr',
                value => $_->[1],
            },
            operands   => [ $universe ],
        }
    } @ops;

    my $coll = $coll_ns->parse('tracknr:42');
    coll_ok($coll, $expected_coll_eq);

    $coll = $coll_ns->parse('tracknr<42');
    coll_ok($coll, $expected_coll_lt);

    $coll = $coll_ns->parse('tracknr>42');
    coll_ok($coll, $expected_coll_gt);

    $coll = $coll_ns->parse('tracknr<=42');
    coll_ok($coll, $expected_coll_lteq);
}
