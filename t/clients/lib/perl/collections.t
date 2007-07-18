#!perl

use strict;
use warnings;
use Test::More;
use Audio::XMMSClient;

my @valid_coll_types = qw/
    reference
    union
    intersection
    complement
    has
    equals
    match
    smaller
    greater
    idlist
    queue
    partyshuffle
/;

eval 'use Test::Exception';
plan skip_all => 'Test::Exception required' if $@;

eval 'use List::Util';
plan skip_all => 'List::Util required' if $@;

plan tests => 567;

my $coll_ns = 'Audio::XMMSClient::Collection';

dies_ok(sub {
        $coll_ns->new;
}, 'new dies without a coll type');

dies_ok(sub {
        $coll_ns->new('invalid collection type');
}, 'new dies with invalid coll type');

{
    for my $type (@valid_coll_types) {
        my $coll;

        lives_ok(sub {
                $coll = $coll_ns->new($type);
        }, "new lives with valid coll type: $type");

        isa_ok($coll, $coll_ns);

        is($coll->get_type, $type, 'get_type returns the type the coll was created with');
    }
}

{
    for my $filter (sub { ucfirst $_[0] }, sub { uc $_[0] }) {
        for my $type (map { $filter->($_) } @valid_coll_types) {
            dies_ok(sub {
                    $coll_ns->new($type);
            }, "coll types are case sensitive");
        }
    }
}

throws_ok(sub {
        $coll_ns->new($valid_coll_types[0], 'foo');
}, qr/expected hash reference or hash/, 'new dies with just one non-reference argument after the type');

throws_ok(sub {
        $coll_ns->new($valid_coll_types[0], \do { my $o });
}, qr/expected hash reference or hash/, 'new dies with just one scalar reference argument after the type');

{
    my $coll;

    lives_ok(sub {
            $coll = $coll_ns->new($valid_coll_types[0], foo => 'bar');
    }, 'new lives with two optional non-reference arguments');

    isa_ok($coll, $coll_ns);

    is_deeply({ $coll->attribute_list }, { foo => 'bar' }, 'new set only the given attributes');
}

throws_ok(sub {
        $coll_ns->new($valid_coll_types[0], qw/foo bar korv/);
}, qr{expected even number of attributes/values}, 'new throws with an odd number of keys/values');

{
    my @attrs_list = map { +{
        map {
            'korv' . $_ => 'sausage'
        } 0 .. $_
    } } 1 .. 21;

    for my $attrs (@attrs_list) {
        my $coll;

        lives_ok(sub {
                $coll = $coll_ns->new($valid_coll_types[0], %{ $attrs });
        }, 'new lives with multiple optional non-reference arguments');

        isa_ok($coll, $coll_ns);

        is_deeply({ $coll->attribute_list }, $attrs, 'new set only the given attributes');
    }
}

{
    my @attrs_list = map { +{
        map {
            'korv' . $_ => 'sausage'
        } 0 .. $_  - 1
    } } 0 .. 20;

    for my $attrs (@attrs_list) {
        my $coll;

        lives_ok(sub {
                $coll = $coll_ns->new($valid_coll_types[0], $attrs);
        }, 'new lives with hash reference attributes');

        isa_ok($coll, $coll_ns);

        is_deeply({ $coll->attribute_list }, $attrs, 'new set only the given attributes');
    }
}

{
    my $coll = $coll_ns->parse('a:b');
    isa_ok($coll, $coll_ns);
}

{
    my $meth = 'DESTROY';
    my $old_meth = $coll_ns->can($meth);
    my $destroy_called = 0;

    no strict 'refs';
    no warnings 'redefine';

    local *{ "${coll_ns}::${meth}" } = sub {
        $destroy_called++;
        $old_meth->(@_);
    };

    {
        my $coll_ref;

        {
            my $coll = $coll_ns->new($valid_coll_types[0]);
            $coll_ref = $coll;
        }

        is($destroy_called, 0, 'coll didn\'t get garbage collected');
    }

    is($destroy_called, 1, 'coll got garbage collected');
}

{
    my $coll = $coll_ns->new('idlist');

    my @idlists = map { [List::Util::shuffle(1 .. $_)] } 1 .. 20;

    {
        for my $list (@idlists) {
            $coll->set_idlist(@{ $list });
            is($coll->idlist_get_size, scalar @{ $list }, 'setting an idlist with ' . scalar @{ $list } . ' elements');

            is_deeply([ $coll->get_idlist ], $list , 'get_idlist');

            for my $i (0 .. scalar @{ $list }) {
                is($coll->idlist_get_index($i), $list->[$i], "idlist_get_index $i");
            }

            $coll->idlist_clear;
            is_deeply([ $coll->get_idlist ], [], 'idlist_clear');
        }
    }

    {
        for my $list (@idlists) {
            $coll->idlist_append($_) for @{ $list };

            is($coll->idlist_get_size, scalar @{ $list }, 'setting an idlist with ' . scalar @{ $list } . ' elements by appending');

            is_deeply([ $coll->get_idlist ], $list , 'get_idlist');

            $coll->idlist_clear;
            is_deeply([ $coll->get_idlist ], [], 'idlist_clear');
        }
    }
}

{
    my $coll = $coll_ns->new('idlist');

    my @e = 1 .. 50;

    my @a = @e;
    my @b = map { $_ % 2 ? () : splice @a, $_ - int($_ / 2), 1 } 0 .. scalar @a;

    {
        $coll->set_idlist(@a);

        for my $i (0 .. scalar @b - 1) {
            $coll->idlist_insert($i * 2, $b[$i]);
        }

        is_deeply([ $coll->get_idlist ], \@e, 'idlist_insert');

        for my $i (0 .. int((scalar @e - 1) / 2)) {
            $coll->idlist_move($i, scalar @e - 1 - $i);
            $coll->idlist_move(scalar @e - 2 - $i, $i);
        }

        is_deeply([ $coll->get_idlist ], [reverse @e], 'idlist_move');
    }

    {
        $coll->set_idlist(map { ($_) x 2 } @a);

        for my $i (0 .. scalar @a - 1) {
            $coll->idlist_set_index($i * 2, $coll->idlist_get_index($i * 2 + 1) - 1);
        }

        is_deeply([ $coll->get_idlist ], \@e, 'idlist_set_index');
    }
}

throws_ok(sub {
        $coll_ns->new('idlist')->set_idlist(1, 2, 0);
}, qr/0 is an invalid mlib id/, 'set_idlist dies when 0 is passed to it');

throws_ok(sub {
        $coll_ns->new('idlist')->idlist_append(0);
}, qr/0 is an invalid mlib id/, 'idlist_append dies when 0 is passed to it');

throws_ok(sub {
        $coll_ns->new('idlist')->idlist_insert(0, 0);
}, qr/0 is an invalid mlib id/, 'idlist_insert dies when 0 is passed to it');

throws_ok(sub {
        $coll_ns->new('idlist')->idlist_insert(42, 2);
}, qr/inserting id after end of idlist/, 'idlist_insert dies when inserting things after the end of the idlist');

throws_ok(sub {
        my $coll = $coll_ns->new('idlist');
        $coll->set_idlist(1 .. 4);
        $coll->idlist_move(0, 4);
}, qr/trying to move id to after the idlists end/, 'idlist_move dies when moving after the end of the idlist');

throws_ok(sub {
        my $coll = $coll_ns->new('idlist');
        $coll->set_idlist(1 .. 4);
        $coll->idlist_move(5, 2);
}, qr/trying to move id from after the idlists end/, 'idlist_move dies when moving from behind the idlists end');

throws_ok(sub {
        $coll_ns->new('idlist')->idlist_get_index(10);
}, qr/trying to get an id from behind the idlists end/, 'idlist_get_index dies when trying to get an id from behind the idlists end');

throws_ok(sub {
        $coll_ns->new('idlist')->idlist_set_index(0, 2);
}, qr/trying to set an id after the end of the idlist/, 'idlist_set_index dies when setting ids after the end of the idlist');

throws_ok(sub {
    my $coll = $coll_ns->new('idlist');
    $coll->set_idlist(1 .. 4);
    $coll->idlist_set_index(4, 2);
}, qr/trying to set an id after the end of the idlist/, 'idlist_set_index dies when setting ids after the end of the idlist (with non-empty idlist)');

{
    my $universe = $coll_ns->universe;

    isa_ok($universe, $coll_ns);
}

{
    my $universe = $coll_ns->universe;

    my $match_artist_foo = $coll_ns->new('match', { field => 'artist', value => 'foo' });
    $match_artist_foo->add_operand($universe);

    my $match_album_bar = $coll_ns->new('match',  { field => 'album',  value => 'bar' });
    $match_artist_foo->add_operand($universe);

    my $union = $coll_ns->new('union');
    $union->add_operand($match_artist_foo);
    $union->add_operand($match_album_bar);

    {
        my @ops;

        for ($union->operand_list_first; my $op = $union->operand_list_entry; $union->operand_list_next) {
            push @ops, $op;
        }

        is_deeply(\@ops, [$match_artist_foo, $match_album_bar], 'operand_list_{first,entry,next}');
    }

    {
        my $i = 0;

        for ($union->operand_list_first; $union->operand_list_valid; $union->operand_list_next) {
            $i++;
        }

        is($i, 2, 'operand_list_valid');
    }

    {

        $union->operand_list_first;
        $union->operand_list_next;
        $union->operand_list_save;

        $union->operand_list_first;
        while ($union->operand_list_valid) {
            $union->operand_list_next;
        }

        $union->operand_list_restore;

        TODO: {
            local $TODO = 'add_operand needs to record the SV wrapper for the operand to return that when requesting the operands SV';
            is($union->operand_list_entry, $match_album_bar, 'operand_list{save,restore}');
        }
    }

    is_deeply([$union->operands], [$match_artist_foo, $match_album_bar], 'operands');
}

{
    my $coll = $coll_ns->new('equals');

    $coll->attribute_set(foo => 'bar');
    is($coll->attribute_get('foo'), 'bar', 'attribute_{set,get}');

    $coll->attribute_remove('foo');
    is($coll->attribute_get('foo'), undef, 'attribute_remove');
}

{
    my $coll = $coll_ns->new('equals');

    my %attrs = (
            foo => 'bar',
            moo => 'kooh',
            korv => 'sausage',
    );

    while (my ($key, $val) = each %attrs) {
        $coll->attribute_set($key => $val);
    }

    is_deeply({$coll->attribute_list}, \%attrs, 'attribute_list');
}
