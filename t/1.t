#!/usr/bin/env perl

use strict;
use warnings;
use Test::More tests => 7;

ok(1, 'one is true');
ok(!0, 'zero is not true');
ok(1);

SKIP: {
    skip 'only root can do that', 2;
    ok(1, 'foo');
    ok(1);
}

{
    local $TODO = 'bug';
    ok(0, 'foo');
    ok(0);
}
