%{
#include <xmmsclient/xmmsclient.h>
#include "bison.tab.h"
%}

%option noyywrap
KEY [[:alnum:]]+
STRING \"{KEY}\"
%%
cl return CLUSTER_LIST;
cd return CLUSTER_DICT;
get return GET;
nul return NUL;
dict return DICT;
count return COUNT;
{KEY} yylval = xmmsv_new_string (yytext); return KEY;
{STRING} yytext[yyleng - 1] = '\0'; yylval = xmmsv_new_string (yytext + 1); return STRING;
[[:blank:]]+ /* Skip whitespace */
\n|. return yytext[0];
%%
