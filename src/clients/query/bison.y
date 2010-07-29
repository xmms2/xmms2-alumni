%{
#include <stdio.h>
#include <stdlib.h>
#include <xmmsclient/xmmsclient.h>

int yylex (void);
void yyerror (const char *);

void handle_line (xmmsv_t *val);
%}

%code requires {
#define YYSTYPE xmmsv_t*
}

%token CLUSTER_LIST CLUSTER_DICT KEY STRING GET NUL DICT COUNT

%%
input: /* Empty */
	 | input line { if ($2 != NULL) handle_line ($2); }
;

line: '\n'       { $$ = NULL; }
	| value '\n' { $$ = $1; }
;

value: cluster_list { $$ = $1; }
	 | cluster_dict { $$ = $1; }
	 | get          { $$ = $1; }
	 | dict         { $$ = $1; }
	 | count        { $$ = $1; }
;

dict_keys: /* Empty */
		 {
			$$ = xmmsv_new_dict ();
			xmmsv_dict_set_string ($$, "_type", "organize");
		 }
		 | dict_keys KEY '(' value ')'
		 {
			const char *str;
			$$ = $1;
			xmmsv_get_string ($2, &str);
			xmmsv_dict_set ($$, str, $4);
		 }
;

dict: DICT dict_keys { $$ = $2; }
;

count: COUNT
	 {
		$$ = xmmsv_new_dict ();
		xmmsv_dict_set_string ($$, "_type", "count");
	 }
;

keys: KEY
	{
		$$ = xmmsv_new_list ();
		xmmsv_list_append ($$, $1);
	}
	| keys KEY
	{
		$$ = $1;
		xmmsv_list_append ($$, $2);
	}
;

list: '(' keys ')' { $$ = $2; }
	| NUL          { $$ = NULL; }
;

cluster_list: CLUSTER_LIST list value
			{
				$$ = xmmsv_new_dict ();
				xmmsv_dict_set_string ($$, "_type", "cluster-list");
				xmmsv_dict_set ($$, "cluster-by", $2);
				xmmsv_dict_set ($$, "data", $3);
			}
;

cluster_dict: CLUSTER_DICT list value
			{
				$$ = xmmsv_new_dict ();
				xmmsv_dict_set_string ($$, "_type", "cluster-dict");
				xmmsv_dict_set ($$, "cluster-by", $2);
				xmmsv_dict_set ($$, "data", $3);
			}
;

get: GET list list
   {
		$$ = xmmsv_new_dict ();
		xmmsv_dict_set_string ($$, "_type", "metadata");
		if ($2 != NULL) xmmsv_dict_set ($$, "keys", $2);
		if ($3 != NULL) xmmsv_dict_set ($$, "get", $3);
   }
   | GET KEY list list
   {
		$$ = xmmsv_new_dict ();
		xmmsv_dict_set_string ($$, "_type", "metadata");
		if ($3 != NULL) xmmsv_dict_set ($$, "keys", $3);
		if ($4 != NULL) xmmsv_dict_set ($$, "get", $4);
		xmmsv_dict_set ($$, "aggregate", $2);
   }
;


%%
void yyerror (const char *err)
{
	printf ("Invalid query\n");
}

static void print_xmmsv (xmmsv_t *xmmsv, int indent)
{
	const char *str;
	int i;
	int32_t ival;
	xmmsv_t *val;
	xmmsv_dict_iter_t *it;

	switch (xmmsv_get_type (xmmsv)) {
	case XMMSV_TYPE_STRING:
		xmmsv_get_string (xmmsv, &str);
		printf ("\"%s\"", str);
		break;
	case XMMSV_TYPE_DICT:
		xmmsv_get_dict_iter (xmmsv, &it);
		printf ("{\n");
		for (; xmmsv_dict_iter_valid (it); xmmsv_dict_iter_next (it)) {
			xmmsv_dict_iter_pair (it, &str, &val);
			for (i = 0; i < indent; i++) {
				printf ("  ");
			}
			printf ("%s: ", str);
			print_xmmsv (val, indent + 1);
			printf ("\n");
		}
		for (i = 0; i < indent; i++) {
			printf ("  ");
		}
		printf ("}");
		break;
	case XMMSV_TYPE_LIST:
		printf ("(");
		for (i = 0; xmmsv_list_get (xmmsv, i, &val); i++) {
			if (i != 0) {
				printf (", ");
			}
			print_xmmsv (val, indent + 1);
		}
		printf (")");
		break;
	case XMMSV_TYPE_INT32:
		xmmsv_get_int (xmmsv, &ival);
		printf ("%i", ival);
		break;
	default: /* Silence compiler warnings */
		break;
	}
}

xmmsc_connection_t *conn;

void handle_line (xmmsv_t *xmmsv)
{
	xmmsv_coll_t *coll = xmmsv_coll_universe ();
	xmmsc_result_t *res;
	xmmsv_t *val;
	res = xmmsc_coll_query (conn, coll, xmmsv);

	xmmsc_result_wait (res);
	val = xmmsc_result_get_value (res);

	print_xmmsv (val, 0);
	printf ("\n");

	xmmsc_result_unref (res);
}

int main (void)
{
	char *path;

	path = getenv ("XMMS_PATH");
	conn = xmmsc_init ("xmms2-query");

	if (!conn) {
		printf ("Could not init xmmsc_connection!\n");
		exit (1);
	}

	if (!xmmsc_connect (conn, path)) {
		printf ("Could not connect to xmms2d: %s\n", xmmsc_get_last_error (conn));
		exit (1);
	}

	/* Keep parsing after syntax errors */
	while (yyparse () == 1);

	return 0;
}
