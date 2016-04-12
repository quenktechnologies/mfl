/* Grammar for the Mongo Query Filter Language */

%lex

/* Definitions */


/* Flags */
%options flex
%x SC_LITERAL
%%

/* Rules */
\s+                                                             /* skips whitespace */
'true'                                                          return 'TRUE';
'false'                                                         return 'FALSE';
':'                                                             return ':';
[0-9]+'.'[0-9]+                                                 return 'FLOAT';
[0-9]+                                                          return 'INTEGER';
'OR'                                                            return 'OR';
[^\[\]?<>:\s,"]+                                                return 'FIELD';
'['                                                             return '[';
']'                                                             return ']';
','                                                             return ',';
'>='                                                            return '>=';
'>'                                                             return '>';
'<='                                                            return '<=';
'<'                                                             return '<';
'='                                                             return '=';
'?'                                                             return '?';
'"'                         this.begin('SC_LITERAL');           return '"';
<SC_LITERAL>[^"]*                                               return 'QUOTED';
<SC_LITERAL>'"'             this.popState();                    return '"';
<*><<EOF>>                                                      return 'EOF';

/lex
%ebnf
%start query
%%

query

            : filters EOF {
                $$ = {
                type:'query',
                filters: $1
                };
                return $$;
              }
            | filter EOF {

              $$ = {
              type:'query',
              filters: {AND:[$1], OR:[]}
              };
              return $$;
            }
            ;

filters
            
            : filter 'OR'? filter   {

                $$ = {AND:[], OR:[]};
                $$[$2 || 'AND'].push($1, $3);

              }
            | filters 'OR'? filter {

              $$ = $1;
              $$[$2 || 'AND'].push($3);

            }
            ;

filter      

            : FIELD ':' operator value {
                $$ = {
                type: 'filter',
                field: $1,
                operator: $3,
                value: $4
                };
              }
            | FIELD ':' value {
                $$ = {
                type: 'filter',
                field: $1,
                operator:'=',
                value: $3
                }
              }
            | FIELD ':' '[' value ']' {
                $$ = {
                type: 'filter',
                field: $1,
                operator: '$in',
                value: [$4]
                }
              }
            | FIELD ':' '[' value_list ']' {
                $$ = {
                type: 'filter',
                field: $1,
                operator: '$in',
                value: $4
                }
              }
            ;

operator   

    : '>'                           { $$ = '>';                   }
    | '<'                           { $$ = '<';                   }
    | '>='                          { $$ = '>=';                  }
    | '<='                          { $$ = '<=';                  }
    | '='                           { $$ = '=';                   }
    | '?'                           { $$ = '?';                   }
    ;

value_list
    : value ',' value              { $$ = [$1, $3]                }
    | value_list ',' value         { $$ = $1.concat($3);          }
    ;

value
    : TRUE                          { $$ = true;                  }
    | FALSE                         { $$ = false;                 }
    | INTEGER                       { $$ = parseInt($1);          }
    | FLOAT                         { $$ = parseFloat($1);        }
    | '"' QUOTED '"'                { $$ = $2;                    }
    | FIELD                         { $1 = $1;                    }
    ;
