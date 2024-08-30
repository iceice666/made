//! Grammar for made-lang.
//!
//! program -> statement* EOF
//!
//! block -> "{" statement* "}"
//!
//! statement -> ( variable_declaration
//!             | typealias_declaration
//!             | named_function_declaration
//!             | while_statement
//!             | for_statement
//!             | return_statement
//!             | break_statement
//!             | continue_statement
//!             | assignment_statement
//!             | struct_declaration
//!             | enum_declaration
//!             | trait_declaration
//!             | impl_declaration
//!             | expression ) ";"?
//!             
//! variable_declaration -> "let" IDENTIFIER (":" type)? "=" expression ";"
//!
//! typealias_declaration -> "typealias" IDENTIFIER "=" type ";"
//!
//! named_function_declaration -> "func" IDENTIFIER function_signature block
//! function_signature -> "(" parameter_list ")" ("->" type)?
//!  
//! while_statement -> "while" expression block
//! for_statement -> "for" IDENTIFIER "in" expression block
//! return_statement -> "return" expression? ";"
//! break_statement -> "break" ";"
//! continue_statement -> "continue" ";"
//!
//! struct_declaration -> "struct" IDENTIFIER "{" field_list "}"
//! field_list -> field ("," field)*
//! field -> IDENTIFIER ":" type
//!
//! enum_declaration -> "enum" IDENTIFIER "{" variant_list "}"
//! variant_list -> variant ("," variant)*
//! variant -> IDENTIFIER ( "(" type_list ")" | "{" field_list "}")?
//!
//! trait_declaration -> "trait" IDENTIFIER "{" named_function_declaration* "}"
//!
//! impl_declaration -> "impl" ( trait "for" )? type "{" named_function_declaration* "}"
//! trait -> type
//!
//!
//! assignment_statement -> IDENTIFIER "=" expression ";"
//!
//! expression -> if_expression
//!             | match_expression
//!             | lambda_declaration
//!             | logical_or_expression
//!
//! logical_or_expression -> logical_and_expression (("or" | "||") logical_and_expression)*
//! logical_and_expression -> equality_expression (("and" | "&&") equality_expression)*
//! equality_expression -> comparison_expression (("==" | "!=") comparison_expression)*
//! comparison_expression -> additive_expression ((">" | "<" | ">=" | "<=") additive_expression)*
//! additive_expression -> multiplicative_expression (("+" | "-") multiplicative_expression)*
//! multiplicative_expression -> unary_expression (("*" | "/" | "%") unary_expression)*
//! unary_expression -> ("+" | "-" | "not") unary_expression | primary_expression
//!
//! primary_expression -> literal
//!                     | IDENTIFIER
//!                     | "(" expression ")"
//!                     | function_call_expression
//!                     | field_access_expression
//!
//! if_expression -> "if" expression block ("else" if_expression | block)?
//! match_expression -> "match" expression "{" match_arm+ "}"
//! match_arm -> (pattern ("|" pattern)*) "=>" block ";"
//!
//! lambda_declaration -> function_signature block
//!
//! field_access_expression -> primary_expression "." IDENTIFIER
//! function_call_expression -> primary_expression "(" argument_list? ")"
//!
//! parameter_list -> parameter ("," parameter)*
//! parameter -> IDENTIFIER ":" type
//! argument_list -> expression ("," expression)*
//!
//! literal -> boolean_literal
//!          | integer_literal
//!          | float_literal
//!          | char_literal
//!          | string_literal
//!
//! boolean_literal -> "true" | "false"
//! integer_literal -> DIGIT (DIGIT | "'")*
//! float_literal -> integer_literal "." integer_literal
//! char_literal -> "'" CHAR "'"
//! string_literal -> "\"" CHAR* "\""
//!
//! type -> IDENTIFIER
//!       | IDENTIFIER "<" type_list ">"
//!
//! type_list -> type ("," type)*
//!
//! DIGIT -> regex("[0-9]")
//! CHAR -> regex("([^\"\\n\\r])")
//! IDENTIFIER -> regex("\S+")
//! WHITESPACE -> regex("\s+") -> skip
//! COMMENT -> regex("#.*") -> skip
