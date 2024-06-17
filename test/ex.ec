
@apply(test_proc_macro)
@tag("test_tag")
struct A {
    int x;
    long x;
};

@proc_macro
ProcMacroError
test_proc_macro(C_Ast_Node *node, C_Ast_Node **out_node) {

}


@macro ident_to_uppercase(id:ident) {
    
}

void
foo() {

}
