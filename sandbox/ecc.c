#include <argp.h>

#include "parsing/c/tr_unit.c"
#include <string.h>

// #define VERSION "0.0.1"

#ifdef VERSION
const char *argp_program_version = "ecc " VERSION;
#endif

bool g_ecc_verbose = false;

static char doc[] = 
"ecc - Extended C Compiler. Source-to-source compiler.";

static char args_doc[] = "file...";

static struct argp_option options[] = {
    {"verbose", 'v', 0, 0, "Produce verbose output"},
    {"proc-macro-file-path", 'p', "<path>", 0, "Specify procedural macro file path"},
    {"output", 'o', "FILE", 0, "Output to FILE instead of standard output"},
    {0}};

typedef struct InputArgs InputArgs;
struct InputArgs {
    str_t proc_macro_path;
    str_t out_file;
    str_t in_file;
};

static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
    InputArgs *input_args = (InputArgs*) state->input;

    switch (key)
    {
    case 'v':
        // g_ecc_verbose = 1;
        break;
    case 'p':
        input_args->proc_macro_path = str_from_c_str_no_copy(arg);
        break;
    case 'o':
        input_args->out_file = str_from_c_str_no_copy(arg);
        break;
    case ARGP_KEY_ARG:
        if (state->arg_num >= 1)
        {
            argp_usage(state);
        }
        // input_args->branches[state->arg_num] = arg;
        input_args->in_file = str_from_c_str_no_copy(arg);
        break;
    case ARGP_KEY_END:
        if (state->arg_num < 1)
        {
            argp_usage(state);
        }
        break;
    default:
        return ARGP_ERR_UNKNOWN;
    }
    return 0;
}

static struct argp argp = { options, parse_opt, args_doc, doc, 0, 0, 0 };

void
argp_test(InputArgs *input_args) {
    print_fmt(S("test\n" "%s -p %s -o %s\n"), 
        input_args->in_file, 
        input_args->proc_macro_path, 
        input_args->out_file);
}


int main(int argc, char *argv[])
{
    ctx_init_default();

    InputArgs input_args;
    // g_ecc_verbose = 0;
    argp_parse(&argp, argc, argv, 0, 0, &input_args);

    // argp_test(&input_args);

    auto build_data = (C_BuildData) {
        .source_path = input_args.in_file,
        .target_path = input_args.out_file,
        .macro_file_path = input_args.proc_macro_path,
        .cc_path = S("/usr/bin/gcc"),
        .lib_macros_path = S("build/libmacros.so"),
    };
    
    C_TranslationUnitData tr_unit;
    c_translation_unit_init(&tr_unit, build_data.source_path);
    if (input_args.proc_macro_path.byte_len == 0) {
        ASSERT(ec_trainslation_unit_compile(&tr_unit, &build_data));
    } else {
        ASSERT(ec_trainslation_unit_compile_with_macros(&tr_unit, &build_data));
    }

    c_translation_unit_deinit(&tr_unit);
 
    ctx_deinit();
}