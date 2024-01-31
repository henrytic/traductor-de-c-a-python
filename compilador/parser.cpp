/* A Bison parser, made by GNU Bison 3.8.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30800

/* Bison version string.  */
#define YYBISON_VERSION "3.8"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "parser.y"

    #include "node.h"
    NBlock *programBlock; 

    extern int yylex();
    void yyerror(const char *s) { fprintf(stderr, "ERROR: %sn", s); }

    int lineNumber = 1;

#line 81 "parser.cpp"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "parser.hpp"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_TIDENTIFIER = 3,                /* TIDENTIFIER  */
  YYSYMBOL_TINTEGER = 4,                   /* TINTEGER  */
  YYSYMBOL_TDOUBLE = 5,                    /* TDOUBLE  */
  YYSYMBOL_TCHAR = 6,                      /* TCHAR  */
  YYSYMBOL_TSTRING = 7,                    /* TSTRING  */
  YYSYMBOL_TINTTYPE = 8,                   /* TINTTYPE  */
  YYSYMBOL_TDOUBLETYPE = 9,                /* TDOUBLETYPE  */
  YYSYMBOL_TCHARTYPE = 10,                 /* TCHARTYPE  */
  YYSYMBOL_TVOIDTYPE = 11,                 /* TVOIDTYPE  */
  YYSYMBOL_TIF = 12,                       /* TIF  */
  YYSYMBOL_TELSE = 13,                     /* TELSE  */
  YYSYMBOL_TFOR = 14,                      /* TFOR  */
  YYSYMBOL_TWHILE = 15,                    /* TWHILE  */
  YYSYMBOL_TRETURN = 16,                   /* TRETURN  */
  YYSYMBOL_TLPAREN = 17,                   /* TLPAREN  */
  YYSYMBOL_TRPAREN = 18,                   /* TRPAREN  */
  YYSYMBOL_TLBRACE = 19,                   /* TLBRACE  */
  YYSYMBOL_TRBRACE = 20,                   /* TRBRACE  */
  YYSYMBOL_TLBRACK = 21,                   /* TLBRACK  */
  YYSYMBOL_TRBRACK = 22,                   /* TRBRACK  */
  YYSYMBOL_TOR = 23,                       /* TOR  */
  YYSYMBOL_TAND = 24,                      /* TAND  */
  YYSYMBOL_TCEQ = 25,                      /* TCEQ  */
  YYSYMBOL_TCNE = 26,                      /* TCNE  */
  YYSYMBOL_TCLT = 27,                      /* TCLT  */
  YYSYMBOL_TCLE = 28,                      /* TCLE  */
  YYSYMBOL_TCGT = 29,                      /* TCGT  */
  YYSYMBOL_TCGE = 30,                      /* TCGE  */
  YYSYMBOL_TEQUAL = 31,                    /* TEQUAL  */
  YYSYMBOL_TPLUS = 32,                     /* TPLUS  */
  YYSYMBOL_TMINUS = 33,                    /* TMINUS  */
  YYSYMBOL_TMUL = 34,                      /* TMUL  */
  YYSYMBOL_TDIV = 35,                      /* TDIV  */
  YYSYMBOL_TAMPERSAND = 36,                /* TAMPERSAND  */
  YYSYMBOL_TCOMMA = 37,                    /* TCOMMA  */
  YYSYMBOL_TDOT = 38,                      /* TDOT  */
  YYSYMBOL_TSEMICOLON = 39,                /* TSEMICOLON  */
  YYSYMBOL_TIFX = 40,                      /* TIFX  */
  YYSYMBOL_YYACCEPT = 41,                  /* $accept  */
  YYSYMBOL_program = 42,                   /* program  */
  YYSYMBOL_stmts = 43,                     /* stmts  */
  YYSYMBOL_stmt = 44,                      /* stmt  */
  YYSYMBOL_block = 45,                     /* block  */
  YYSYMBOL_var_decl = 46,                  /* var_decl  */
  YYSYMBOL_func_decl = 47,                 /* func_decl  */
  YYSYMBOL_func_decl_args = 48,            /* func_decl_args  */
  YYSYMBOL_ident = 49,                     /* ident  */
  YYSYMBOL_numeric = 50,                   /* numeric  */
  YYSYMBOL_type = 51,                      /* type  */
  YYSYMBOL_ptr = 52,                       /* ptr  */
  YYSYMBOL_expr = 53,                      /* expr  */
  YYSYMBOL_logic_expr = 54,                /* logic_expr  */
  YYSYMBOL_call_args = 55,                 /* call_args  */
  YYSYMBOL_comparison = 56,                /* comparison  */
  YYSYMBOL_condition = 57,                 /* condition  */
  YYSYMBOL_loop = 58,                      /* loop  */
  YYSYMBOL_return = 59,                    /* return  */
  YYSYMBOL_array_decl = 60                 /* array_decl  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  39
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   318

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  41
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  20
/* YYNRULES -- Number of rules.  */
#define YYNRULES  63
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  122

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   295


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    62,    62,    65,    66,    69,    70,    71,    72,    73,
      74,    75,    78,    79,    82,    83,    86,    90,    91,    92,
      95,    98,    99,   100,   101,   104,   105,   106,   107,   110,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   127,   128,   129,   130,   133,   134,   135,
     138,   138,   138,   138,   138,   138,   141,   142,   145,   146,
     147,   151,   152,   156
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "TIDENTIFIER",
  "TINTEGER", "TDOUBLE", "TCHAR", "TSTRING", "TINTTYPE", "TDOUBLETYPE",
  "TCHARTYPE", "TVOIDTYPE", "TIF", "TELSE", "TFOR", "TWHILE", "TRETURN",
  "TLPAREN", "TRPAREN", "TLBRACE", "TRBRACE", "TLBRACK", "TRBRACK", "TOR",
  "TAND", "TCEQ", "TCNE", "TCLT", "TCLE", "TCGT", "TCGE", "TEQUAL",
  "TPLUS", "TMINUS", "TMUL", "TDIV", "TAMPERSAND", "TCOMMA", "TDOT",
  "TSEMICOLON", "TIFX", "$accept", "program", "stmts", "stmt", "block",
  "var_decl", "func_decl", "func_decl_args", "ident", "numeric", "type",
  "ptr", "expr", "logic_expr", "call_args", "comparison", "condition",
  "loop", "return", "array_decl", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-31)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-45)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     144,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -10,    -2,    -1,    30,     5,    58,    68,   144,   -31,    33,
     -31,    39,   -31,    71,   -31,   213,    16,   -31,   -31,   -31,
      43,     5,   160,     5,   -31,   228,   176,    41,   -31,   -31,
     -31,   -31,     5,     5,     5,    50,   -31,   -31,   -31,   -31,
     -31,   -31,     5,     5,     5,     5,   -31,     5,     5,     5,
     -31,   283,    80,    44,    71,   243,    82,   -31,   -31,   -31,
     283,   -13,   258,   283,    18,     5,     5,    14,    14,   -31,
     -31,   283,    62,   161,    75,     5,    56,     5,    75,   -31,
       5,    64,   -31,     1,   272,   283,   108,    84,   -21,     7,
     -31,   283,     5,    75,    18,   -31,   -31,   126,    75,     5,
       5,   283,   -31,   -31,   -31,   -31,   189,   202,    75,    75,
     -31,   -31
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,    20,    21,    22,    24,    23,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     2,     3,     0,
       5,    34,    36,     0,    35,     0,    30,     8,     9,    10,
       0,     0,     0,     0,    62,     0,     0,    30,    29,     1,
       4,     6,    47,     0,     0,    14,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     7,     0,     0,     0,
      11,     0,    30,     0,     0,     0,    30,    61,    41,    46,
      48,     0,     0,    31,    17,     0,     0,    37,    38,    39,
      40,    45,    30,    30,     0,     0,    14,     0,     0,    33,
       0,    42,    18,     0,     0,    15,     0,    56,    30,    30,
      60,    49,     0,     0,     0,    63,    13,     0,     0,     0,
       0,    32,    16,    19,    12,    57,     0,     0,     0,     0,
      59,    58
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -31,   -31,    12,   -16,   -30,   -12,   -31,   -31,   -19,   -31,
     -11,   -31,     0,    -8,   -31,   -31,   -31,   -31,   -31,   -31
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,    16,    17,    18,    97,    19,    20,    93,    21,    22,
      23,    24,    61,    26,    71,    57,    27,    28,    29,    30
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      25,    40,    58,    59,    45,    89,    37,    31,     1,     2,
       3,     4,     5,    35,    36,    32,    33,    25,   109,   103,
      63,    64,    14,    62,    90,    66,     6,     7,     8,     9,
      58,    59,    65,     1,     2,     3,     4,     5,   104,    58,
      59,    15,    70,    72,    73,    86,   110,    14,    54,    55,
      82,    83,    77,    78,    79,    80,    42,    81,   100,    69,
      43,    38,    92,    64,    58,    59,    15,    74,    39,    34,
      44,    75,    41,   112,     1,    94,    95,    98,   115,    99,
     -44,    76,    60,    85,   -44,   -44,   -44,    76,   120,   121,
     101,    40,   113,    64,    96,   102,    25,   108,    84,   -44,
      88,   -44,   111,    58,    59,    58,    59,    25,   107,   116,
     117,     1,     2,     3,     4,     5,     6,     7,     8,     9,
      10,     0,    11,    12,    13,    14,     0,     0,   106,     1,
       2,     3,     4,     5,     6,     7,     8,     9,    10,     0,
      11,    12,    13,    14,    15,     0,   114,     1,     2,     3,
       4,     5,     6,     7,     8,     9,    10,     0,    11,    12,
      13,    14,    15,     1,     2,     3,     4,     5,     6,     7,
       8,     9,     0,     0,     0,     0,     0,    14,     0,   -43,
      15,     0,     0,   -43,   -43,   -43,     0,     0,     0,     0,
       0,     0,     0,     0,    68,     0,    15,     0,   -43,     0,
     -43,    46,    47,    48,    49,    50,    51,   118,    52,    53,
      54,    55,     0,     0,    46,    47,    48,    49,    50,    51,
     119,    52,    53,    54,    55,     0,     0,    46,    47,    48,
      49,    50,    51,     0,    52,    53,    54,    55,    46,    47,
      48,    49,    50,    51,     0,    52,    53,    54,    55,     0,
       0,     0,    56,    46,    47,    48,    49,    50,    51,     0,
      52,    53,    54,    55,     0,     0,     0,    67,    46,    47,
      48,    49,    50,    51,     0,    52,    53,    54,    55,     0,
      91,     0,    87,    46,    47,    48,    49,    50,    51,     0,
      52,    53,    54,    55,   105,     0,     0,    46,    47,    48,
      49,    50,    51,     0,    52,    53,    54,    55,    46,    47,
      48,    49,    50,    51,     0,    52,    53,    54,    55
};

static const yytype_int8 yycheck[] =
{
       0,    17,    23,    24,    23,    18,    14,    17,     3,     4,
       5,     6,     7,    13,    14,    17,    17,    17,    39,    18,
      32,    32,    17,    31,    37,    33,     8,     9,    10,    11,
      23,    24,    32,     3,     4,     5,     6,     7,    37,    23,
      24,    36,    42,    43,    44,    64,    39,    17,    34,    35,
      58,    59,    52,    53,    54,    55,    17,    57,    88,    18,
      21,     3,    74,    74,    23,    24,    36,    17,     0,    39,
      31,    21,    39,   103,     3,    75,    76,    85,   108,    87,
      18,    31,    39,    39,    22,    23,    24,    31,   118,   119,
      90,   107,   104,   104,    19,    31,    96,    13,    18,    37,
      18,    39,   102,    23,    24,    23,    24,   107,    96,   109,
     110,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    14,    15,    16,    17,    -1,    -1,    20,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      14,    15,    16,    17,    36,    -1,    20,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    14,    15,
      16,    17,    36,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    17,    -1,    18,
      36,    -1,    -1,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    36,    -1,    37,    -1,
      39,    25,    26,    27,    28,    29,    30,    18,    32,    33,
      34,    35,    -1,    -1,    25,    26,    27,    28,    29,    30,
      18,    32,    33,    34,    35,    -1,    -1,    25,    26,    27,
      28,    29,    30,    -1,    32,    33,    34,    35,    25,    26,
      27,    28,    29,    30,    -1,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    25,    26,    27,    28,    29,    30,    -1,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    25,    26,
      27,    28,    29,    30,    -1,    32,    33,    34,    35,    -1,
      22,    -1,    39,    25,    26,    27,    28,    29,    30,    -1,
      32,    33,    34,    35,    22,    -1,    -1,    25,    26,    27,
      28,    29,    30,    -1,    32,    33,    34,    35,    25,    26,
      27,    28,    29,    30,    -1,    32,    33,    34,    35
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    14,    15,    16,    17,    36,    42,    43,    44,    46,
      47,    49,    50,    51,    52,    53,    54,    57,    58,    59,
      60,    17,    17,    17,    39,    53,    53,    54,     3,     0,
      44,    39,    17,    21,    31,    49,    25,    26,    27,    28,
      29,    30,    32,    33,    34,    35,    39,    56,    23,    24,
      39,    53,    54,    46,    51,    53,    54,    39,    18,    18,
      53,    55,    53,    53,    17,    21,    31,    53,    53,    53,
      53,    53,    54,    54,    18,    39,    49,    39,    18,    18,
      37,    22,    46,    48,    53,    53,    19,    45,    54,    54,
      45,    53,    31,    18,    37,    22,    20,    43,    13,    39,
      39,    53,    45,    46,    20,    45,    53,    53,    18,    18,
      45,    45
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    41,    42,    43,    43,    44,    44,    44,    44,    44,
      44,    44,    45,    45,    46,    46,    47,    48,    48,    48,
      49,    50,    50,    50,    50,    51,    51,    51,    51,    52,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    54,    54,    54,    54,    55,    55,    55,
      56,    56,    56,    56,    56,    56,    57,    57,    58,    58,
      58,    59,    59,    60
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     2,     1,     2,     2,     1,     1,
       1,     2,     3,     2,     2,     4,     6,     0,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     3,     6,     4,     1,     1,     1,     3,     3,     3,
       3,     3,     4,     3,     3,     3,     3,     0,     1,     3,
       1,     1,     1,     1,     1,     1,     5,     7,     9,     9,
       5,     3,     2,     5
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;
      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: stmts  */
#line 62 "parser.y"
                { programBlock = (yyvsp[0].block); }
#line 1514 "parser.cpp"
    break;

  case 3: /* stmts: stmt  */
#line 65 "parser.y"
             { fprintf(stderr, "stmts->stmt\n"); (yyval.block) = new NBlock(); (yyval.block)->statements.push_back((yyvsp[0].stmt)); }
#line 1520 "parser.cpp"
    break;

  case 4: /* stmts: stmts stmt  */
#line 66 "parser.y"
                   { fprintf(stderr, "stmts->stmts stmt\n"); (yyvsp[-1].block)->statements.push_back((yyvsp[0].stmt)); }
#line 1526 "parser.cpp"
    break;

  case 5: /* stmt: func_decl  */
#line 69 "parser.y"
                 { fprintf(stderr, "stmt->func_decl\n"); }
#line 1532 "parser.cpp"
    break;

  case 6: /* stmt: var_decl TSEMICOLON  */
#line 70 "parser.y"
                           { fprintf(stderr, "stmt->var_decl PUNTO Y COMA\n"); }
#line 1538 "parser.cpp"
    break;

  case 7: /* stmt: expr TSEMICOLON  */
#line 71 "parser.y"
                       { fprintf(stderr, "stmt->expr PUNTO Y COMA\n"); (yyval.stmt) = new NExpressionStatement(*(yyvsp[-1].expr)); }
#line 1544 "parser.cpp"
    break;

  case 8: /* stmt: condition  */
#line 72 "parser.y"
                 { fprintf(stderr, "stmt->condición\n"); }
#line 1550 "parser.cpp"
    break;

  case 9: /* stmt: loop  */
#line 73 "parser.y"
            { fprintf(stderr, "stmt->bucle\n"); }
#line 1556 "parser.cpp"
    break;

  case 10: /* stmt: return  */
#line 74 "parser.y"
              { fprintf(stderr, "stmt->retorno\n"); }
#line 1562 "parser.cpp"
    break;

  case 11: /* stmt: array_decl TSEMICOLON  */
#line 75 "parser.y"
                             { fprintf(stderr, "stmt->array_decl PUNTO Y COMA\n"); }
#line 1568 "parser.cpp"
    break;

  case 12: /* block: TLBRACE stmts TRBRACE  */
#line 78 "parser.y"
                              { fprintf(stderr, "bloque->LLAVE IZQUIERDA stmts LLAVE DERECHA\n"); (yyval.block) = (yyvsp[-1].block); }
#line 1574 "parser.cpp"
    break;

  case 13: /* block: TLBRACE TRBRACE  */
#line 79 "parser.y"
                        { (yyval.block) = new NBlock(); }
#line 1580 "parser.cpp"
    break;

  case 14: /* var_decl: type ident  */
#line 82 "parser.y"
                      { fprintf(stderr, "var_decl->ident ident\n"); (yyval.stmt) = new NVariableDeclaration((yyvsp[-1].type), *(yyvsp[0].ident)); }
#line 1586 "parser.cpp"
    break;

  case 15: /* var_decl: type ident TEQUAL expr  */
#line 83 "parser.y"
                                  { fprintf(stderr, "var_decl->ident ident IGUAL expr\n"); (yyval.stmt) = new NVariableDeclaration((yyvsp[-3].type), *(yyvsp[-2].ident), (yyvsp[0].expr)); }
#line 1592 "parser.cpp"
    break;

  case 16: /* func_decl: type ident TLPAREN func_decl_args TRPAREN block  */
#line 87 "parser.y"
            { (yyval.stmt) = new NFunctionDeclaration((yyvsp[-5].type), *(yyvsp[-4].ident), *(yyvsp[-2].varvec), *(yyvsp[0].block)); delete (yyvsp[-2].varvec); }
#line 1598 "parser.cpp"
    break;

  case 17: /* func_decl_args: %empty  */
#line 90 "parser.y"
                            { (yyval.varvec) = new VariableList(); }
#line 1604 "parser.cpp"
    break;

  case 18: /* func_decl_args: var_decl  */
#line 91 "parser.y"
                     { fprintf(stderr, "func_decl_args->var_decl\n"); (yyval.varvec) = new VariableList(); (yyval.varvec)->push_back((yyvsp[0].var_decl)); }
#line 1610 "parser.cpp"
    break;

  case 19: /* func_decl_args: func_decl_args TCOMMA var_decl  */
#line 92 "parser.y"
                                           { (yyvsp[-2].varvec)->push_back((yyvsp[0].var_decl)); }
#line 1616 "parser.cpp"
    break;

  case 20: /* ident: TIDENTIFIER  */
#line 95 "parser.y"
                    { std::cerr<<"ident->"<<*(yyvsp[0].string)<<std::endl; (yyval.ident) = new NIdentifier(*(yyvsp[0].string)); delete (yyvsp[0].string); }
#line 1622 "parser.cpp"
    break;

  case 21: /* numeric: TINTEGER  */
#line 98 "parser.y"
                      { fprintf(stderr, "numérico->TINTEGER %s\n", (yyvsp[0].string)->c_str()); (yyval.expr) = new NInteger(atol((yyvsp[0].string)->c_str())); delete (yyvsp[0].string); }
#line 1628 "parser.cpp"
    break;

  case 22: /* numeric: TDOUBLE  */
#line 99 "parser.y"
                      { fprintf(stderr, "numérico->TDOUBLE %s\n", (yyvsp[0].string)->c_str()); (yyval.expr) = new NDouble(atof((yyvsp[0].string)->c_str())); delete (yyvsp[0].string); }
#line 1634 "parser.cpp"
    break;

  case 23: /* numeric: TSTRING  */
#line 100 "parser.y"
                      { fprintf(stderr, "numérico->TSTRING %s\n", (yyvsp[0].string)->c_str()); (yyval.expr) = new NString((yyvsp[0].string)->substr(1, (yyvsp[0].string)->length()-2)); delete (yyvsp[0].string); }
#line 1640 "parser.cpp"
    break;

  case 24: /* numeric: TCHAR  */
#line 101 "parser.y"
                      { fprintf(stderr, "numérico->TCHAR %s\n", (yyvsp[0].string)->c_str()); (yyval.expr) = new NChar((*(yyvsp[0].string))[1]); delete (yyvsp[0].string); }
#line 1646 "parser.cpp"
    break;

  case 25: /* type: TINTTYPE  */
#line 104 "parser.y"
                      { (yyval.type) = INT; }
#line 1652 "parser.cpp"
    break;

  case 26: /* type: TDOUBLETYPE  */
#line 105 "parser.y"
                      { (yyval.type) = DOUBLE; }
#line 1658 "parser.cpp"
    break;

  case 27: /* type: TCHARTYPE  */
#line 106 "parser.y"
                      { (yyval.type) = CHAR; }
#line 1664 "parser.cpp"
    break;

  case 28: /* type: TVOIDTYPE  */
#line 107 "parser.y"
                      { (yyval.type) = VOID; }
#line 1670 "parser.cpp"
    break;

  case 29: /* ptr: TAMPERSAND TIDENTIFIER  */
#line 110 "parser.y"
                             { fprintf(stderr, "ptr->AMPERSAND ident\n"); (yyval.ident) = new NIdentifier(*(yyvsp[0].string)); delete (yyvsp[0].string); }
#line 1676 "parser.cpp"
    break;

  case 30: /* expr: logic_expr  */
#line 112 "parser.y"
                  { fprintf(stderr, "expr->expr_lógica\n"); }
#line 1682 "parser.cpp"
    break;

  case 31: /* expr: ident TEQUAL expr  */
#line 113 "parser.y"
                         { fprintf(stderr, "expr->ident IGUAL expr\n"); (yyval.expr) = new NAssignment(*(yyvsp[-2].ident), *(yyvsp[0].expr)); }
#line 1688 "parser.cpp"
    break;

  case 32: /* expr: ident TLBRACK expr TRBRACK TEQUAL expr  */
#line 114 "parser.y"
                                              { fprintf(stderr, "array[expr] = expr\n"); (yyval.expr) = new NAssignment(*(yyvsp[-5].ident), *(yyvsp[0].expr), *(yyvsp[-3].expr)); }
#line 1694 "parser.cpp"
    break;

  case 33: /* expr: ident TLPAREN call_args TRPAREN  */
#line 115 "parser.y"
                                       { fprintf(stderr, "expr->ident PARENTESIS IZQUIERDO args_llamada PARENTESIS DERECHO\n"); (yyval.expr) = new NMethodCall(*(yyvsp[-3].ident), *(yyvsp[-1].exprvec)); delete (yyvsp[-1].exprvec); }
#line 1700 "parser.cpp"
    break;

  case 34: /* expr: ident  */
#line 116 "parser.y"
             { fprintf(stderr, "expr->ident\n"); (yyval.ident) = (yyvsp[0].ident);}
#line 1706 "parser.cpp"
    break;

  case 35: /* expr: ptr  */
#line 117 "parser.y"
           { fprintf(stderr, "expr->ptr\n"); (yyval.ident) = (yyvsp[0].ident); }
#line 1712 "parser.cpp"
    break;

  case 36: /* expr: numeric  */
#line 118 "parser.y"
               { fprintf(stderr, "expr->numérico\n"); }
#line 1718 "parser.cpp"
    break;

  case 37: /* expr: expr TPLUS expr  */
#line 119 "parser.y"
                       { fprintf(stderr, "expr->expr MÁS expr\n"); (yyval.expr) = new NBinaryOperator(*(yyvsp[-2].expr), (yyvsp[-1].token), *(yyvsp[0].expr)); }
#line 1724 "parser.cpp"
    break;

  case 38: /* expr: expr TMINUS expr  */
#line 120 "parser.y"
                        { fprintf(stderr, "expr->expr MENOS expr\n"); (yyval.expr) = new NBinaryOperator(*(yyvsp[-2].expr), (yyvsp[-1].token), *(yyvsp[0].expr)); }
#line 1730 "parser.cpp"
    break;

  case 39: /* expr: expr TMUL expr  */
#line 121 "parser.y"
                      { fprintf(stderr, "expr->expr POR expr\n"); (yyval.expr) = new NBinaryOperator(*(yyvsp[-2].expr), (yyvsp[-1].token), *(yyvsp[0].expr)); }
#line 1736 "parser.cpp"
    break;

  case 40: /* expr: expr TDIV expr  */
#line 122 "parser.y"
                      { fprintf(stderr, "expr->expr DIVIDIDO POR expr\n"); (yyval.expr) = new NBinaryOperator(*(yyvsp[-2].expr), (yyvsp[-1].token), *(yyvsp[0].expr)); }
#line 1742 "parser.cpp"
    break;

  case 41: /* expr: TLPAREN expr TRPAREN  */
#line 123 "parser.y"
                            { fprintf(stderr, "expr->PARENTESIS IZQUIERDO expr PARENTESIS DERECHO\n"); (yyval.expr) = (yyvsp[-1].expr); }
#line 1748 "parser.cpp"
    break;

  case 42: /* expr: ident TLBRACK expr TRBRACK  */
#line 124 "parser.y"
                                  { fprintf(stderr, "array ident[expr]\n"); (yyval.expr) = new NArrayVariable(*(yyvsp[-3].ident), *(yyvsp[-1].expr)); }
#line 1754 "parser.cpp"
    break;

  case 43: /* logic_expr: logic_expr TAND logic_expr  */
#line 127 "parser.y"
                                       { fprintf(stderr, "expr_lógica->expr_lógica Y expr_lógica\n"); (yyval.expr) = new NBinaryOperator(*(yyvsp[-2].expr), (yyvsp[-1].token), *(yyvsp[0].expr)); }
#line 1760 "parser.cpp"
    break;

  case 44: /* logic_expr: logic_expr TOR logic_expr  */
#line 128 "parser.y"
                                      { fprintf(stderr, "expr_lógica->expr_lógica O expr_lógica\n"); (yyval.expr) = new NBinaryOperator(*(yyvsp[-2].expr), (yyvsp[-1].token), *(yyvsp[0].expr)); }
#line 1766 "parser.cpp"
    break;

  case 45: /* logic_expr: expr comparison expr  */
#line 129 "parser.y"
                                 { fprintf(stderr, "expr_lógica->expr comparación expr\n"); (yyval.expr) = new NBinaryOperator(*(yyvsp[-2].expr), (yyvsp[-1].token), *(yyvsp[0].expr)); }
#line 1772 "parser.cpp"
    break;

  case 46: /* logic_expr: TLPAREN logic_expr TRPAREN  */
#line 130 "parser.y"
                                       { fprintf(stderr, "expr_lógica->PARENTESIS IZQUIERDO expr_lógica PARENTESIS DERECHO\n"); (yyval.expr) = (yyvsp[-1].expr); }
#line 1778 "parser.cpp"
    break;

  case 47: /* call_args: %empty  */
#line 133 "parser.y"
                       { (yyval.exprvec) = new ExpressionList(); }
#line 1784 "parser.cpp"
    break;

  case 48: /* call_args: expr  */
#line 134 "parser.y"
                 { fprintf(stderr, "args_llamada->expr\n"); (yyval.exprvec) = new ExpressionList(); (yyval.exprvec)->push_back((yyvsp[0].expr)); }
#line 1790 "parser.cpp"
    break;

  case 49: /* call_args: call_args TCOMMA expr  */
#line 135 "parser.y"
                                   { fprintf(stderr, "args_llamada->args_llamada COMA expr\n"); (yyvsp[-2].exprvec)->push_back((yyvsp[0].expr)); }
#line 1796 "parser.cpp"
    break;

  case 56: /* condition: TIF TLPAREN logic_expr TRPAREN block  */
#line 141 "parser.y"
                                                             { (yyval.stmt) = new NIfStatement(*(yyvsp[-2].expr), (yyvsp[0].block)); }
#line 1802 "parser.cpp"
    break;

  case 57: /* condition: TIF TLPAREN logic_expr TRPAREN block TELSE block  */
#line 142 "parser.y"
                                                            { (yyval.stmt) = new NIfStatement(*(yyvsp[-4].expr), (yyvsp[-2].block), (yyvsp[0].block)); }
#line 1808 "parser.cpp"
    break;

  case 58: /* loop: TFOR TLPAREN expr TSEMICOLON logic_expr TSEMICOLON expr TRPAREN block  */
#line 145 "parser.y"
                                                                             { fprintf(stderr, "bucle->TFOR PARENTESIS IZQUIERDO expr PUNTO Y COMA expr_lógica PUNTO Y COMA expr PARENTESIS DERECHO bloque\n"); (yyval.stmt) = new NForStatement((yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].block)); }
#line 1814 "parser.cpp"
    break;

  case 59: /* loop: TFOR TLPAREN var_decl TSEMICOLON logic_expr TSEMICOLON expr TRPAREN block  */
#line 146 "parser.y"
                                                                                 { fprintf(stderr, "bucle->TFOR PARENTESIS IZQUIERDO var_decl PUNTO Y COMA expr_lógica PUNTO Y COMA expr PARENTESIS DERECHO bloque\n"); (yyval.stmt) = new NForStatement((yyvsp[-6].stmt), (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].block)); }
#line 1820 "parser.cpp"
    break;

  case 60: /* loop: TWHILE TLPAREN logic_expr TRPAREN block  */
#line 147 "parser.y"
                                               { fprintf(stderr, "bucle->TWHILE PARENTESIS IZQUIERDO expr_lógica PARENTESIS DERECHO bloque\n"); (yyval.stmt) = new NWhileStatement((yyvsp[-2].expr), (yyvsp[0].block)); }
#line 1826 "parser.cpp"
    break;

  case 61: /* return: TRETURN expr TSEMICOLON  */
#line 151 "parser.y"
                                 { fprintf(stderr, "retorno->TRETURN expr PUNTO Y COMA\n"); (yyval.stmt) = new NReturnStatement((yyvsp[-1].expr)); }
#line 1832 "parser.cpp"
    break;

  case 62: /* return: TRETURN TSEMICOLON  */
#line 152 "parser.y"
                            { fprintf(stderr, "retorno->TRETURN PUNTO Y COMA\n"); (yyval.stmt) = new NReturnStatement(); }
#line 1838 "parser.cpp"
    break;

  case 63: /* array_decl: type ident TLBRACK expr TRBRACK  */
#line 156 "parser.y"
                                             { fprintf(stderr, "array_decl->type ident CORCHETE IZQUIERDO expr CORCHETE DERECHO\n"); (yyval.stmt) = new NArrayDeclaration((yyvsp[-4].type), *(yyvsp[-3].ident), (yyvsp[-1].expr)); }
#line 1844 "parser.cpp"
    break;


#line 1848 "parser.cpp"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 159 "parser.y"
