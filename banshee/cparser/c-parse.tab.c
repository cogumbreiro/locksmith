/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 58 "c-parse.y" /* yacc.c:339  */

#include <stdio.h>
#include <errno.h>
#include <setjmp.h>

#include "parser.h"
#include "c-parse.h"
#include "c-lex.h"
#include "semantics.h"
#include "input.h"
#include "expr.h"
#include "stmt.h"

int yyparse(void) deletes;

void yyerror();

/* Like YYERROR but do call yyerror.  */
#define YYERROR1 { yyerror ("syntax error"); YYERROR; }

/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1
#line 187 "c-parse.y" /* yacc.c:339  */

/* Region in which to allocate parse structures. Idea: the AST user can set
   this to different regions at appropriate junctures depending on what's
   being done with the AST */
region parse_region;
/* We'll see this a LOT below */
#define pr parse_region

/* Number of statements (loosely speaking) and compound statements 
   seen so far.  */
static int stmt_count;
static int compstmt_count;
  
/* List of types and structure classes of the current declaration.  */
static type_element current_declspecs = NULL;
static attribute prefix_attributes = NULL;

/* >0 if currently parsing an expression that will not be evaluated (argument
   to alignof, sizeof. Currently not typeof though that could be considered
   a bug) */
int unevaluated_expression;

#ifdef RC_ADJUST
static size_t rc_adjust_yystype(void *x, int by) 
{
  struct yystype *p = x;
  RC_ADJUST_PREAMBLE;

  RC_ADJUST(p->u.ptr, by);
  RC_ADJUST(p->idtoken.location.filename, by);
  RC_ADJUST(p->idtoken.id.data, by);
  RC_ADJUST(p->idtoken.decl, by);

  return sizeof *p;
}

static void rc_update_yystype(struct yystype *old, struct yystype *new)
{
  regionid base = regionidof(old);

  RC_UPDATE(base, old->u.ptr, new->u.ptr);
  RC_UPDATE(base, old->idtoken.location.filename, new->idtoken.location.filename);
  RC_UPDATE(base, old->idtoken.id.data, new->idtoken.id.data);
  RC_UPDATE(base, old->idtoken.decl, new->idtoken.decl);
}
#endif

/* A stack of declspecs and attributes for use during parsing */
typedef struct spec_stack *spec_stack;
struct spec_stack { 
  type_element parentptr declspecs;
  attribute parentptr attributes;
  spec_stack sameregion next;
};

/* Stack of saved values of current_declspecs and prefix_attributes.  */
/* In an ideal world, we would be able to eliminate most rc ops for
   declspec_stack and ds_region assignments. Seems tricky though. */
static spec_stack declspec_stack;
static region ds_region;

/* Pop top entry of declspec_stack back into current_declspecs,
   prefix_attributes */
static void pop_declspec_stack(void) deletes
{
  current_declspecs = declspec_stack->declspecs;
  prefix_attributes = declspec_stack->attributes;
  declspec_stack = declspec_stack->next;
  if (!declspec_stack)
    deleteregion_ptr(&ds_region);
}

static void push_declspec_stack(void)
{
  spec_stack news;

  if (!ds_region) ds_region = newsubregion(parse_region);
  news = ralloc(ds_region, struct spec_stack);
  news->declspecs = current_declspecs;
  news->attributes = prefix_attributes;
  news->next = declspec_stack;
  declspec_stack = news;
}

/* Tell yyparse how to print a token's value, if yydebug is set.  */

#define YYPRINT(FILE,YYCHAR,YYLVAL) yyprint(FILE,YYCHAR,YYLVAL)
void yyprint();

#line 179 "c-parse.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "c-parse.tab.h".  */
#ifndef YY_YY_C_PARSE_TAB_H_INCLUDED
# define YY_YY_C_PARSE_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    IDENTIFIER = 258,
    TYPENAME = 259,
    SCSPEC = 260,
    TYPESPEC = 261,
    TYPE_QUAL = 262,
    FN_QUAL = 263,
    CONSTANT = 264,
    STRING = 265,
    MAGIC_STRING = 266,
    ELLIPSIS = 267,
    SIZEOF = 268,
    ENUM = 269,
    STRUCT = 270,
    UNION = 271,
    IF = 272,
    ELSE = 273,
    WHILE = 274,
    DO = 275,
    FOR = 276,
    SWITCH = 277,
    CASE = 278,
    DEFAULT = 279,
    BREAK = 280,
    CONTINUE = 281,
    RETURN = 282,
    GOTO = 283,
    ASM_KEYWORD = 284,
    TYPEOF = 285,
    ALIGNOF = 286,
    ATTRIBUTE = 287,
    EXTENSION = 288,
    LABEL = 289,
    REALPART = 290,
    IMAGPART = 291,
    VA_ARG = 292,
    ASSIGN = 293,
    OROR = 294,
    ANDAND = 295,
    EQCOMPARE = 296,
    ARITHCOMPARE = 297,
    LSHIFT = 298,
    RSHIFT = 299,
    UNARY = 300,
    PLUSPLUS = 301,
    MINUSMINUS = 302,
    HYPERUNARY = 303,
    POINTSAT = 304
  };
#endif

/* Value type.  */


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_C_PARSE_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 275 "c-parse.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

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

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
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


#if ! defined yyoverflow || YYERROR_VERBOSE

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
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
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
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2671

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  72
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  148
/* YYNRULES -- Number of rules.  */
#define YYNRULES  390
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  673

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   304

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    65,     2,     2,     2,    55,    46,     2,
      62,    68,    53,    51,    70,    52,    61,    54,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    41,    67,
       2,    39,     2,    40,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    63,     2,    71,    45,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    66,    44,    69,    64,     2,     2,     2,
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
      35,    36,    37,    38,    42,    43,    47,    48,    49,    50,
      56,    57,    58,    59,    60
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   279,   279,   283,   294,   294,   295,   295,   300,   301,
     302,   307,   313,   321,   324,   327,   329,   333,   334,   335,
     343,   347,   342,   351,   355,   359,   354,   363,   367,   371,
     366,   375,   381,   382,   386,   390,   392,   394,   396,   398,
     400,   402,   404,   406,   408,   412,   421,   422,   426,   428,
     433,   434,   437,   440,   447,   452,   462,   465,   468,   474,
     478,   482,   483,   486,   485,   531,   532,   534,   536,   538,
     540,   542,   544,   546,   548,   550,   552,   554,   556,   558,
     560,   563,   562,   568,   570,   575,   581,   582,   583,   585,
     588,   587,   602,   604,   606,   608,   610,   613,   615,   621,
     624,   625,   630,   631,   638,   639,   640,   652,   653,   654,
     655,   663,   666,   669,   674,   684,   685,   686,   687,   695,
     705,   715,   718,   721,   724,   727,   731,   733,   743,   745,
     750,   751,   753,   758,   763,   765,   771,   772,   774,   787,
     788,   789,   791,   796,   797,   798,   800,   813,   815,   820,
     821,   829,   830,   831,   835,   837,   843,   844,   845,   849,
     850,   854,   855,   860,   861,   868,   867,   873,   881,   880,
     886,   893,   894,   899,   901,   906,   911,   913,   919,   920,
     922,   925,   929,   937,   938,   939,   940,   946,   947,   951,
     958,   961,   965,   966,   972,   973,   975,   979,   981,   983,
     985,   987,   993,  1000,   992,  1013,  1020,  1012,  1035,  1036,
    1042,  1044,  1046,  1048,  1050,  1057,  1059,  1068,  1070,  1072,
    1074,  1081,  1083,  1090,  1092,  1094,  1096,  1098,  1105,  1107,
    1111,  1116,  1115,  1121,  1125,  1128,  1127,  1131,  1135,  1138,
    1137,  1142,  1141,  1145,  1149,  1151,  1154,  1156,  1161,  1163,
    1169,  1170,  1172,  1188,  1191,  1197,  1200,  1206,  1208,  1214,
    1215,  1220,  1223,  1226,  1232,  1233,  1235,  1241,  1243,  1248,
    1250,  1256,  1257,  1261,  1262,  1268,  1269,  1274,  1277,  1279,
    1281,  1283,  1285,  1287,  1289,  1291,  1302,  1318,  1319,  1321,
    1326,  1327,  1330,  1334,  1340,  1341,  1348,  1349,  1353,  1360,
    1361,  1364,  1366,  1368,  1370,  1373,  1379,  1382,  1386,  1397,
    1396,  1408,  1410,  1415,  1417,  1423,  1425,  1429,  1428,  1436,
    1445,  1448,  1450,  1447,  1459,  1466,  1469,  1470,  1472,  1469,
    1479,  1478,  1487,  1492,  1497,  1501,  1505,  1510,  1515,  1519,
    1523,  1528,  1534,  1541,  1544,  1547,  1550,  1559,  1560,  1565,
    1566,  1572,  1573,  1577,  1578,  1583,  1590,  1592,  1599,  1599,
    1607,  1609,  1608,  1619,  1626,  1627,  1637,  1639,  1644,  1645,
    1652,  1656,  1660,  1664,  1668,  1678,  1678,  1685,  1686,  1691,
    1693,  1698,  1703,  1704,  1711,  1712,  1716,  1723,  1727,  1731,
    1735
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENTIFIER", "TYPENAME", "SCSPEC",
  "TYPESPEC", "TYPE_QUAL", "FN_QUAL", "CONSTANT", "STRING", "MAGIC_STRING",
  "ELLIPSIS", "SIZEOF", "ENUM", "STRUCT", "UNION", "IF", "ELSE", "WHILE",
  "DO", "FOR", "SWITCH", "CASE", "DEFAULT", "BREAK", "CONTINUE", "RETURN",
  "GOTO", "ASM_KEYWORD", "TYPEOF", "ALIGNOF", "ATTRIBUTE", "EXTENSION",
  "LABEL", "REALPART", "IMAGPART", "VA_ARG", "ASSIGN", "'='", "'?'", "':'",
  "OROR", "ANDAND", "'|'", "'^'", "'&'", "EQCOMPARE", "ARITHCOMPARE",
  "LSHIFT", "RSHIFT", "'+'", "'-'", "'*'", "'/'", "'%'", "UNARY",
  "PLUSPLUS", "MINUSMINUS", "HYPERUNARY", "POINTSAT", "'.'", "'('", "'['",
  "'~'", "'!'", "'{'", "';'", "')'", "'}'", "','", "']'", "$accept",
  "program", "extdefs", "@1", "@2", "extdef", "datadef", "fndef", "$@3",
  "$@4", "$@5", "$@6", "$@7", "$@8", "identifier", "id_label", "idword",
  "unop", "expr", "exprlist", "nonnull_exprlist", "unary_expr", "sizeof",
  "alignof", "cast_expr", "$@9", "expr_no_commas", "$@10", "primary",
  "$@11", "string", "string_list", "string_component",
  "old_style_parm_decls", "datadecls", "datadecl", "decls", "setspecs",
  "setattrs", "decl", "typed_declspecs", "reserved_declspecs",
  "typed_declspecs_no_prefix_attr", "reserved_declspecs_no_prefix_attr",
  "declmods", "declmods_no_prefix_attr", "typed_typespecs",
  "reserved_typespecquals", "typespec", "typespecqual_reserved",
  "initdecls", "notype_initdecls", "maybeasm", "initdcl", "@12",
  "notype_initdcl", "@13", "maybe_attribute", "attributes", "attribute",
  "attribute_list", "attrib", "any_word", "init", "initlist_maybe_comma",
  "initlist1", "initelt", "nested_function", "$@14", "$@15",
  "notype_nested_function", "$@16", "$@17", "declarator",
  "after_type_declarator", "parm_declarator", "notype_declarator", "tag",
  "structsp", "@18", "@19", "@20", "@21", "maybecomma", "maybecomma_warn",
  "component_decl_list", "component_decl_list2", "component_decl",
  "components", "component_declarator", "enumlist", "enumerator",
  "typename", "absdcl", "nonempty_type_quals", "type_quals", "absdcl1",
  "stmts", "stmt_or_labels", "xstmts", "errstmt", "pushlevel",
  "maybe_label_decls", "label_decls", "label_decl", "compstmt_or_error",
  "compstmt_start", "compstmt", "simple_if", "if_prefix", "do_stmt_start",
  "@22", "labeled_stmt", "stmt_or_label", "stmt", "$@23", "$@24", "@25",
  "$@26", "$@27", "@28", "@29", "label", "maybe_type_qual", "xexpr",
  "asm_operands", "nonnull_asm_operands", "asm_operand", "asm_clobbers",
  "parmlist", "$@30", "parmlist_1", "$@31", "parmlist_2", "parms", "parm",
  "parmlist_or_identifiers", "$@32", "parmlist_or_identifiers_1",
  "identifiers", "old_parameter", "identifiers_or_typenames", "fn_quals",
  "extension", "scspec", "type_qual", "fn_qual", "type_spec", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,    61,
      63,    58,   294,   295,   124,    94,    38,   296,   297,   298,
     299,    43,    45,    42,    47,    37,   300,   301,   302,   303,
     304,    46,    40,    91,   126,    33,   123,    59,    41,   125,
      44,    93
};
# endif

#define YYPACT_NINF -564

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-564)))

#define YYTABLE_NINF -365

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      76,    89,   127,  1596,  -564,  1596,    28,  -564,  -564,  -564,
    -564,    77,    83,    96,    88,   108,   114,  -564,  -564,  -564,
    -564,  -564,    60,  -564,   501,   226,  -564,   166,  -564,  -564,
    1596,  -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,
    -564,   152,  -564,   179,  -564,   186,  2260,  2203,   216,  -564,
    -564,    60,   153,  -564,   166,   662,   304,  -564,    60,   226,
    -564,   166,  -564,  -564,   540,  -564,  -564,   452,  -564,   224,
     374,  -564,   235,  -564,  -564,  -564,  -564,  -564,  -564,  -564,
    -564,  -564,   227,   188,  -564,  -564,  -564,  2260,  -564,  -564,
    1629,  -564,  -564,  2260,   255,   258,  -564,  2317,  2374,  -564,
    2616,  1142,  -564,   408,  -564,  2260,   264,   181,  -564,   270,
    1108,  -564,   731,   273,   279,  -564,   628,    60,  -564,   286,
    -564,  1098,  1011,   166,  -564,  -564,   628,  -564,   201,  -564,
     166,   701,   375,   414,   217,   682,   540,  -564,   166,  -564,
    -564,  -564,  -564,  -564,   338,   297,  -564,   452,   166,  -564,
    -564,  -564,   316,  1703,  1223,  -564,   166,  -564,  2260,  -564,
    -564,  -564,   319,   335,   347,   356,  -564,   373,  2260,  1629,
    -564,  1629,  -564,  2260,  2260,  2260,  2260,  2260,  2260,  2260,
    2260,  2260,  2260,  2260,  2260,  2260,  2260,  2260,  2260,  2260,
    -564,  -564,   188,   188,  2260,  2260,  -564,  -564,  -564,  -564,
     181,  1155,  -564,   425,   568,  -564,  -564,  -564,  -564,  -564,
    -564,   167,  -564,   369,  -564,  -564,  -564,   414,  -564,  -564,
    -564,   419,   414,   408,   450,   839,  -564,   391,   378,  -564,
    1601,   956,  -564,   852,  -564,  -564,   433,   166,   262,   311,
    -564,   628,   628,  -564,  1011,   166,  -564,  1212,  -564,  -564,
    1011,  2260,   188,   405,   297,  -564,   308,  -564,   308,  -564,
     424,  -564,   432,  2551,  -564,  -564,  -564,  -564,   417,  2076,
    -564,  2616,   435,   444,  2616,  2616,   473,   477,  1798,  1986,
    2045,  1001,  2110,  1602,  1569,  1020,  1020,   416,   416,  -564,
    -564,  -564,  -564,  -564,   456,   258,   468,   134,   354,   450,
     537,  -564,   486,  -564,  1269,  -564,   568,   491,   731,  2431,
     493,  -564,  -564,  -564,   494,  -564,  -564,  -564,  1338,  -564,
     496,   239,  -564,  -564,   248,  -564,  -564,  -564,    57,  -564,
    -564,  -564,   359,  -564,    60,  -564,   749,  -564,   375,  -564,
    -564,   375,  -564,   532,   450,  -564,   505,  -564,  2616,  -564,
     166,   510,  2260,    52,   516,  -564,   516,   166,   166,  1703,
      34,  -564,  -564,  -564,  -564,  -564,  2260,  2260,  -564,  -564,
     425,  -564,  -564,  -564,  -564,   450,  -564,   509,  -564,  -564,
     284,   520,  -564,  -564,   447,   563,  -564,  -564,  1281,  -564,
     590,   347,  -564,  -564,  -564,   260,   276,   165,   749,  -564,
    -564,  1827,    57,  -564,  -564,  -564,    57,  -564,   166,  2580,
    2260,  -564,   308,  -564,  -564,   526,   188,  -564,  1395,   569,
    -564,   942,  1733,  1733,  -564,  -564,  -564,  2260,  -564,  -564,
    -564,   563,   166,   140,   271,   166,  -564,   271,   166,   537,
    -564,  -564,  -564,  -564,  -564,  -564,  -564,   942,  2616,  -564,
    -564,  1827,  -564,  -564,  -564,  2580,  -564,  -564,  -564,   302,
     328,   556,   561,   545,  -564,  -564,   547,   550,  2260,   572,
     552,   554,  2016,   196,   607,  -564,   581,   557,  1891,   769,
    -564,   585,   558,   875,  -564,  -564,   619,  1462,    43,  -564,
    -564,  -564,  2140,  -564,  -564,   188,  2260,   942,   615,  2616,
     592,   589,  -564,   357,  1025,   755,  -564,  1326,  -564,  -564,
    -564,   176,  -564,  -564,  -564,   593,  -564,  -564,  -564,   188,
    -564,  2260,   603,  1956,  2260,  2260,  2503,  -564,  -564,  -564,
    -564,   605,  2260,   606,   608,  -564,  -564,  -564,  -564,  -564,
     610,  -564,   382,  -564,    60,  -564,  -564,  -564,  1529,  -564,
    -564,  -564,  1956,  -564,  2260,  -564,   635,  2459,   611,  1761,
    -564,  1033,  -564,  -564,   466,  -564,   466,   450,  -564,   613,
    -564,  -564,   614,  2260,   666,  -564,   626,   627,  2260,  -564,
    -564,   632,  -564,  2260,  -564,  -564,   329,  -564,   228,   341,
    -564,  1113,  -564,  1956,  -564,   636,  1761,  2260,  1695,  -564,
    -564,  -564,   977,  -564,  -564,  -564,   641,  -564,  -564,  -564,
    2598,  -564,    49,  -564,  1011,  -564,  1011,  -564,   633,  -564,
    2521,  1761,  -564,  -564,  2260,  1956,  -564,   693,   643,  -564,
    -564,  -564,   679,  -564,  1956,   652,  -564,   658,   148,   653,
    -564,  -564,   347,   347,  1761,  -564,  -564,  2260,   693,   655,
     693,  -564,  -564,  -564,  2260,   659,   149,  -564,  -564,   671,
    -564,   408,   674,  -564,  -564,   364,  -564,  1956,   675,   408,
    -564,  -564,  -564
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       4,     0,     6,     0,     1,     0,     0,   153,   387,   390,
     388,     0,     0,     0,     0,     0,     0,   386,    19,     5,
       9,     8,     0,   119,   119,   139,   130,   140,   173,   152,
       0,   144,   143,   151,     7,    17,    18,    32,    33,   241,
     230,   243,   250,   234,   250,   238,     0,     0,     0,   229,
     275,     0,     0,   161,   120,     0,     0,    15,     0,   141,
     130,   142,   146,   145,   128,   174,    11,     0,   239,     0,
       0,   231,     0,   235,    85,    86,   102,   103,    59,    60,
      43,    44,     0,     0,    36,    38,    37,     0,    39,    40,
       0,    41,    42,     0,     0,    45,    61,     0,     0,    65,
      48,    50,    87,    99,   100,     0,     0,   271,   149,     0,
     271,   273,   178,     0,     0,    12,     0,     0,    31,     0,
     375,     0,     0,   171,   216,   275,     0,    16,     0,   159,
     120,     0,   208,   209,     0,     0,   129,   131,   133,   158,
     132,   157,   156,   266,   267,   246,   264,     0,   171,   257,
     252,   119,   249,   119,     0,   250,   171,   250,     0,    34,
      54,    51,     0,     0,     0,     0,    53,     0,     0,     0,
      55,     0,    57,     0,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      97,    98,     0,     0,    46,     0,   101,    52,   154,   275,
     358,     0,   269,   272,   147,   155,   149,   270,   274,    35,
     183,     0,   176,   179,   184,   186,   185,   225,   276,   224,
     162,   163,   228,     0,   384,     0,   227,     0,     0,    29,
     105,     0,   119,   119,   136,   108,   170,   172,     0,     0,
      14,     0,     0,    23,     0,   171,   375,     0,    13,    27,
       0,     0,   247,     0,   246,   233,   254,   251,   256,   258,
       0,   237,     0,     0,    89,    88,   301,   293,     0,     0,
      10,    49,     0,     0,    84,    83,     0,     0,    79,    78,
      76,    77,    75,    74,    73,    71,    72,    66,    67,    68,
      69,    70,    96,    95,     0,    47,     0,   279,     0,   384,
       0,   285,     0,   358,     0,   150,   148,     0,   178,    46,
       0,   389,   223,   385,     0,   381,   365,   119,   119,   377,
       0,   366,   368,   376,     0,   379,   226,   292,     0,   106,
     109,   110,     0,   114,     0,   136,   134,   168,   214,   210,
     160,   215,    21,   167,   384,   213,     0,    25,   268,   265,
     171,     0,     0,   171,   253,   259,   255,   171,   171,     0,
     294,    91,    63,    62,    56,    58,     0,     0,    92,    94,
     278,   277,   283,   359,   284,   384,   282,     0,   175,   177,
      85,     0,   164,   363,   271,   271,   360,   361,     0,   378,
       0,     0,    30,   299,   113,     0,     0,   163,   135,   137,
     138,     0,     0,   165,   211,   212,     0,   242,   171,   171,
       0,   261,     0,   232,   236,     0,     0,   302,     0,   295,
     296,     0,    80,    82,   280,   281,   180,     0,   182,   222,
     275,   358,   120,   171,   171,   171,   275,   171,   171,     0,
     367,   369,   380,   300,   111,   112,   189,     0,   187,   169,
      22,     0,    26,   240,   263,   171,   260,    93,   382,     0,
       0,    85,   153,     0,   321,   309,     0,     0,     0,     0,
       0,     0,     0,     0,   347,   342,     0,     0,   290,     0,
     119,   119,     0,     0,   116,   315,   319,     0,     0,   287,
     313,   314,     0,   297,   196,     0,     0,     0,     0,   194,
       0,   244,   192,     0,   279,     0,   375,     0,   370,   371,
     372,   279,   373,   374,   362,     0,   166,   262,   298,     0,
     304,     0,     0,     0,   349,     0,     0,   345,   332,   333,
     334,     0,     0,     0,     0,   348,   346,   316,   117,   291,
       0,   118,     0,   126,     0,   305,   289,   288,     0,   307,
     306,   311,     0,   325,     0,   127,     0,     0,     0,     0,
      64,     0,   191,   181,   220,   275,   221,   384,   219,     0,
     188,   383,     0,     0,     0,   350,     0,     0,     0,   343,
     335,     0,   340,     0,   303,   125,     0,   123,   202,     0,
     124,   205,   320,     0,   312,     0,     0,     0,     0,   195,
     200,   193,     0,   217,   218,   308,     0,   310,   326,   330,
       0,   341,     0,   121,     0,   122,     0,   318,     0,   201,
       0,     0,   199,   322,   349,     0,   344,   351,     0,   203,
     206,   324,     0,   198,     0,     0,   331,     0,     0,   352,
     353,   336,     0,     0,     0,   323,   327,     0,   351,     0,
       0,   204,   207,   197,   349,     0,     0,   337,   354,     0,
     355,     0,     0,   328,   356,     0,   338,     0,     0,     0,
     329,   339,   357
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -564,  -564,  -564,  -564,  -564,   101,  -564,  -564,  -564,  -564,
    -564,  -564,  -564,  -564,   -11,   -58,   -93,  -564,   -43,   434,
    -142,   437,  -564,  -564,   -64,  -564,   462,  -564,  -564,  -564,
    -202,  -564,   623,  -222,  -564,   517,  -564,    42,  -110,  -357,
       2,   686,  -564,   415,    12,   -18,   -10,   551,    -8,  -150,
    -286,   -46,  -113,   -76,  -564,  -564,  -564,   220,    21,    10,
    -564,   443,  -564,   309,  -283,  -564,  -443,  -564,  -564,  -564,
    -564,  -564,  -564,   -47,   -69,  -331,    16,   524,   -53,  -564,
    -564,  -564,  -564,  -564,   499,    11,  -564,   612,   503,   365,
     622,   529,   -60,   -83,     0,  -111,  -167,   325,  -564,  -564,
    -197,  -564,  -564,  -564,   388,  -196,  -564,  -151,  -564,  -564,
    -564,  -564,  -454,   326,  -368,  -564,  -564,  -564,  -564,  -564,
    -564,  -564,  -359,  -564,  -563,   162,  -564,   161,  -564,   513,
    -564,  -264,  -564,  -564,  -564,   430,  -215,  -564,  -564,  -564,
     423,  -564,  -250,     5,    48,   160,  -564,   -16
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     3,     5,    19,    20,    21,   244,   402,
     250,   406,   122,   328,   159,   476,   498,    93,   477,   294,
      95,    96,    97,    98,    99,   421,   100,   277,   101,   164,
     102,   103,   104,   229,   230,   231,   478,    22,   117,   479,
     317,    64,   232,   336,   318,    25,   107,   204,    26,   137,
     128,    52,   123,   129,   451,    53,   401,   236,   237,    28,
     211,   212,   213,   449,   500,   501,   502,   587,   614,   642,
     590,   616,   643,   221,   132,   564,   133,    41,    29,   155,
     157,   147,    67,   562,   253,    69,    70,   152,   354,   355,
     145,   146,   109,   202,   110,   113,   203,   482,   483,   540,
     235,   360,   418,   419,   420,   392,   267,   485,   486,   487,
     488,   523,   550,   489,   551,   593,   522,   634,   624,   654,
     667,   625,   552,   534,   576,   638,   639,   640,   665,   299,
     300,   319,   439,   320,   321,   322,   224,   225,   323,   324,
     325,   459,   312,   105,    31,    32,   313,    33
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      40,    40,    40,    94,   106,    23,    59,    23,    30,   131,
      30,   139,   134,   268,   238,    24,    60,    24,   245,   210,
     242,   310,   342,   161,    27,   160,    27,   207,   347,   166,
     165,   344,    23,   298,   331,    30,   373,    65,    55,   108,
     220,   197,    24,    54,   553,    61,   395,   163,   142,   372,
     490,    27,   295,   433,   305,    72,   144,   239,   391,   491,
     151,   635,   108,    49,    65,    56,    58,   114,   416,   574,
     153,    65,    54,    62,   135,   154,    -2,   130,   227,    54,
      37,    38,   108,   139,    16,   138,    37,    38,   297,     4,
     627,   659,    16,   410,   404,    35,   216,    36,   594,    37,
      38,   209,   206,   417,   233,   554,    34,    62,   245,   272,
     490,   273,   140,    50,   234,   490,   600,   628,   601,   491,
     142,   538,    51,   266,   491,   424,   163,    -3,   163,   217,
     370,    66,   276,   222,    54,   555,   144,   130,    54,   617,
      65,    10,   114,    39,   151,   206,   108,   130,    65,    42,
      46,   139,   296,   619,   153,   622,   305,   138,   302,   154,
     214,   108,    44,   108,   515,   340,   260,   295,   262,   338,
      47,   636,    16,   341,   566,   514,    48,   393,   633,    49,
     645,   292,   293,    10,   140,    63,   399,   199,   142,   648,
     661,    37,    38,   256,   119,   258,   200,   201,    16,    37,
      38,   653,   506,   507,   346,   363,   450,   111,    16,   353,
     452,   353,   233,   670,   558,   210,   649,   662,    68,    63,
     115,   484,   234,   116,   141,   335,   233,   120,   121,   436,
     111,     8,   233,    10,   199,   307,   234,   308,   431,   201,
     443,   144,   234,   200,   201,    71,    27,    65,   399,   532,
     111,   393,    73,   139,   217,   393,   586,   119,   222,   130,
    -163,   377,   130,   130,   298,    49,   124,  -163,   240,    10,
     208,   241,   215,   218,   332,   334,    49,   130,   112,   130,
      10,    62,   541,   139,   248,   503,   546,   116,   396,   158,
     142,   567,   216,   148,    16,  -163,   141,   209,  -163,   415,
      59,   435,   438,    16,   156,    16,   387,    49,   124,   388,
      60,    49,   124,   208,   111,   125,   389,   603,   390,   504,
     142,    27,   505,   167,   126,   511,    50,   444,   168,   111,
     241,   111,   198,   120,   121,    51,    16,   370,   205,    61,
      16,   120,   121,   445,   370,   139,   116,   219,   223,   352,
     397,   108,   426,   130,   427,    54,   214,   125,   458,   384,
     385,   125,    49,   124,   141,   353,   126,   252,   255,   518,
     126,   127,   519,   246,   247,   149,   261,   251,     7,   339,
       9,    10,   142,   257,   400,    49,   124,   264,    11,    12,
      13,    16,   629,    63,   630,   327,   613,   520,   218,   241,
     434,   437,   556,   265,    15,   432,    54,    17,   615,    27,
     209,   116,   125,   266,    16,   533,   303,   304,    76,    77,
     480,   126,   371,   492,   269,   563,   394,   168,   197,   531,
     481,   309,   668,   130,   669,   125,   209,   246,   247,    27,
     270,   150,    65,  -248,   126,   327,   400,   114,   119,   585,
      49,   429,    54,   143,   602,    37,    38,   218,   311,   664,
      27,   571,   326,    59,   569,   343,   141,   672,   215,   187,
     188,   189,   337,    60,   350,   245,   120,   121,   572,    16,
     480,   575,   577,   492,   209,   361,   209,   303,   304,   581,
     481,   651,   652,   357,   480,   588,   141,   492,   589,    27,
     430,   358,    61,   364,   481,     7,     8,     9,    10,   431,
     201,   595,   365,    27,   366,    11,    12,    13,   367,   111,
     217,   222,   542,   544,   368,   432,   432,   217,   506,   507,
     606,    15,    54,    16,   170,   172,    43,    45,   314,   369,
     612,     7,     8,     9,    10,     8,     9,    10,   209,   316,
     209,    11,    12,    13,    11,    12,    13,   374,   141,   378,
     591,   382,   383,   130,   386,    54,    49,    15,    57,    16,
     407,   403,    16,   411,     9,    10,   405,   413,   414,   408,
     425,   575,    11,    12,    13,   209,   412,   209,   428,     7,
       8,     9,    10,   315,   457,    16,   233,   -32,   233,    11,
      12,    13,   -33,   416,   655,  -364,   234,   521,   234,   524,
     209,   575,   525,   527,    10,    15,   436,    16,   217,   528,
     263,   529,   536,   432,   537,   431,   201,   545,   453,   454,
     271,    49,   124,   209,   535,   274,   275,   548,   278,   279,
     280,   281,   282,   283,   284,   285,   286,   287,   288,   289,
     290,   291,   543,   508,   509,   510,   559,   512,   513,   561,
      16,   560,   570,   118,   218,   573,   -28,   -28,   -28,   -28,
     583,   218,   580,   582,   596,   517,   -28,   -28,   -28,   584,
     599,   125,   605,   249,   604,   607,   -24,   -24,   -24,   -24,
     126,   119,   -28,   608,  -163,   609,   -24,   -24,   -24,   611,
     631,  -163,   243,   637,   618,   -20,   -20,   -20,   -20,   623,
     641,   119,   -24,   348,  -163,   -20,   -20,   -20,   644,   646,
     647,  -163,   657,   650,   120,   121,   196,   660,   -28,  -163,
     119,   -20,  -163,  -163,    37,    38,     8,     9,    10,   663,
    -163,   666,   671,   381,   120,   121,   136,   330,   -24,  -163,
     398,   379,  -163,   351,     8,     9,    10,   306,    49,   429,
     516,   356,   218,    11,    12,    13,   259,   -20,  -163,   254,
     228,  -163,  -115,  -115,  -115,  -115,  -115,   456,  -115,  -115,
    -115,   349,  -115,  -115,  -115,  -115,  -115,    16,  -115,  -115,
    -115,  -115,  -115,  -115,  -115,  -115,  -115,  -115,  -115,  -115,
    -115,  -115,  -115,   539,  -115,  -115,  -115,   493,   565,   547,
     656,   658,  -115,   442,   409,  -115,   375,    51,   441,     0,
    -115,  -115,  -115,     0,     0,     0,  -115,  -115,   422,   423,
       0,  -115,     0,  -115,  -115,  -115,  -115,     0,  -115,     0,
     314,     0,   315,     7,     8,     9,    10,     0,     0,     0,
       0,   316,     0,    11,    12,    13,     7,     8,     9,    10,
       0,     0,     0,   448,     0,     0,    11,    12,    13,    15,
       0,    16,   455,     0,     0,     0,   228,     0,   461,    38,
       0,     0,    15,   499,    75,    76,    77,     0,    78,     0,
       0,     0,   463,     0,   464,   465,   466,   467,   468,   469,
     470,   471,   472,   473,   474,     0,    79,  -364,    17,   499,
      80,    81,    82,   448,     0,     0,     0,     0,    83,   333,
       0,    84,     0,     0,     0,     0,    85,    86,    87,     0,
     526,     0,    88,    89,     0,     0,     0,    90,     0,    91,
      92,   266,   475,   494,  -286,   461,    38,     0,     0,     0,
       0,    75,    76,    77,     0,    78,     0,   228,   557,   499,
    -107,  -107,  -107,  -107,     0,     0,     0,     0,  -107,     0,
    -107,  -107,  -107,    79,     0,    17,     0,    80,    81,    82,
      49,   429,     0,     0,    10,    83,  -107,     0,    84,     0,
       0,     0,     0,    85,    86,    87,     0,     0,     0,    88,
      89,     0,     0,   495,    90,   496,    91,    92,   497,    16,
       0,  -190,   228,     0,     0,     7,     8,     9,    10,     0,
       0,   499,  -107,   499,     0,    11,    12,    13,    49,   429,
     565,     0,    10,     0,   494,     0,   461,    38,     0,    51,
     610,    15,    75,    76,    77,     0,    78,   180,   181,   182,
     183,   184,   185,   186,   187,   188,   189,    16,   499,   620,
     499,     0,     0,     0,    79,     0,    17,     0,    80,    81,
      82,   185,   186,   187,   188,   189,    83,  -104,   430,    84,
       0,     0,     0,   499,    85,    86,    87,   431,   201,     0,
      88,    89,     0,     0,   495,    90,   496,    91,    92,   497,
       0,    74,  -245,     0,     0,     0,   499,    75,    76,    77,
       0,    78,     7,     0,     9,    10,     0,     0,     0,     0,
       0,     0,    11,    12,    13,     0,     0,     0,     0,    79,
       0,    17,     0,    80,    81,    82,     0,     0,    15,     0,
       0,    83,   119,     0,    84,  -163,     0,     0,     0,    85,
      86,    87,  -163,     0,     0,    88,    89,     0,    74,     0,
      90,   199,    91,    92,    75,    76,    77,     0,    78,   226,
     200,   201,     0,     0,     0,   120,   121,     0,     0,     0,
    -163,     0,     0,  -163,     0,     0,    79,     0,    17,     0,
      80,    81,    82,     0,     0,     0,     0,     0,    83,   190,
     191,    84,   192,   193,   194,   195,    85,    86,    87,     0,
       0,     0,    88,    89,     0,    74,     0,    90,     0,    91,
      92,    75,    76,    77,   149,    78,   301,     7,     0,     9,
      10,     0,     0,     0,     0,     0,     0,    11,    12,    13,
       0,     0,     0,    79,     0,    17,     0,    80,    81,    82,
       0,     0,     0,    15,     0,    83,    17,     0,    84,     0,
       0,     0,     0,    85,    86,    87,     0,     0,     0,    88,
      89,     0,    74,     0,    90,     0,    91,    92,    75,    76,
      77,     0,    78,   345,     0,     7,     8,     9,    10,     0,
       0,     0,     0,   440,     0,    11,    12,    13,     0,     0,
      79,     0,    17,     0,    80,    81,    82,     0,     0,     0,
       0,    15,    83,    16,     0,    84,     0,     0,     0,     0,
      85,    86,    87,     0,     0,     0,    88,    89,     0,    74,
       0,    90,     0,    91,    92,    75,    76,    77,     0,    78,
     376,     0,     7,     8,     9,    10,     0,     0,     0,     0,
       0,     0,    11,    12,    13,     0,     0,    79,     0,    17,
       0,    80,    81,    82,     0,     0,     0,     0,    15,    83,
      16,     0,    84,     0,     0,     0,     0,    85,    86,    87,
       0,     0,     0,    88,    89,     0,     0,     0,    90,     0,
      91,    92,     0,     0,     0,     0,   460,   568,   461,   462,
       8,     9,    10,     0,    75,    76,    77,     0,    78,    11,
      12,    13,   463,     0,   464,   465,   466,   467,   468,   469,
     470,   471,   472,   473,   474,    15,    79,    16,    17,     0,
      80,    81,    82,     0,     0,     0,     0,     0,    83,     0,
       0,    84,     0,     0,     0,     0,    85,    86,    87,     0,
       0,     0,    88,    89,     0,     0,     0,    90,     0,    91,
      92,   266,   475,   549,     0,   461,    38,     0,     0,     0,
       0,    75,    76,    77,     0,    78,     0,     0,     0,   463,
       0,   464,   465,   466,   467,   468,   469,   470,   471,   472,
     473,   474,     0,    79,     0,    17,     0,    80,    81,    82,
       0,     0,     0,     0,     0,    83,     0,     0,    84,     0,
       0,     0,     0,    85,    86,    87,     0,     0,     0,    88,
      89,     0,     0,     0,    90,     0,    91,    92,   266,   475,
     592,     0,  -317,  -317,     0,     0,     0,     0,  -317,  -317,
    -317,     0,  -317,     0,     0,     0,  -317,     0,  -317,  -317,
    -317,  -317,  -317,  -317,  -317,  -317,  -317,  -317,  -317,     0,
    -317,     0,  -317,     0,  -317,  -317,  -317,     0,     0,     0,
       0,     0,  -317,     0,     0,  -317,     0,     0,     0,     0,
    -317,  -317,  -317,     0,     0,     0,  -317,  -317,     0,     0,
       0,  -317,     0,  -317,  -317,  -317,  -317,     6,     0,  -119,
       7,     8,     9,    10,     0,     7,     8,     9,    10,     0,
      11,    12,    13,   329,     0,    11,    12,    13,   183,   184,
     185,   186,   187,   188,   189,    14,    15,     0,    16,    17,
     162,    15,    74,     7,     0,     9,    10,     0,    75,    76,
      77,     0,    78,    11,    12,    13,     0,     0,     0,  -119,
     182,   183,   184,   185,   186,   187,   188,   189,  -119,    15,
      79,     0,    17,    18,    80,    81,    82,     0,     0,     0,
       0,     0,    83,     0,     0,    84,     0,     0,     0,     0,
      85,    86,    87,     0,     0,     0,    88,    89,     0,     0,
       0,    90,     0,    91,    92,   -90,   494,     0,   461,    38,
       0,     0,     0,     0,    75,    76,    77,     7,    78,     9,
      10,     0,     0,     0,     0,     0,     0,    11,    12,    13,
       0,     0,     0,     0,     0,     0,    79,     0,    17,     0,
      80,    81,    82,    15,   621,     0,     0,     0,    83,     0,
       0,    84,     0,     0,     0,     0,    85,    86,    87,     0,
       0,     0,    88,    89,     0,     0,   495,    90,   496,    91,
      92,   497,   494,     0,   461,    38,     0,     0,     0,     0,
      75,    76,    77,   175,    78,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   187,   188,   189,     0,
       0,     0,    79,     0,    17,     0,    80,    81,    82,     0,
       0,     0,     0,     0,    83,     0,     0,    84,     0,     0,
       0,     0,    85,    86,    87,     0,     0,     0,    88,    89,
       0,     0,   495,    90,   496,    91,    92,   497,   446,     0,
      74,     0,     0,     0,     0,     0,    75,    76,    77,     0,
      78,   177,   178,   179,   180,   181,   182,   183,   184,   185,
     186,   187,   188,   189,     0,     0,     0,     0,    79,     0,
      17,     0,    80,    81,    82,     0,     0,     0,     0,     0,
      83,     0,     0,    84,     0,     0,     0,     0,    85,    86,
      87,     0,     0,     0,    88,    89,     0,     0,     0,    90,
       0,    91,    92,   447,   461,   462,     8,     9,    10,     0,
      75,    76,    77,     0,    78,    11,    12,    13,   463,     0,
     464,   465,   466,   467,   468,   469,   470,   471,   472,   473,
     474,    15,    79,    16,    17,     0,    80,    81,    82,     0,
       0,     0,     0,     0,    83,     0,     0,    84,     0,     0,
       0,     0,    85,    86,    87,     0,     0,     0,    88,    89,
       0,     0,     0,    90,     0,    91,    92,   266,   475,   461,
      38,     0,     0,     0,     0,    75,    76,    77,     0,    78,
       0,     0,     0,   463,     0,   464,   465,   466,   467,   468,
     469,   470,   471,   472,   473,   474,     0,    79,     0,    17,
       0,    80,    81,    82,     0,     0,     0,     0,     0,    83,
       0,     0,    84,     0,     0,     0,     0,    85,    86,    87,
       0,     0,     0,    88,    89,     0,     0,     0,    90,    74,
      91,    92,   266,   475,     0,    75,    76,    77,     0,    78,
     178,   179,   180,   181,   182,   183,   184,   185,   186,   187,
     188,   189,     0,     0,     0,     0,     0,    79,     0,    17,
       0,    80,    81,    82,     0,     0,     0,     0,     0,    83,
       0,     0,    84,     0,     0,     0,     0,    85,    86,    87,
       0,     0,     0,    88,    89,     0,     0,     0,    90,    74,
      91,    92,     0,   530,     0,    75,    76,    77,     0,    78,
     179,   180,   181,   182,   183,   184,   185,   186,   187,   188,
     189,     0,     0,     0,     0,     0,     0,    79,     0,    17,
       0,    80,    81,    82,     0,     0,     0,     0,     0,    83,
       0,     0,    84,     0,     0,     0,     0,    85,    86,    87,
       0,     0,     0,    88,    89,     0,     0,     0,    90,     0,
      91,    92,   362,    74,     7,     8,     9,    10,     0,    75,
      76,    77,     0,    78,    11,    12,    13,   181,   182,   183,
     184,   185,   186,   187,   188,   189,     0,     0,     0,     0,
      15,    79,    16,    17,     0,    80,    81,    82,     0,     0,
       0,     0,     0,    83,     0,     0,    84,     0,     0,     0,
       0,    85,    86,    87,     0,     0,     0,    88,    89,     0,
       0,     0,    90,     0,    91,    92,    74,     7,     0,     9,
      10,     0,    75,    76,    77,     0,    78,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    15,    79,     0,    17,     0,    80,    81,
      82,     0,     0,     0,     0,     0,    83,     0,     0,    84,
       0,     0,     0,     0,    85,    86,    87,     0,     0,     0,
      88,    89,     0,    74,     0,    90,     0,    91,    92,    75,
      76,    77,     0,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    79,     0,    17,     0,    80,    81,    82,     0,     0,
       0,     0,     0,    83,     0,     0,    84,     0,     0,     0,
       0,    85,    86,    87,     0,     0,     0,    88,    89,     0,
      74,     0,    90,     0,    91,    92,    75,    76,    77,     0,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    79,     0,
      17,     0,    80,    81,    82,     0,     0,     0,     0,     0,
      83,     0,     0,    84,     0,     0,     0,     0,    85,    86,
      87,     0,     0,     0,    88,    89,     0,    74,     0,   169,
       0,    91,    92,    75,    76,    77,     0,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    79,     0,    17,     0,    80,
      81,    82,     0,     0,     0,     0,     0,    83,     0,     0,
      84,     0,     0,     0,     0,    85,    86,    87,     0,     0,
       0,    88,    89,     0,   380,     0,   171,     0,    91,    92,
      75,    76,    77,     0,    78,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    79,     0,    17,     0,    80,    81,    82,     0,
       0,   597,     0,     0,    83,     0,     0,    84,     0,     0,
       0,     0,    85,    86,    87,     0,     0,     0,    88,    89,
       0,     0,     0,    90,     0,    91,    92,   173,   174,   175,
       0,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   578,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     598,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   173,   174,   175,   579,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   187,   188,   189,   173,
     174,   175,     0,   176,   177,   178,   179,   180,   181,   182,
     183,   184,   185,   186,   187,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   173,
     174,   175,   632,   176,   177,   178,   179,   180,   181,   182,
     183,   184,   185,   186,   187,   188,   189,     0,     0,     0,
       0,     0,    16,     0,     0,     0,     0,     0,   173,   174,
     175,   359,   176,   177,   178,   179,   180,   181,   182,   183,
     184,   185,   186,   187,   188,   189,   173,   174,   175,   626,
     176,   177,   178,   179,   180,   181,   182,   183,   184,   185,
     186,   187,   188,   189,   173,   174,   175,     0,   176,   177,
     178,   179,   180,   181,   182,   183,   184,   185,   186,   187,
     188,   189
};

static const yytype_int16 yycheck[] =
{
      11,    12,    13,    46,    47,     3,    24,     5,     3,    56,
       5,    64,    58,   164,   125,     3,    24,     5,   131,   112,
     130,   223,   244,    87,     3,    83,     5,   110,   250,    93,
      90,   246,    30,   200,   231,    30,   300,    27,    22,    47,
     116,   105,    30,    22,     1,    24,   332,    90,    64,   299,
     418,    30,   194,   384,   204,    44,    67,   126,     1,   418,
      70,   624,    70,     3,    54,    23,    24,    51,    34,   523,
      70,    61,    51,    25,    58,    70,     0,    56,   121,    58,
       3,     4,    90,   136,    32,    64,     3,     4,   199,     0,
      41,   654,    32,    41,   344,    67,   112,    69,   552,     3,
       4,   112,   110,    69,   122,    62,     5,    59,   221,   169,
     478,   171,    64,    53,   122,   483,   559,    68,   561,   478,
     136,   478,    62,    66,   483,   375,   169,     0,   171,   113,
     297,    30,   175,   117,   113,   492,   147,   116,   117,   593,
     130,     7,   126,    66,   154,   153,   154,   126,   138,    66,
      62,   204,   195,   596,   154,   598,   306,   136,   201,   154,
     112,   169,    66,   171,   447,   241,   155,   309,   157,   238,
      62,   625,    32,   242,   505,   439,    62,   328,   621,     3,
     634,   192,   193,     7,   136,    25,   336,    53,   204,    41,
      41,     3,     4,   151,    29,   153,    62,    63,    32,     3,
       4,   644,    62,    63,   247,   269,   402,    47,    32,   256,
     406,   258,   230,   667,   497,   308,    68,    68,    66,    59,
      67,   418,   230,    70,    64,   233,   244,    62,    63,    53,
      70,     5,   250,     7,    53,    68,   244,    70,    62,    63,
     391,   252,   250,    62,    63,    66,   225,   237,   398,    53,
      90,   402,    66,   306,   238,   406,   542,    29,   242,   238,
      32,   304,   241,   242,   431,     3,     4,    39,    67,     7,
     110,    70,   112,   113,   232,   233,     3,   256,    62,   258,
       7,   233,   479,   336,    67,   427,   483,    70,   334,    62,
     306,   506,   308,    69,    32,    67,   136,   308,    70,   359,
     318,   384,   385,    32,    69,    32,    67,     3,     4,    70,
     318,     3,     4,   153,   154,    53,    68,   567,    70,   430,
     336,   300,   432,    68,    62,   436,    53,    67,    70,   169,
      70,   171,    68,    62,    63,    62,    32,   504,    68,   318,
      32,    62,    63,    67,   511,   398,    70,    68,    62,    41,
     334,   359,    68,   332,    70,   334,   308,    53,   416,   317,
     318,    53,     3,     4,   204,   412,    62,    70,   148,    67,
      62,    67,    70,    62,    63,     1,   156,    39,     4,    68,
       6,     7,   398,    67,   336,     3,     4,    68,    14,    15,
      16,    32,   614,   233,   616,    67,    67,    69,   238,    70,
     384,   385,   495,    68,    30,   384,   385,    33,    67,   388,
     421,    70,    53,    66,    32,   473,    62,    63,    10,    11,
     418,    62,    68,   418,    68,    68,    67,    70,   492,   472,
     418,    62,    68,   412,    70,    53,   447,    62,    63,   418,
      67,    67,   432,    69,    62,    67,   398,   431,    29,    67,
       3,     4,   431,     1,   565,     3,     4,   297,     8,   661,
     439,   519,    71,   481,   507,   245,   306,   669,   308,    53,
      54,    55,    39,   481,    69,   588,    62,    63,   521,    32,
     478,   524,   525,   478,   495,    68,   497,    62,    63,   532,
     478,   642,   643,    69,   492,   542,   336,   492,   544,   478,
      53,    69,   481,    68,   492,     4,     5,     6,     7,    62,
      63,   554,    68,   492,    41,    14,    15,    16,    41,   359,
     504,   505,   480,   481,    68,   504,   505,   511,    62,    63,
     573,    30,   511,    32,    97,    98,    12,    13,     1,    71,
     583,     4,     5,     6,     7,     5,     6,     7,   559,    12,
     561,    14,    15,    16,    14,    15,    16,    71,   398,    68,
     544,    68,    68,   542,    68,   544,     3,    30,    67,    32,
     350,    39,    32,   353,     6,     7,    71,   357,   358,    69,
      71,   624,    14,    15,    16,   596,    70,   598,    68,     4,
       5,     6,     7,     3,    68,    32,   614,    41,   616,    14,
      15,    16,    41,    34,   647,    68,   614,    62,   616,    62,
     621,   654,    62,    41,     7,    30,    53,    32,   602,    67,
     158,    67,    41,   602,    67,    62,    63,    69,   408,   409,
     168,     3,     4,   644,   474,   173,   174,    18,   176,   177,
     178,   179,   180,   181,   182,   183,   184,   185,   186,   187,
     188,   189,    67,   433,   434,   435,    41,   437,   438,    70,
      32,    69,    69,     1,   504,    62,     4,     5,     6,     7,
      62,   511,    67,    67,    39,   455,    14,    15,    16,    69,
      69,    53,    68,     1,    71,    19,     4,     5,     6,     7,
      62,    29,    30,    67,    32,    68,    14,    15,    16,    67,
      67,    39,     1,    10,    68,     4,     5,     6,     7,    68,
      67,    29,    30,   251,    32,    14,    15,    16,    39,    67,
      62,    39,    67,    70,    62,    63,   103,    68,    66,    67,
      29,    30,    70,    32,     3,     4,     5,     6,     7,    68,
      39,    67,    67,   309,    62,    63,    60,   230,    66,    67,
     335,   308,    70,   254,     5,     6,     7,   206,     3,     4,
     451,   258,   602,    14,    15,    16,   154,    66,    67,   147,
       1,    70,     3,     4,     5,     6,     7,   412,     9,    10,
      11,   252,    13,    14,    15,    16,    17,    32,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,   478,    35,    36,    37,   419,    53,   483,
     648,   650,    43,   390,   352,    46,   303,    62,   388,    -1,
      51,    52,    53,    -1,    -1,    -1,    57,    58,   366,   367,
      -1,    62,    -1,    64,    65,    66,    67,    -1,    69,    -1,
       1,    -1,     3,     4,     5,     6,     7,    -1,    -1,    -1,
      -1,    12,    -1,    14,    15,    16,     4,     5,     6,     7,
      -1,    -1,    -1,   401,    -1,    -1,    14,    15,    16,    30,
      -1,    32,   410,    -1,    -1,    -1,     1,    -1,     3,     4,
      -1,    -1,    30,   421,     9,    10,    11,    -1,    13,    -1,
      -1,    -1,    17,    -1,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    -1,    31,    68,    33,   447,
      35,    36,    37,   451,    -1,    -1,    -1,    -1,    43,    67,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,
     468,    -1,    57,    58,    -1,    -1,    -1,    62,    -1,    64,
      65,    66,    67,     1,    69,     3,     4,    -1,    -1,    -1,
      -1,     9,    10,    11,    -1,    13,    -1,     1,   496,   497,
       4,     5,     6,     7,    -1,    -1,    -1,    -1,    12,    -1,
      14,    15,    16,    31,    -1,    33,    -1,    35,    36,    37,
       3,     4,    -1,    -1,     7,    43,    30,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    -1,    -1,    -1,    57,
      58,    -1,    -1,    61,    62,    63,    64,    65,    66,    32,
      -1,    69,     1,    -1,    -1,     4,     5,     6,     7,    -1,
      -1,   559,    66,   561,    -1,    14,    15,    16,     3,     4,
      53,    -1,     7,    -1,     1,    -1,     3,     4,    -1,    62,
     578,    30,     9,    10,    11,    -1,    13,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    32,   596,   597,
     598,    -1,    -1,    -1,    31,    -1,    33,    -1,    35,    36,
      37,    51,    52,    53,    54,    55,    43,    66,    53,    46,
      -1,    -1,    -1,   621,    51,    52,    53,    62,    63,    -1,
      57,    58,    -1,    -1,    61,    62,    63,    64,    65,    66,
      -1,     3,    69,    -1,    -1,    -1,   644,     9,    10,    11,
      -1,    13,     4,    -1,     6,     7,    -1,    -1,    -1,    -1,
      -1,    -1,    14,    15,    16,    -1,    -1,    -1,    -1,    31,
      -1,    33,    -1,    35,    36,    37,    -1,    -1,    30,    -1,
      -1,    43,    29,    -1,    46,    32,    -1,    -1,    -1,    51,
      52,    53,    39,    -1,    -1,    57,    58,    -1,     3,    -1,
      62,    53,    64,    65,     9,    10,    11,    -1,    13,    71,
      62,    63,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      67,    -1,    -1,    70,    -1,    -1,    31,    -1,    33,    -1,
      35,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,    57,
      58,    46,    60,    61,    62,    63,    51,    52,    53,    -1,
      -1,    -1,    57,    58,    -1,     3,    -1,    62,    -1,    64,
      65,     9,    10,    11,     1,    13,    71,     4,    -1,     6,
       7,    -1,    -1,    -1,    -1,    -1,    -1,    14,    15,    16,
      -1,    -1,    -1,    31,    -1,    33,    -1,    35,    36,    37,
      -1,    -1,    -1,    30,    -1,    43,    33,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    -1,    -1,    -1,    57,
      58,    -1,     3,    -1,    62,    -1,    64,    65,     9,    10,
      11,    -1,    13,    71,    -1,     4,     5,     6,     7,    -1,
      -1,    -1,    -1,    12,    -1,    14,    15,    16,    -1,    -1,
      31,    -1,    33,    -1,    35,    36,    37,    -1,    -1,    -1,
      -1,    30,    43,    32,    -1,    46,    -1,    -1,    -1,    -1,
      51,    52,    53,    -1,    -1,    -1,    57,    58,    -1,     3,
      -1,    62,    -1,    64,    65,     9,    10,    11,    -1,    13,
      71,    -1,     4,     5,     6,     7,    -1,    -1,    -1,    -1,
      -1,    -1,    14,    15,    16,    -1,    -1,    31,    -1,    33,
      -1,    35,    36,    37,    -1,    -1,    -1,    -1,    30,    43,
      32,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,
      -1,    -1,    -1,    57,    58,    -1,    -1,    -1,    62,    -1,
      64,    65,    -1,    -1,    -1,    -1,     1,    71,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    -1,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
      35,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,
      -1,    -1,    57,    58,    -1,    -1,    -1,    62,    -1,    64,
      65,    66,    67,     1,    -1,     3,     4,    -1,    -1,    -1,
      -1,     9,    10,    11,    -1,    13,    -1,    -1,    -1,    17,
      -1,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    -1,    31,    -1,    33,    -1,    35,    36,    37,
      -1,    -1,    -1,    -1,    -1,    43,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    -1,    -1,    -1,    57,
      58,    -1,    -1,    -1,    62,    -1,    64,    65,    66,    67,
       1,    -1,     3,     4,    -1,    -1,    -1,    -1,     9,    10,
      11,    -1,    13,    -1,    -1,    -1,    17,    -1,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    -1,
      31,    -1,    33,    -1,    35,    36,    37,    -1,    -1,    -1,
      -1,    -1,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      51,    52,    53,    -1,    -1,    -1,    57,    58,    -1,    -1,
      -1,    62,    -1,    64,    65,    66,    67,     1,    -1,     3,
       4,     5,     6,     7,    -1,     4,     5,     6,     7,    -1,
      14,    15,    16,    12,    -1,    14,    15,    16,    49,    50,
      51,    52,    53,    54,    55,    29,    30,    -1,    32,    33,
       1,    30,     3,     4,    -1,     6,     7,    -1,     9,    10,
      11,    -1,    13,    14,    15,    16,    -1,    -1,    -1,    53,
      48,    49,    50,    51,    52,    53,    54,    55,    62,    30,
      31,    -1,    33,    67,    35,    36,    37,    -1,    -1,    -1,
      -1,    -1,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      51,    52,    53,    -1,    -1,    -1,    57,    58,    -1,    -1,
      -1,    62,    -1,    64,    65,    66,     1,    -1,     3,     4,
      -1,    -1,    -1,    -1,     9,    10,    11,     4,    13,     6,
       7,    -1,    -1,    -1,    -1,    -1,    -1,    14,    15,    16,
      -1,    -1,    -1,    -1,    -1,    -1,    31,    -1,    33,    -1,
      35,    36,    37,    30,    39,    -1,    -1,    -1,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,
      -1,    -1,    57,    58,    -1,    -1,    61,    62,    63,    64,
      65,    66,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,
       9,    10,    11,    40,    13,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    -1,
      -1,    -1,    31,    -1,    33,    -1,    35,    36,    37,    -1,
      -1,    -1,    -1,    -1,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    -1,    -1,    -1,    57,    58,
      -1,    -1,    61,    62,    63,    64,    65,    66,     1,    -1,
       3,    -1,    -1,    -1,    -1,    -1,     9,    10,    11,    -1,
      13,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    -1,    -1,    -1,    -1,    31,    -1,
      33,    -1,    35,    36,    37,    -1,    -1,    -1,    -1,    -1,
      43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    -1,    -1,    -1,    57,    58,    -1,    -1,    -1,    62,
      -1,    64,    65,    66,     3,     4,     5,     6,     7,    -1,
       9,    10,    11,    -1,    13,    14,    15,    16,    17,    -1,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    -1,
      -1,    -1,    -1,    -1,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    -1,    -1,    -1,    57,    58,
      -1,    -1,    -1,    62,    -1,    64,    65,    66,    67,     3,
       4,    -1,    -1,    -1,    -1,     9,    10,    11,    -1,    13,
      -1,    -1,    -1,    17,    -1,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    -1,    31,    -1,    33,
      -1,    35,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,
      -1,    -1,    -1,    57,    58,    -1,    -1,    -1,    62,     3,
      64,    65,    66,    67,    -1,     9,    10,    11,    -1,    13,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    -1,    -1,    -1,    -1,    -1,    31,    -1,    33,
      -1,    35,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,
      -1,    -1,    -1,    57,    58,    -1,    -1,    -1,    62,     3,
      64,    65,    -1,    67,    -1,     9,    10,    11,    -1,    13,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    31,    -1,    33,
      -1,    35,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,
      -1,    -1,    -1,    57,    58,    -1,    -1,    -1,    62,    -1,
      64,    65,    66,     3,     4,     5,     6,     7,    -1,     9,
      10,    11,    -1,    13,    14,    15,    16,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    -1,    35,    36,    37,    -1,    -1,
      -1,    -1,    -1,    43,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    51,    52,    53,    -1,    -1,    -1,    57,    58,    -1,
      -1,    -1,    62,    -1,    64,    65,     3,     4,    -1,     6,
       7,    -1,     9,    10,    11,    -1,    13,    14,    15,    16,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    30,    31,    -1,    33,    -1,    35,    36,
      37,    -1,    -1,    -1,    -1,    -1,    43,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    51,    52,    53,    -1,    -1,    -1,
      57,    58,    -1,     3,    -1,    62,    -1,    64,    65,     9,
      10,    11,    -1,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    31,    -1,    33,    -1,    35,    36,    37,    -1,    -1,
      -1,    -1,    -1,    43,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    51,    52,    53,    -1,    -1,    -1,    57,    58,    -1,
       3,    -1,    62,    -1,    64,    65,     9,    10,    11,    -1,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    31,    -1,
      33,    -1,    35,    36,    37,    -1,    -1,    -1,    -1,    -1,
      43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    -1,    -1,    -1,    57,    58,    -1,     3,    -1,    62,
      -1,    64,    65,     9,    10,    11,    -1,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    31,    -1,    33,    -1,    35,
      36,    37,    -1,    -1,    -1,    -1,    -1,    43,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,    -1,
      -1,    57,    58,    -1,     3,    -1,    62,    -1,    64,    65,
       9,    10,    11,    -1,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    31,    -1,    33,    -1,    35,    36,    37,    -1,
      -1,    12,    -1,    -1,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    -1,    -1,    -1,    57,    58,
      -1,    -1,    -1,    62,    -1,    64,    65,    38,    39,    40,
      -1,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    38,
      39,    40,    -1,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,
      39,    40,    71,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    -1,    -1,    -1,
      -1,    -1,    32,    -1,    -1,    -1,    -1,    -1,    38,    39,
      40,    70,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    38,    39,    40,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    73,    74,    75,     0,    76,     1,     4,     5,     6,
       7,    14,    15,    16,    29,    30,    32,    33,    67,    77,
      78,    79,   109,   112,   116,   117,   120,   130,   131,   150,
     215,   216,   217,   219,    77,    67,    69,     3,     4,    66,
      86,   149,    66,   149,    66,   149,    62,    62,    62,     3,
      53,    62,   123,   127,   130,   148,   109,    67,   109,   117,
     120,   130,   216,   217,   113,   131,    77,   154,    66,   157,
     158,    66,   157,    66,     3,     9,    10,    11,    13,    31,
      35,    36,    37,    43,    46,    51,    52,    53,    57,    58,
      62,    64,    65,    89,    90,    92,    93,    94,    95,    96,
      98,   100,   102,   103,   104,   215,    90,   118,   120,   164,
     166,   217,    62,   167,   148,    67,    70,   110,     1,    29,
      62,    63,    84,   124,     4,    53,    62,    67,   122,   125,
     130,   145,   146,   148,   123,   148,   113,   121,   130,   150,
     216,   217,   219,     1,    86,   162,   163,   153,    69,     1,
      67,   118,   159,   166,   215,   151,    69,   152,    62,    86,
      87,    96,     1,    90,   101,   164,    96,    68,    70,    62,
      93,    62,    93,    38,    39,    40,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      57,    58,    60,    61,    62,    63,   104,    96,    68,    53,
      62,    63,   165,   168,   119,    68,   120,   165,   217,    86,
      88,   132,   133,   134,   216,   217,   219,   148,   217,    68,
     125,   145,   148,    62,   208,   209,    71,    90,     1,   105,
     106,   107,   114,   117,   120,   172,   129,   130,   167,   146,
      67,    70,   110,     1,    80,   124,    62,    63,    67,     1,
      82,    39,    70,   156,   162,   129,   109,    67,   109,   159,
     157,   129,   157,    98,    68,    68,    66,   178,   179,    68,
      67,    98,   164,   164,    98,    98,    90,    99,    98,    98,
      98,    98,    98,    98,    98,    98,    98,    98,    98,    98,
      98,    98,    86,    86,    91,    92,    90,   167,   168,   201,
     202,    71,    90,    62,    63,   121,   119,    68,    70,    62,
     102,     8,   214,   218,     1,     3,    12,   112,   116,   203,
     205,   206,   207,   210,   211,   212,    71,    67,    85,    12,
     107,   172,   109,    67,   109,   120,   115,    39,   146,    68,
     125,   146,   105,   129,   208,    71,    90,   105,    98,   163,
      69,   156,    41,   145,   160,   161,   160,    69,    69,    70,
     173,    68,    66,    96,    68,    68,    41,    41,    68,    71,
     168,    68,   214,   203,    71,   201,    71,    90,    68,   133,
       3,    91,    68,    68,   109,   109,    68,    67,    70,    68,
      70,     1,   177,   179,    67,   122,   123,   148,   115,   121,
     216,   128,    81,    39,   214,    71,    83,   129,    69,    98,
      41,   129,    70,   129,   129,   164,    34,    69,   174,   175,
     176,    97,    98,    98,   214,    71,    68,    70,    68,     4,
      53,    62,   130,   147,   148,   165,    53,   148,   165,   204,
      12,   207,   212,   179,    67,    67,     1,    66,    98,   135,
     177,   126,   177,   129,   129,    98,   161,    68,    87,   213,
       1,     3,     4,    17,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    67,    87,    90,   108,   111,
     112,   116,   169,   170,   172,   179,   180,   181,   182,   185,
     186,   194,   215,   176,     1,    61,    63,    66,    88,    98,
     136,   137,   138,    92,   167,   110,    62,    63,   129,   129,
     129,   167,   129,   129,   203,   136,   135,   129,    67,    70,
      69,    62,   188,   183,    62,    62,    98,    41,    67,    67,
      67,    90,    53,    87,   195,   217,    41,    67,   111,   169,
     171,   172,   109,    67,   109,    69,   172,   185,    18,     1,
     184,   186,   194,     1,    62,   111,    88,    98,   136,    41,
      69,    70,   155,    68,   147,    53,   147,   208,    71,    90,
      69,    87,    90,    62,   184,    90,   196,    90,    12,    41,
      67,    90,    67,    62,    69,    67,   122,   139,   145,   123,
     142,   148,     1,   187,   184,    90,    39,    12,    71,    69,
     138,   138,   167,   214,    71,    68,    90,    19,    67,    68,
      98,    67,    90,    67,   140,    67,   143,   184,    68,   138,
      98,    39,   138,    68,   190,   193,    41,    41,    68,   105,
     105,    67,    71,   138,   189,   196,   184,    10,   197,   198,
     199,    67,   141,   144,    39,   184,    67,    62,    41,    68,
      70,   179,   179,   138,   191,    90,   197,    67,   199,   196,
      68,    41,    68,    68,   102,   200,    67,   192,    68,    70,
     184,    67,   102
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    72,    73,    73,    75,    74,    76,    74,    77,    77,
      77,    77,    78,    78,    78,    78,    78,    78,    78,    78,
      80,    81,    79,    79,    82,    83,    79,    79,    84,    85,
      79,    79,    86,    86,    87,    88,    89,    89,    89,    89,
      89,    89,    89,    89,    89,    90,    91,    91,    92,    92,
      93,    93,    93,    93,    93,    93,    93,    93,    93,    94,
      95,    96,    96,    97,    96,    98,    98,    98,    98,    98,
      98,    98,    98,    98,    98,    98,    98,    98,    98,    98,
      98,    99,    98,    98,    98,   100,   100,   100,   100,   100,
     101,   100,   100,   100,   100,   100,   100,   100,   100,   102,
     103,   103,   104,   104,   105,   105,   105,   106,   106,   106,
     106,   107,   107,   107,   107,   108,   108,   108,   108,   109,
     110,   111,   111,   111,   111,   111,   111,   111,   112,   112,
     113,   113,   113,   113,   114,   114,   115,   115,   115,   116,
     116,   116,   116,   117,   117,   117,   117,   118,   118,   119,
     119,   120,   120,   120,   120,   120,   121,   121,   121,   122,
     122,   123,   123,   124,   124,   126,   125,   125,   128,   127,
     127,   129,   129,   130,   130,   131,   132,   132,   133,   133,
     133,   133,   133,   134,   134,   134,   134,   135,   135,   135,
     136,   136,   137,   137,   138,   138,   138,   138,   138,   138,
     138,   138,   140,   141,   139,   143,   144,   142,   145,   145,
     146,   146,   146,   146,   146,   146,   146,   147,   147,   147,
     147,   147,   147,   148,   148,   148,   148,   148,   148,   148,
     149,   151,   150,   150,   150,   152,   150,   150,   150,   153,
     150,   154,   150,   150,   155,   155,   156,   156,   157,   157,
     158,   158,   158,   159,   159,   159,   159,   159,   159,   160,
     160,   161,   161,   161,   162,   162,   162,   163,   163,   164,
     164,   165,   165,   166,   166,   167,   167,   168,   168,   168,
     168,   168,   168,   168,   168,   168,   169,   170,   170,   170,
     171,   171,   172,   173,   174,   174,   175,   175,   176,   177,
     177,   178,   179,   179,   179,   179,   180,   180,   181,   183,
     182,   184,   184,   185,   185,   186,   186,   187,   186,   186,
     186,   188,   189,   186,   186,   186,   190,   191,   192,   186,
     193,   186,   186,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   194,   194,   194,   194,   195,   195,   196,
     196,   197,   197,   198,   198,   199,   200,   200,   202,   201,
     203,   204,   203,   203,   205,   205,   205,   205,   206,   206,
     207,   207,   207,   207,   207,   209,   208,   210,   210,   211,
     211,   212,   213,   213,   214,   214,   215,   216,   217,   218,
     219
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     0,     2,     0,     3,     1,     1,
       5,     2,     3,     4,     4,     2,     3,     2,     2,     1,
       0,     0,     7,     4,     0,     0,     7,     4,     0,     0,
       6,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     3,
       1,     2,     2,     2,     2,     2,     4,     2,     4,     1,
       1,     1,     4,     0,     7,     1,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       5,     0,     5,     3,     3,     1,     1,     1,     3,     3,
       0,     4,     4,     6,     4,     3,     3,     2,     2,     1,
       1,     2,     1,     1,     0,     1,     2,     1,     1,     2,
       2,     4,     4,     3,     2,     1,     1,     2,     2,     0,
       0,     4,     4,     3,     3,     3,     2,     2,     2,     3,
       0,     2,     2,     2,     2,     3,     0,     2,     2,     1,
       1,     2,     2,     1,     1,     2,     2,     2,     3,     0,
       2,     1,     1,     1,     4,     4,     1,     1,     1,     1,
       3,     1,     3,     0,     4,     0,     6,     3,     0,     6,
       3,     0,     1,     1,     2,     6,     1,     3,     0,     1,
       4,     6,     4,     1,     1,     1,     1,     1,     3,     1,
       0,     2,     1,     3,     1,     3,     1,     7,     5,     4,
       3,     4,     0,     0,     5,     0,     0,     5,     1,     1,
       3,     4,     4,     3,     3,     3,     1,     4,     4,     3,
       3,     3,     1,     4,     3,     3,     4,     3,     3,     1,
       1,     0,     7,     5,     2,     0,     7,     5,     2,     0,
       8,     0,     7,     2,     0,     1,     0,     1,     1,     2,
       0,     3,     2,     3,     2,     3,     2,     1,     2,     1,
       3,     2,     4,     3,     1,     3,     1,     1,     3,     2,
       2,     0,     1,     1,     2,     0,     2,     3,     3,     2,
       4,     4,     3,     3,     3,     2,     1,     1,     2,     2,
       0,     1,     2,     0,     0,     1,     1,     2,     3,     1,
       2,     1,     3,     6,     5,     5,     2,     2,     4,     0,
       4,     1,     2,     1,     1,     1,     2,     0,     4,     1,
       3,     0,     0,     7,     5,     2,     0,     0,     0,    12,
       0,     6,     2,     2,     2,     3,     6,     8,    10,    12,
       3,     4,     1,     3,     5,     2,     2,     0,     1,     0,
       1,     0,     1,     1,     3,     4,     1,     3,     0,     2,
       2,     0,     4,     2,     0,     1,     1,     3,     1,     3,
       4,     4,     4,     4,     4,     0,     2,     1,     2,     1,
       3,     1,     1,     3,     0,     1,     1,     1,     1,     1,
       1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
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

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



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

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
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
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
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
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
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


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
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
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
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
            /* Fall through.  */
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

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
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
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
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
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
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
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
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
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

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

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
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

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

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
| yyreduce -- Do a reduction.  |
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
        case 2:
#line 279 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    pedwarn("ANSI C forbids an empty source file");
		  the_program = NULL;
		}
#line 2273 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 284 "c-parse.y" /* yacc.c:1646  */
    {
		  the_program = (yyvsp[0].u.decl);
		}
#line 2281 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 294 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = NULL; }
#line 2287 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 294 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[0].u.decl); }
#line 2293 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 295 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = NULL; }
#line 2299 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 296 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-2].u.decl), (yyvsp[0].u.decl)); }
#line 2305 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 303 "c-parse.y" /* yacc.c:1646  */
    { 
		  (yyval.u.decl) = CAST(declaration, new_asm_decl
		    (pr, (yyvsp[-4].u.itoken).location,
		     new_asm_stmt(pr, (yyvsp[-4].u.itoken).location, (yyvsp[-2].u.expr), NULL, NULL, NULL, NULL))); }
#line 2314 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 308 "c-parse.y" /* yacc.c:1646  */
    { pedantic = (yyvsp[-1].u.itoken).i; 
		  (yyval.u.decl) = CAST(declaration, new_extension_decl(pr, (yyvsp[-1].u.itoken).location, (yyvsp[0].u.decl))); }
#line 2321 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 314 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    error("ANSI C forbids data definition with no type or storage class");
		  else if (!flag_traditional)
		    warning("data definition has no type or storage class"); 

		  (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-1].u.decl)->location, NULL, NULL, (yyvsp[-1].u.decl)));
		  pop_declspec_stack(); }
#line 2333 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 322 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-3].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[-1].u.decl)));
		  pop_declspec_stack(); }
#line 2340 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 325 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-3].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[-1].u.decl)));
		  pop_declspec_stack(); }
#line 2347 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 328 "c-parse.y" /* yacc.c:1646  */
    { pedwarn("empty declaration"); }
#line 2353 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 330 "c-parse.y" /* yacc.c:1646  */
    { shadow_tag((yyvsp[-2].u.telement)); 
	    (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-2].u.telement)->location, current_declspecs, prefix_attributes, NULL));
	    pop_declspec_stack(); }
#line 2361 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 333 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location); }
#line 2367 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 334 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location); }
#line 2373 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 336 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    pedwarn("ANSI C does not allow extra `;' outside of a function");
		  (yyval.u.decl) = NULL; }
#line 2381 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 343 "c-parse.y" /* yacc.c:1646  */
    { if (!start_function(current_declspecs, (yyvsp[0].u.declarator),
				      prefix_attributes, 0))
		    YYERROR1; }
#line 2389 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 347 "c-parse.y" /* yacc.c:1646  */
    { store_parm_decls((yyvsp[0].u.decl)); }
#line 2395 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 349 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = finish_function((yyvsp[0].u.stmt));
		  pop_declspec_stack(); }
#line 2402 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 352 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location);
		  pop_declspec_stack(); }
#line 2409 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 355 "c-parse.y" /* yacc.c:1646  */
    { if (!start_function(current_declspecs, (yyvsp[0].u.declarator),
				      prefix_attributes, 0))
		    YYERROR1; }
#line 2417 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 359 "c-parse.y" /* yacc.c:1646  */
    { store_parm_decls((yyvsp[0].u.decl)); }
#line 2423 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 361 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = finish_function((yyvsp[0].u.stmt)); 
		  pop_declspec_stack(); }
#line 2430 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 364 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location);
		  pop_declspec_stack(); }
#line 2437 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 367 "c-parse.y" /* yacc.c:1646  */
    { if (!start_function(NULL, (yyvsp[0].u.declarator),
				      prefix_attributes, 0))
		    YYERROR1; }
#line 2445 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 371 "c-parse.y" /* yacc.c:1646  */
    { store_parm_decls((yyvsp[0].u.decl)); }
#line 2451 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 373 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = finish_function((yyvsp[0].u.stmt)); 
		  pop_declspec_stack(); }
#line 2458 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 376 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location);
		  pop_declspec_stack(); }
#line 2465 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 386 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.id_label) = new_id_label(pr, (yyvsp[0].idtoken).location, (yyvsp[0].idtoken).id); }
#line 2471 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 390 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.word) = new_word(pr, (yyvsp[0].idtoken).location, (yyvsp[0].idtoken).id); }
#line 2477 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 393 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.itoken) = (yyvsp[0].u.itoken); (yyval.u.itoken).i = kind_address_of; }
#line 2483 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 395 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.itoken) = (yyvsp[0].u.itoken); (yyval.u.itoken).i = kind_unary_minus; }
#line 2489 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 397 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.itoken) = (yyvsp[0].u.itoken); (yyval.u.itoken).i = kind_unary_plus; }
#line 2495 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 399 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.itoken) = (yyvsp[0].u.itoken); (yyval.u.itoken).i = kind_preincrement; }
#line 2501 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 401 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.itoken) = (yyvsp[0].u.itoken); (yyval.u.itoken).i = kind_predecrement; }
#line 2507 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 403 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.itoken) = (yyvsp[0].u.itoken); (yyval.u.itoken).i = kind_bitnot; }
#line 2513 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 405 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.itoken) = (yyvsp[0].u.itoken); (yyval.u.itoken).i = kind_not; }
#line 2519 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 407 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.itoken) = (yyvsp[0].u.itoken); (yyval.u.itoken).i = kind_realpart; }
#line 2525 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 409 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.itoken) = (yyvsp[0].u.itoken); (yyval.u.itoken).i = kind_imagpart; }
#line 2531 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 413 "c-parse.y" /* yacc.c:1646  */
    { if ((yyvsp[0].u.expr)->next)
		    (yyval.u.expr) = make_comma((yyvsp[0].u.expr)->location, (yyvsp[0].u.expr));
		  else
		    (yyval.u.expr) = (yyvsp[0].u.expr); }
#line 2540 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 421 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = NULL; }
#line 2546 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 427 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = (yyvsp[0].u.expr); }
#line 2552 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 429 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = expression_chain((yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2558 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 435 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_dereference((yyvsp[-1].u.itoken).location, (yyvsp[0].u.expr)); }
#line 2564 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 438 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_extension_expr((yyvsp[-1].u.itoken).location, (yyvsp[0].u.expr));
		  pedantic = (yyvsp[-1].u.itoken).i; }
#line 2571 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 441 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_unary((yyvsp[-1].u.itoken).location, (yyvsp[-1].u.itoken).i, (yyvsp[0].u.expr));
#if 0
		  overflow_warning((yyval.u.expr)); 
#endif
		}
#line 2581 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 448 "c-parse.y" /* yacc.c:1646  */
    {
		  (yyval.u.expr) = CAST(expression, make_label_address((yyvsp[-1].u.itoken).location, (yyvsp[0].u.id_label)));
		  use_label((yyvsp[0].u.id_label));
		}
#line 2590 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 453 "c-parse.y" /* yacc.c:1646  */
    { 
#if 0
		  if (TREE_CODE ((yyvsp[0].u.expr)) == COMPONENT_REF
		      && DECL_C_BIT_FIELD (TREE_OPERAND ((yyvsp[0].u.expr), 1)))
		    error("`sizeof' applied to a bit-field");
		  (yyval.u.expr) = c_sizeof (TREE_TYPE ((yyvsp[0].u.expr))); 
#endif
		  (yyval.u.expr) = make_sizeof_expr((yyvsp[-1].u.itoken).location, (yyvsp[0].u.expr));
		  unevaluated_expression--; }
#line 2604 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 463 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_sizeof_type((yyvsp[-3].u.itoken).location, (yyvsp[-1].u.type));
		  unevaluated_expression--; }
#line 2611 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 466 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_alignof_expr((yyvsp[-1].u.itoken).location, (yyvsp[0].u.expr));
		  unevaluated_expression--; }
#line 2618 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 469 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_alignof_type((yyvsp[-3].u.itoken).location, (yyvsp[-1].u.type)); 
		  unevaluated_expression--; }
#line 2625 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 474 "c-parse.y" /* yacc.c:1646  */
    { unevaluated_expression++; (yyval.u.itoken) = (yyvsp[0].u.itoken); }
#line 2631 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 478 "c-parse.y" /* yacc.c:1646  */
    { unevaluated_expression++; (yyval.u.itoken) = (yyvsp[0].u.itoken); }
#line 2637 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 484 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_cast((yyvsp[-3].u.itoken).location, (yyvsp[-2].u.type), (yyvsp[0].u.expr)); }
#line 2643 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 486 "c-parse.y" /* yacc.c:1646  */
    { 
#if 0
		  start_init (NULL, NULL, 0);
		  (yyvsp[-2].u.type) = groktypename ((yyvsp[-2].u.type));
		  really_start_incremental_init ((yyvsp[-2].u.type)); 
#endif
		}
#line 2655 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 494 "c-parse.y" /* yacc.c:1646  */
    { 
		  (yyval.u.expr) = CAST(expression, new_cast_list(pr, (yyvsp[-6].u.itoken).location, (yyvsp[-5].u.type), CAST(expression, new_init_list(pr, (yyvsp[-1].u.expr)->location, (yyvsp[-1].u.expr)))));
		  (yyval.u.expr)->type = (yyvsp[-5].u.type)->type;
		  /* XXX: Evil hack for foo((int[5]) {1, 2, 3}) */
		  /* XXX: what does gcc do ? */
		  if (type_array((yyval.u.expr)->type))
		    (yyval.u.expr)->lvalue = TRUE;

		  if (pedantic)
		    pedwarn("ANSI C forbids constructor expressions");
#if 0
		  char *name;
		  tree result = pop_init_level (0);
		  tree type = (yyvsp[-5].u.type);
		  finish_init ();

		  if (TYPE_NAME (type) != 0)
		    {
		      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
			name = IDENTIFIER_POINTER (TYPE_NAME (type));
		      else
			name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
		    }
		  else
		    name = "";
		  (yyval.u.expr) = result;
		  if (TREE_CODE (type) == ARRAY_TYPE && TYPE_SIZE (type) == 0)
		    {
		      int failure = complete_array_type (type, (yyval.u.expr), 1);
		      if (failure)
			abort ();
		    }
#endif
		}
#line 2694 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 533 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_plus, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2700 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 535 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_minus, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2706 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 537 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_times, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2712 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 539 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_divide, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2718 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 541 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_modulo, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2724 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 543 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_lshift, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2730 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 545 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_rshift, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2736 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 547 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, (yyvsp[-1].u.itoken).i, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2742 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 549 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, (yyvsp[-1].u.itoken).i, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2748 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 551 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_bitand, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2754 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 553 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_bitor, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2760 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 555 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_bitxor, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2766 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 557 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_andand, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2772 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 559 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_binary((yyvsp[-1].u.itoken).location, kind_oror, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2778 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 561 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_conditional((yyvsp[-3].u.itoken).location, (yyvsp[-4].u.expr), (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2784 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 563 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    pedwarn("ANSI C forbids omitting the middle term of a ?: expression"); 
		}
#line 2792 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 567 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_conditional((yyvsp[-3].u.itoken).location, (yyvsp[-4].u.expr), NULL, (yyvsp[0].u.expr)); }
#line 2798 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 569 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_assign((yyvsp[-1].u.itoken).location, kind_assign, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2804 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 571 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_assign((yyvsp[-1].u.itoken).location, (yyvsp[-1].u.itoken).i, (yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 2810 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 576 "c-parse.y" /* yacc.c:1646  */
    { 
		  if (yychar == YYEMPTY)
		    yychar = yylex();
		  (yyval.u.expr) = make_identifier((yyvsp[0].idtoken).location, (yyvsp[0].idtoken).id, yychar == '('); 
		}
#line 2820 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 581 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = CAST(expression, (yyvsp[0].u.constant)); }
#line 2826 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 582 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = CAST(expression, (yyvsp[0].u.string)); }
#line 2832 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 584 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = (yyvsp[-1].u.expr); }
#line 2838 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 586 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_error_expr(last_location); }
#line 2844 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 588 "c-parse.y" /* yacc.c:1646  */
    { if (current_function_decl == 0)
		    {
		      error("braced-group within expression allowed only inside a function");
		      YYERROR;
		    }
		    push_label_level();
		}
#line 2856 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 596 "c-parse.y" /* yacc.c:1646  */
    { 
		  pop_label_level();
		  if (pedantic)
		    pedwarn("ANSI C forbids braced-groups within expressions");
		  (yyval.u.expr) = make_compound_expr((yyvsp[-3].u.itoken).location, (yyvsp[-1].u.stmt));
		}
#line 2867 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 603 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_function_call((yyvsp[-2].u.itoken).location, (yyvsp[-3].u.expr), (yyvsp[-1].u.expr)); }
#line 2873 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 605 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_va_arg((yyvsp[-5].u.itoken).location, (yyvsp[-3].u.expr), (yyvsp[-1].u.type)); }
#line 2879 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 607 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_array_ref((yyvsp[-2].u.itoken).location, (yyvsp[-3].u.expr), (yyvsp[-1].u.expr)); }
#line 2885 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 609 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_field_ref((yyvsp[-1].u.itoken).location, (yyvsp[-2].u.expr), (yyvsp[0].idtoken).id); }
#line 2891 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 96:
#line 611 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_field_ref((yyvsp[-1].u.itoken).location, make_dereference((yyvsp[-1].u.itoken).location, (yyvsp[-2].u.expr)),
				      (yyvsp[0].idtoken).id); }
#line 2898 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 614 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_postincrement((yyvsp[0].u.itoken).location, (yyvsp[-1].u.expr)); }
#line 2904 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 98:
#line 616 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_postdecrement((yyvsp[0].u.itoken).location, (yyvsp[-1].u.expr)); }
#line 2910 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 621 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.string) = make_string((yyvsp[0].u.expr)->location, (yyvsp[0].u.expr)); }
#line 2916 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 624 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = (yyvsp[0].u.expr); }
#line 2922 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 101:
#line 626 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = expression_chain((yyvsp[-1].u.expr), (yyvsp[0].u.expr)); }
#line 2928 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 630 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = CAST(expression, (yyvsp[0].u.expr)); }
#line 2934 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 632 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_identifier((yyvsp[0].idtoken).location, (yyvsp[0].idtoken).id, FALSE);
	  }
#line 2941 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 104:
#line 638 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = NULL; }
#line 2947 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 106:
#line 642 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    pedwarn("ANSI C does not permit use of `varargs.h'"); 
		  (yyval.u.decl) = declaration_chain((yyvsp[-1].u.decl), CAST(declaration, new_ellipsis_decl(pr, (yyvsp[0].u.itoken).location)));
		}
#line 2956 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 108:
#line 653 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location); }
#line 2962 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 109:
#line 654 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-1].u.decl), (yyvsp[0].u.decl)); }
#line 2968 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 110:
#line 655 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location); }
#line 2974 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 111:
#line 664 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-3].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[-1].u.decl)));
		  pop_declspec_stack(); }
#line 2981 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 112:
#line 667 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-3].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[-1].u.decl)));
		  pop_declspec_stack(); }
#line 2988 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 113:
#line 670 "c-parse.y" /* yacc.c:1646  */
    { shadow_tag_warned((yyvsp[-2].u.telement), 1);
		  (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-2].u.telement)->location, current_declspecs, prefix_attributes, NULL));
		  pop_declspec_stack();
		  pedwarn("empty declaration"); }
#line 2997 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 114:
#line 675 "c-parse.y" /* yacc.c:1646  */
    { pedwarn("empty declaration"); 
		  (yyval.u.decl) = NULL; }
#line 3004 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 116:
#line 685 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location); }
#line 3010 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 117:
#line 686 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-1].u.decl), (yyvsp[0].u.decl)); }
#line 3016 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 118:
#line 687 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location); }
#line 3022 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 119:
#line 695 "c-parse.y" /* yacc.c:1646  */
    { 
		  push_declspec_stack();
		  pending_xref_error();
		  split_type_elements((yyvsp[0].u.telement),
				      &current_declspecs, &prefix_attributes);
		}
#line 3033 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 120:
#line 705 "c-parse.y" /* yacc.c:1646  */
    { prefix_attributes = attribute_chain(prefix_attributes,
						      (yyvsp[0].u.attribute)); 
		/* This syntax is broken as it will apply to all remaining
		   declarations, not just the current declarator 
		   (this is broken in base GCC too) */
		/* XXX: Used extensively in the linux kernel. YUCK. */
		/*error("Unsupported attribute syntax");*/ }
#line 3045 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 121:
#line 716 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-3].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[-1].u.decl)));
		  pop_declspec_stack(); }
#line 3052 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 122:
#line 719 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-3].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[-1].u.decl)));
		  pop_declspec_stack(); }
#line 3059 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 123:
#line 722 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[0].u.decl);
		  pop_declspec_stack(); }
#line 3066 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 124:
#line 725 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[0].u.decl);
		  pop_declspec_stack(); }
#line 3073 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 125:
#line 728 "c-parse.y" /* yacc.c:1646  */
    { shadow_tag((yyvsp[-2].u.telement));
		  (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-2].u.telement)->location, current_declspecs, prefix_attributes, NULL));
		  pop_declspec_stack(); }
#line 3081 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 126:
#line 732 "c-parse.y" /* yacc.c:1646  */
    { pedwarn("empty declaration"); }
#line 3087 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 127:
#line 734 "c-parse.y" /* yacc.c:1646  */
    { pedantic = (yyvsp[-1].u.itoken).i; 
		  (yyval.u.decl) = CAST(declaration, new_extension_decl(pr, (yyvsp[-1].u.itoken).location, (yyvsp[0].u.decl))); }
#line 3094 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 128:
#line 744 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = (yyvsp[-1].u.telement); (yyvsp[-1].u.telement)->next = CAST(node, (yyvsp[0].u.telement)); }
#line 3100 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 129:
#line 746 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-2].u.telement), (yyvsp[-1].u.telement)); (yyvsp[-1].u.telement)->next = CAST(node, (yyvsp[0].u.telement)); }
#line 3106 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 130:
#line 750 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = NULL; }
#line 3112 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 131:
#line 752 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), (yyvsp[0].u.telement)); }
#line 3118 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 132:
#line 754 "c-parse.y" /* yacc.c:1646  */
    { if (extra_warnings)
		    warning("`%s' is not at beginning of declaration",
			    rid_name(CAST(rid, (yyvsp[0].u.telement))));
		  (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), (yyvsp[0].u.telement)); }
#line 3127 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 133:
#line 759 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), CAST(type_element, (yyvsp[0].u.attribute))); }
#line 3133 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 134:
#line 764 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = (yyvsp[-1].u.telement); (yyvsp[-1].u.telement)->next = CAST(node, (yyvsp[0].u.telement)); }
#line 3139 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 135:
#line 766 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-2].u.telement), (yyvsp[-1].u.telement)); (yyvsp[-1].u.telement)->next = CAST(node, (yyvsp[0].u.telement)); }
#line 3145 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 136:
#line 771 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = NULL; }
#line 3151 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 137:
#line 773 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), (yyvsp[0].u.telement)); }
#line 3157 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 138:
#line 775 "c-parse.y" /* yacc.c:1646  */
    { if (extra_warnings)
		    warning("`%s' is not at beginning of declaration",
			    rid_name(CAST(rid, (yyvsp[0].u.telement))));
		  (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), (yyvsp[0].u.telement)); }
#line 3166 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 140:
#line 788 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = CAST(type_element, (yyvsp[0].u.attribute)); }
#line 3172 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 141:
#line 790 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), (yyvsp[0].u.telement)); }
#line 3178 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 142:
#line 792 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), CAST(type_element, (yyvsp[0].u.attribute))); }
#line 3184 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 145:
#line 799 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), (yyvsp[0].u.telement)); }
#line 3190 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 146:
#line 801 "c-parse.y" /* yacc.c:1646  */
    { if (extra_warnings /*&& TREE_STATIC ($1)*/)
		    warning("`%s' is not at beginning of declaration",
			    rid_name(CAST(rid, (yyvsp[0].u.telement))));
		  (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), (yyvsp[0].u.telement)); }
#line 3199 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 147:
#line 814 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = (yyvsp[-1].u.telement); (yyvsp[-1].u.telement)->next = CAST(node, (yyvsp[0].u.telement)); }
#line 3205 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 148:
#line 816 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-2].u.telement), (yyvsp[-1].u.telement)); (yyvsp[-1].u.telement)->next = CAST(node, (yyvsp[0].u.telement)); }
#line 3211 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 149:
#line 820 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = NULL; }
#line 3217 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 150:
#line 822 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), (yyvsp[0].u.telement)); }
#line 3223 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 153:
#line 832 "c-parse.y" /* yacc.c:1646  */
    { /* For a typedef name, record the meaning, not the name.
		     In case of `foo foo, bar;'.  */
		  (yyval.u.telement) = CAST(type_element, new_typename(pr, (yyvsp[0].idtoken).location, (yyvsp[0].idtoken).decl)); }
#line 3231 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 154:
#line 836 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = CAST(type_element, new_typeof_expr(pr, (yyvsp[-3].u.itoken).location, (yyvsp[-1].u.expr))); }
#line 3237 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 155:
#line 838 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = CAST(type_element, new_typeof_type(pr, (yyvsp[-3].u.itoken).location, (yyvsp[-1].u.type))); }
#line 3243 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 160:
#line 850 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-2].u.decl), (yyvsp[0].u.decl)); }
#line 3249 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 161:
#line 854 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[0].u.decl); }
#line 3255 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 162:
#line 855 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-2].u.decl), (yyvsp[0].u.decl)); }
#line 3261 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 163:
#line 860 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.asm_stmt) = NULL; }
#line 3267 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 164:
#line 862 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.asm_stmt) = new_asm_stmt(pr, (yyvsp[-3].u.itoken).location, CAST(expression, (yyvsp[-1].u.string)),
				    NULL, NULL, NULL, NULL); }
#line 3274 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 165:
#line 868 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = start_decl((yyvsp[-3].u.declarator), (yyvsp[-2].u.asm_stmt), current_declspecs, 1,
					(yyvsp[-1].u.attribute), prefix_attributes); }
#line 3281 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 166:
#line 872 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = finish_decl((yyvsp[-1].u.decl), (yyvsp[0].u.expr)); }
#line 3287 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 167:
#line 874 "c-parse.y" /* yacc.c:1646  */
    { declaration d = start_decl((yyvsp[-2].u.declarator), (yyvsp[-1].u.asm_stmt), current_declspecs, 0,
					     (yyvsp[0].u.attribute), prefix_attributes);
		  (yyval.u.decl) = finish_decl(d, NULL); }
#line 3295 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 168:
#line 881 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = start_decl((yyvsp[-3].u.declarator), (yyvsp[-2].u.asm_stmt), current_declspecs, 1,
					 (yyvsp[-1].u.attribute), prefix_attributes); }
#line 3302 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 169:
#line 885 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = finish_decl((yyvsp[-1].u.decl), (yyvsp[0].u.expr)); }
#line 3308 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 170:
#line 887 "c-parse.y" /* yacc.c:1646  */
    { declaration d = start_decl((yyvsp[-2].u.declarator), (yyvsp[-1].u.asm_stmt), current_declspecs, 0,
					     (yyvsp[0].u.attribute), prefix_attributes);
		  (yyval.u.decl) = finish_decl(d, NULL); }
#line 3316 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 171:
#line 893 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = NULL; }
#line 3322 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 172:
#line 895 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = (yyvsp[0].u.attribute); }
#line 3328 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 173:
#line 900 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = (yyvsp[0].u.attribute); }
#line 3334 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 174:
#line 902 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = attribute_chain((yyvsp[-1].u.attribute), (yyvsp[0].u.attribute)); }
#line 3340 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 175:
#line 907 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = (yyvsp[-2].u.attribute); }
#line 3346 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 176:
#line 912 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = (yyvsp[0].u.attribute); }
#line 3352 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 177:
#line 914 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = attribute_chain((yyvsp[-2].u.attribute), (yyvsp[0].u.attribute)); }
#line 3358 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 178:
#line 919 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = NULL; }
#line 3364 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 179:
#line 921 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = new_attribute(pr, (yyvsp[0].u.word)->location, (yyvsp[0].u.word), NULL, NULL); }
#line 3370 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 180:
#line 923 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = new_attribute
		    (pr, (yyvsp[-3].u.word)->location, (yyvsp[-3].u.word), new_word(pr, (yyvsp[-1].idtoken).location, (yyvsp[-1].idtoken).id), NULL); }
#line 3377 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 181:
#line 926 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = new_attribute
		    (pr, (yyvsp[-4].u.itoken).location, (yyvsp[-5].u.word), new_word(pr, (yyvsp[-3].idtoken).location, (yyvsp[-3].idtoken).id), (yyvsp[-1].u.expr));
		}
#line 3385 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 182:
#line 930 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.attribute) = new_attribute(pr, (yyvsp[-2].u.itoken).location, (yyvsp[-3].u.word), NULL, (yyvsp[-1].u.expr)); }
#line 3391 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 184:
#line 938 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.word) = new_word(pr, (yyvsp[0].u.telement)->location, str2cstring(pr, rid_name(CAST(rid, (yyvsp[0].u.telement))))); }
#line 3397 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 185:
#line 939 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.word) = new_word(pr, (yyvsp[0].u.telement)->location, str2cstring(pr, rid_name(CAST(rid, (yyvsp[0].u.telement))))); }
#line 3403 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 186:
#line 940 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.word) = new_word(pr, (yyvsp[0].u.telement)->location, str2cstring(pr, qualifier_name(CAST(qualifier, (yyvsp[0].u.telement))->id))); }
#line 3409 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 188:
#line 949 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = CAST(expression, new_init_list(pr, (yyvsp[-2].u.itoken).location, (yyvsp[-1].u.expr))); 
		  (yyval.u.expr)->type = error_type; }
#line 3416 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 189:
#line 952 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_error_expr(last_location); }
#line 3422 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 190:
#line 958 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    pedwarn("ANSI C forbids empty initializer braces"); 
		  (yyval.u.expr) = NULL; }
#line 3430 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 191:
#line 961 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = (yyvsp[-1].u.expr); }
#line 3436 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 193:
#line 966 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = expression_chain((yyvsp[-2].u.expr), (yyvsp[0].u.expr)); }
#line 3442 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 194:
#line 972 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = (yyvsp[0].u.expr); }
#line 3448 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 195:
#line 974 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = CAST(expression, new_init_list(pr, (yyvsp[-2].u.itoken).location, (yyvsp[-1].u.expr))); }
#line 3454 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 196:
#line 975 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = make_error_expr(last_location); }
#line 3460 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 197:
#line 980 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = CAST(expression, new_init_index(pr, (yyvsp[-6].u.itoken).location, (yyvsp[-5].u.expr), (yyvsp[-3].u.expr), (yyvsp[0].u.expr))); }
#line 3466 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 198:
#line 982 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = CAST(expression, new_init_index(pr, (yyvsp[-4].u.itoken).location, (yyvsp[-3].u.expr), NULL, (yyvsp[0].u.expr))); }
#line 3472 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 199:
#line 984 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = CAST(expression, new_init_index(pr, (yyvsp[-3].u.itoken).location, (yyvsp[-2].u.expr), NULL, (yyvsp[0].u.expr))); }
#line 3478 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 200:
#line 986 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = CAST(expression, new_init_field(pr, (yyvsp[-2].u.word)->location, (yyvsp[-2].u.word), (yyvsp[0].u.expr))); }
#line 3484 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 201:
#line 988 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = CAST(expression, new_init_field(pr, (yyvsp[-3].u.itoken).location, (yyvsp[-2].u.word), (yyvsp[0].u.expr))); }
#line 3490 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 202:
#line 993 "c-parse.y" /* yacc.c:1646  */
    { if (!start_function(current_declspecs, (yyvsp[0].u.declarator),
				      prefix_attributes, 1))
		    {
		      YYERROR1;
		    }
		  }
#line 3501 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 203:
#line 1000 "c-parse.y" /* yacc.c:1646  */
    { store_parm_decls((yyvsp[0].u.decl)); }
#line 3507 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 204:
#line 1008 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = finish_function((yyvsp[0].u.stmt)); }
#line 3513 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 205:
#line 1013 "c-parse.y" /* yacc.c:1646  */
    { if (!start_function(current_declspecs, (yyvsp[0].u.declarator),
				      prefix_attributes, 1))
		    {
		      YYERROR1;
		    }
		}
#line 3524 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 206:
#line 1020 "c-parse.y" /* yacc.c:1646  */
    { store_parm_decls((yyvsp[0].u.decl)); }
#line 3530 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 207:
#line 1028 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = finish_function((yyvsp[0].u.stmt)); }
#line 3536 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 210:
#line 1043 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = (yyvsp[-1].u.declarator); }
#line 3542 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 211:
#line 1045 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = make_function_declarator((yyvsp[-2].u.itoken).location, (yyvsp[-3].u.declarator), (yyvsp[-1].u.decl), (yyvsp[0].u.telement)); }
#line 3548 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 212:
#line 1047 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[-2].u.itoken).location, (yyvsp[-3].u.declarator), (yyvsp[-1].u.expr))); }
#line 3554 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 213:
#line 1049 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[-1].u.itoken).location, (yyvsp[-2].u.declarator), NULL)); }
#line 3560 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 214:
#line 1051 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_pointer_declarator(pr, (yyvsp[-2].u.itoken).location, (yyvsp[0].u.declarator), (yyvsp[-1].u.telement))); }
#line 3566 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 215:
#line 1058 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = (yyvsp[0].u.declarator); }
#line 3572 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 216:
#line 1059 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_identifier_declarator(pr, (yyvsp[0].idtoken).location, (yyvsp[0].idtoken).id)); }
#line 3578 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 217:
#line 1069 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = make_function_declarator((yyvsp[-2].u.itoken).location, (yyvsp[-3].u.declarator), (yyvsp[-1].u.decl), (yyvsp[0].u.telement)); }
#line 3584 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 218:
#line 1071 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[-2].u.itoken).location, (yyvsp[-3].u.declarator), (yyvsp[-1].u.expr))); }
#line 3590 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 219:
#line 1073 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[-1].u.itoken).location, (yyvsp[-2].u.declarator), NULL)); }
#line 3596 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 220:
#line 1075 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_pointer_declarator(pr, (yyvsp[-2].u.itoken).location, (yyvsp[0].u.declarator), (yyvsp[-1].u.telement))); }
#line 3602 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 221:
#line 1082 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = (yyvsp[0].u.declarator); }
#line 3608 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 222:
#line 1083 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_identifier_declarator(pr, (yyvsp[0].idtoken).location, (yyvsp[0].idtoken).id)); }
#line 3614 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 223:
#line 1091 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = make_function_declarator((yyvsp[-2].u.itoken).location, (yyvsp[-3].u.declarator), (yyvsp[-1].u.decl), (yyvsp[0].u.telement)); }
#line 3620 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 224:
#line 1093 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = (yyvsp[-1].u.declarator); }
#line 3626 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 225:
#line 1095 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_pointer_declarator(pr, (yyvsp[-2].u.itoken).location, (yyvsp[0].u.declarator), (yyvsp[-1].u.telement))); }
#line 3632 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 226:
#line 1097 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[-2].u.itoken).location, (yyvsp[-3].u.declarator), (yyvsp[-1].u.expr))); }
#line 3638 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 227:
#line 1099 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[-1].u.itoken).location, (yyvsp[-2].u.declarator), NULL)); }
#line 3644 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 228:
#line 1106 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = (yyvsp[0].u.declarator); }
#line 3650 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 229:
#line 1107 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_identifier_declarator(pr, (yyvsp[0].idtoken).location, (yyvsp[0].idtoken).id)); }
#line 3656 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 230:
#line 1111 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.word) = new_word(pr, (yyvsp[0].idtoken).location, (yyvsp[0].idtoken).id); }
#line 3662 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 231:
#line 1116 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = start_struct((yyvsp[-2].u.itoken).location, kind_struct_ref, (yyvsp[-1].u.word));
		  /* Start scope of tag before parsing components.  */
		}
#line 3670 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 232:
#line 1120 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = finish_struct((yyvsp[-3].u.telement), (yyvsp[-2].u.decl), (yyvsp[0].u.attribute)); }
#line 3676 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 233:
#line 1122 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = finish_struct(start_struct((yyvsp[-4].u.itoken).location, kind_struct_ref, NULL),
				     (yyvsp[-2].u.decl), (yyvsp[0].u.attribute));
		}
#line 3684 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 234:
#line 1126 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = xref_tag((yyvsp[-1].u.itoken).location, kind_struct_ref, (yyvsp[0].u.word)); }
#line 3690 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 235:
#line 1128 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = start_struct ((yyvsp[-2].u.itoken).location, kind_union_ref, (yyvsp[-1].u.word)); }
#line 3696 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 236:
#line 1130 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = finish_struct((yyvsp[-3].u.telement), (yyvsp[-2].u.decl), (yyvsp[0].u.attribute)); }
#line 3702 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 237:
#line 1132 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = finish_struct(start_struct((yyvsp[-4].u.itoken).location, kind_union_ref, NULL),
				     (yyvsp[-2].u.decl), (yyvsp[0].u.attribute));
		}
#line 3710 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 238:
#line 1136 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = xref_tag((yyvsp[-1].u.itoken).location, kind_union_ref, (yyvsp[0].u.word)); }
#line 3716 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 239:
#line 1138 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = start_enum((yyvsp[-2].u.itoken).location, (yyvsp[-1].u.word)); }
#line 3722 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 240:
#line 1140 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = finish_enum((yyvsp[-4].u.telement), (yyvsp[-3].u.decl), (yyvsp[0].u.attribute)); }
#line 3728 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 241:
#line 1142 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = start_enum((yyvsp[-1].u.itoken).location, NULL); }
#line 3734 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 242:
#line 1144 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = finish_enum((yyvsp[-4].u.telement), (yyvsp[-3].u.decl), (yyvsp[0].u.attribute)); }
#line 3740 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 243:
#line 1146 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = xref_tag((yyvsp[-1].u.itoken).location, kind_enum_ref, (yyvsp[0].u.word)); }
#line 3746 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 247:
#line 1157 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic) pedwarn("comma at end of enumerator list"); }
#line 3752 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 248:
#line 1162 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[0].u.decl); }
#line 3758 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 249:
#line 1164 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-1].u.decl), (yyvsp[0].u.decl));
		  pedwarn("no semicolon at end of struct or union"); }
#line 3765 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 250:
#line 1169 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = NULL; }
#line 3771 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 251:
#line 1171 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-2].u.decl), (yyvsp[-1].u.decl)); }
#line 3777 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 252:
#line 1173 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    pedwarn("extra semicolon in struct or union specified"); 
		   (yyval.u.decl) = (yyvsp[-1].u.decl); }
#line 3785 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 253:
#line 1189 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-2].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[0].u.decl)));
		  pop_declspec_stack(); }
#line 3792 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 254:
#line 1192 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    pedwarn("ANSI C forbids member declarations with no members");
		  shadow_tag((yyvsp[-1].u.telement));
		  (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-1].u.telement)->location, current_declspecs, prefix_attributes, NULL));
		  pop_declspec_stack(); }
#line 3802 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 255:
#line 1198 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-2].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[0].u.decl)));
		  pop_declspec_stack(); }
#line 3809 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 256:
#line 1201 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    pedwarn("ANSI C forbids member declarations with no members");
		  shadow_tag((yyvsp[-1].u.telement));
		  (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[-1].u.telement)->location, current_declspecs, prefix_attributes, NULL));	
		  pop_declspec_stack(); }
#line 3819 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 257:
#line 1207 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location); }
#line 3825 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 258:
#line 1209 "c-parse.y" /* yacc.c:1646  */
    { pedantic = (yyvsp[-1].u.itoken).i;
		  (yyval.u.decl) = CAST(declaration, new_extension_decl(pr, (yyvsp[-1].u.itoken).location, (yyvsp[0].u.decl))); }
#line 3832 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 260:
#line 1216 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-2].u.decl), (yyvsp[0].u.decl)); }
#line 3838 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 261:
#line 1221 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = make_field((yyvsp[-1].u.declarator), NULL, current_declspecs,
				  (yyvsp[0].u.attribute), prefix_attributes); }
#line 3845 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 262:
#line 1224 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = make_field((yyvsp[-3].u.declarator), (yyvsp[-1].u.expr), current_declspecs,
				  (yyvsp[0].u.attribute), prefix_attributes); }
#line 3852 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 263:
#line 1227 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = make_field(NULL, (yyvsp[-1].u.expr), current_declspecs,
				  (yyvsp[0].u.attribute), prefix_attributes); }
#line 3859 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 265:
#line 1234 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-2].u.decl), (yyvsp[0].u.decl)); }
#line 3865 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 266:
#line 1236 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = NULL; }
#line 3871 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 267:
#line 1242 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = make_enumerator((yyvsp[0].idtoken).location, (yyvsp[0].idtoken).id, NULL); }
#line 3877 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 268:
#line 1244 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = make_enumerator((yyvsp[-2].idtoken).location, (yyvsp[-2].idtoken).id, (yyvsp[0].u.expr)); }
#line 3883 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 269:
#line 1249 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.type) = make_type((yyvsp[-1].u.telement), (yyvsp[0].u.declarator)); }
#line 3889 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 270:
#line 1251 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.type) = make_type((yyvsp[-1].u.telement), (yyvsp[0].u.declarator)); }
#line 3895 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 271:
#line 1256 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = NULL; }
#line 3901 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 274:
#line 1263 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), (yyvsp[0].u.telement)); }
#line 3907 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 275:
#line 1268 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = NULL; }
#line 3913 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 276:
#line 1270 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = type_element_chain((yyvsp[-1].u.telement), (yyvsp[0].u.telement)); }
#line 3919 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 277:
#line 1275 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = (yyvsp[-1].u.declarator); }
#line 3925 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 278:
#line 1278 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_pointer_declarator(pr, (yyvsp[-2].u.itoken).location, (yyvsp[0].u.declarator), (yyvsp[-1].u.telement))); }
#line 3931 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 279:
#line 1280 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_pointer_declarator(pr, (yyvsp[-1].u.itoken).location, NULL, (yyvsp[0].u.telement))); }
#line 3937 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 280:
#line 1282 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = make_function_declarator((yyvsp[-2].u.itoken).location, (yyvsp[-3].u.declarator), (yyvsp[-1].u.decl), (yyvsp[0].u.telement)); }
#line 3943 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 281:
#line 1284 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[-2].u.itoken).location, (yyvsp[-3].u.declarator), (yyvsp[-1].u.expr))); }
#line 3949 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 282:
#line 1286 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[-1].u.itoken).location, (yyvsp[-2].u.declarator), NULL)); }
#line 3955 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 283:
#line 1288 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = make_function_declarator((yyvsp[-2].u.itoken).location, NULL, (yyvsp[-1].u.decl), (yyvsp[0].u.telement)); }
#line 3961 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 284:
#line 1290 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[-2].u.itoken).location, NULL, (yyvsp[-1].u.expr))); }
#line 3967 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 285:
#line 1292 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[-1].u.itoken).location, NULL, NULL)); }
#line 3973 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 286:
#line 1303 "c-parse.y" /* yacc.c:1646  */
    {
		  if (pedantic && (yyvsp[0].u.istmt).i)
		    pedwarn("ANSI C forbids label at end of compound statement");
		  /* Add an empty statement to last label if stand-alone */
		  if ((yyvsp[0].u.istmt).i)
		    {
		      statement last_label = CAST(statement, last_node(CAST(node, (yyvsp[0].u.istmt).stmt)));

		      chain_with_labels(last_label, CAST(statement, new_empty_stmt(pr, last_label->location)));
		    }
		  (yyval.u.stmt) = (yyvsp[0].u.istmt).stmt;
		}
#line 3990 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 288:
#line 1320 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.istmt).i = (yyvsp[0].u.istmt).i; (yyval.u.istmt).stmt = chain_with_labels((yyvsp[-1].u.istmt).stmt, (yyvsp[0].u.istmt).stmt); }
#line 3996 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 289:
#line 1322 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.istmt).i = 0; (yyval.u.istmt).stmt = new_error_stmt(pr, last_location); }
#line 4002 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 290:
#line 1326 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = NULL; }
#line 4008 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 293:
#line 1334 "c-parse.y" /* yacc.c:1646  */
    { pushlevel(FALSE); }
#line 4014 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 294:
#line 1340 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.id_label) = NULL; }
#line 4020 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 295:
#line 1342 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    pedwarn("ANSI C forbids label declarations"); 
		  (yyval.u.id_label) = (yyvsp[0].u.id_label); }
#line 4028 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 297:
#line 1349 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.id_label) = id_label_chain((yyvsp[-1].u.id_label), (yyvsp[0].u.id_label)); }
#line 4034 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 298:
#line 1354 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.id_label) = (yyvsp[-1].u.id_label); }
#line 4040 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 300:
#line 1361 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = (yyvsp[0].u.stmt); }
#line 4046 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 301:
#line 1364 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.itoken) = (yyvsp[0].u.itoken); compstmt_count++; }
#line 4052 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 302:
#line 1367 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = CAST(statement, new_compound_stmt(pr, (yyvsp[-2].u.itoken).location, NULL, NULL, NULL, poplevel())); }
#line 4058 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 303:
#line 1369 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = CAST(statement, new_compound_stmt(pr, (yyvsp[-5].u.itoken).location, (yyvsp[-3].u.id_label), (yyvsp[-2].u.decl), (yyvsp[-1].u.stmt), poplevel())); }
#line 4064 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 304:
#line 1371 "c-parse.y" /* yacc.c:1646  */
    { poplevel();
		  (yyval.u.stmt) = new_error_stmt(pr, last_location); }
#line 4071 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 305:
#line 1374 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = CAST(statement, new_compound_stmt(pr, (yyvsp[-4].u.itoken).location, (yyvsp[-2].u.id_label), NULL, (yyvsp[-1].u.stmt), poplevel())); }
#line 4077 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 306:
#line 1380 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.istmt).stmt = CAST(statement, new_if_stmt(pr, (yyvsp[-1].u.iexpr).expr->location, (yyvsp[-1].u.iexpr).expr, (yyvsp[0].u.stmt), NULL));
		  (yyval.u.istmt).i = (yyvsp[-1].u.iexpr).i; }
#line 4084 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 307:
#line 1382 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.istmt).i = (yyvsp[-1].u.iexpr).i; (yyval.u.istmt).stmt = new_error_stmt(pr, last_location); }
#line 4090 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 308:
#line 1387 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.iexpr).i = stmt_count;
		  (yyval.u.iexpr).expr = (yyvsp[-1].u.expr);
		  check_condition("if", (yyvsp[-1].u.expr)); }
#line 4098 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 309:
#line 1397 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++;
		  compstmt_count++; 
		  (yyval.u.cstmt) = CAST(conditional_stmt,
				   new_dowhile_stmt(pr, (yyvsp[0].u.itoken).location, NULL, NULL));
		 push_loop(CAST(statement, (yyval.u.cstmt))); }
#line 4108 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 310:
#line 1403 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.cstmt) = (yyvsp[-2].u.cstmt); 
		  (yyval.u.cstmt)->stmt = (yyvsp[-1].u.stmt); }
#line 4115 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 311:
#line 1409 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = (yyvsp[0].u.stmt); }
#line 4121 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 312:
#line 1411 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = CAST(statement, new_labeled_stmt(pr, (yyvsp[-1].u.label)->location, (yyvsp[-1].u.label), (yyvsp[0].u.stmt))); }
#line 4127 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 313:
#line 1416 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.istmt).i = 0; (yyval.u.istmt).stmt = (yyvsp[0].u.stmt); }
#line 4133 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 314:
#line 1418 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.istmt).i = 1; (yyval.u.istmt).stmt = CAST(statement, new_labeled_stmt(pr, (yyvsp[0].u.label)->location, (yyvsp[0].u.label), NULL)); }
#line 4139 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 315:
#line 1424 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++; (yyval.u.stmt) = (yyvsp[0].u.stmt); }
#line 4145 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 316:
#line 1426 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_expression_stmt(pr, (yyvsp[-1].u.expr)->location, (yyvsp[-1].u.expr))); }
#line 4152 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 317:
#line 1429 "c-parse.y" /* yacc.c:1646  */
    { (yyvsp[-1].u.istmt).i = stmt_count; }
#line 4158 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 318:
#line 1431 "c-parse.y" /* yacc.c:1646  */
    { if (extra_warnings && stmt_count == (yyvsp[-3].u.istmt).i)
		    warning("empty body in an else-statement");
		  (yyval.u.stmt) = (yyvsp[-3].u.istmt).stmt;
		  CAST(if_stmt, (yyval.u.stmt))->stmt2 = (yyvsp[0].u.stmt);
		}
#line 4168 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 319:
#line 1437 "c-parse.y" /* yacc.c:1646  */
    { /* This warning is here instead of in simple_if, because we
		     do not want a warning if an empty if is followed by an
		     else statement.  Increment stmt_count so we don't
		     give a second error if this is a nested `if'.  */
		  if (extra_warnings && stmt_count++ == (yyvsp[0].u.istmt).i)
		    warning_with_location ((yyvsp[0].u.istmt).stmt->location,
					   "empty body in an if-statement");
		  (yyval.u.stmt) = (yyvsp[0].u.istmt).stmt; }
#line 4181 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 320:
#line 1446 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = new_error_stmt(pr, last_location); }
#line 4187 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 321:
#line 1448 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++; }
#line 4193 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 322:
#line 1450 "c-parse.y" /* yacc.c:1646  */
    { check_condition("while", (yyvsp[-1].u.expr)); 
		  (yyval.u.cstmt) = CAST(conditional_stmt,
			           new_while_stmt(pr, (yyvsp[-4].u.itoken).location, (yyvsp[-1].u.expr), NULL));
		  /* The condition is not "in the loop" for break or continue */
		  push_loop(CAST(statement, (yyval.u.cstmt))); }
#line 4203 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 323:
#line 1456 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = CAST(statement, (yyvsp[-1].u.cstmt));
		  (yyvsp[-1].u.cstmt)->stmt = (yyvsp[0].u.stmt); 
		  pop_loop(); }
#line 4211 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 324:
#line 1460 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = CAST(statement, (yyvsp[-4].u.cstmt));
		  (yyvsp[-4].u.cstmt)->condition = (yyvsp[-2].u.expr);
		  check_condition("do-while", (yyvsp[-2].u.expr)); 
		  /* Note that pop_loop should be before the expr to be consistent
		     with while, but GCC is inconsistent. See loop1.c */
		  pop_loop(); }
#line 4222 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 325:
#line 1467 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = new_error_stmt(pr, last_location); 
		  pop_loop(); }
#line 4229 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 326:
#line 1469 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++; }
#line 4235 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 327:
#line 1470 "c-parse.y" /* yacc.c:1646  */
    { if ((yyvsp[-1].u.expr)) check_condition("for", (yyvsp[-1].u.expr)); }
#line 4241 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 328:
#line 1472 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.for_stmt) = new_for_stmt(pr, (yyvsp[-9].u.itoken).location, (yyvsp[-7].u.expr), (yyvsp[-4].u.expr), (yyvsp[-1].u.expr), NULL);
		  push_loop(CAST(statement, (yyval.u.for_stmt))); }
#line 4248 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 329:
#line 1475 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = CAST(statement, (yyvsp[-1].u.for_stmt));
		  (yyvsp[-1].u.for_stmt)->stmt = (yyvsp[0].u.stmt); 
		  pop_loop(); }
#line 4256 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 330:
#line 1479 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++; check_switch((yyvsp[-1].u.expr)); 
		  (yyval.u.cstmt) = CAST(conditional_stmt,
			           new_switch_stmt(pr, (yyvsp[-3].u.itoken).location, (yyvsp[-1].u.expr), NULL)); 
		  push_loop(CAST(statement, (yyval.u.cstmt))); }
#line 4265 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 331:
#line 1484 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = CAST(statement, (yyvsp[-1].u.cstmt)); 
		  (yyvsp[-1].u.cstmt)->stmt = (yyvsp[0].u.stmt);
		  pop_loop(); }
#line 4273 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 332:
#line 1488 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_break_stmt(pr, (yyvsp[-1].u.itoken).location));
		  check_break((yyval.u.stmt));
		}
#line 4282 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 333:
#line 1493 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_continue_stmt(pr, (yyvsp[-1].u.itoken).location));
		  check_continue((yyval.u.stmt));
		}
#line 4291 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 334:
#line 1498 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_return_stmt(pr, (yyvsp[-1].u.itoken).location, NULL)); 
		  check_void_return(); }
#line 4299 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 335:
#line 1502 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_return_stmt(pr, (yyvsp[-2].u.itoken).location, (yyvsp[-1].u.expr))); 
		  check_return((yyvsp[-1].u.expr)); }
#line 4307 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 336:
#line 1506 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_asm_stmt(pr, (yyvsp[-5].u.itoken).location, (yyvsp[-2].u.expr), NULL,
					       NULL, NULL, (yyvsp[-4].u.telement))); }
#line 4315 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 337:
#line 1511 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_asm_stmt(pr, (yyvsp[-7].u.itoken).location, (yyvsp[-4].u.expr), (yyvsp[-2].u.asm_operand), NULL,
					       NULL, (yyvsp[-6].u.telement))); }
#line 4323 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 338:
#line 1516 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_asm_stmt(pr, (yyvsp[-9].u.itoken).location, (yyvsp[-6].u.expr), (yyvsp[-4].u.asm_operand), (yyvsp[-2].u.asm_operand), NULL, (yyvsp[-8].u.telement))); }
#line 4330 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 339:
#line 1521 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_asm_stmt(pr, (yyvsp[-11].u.itoken).location, (yyvsp[-8].u.expr), (yyvsp[-6].u.asm_operand), (yyvsp[-4].u.asm_operand), (yyvsp[-2].u.string), (yyvsp[-10].u.telement))); }
#line 4337 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 340:
#line 1524 "c-parse.y" /* yacc.c:1646  */
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_goto_stmt(pr, (yyvsp[-2].u.itoken).location, (yyvsp[-1].u.id_label)));
		  use_label((yyvsp[-1].u.id_label));
		}
#line 4346 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 341:
#line 1529 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    pedwarn("ANSI C forbids `goto *expr;'");
		  stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_computed_goto_stmt(pr, (yyvsp[-3].u.itoken).location, (yyvsp[-1].u.expr))); 
		  check_computed_goto((yyvsp[-1].u.expr)); }
#line 4356 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 342:
#line 1534 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.stmt) = CAST(statement, new_empty_stmt(pr, (yyvsp[0].u.itoken).location)); }
#line 4362 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 343:
#line 1542 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.label) = CAST(label, new_case_label(pr, (yyvsp[-2].u.itoken).location, (yyvsp[-1].u.expr), NULL)); 
		  check_case((yyval.u.label)); }
#line 4369 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 344:
#line 1545 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.label) = CAST(label, new_case_label(pr, (yyvsp[-4].u.itoken).location, (yyvsp[-3].u.expr), (yyvsp[-1].u.expr))); 
		  check_case((yyval.u.label)); }
#line 4376 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 345:
#line 1548 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.label) = CAST(label, new_default_label(pr, (yyvsp[-1].u.itoken).location)); 
		  check_default((yyval.u.label)); }
#line 4383 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 346:
#line 1551 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.label) = CAST(label, (yyvsp[-1].u.id_label)); 
		  define_label((yyvsp[-1].u.id_label)); }
#line 4390 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 347:
#line 1559 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = NULL; }
#line 4396 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 349:
#line 1565 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.expr) = NULL; }
#line 4402 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 351:
#line 1572 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.asm_operand) = NULL; }
#line 4408 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 354:
#line 1579 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.asm_operand) = asm_operand_chain((yyvsp[-2].u.asm_operand), (yyvsp[0].u.asm_operand)); }
#line 4414 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 355:
#line 1584 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.asm_operand) = new_asm_operand(pr, (yyvsp[-3].u.expr)->location,
				       make_string((yyvsp[-3].u.expr)->location, CAST(expression, (yyvsp[-3].u.expr))),
				       (yyvsp[-1].u.expr));  }
#line 4422 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 356:
#line 1591 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.string) = (yyvsp[0].u.string); }
#line 4428 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 357:
#line 1593 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.string) = string_chain((yyvsp[-2].u.string), (yyvsp[0].u.string)); }
#line 4434 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 358:
#line 1599 "c-parse.y" /* yacc.c:1646  */
    { pushlevel(TRUE); }
#line 4440 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 359:
#line 1601 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[0].u.decl);
		  /* poplevel() is done when building the declarator */
		}
#line 4448 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 360:
#line 1607 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[-1].u.decl); }
#line 4454 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 361:
#line 1609 "c-parse.y" /* yacc.c:1646  */
    { if (pedantic)
		    pedwarn("ANSI C forbids forward parameter declarations");
#if 0
		  /* Mark the forward decls as such.  */
		  for (parm = getdecls (); parm; parm = TREE_CHAIN (parm))
		    TREE_ASM_WRITTEN (parm) = 1;
#endif
		  }
#line 4467 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 362:
#line 1618 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[0].u.decl); }
#line 4473 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 363:
#line 1620 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location); }
#line 4479 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 364:
#line 1626 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = NULL; }
#line 4485 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 365:
#line 1628 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = new_error_decl(pr, last_location);
		  /* Gcc used to allow this as an extension.  However, it does
		     not work for all targets, and thus has been disabled.
		     Also, since func (...) and func () are indistinguishable,
		     it caused problems with the code in expand_builtin which
		     tries to verify that BUILT_IN_NEXT_ARG is being used
		     correctly.  */
		  error("ANSI C requires a named argument before `...'");
		}
#line 4499 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 366:
#line 1638 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[0].u.decl); }
#line 4505 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 367:
#line 1640 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-2].u.decl), CAST(declaration, new_ellipsis_decl(pr, (yyvsp[0].u.itoken).location))); }
#line 4511 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 369:
#line 1646 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-2].u.decl), (yyvsp[0].u.decl)); }
#line 4517 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 370:
#line 1653 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declare_parameter((yyvsp[-1].u.declarator), current_declspecs, (yyvsp[0].u.attribute),
					 prefix_attributes, FALSE);
		  pop_declspec_stack(); }
#line 4525 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 371:
#line 1657 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declare_parameter((yyvsp[-1].u.declarator), current_declspecs, (yyvsp[0].u.attribute),
					 prefix_attributes, FALSE);
		  pop_declspec_stack(); }
#line 4533 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 372:
#line 1661 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declare_parameter((yyvsp[-1].u.declarator), current_declspecs, (yyvsp[0].u.attribute),
					 prefix_attributes, TRUE);
		pop_declspec_stack(); }
#line 4541 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 373:
#line 1665 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declare_parameter((yyvsp[-1].u.declarator), current_declspecs, (yyvsp[0].u.attribute),
					 prefix_attributes, FALSE);
		  pop_declspec_stack(); }
#line 4549 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 374:
#line 1669 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declare_parameter((yyvsp[-1].u.declarator), current_declspecs, (yyvsp[0].u.attribute),
					 prefix_attributes, TRUE);
		  pop_declspec_stack(); }
#line 4557 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 375:
#line 1678 "c-parse.y" /* yacc.c:1646  */
    { pushlevel(TRUE); }
#line 4563 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 376:
#line 1680 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[0].u.decl);
		  /* poplevel is done when building the declarator */ }
#line 4570 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 378:
#line 1686 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[-1].u.decl); }
#line 4576 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 379:
#line 1692 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = (yyvsp[0].u.decl); }
#line 4582 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 380:
#line 1694 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declaration_chain((yyvsp[-2].u.decl), (yyvsp[0].u.decl)); }
#line 4588 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 381:
#line 1698 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.decl) = declare_old_parameter((yyvsp[0].idtoken).location, (yyvsp[0].idtoken).id); }
#line 4594 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 382:
#line 1703 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.id_label) = (yyvsp[0].u.id_label); declare_label((yyvsp[0].u.id_label)); }
#line 4600 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 383:
#line 1705 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.id_label) = id_label_chain((yyvsp[-2].u.id_label), (yyvsp[0].u.id_label));
		  declare_label((yyvsp[0].u.id_label)); }
#line 4607 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 384:
#line 1711 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = NULL; }
#line 4613 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 385:
#line 1712 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = (yyvsp[0].u.telement); }
#line 4619 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 386:
#line 1717 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.itoken).location = (yyvsp[0].u.itoken).location;
		  (yyval.u.itoken).i = pedantic;
		  pedantic = 0; }
#line 4627 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 387:
#line 1723 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = CAST(type_element, new_rid(pr, (yyvsp[0].u.itoken).location, (yyvsp[0].u.itoken).i)); }
#line 4633 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 388:
#line 1727 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = CAST(type_element, new_qualifier(pr, (yyvsp[0].u.itoken).location, (yyvsp[0].u.itoken).i)); }
#line 4639 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 389:
#line 1731 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = CAST(type_element, new_qualifier(pr, (yyvsp[0].u.itoken).location, (yyvsp[0].u.itoken).i)); }
#line 4645 "c-parse.tab.c" /* yacc.c:1646  */
    break;

  case 390:
#line 1735 "c-parse.y" /* yacc.c:1646  */
    { (yyval.u.telement) = CAST(type_element, new_rid(pr, (yyvsp[0].u.itoken).location, (yyvsp[0].u.itoken).i)); }
#line 4651 "c-parse.tab.c" /* yacc.c:1646  */
    break;


#line 4655 "c-parse.tab.c" /* yacc.c:1646  */
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
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
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

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

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

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
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
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
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
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 1739 "c-parse.y" /* yacc.c:1906  */

