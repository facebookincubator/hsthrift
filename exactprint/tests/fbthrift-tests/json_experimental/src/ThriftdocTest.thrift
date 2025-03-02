// THIS FILE IS COPIED FROM FBTHRIFT, DO NOT MODIFY ITS CONTENTS DIRECTLY
// generated-by : fbcode/common/hs/thrift/exactprint/tests/sync-fbthrift-tests.sh
// source: xplat/thrift/compiler/test/fixtures/*
// @generated
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * Program doctext.
 *
 * Seriously, this is the documentation for this whole program.
 */

namespace java thrift.test
namespace cpp thrift.test
namespace json thrift.test.doc
namespace py thrift.compiler.test.fixtures.json_experimental.src.ThriftdocTest

// C++ comment
/* c style comment */

# the new unix comment

enum NoDoc {
  NO = 1,
  DOC = 2,
}

/** Some doc text goes here.  Wow I am [nesting these] (no more nesting.) */
enum Numberz {
  /** This is how to document a parameter */
  ONE = 1,

  /** And this is a doc for a parameter that has no specific value assigned */
  TWO = 2,

  THREE = 3,
  FIVE = 5,
  SIX = 6,
  EIGHT = 8,
}

/** This is how you would do a typedef doc */
typedef i64 UserId

/**
  Test JSON "escaping" (e.g., forwardslash /, backwardslash \, tab , newline)
  and some default values
*/
struct DefaultValue {
  // 1: map<map<i32, i32>, i32> map_one = {{1:2} : 1},
  2: map<string, list<i32>> map_two = {"list1": [1, 2, 3], "list2": [4, 5, 6]};
  3: map<i32, map<string, string>> map_three = {1: {"a": "one"}};
}

/** And this is where you would document a struct */
struct Xtruct {
  /** And the members of a struct */
  1: string string_thing = "";

  /** doct text goes before a comma */
  4: byte byte_thing;

  9: i32 i32_thing;
  11: i64 i64_thing;
}

/**
 * You can document constants now too.  Yeehaw!
 */
const i32 INT32CONSTANT = 9853;
const i16 INT16CONSTANT = 1616;
/** Everyone get in on the docu-action! */
const map<string, string> MAPCONSTANT = {'hello': 'world', 'goodnight': 'moon'};

struct Xtruct2 {
  1: byte byte_thing;
  2: Xtruct struct_thing;
  3: i32 i32_thing;
}

/** Struct insanity */
struct Insanity {
  /** This is doc for field 1 */
  1: map<Numberz, UserId> userMap;

  /** And this is doc for field 2 */
  2: list<Xtruct> xtructs;
}

exception Xception {
  1: i32 errorCode;
  2: string message;
}

exception Xception2 {
  1: i32 errorCode;
  2: Xtruct struct_thing;
}

/* C1 */
/**  */
/* C2 */
/* C3 */
struct EmptyStruct {}

struct OneField {
  1: EmptyStruct field;
}

/** This is where you would document a Service */
service ThriftTest {
  /** And this is how you would document functions in a service */
  void testVoid();
  string testString(1: string thing);
  byte testByte(1: byte thing);
  i32 testI32(1: i32 thing);

  /** Like this one */
  i64 testI64(1: i64 thing);
  double testDouble(1: double thing);
  Xtruct testStruct(1: Xtruct thing);
  Xtruct2 testNest(1: Xtruct2 thing);
  map<i32, i32> testMap(1: map<i32, i32> thing);
  set<i32> testSet(1: set<i32> thing);
  list<i32> testList(1: list<i32> thing);

  /** This is an example of a function with params documented */
  Numberz testEnum(
    /** This param is a thing */
    1: Numberz thing,
  );

  UserId testTypedef(1: UserId thing);

  map<i32, map<i32, i32>> testMapMap(1: i32 hello);

  /* So you think you've got this all worked, out eh? */
  map<UserId, map<Numberz, Insanity>> testInsanity(1: Insanity argument);
}

/// This style of Doxy-comment doesn't work.
typedef i32 SorryNoGo

/**
 * This is a trivial example of a multiline docstring.
 */
typedef i32 TrivialMultiLine

/**
 * This is the cannonical example
 * of a multiline docstring.
 */
typedef i32 StandardMultiLine

/**
 * The last line is non-blank.
 * I said non-blank! */
typedef i32 LastLine

/** Both the first line
 * are non blank. ;-)
 * and the last line */
typedef i32 FirstAndLastLine

/**
 *    INDENTED TITLE
 * The text is less indented.
 */
typedef i32 IndentedTitle

/**       First line indented.
 * Unfortunately, this does not get indented.
 */
typedef i32 FirstLineIndent

/**
 * void code_in_comment() {
 *   printf("hooray code!");
 * }
 */
typedef i32 CodeInComment

/**
     * Indented Docstring.
     * This whole docstring is indented.
     *   This line is indented further.
     */
typedef i32 IndentedDocstring

/** Irregular docstring.
 * We will have to punt
  * on this thing */
typedef i32 Irregular1

/**
 * note the space
 * before these lines
* but not this
 * one
 */
typedef i32 Irregular2

/**
* Flush against
* the left.
*/
typedef i32 Flush

/**
  No stars in this one.
  It should still work fine, though.
    Including indenting.
    */
typedef i32 NoStars

/** Trailing whitespace
Sloppy trailing whitespace
is truncated.   */
typedef i32 TrailingWhitespace

/**
 * This is a big one.
 *
 * We'll have some blank lines in it.
 *
 * void as_well_as(some code) {
 *   puts("YEEHAW!");
 * }
 */
typedef i32 BigDog

/**
*
*
*/
typedef i32 TotallyDegenerate

/**
    A bool constant, experimental will render it as string
*/
const bool BOOLCONSTANT = true;

/**
    Simple list
*/
const list<BigDog> SIMPLE_LIST = [42, 73];

/**
    Complicated list
*/
const list<list<string>> COMPLICATED_LIST = [
  ["Rick", "Morty"],
  ["Brian", "Stewie"],
];

/**
   Simple map
*/
const map<string, string> SIMPLE_MAP = {"Rick": "Genius", "Morty": "Not realy"};

/**
   Complicated map
*/
const map<string, list<string>> COMPLICATED_MAP = {
  "Witcher": ["Geralt", "Yennefer"],
  "1984": ["2+2=5"],
};

/**
  String const split on multiple lines by '\'
*/
const string MULTI_LINE_STR = "Line 1
Line 2
...\
Last";

/**
  String with escapes an unicodes.
  Basic testing of https://www.ietf.org/rfc/rfc4627.txt
*/
const string WEIRD_STR = "How much is 15\u00c2\u00b0C in \\ F";
/* THE END */
