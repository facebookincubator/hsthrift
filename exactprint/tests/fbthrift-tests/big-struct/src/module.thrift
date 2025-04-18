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

namespace java.swift test.fixtures.basic

include "thrift/annotation/java.thrift"

struct MyNomralStruct {
  1: string msg1;
  2: string msg2;
  3: string msg3;
  4: string msg4;
  5: string msg5;
  6: string msg6;
  7: string msg7;
  8: string msg8;
  9: string msg9;
  10: i32 myIntField10;
  11: i32 myIntField11;
  12: i32 myIntField12;
  13: i32 myIntField13;
  14: i32 myIntField14;
  15: i32 myIntField15;
  16: i32 myIntField16;
  17: i32 myIntField17;
  18: i32 myIntField18;
  19: i32 myIntField19;
  20: i32 myIntField20;
  21: double myDoubleField21;
  22: double myDoubleField22;
  23: double myDoubleField23;
  24: double myDoubleField24;
  25: double myDoubleField25;
  26: double myDoubleField26;
  27: double myDoubleField27;
  28: double myDoubleField28;
  29: double myDoubleField29;
  30: double myDoubleField30;
}

struct MyBigStruct {
  1: string msg1;
  2: string msg2;
  3: string msg3;
  4: string msg4;
  5: string msg5;
  6: string msg6;
  7: string msg7;
  8: string msg8;
  9: string msg9;
  10: i32 myIntField10;
  11: i32 myIntField11;
  12: i32 myIntField12;
  13: i32 myIntField13;
  14: i32 myIntField14;
  15: i32 myIntField15;
  16: i32 myIntField16;
  17: i32 myIntField17;
  18: i32 myIntField18;
  19: i32 myIntField19;
  20: i32 myIntField20;
  21: double myDoubleField21;
  22: double myDoubleField22;
  23: double myDoubleField23;
  24: double myDoubleField24;
  25: double myDoubleField25;
  26: double myDoubleField26;
  27: double myDoubleField27;
  28: double myDoubleField28;
  29: double myDoubleField29;
  30: double myDoubleField30;
  31: string msg31;
  32: string msg32;
  33: string msg33;
  34: string msg34;
  35: string msg35;
  36: string msg36;
  37: string msg37;
  38: string msg38;
  39: string msg39;
  40: string msg40;
  41: string msg41;
  42: string msg42;
  43: string msg43;
  44: string msg44;
  45: string msg45;
  46: string msg46;
  47: string msg47;
  48: string msg48;
  49: string msg49;
  50: string msg50;
  51: string msg51;
  52: string msg52;
  53: string msg53;
  54: string msg54;
  55: string msg55;
  56: string msg56;
  57: string msg57;
  58: string msg58;
  59: string msg59;
  60: string msg60;
  61: string msg61;
  62: string msg62;
  63: string msg63;
  64: string msg64;
  65: string msg65;
  66: string msg66;
  67: string msg67;
  68: string msg68;
  69: string msg69;
  70: string msg70;
  71: string msg71;
  72: string msg72;
  73: string msg73;
  74: string msg74;
  75: string msg75;
  76: string msg76;
  77: string msg77;
  78: string msg78;
  79: string msg79;
  80: string msg80;
  81: string msg81;
  82: string msg82;
  83: string msg83;
  84: string msg84;
  85: string msg85;
  86: string msg86;
  87: string msg87;
  88: string msg88;
  89: string msg89;
  90: string msg90;
  91: string msg91;
  92: string msg92;
  93: string msg93;
  94: string msg94;
  95: string msg95;
  96: string msg96;
  97: string msg97;
  98: string msg98;
  99: string msg99;
  100: string msg100;
  101: string msg101;
  102: string msg102;
  103: string msg103;
  104: string msg104;
  105: string msg105;
  106: string msg106;
  107: string msg107;
  108: string msg108;
  109: string msg109;
  110: string msg110;
  111: string msg111;
  112: string msg112;
  113: string msg113;
  114: string msg114;
  115: string msg115;
  116: string msg116;
  117: string msg117;
  118: string msg118;
  119: string msg119;
  120: string msg120;
  121: string msg121;
  122: string msg122;
  123: string msg123;
  124: string msg124;
  125: string msg125;
  126: string msg126;
  127: string msg127;
  128: string msg128;
  129: string msg129;
  130: string msg130;
  131: string msg131;
  132: string msg132;
  133: string msg133;
  134: string msg134;
  135: string msg135;
  136: string msg136;
  137: string msg137;
  138: string msg138;
  139: string msg139;
  140: string msg140;
  141: string msg141;
  142: string msg142;
  143: string msg143;
  144: string msg144;
  145: string msg145;
  146: string msg146;
  147: string msg147;
  148: string msg148;
  149: string msg149;
  150: string msg150;
  151: string msg151;
  152: string msg152;
  153: string msg153;
  154: string msg154;
  155: string msg155;
  156: string msg156;
  157: string msg157;
  158: string msg158;
  159: string msg159;
  160: string msg160;
  161: string msg161;
  162: string msg162;
  163: string msg163;
  164: string msg164;
  165: string msg165;
  166: string msg166;
  167: string msg167;
  168: string msg168;
  169: string msg169;
  170: string msg170;
  171: string msg171;
  172: string msg172;
  173: string msg173;
  174: string msg174;
  175: string msg175;
  176: string msg176;
  177: string msg177;
  178: string msg178;
  179: string msg179;
  180: string msg180;
  181: string msg181;
  182: string msg182;
  183: string msg183;
  184: string msg184;
  185: string msg185;
  186: string msg186;
  187: string msg187;
  188: string msg188;
  189: string msg189;
  190: string msg190;
  191: string msg191;
  192: string msg192;
  193: string msg193;
  194: string msg194;
  195: string msg195;
  196: string msg196;
  197: string msg197;
  198: string msg198;
  199: string msg199;
  200: string msg200;
  201: string msg201;
  202: string msg202;
  203: string msg203;
  204: string msg204;
  205: string msg205;
  206: string msg206;
  207: string msg207;
  208: string msg208;
  209: string msg209;
  210: string msg210;
  211: string msg211;
  212: string msg212;
  213: string msg213;
  214: string msg214;
  215: string msg215;
  216: string msg216;
  217: string msg217;
  218: string msg218;
  219: string msg219;
  220: string msg220;
  221: string msg221;
  222: string msg222;
  223: string msg223;
  224: string msg224;
  225: string msg225;
  226: string msg226;
  227: string msg227;
  228: string msg228;
  229: string msg229;
  230: string msg230;
  231: string msg231;
  232: string msg232;
  233: string msg233;
  234: string msg234;
  235: string msg235;
  236: string msg236;
  237: string msg237;
  238: string msg238;
  239: string msg239;
  240: string msg240;
  241: string msg241;
  242: string msg242;
  243: string msg243;
  244: string msg244;
  245: string msg245;
  246: string msg246;
  247: string msg247;
  248: string msg248;
  249: string msg249;
  250: string msg250;
  251: string msg251;
  252: string msg252;
  253: string msg253;
  254: string msg254;
  255: string msg255;
  256: string msg256;
  257: string msg257;
}

@java.Mutable
struct MyMutableBigStruct {
  1: string msg1;
  2: string msg2;
  3: string msg3;
  4: string msg4;
  5: string msg5;
  6: string msg6;
  7: string msg7;
  8: string msg8;
  9: string msg9;
  10: i32 myIntField10;
  11: i32 myIntField11;
  12: i32 myIntField12;
  13: i32 myIntField13;
  14: i32 myIntField14;
  15: i32 myIntField15;
  16: i32 myIntField16;
  17: i32 myIntField17;
  18: i32 myIntField18;
  19: i32 myIntField19;
  20: i32 myIntField20;
  21: double myDoubleField21;
  22: double myDoubleField22;
  23: double myDoubleField23;
  24: double myDoubleField24;
  25: double myDoubleField25;
  26: double myDoubleField26;
  27: double myDoubleField27;
  28: double myDoubleField28;
  29: double myDoubleField29;
  30: double myDoubleField30;
  31: string msg31;
  32: string msg32;
  33: string msg33;
  34: string msg34;
  35: string msg35;
  36: string msg36;
  37: string msg37;
  38: string msg38;
  39: string msg39;
  40: string msg40;
  41: string msg41;
  42: string msg42;
  43: string msg43;
  44: string msg44;
  45: string msg45;
  46: string msg46;
  47: string msg47;
  48: string msg48;
  49: string msg49;
  50: string msg50;
  51: string msg51;
  52: string msg52;
  53: string msg53;
  54: string msg54;
  55: string msg55;
  56: string msg56;
  57: string msg57;
  58: string msg58;
  59: string msg59;
  60: string msg60;
  61: string msg61;
  62: string msg62;
  63: string msg63;
  64: string msg64;
  65: string msg65;
  66: string msg66;
  67: string msg67;
  68: string msg68;
  69: string msg69;
  70: string msg70;
  71: string msg71;
  72: string msg72;
  73: string msg73;
  74: string msg74;
  75: string msg75;
  76: string msg76;
  77: string msg77;
  78: string msg78;
  79: string msg79;
  80: string msg80;
  81: string msg81;
  82: string msg82;
  83: string msg83;
  84: string msg84;
  85: string msg85;
  86: string msg86;
  87: string msg87;
  88: string msg88;
  89: string msg89;
  90: string msg90;
  91: string msg91;
  92: string msg92;
  93: string msg93;
  94: string msg94;
  95: string msg95;
  96: string msg96;
  97: string msg97;
  98: string msg98;
  99: string msg99;
  100: string msg100;
  101: string msg101;
  102: string msg102;
  103: string msg103;
  104: string msg104;
  105: string msg105;
  106: string msg106;
  107: string msg107;
  108: string msg108;
  109: string msg109;
  110: string msg110;
  111: string msg111;
  112: string msg112;
  113: string msg113;
  114: string msg114;
  115: string msg115;
  116: string msg116;
  117: string msg117;
  118: string msg118;
  119: string msg119;
  120: string msg120;
  121: string msg121;
  122: string msg122;
  123: string msg123;
  124: string msg124;
  125: string msg125;
  126: string msg126;
  127: string msg127;
  128: string msg128;
  129: string msg129;
  130: string msg130;
  131: string msg131;
  132: string msg132;
  133: string msg133;
  134: string msg134;
  135: string msg135;
  136: string msg136;
  137: string msg137;
  138: string msg138;
  139: string msg139;
  140: string msg140;
  141: string msg141;
  142: string msg142;
  143: string msg143;
  144: string msg144;
  145: string msg145;
  146: string msg146;
  147: string msg147;
  148: string msg148;
  149: string msg149;
  150: string msg150;
  151: string msg151;
  152: string msg152;
  153: string msg153;
  154: string msg154;
  155: string msg155;
  156: string msg156;
  157: string msg157;
  158: string msg158;
  159: string msg159;
  160: string msg160;
  161: string msg161;
  162: string msg162;
  163: string msg163;
  164: string msg164;
  165: string msg165;
  166: string msg166;
  167: string msg167;
  168: string msg168;
  169: string msg169;
  170: string msg170;
  171: string msg171;
  172: string msg172;
  173: string msg173;
  174: string msg174;
  175: string msg175;
  176: string msg176;
  177: string msg177;
  178: string msg178;
  179: string msg179;
  180: string msg180;
  181: string msg181;
  182: string msg182;
  183: string msg183;
  184: string msg184;
  185: string msg185;
  186: string msg186;
  187: string msg187;
  188: string msg188;
  189: string msg189;
  190: string msg190;
  191: string msg191;
  192: string msg192;
  193: string msg193;
  194: string msg194;
  195: string msg195;
  196: string msg196;
  197: string msg197;
  198: string msg198;
  199: string msg199;
  200: string msg200;
  201: string msg201;
  202: string msg202;
  203: string msg203;
  204: string msg204;
  205: string msg205;
  206: string msg206;
  207: string msg207;
  208: string msg208;
  209: string msg209;
  210: string msg210;
  211: string msg211;
  212: string msg212;
  213: string msg213;
  214: string msg214;
  215: string msg215;
  216: string msg216;
  217: string msg217;
  218: string msg218;
  219: string msg219;
  220: string msg220;
  221: string msg221;
  222: string msg222;
  223: string msg223;
  224: string msg224;
  225: string msg225;
  226: string msg226;
  227: string msg227;
  228: string msg228;
  229: string msg229;
  230: string msg230;
  231: string msg231;
  232: string msg232;
  233: string msg233;
  234: string msg234;
  235: string msg235;
  236: string msg236;
  237: string msg237;
  238: string msg238;
  239: string msg239;
  240: string msg240;
  241: string msg241;
  242: string msg242;
  243: string msg243;
  244: string msg244;
  245: string msg245;
  246: string msg246;
  247: string msg247;
  248: string msg248;
  249: string msg249;
  250: string msg250;
  251: string msg251;
  252: string msg252;
  253: string msg253;
  254: string msg254;
  255: string msg255;
  256: string msg256;
  257: string msg257;
}
