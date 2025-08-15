-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe-uxs.ads
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  LibreFrame framework - UTF-8 Strings package
--
--  @description
--
--  @authors
--  Gautier de Montmollin - gdm - krikos@bluewin.ch
--  Stéphane Rivière - sr - dev@soweb.io
--  Xavier Petit - xp - dev@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

package LibreFrame.Uxs is

   package ACH renames Ada.Characters.Handling;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   function Char_Count (String_To_Process : UString; Char_Set_Pattern : UString) return Integer;
   --  Count each char in String_To_Process relative to Char_Set_Pattern.

   function Ends_With (Item : UString; Pattern : ASCII_Character) return Boolean;
   --  Check if String Item ends with another String or String Pattern.

   function Field_By_Index (String_Input : UString; Index_Field : Integer; Field_Delimiter : UString) return UString;
   --  Return a field indexed by Index_Field and delimited by Field_Delimiter
   --  from String_To_Process.

   function Field_By_Name (String_Input : UString; Field_To_Search : UString; Field_Delimiter : UString) return UString;
   --  Return a field from a search string and delimited by Field_Delimiter.

   function Field_Count (String_To_Process : UString; Field_Delimiter : UString) return Integer;
   --  Count fields in String_To_Process and return fields number.

   procedure Field_Display (String_To_Process : UString; Column_Delimiter : UString; Row_Delimiter : UString; Custom_Header : UString := "");
   --  Formatted display of a string fields structured in rows and columns

   function Field_Get_Data (Datas : UString; Field_Name : UString) return UString;
   --  Return space trimmed datas from Field_Name identifier in Datas or an empty string if  Field_Name not found.

   function Field_Included (String_To_Process : UString; Items_List  : UString; Field_Delimiter : UString) return Boolean;
   --  Returns True if all Items_List are included in String_To_Process list, which is delimited by Field_Delimiter.

   function Field_Search (String_Input : UString; Field_To_Search : UString; Field_Delimiter : UString) return Boolean;
   --  Search Field_To_Search in String_To_Process and return True if found.

   function From_DB_To_Date_String (DB_Value : UString; Separator : UString := "/") return UString;
   --  Converts a ISO 8601 YYYY-MM-DD string to DD/MM/YYYY string with optional separator replacement.

   function From_DB_To_Money (DB_Value : UString) return Money;
   --  Converts a String (as an image of type Bigint in database) into a Money type.

   function From_DB_To_Money_String (DB_Value : UString) return UString;
   --  Converts a string (as an image of  type Bigint in database) into a string (as an image of type Money).
   --  Bigint (Long_Long_Integer) is used for accurate storage in database.
   --
   --  Examples:
   --   000   =>    0.00
   --   001   =>    0.01
   --   012   =>    0.12
   --   12312 =>  123.12
   --  -001   =>   -0.01
   --  -012   =>   -0.12
   --  -12312 => -123.12

   function From_DB_To_Money_String_With_Padding_Sign (DB_Value : UString) return UString;
   --  Converts a string (as an image of  type Bigint in database) into a string (as an image of type Money).
   --  With positive and invisible padding sign to keep vertical alignment.
   --  Bigint (Long_Long_Integer) is used for accurate storage in database.

   --  function From_Money (DB_Money : Money) return Long_Long_Integer;
   --  function From_Money (DB_Money : Money) return String;
   --  --  Converts Money to Bigint database storage.
   --  --  Converts Money to String.
   --  --  Bigint (Long_Long_Integer) is used for accurate storage in database.

   function From_Money_To_DB (DB_Value : Money) return UString;
   function From_Money_To_DB (DB_Value : UString) return UString;
   --  Converts a String (as an image of type Money) into a String compatible with storage as a Bigint in database.
   --  Bigint (Long_Long_Integer) is used for accurate storage in database.
   --
   --  Examples:
   --  123        =>  12300
   --  123.       =>  12300
   --  123.1      =>  12301
   --  123.12     =>  12312
   --  123.123456 =>  12312
   --     .1      =>  001
   --     .12     =>  012
   --     .123456 =>  012
   --   -0.123456 => -012
   --    -.123456 => -012
   --    -.       =>  000
   --    -        =>  000

   function Is_Numeric (Item : UString; Signs : UString := "") return Boolean;
   --  Returns True if Item string is numeric (i.e. contains only digits with or without leading signs like space, plus ou minus).

   function Padding_Left (String_To_Process : UString; Padding_Character : UString; Result_Length : Positive) return UString;
   --  Left padding untill Result_Length a String_To_Process with a Padding_Character.

   function Replace_Char (String_To_Process : UString; Char_In : ASCII_Character; Char_Out : ASCII_Character) return UString;
   --  Replace all Char_In by Char_Out in String_To_Process

   function Starts_With (Item : UString; Pattern : ASCII_Character) return Boolean;
   --  Check if String Item starts with another String or String Pattern.

   function Stript_Chars (String_To_Process : UString; Char_List : UString) return UString;
   --  Stript each char in String_To_Process relative to Char_List

   function Tail_After_Match (Source : UString; Pattern : ASCII_Character) return UString;
   function Tail_After_Match (Source : UString; Pattern : UString) return UString;
   --  Extract a String from Source starting from Pattern+1 position to the
   --  end. Returns a String
   --
   --  Examples : Tail_After_Match (+"/etc/genesix/gnx-startup",
   --    "/")) returns "gnx-startup"
   --    "ix")) returns "/gnx-startup"
   --    "gene")) returns "six/gnx-startup"
   --    "etc/genesix/gnx-startu")) returns "p"
   --    "/etc/genesix/gnx-startu")) returns "p"
   --    "/etc/genesix/gnx-startup")) returns empty string
   --    "/etc/genesix/gnx-startupp")) returns empty string

   function To_Hex (Byte : Unsigned_8) return UString;
   --  Convert a Byte to a String hexadecimal output.

   function To_Hex (String_In : UString) return UString;
   --  Convert a String to a String hexadecimal formatted output.

   function To_Hex_From_Val (Input : UString) return UString;
   --  Convert an ASCII String value ranging 0..127 to a String hexadecimal
   --  output. For example, with input is "61" dec, result is "3D" hex.

   function To_Hex_Control_Codes (String_To_Convert : UString) return UString;
   --  Convert any ASCII character value ranging 0..32 to a hexadecimal output but leave others characters unchanged.

   function To_Integer (V : UString) return Integer;
   --  Convert a String to an Integer.
   --  Leading and trailing spaces are trimmed before conversion.
   --  Returns 0 if String is empty or contains a least one non numeric character.

   function To_Long_Long_Integer (V : UString) return Long_Long_Integer;
   --  Convert a String to a Long_Long_Integer.
   --  Leading and trailing spaces are trimmed before conversion.
   --  Returns 0 if String is empty or contains a least one non numeric character.

   function To_Long_Integer_From_Hex (Hex_Val : UString) return Long_Integer;
   --  Convert a hexadecimal string to a Long_Integer. Leading and trailing spaces are trimmed
   --  before conversion. Returns 0 if String is empty or contains invalid character.

   function To_Money (DB_Value : Long_Long_Integer) return Money;
   function To_Money (DB_Value : UString) return Money;
   --  Convert Bigint database storage (Long_Long_Integer) or String to Money type.
   --  Bigint (Long_Long_Integer) is used for accurate storage in database.

   function To_String (B : Boolean) return UString;
   function To_String (I : Integer) return UString;
   function To_String (I : Long_Integer) return UString;
   function To_String (I : Long_Long_Integer) return UString;
   function To_String (F : Float) return UString;
   function To_String (M : Money) return UString;
   function To_String (C : ASCII_Character) return UString;
   --  Convert a Boolean, an On_Off type, an Integer, a Long Integer,
   --  a Long Long Integer, a Money type or a Char into String type.

   function To_String_From_Hex (String_To_Convert : UString) return UString;
   --  Convert a hexadecimal string to an ASCII String. Leading and trailing spaces are trimmed
   --  before conversion. Returns 0 if String is empty or contains invalid character.

   function To_String_Unsigned (I : Integer) return UString;
   function To_String_Unsigned (I : Unsigned_8) return UString;
   function To_String_Unsigned (I : Unsigned_16) return UString;
   function To_String_Unsigned (I : Long_Integer) return UString;
   --  Convert an Integer into String type removing the sign, i.e
   --  ' ' space for plus and '-' for minus

   function To_Val (String_To_Convert : UString) return UString;
   --  Convert a String to String ASCII decimal values formatted output.

   function Trim_Both  (Source : UString) return UString;
   --  Returns an all trimmed spaces String of String Source.

   function Trim_Left (Source : UString) return UString;
   --  Returns a trimmed leading spaces String of String Source.

   function Trim_Right (Source : UString) return UString;
   --  Returns a trimmed trailing spaces String of String Source.

   function Trim_Slashes (Source : UString) return UString;
   --  Returns an all trimmed slahes String of String Source.
   --
   --  Examples : Trim_Slashes (
   --    "/") returns ""
   --    "i") returns "i"
   --    "/i") returns "i"
   --    "//////i/////") returns "i"

   -------------------------------------------------------------------------------
   private

   Package_Name : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
   --  Package's name

------------------------------------------------------------------------------
end LibreFrame.Uxs;
------------------------------------------------------------------------------
