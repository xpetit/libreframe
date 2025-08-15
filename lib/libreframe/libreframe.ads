-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe.ads
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  LibreFrame framework
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - dev@soweb.io
--  Xavier Petit - xp - dev@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Exceptions;

with Interfaces;

with GNAT.Source_Info;

with UXStrings; use UXStrings;
with UXStrings.Conversions;

package LibreFrame is

   package GSI renames GNAT.Source_Info;

   ----------------------------------------------------------------------------
   --  PUBLIC TYPES
   ----------------------------------------------------------------------------

   subtype UString is UXString;
   Null_String : constant UString := Null_UXString;

   type Money is delta 0.01 digits 14 range -999_999_999_999.99 .. 999_999_999_999.99;
   --  Type money (14 digits including decimals)

   ----------------------------------------------------------------------------
   --  PUBLIC CONSTANTS
   ----------------------------------------------------------------------------

   --  Redirection constants
   STD_OUT_REDIRECT  : constant UString := " 1>/dev/null";
   ERR_OUT_REDIRECT  : constant UString := " 2>/dev/null";
   STD_ERR_OUT_REDIRECT : constant UString := " 2>/dev/null 1>/dev/null";

   --  Flag files
   ACCESS_OK : constant UString := "access_ok_dont_delete_this_file";
   INSTALL_OK : constant UString := "install_ok_dont_delete_this_file";

   --  Character constants
   Nul : constant Character := Character'First; -- 00h terminator for binding to C string

   --  String constants
   HT   : constant UString := From_ASCII (ASCII.HT);  -- 09d 09h Tab
   LF   : constant UString := From_ASCII (ASCII.LF);  -- 10d 0Ah Line Feed
   CR   : constant UString := From_ASCII (ASCII.CR);  -- 13d 0Dh Carriage return
   ESC  : constant UString := From_ASCII (ASCII.ESC); -- 27d 1Bh Escape
   DQ   : constant UString := From_ASCII ('"');       -- 34d 22h Double quote
   SQ   : constant UString := "'";                    -- 39d 27h Simple quote
   BK   : constant UString := "\";                    -- 92d 5Ch Backslash
   CRLF : constant UString := CR & LF;

   SP   : constant UString := " "; --  32d 20h Space
   VD   : constant UString := ","; --  44d 2Ch Virgule (comma) delimiter
   DD   : constant UString := "."; --  46d 2Eh Dot delimiter
   SD   : constant UString := ":"; --  58d 3Ah Column delimiter
   SC   : constant UString := ";"; --  59d 3Bh Semicolon
   AR   : constant UString := "@"; --  64d 40h Email delimiter
   RD   : constant UString := "\"; --  92d 5Ch Row delimiter
   CD   : constant UString := "^"; --  94d 5Eh Column delimiter
   ND   : constant UString := "~"; -- 126d 7Eh Name/value delimiter

   --  Signs
   SIGN_MINUS : constant UString := "-";
   SIGN_PLUS : constant UString := "&#x2004"; -- 2x 2009, 2007, 2004

   --  Password constants
   PASSWORD_VALIDITY : constant Positive := 10 * 60;
   PASSWORD_MINIMUM_LENGTH : constant Positive := 8;

   --  Grants role constants
   GRANTS_ROLE_NONE : constant UString := "N";
   GRANTS_ROLE_ADMINISTRATOR : constant UString := "A";

   GRANTS_ROLE_NONE_DISPLAY : constant UString := "None";
   GRANTS_ROLE_ADMINISTRATOR_DISPLAY : constant UString := "Administrator";

   --  Grants rights constants
   GRANTS_RIGHTS_NONE : constant UString := "N";
   GRANTS_RIGHTS_CREATE : constant UString := "C";
   GRANTS_RIGHTS_READ : constant UString := "R";
   GRANTS_RIGHTS_UPDATE : constant UString := "U";
   GRANTS_RIGHTS_DELETE : constant UString := "D";
   GRANTS_RIGHTS_PRINT : constant UString := "P";
   GRANTS_RIGHTS_EXPORT : constant UString := "E";
   GRANTS_RIGHTS_FULL : constant UString := GRANTS_RIGHTS_CREATE &
                                           GRANTS_RIGHTS_READ &
                                           GRANTS_RIGHTS_UPDATE &
                                           GRANTS_RIGHTS_DELETE &
                                           GRANTS_RIGHTS_PRINT &
                                           GRANTS_RIGHTS_EXPORT;
   GRANTS_RIGHTS_EMAIL : constant UString := "@";
   GRANTS_RIGHTS_SMS : constant UString := "S";

   --  Log severity levels
   SEVERITY_NONE : constant UString := "N";
   SEVERITY_INFO : constant UString := "I";
   SEVERITY_WARNING : constant UString := "W";
   SEVERITY_ERROR : constant UString := "E";

   SEVERITY_INFO_DISPLAY : UString := "Information";
   SEVERITY_WARNING_DISPLAY : UString := "Avertissement";
   SEVERITY_ERROR_DISPLAY : UString := "Erreur";

   --  Log constants
   LOG_PROMPT : constant UString := " > ";
   DEBUG_NO_FILTER : constant UString := "";

   --  DB modes
   DB_NONE : constant UString := "N";
   DB_CREATE : constant UString := "C";
   DB_READ : constant UString := "R";
   DB_UPDATE : constant UString := "U";
   DB_DELETE : constant UString := "D";
   DB_SEARCH : constant UString := "S";
   DB_FILTER : constant UString := "F";
   DB_EXPORT : constant UString := "E";

   --  DB tags
   DB_NO_DISPLAY : constant UString := "1"; --  Special Id value used to not display a row in list

   --  DB List tags
   HC   : constant UString := "#"; --  33d 23h Hide column in list
   IC   : constant UString := "!"; --  35d 21h Indexed column in list

   --  Common characters enumerations
   LETTERS_LOWER : constant UString := "abcdefghijklmnopqrstuvwxyz";
   LETTERS_UPPER : constant UString := To_Upper (LETTERS_LOWER);
   DIGITS_DECIMAL : constant UString := "0123456789";
   DIGITS_HEXADECIMAL : constant UString := DIGITS_DECIMAL & "ABCDEF";
   SPECIAL_CHARACTERS : constant UString := "-_";

   --  ANSI colors (ISO 6429 standard)
   CONSOLE_COLOR_GREEN  : constant UString := ESC & "[1;32m";
   CONSOLE_COLOR_RED    : constant UString := ESC & "[1;31m";
   CONSOLE_COLOR_YELLOW : constant UString := ESC & "[1;33m";
   CONSOLE_COLOR_RESET  : constant UString := ESC & "[0m";

   --  LibreFrame exit codes
   EXIT_CODE_SUCCESS : constant Natural := 0;
   EXIT_CODE_AFTER_HELP : constant Natural := 1;
   EXIT_CODE_INVALID_PARAMETER : constant Natural := 2;
   EXIT_CODE_EXCEPTION_CTRL_C : constant Natural := 8;
   EXIT_CODE_EXCEPTION_UNEXPECTED : constant Natural := 9;

   procedure Exception_Handling (Exception_Hook : Ada.Exceptions.Exception_Occurrence);
   --  Process unexpected exceptions.

   procedure Exception_Ctrl_C_Handling;
   --  Process Ctrl-C exceptions.

   procedure Finalize (Handling_Type : UString := "");
   --  Finalize application, closing log file, SQL connections and restore cursor state.
   --  A handling_Type parameter is provide for Ctrl-C handling or exceptions.

   function Get_Build return UString;
   --  Returns the formatted build date stamp like:
   --  “build YYYY-mm-dd hh:mm:ss”.

   function Get_Compiler_Version return UString;
   --  This function returns the version in the form "GNAT v.vvx (yyyyddmm)" where
   --  v.vv is the main version number (e.g. 3.16), x is the version
   --  designator (e.g. a1 in 3.16a1), and yyyyddmm is the date in ISO form.

   --  An example of the returned value would be "3.16w (20021029)". The
   --  version is actually that of the binder used to bind the program,
   --  which will be the same as the compiler version if a consistent
   --  set of tools is used to build the program.

   --  this unit is only useable if the main program is written in Ada.

   function Get_Debug_Filter return UString;
   --  Get debug messages filter.

   function Get_Log_Dir return UString;
   --  Returns the log directory

   function Get_Tmp_Dir return UString;
   --  Returns the temporary files directory

   function Get_Version return UString;
   --  Returns the Library name and formatted version like:
   --  “LibreFrame v.minor.major”.

   procedure Raise_Exception;
   --  Raise an exception for reporting test and <program_Name.err> file
   --  creation. In addition to the usual trace, a v20 exception give some
   --  extra information like : exception time, program uptime, program &
   --  library names & versions, start & home directories and Ada and all
   --  languages memory allocation, current & maximum (peak) values.

   --  Convenient string conversions of usual types
   function Value is new UXStrings.Conversions.Scalar_Value (Boolean);
   function Value is new UXStrings.Conversions.Integer_Value (Integer);
   function Value is new UXStrings.Conversions.Floating_Point_Value (Float);
   function Image is new UXStrings.Conversions.Scalar_Image (Boolean);
   function Image is new UXStrings.Conversions.Integer_Image (Integer);
   function Image is new UXStrings.Conversions.Floating_Point_Image (Float);

   ----------------------------------------------------------------------------
   protected type Log_Mutex_T is
      entry Lock;
      procedure Unlock;
   end Log_Mutex_T;
   Log_Mutex : Log_Mutex_T;

   protected type Sql_Mutex_T is
      entry Lock;
      procedure Unlock;
   end Sql_Mutex_T;
   Sql_Mutex : Sql_Mutex_T;

-------------------------------------------------------------------------------
private

   Name : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
   --  Library's name

   Log_Mutex_Owned : Boolean := False;
   Sql_Mutex_Owned : Boolean := False;

   Log_Exception : UString := "";

   --  Name : constant String := "LibreFrame";
   --  --  Library's name
   --
   --  Name2 : constant String := From_Latin_1 (GSI.Enclosing_Entity);

   Version_Major : constant Natural := 0;
   --  Library major version number

   Version_Minor : constant Natural := 8;
   --  Library minor version number

   --  135 cols width is the max full screen standard console on a rather old,
   --  but so good, Dell UltraSharp 1907Fp 1280x1024 4:3 monitor
   --  92 cols width is the max length useable in the "Listing 7" paragraph
   --  style of AIDE Manual with B612 font.
   --  79 is the standard width.
   --  Definitely 512 to display erroneous long SQL queries ;>
   --  Definitely 1024 to display long buffers ;>

   Line_Max_Length : constant Natural := 1024;
   Title_Max_Length : constant Natural := 85;
   --  Maximum line length for exceptions (.err) and log reports (.log)

   Log_Dir : constant UString := "/var/log/";
   --  Log directory

   Tmp_Dir : constant UString := "/tmp/";
   --  Temporary directory

   Debug_Filter : UString := DEBUG_NO_FILTER;
   --  Debug messages filter

   Debug_Status : Boolean := False;
   --  Debug messages status

   Errorlevel : Natural := 0;

------------------------------------------------------------------------------
end LibreFrame;
------------------------------------------------------------------------------
