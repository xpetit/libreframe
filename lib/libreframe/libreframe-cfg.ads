-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe-cfg.ads
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

with LibreFrame.Prg;
with LibreFrame.Tio;
with LibreFrame.Uxs; use LibreFrame.Uxs;

package LibreFrame.Cfg is

   pragma Elaborate_Body;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   procedure Close;
   --  Close Cfg file. For sanity only as each setting is instantly flushed to
   --  disk.

   procedure Comment (Text : UString);
   --  Insert a comment to the next line.

   procedure Delete (Section : UString; Parameter : UString);
   --  Delete parameter in section. If no other parameter in this section,
   --  delete section too. Avoid reserved chars [ ] = # inside parameters.
   --  If reserved chars are passed, the procedure does nothing.

   function Get (Section : UString; Parameter : UString) return UString;
   --  Return parameter in section or empty string if not found. Avoid reserved
   --  chars [ ] = # inside parameters.

   function Get_Name return UString;
   --  Returns full qualified (with path) application configuration file name.

   procedure New_Line;
   --  Insert a blank line to the next line.

   function Open (Cfg_File_Read_In : UString := "") return Boolean;
   --  Open and load if exist a configuration file. Create blank if non
   --  existent. Default configuration file name is full qualified program name
   --  followed by .cfg extension and created in the program start directory.
   --  This default file name may be changed by Set_Name procedure.

   procedure Set (Section : UString; Parameter : UString; Value : UString; Comment : UString := "");
   --  Create or replace an existing parameter in a section. If this latter
   --  does not exist, also creating it. New setting is persistent even program
   --  quits unexpectedly after. Avoid reserved chars [ ] = # inside
   --  parameters. If reserved chars are passed, the procedure does nothing. A
   --  optional trailing comment can also be added.

   procedure Set_Name (Cfg_File_Read_In : UString);
   --  Set application configuration file name, relative or full qualified.

-------------------------------------------------------------------------------
private

   Package_Name : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
   --  Package's name

   Cfg_File_Read : UString := Prg.Start_Dir & "/" & Prg.Name & ".cfg";
   Cfg_File_Write : UString := Prg.Start_Dir & "/" & Prg.Name & ".tmp";

   Cfg_Open_Section : constant UString := "[";
   Cfg_Close_Section : constant UString := "]";
   Cfg_Assignment : constant UString := "=";
   Cfg_Comment : constant UString := "#";
   Cfg_Command_Delete : constant UString := Cfg_Open_Section & Cfg_Open_Section & "D";
   Cfg_Command_Add : constant UString := Cfg_Open_Section & Cfg_Open_Section & "P";

   --  Memory consumption test of an array of String
   --  Table_Max  ram
   --  (elements) (bytes)
   --  500        1088
   --  250        488

   Table_Max : Natural := 250;
   type Table_Lines is array (1 .. Table_Max) of UString;
   Cfg_Table : Table_Lines;

   Cfg_Last : Natural := 0;
   Cfg_Section : Natural := 0;
   Cfg_Parameter : Natural := 0;

   Handle_Read : Tio.File;
   Handle_Write : Tio.File;

   --  Service functions
   function Cfg_Read return Boolean;
   procedure Cfg_Search (Section : UString; Parameter : UString);
   function Cfg_Write (Section : UString := ""; Parameter : UString := ""; Value : UString := ""; Trailing_Comment : UString := "")
                       return Boolean;
   function Check_Parameters (Section : UString; Parameter : UString; Value : UString) return Boolean;
   function Table_Write (Line : UString) return Boolean;
   --  procedure Table_Write (Line : String);

-------------------------------------------------------------------------------
end LibreFrame.Cfg;
-------------------------------------------------------------------------------
