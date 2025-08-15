-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe-tio.ads
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  LibreFrame framework - Text I/O package
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

with Ada.Text_IO;
with Interfaces;

with LibreFrame.Uxs; use LibreFrame.Uxs;

package LibreFrame.Tio is

   package ATI renames Ada.Text_IO;

   ----------------------------------------------------------------------------
   --  API - Terminal
   ----------------------------------------------------------------------------

   --  Max_Row  : constant Natural := 29;
   --  Max_Column : constant Natural := 79;

   Max_Row  : constant Natural := 99;
   Max_Column : constant Natural := 299;

   subtype Row is Natural range 0 .. Max_Row;
   subtype Column  is Natural range 0 .. Max_Column;
   subtype Integer_64 is Interfaces.Integer_64;

   -- ANSI ISO 6429 standard
   -- https://www.perpetualpc.net/6429_colors.html
   -- https://tintin.mudhalla.net/info/ansicolor
   type ANSI_Color is (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White);
   type ANSI_Attribute is (Reset, Bold, Faint, Italic, Underline, Slow_Blink, Fast_Blink, Reverse_Video, Erase, Strikethrough);

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   procedure Animated_Delay (Delay_Seconds : Positive);
   --  Animated delay in seconds with markers each 5 and 10 seconds.
   --  ....!....|....!....|....!./ < animated wheel with /-\|/-| characters
   --  .1s !5s  |10s

   procedure Beep;
   --  Send a beep.

   procedure Bell;
   --  Send a bell sound.

   procedure Clear_Screen;
   --  Clear the screen.

   function Confirm_Twice (User_Prompt_1 : UString; User_Prompt_2 : UString) return Boolean;
   --  Double check by user before action. Returns True if user has validate.

   procedure Cursor_Move (X : Row; Y : Column);
   --  Move the cursor at the specified X,Y coordinates.

   procedure Cursor_Line_Forward (X : Row);
   --  Move the cursor forward X rows.

   procedure Cursor_Line_Backward (X : Row);
   --  Move the cursor backward X rows.

   procedure Cursor_Line_Erase;
   --  Erase the current line from the current cursor position to the end of
   --  the line.

   procedure Cursor_Save;
   --  Save the current cursor position.

   procedure Cursor_Restore;
   --  Restore the previous saved cursor position.

   procedure Get_Immediate (C : out Character) renames Ada.Text_IO.Get_Immediate;
   --  Get a character validated by [Enter].

   function Get_Line return UString;
   --  Get a string validated by [Enter].

   function Get_Password return UString;
   --  Returns a password blind typed

   function Is_Ansi return Boolean;
   --  Return ANSI status

   procedure New_Line (Spacing : ATI.Positive_Count := 1) renames ATI.New_Line;
   --  Add a new line to the console

   procedure Put (B : Boolean);
   procedure Put (C : Character) renames ATI.Put;
   procedure Put (V : UString);
   procedure Put (I : Integer);
   procedure Put (I : Long_Integer);
   procedure Put (I : Integer_64);
   procedure Put (M : Money);
   --  Print to the console.

   procedure Put_Line (B : Boolean);
   procedure Put_Line (C : Character);
   procedure Put_Line (V : UString);
   procedure Put_Line (I : Integer);
   procedure Put_Line (I : Long_Integer);
   procedure Put_Line (I : Integer_64);
   procedure Put_Line (M : Money);
   --  Print to the console then add a new line.

   procedure Set_Ansi (Enable : Boolean);
   --  Set ANSI state

   procedure Set_ANSI_Color (Attribute :  ANSI_Attribute; Foreground : ANSI_Color := White; Background : ANSI_Color := Black);
   --  Set ANSI state

   procedure Set_Cursor (Enable : Boolean);
   --  Display or hide cursor.

   ----------------------------------------------------------------------------
   --  API - Text File
   ----------------------------------------------------------------------------

   subtype File is ATI.File_Type;

   procedure Append (Handle : in out File; Name : UString);
   --  Append on an existing file.

   procedure Close (Handle : in out File) renames ATI.Close;
   --  Close a file.

   procedure Create (Handle : in out File; Name : UString);
   --  Create a file.

   function End_Of_File (Handle : File) return Boolean renames ATI.End_Of_File;
   --  Test if enf of file reached.

   function End_Of_Line (Handle : File) return Boolean renames ATI.End_Of_Line;
   --  Test if end of line reached.

   procedure Flush (Handle : File) renames ATI.Flush;
   --  Flush file buffer to disk.

   function Get_Line (Handle : File) return UString;
   procedure Get_Line (Handle : File; V : out UString);
   --  Read a line then move the file pointer to the next line.

   function Is_Open (Handle : File) return Boolean renames ATI.Is_Open;
   --  Test if a file is open.

   procedure New_Line (Handle : File; Spacing : ATI.Positive_Count := 1) renames ATI.New_Line;
   --  Add a new line to a file.

   procedure Open_Conf (Handle : in out File; Name : UString; Wipe_Before_Process : Boolean := False; Permissions : UString := "");
   --  Special Open procedure for config files. Creates or Append if needed.
   --  Ensure that the complete directory tree structure exists before
   --  creating file. Creating this directory tree if needed.
   --  Allways make backup before Append. If Wipe_Before_Process is True, the
   --  file Name is backuped before beeing deleted

   procedure Open_Read (Handle : in out File; Name : UString);
   --  Open a file in read mode.

   procedure Pause;
   --  Displays Press any key to continue or [Ctrl-C] to abort... waiting for user input.

   procedure Put (Handle  : File; C : Character) renames ATI.Put;
   procedure Put (Handle  : File; S : Standard.String) renames ATI.Put;
   procedure Put (Handle  : File; V : UString);
   --  Write to a file.

   procedure Put_Line (Handle  : File; C : Character);
   procedure Put_Line (Handle  : File; S : Standard.String) renames ATI.Put_Line;
   procedure Put_Line (Handle  : File; V : UString);
   --  Write a file and then add a new line

   function Read_File (File_Name : UString) return UString;
   --  Read a text file File_To_Read and returning a String buffer. LF
   --  (line feed) are preserved.

   procedure Reset (Handle : in out File) renames ATI.Reset;
   --  Reset the file pointer to the start of the file

   procedure Write_File (File_Name : UString; Content : UString; Permissions : UString := "");
   --  Write a text file File_To_Write with Content. LF in content are
   --  preserved and used as line feed. Read Open_Conf documentation for
   --  implementation details.

-------------------------------------------------------------------------------
private

   Package_Name : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
   --  Package's name

   Ansi_Status : Boolean := True;

-------------------------------------------------------------------------------
end LibreFrame.Tio;
-------------------------------------------------------------------------------
