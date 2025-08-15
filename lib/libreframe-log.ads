-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe-log.ads
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

with LibreFrame.Tio;
with LibreFrame.Prg;
with LibreFrame.Uxs; use LibreFrame.Uxs;

package LibreFrame.Log is

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   procedure Debug (Message : Boolean;
                    Filter : UString := "");
   procedure Debug (Message : UString;
                    Filter : UString := "");
   procedure Debug (Message : Integer;
                    Filter : UString := "");
   procedure Debug_Latin_1 (Message : Standard.String;
                    Filter : UString := "");
   --  Log a debug message.

   procedure Error (Message : UString);
   procedure Error_Latin_1 (Message : Standard.String);
   --  Log a error message.

   function Get_Dir return UString;
   --  Returns log file directory.

   function Get_Info_Exception return UString;
   --  Returns last exception message.

   procedure New_Line;
   --  Log a blank line.

   procedure Info (Message : Boolean);
   procedure Info (Message : ASCII_Character);
   procedure Info (Message : UString);
   procedure Info (Message : Integer);
   procedure Info (Message : Long_Integer);
   procedure Info (Message : Long_Long_Integer);
   procedure Info (Message : Float);
   procedure Info (Message : Money);
   procedure Info_Latin_1 (Message : Standard.String);
   --  Log an information message.

   function Is_Debug return Boolean;
   --  Returns true if debug messages is activated

   function Is_Debug_Filter (Filter : UString := "") return Boolean;
   --  Returns true if Filter is set through Set_Debug_Filter.

   procedure Set_Debug (Enable : Boolean := True);
   --  Set debug messages log status on/[off].

   procedure Set_Debug_Filter (Filter : UString);
   --  Set debug messages attributes.

   procedure Set_Dir (Dir_In : UString);
   --  Set log file directory.

   procedure Set_Disk (Enable : Boolean := True);
   --  Log to disk on/[off].

   procedure Set_Display (Enable : Boolean := True);
   --  Log to display on/[off].

   procedure Set_Header (Enable : Boolean := True);
   --  Line header on/[off].

   procedure Set_Info_Exception (Message : UString := "");
   --  Set exception message.

   procedure Set_Task (New_Task : UString);
   --  Set new current log task.

   procedure Title (Message : UString);
   --  Log a title.

   Error_Trace : exception;

-------------------------------------------------------------------------------
private

   Package_Name : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
   --  Package's name

   Task_State : UString := "INIT";

   --  0         1         2         3      3
   --  01234567890123456789012345678901234567
   --  20210327-160010.NNN - STEP 3  - INF -
   --  \-------19-------/  3 \--7--/ 3 \3/ 3 = Header_Length
   --      Timestamp           Task   Class

   --  Line_Max_Length declared in LibreFrame.ads

   Task_Max_Length : constant Natural := 7;
   Header_Length : constant Natural := 37;

   Header_Status : Boolean := False;
   --  Line header status

   Display_Status : Boolean := False;
   --  Log to display status

   Disk_Status : Boolean := False;
   --  Log to disk status

   Handle : Tio.File;

   Log_Dir_Store : UString := Prg.Start_Dir & "/";

   procedure Debug_Put (Message : UString;
                        Filter : UString := "");
   procedure Put (Line_In : UString;
                  Line_Level : UString;
                  Title_On : Boolean := False);

-------------------------------------------------------------------------------
end LibreFrame.Log;
-------------------------------------------------------------------------------
