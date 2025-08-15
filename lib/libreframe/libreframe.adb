-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe.adb
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

with Ada.Directories;
with Ada.Strings.Fixed;

with GNAT.Compiler_Version;
with GNAT.Source_Info;
with GNAT.OS_Lib;

with LibreFrame.Log;
with LibreFrame.Prg;
with LibreFrame.Sql;
with LibreFrame.Sys;
with LibreFrame.Tio;
with LibreFrame.Uxs;

package body LibreFrame is

   use LibreFrame;

   package GCV is new GNAT.Compiler_Version;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Finalize (Handling_Type : UString := "") is
      Message : UString := Prg.Name & " > " & Handling_Type & " detected, finalize application";
   begin
      Log.New_Line;
      Log.Set_Task ("EXIT");
      if not Is_Empty (Handling_Type) then
         Log.Info (Message);
      end if;
      Sql.Close_All_Databases; --  Close all SQL Connection(s)
      Log.Info ("Total execution time: " & Prg.Duration_Stamp (Prg.Start_Time));
      Log.New_Line;

      Log.Set_Disk (False);  --  Close <application>.log file, flushing the remaining messages still in buffer
      Tio.Set_Cursor (True);

      --  Quit framework tasks
      abort Sql.Ping;

      --  Release framework Mutex, which must happen after the last call to Log and Sql packages
      Log_Mutex.Unlock;
      Sql_Mutex.Unlock;

   end Finalize;

   ----------------------------------------------------------------------------
   function Get_Build return UString is
   begin
      return ("build " &
              From_Latin_1 (GNAT.Source_Info.Compilation_ISO_Date) & " " &
              From_Latin_1 (GNAT.Source_Info.Compilation_Time));
   end Get_Build;

   ----------------------------------------------------------------------------
   function Get_Compiler_Version return UString is
   begin
      return "gnat gcc v" & From_Latin_1 (GCV.Version);
   end Get_Compiler_Version;

   ----------------------------------------------------------------------------
   function Get_Debug_Filter return UString  is
   begin
      return Debug_Filter;
   end Get_Debug_Filter;

   ----------------------------------------------------------------------------
   function Get_Log_Dir return UString is
   begin
     return Log_Dir;
   end Get_Log_Dir;

   --  ----------------------------------------------------------------------------
   --  function Get_Name return String is
   --  begin
   --     return (Name & " v" &
   --             From_Latin_1 (Ada.Strings.Fixed.Trim (Natural'Image (Version_Major), Ada.Strings.Left)) & "." &
   --             From_Latin_1 (Ada.Strings.Fixed.Trim (Natural'Image (Version_Minor), Ada.Strings.Left)));
   --  end Get_Name;

   ----------------------------------------------------------------------------
   function Get_Tmp_Dir return UString is
   begin
     return Tmp_Dir;
   end Get_Tmp_Dir;

   ----------------------------------------------------------------------------
   function Get_Version return UString is
   begin
      return (Name & " v" &
              From_Latin_1 (Ada.Strings.Fixed.Trim (Natural'Image (Version_Major), Ada.Strings.Left)) & "." &
              From_Latin_1 (Ada.Strings.Fixed.Trim (Natural'Image (Version_Minor), Ada.Strings.Left)));
   end Get_Version;

   ----------------------------------------------------------------------------
   procedure Raise_Exception is
      LibreFrame_Exception_Test : exception;
   begin
      raise LibreFrame_Exception_Test;
   end Raise_Exception;

   ----------------------------------------------------------------------------
   procedure Exception_Handling (Exception_Hook : Ada.Exceptions.Exception_Occurrence) is
      Exception_Handle : Tio.File;
      Exception_File_Name : constant UString := Prg.Start_Dir & "/" & Prg.Name & ".err";
      Trace_Output : UString := From_Latin_1 (Ada.Exceptions.Exception_Information (Exception_Hook));
      Trace_Lines : constant Natural := Uxs.Field_Count (Trace_Output, LF);
      Current_Line : UString;
      Current_Address : UString;
      Number_Position, First_Space_Position : Natural;
      Output_Processed : UString := "";
      Trace_without_Lines : Boolean := False;
      SE_Result : Integer := 0;
      SE_Output : UString := "";
   begin

      Tio.New_Line;
      Tio.Put_Line (Title_Max_Length * "-");
      Tio.New_Line;
      Tio.Put_Line ("Exception time          : " & Prg.Date_Time_Milli_Stamp);
      Tio.Put_Line ("Program uptime          : " & Prg.Duration_Stamp (Prg.Start_Time));
      Tio.Put_Line ("Program build DT stamp  : " & LibreFrame.Get_Build);
      Tio.Put_Line ("Program name & version  : " & Prg.Get_Version);
      Tio.Put_Line ("Framework name & version: " & LibreFrame.Get_Version);
      Tio.Put_Line ("Start directory         : " & Prg.Start_Dir);
      Tio.Put_Line ("Home directory          : " & Sys.Get_Home);
      Tio.Put_Line ("Ada mem. alloc. (bytes) : " & Sys.Get_Alloc_Ada);
      Tio.Put_Line ("All mem. alloc. (bytes) : " & Sys.Get_Alloc_All);
      Tio.Put_Line ("Log Info Exception      : " & (if Is_Empty (Log.Get_Info_Exception)
                                                   then "No info message set"
                                                   else Log.Get_Info_Exception));

      --  See LibreFrame Manual > Architecture > Design > Exceptions
      if (Trace_Lines > 0) and Sys.Is_Command ("addr2line") then

         for I in 1 .. Trace_Lines loop
            Current_Line := Uxs.Field_By_Index (Trace_Output, I, LF);
            Number_Position := UXStrings.Index (Current_Line, "???");
            First_Space_Position := UXStrings.Index (Current_Line, SP);
            if (Number_Position > 2) and (First_Space_Position > 2) then
               Current_Address := Slice (Current_Line, 1, UXStrings.Index (Current_Line, SP) - 1);
               Sys.Shell_Execute ("addr2line -e " & Prg.Name & " " & Current_Address, SE_Result, SE_Output);
               if (UXStrings.Index (SE_Output, "/") > 0) then
                  Output_Processed := Output_Processed &
                                      Slice (Current_Line, 1, Number_Position - 1) &
                                      Uxs.Tail_After_Match (SE_Output, "/") & LF;
               else
                  Output_Processed := Output_Processed & Current_Line & LF;
               end if;
               Trace_without_Lines := True;
            else
               Output_Processed := Output_Processed & Current_Line & LF;
            end if;
         end loop;

         if Trace_without_Lines then
            Tio.New_Line;
            Tio.Put_Line ("Where possible, addresses have been replaced by line numbers.");
            Trace_Output := Output_Processed;
         end if;

         Tio.New_Line;
         Tio.Put (Trace_Output);
         Tio.Put_Line (Title_Max_Length * "-");

         if Ada.Directories.Exists (To_UTF_8 (Exception_File_Name)) then
            Tio.Append (Exception_Handle, Exception_File_Name);
         else
            Tio.Create (Exception_Handle, Exception_File_Name);
         end if;

         if Tio.Is_Open (Exception_Handle) then
            --Tio.Put_Line (Exception_Handle, Title_Max_Length * "-");
            Tio.New_Line (Exception_Handle);
            Tio.Put_Line (Exception_Handle, "Exception time          : " & Prg.Date_Time_Milli_Stamp);
            Tio.Put_Line (Exception_Handle, "Program uptime          : " & Prg.Duration_Stamp (Prg.Start_Time));
            Tio.Put_Line (Exception_Handle, "Program build DT stamp  : " & LibreFrame.Get_Build);
            Tio.Put_Line (Exception_Handle, "Program name & version  : " & Prg.Get_Version);
            Tio.Put_Line (Exception_Handle, "Framework name & version: " & LibreFrame.Get_Version);
            Tio.Put_Line (Exception_Handle, "Start directory         : " & Prg.Start_Dir);
            Tio.Put_Line (Exception_Handle, "Home directory          : " & Sys.Get_Home);
            Tio.Put_Line (Exception_Handle, "Ada mem. alloc. (bytes) : " & Sys.Get_Alloc_Ada);
            Tio.Put_Line (Exception_Handle, "All mem. alloc. (bytes) : " & Sys.Get_Alloc_All);
            Tio.Put_Line (Exception_Handle, "Log Info Exception      : " & (if Is_Empty (Log.Get_Info_Exception)
                                                                            then "No info message set"
                                                                            else Log.Get_Info_Exception));
            Tio.New_Line (Exception_Handle);

            if Trace_without_Lines then
               Tio.Put_Line (Exception_Handle, From_Latin_1 ("Where possible, addresses have been replaced by line numbers."));
               Tio.New_Line (Exception_Handle);
            end if;

            Tio.Put (Exception_Handle, Trace_Output);
            Tio.Put_Line (Exception_Handle, Title_Max_Length * "-");
            Tio.Close (Exception_Handle);
         end if;

      end if;

      Finalize ("Exception");
      Prg.Set_Exit_Status (EXIT_CODE_EXCEPTION_UNEXPECTED);

   end Exception_Handling;

   ----------------------------------------------------------------------------
   procedure Exception_Ctrl_C_Handling is
   begin
      if Prg.Is_Handler_Ctrl_C then
         Log.New_Line;
         Finalize ("Ctrl-C");
         Tio.New_Line;
         GNAT.OS_Lib.OS_Exit (EXIT_CODE_EXCEPTION_CTRL_C); --  Set exit code and quit
      else
         Tio.New_Line;
         Log.Info (Prg.Name & " > Ctrl-C detected, program interrupt is inhibited");
      end if;
   end Exception_Ctrl_C_Handling;

   ----------------------------------------------------------------------------
   --  Private
   ----------------------------------------------------------------------------

   protected body Log_Mutex_T is
      entry Lock when not Log_Mutex_Owned is
      begin
         Log_Mutex_Owned := True;
      end Lock;
      procedure Unlock is
      begin
         Log_Mutex_Owned := False;
      end Unlock;
   end Log_Mutex_T;

   protected body Sql_Mutex_T is
      entry Lock when not Sql_Mutex_Owned is
      begin
         Sql_Mutex_Owned := True;
      end Lock;
      procedure Unlock is
      begin
         Sql_Mutex_Owned := False;
      end Unlock;
   end Sql_Mutex_T;

-------------------------------------------------------------------------------
end LibreFrame;
-------------------------------------------------------------------------------
