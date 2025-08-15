-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe-log.adb
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

with Ada.Characters.Handling;

with LibreFrame.Fls;

package body LibreFrame.Log is

   --  ----------------------------------------------------------------------------
   --  procedure Clear_Info_Exception is
   --  begin
   --     Log_Exception := "";
   --  end Clear_Info_Exception;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Debug (Message : Boolean;
                    Filter : UString := "") is
   begin
      if Is_Debug then
         Debug_Put (From_Latin_1 ((if (Message) then "True" else "False")), Filter);
      end if;
   end Debug;

   procedure Debug (Message : UString;
                    Filter : UString := "") is
   begin
      if Is_Debug then
         Debug_Put (Message, Filter);
      end if;
   end Debug;

   procedure Debug (Message : Integer;
                    Filter : UString := "") is
   begin
      if Is_Debug then
         Debug_Put (To_String (Message), Filter);
      end if;
   end Debug;

   procedure Debug_Latin_1 (Message : Standard.String;
                    Filter : UString := "") is
   begin
      if Is_Debug then
         Debug_Put (From_Latin_1 (Message), Filter);
      end if;
   end Debug_Latin_1;

   ----------------------------------------------------------------------------
   procedure Error (Message : UString) is
   begin
      Put (Message, "ERR");
      --  To ease debugging with a call stack
      if Is_Debug then
         raise Error_Trace;
      end if;
   end Error;

   procedure Error_Latin_1 (Message : Standard.String) is
   begin
      Put (From_Latin_1 (Message), "ERR");
      --  To ease debugging with a call stack
      if Is_Debug then
         raise Error_Trace;
      end if;
   end Error_Latin_1;

   ----------------------------------------------------------------------------
   function Get_Dir return UString is
   begin
      return Log_Dir_Store;
   end Get_Dir;

   ----------------------------------------------------------------------------
   function Get_Info_Exception return UString is
   begin
      return Log_Exception;
   end Get_Info_Exception;

   ----------------------------------------------------------------------------
   procedure Info (Message : Boolean) is
   begin
      Put (From_Latin_1 ((if (Message) then "True" else "False")), "Log");
   end Info;

   procedure Info (Message : ASCII_Character) is
   begin
      Put (From_ASCII (Message), "MSG");
   end Info;

   procedure Info (Message : UString) is
   begin
      Put (Trim_Left (Message), "MSG"); -- Suppress the space left for positive sign
   end Info;

   procedure Info (Message : Integer) is
   begin
      Put (To_String (Message), "MSG");
   end Info;

   procedure Info (Message : Long_Integer) is
   begin
      Put (To_String (Message), "MSG");
   end Info;

   procedure Info (Message : Long_Long_Integer) is
   begin
      Put (To_String (Message), "MSG");
   end Info;

   procedure Info (Message : Float) is
   begin
      Put (To_String (Message), "MSG");
   end Info;

   procedure Info (Message : Money) is
   begin
      Put (From_Latin_1 (Money'Image (Message)), "MSG");
   end Info;

   procedure Info_Latin_1 (Message : Standard.String) is
   begin
      Put (From_Latin_1 (Message), "MSG");
   end Info_Latin_1;

   ----------------------------------------------------------------------------
   function Is_Debug return Boolean is
   begin
      return (Debug_Status);
   end Is_Debug;

   ----------------------------------------------------------------------------
   function Is_Debug_Filter (Filter : UString := "") return Boolean is
      Filter_Local : UString := (if Is_Empty (Filter) then "*" else Filter);
   begin
      return (Index (Get_Debug_Filter, Filter_Local) > 0);
   end Is_Debug_Filter;

   ----------------------------------------------------------------------------
   procedure New_Line is
   begin
      Log_Mutex.Lock;
      Tio.New_Line;
      if Disk_Status then
         Tio.New_Line (Handle);
      end if;
      Log_Mutex.Unlock;
   end New_Line;

   ----------------------------------------------------------------------------
   procedure Set_Debug (Enable : Boolean := True) is
   begin
      Debug_Status := Enable;
   end Set_Debug;

   ----------------------------------------------------------------------------
   procedure Set_Debug_Filter (Filter : UString)  is
   begin
      Debug_Filter := Filter;
   end Set_Debug_Filter;


   ----------------------------------------------------------------------------
   procedure Set_Dir (Dir_In : UString) is
   begin
      Log_Mutex.Lock;
      Log_Dir_Store := Dir_In;
      Log_Mutex.Unlock;
   end Set_Dir;

   ----------------------------------------------------------------------------
   procedure Set_Disk (Enable : Boolean := True) is
      Log_File_Name : constant UString := Log_Dir_Store & Prg.Name & ".log";
      Log_Dir_Name : constant UString := Fls.Extract_Directory (Log_File_Name);
   begin
      Log_Mutex.Lock;
      Disk_Status := Enable;
      if Disk_Status then
         if Fls.Exists (Log_File_Name) then
            Tio.Append (Handle, Log_File_Name);
         else
            -- Ensure that the complete tree structure exists before creating file
            if Fls.Create_Directory_Tree (Log_Dir_Name) then
               Tio.Create (Handle, Log_File_Name);
            else
               Disk_Status := False;
               Error ("Log.Set_Disk > Can't create directory: " & Log_File_Name);
            end if;
         end if;
         if not Tio.Is_Open (Handle) then
            Disk_Status := False;
            Error ("Log.Set_Disk > can't log on disk to: " & Log_File_Name);
         end if;
      else
         if Tio.Is_Open (Handle) then
            Tio.Close (Handle);
         end if;
      end if;
      Log_Mutex.Unlock;
   end Set_Disk;

   ----------------------------------------------------------------------------
   procedure Set_Display (Enable : Boolean := True)  is
   begin
      Log_Mutex.Lock;
      Display_Status := Enable;
      Log_Mutex.Unlock;
   end Set_Display;

   ----------------------------------------------------------------------------
   procedure Set_Header (Enable : Boolean := True) is
   begin
      Log_Mutex.Lock;
      Header_Status := Enable;
      Log_Mutex.Unlock;
   end Set_Header;

   ----------------------------------------------------------------------------
   procedure Set_Info_Exception (Message : UString := "") is
   begin
      Log_Exception := Message;
   end Set_Info_Exception;

   ----------------------------------------------------------------------------
   procedure Set_Task (New_Task : UString) is
   begin
      Log_Mutex.Lock;
      Task_State := New_Task;
      Log_Mutex.Unlock;
   end Set_Task;

   ----------------------------------------------------------------------------
   procedure Title (Message : UString) is
   begin
      Put (To_Upper (Message), "---", True);
   end Title;

   ----------------------------------------------------------------------------
   --  Private
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Debug_Put (Message : UString;
                        Filter : UString := "") is
   begin
      --  if Filter is empty or Filter exists in filter list
      --  The (if Is_Empty (Filter) then "*" else Filter) avoid an exception if Filter is empty
      if (Is_Empty (Filter)) or else
         (Index (Get_Debug_Filter, (if Is_Empty (Filter) then "*" else Filter)) > 0) then
         Put (Message, "DBG");
      end if;
   end Debug_Put;

   ----------------------------------------------------------------------------
   procedure Put (Line_In : UString;
                  Line_Level : UString;
                  Title_On : Boolean := False) is
      Line : UString := Line_In;
      Line_Disk : UString := Line;
      Line_Task : UString := "";
      Ansi_Begin : UString := "";
      Ansi_End : UString := "";
   begin
      Log_Mutex.Lock;
      if Display_Status and Tio.Is_Ansi then
         if Line_Level = "DBG" then
            Ansi_Begin := CONSOLE_COLOR_YELLOW;
         elsif Line_Level = "ERR" then
            Ansi_Begin := CONSOLE_COLOR_RED;
         end if;
         Ansi_End := CONSOLE_COLOR_RESET;
      end if;

      --  Task length control
      if Length (Task_State) > Task_Max_Length then
         --                  these two numbers must be equals v     v
         Line_Task := Slice (Task_State, 1, Task_Max_Length - 1) & (1 * "*");
      elsif  Length (Task_State) < Task_Max_Length then
         Line_Task :=  Task_State &
                      (Task_Max_Length - Length (Task_State)) * " ";
      else
         Line_Task :=  Task_State;
      end if;

      --  Header console and disk mode
      if Header_Status then

         --  Line Length control
         if (Header_Length + Length (Line) + 1) > Line_Max_Length then
            --                                      these two numbers v
            Line := Slice (Line, 1, Line_Max_Length - Header_Length - 1) & (1 * "*");
            --                                               must be equals ^
         end if;

         if Title_On then
            if (Header_Length + Length (Line) + 2) < Title_Max_Length then
               Line := Line & " " &
                      (Title_Max_Length - Header_Length - Length (Line) - 1) * "-";
            end if;
         end if;

         if Display_Status then
            --  Console write with limited length line and ansi fancy
            Tio.Put_Line (Prg.Date_Time_Milli_Stamp & " - " &
                            Line_Task & " - " &
                            Ansi_Begin & Line_Level & Ansi_End & " - " &
                            Line);
         end if;

      --  Free console mode
      else
         if Display_Status then
            Tio.Put_Line (Ansi_Begin & Line & Ansi_End);
         end if;
      end if;

      --  Disk write with unlimited length line
      if Disk_Status then
         if Title_On then
            if (Header_Length + Length (Line_Disk) + 1) < Title_Max_Length then
                Line_Disk := Line_Disk &
               (Title_Max_Length - Header_Length - Length (Line_Disk)) * "-";
            end if;
         end if;
         Tio.Put_Line (Handle, Prg.Date_Time_Milli_Stamp & " - " &
                               Line_Task & " - " &
                               Line_Level & " - " &
                               Line_Disk);
         Tio.Flush (Handle); -- Flush to disk for an allways up to date log file
      end if;
      Log_Mutex.Unlock;
   end Put;

------------------------------------------------------------------------------
end LibreFrame.Log;
------------------------------------------------------------------------------
