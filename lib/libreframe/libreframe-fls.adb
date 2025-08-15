-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe-fls.adb
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

with Ada.IO_Exceptions;
with Ada.Strings;

with LibreFrame.Log;
with LibreFrame.Sys;
with LibreFrame.Tio;

package body LibreFrame.Fls is

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Backup_File (File_To_Backup : UString) is
      Backup_File : constant UString := File_To_Backup & ".bak.";
      Backup_To_Delete : Integer := 0;
   begin
      for I in 0 .. 9 loop
         if not Exists (Backup_File & To_String (I)) then
            Move_File (File_To_Backup, Backup_File & To_String (I));
            if I = 9 then
               Backup_To_Delete := 0;
            else
               Backup_To_Delete := I + 1;
            end if;
            if Exists (Backup_File & To_String (Backup_To_Delete)) then
               Delete_File (Backup_File & To_String (Backup_To_Delete));
            end if;
            exit;
         end if;
      end loop;
   end Backup_File;

   ----------------------------------------------------------------------------
   procedure Copy_File (Source_Name, Target_Name : UString) is

      Destination_Valid : Boolean := True;

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      -- Check if destination is current or contains a leading directory
      if (Index (Target_Name, "/") > 0) then
         if not (AD.Exists (To_Latin_1 (Slice (Source_Name, 1, Index (Source_Name, "/", Ada.Strings.Backward))))) then
            Destination_Valid := False;
             Log.Error (Log_Header & "Destination directory does not exist: " & Target_Name);
         end if;
      end if;
      if Destination_Valid then
         AD.Copy_File (To_Latin_1 (Source_Name), To_Latin_1 (Target_Name), To_Latin_1 (Copy_Form));
      end if;
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
   end Copy_File;

   ----------------------------------------------------------------------------
   procedure Copy_Directory_Tree (Dir_Tree_Origin : UString; Dir_Tree_Destination : UString) is

      -- https://rosettacode.org/wiki/Walk_a_directory/Recursively
      procedure Walk (Name : UString; Pattern : UString) is
         procedure Print (Item : AD.Directory_Entry_Type) is
         begin
            Tio.Put_Line (From_Latin_1 (AD.Full_Name (Item)));
         end Print;

         procedure Walk (Item : AD.Directory_Entry_Type) is
         begin
            if AD.Simple_Name (Item) /= "." and AD.Simple_Name (Item) /= ".." then
               Walk (From_Latin_1 (AD.Full_Name (Item)), Pattern);
            end if;
         exception
            when Ada.IO_Exceptions.Name_Error => null;
         end Walk;

      begin
         AD.Search (To_Latin_1 (Name), To_Latin_1 (Pattern),                       (others => True), Print'Access);
         AD.Search (To_Latin_1 (Name),                   "", (AD.Directory => True, others => False), Walk'Access);
      end Walk;


   begin
      Walk (Dir_Tree_Origin, "*");
   end Copy_Directory_Tree;

   ----------------------------------------------------------------------------
   function Create_Directory_Tree (Dir_Tree : UString) return Boolean is
   begin
      if AD.Exists (To_Latin_1 (Dir_Tree)) then
         return True;
      else
         AD.Create_Path (To_Latin_1 (Dir_Tree));
         return AD.Exists (To_Latin_1 (Dir_Tree));
      end if;
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
         return False;
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
         return False;
   end Create_Directory_Tree;

   ----------------------------------------------------------------------------
   function Delete_Directory_Tree (Dir_Tree : UString)
                                   return Boolean is

      Result : Boolean := False;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      if not If_Root_Directory (Dir_Tree) then
         if AD.Exists (To_Latin_1 (Dir_Tree)) then
            AD.Delete_Tree (To_Latin_1 (Dir_Tree));
            if not AD.Exists (To_Latin_1 (Dir_Tree)) then
               Result := True;
            end if;
         else
            Result := True;
         end if;
      else
          Log.Error (Log_Header & "Attempt to delete a root directory: " & Dir_Tree);
      end if;
      return Result;
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
         return False;
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
         return False;
   end Delete_Directory_Tree;

   procedure Delete_Directory_Tree (Dir_Tree : UString) is
      Dummy : Boolean;
   begin
      Dummy := Delete_Directory_Tree (Dir_Tree);
   end Delete_Directory_Tree;

   ----------------------------------------------------------------------------
   procedure Delete_File (Name : UString) is
   begin
      if AD.Exists (To_Latin_1 (Name)) then
         AD.Delete_File (To_Latin_1 (Name));
      end if;
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
   end Delete_File;

   ----------------------------------------------------------------------------
   procedure Delete_Lines (File_Name, Pattern : UString) is

      File_Write : constant UString := File_Name  & ".new";
      Handle_Read, Handle_Write : Tio.File;
      Line_Deleted : Boolean := False;
      Line_Buffer : UString := "";

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      if Fls.Exists (File_Name) then
         Tio.Open_Read (Handle_Read, File_Name);
         if Tio.Is_Open (Handle_Read) then
            Tio.Create (Handle_Write, File_Write);
            if Tio.Is_Open (Handle_Write) then
               while not Tio.End_Of_File (Handle_Read) loop
                  Tio.Get_Line (Handle_Read, Line_Buffer);
                  if Index (Line_Buffer, Pattern) > 0 then
                     Line_Deleted := True;
                  else
                     Tio.Put_Line (Handle_Write, Line_Buffer);
                  end if;
               end loop;
               Tio.Close (Handle_Write);
            end if;
            Tio.Close (Handle_Read);
         end if;
         if Line_Deleted then
            Rename (File_Name, File_Name & ".bak");
            Rename (File_Write, File_Name);
         end if;
      else
          Log.Error (Log_Header & "File not found: " & File_Name);
      end if;
   end Delete_Lines;

   ----------------------------------------------------------------------------
   function Download_File (Url : UString;
                           Dlfile : UString;
                           Name : UString := "";
                           DlSize : Integer := 0) return Boolean is

      Exec_Error : Integer;
      Result : Boolean := False;
      Message_Name : UString := Name;

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      if Length (Name) = 0 then
         Message_Name := Url;
      end if;

      --  Processing existing file
      if Fls.Exists (Dlfile) then
         if DlSize > 0 then
            --  If Size correct
            if Fls.File_Size (Dlfile) = DlSize then
               Result := True;
               Log.Info ("Keep existing and valid file: " & Dlfile);
            end if;
         end if;
         if not Result then
            Log.Info ("Delete old file: " & Dlfile);
            Fls.Delete_File (Dlfile);
         end if;
      end if;
      --  Proceed to download if needed
      if not Result then
         Log.Info ("Download file: " & Message_Name);

         -- http1.1 to avoid curl error 'HTTP/2 stream 0 was not closed cleanly'
         Sys.Shell_Execute ("curl --http1.1 --location --output " &
                         Dlfile & " " & Url, Exec_Error);
         if (Exec_Error = 0) and Fls.Exists (Dlfile) then
            if DlSize > 0 then
               if Fls.File_Size (Dlfile) = DlSize then
                  Result := True;
               end if;
            else
               Result := True;
            end if;
         end if;
         if not Result then
             Log.Error (Log_Header & "Download file failed: " & Message_Name);
         end if;
      end if;
      return Result;
   end Download_File;

   ----------------------------------------------------------------------------
   function Exists (Name : UString) return Boolean is
   begin
      return AD.Exists (To_Latin_1 (Name));
   end Exists;

   ----------------------------------------------------------------------------
   function Extract_Directory (Name : UString) return UString is
      Slash : constant UString := "/";
      Dir_End : Natural;
      Dir_Result : UString := "";
   begin
      Dir_End := Index (Name, Slash, Ada.Strings.Backward);
      if (Dir_End > 1) then
         Dir_Result := Slice (Name, 1, Dir_End - 1);
      end if;
      return Dir_Result;
   end Extract_Directory;

   ----------------------------------------------------------------------------
   function Extract_Filename (Name : UString) return UString is
   begin
      return Tail_After_Match (Name, '/');
   end Extract_Filename;

   ----------------------------------------------------------------------------
   function File_Size (Name : UString) return Integer is
   begin
      return To_Integer (From_Latin_1 (AD.File_Size'Image (AD.Size (To_Latin_1 (Name)))));
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
         return 0;
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
         return 0;
   end File_Size;

   ----------------------------------------------------------------------------
   function Get_Directory return UString is
   begin
      return From_Latin_1 (AD.Current_Directory);
   end Get_Directory;

   ----------------------------------------------------------------------------
   function If_Root_Directory (Dir_Tree : UString) return Boolean is
      Test_Dir_Tree : UString := Dir_Tree;
      Slash : constant UString := "/";
      Root_Dirs : constant UString := "bin,boot,dev,etc,home,lib,lib32,lib64," &
         "libx32,lost+found,media,mnt,opt,proc,root,run,sbin,srv,sys," &
         "tmp,usr,var,Test_Delete_Directory_Tree";
      Result : Boolean := False;
   begin
      -- Dir_Tree must be fully qualified, ie starting with a slash (/)
      if Starts_With (Dir_Tree, Slash) then
         --  Add a slash if Dir_Tree does not ends with it
         if not Ends_With (Dir_Tree, Slash) then
            Test_Dir_Tree := Test_Dir_Tree & Slash;
         end if;
         --  If Dir_Tree counts only two slashes and at least one char between
         if (Char_Count (Test_Dir_Tree, Slash) = 2) and
            (Length (Dir_Tree) > 2) then
            Test_Dir_Tree := Slice (Test_Dir_Tree, 2, Length (Test_Dir_Tree) - 1);
            if not Is_Empty (Field_By_Name (Root_Dirs, Test_Dir_Tree, Slash)) then
               Result := True;
            end if;
         end if;
      end if;
      return Result;
   end If_Root_Directory;

   ----------------------------------------------------------------------------
   procedure Move_File (Source_Name, Target_Name : UString) is
   begin
      Copy_File (Source_Name, Target_Name);
      Delete_File (Source_Name);
   end Move_File;

   ----------------------------------------------------------------------------
   procedure Rename (Old_Name, New_Name : UString) is
      use Ada.Directories;  --  For File_Kind.Directory visibility
   begin
      if Exists (New_Name) then
         if not (Kind (To_Latin_1 (New_Name)) = Directory) then
            AD.Delete_File (To_Latin_1 (New_Name));
         end if;
      end if;
      AD.Rename (To_Latin_1 (Old_Name), To_Latin_1 (New_Name));
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
   end Rename;

   ----------------------------------------------------------------------------
   function Search_Lines (File_Name, Pattern : UString) return Boolean is
      Handle_Read : Tio.File;
      Line_Found : Boolean := False;
      Line_Buffer : UString := "";
   begin
      Tio.Open_Read (Handle_Read, File_Name);
      if Tio.Is_Open (Handle_Read) then
         while not Tio.End_Of_File (Handle_Read) loop
            Tio.Get_Line (Handle_Read, Line_Buffer);
            if Index (Line_Buffer, Pattern) > 0 then
               Line_Found := True;
               exit;
            end if;
         end loop;
         Tio.Close (Handle_Read);
      end if;
      return Line_Found;
   end Search_Lines;

   ----------------------------------------------------------------------------
   function Set_Directory (Directory : UString) return Boolean is
      Result : Boolean := False;
   begin
      if AD.Exists (To_Latin_1 (Directory)) then
         AD.Set_Directory (To_Latin_1 (Directory));
         Result := True;
      end if;
      return Result;
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
         return False;
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
         return False;
   end Set_Directory;

   ----------------------------------------------------------------------------
   --  Private
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Log_Err (Err_Exc : Ada.Exceptions.Exception_Occurrence) is

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
       Log.Error (Log_Header & "Exception: " & From_Latin_1 (Ada.Exceptions.Exception_Information (Err_Exc)));
   end Log_Err;

-------------------------------------------------------------------------------
end LibreFrame.Fls;
-------------------------------------------------------------------------------
