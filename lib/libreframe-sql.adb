-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe-sql.adb
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  LibreFrame framework
--
--  @description
--  High level SQL binding
--
--  @authors
--  Stéphane Rivière - sr - dev@soweb.io
--  Xavier Petit - xp - dev@soweb.io
--
--  Studying Gnoga's sources saved time for the low-level SQLite and MySQL
--  C bindings. Many thanks to David Botton - david@botton.com
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Iterator_Interfaces; -- Vérifier si nécessaire
with Ada.Tags;

with GNAT.SHA512;

with UXStrings.Conversions; use UXStrings.Conversions;

with LibreFrame.Fls;
with LibreFrame.Log;
with LibreFrame.Tio;

package body LibreFrame.Sql is

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Clear_Table (Database_Name : UString;
                          Table_Name : UString ;
                          Optimize : Boolean := False) is

      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Name);
   begin
      --  As the table may not exist, there is no error message handling
      if Is_Table_Exists (Database_Name, Table_Name) then
         Query (Database_Name, "DELETE FROM " & Table_Name & ";");
         if Optimize then
            case Database_Brand is
            when SQLite =>
               Query (Database_Name, "VACUUM " & Table_Name & ";");
            when MySQL =>
               Query (Database_Name, "OPTIMIZE TABLE " & Table_Name & ";");
            when PostgreSQL =>
               Query (Database_Name, "VACUUM " & Table_Name & ";");
            when Firebird =>
               null;
            when others => null;
            end case;
         end if;
      end if;
   end Clear_Table;

   ----------------------------------------------------------------------------
   procedure Close_Database (Database_Name : UString) is
   begin
      Close_Delete (Database_Name, To_Delete => True);
   end Close_Database;

   ----------------------------------------------------------------------------
   procedure Close_All_Databases is
   begin
      for Database of Databases loop
         Log.Info ("Closing " & From_Latin_1 (Database.Brand'Image) & " database: " & Database.Name);
         Close_Delete (Database.Name, To_Delete => False);
      end loop;
      Databases.Clear;
   end Close_All_Databases;

   -------------------------------------------------------------------------------
   procedure Close_Record_Set (Record_Set : in out Record_Set_SQLite_Type) is

      --  https://sqlite.org/c3ref/finalize.html
      procedure Sqlite_Finalize (Result : Database_Handle_Type := Record_Set.Handle)
                                 with Import, Convention => C, External_Name => "sqlite3_finalize";
   begin
      Sqlite_Finalize;
   end Close_Record_Set;

   procedure Close_Record_Set (Record_Set : in out Record_Set_MySQL_Type) is

      --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_free_result
      procedure MySQL_Free_Result (Result : Database_Handle_Type := Record_Set.Handle)
                                   with Import, Convention => C, External_Name => "mysql_free_result";
   begin
      MySQL_Free_Result;
   end Close_Record_Set;

   ----------------------------------------------------------------------------
   function Count_Rows (Database_Name : UString;
                       Table_Name : UString;
                       Method : UString := "COUNT";
                       Option : UString := "*")
                       return Integer is

      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Name);
      Database_Query : UString := "SELECT " & To_Upper (Method) & "(" & Option & ") FROM " & Table_Name;

      Result : Natural := 0;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      if Is_Table_Exists (Database_Name, Table_Name) then
         --  A query such as SELECT MAX(rowid) FROM Table can't be used to count
         --  rows. There are only two ways to get the correct count: either use
         --  SELECT COUNT(*) FROM Table, but this is not fast (expect a few seconds
         --  for a few million records) or tinker with a table of counters incremented
         --  and decremented by triggers (which means updating the counters separately
         --  when importing using COPY).
         --  CREATE TABLE Counters (Name, Counter);
         --  INSERT INTO Counters VALUES("Table", 0);
         --  CREATE TRIGGER Table_Insert_Counter AFTER INSERT on Table
         --  BEGIN
         --     UPDATE Counters SET Counter = Counter + 1 WHERE Name = "Table";
         --  END;
         --  CREATE TRIGGER foo_delete_counter AFTER DELETE on foo
         --  BEGIN
         --     UPDATE Counters SET Counter = Counter - 1 WHERE Name = "Table";
         --  END;
         case Database_Brand is
         when SQLite =>
            declare
               Record_Set : Record_Set_SQLite_Type := Query (Database_Name, Database_Query);
            begin
               Next (Record_Set);
               while Next (Record_Set) loop
                  Result := To_Integer (Get_Column_Data (Record_Set, 1));
               end loop;
               Close_Record_Set (Record_Set);
            end;
         when MySQL =>
            declare
               Record_Set : Record_Set_MySQL_Type := Query (Database_Name, Database_Query);
            begin
               while Next (Record_Set) loop
                  Result := To_Integer (Get_Column_Data (Record_Set, 1));
               end loop;
               Close_Record_Set (Record_Set);
            end;
         when PostgreSQL =>
            declare
            begin
               null;
            end;
         when Firebird =>
            declare
            begin
               null;
            end;
         when others => null;
         end case;
      else
         Log_Error_Table_Not_Found (Log_Header, Table_Name);
      end if;
      return Result;
   end Count_Rows;

   ----------------------------------------------------------------------------
   procedure Delete (Database_Name : UString;
                     Table_Name : UString;
                     Condition : UString := "") is

      Sql_Query : UString := "DELETE FROM " & Table_Name & " WHERE " & Condition;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      Sql_Mutex.Lock;
      if Is_Table_Exists (Database_Name, Table_Name) then
         Log.Debug (Log_Header & "Insert_Into: " & Sql_Query, Log_Filter);
         Query (Database_Name, Sql_Query);
      else
         Log_Error_Table_Not_Found (Log_Header, Table_Name);
      end if;
      Sql_Mutex.Unlock;
   exception
      when E : others =>
         Log.Error (Log_Header & "Error: " & From_Latin_1 (Ada.Exceptions.Exception_Information(E)));
         Sql_Mutex.Unlock;
   end Delete;

   ----------------------------------------------------------------------------
   function Escape_String (String_To_Process : UString)
                           return UString is
      Current_Character : UString;
      Result : UString := "";
   begin
      for I in 1 .. String_To_Process.Length loop
         Current_Character := Slice (String_To_Process, I, I);

         if Current_Character = From_ASCII (ASCII.NUL) then    --  00d
            Result := Result & BK & "0";
         elsif Current_Character = From_ASCII (ASCII.BS) then  --  08d Backspace
            Result := Result & BK & "b";
         elsif Current_Character = From_ASCII (ASCII.HT) then  --  09d Tabulation
            Result := Result & BK & "t";
         elsif Current_Character = From_ASCII (ASCII.LF) then  --  10d Line feed
            Result := Result & BK & "n";
         elsif Current_Character = From_ASCII (ASCII.CR) then  --  13d Carriage return
            Result := Result & BK & "r";
         elsif Current_Character = DQ then                     --  22d Double Quote
            Result := Result & BK & DQ;
         elsif Current_Character = From_ASCII (ASCII.SUB) then --  26d ASCII 26 (Control+Z)
            Result := Result & BK & "Z";
         elsif Current_Character = SQ then                     --  27d Single quote
            Result := Result & SQ & SQ;                        --  '' instead of \' to comply with SQLite and Firebird
                                                               --  when MySQL and PostgreSQL accept both '' and \'
         elsif Current_Character = "%" then                    --  37d
            Result := Result & BK & "%";
         elsif Current_Character = BK then                     --  92d Backslash
            Result := Result & BK & BK;
         else
            Result := Result & String_To_Process(I);
         end if;
      end loop;
      return Result;
   end Escape_String;

   -------------------------------------------------------------------------------
   function Get_Affected_Rows (Database_Handle : aliased Database_Handle_Type)
                               return Natural is

      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Handle);
      Result : Natural := 0;
   begin
      case Database_Brand is
      when SQLite =>
         declare
            --  https://sqlite.org/c3ref/changes.html
            function Sqlite_Changes (Handle : Database_Handle_Type := Database_Handle)
                                     return Natural
                                     with Import, Convention => C, External_Name => "sqlite3_changes";
         begin
            Result := Sqlite_Changes;
         end;
      when MySQL =>
         declare
            --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_affected_rows
            function MySQL_Affected_Rows (Handle : Database_Handle_Type := Database_Handle)
                                          return MySQL_Unsigned_Long_Long_Type
                                          with Import, Convention => C, External_Name => "mysql_affected_rows";
         begin
            Result := Natural (MySQL_Affected_Rows);
         end;
      when PostgreSQL =>
         declare
         begin
            null;
         end;
      when Firebird =>
         declare
         begin
            null;
         end;
      when others => null;
      end case;
      return Result;
   end Get_Affected_Rows;

   -------------------------------------------------------------------------------
   function Get_All_Columns_Data (Record_Set : Record_Set_SQLite_Type)
                                  return Data_Map_Type is

      Row : Data_Map_Type;
   begin
      for I in 1 .. Record_Set.Column_Count loop
         Row.Insert (Get_Column_Name (Record_Set, I), Get_Column_Data (Record_Set, I));
      end loop;
      return Row;
   end Get_All_Columns_Data;

   function Get_All_Columns_Data (Record_Set : Record_Set_MySQL_Type)
                                  return Data_Map_Type is

      Row : Data_Map_Type;
   begin
      for I in 1 .. Record_Set.Column_Count loop
         Row.Insert (Get_Column_Name (Record_Set, I), Get_Column_Data (Record_Set, I));
      end loop;
      return Row;
   end Get_All_Columns_Data;

    -------------------------------------------------------------------------------
   function Get_All_Columns_Properties (Database_Handle : aliased Database_Handle_Type;
                                        Table_Name : UString)
                                        return Column_List_Type is

      Database_Name : UString := Get_Database_Name (Database_Handle);
      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Handle);
      Descriptions : Column_List_Type;
   begin
      case Database_Brand is
      when SQLite =>
         declare
            Record_Set : Record_Set_SQLite_Type := Query (Database_Handle, "pragma table_info (" & Table_Name & ")");
         begin
            Next (Record_Set);
            while Next (Record_Set) loop
               declare
                  Description : Column_Record_Type;
               begin
                  Description.Database_Name := Database_Name;
                  Description.Table_Name := Table_Name;
                  Description.Column_Name := Get_Column_Data (Record_Set, 2);
                  Description.Data_Type := Get_Column_Data (Record_Set, 3);
                  Description.Can_Be_Null := (Get_Column_Data (Record_Set, 4) = "1");
                  Description.Default_Value := Get_Column_Data (Record_Set, 5);

                  Descriptions.Append (Description);
               end;
            end loop;
            Close_Record_Set (Record_Set);
         end;
      when MySQL =>
         declare
            Record_Set : Record_Set_MySQL_Type := Query (Database_Handle, "DESCRIBE " & Table_Name);
         begin
            while Next (Record_Set) loop
               declare
                  Description : Column_Record_Type;
               begin
                  Description.Database_Name := Database_Name;
                  Description.Table_Name := Table_Name;
                  Description.Column_Name := Get_Column_Data (Record_Set, 1);
                  Description.Data_Type := Get_Column_Data (Record_Set, 2);
                  Description.Can_Be_Null := (Get_Column_Data (Record_Set, 3) = "YES");
                  Description.Default_Value := Get_Column_Data (Record_Set, 5);

                  Descriptions.Append (Description);
               end;
            end loop;
            Close_Record_Set (Record_Set);
         end;
      when PostgreSQL =>
         declare
         begin
            null;
         end;
      when Firebird =>
         declare
         begin
            null;
         end;
      when others => null;
      end case;
      return Descriptions;
   end Get_All_Columns_Properties;

   -------------------------------------------------------------------------------
   function Get_Column_Data (Record_Set : Record_Set_SQLite_Type;
                             Column_Name : UString)
                             return UString is

      Result : UString := "";
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      for I in 1 .. Record_Set.Column_Count loop
         if Column_Name = Get_Column_Name (Record_Set, I) then
            Result := Get_Column_Data (Record_Set, I);
            exit;
         end if;
      end loop;
      if Is_Empty (Result) then
         Log.Error (Log_Header & "Column not found: " & Column_Name);
      end if;
      return Result;
   end Get_Column_Data;

   function Get_Column_Data (Record_Set : Record_Set_SQLite_Type;
                         Column_Number : Natural)
                         return UString is

      subtype Column_Data is Interfaces.C.char_array (0 .. Interfaces.C.size_t'Last);
      type Column_Access is access all Column_Data;

      --  https://sqlite.org/c3ref/column_blob.html
      function SQLite_Column_Text (Handle : Database_Handle_Type := Record_Set.Handle;
                                   Number : Interfaces.C.int := Interfaces.C.int (Column_Number - 1)) --  Leftmost column is 0
                                   return Column_Access
                                   with Import, Convention => C, External_Name => "sqlite3_column_text";

      --  https://sqlite.org/c3ref/column_blob.html
      function SQLite_Column_Bytes (Handle : Database_Handle_Type := Record_Set.Handle;
                                    Number  : Interfaces.C.int := Interfaces.C.int (Column_Number - 1)) --  Leftmost column is 0
                                    return Interfaces.C.int
                                    with Import, Convention => C, External_Name => "sqlite3_column_bytes";
      Result : UString := "";
   begin
      if not Is_Null (Record_Set, Column_Number) then
         declare
            Value : constant Standard.String :=
                    Interfaces.C.To_Ada (SQLite_Column_Text.all (0 .. Interfaces.C.size_t (SQLite_Column_Bytes)));
         begin
            Result := From_UTF_8 (UTF_8_Character_Array (Value));
         end;
      end if;
      return Result;
   end Get_Column_Data;

   function Get_Column_Data (Record_Set : Record_Set_MySQL_Type;
                             Column_Name : UString)
                             return UString is

      Result : UString := "";
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity) & ".MySQL";
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      Log.Debug (Log_Header & "Column_Name_String: " & Column_Name, Log_Filter);
      Log.Debug (Log_Header & "Record_Set.Column_Count: " & To_String (Record_Set.Column_Count), Log_Filter);
      for I in 1 .. Record_Set.Column_Count loop
         if Column_Name = Get_Column_Name (Record_Set, I) then
            Result := Get_Column_Data (Record_Set, I);
            exit;
         end if;
      end loop;
      if Is_Empty (Result) then
         Log.Error (Log_Header & "Column not found: " & Column_Name);
      end if;
      return Result;
   end Get_Column_Data;

   function Get_Column_Data (Record_Set : Record_Set_MySQL_Type;
                             Column_Number : Natural)
                             return UString is

      use type Interfaces.C.char_array;
      use type Interfaces.C.size_t;

      Result : UString := "";
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity) & ".MySQL";
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      if Record_Set.Last_Row /= null then
         if not Is_Null (Record_Set, Column_Number) then
            Result := From_UTF_8 (Interfaces.C.To_Ada (
                                  Record_Set.Last_Row (Column_Number) (0 .. Interfaces.C.size_t (Record_Set.Lengths (Column_Number)) - 1) &
                                  Interfaces.C.nul));
         end if;
      --  else
      --     Log.Error (Log_Header & "Null field");
      end if;
      return Result;
   end Get_Column_Data;

   -------------------------------------------------------------------------------
   function Get_Column_Length (Column : Column_Record_Type)
                               return Natural is

      Type_Of_Column : UString := Column.Data_Type;
      Type_Length : UString;
      Left : constant Natural := Index (Type_Of_Column, "(");
      Right : constant Natural := Index (Type_Of_Column, ")");

      Comma, Integer_Length, Decimal_Length, Result : Natural := 0;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      Log.Debug (Log_Header & "Type_Of_Column: " & Type_Of_Column, Log_Filter);
      if not (Right = 0 or Left = 0) then
         Type_Length := Slice (Type_Of_Column, Left + 1, Right - 1);
         if not Is_Empty (Type_Length) then
            Comma := Index (Type_Length, VD);
            if Comma = 0 then
               Result := To_Integer (Type_Length);
            else
               Integer_Length := To_Integer (Slice (Type_Length, 1, Comma - 1));
               Decimal_Length := To_Integer (Slice (Type_Length, Comma + 1, Length (Type_Length)));
               Result := Integer_Length + 1 + Decimal_Length;
            end if;
         end if;
      end if;
      Log.Debug (Log_Header & "Result: " & To_String (Result), Log_Filter);
      return Result;
   end Get_Column_Length;

   -------------------------------------------------------------------------------
   function Get_Column_Name (Record_Set : Record_Set_SQLite_Type;
                             Column_Number : Natural)
                             return UString is

      subtype charbuf is Interfaces.C.char_array (1 .. Interfaces.C.size_t'Last);
      type charbuf_access is access all charbuf;

      --  https://sqlite.org/c3ref/column_name.html
      function SQLite_Column_Name (Handle : Database_Handle_Type := Record_Set.Handle;
                                   Number : Integer   := Column_Number - 1) --  Leftmost column is 0
                                   return charbuf_access
                                   with Import, Convention => C, External_Name => "sqlite3_column_name";
   begin
      return From_UTF_8 (UTF_8_Character_Array (Interfaces.C.To_Ada (SQLite_Column_Name.all)));
   end Get_Column_Name;

   function Get_Column_Name (Record_Set : Record_Set_MySQL_Type;
                             Column_Number : Natural)
                             return UString is

      use type Interfaces.C.char_array;
      use type Interfaces.C.size_t;

      --  type MySQL_Column is record
      --     Name          : Column_Access;
      --     Org_Name      : Column_Access;
      --     Table         : Column_Access;
      --     Org_Table     : Column_Access;
      --     DB            : Column_Access;
      --     Catalog       : Column_Access;
      --     Default       : Column_Access;
      --     Create_Length : Interfaces.C.unsigned_long;
      --     Max_Length    : Interfaces.C.unsigned_long;
      --     Name_L        : Interfaces.C.unsigned;
      --     Org_Name_L    : Interfaces.C.unsigned;
      --     Table_L       : Interfaces.C.unsigned;
      --     DB_L          : Interfaces.C.unsigned;
      --     Catalog_L     : Interfaces.C.unsigned;
      --     Default_L     : Interfaces.C.unsigned;
      --  end record with Convention => C;

      type MySQL_Column_Access_Type is access all MySQL_Column;

      --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_fetch_field_direct
      function MySQL_Fetch_Field_Direct (Result : Database_Handle_Type := Record_Set.Handle;
                                         Number : Interfaces.C.unsigned := Interfaces.C.unsigned (Column_Number - 1))
                                         return MySQL_Column_Type
                                         with Import, Convention => C, External_Name => "mysql_fetch_field_direct";
      Column : MySQL_Column_Type;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      Column := MySQL_Fetch_Field_Direct;
      if Column = null then
         Log.Error (Log_Header & "Column error: " & Error_Message_MySQL (Record_Set.Handle));
      end if;
      return From_UTF_8 (Interfaces.C.To_Ada (Column.Name (0 .. Interfaces.C.size_t (Column.Name_L) - 1) & Interfaces.C.nul));
    end Get_Column_Name;

   ----------------------------------------------------------------------------
   function Get_Config (Parameter : UString)
                        return UString is

      Database_Name : UString := Get_Main_Database;
      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Name);
      Database_Handle : aliased Database_Handle_Type := Get_Database_Handle (Database_Name);
   begin
      return Get_Config (Database_Brand, Database_Name, Database_Handle, Parameter);
   end Get_Config;

   function Get_Config (Database_Brand : Database_Brand_Type;
                        Database_Name : UString;
                        Database_Handle : aliased Database_Handle_Type;
                        Parameter : UString)
                        return UString is

      Database_Query : UString := "SELECT Value FROM " & Table_Sys_Config & " WHERE Parameter='" & Parameter & "'";

      Result : UString := "";
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Name & "." & Log_Filter & LOG_PROMPT;
   begin
      Log.Debug (Log_Header & "Enter", Log_Filter);
      Log.Debug (Log_Header & "Database_Query: " & Database_Query, Log_Filter);
      if Is_Table_Exists (Database_Brand, Database_Handle, Table_Sys_Config) then
         case Database_Brand is
         when SQLite =>
            declare
               Record_Set : Record_Set_SQLite_Type := Query (Database_Handle, Database_Query);
            begin
               Next (Record_Set);
               while Next (Record_Set) loop
                  Result := Get_Column_Data (Record_Set, 1);
               end loop;
               Close_Record_Set (Record_Set);
            end;
         when MySQL =>
            declare
               Record_Set : Record_Set_MySQL_Type := Query (Database_Handle, Database_Query);
            begin
               while Next (Record_Set) loop
                  Result := Get_Column_Data (Record_Set, 1);
               end loop;
               Close_Record_Set (Record_Set);
            end;
        when PostgreSQL =>
            declare
            begin
               null;
            end;
         when Firebird =>
            declare
            begin
               null;
            end;
         when others => null;
         end case;
      else
         Log_Error_Table_Not_Found (Log_Header, Table_Sys_Config);
      end if;
      Log.Debug (Log_Header & "Result: " & Result, Log_Filter);
      Log.Debug (Log_Header & "Quit", Log_Filter);
      return Result;
   end Get_Config;

   ----------------------------------------------------------------------------
   function Get_Database_Brand (Database_Name : UString)
                                return Database_Brand_Type is

      Database_Record : Database_List_Type;
      Brand : Database_Brand_Type := Unknown;
   begin
      if Sql.Get_Database_Properties (Database_Name, Database_Record) then
         Brand := Database_Record.Brand;
      else
         declare
            Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
            Log_Header : constant UString := Log_Filter & LOG_PROMPT;
         begin
            Log.Error (Log_Header & "Database name not found: " & Database_Name);
         end;
      end if;
      return Brand;
   end Get_Database_Brand;

   function Get_Database_Brand (Database_Handle : aliased Database_Handle_Type)
                                return Database_Brand_Type is

      Database_Brand : Database_Brand_Type;
      Database_Record : Database_List_Type;
   begin
      if Sql.Get_Database_Properties (Database_Handle, Database_Record) then
         Database_Brand := Database_Record.Brand;
      else
         declare
            Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
            Log_Header : constant UString := Log_Filter & LOG_PROMPT;
         begin
            Log_Error_Database_Not_Found (Log_Header, "Database handle not found", From_Latin_1 (Database_Handle'Image));
         end;
      end if;
      return Database_Brand;
   end Get_Database_Brand;

   ----------------------------------------------------------------------------
   function Get_Database_Handle (Database_Name : UString)
                                 return Database_Handle_Type is

      Database_Record : Database_List_Type;
      Handle : aliased Database_Handle_Type := null;
   begin
      if Sql.Get_Database_Properties (Database_Name, Database_Record) then
         Handle := Database_Record.Handle;
      else
         declare
            Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
            Log_Header : constant UString := Log_Filter & LOG_PROMPT;
         begin
            Log.Error (Log_Header & "Database name not found: " & Database_Name);
         end;
      end if;
      return Handle;
   end Get_Database_Handle;

   ----------------------------------------------------------------------------
   function Get_Main_Database return UString is
   begin
      return Main_Database;
   end Get_Main_Database;

   ----------------------------------------------------------------------------
   function Get_Database_Name (Database_Handle : aliased Database_Handle_Type)
                               return UString is

      Database_Record : Database_List_Type;
      Database_Name : UString := "";
   begin
      if Sql.Get_Database_Properties (Database_Handle, Database_Record) then
         Database_Name := Database_Record.Name;
      else
         declare
            Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
            Log_Header : constant UString := Log_Filter & LOG_PROMPT;
         begin
            Log_Error_Database_Not_Found (Log_Header, "Database handle not found", From_Latin_1 (Database_Handle'Image));
         end;
      end if;
      return Database_Name;
   end Get_Database_Name;

   ----------------------------------------------------------------------------
   function Get_Database_Properties (Database_Name : UString;
                                     Database_Record : in out Database_Record_Type)
                                     return Boolean is

      Status : Boolean := False;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      --  Spent too much time trying to use the vector search
      --  functions built in the package; need a better brain.
      for C in Databases.Iterate loop
         --Log.Debug ("Cursor (C): " & From_Latin_1 (Databases_List.Extended_Index'Image (To_Index (C))), Log_Filter);
         --Log.Debug ("Databases(C).Name: " & Databases(C).Name, Log_Filter);
         if Databases(C).Name = Database_Name then
            Database_Record := Databases(C);
            Status := True;
            exit;
         end if;
      end loop;
      if not Status then
         Log_Error_Database_Not_Found (Log_Header, "name", Database_Name);
      end if;
      return Status;
   end Get_Database_Properties;

   function Get_Database_Properties (Database_Handle : aliased Database_Handle_Type;
                                     Database_Record : in out Database_Record_Type)
                                     return Boolean is

      Status : Boolean := False;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
    begin
      Log.Debug (Log_Header & "Enter", Log_Filter);
      for C in Databases.Iterate loop
         --Log.Debug (Log_Header & "Index: " & From_Latin_1 (Databases_List.Extended_Index'Image (To_Index (C))));
         if  Databases(C).Handle = Database_Handle then
              Database_Record := Databases(C);
              Status := True;
            exit;
         end if;
      end loop;
      --  As this function can be used at the very beginning of the application,
      --  even before a single recorded database, we must not output a log error
      --  when the handle's not found.
      return Status;
   end Get_Database_Properties;

   function Get_Database_Properties (Database_Index : Positive;
                            Database_Record : in out Database_Record_Type)
                            return Boolean is

      use Databases_List; -- for operators

      Status : Boolean := False;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      for C in Databases.Iterate loop
         --Log.Debug (Log_Header & "Index: " & From_Latin_1 (Databases_List.Extended_Index'Image (To_Index (C))));
         if  Databases(C).Index = Database_Index then
              Database_Record := Databases(C);
              Status := True;
            exit;
         end if;
      end loop;
      if not Status then
         Log_Error_Database_Not_Found (Log_Header, "name", Trim_Left (To_String (Database_Index)));
      end if;
      return Status;
   end Get_Database_Properties;

   ----------------------------------------------------------------------------
   function Get_Version (Database_Name : UString) return UString is

      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Name);
      Database_Version : UString := "";
   begin
      case Database_Brand is
      when SQLite =>
         declare
            --  https://sqlite.org/c3ref/libversion.html
            function Sqlite_Libversion
                     return ICS.chars_ptr
                     with Import, Convention => C, External_Name => "sqlite3_libversion";
         begin
            Database_Version := "SQLite: v" & From_Latin_1 (ICS.Value (Sqlite_Libversion));
         end;
      when MySQL =>
         declare
            Record_Set : Record_Set_MySQL_Type := Query (Database_Name, "SELECT @@Version");
         begin
            while Next (Record_Set) loop
               Database_Version := "MySQL: v" & Get_Column_Data (Record_Set, 1);
            end loop;
            Close_Record_Set (Record_Set);
         end;
      when PostgreSQL =>
         declare
         begin
            null;
         end;
      when Firebird =>
         declare
         begin
            null;
         end;
      when others => null;
      end case;
      return Database_Version;
   end Get_Version;

   --------------------------------------------------------------------------
   procedure Insert (Database_Handle : aliased Database_Handle_Type;
                     Table_Name : UString;
                     Columns_Values : UString) is

      Description_List : Column_List_Type;
      Description : Column_Record_Type;
      Current_Column, Current_Value, Current_Type, Insert_Columns_Names,
      Insert_Columns_Values, Sql_Query : UString := "";
      Counter_Columns : constant Natural := Field_Count (Columns_Values, CD);

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      Sql_Mutex.Lock;
      if Is_Table_Exists (Database_Handle, Table_Name) then
         Description_List := Get_All_Columns_Properties (Database_Handle, Table_Name);
         Log.Debug (Log_Header & "Counter_Columns: " & To_String (Natural (Description_List.Last_Index)), Log_Filter);

         --  Check each field in parameter against the current table's column
         for Index in 1 .. Counter_Columns loop
            --  Iterate through each column
            for I in Description_List.First_Index .. Description_List.Last_Index loop
               Description := Description_List.Element (I);
               Current_Column := Trim_Both (Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 1, ND));
               Log.Debug (Log_Header & "Index:" & To_String(I), Log_Filter);
               Log.Debug (Log_Header & "Description_List.Last_Index: " & To_String (Description_List.Last_Index), Log_Filter);
               Log.Debug (Log_Header & "Current_Column (PRG name): " & To_Upper (Current_Column), Log_Filter);
               Log.Debug (Log_Header & "Current_Column (DB name): " & To_Upper (Description.Column_Name), Log_Filter);

               --  If field name and column name match
               if To_Upper (Current_Column) = To_Upper (Description.Column_Name) then
                  --  Fill Name and Value, according to field type
                  Current_Value := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 2, ND);
                  Current_Type := To_Upper (Slice (Description.Data_Type, 1, 3));
                  Insert_Columns_Names := Insert_Columns_Names & Current_Column & ",";
                  Log.Debug (Log_Header & "Current_Value: " & Current_Value, Log_Filter);
                  Log.Debug (Log_Header & "Current_Type: " & Current_Type, Log_Filter);
                  Log.Debug (Log_Header & "Insert_Columns_Names: " & Insert_Columns_Names, Log_Filter);

                  --  Apply, depending of the column type:
                  --  BLOB, TEXT, VARCHAR: Single quotes outside string and escaping characters inside string
                  --  BIGINT, DECIMAL, DOUBLE, FLOAT, INTEGER: Insert 0 if empty
                  if Current_Type = "BIG" then --  BIGINT
                     Insert_Columns_Values := Insert_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "BLO" then --  BLOB
                     Insert_Columns_Values := Insert_Columns_Values & "'" & Escape_String (Current_Value) & "',";
                  elsif Current_Type = "DEC" then --  DECIMAL
                     Insert_Columns_Values := Insert_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "DOU" then --  DOUBLE
                     Insert_Columns_Values := Insert_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "FLO" then --  FLOAT
                     Insert_Columns_Values := Insert_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "INT" then --  MySQL INT but SQLite INTEGER
                     Insert_Columns_Values := Insert_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "TEX" then --  TEXT
                     Insert_Columns_Values := Insert_Columns_Values & "'" & Escape_String (Current_Value) & "',";
                  elsif Current_Type = "VAR" then --  VARCHAR
                     Current_Value := Escape_String (Current_Value);
                     --  Truncate string adjusting it to maximum column length
                     if Current_Value.Length > Get_Column_Length (Description) then
                        Current_Value := Slice (Current_Value, 1, Get_Column_Length (Description));
                        Log.Error ("LibreFrame.Sql.Insert > String too long to fit in VARCHAR(" &
                                   To_String_Unsigned (Get_Column_Length (Description)) &
                                   "). Truncate it: " & Slice (Current_Value, 1, Get_Column_Length (Description)));
                     end if;
                     Insert_Columns_Values := Insert_Columns_Values & "'" & Current_Value & "',";
                  else
                      Log.Error (Log_Header & "Column: " & Current_Column & " does not handle Type: " & Current_Type);
                  end if;
                  exit; --  No need to iterate further after match
               else
                  if I = Description_List.Last_Index and not Is_Empty (Current_Column) then
                      Log.Error (Log_Header & "Column: '" & Current_Column & "' does not exists in Table: " & Table_Name);
                  end if;
               end if;
            end loop;
         end loop;
         --  If at least one Column/Value pair has been processed
         if (Index (Insert_Columns_Names, ",") > 0) and
            (Index (Insert_Columns_Values, ",") > 0) then
            --  Trailing comma deletion
            Insert_Columns_Names := Slice (Insert_Columns_Names, 1, Length (Insert_Columns_Names) - 1);
            Insert_Columns_Values := Slice (Insert_Columns_Values, 1, Length (Insert_Columns_Values) - 1);
            Log.Debug (Log_Header & "Insert_Columns_Names: " & Insert_Columns_Names, Log_Filter);
            Log.Debug (Log_Header & "Insert_Columns_Values: " & Insert_Columns_Values, Log_Filter);
            Sql_Query := "INSERT INTO " & Table_Name & " (" & Insert_Columns_Names & ") VALUES (" & Insert_Columns_Values & ");";
            Log.Debug (Log_Header & "Insert_Into: " & Sql_Query, Log_Filter);
            Query (Database_Handle, Sql_Query);
         end if;
      else
          Log.Error (Log_Header & "Table does not exists: " & Table_Name);
      end if;
      Sql_Mutex.Unlock;
   exception
      when E : others =>
         Log.Error (Log_Header & "Error: " & From_Latin_1 (Ada.Exceptions.Exception_Information(E)));
         Sql_Mutex.Unlock;
   end Insert;

   ----------------------------------------------------------------------------
   function Is_Column_Exists (Database_Name : UString;
                             Table_Name : UString;
                             Column_Name : UString)
                             return Boolean is

      Columns : Data_Array_Type;

      Result : Boolean := False;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      if Is_Table_Exists (Database_Name, Table_Name) then
         Columns := List_Columns_Of_Table (Database_Name, Table_Name);
         Log.Debug (Log_Header & "Columns.Length (I): " & To_String (Natural (Columns.Length)), Log_Filter);
         for I in 1 .. Natural (Columns.Length) loop
            Log.Debug (Log_Header & "Columns.Element (I): " & Columns.Element (I), Log_Filter);
            if Columns.Element (I) = Column_Name then
               Result := True;
               exit;
            end if;
         end loop;
      else
         Log_Error_Table_Not_Found (Log_Header, Table_Name);
      end if;
      return Result;
   end Is_Column_Exists;

   ----------------------------------------------------------------------------
   function Is_Index_Exists (Database_Name : UString;
                          Table_Name : UString;
                          Index_Name : UString)
                          return Boolean is

      Database_Handle : aliased Database_Handle_Type := Get_Database_Handle (Database_Name);
      Result : Boolean;
   begin
      Result := Is_Index_Exists (Database_Handle, Table_Name, Index_Name);
      return Result;
   end Is_Index_Exists;

   function Is_Index_Exists (Database_Handle : aliased Database_Handle_Type;
                          Table_Name : UString;
                          Index_Name : UString)
                          return Boolean is

      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Handle);
      Input : UString;

      Result : Boolean;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      if Is_Table_Exists (Database_Handle, Table_Name) then
         case Database_Brand is
         when SQLite =>
            --  SELECT * FROM sqlite_master WHERE type = 'index'
            --  SELECT * FROM sqlite_master WHERE type = 'index' and tbl_name = 'Tbl_Test' and name = 'Idx_Test_Number'
            --  Database_Query : String := "PRAGMA index_list('"& Table_Name  & "');"; -- Other way to list all index of a table
            declare
               Database_Query : UString := "SELECT * FROM sqlite_master WHERE type = 'index'" &
                                          " and tbl_name = '" & Table_Name  & "' and name = '" & Index_Name & "'";
               Record_Set : Record_Set_SQLite_Type := Query (Database_Handle, Database_Query);
            begin
               Log.Debug (Log_Header & "Query: " & Database_Query, Log_Filter);
               Next (Record_Set);
               Result := Next (Record_Set);
               Close_Record_Set (Record_Set);
            end;
         when MySQL =>
            declare
               Database_Query : UString := "SHOW INDEX FROM " & Table_Name & " WHERE Key_name = '" & Index_Name & "'";
               Record_Set : Record_Set_MySQL_Type := Query (Database_Handle, Database_Query);
            begin
               Log.Debug (Log_Header & "Query: " & Database_Query, Log_Filter);
               Result := Next (Record_Set);
               Close_Record_Set (Record_Set);
            end;
         when PostgreSQL =>
            declare
            begin
               null;
            end;
         when Firebird =>
            declare
            begin
               null;
            end;
         when others => null;
         end case;
      else
         Log_Error_Table_Not_Found (Log_Header, Table_Name);
      end if;
      return Result;
   end Is_Index_Exists;

   ----------------------------------------------------------------------------
   function Is_Table_Exists (Database_Name : UString;
                          Table_Name : UString)
                          return Boolean is

      Database_Record : Database_List_Type;
      Result : Boolean := False;
   begin
      if Get_Database_Properties (Database_Name, Database_Record) then
         Result := Is_Table_Exists (Database_Record.Handle, Table_Name);
      end if;
      return Result;
   end Is_Table_Exists;

   function Is_Table_Exists (Database_Handle : aliased Database_Handle_Type;
                          Table_Name : UString)
                          return Boolean is

      Database_Record : Database_List_Type;
      Result : Boolean := False;
   begin
      if Get_Database_Properties (Database_Handle, Database_Record) then
         Result := Is_Table_Exists (Database_Record.Brand, Database_Record.Handle, Table_Name);
      end if;
      return Result;
   end Is_Table_Exists;

   function Is_Table_Exists (Database_Brand : Database_Brand_Type;
                          Database_Handle : aliased Database_Handle_Type;
                          Table_Name : UString)
                          return Boolean is

      Tables : Data_Array_Type;
      Result : Boolean := False;

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      Tables := List_Of_Tables (Database_Brand, Database_Handle);
      Log.Debug (Log_Header & "Table_Name: " & Table_Name, Log_Filter);
      Log.Debug (Log_Header & "Tables.Length: " & To_String_Unsigned (Natural (Tables.Length)), Log_Filter);
      for I in 1 .. Natural (Tables.Length) loop
         Log.Debug (Log_Header & "Tables.Element (" & Trim_Left (To_String (I)) & ")", Log_Filter);
         Log.Debug (Log_Header & "Tables.Element (I): " & Tables.Element (I), Log_Filter);
         if Tables.Element (I) = Table_Name then
            Result := True;
            exit;
         end if;
      end loop;
      return Result;
   end Is_Table_Exists;

   ----------------------------------------------------------------------------
   procedure Insert (Database_Name : UString;
                     Table_Name : UString;
                     Columns_Values : UString) is

      Database_Handle : aliased Database_Handle_Type := Get_Database_Handle (Database_Name);
   begin
      if Database_Handle /= null then
         Insert (Database_Handle, Table_Name, Columns_Values);
      end if;
   end Insert;

   -------------------------------------------------------------------------------
   function List_Columns_Of_Table (Database_Name : UString;
                                  Table_Name : UString)
                                  return Data_Array_Type is

      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Name);
      Data_Columns : Data_Array_Type;

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      case Database_Brand is
      when SQLite =>
         declare
            Record_Set : Record_Set_SQLite_Type := Query (Database_Name, "SELECT * FROM " & Table_Name & " LIMIT 1");

            --  https://sqlite.org/c3ref/column_count.html
            function Sqlite_Column_Count (Handle : Database_Handle_Type := Record_Set_SQLite_Type (Record_Set).Handle)
                                          return Natural
                                          with Import, Convention => C, External_Name => "sqlite3_column_count";
         begin
            for Columns in 1 .. Sqlite_Column_Count loop
               Data_Columns.Append (Get_Column_Name (Record_Set, Columns));
            end loop;
            Close_Record_Set (Record_Set);
         end;
      when MySQL =>
         declare
            Record_Set : Record_Set_MySQL_Type := Query (Database_Name, "DESCRIBE " & Table_Name);
         begin
            Log.Debug (Log_Header & "Database_Name: " & Database_Name, Log_Filter);
            Log.Debug (Log_Header & "Table_Name: " & Table_Name, Log_Filter);
            Log.Debug (Log_Header & "Number_Of_Rows: " & To_String (Number_Of_Rows (Record_Set)), Log_Filter);
            for Columns in 1 .. Number_Of_Rows (Record_Set) loop
               Next (Record_Set);
               Data_Columns.Append (Get_Column_Data (Record_Set, 1));
            end loop;
         end;
      when PostgreSQL =>
         declare
         begin
            null;
         end;
      when Firebird =>
         declare
         begin
            null;
         end;
      when others => null;
      end case;
      return Data_Columns;
   end List_Columns_Of_Table;

   -------------------------------------------------------------------------------
   function List_Of_Tables (Database_Name : UString)
                            return Data_Array_Type is

      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Name);
      Database_Handle : aliased Database_Handle_Type := Get_Database_Handle (Database_Name);
      Tables : Data_Array_Type := List_Of_Tables (Database_Brand, Database_Handle);
   begin
      return Tables;
   end List_Of_Tables;

   function List_Of_Tables (Database_Handle : aliased Database_Handle_Type)
                            return Data_Array_Type is

      Tables : Data_Array_Type;
      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Handle);
   begin
      return List_Of_Tables (Database_Brand, Database_Handle);
   end List_Of_Tables;

   function List_Of_Tables (Database_Brand : Database_Brand_Type;
                            Database_Handle : aliased Database_Handle_Type)
                            return Data_Array_Type is

      Tables : Data_Array_Type;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      case Database_Brand is
      when SQLite =>
         declare
            Record_Set : Record_Set_SQLite_Type := Query (Database_Handle, "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name");
            Value : UString;
         begin
            Next (Record_Set); --  First record not significant
            while Next (Record_Set) loop
               Value := Get_Column_Data (Record_Set, 1);
               Log.Debug (Log_Header & "List_Of_Tables: " & Value, Log_Filter);
               Tables.Append (Value);
            end loop;
            Close_Record_Set (Record_Set);
         end;
      when MySQL =>
         declare
            Record_Set : Record_Set_MySQL_Type := Query (Database_Handle, "SHOW TABLES");
            Value : UString;
         begin
            while Next (Record_Set) loop
               Value := Get_Column_Data (Record_Set, 1);
               Log.Debug (Log_Header & "List_Of_Tables: " & Value, Log_Filter);
               Tables.Append (Value);
            end loop;
            Close_Record_Set (Record_Set);
         end;
      when PostgreSQL =>
         declare
         begin
            null;
         end;
      when Firebird =>
         declare
         begin
            null;
         end;
      when others => null;
      end case;
      return Tables;
   end List_Of_Tables;

   -------------------------------------------------------------------------------
   procedure Next (Record_Set : in out Record_Set_SQLite_Type) is
      Dummy : Boolean;
   begin
      Dummy := Next (Record_Set);
   end Next;

   function Next (Record_Set : in out Record_Set_SQLite_Type)
                  return Boolean is

      Record_Set_Access : constant access Record_Set_SQLite_Type := Record_Set'Unrestricted_Access;

      --  https://sqlite.org/c3ref/step.html
      function SQLite_Step (Handle : Database_Handle_Type := Record_Set.Handle)
                            return Integer
                            with Import, Convention => C, External_Name => "sqlite3_step";

      Result : Boolean := False;
   begin
      --  SQLITE_DONE > no more rows - return False, SQLITE_ROW > a row is available - return True
      if Record_Set.Last_Result /= SQLITE_DONE then
         if Record_Set.First_Row then
            Record_Set_Access.First_Row := False;
            Result := True;
         else
            Record_Set_Access.Last_Result := SQLite_Step;
            Result := not (Record_Set.Last_Result = SQLITE_DONE);
         end if;
      end if;
      return Result;
   end Next;

   procedure Next (Record_Set : in out Record_Set_MySQL_Type) is
      Dummy : Boolean;
   begin
      Dummy := Next (Record_Set);
   end Next;

   function Next (Record_Set : in out Record_Set_MySQL_Type)
                  return Boolean is

      Record_Set_Access : constant access Record_Set_MySQL_Type := Record_Set'Unrestricted_Access;

      --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_fetch_row
      function MySQL_Fetch_Row (Result : Database_Handle_Type := Record_Set.Handle)
                                return Row_Access
                                with Import, Convention => C, External_Name => "mysql_fetch_row";

      --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_fetch_lengths
      function MySQL_Fetch_Lengths (Result : Database_Handle_Type := Record_Set.Handle)
                                    return List_of_Lengths_Access
                                    with Import, Convention => C, External_Name => "mysql_fetch_lengths";

      Result : Boolean := False;
   begin
      Record_Set_Access.Last_Row := MySQL_Fetch_Row;
      if Record_Set_Access.Last_Row /= null then
         Record_Set_Access.Lengths := MySQL_Fetch_Lengths;
         if Record_Set_Access.Lengths /= null then
            Result := True;
         end if;
      end if;
      return Result;
   end Next;

   -------------------------------------------------------------------------------
   function Number_Of_Columns (Record_Set : Record_Set_SQLite_Type)
                              return Natural is
   begin
      return Record_Set.Column_Count;
   end Number_Of_Columns;

   function Number_Of_Columns (Record_Set : Record_Set_MySQL_Type)
                              return Natural is
   begin
      return Record_Set.Column_Count;
   end Number_Of_Columns;

   ----------------------------------------------------------------------------
   function Open_Database (Database_Brand : Database_Brand_Type;
                           Database_URI : UString := "";
                           Database_Version : UString := "";
                           Database_Rank : Database_Rank_Type := Main)
                           return Database_Status_Type is

      Database_Status : Database_Status_Type := None;
      Database_Name, Database_Host, Database_User, Database_Password, Database_File, Field_2 : UString;
      Database_Port : Natural := 0;
      Database_Handle : aliased Database_Handle_Type := null;
      Database_URI_Parameter, Database_URI_Parameter_Name : UString := "";

      function Is_Parameters_Valid return Boolean is
         Result : Boolean := False;
      begin
         --  db:db_name?host=host_name&port=port_number&user=user_name&password=user_password
         --  This parameters string conforms to RFC 3986
         if (Index (To_Lower (Database_URI), "db:") > 0) and
            (Index (To_Lower (Database_URI), "?host=") > 0) and
            (Index (To_Lower (Database_URI), "&port=") > 0) and
            (Index (To_Lower (Database_URI), "&user=") > 0) and
            (Index (To_Lower (Database_URI), "&password=") > 0) then
            Database_Name := Field_By_Index (Field_By_Index (Database_URI, 2, ":"), 1, "?");
            Field_2 := Field_By_Index (Database_URI, 2, "?");
            for I in 1 .. Field_Count (Field_2, "&") loop
               Database_URI_Parameter := Field_By_Index (Field_2, I, "&");
               Database_URI_Parameter_Name := To_Lower (Field_By_Index (Database_URI_Parameter, 1, "="));
               if Database_URI_Parameter_Name = "host" then
                  Database_Host := Field_By_Index (Database_URI_Parameter, 2, "=");
               elsif Database_URI_Parameter_Name = "port" then
                  Database_Port := To_Integer (Field_By_Index (Database_URI_Parameter, 2, "="));
               elsif Database_URI_Parameter_Name = "user" then
                  Database_User := Field_By_Index (Database_URI_Parameter, 2, "=");
               elsif Database_URI_Parameter_Name = "password" then
                  Database_Password := Field_By_Index (Database_URI_Parameter, 2, "=");
               end if;
            end loop;
            Result := True;
         end if;
         return Result;
      end Is_Parameters_Valid;

      function Is_Name_Not_In_Use (Database_Name : UString) return Boolean is
         Result : Boolean := True;
      begin
         for C in Databases.Iterate loop
            if Databases(C).Name = Database_Name then
               Result := False;
               exit;
            end if;
         end loop;
         return Result;
      end Is_Name_Not_In_Use;

   begin
      case Database_Brand is
      when SQLite =>
         -- file:data.db or file:data.db?mode=ro&cache=private
         -- SQLite has been set in RFC 3986 URI mode (see sqlite.c)
         if Index (To_Lower (Database_URI), "file:") > 0 then
            Database_File := Field_By_Index (Field_By_Index (Database_URI, 2, ":"), 1, "?");
            Database_Name := Field_By_Index (Tail_After_Match (Database_File, "/"), 1, ".");

            if Is_Name_Not_In_Use (Database_Name) then
               Database_Status := (if Open_Connect (Database_Brand,
                                                    Database_URI,
                                                    Database_Handle)
                                   then Open_Success
                                   else Open_Failed);
            else
               Database_Status := Open_Failed_Name_In_Use;
            end if;
         end if;
      when MySQL =>
         Database_Port := 3306; --  Default port for MySQL
         if Is_Parameters_Valid then
            if Is_Name_Not_In_Use (Database_Name) then
               Database_Status := (if Open_Connect (Database_Brand,
                                                    Database_Name,
                                                    Database_Handle,
                                                    Database_Host,
                                                    Database_Port,
                                                    Database_User,
                                                    Database_Password)
                                   then Open_Success
                                   else Open_Failed);
            else
               Database_Status := Open_Failed_Name_In_Use;
            end if;
         else
            Database_Status := Open_Failed_Parameter_Invalid;
         end if;
      when PostgreSQL =>
         Database_Port := 5432; --  Default port for PostgreSQL
         if Is_Parameters_Valid then
            if Is_Name_Not_In_Use (Database_Name) then
               Database_Status := (if Open_Connect (Database_Brand,
                                                    Database_Name,
                                                    Database_Handle,
                                                    Database_Host,
                                                    Database_Port,
                                                    Database_User,
                                                    Database_Password)
                                   then Open_Success
                                   else Open_Failed);
            else
               Database_Status := Open_Failed_Name_In_Use;
            end if;
         else
            Database_Status := Open_Failed_Parameter_Invalid;
         end if;
      when Firebird =>
         null;
      when others => null;
      end case;

      if (Database_Status = Open_Success) then
         Open_Load (Database_Brand,
                    Database_Name,
                    Database_Status,
                    Database_URI,
                    Database_Host,
                    Database_Port,
                    Database_User,
                    Database_Password,
                    Database_File,
                    Database_Version,
                    Database_Handle,
                    Database_Rank);
      end if;
      return Database_Status;
   end Open_Database;

   ----------------------------------------------------------------------------
   task body Ping is
      Delay_Value : Duration := 900.0;  -- Wait 15 minutes between pings
      function Image is new UXStrings.Conversions.Fixed_Point_Image (Duration);
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      accept Start;
      Log.Debug (Log_Header & "On databases armed for " &
                   Trim_Left (Field_By_Index (Image (Delay_Value), 1, ".")) & "s cycles", Log_Filter);
      loop
         delay Delay_Value;
         Sql.Ping_Send;
      end loop;
   end Ping;

   -------------------------------------------------------------------------------
   procedure Query (Database_Name : UString;
                    Database_Query : UString) is

      Database_Handle : aliased Database_Handle_Type := Get_Database_Handle (Database_Name);
   begin
      Query (Database_Handle, Database_Query);
   end Query;

   procedure Query (Database_Handle : aliased Database_Handle_Type;
                    Database_Query : UString) is

      Result : Boolean;
   begin
      Result := Query (Database_Handle, Database_Query);
   end Query;

   function Query (Database_Name : UString;
                   Database_Query : UString) return Natural is

      Database_Handle : aliased Database_Handle_Type := Get_Database_Handle (Database_Name);
      Result : Natural := Query (Database_Handle, Database_Query);
   begin
      return Result;
   end Query;

   function Query (Database_Handle : aliased Database_Handle_Type;
                    Database_Query : UString) return Natural is

      Dummy : Boolean := Query (Database_Handle, Database_Query);
      Result : Natural := Get_Affected_Rows (Database_Handle);
   begin
      return Result;
   end Query;

   function Query (Database_Handle : aliased Database_Handle_Type;
                    Database_Query : UString) return Boolean is

      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Handle);
      Result : Boolean := Query (Database_Brand, Database_Handle, Database_Query);
   begin
      return Result;
   end Query;

   function Query (Database_Brand : Database_Brand_Type;
                   Database_Handle : aliased Database_Handle_Type;
                   Database_Query : UString)
                   return Boolean is

      Result : Boolean := False;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      case Database_Brand is
      when SQLite =>
         declare
            SQL_Query : constant Standard.String := To_UTF_8 (Database_Query);
            Statement : aliased Database_Handle_Type;
            Tail : aliased Database_Handle_Type;

            --  https://sqlite.org/c3ref/prepare.html
            function SQLite_Prepare (Handle  : Database_Handle_Type := Database_Handle;
                                     Query   : Standard.String := SQL_Query;
                                     Length  : Natural := SQL_Query'Length;
                                     ppStmt  : access Database_Handle_Type := Statement'Access;
                                     ppzTail : access Database_Handle_Type := Tail'Access)
                                     return Integer
                                     with Import, Convention => C, External_Name => "sqlite3_prepare_v2";

            --  https://sqlite.org/c3ref/step.html
            function SQLite_Step (Handle : Database_Handle_Type := Statement)
                                  return Integer
                                  with Import, Convention => C, External_Name => "sqlite3_step";
            --  https://sqlite.org/c3ref/finalize.html
            procedure SQLite_Finalize (Handle : Database_Handle_Type := Statement)
                                       with Import, Convention => C, External_Name => "sqlite3_finalize";
         begin
            if SQLite_Prepare = SQLITE_OK then
               declare
                  Result_Step : constant Integer := SQLite_Step;
               begin
                  if Result_Step /= SQLITE_OK and
                     Result_Step /= SQLITE_ROW and
                     Result_Step /= SQLITE_DONE then
                     Log.Error (Log_Header & "Bad query: "
                                & Database_Query & " => "
                                & From_Latin_1 (Result_Step'Image)
                                & " - " & Error_Message_SQLite (Database_Handle));
                  else
                     Result := True;
                     Log.Debug (Log_Header & "Result: True", Log_Filter);
                  end if;
               end;
            else
               Log.Error (Log_Header & "Query error: "
                          & Database_Query & " => "
                          & Error_Message_SQLite (Database_Handle));
            end if;
            SQLite_Finalize;
         end;
      when MySQL =>
         declare
            Record_Set : Record_Set_MySQL_Type;
            SQL_Query : constant Standard.String := To_UTF_8 (Database_Query);

            --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_real_query
            function MySQL_Real_Query (Handle : Database_Handle_Type := Database_Handle;
                                       Query : Standard.String   := SQL_Query;
                                       Length : Natural := SQL_Query'Length)
                                       return Integer
                                       with Import, Convention => C, External_Name => "mysql_real_query";

            --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_store_result
            function MySQL_Store_Result (Handle : Database_Handle_Type := Database_Handle)
                                         return Database_Handle_Type
                                         with Import, Convention => C, External_Name => "mysql_store_result";

            Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
            Log_Header : constant UString := Log_Filter & LOG_PROMPT;
         begin
            if MySQL_Real_Query = 0 then
               --  From MySQL doc : After invoking mysql_query() or mysql_real_query(),
               --  you must call mysql_store_result() or mysql_use_result() for every
               --  statement that successfully produces a result set (SELECT, SHOW,
               --  DESCRIBE, EXPLAIN, CHECK TABLE, and so forth).
               Record_Set.Handle := MySQL_Store_Result;
               if Record_Set.Handle /= null then
                  Result := True;
                  Log.Debug (Log_Header & "Result: True", Log_Filter);
                  Close_Record_Set (Record_Set);
               end if;
            else
               Log.Error (Log_Header & "Query error: "
                          & Database_Query & " => "
                          & Error_Message_MySQL (Database_Handle));
            end if;
         end;
      when PostgreSQL =>
         declare
         begin
            null;
         end;
      when Firebird =>
         declare
         begin
            null;
         end;
      when others => null;
      end case;
      return Result;
   end Query;

   function Query (Database_Name : UString;
                   Database_Query : UString)
                   return Record_Set_SQLite_Type is

      --  Intermediate variable to avoid GNAT error: actual for
      --  aliased formal Database_Handle must be aliased object
      Database_Handle : aliased Database_Handle_Type := Get_Database_Handle (Database_Name);
      Record_Set : Record_Set_SQLite_Type;
   begin
      Record_Set := Query (Database_Handle, Database_Query);
      return Record_Set;
   end Query;

   function Query (Database_Handle : aliased Database_Handle_Type;
                   Database_Query : UString) return Record_Set_SQLite_Type is

      SQL_Query : constant Standard.String := To_UTF_8 (Database_Query);

      Record_Set : Record_Set_SQLite_Type;
      Statement : aliased Database_Handle_Type;
      Tail : aliased Database_Handle_Type;

      --  https://sqlite.org/c3ref/prepare.html
      function SQLite_Prepare (Handle : Database_Handle_Type := Database_Handle;
                               Query : Standard.String  := SQL_Query;
                               Length : Natural := SQL_Query'Length;
                               ppStmt  : access Database_Handle_Type := Statement'Access;
                               ppzTail : access Database_Handle_Type := Tail'Access)
                               return Integer
                               with Import, Convention => C, External_Name => "sqlite3_prepare_v2";

      --  https://sqlite.org/c3ref/step.html
      function SQLite_Step (Handle : Database_Handle_Type := Database_Handle)
                            return Integer
                            with Import, Convention => C, External_Name => "sqlite3_step";

      --  https://sqlite.org/c3ref/column_count.html
      function SQLite_Column_Count (Handle : Database_Handle_Type := Database_Handle)
                                    return Natural
                                    with Import, Convention => C, External_Name => "sqlite3_column_count";

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity) & ".SQLite";
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      if Database_Handle /= null then
         if SQLite_Prepare = SQLITE_OK then
            Log.Debug (Log_Header & "Prepare OK", Log_Filter);

            Record_Set.Handle := Statement;

            declare
               Result_Step : constant Integer := SQLite_Step;
            begin
               if Result_Step /= SQLITE_OK and
                  Result_Step /= SQLITE_ROW and
                  Result_Step /= SQLITE_DONE then
                  Log.Error (Log_Header & "Bad query: "
                             & Database_Query & " => "
                             & From_Latin_1 (Result_Step'Image)
                             & " - " & Error_Message_SQLite (Database_Handle));
               else
                  Log.Debug (Log_Header & "Step OK", Log_Filter);
                  Record_Set.Last_Result := Result_Step;
                  Record_Set.First_Row   := True;
                  Record_Set.Column_Count := SQLite_Column_Count;
                  Log.Debug (Log_Header & "Column_Count: " & Trim_Left ( To_String (Record_Set.Column_Count)), Log_Filter);
                  Log.Debug (Log_Header & "Last_Result: " & Trim_Left ( To_String (Record_Set.Last_Result)), Log_Filter);
                  if Record_Set.Column_Count = 0 then
                     Log.Error (Log_Header & "Column_Count equal to zero");
                  end if;
               end if;
            end;
         else
            Log.Error (Log_Header & "Query error: "
                       & Database_Query & " => "
                       & Error_Message_SQLite (Database_Handle));
         end if;
      else
         Log.Error (Log_Header & "Database handle not found");
      end if;
      return Record_Set;
   end Query;

   function Query (Database_Name : UString;
                   Database_Query : UString)
                   return Record_Set_MySQL_Type is

      --  Intermediate variable to avoid GNAT error: actual for
      --  aliased formal Database_Handle must be aliased object
      Database_Handle : aliased Database_Handle_Type := Get_Database_Handle (Database_Name);
      Record_Set : Record_Set_MySQL_Type;
   begin
      Record_Set := Query (Database_Handle, Database_Query);
      return Record_Set;
   end Query;

   function Query (Database_Handle : aliased Database_Handle_Type;
                   Database_Query : UString) return Record_Set_MySQL_Type is

      SQL_Query : constant Standard.String := To_UTF_8 (Database_Query);
      Record_Set : Record_Set_MySQL_Type;

      --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_real_query
      function MySQL_Real_Query (Handle : Database_Handle_Type := Database_Handle;
                                 Query : Standard.String   := SQL_Query;
                                 Length : Natural := SQL_Query'Length)
                                 return Integer
                                 with Import, Convention => C, External_Name => "mysql_real_query";

      --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_store_result
      function MySQL_Store_Result (Handle : Database_Handle_Type := Database_Handle)
                                   return Database_Handle_Type
                                   with Import, Convention => C, External_Name => "mysql_store_result";

      --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_num_rows
      function MySQL_Num_Rows (Result : Database_Handle_Type := Record_Set.Handle)
                               return MySQL_Unsigned_Long_Long_Type
                               with Import, Convention => C, External_Name => "mysql_num_rows";

      --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_num_fields
      function MySQL_Num_Fields (Result : Database_Handle_Type := Record_Set.Handle)
                                 return Natural
                                 with Import, Convention => C, External_Name => "mysql_num_fields";

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity) & ".MySQL";
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      Log.Debug (Log_Header & "Database_Query: " & Database_Query, Log_Filter);
      if MySQL_Real_Query = 0 then
         Record_Set.Handle := MySQL_Store_Result;
         if Record_Set.Handle /= null then
            Record_Set.Row_Count := Natural (MySQL_Num_Rows);
            Record_Set.Column_Count := MySQL_Num_Fields;
            Log.Debug (Log_Header & "Record_Set.Row_Count: " & To_String (Record_Set.Row_Count), Log_Filter);
            Log.Debug (Log_Header & "Record_Set.Column_Count: " & To_String (Record_Set.Column_Count), Log_Filter);
         else
            Log.Error (Log_Header & "Empty Record_Set");
         end if;
      else
         Log.Error (Log_Header & "Query error: "
                    & Database_Query & " => "
                    & Error_Message_MySQL (Database_Handle));
      end if;
      return Record_Set;
   end Query;

   --------------------------------------------------------------------------
   function Read (Database_Name : UString;
                  Table_Name : UString;
                  Columns : UString;
                  Condition : UString := "")
                  return UString is

      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Name);
      Counter_Columns : constant Natural:= Field_Count (Columns, ",");
      Sql_Condition : UString := Condition;
      Sql_Query, Sql_Result : UString := "";

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      if Is_Table_Exists (Database_Name, Table_Name) then

         if not Is_Empty (Sql_Condition) then
            --  No ORDER BY nor WHERE, add WHERE by default
            if Index (To_Upper (Sql_Condition), "ORDER") = 0 then
               if Index (To_Upper (Sql_Condition), "WHERE") = 0 then
                  Sql_Condition := "WHERE " & Sql_Condition;
               end if;
            end if;
         end if;

         Sql_Query := "SELECT " & Columns & " FROM " & Table_Name & " " & Sql_Condition;
         Log.Debug (Log_Header & "Query: " & Sql_Query, Log_Filter);

         case Database_Brand is
         when SQLite =>
            declare
               Record_Set : Record_Set_SQLite_Type := Query (Database_Name, Sql_Query);
            begin
               Next (Record_Set);
               while Next (Record_Set) loop
                  --  Record_Set.Column_Count allways return 1... to investigate
                  --  Log.Debug (Log_Header & "Record_Set.Column_Count: " & Trim_Left (To_String (Counter_Columns)), Log_Filter);
                  for Index in 1.. Counter_Columns loop
              --  for Index in 1.. Record_Set.Column_Count loop
                     Sql_Result := Sql_Result & Get_Column_Data (Record_Set, Index) & CD;
                  end loop;
                  if (Index (Sql_Result, CD) > 0) then --  Delete last CD and add row delimiter RD
                     Sql_Result := Slice (Sql_Result, 1, Length (Sql_Result) - 1) & RD;
                  end if;
               end loop;
               Close_Record_Set (Record_Set);
            end;
         when MySQL =>
            declare
               Record_Set : Record_Set_MySQL_Type := Query (Database_Name, Sql_Query);
            begin
               while Next (Record_Set) loop
                  for Index in 1.. Counter_Columns loop
                     Sql_Result := Sql_Result & Get_Column_Data (Record_Set, Index) & CD;
                  end loop;
                  if (Index (Sql_Result, CD) > 0) then --  Delete last CD and add row delimiter
                     Sql_Result := Slice (Sql_Result, 1, Length (Sql_Result) - 1) & RD;
                  end if;
               end loop;
               Close_Record_Set (Record_Set);
            end;
         when PostgreSQL =>
            declare
            begin
               null;
            end;
         when Firebird =>
            declare
            begin
               null;
            end;
         when others => null;
         end case;

         --  Delete last RD
         if Length (Sql_Result) >= 2 then --  To handle one digit answer with a trailing RD = 2 chars
            if Slice (Sql_Result, Length (Sql_Result), Length (Sql_Result)) = RD then
               Sql_Result := Slice (Sql_Result, 1, Length (Sql_Result) - 1);
            end if;
         end if;
         Log.Debug (Log_Header & "Read result: " & Sql_Result, Log_Filter);
      else
         Log_Error_Table_Not_Found (Log_Header, Table_Name);
      end if;
      return Sql_Result;
   end Read;

   ----------------------------------------------------------------------------
   procedure Schema_Load (Command : in Schema_Command_Type := Schema_Null;
                          Name : in UString := "";
                          Attribute : in UString := "";
                          Comment : in UString := "";
                          Version : in UString := "") is
   begin
      Schema.Append (Schema_Record_Type'(Command, Name, Attribute, Comment, Version));
   end Schema_Load;

   ----------------------------------------------------------------------------
   procedure Schema_Update (Database_Name : UString) is

      Database_Record : Database_List_Type;
      Database_Version : UString;

      Current_Table_Name, Current_Table_Constraint, Current_Table_Comment,
      Current_Column_Name, Current_Column_Type, Current_Column_Constraint,
      Current_Column_Version, Current_Column_Comment, Current_Index_Name,
      Current_Index_Key, Current_Index_Constraint : UString := "";

      Current_Table_Not_Exists : Boolean := False;
      Columns_Counter, Index_Counter, Constraint_Counter : Natural := 0;

      type Parsing_States is (Idle, Init, Table, Column, Index,
                              Table_Name, Table_Constraint,
                              Column_Name, Column_Constraint,
                              Index_Name, Constraint);

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;

      -------------------------------
      procedure Add_Sys_Schema is
         DB_Query : UString := "INSERT INTO " & Table_Sys_Schema &
                              " (Table_Name, Column_Name, Column_Type, Column_Constraint, Version, Comment)" &
                              " VALUES ('" &
                              Current_Table_Name & "', '" &
                              Current_Column_Name & "', '" &
                              Current_Column_Type & "', '" &
                              Current_Column_Constraint & "', '" &
                              Current_Column_Version & "', '" &
                              Replace (Current_Column_Comment, "'", " ") & "')";
      begin
         if Is_Table_Exists (Database_Name, Table_Sys_Schema) then
            if Current_Table_Name /= Table_Sys_Schema then
               if Is_Column_Exists (Database_Name, Table_Sys_Schema, "Table_Name") and
                  Is_Column_Exists (Database_Name, Table_Sys_Schema, "Column_Name") and
                  Is_Column_Exists (Database_Name, Table_Sys_Schema, "Column_Type") and
                  Is_Column_Exists (Database_Name, Table_Sys_Schema, "Version") and
                  Is_Column_Exists (Database_Name, Table_Sys_Schema, "Comment") then
                  Query (Database_Name, DB_Query);
               end if;
            end if;
         end if;
      end Add_Sys_Schema;

      ---------------------------------
      procedure Clear_Column is
      begin
         Current_Column_Name := "";
         Current_Column_Type := "";
         Current_Column_Comment := "";
         Current_Column_Constraint := "";
         Constraint_Counter := 0;
      end Clear_Column;

      ---------------------------------
      procedure Create_Column is
      begin
         --  Test if non empty column name to handle table break
         --  when previous table column already exists
         if not Is_Empty (Current_Column_Name) then
            if Is_Column_Exists (Database_Name, Current_Table_Name, Current_Column_Name) then
               Log.Debug (Log_Header & "Create_Column > " &
                                       "Existing Table: " & Current_Table_Name &
                                       " - Existing Column: " & Current_Column_Name &
                                       " " & Current_Column_Type &
                                       " " & Current_Column_Constraint, Log_Filter);
            else
               Log.Info ("Table: " & Current_Table_Name &
                          " - Create Column: " & Current_Column_Name &
                          " " & Current_Column_Type &
                          " " & Current_Column_Constraint);
               Query (Database_Name, "ALTER TABLE " & Current_Table_Name  &
                                         " ADD COLUMN " & Current_Column_Name &
                                         " " & Current_Column_Type &
                                         " " & Current_Column_Constraint);
            end if;
            --  Add item in data dictionnary
            Add_Sys_Schema;
            Clear_Column;
         end if;
      end Create_Column;

      ---------------------------------
      procedure Clear_Index is
      begin
         Current_Index_Name := "";
         Current_Index_Key := "";
         Current_Index_Constraint := "";
      end Clear_Index;

      ---------------------------------
      procedure Create_Index is
      begin
         --  Test if non empty column name to handle table break
         --  when previous table index already exists
         if not Is_Empty (Current_Index_Name) then
            if Is_Index_Exists (Database_Name, Current_Table_Name, Current_Index_Name) then
               Log.Debug (Log_Header & "Create_Index > " &
                          "Existing Table: " & Current_Table_Name &
                          " - Existing Index: " & Current_Index_Name &
                          " " & Current_Index_Key &
                          " " & Current_Index_Constraint, Log_Filter);
            else
               Log.Info ("Table: " & Current_Table_Name &
                          " - Creating Index: " & Current_Index_Name &
                          " " & Current_Index_Key &
                          " " & Current_Index_Constraint);

               Query (Database_Name, "CREATE " & Current_Index_Constraint &
                                         " INDEX " & Current_Index_Name  &
                                         " ON " & Current_Table_Name &
                                         " (" & Current_Index_Key & ");");
            end if;
            Clear_Index;
         end if;
      end Create_Index;

      ---------------------------------
      procedure Create_Table is
         DB_Query : UString := "CREATE TABLE " & Current_Table_Name &
                             " (" & Current_Column_Name &
                             " " & Current_Column_Type &
                             " " & Current_Column_Constraint &
                             " " & Current_Table_Constraint & ")";
      begin
         if Current_Table_Not_Exists then
            Log.Info ("Create Table: " & Current_Table_Name &
                       " - Create Column: " & Current_Column_Name &
                       " " & Current_Column_Type &
                       " " & Current_Column_Constraint &
                       " " & Current_Table_Constraint);
            Query (Database_Name, DB_Query);
            Add_Sys_Schema; --  Add item in data dictionnary

            Current_Table_Not_Exists := False;
            Current_Table_Constraint := "";
            Current_Table_Comment := "";
            Clear_Column;
         else
            Create_Column;
         end if;
      end Create_Table;

    begin

      --  Display Schema list for tests
      if Log.Is_Debug and Log.Is_Debug_Filter (Log_Filter) then
         for I of Schema loop
            Log.Debug (From_Latin_1 (I.Command'Image) & " - " & I.Name & " - " & I.Attribute);
         end loop;
         --  for C in Schema.Iterate loop
         --     Log.Debug ("Schema(C).Name: " & Schema(C).Name);
         --  end loop;
      end if;

     --  Clear old Sys_Schema before update
      Clear_Table (Database_Name, "Sys_Schema");

      for I of Schema loop

         --  Schema_Command_List.Put (I.Command);
         --  Tio.Put_Line (" - " & I.Name & " - " & I.Attribute);

         if    I.Command = Schema_Null then
            null; --  No processing

         elsif I.Command = Sql.Schema_Database_Name then

            if Get_Database_Properties (I.Name, Database_Record) then
               if Database_Record.Brand /= None then -- ie DB name not found
                  Log.Info ("Database " & I.Name & " needs creation or update");
                  Database_Version := Database_Record.Version;
               else
                  Log.Error (Log_Header & "Database not found: " & I.Name);
               end if;
            end if;

         elsif I.Command = Schema_Database_Pragma then
            null; --  No processing

         elsif I.Command = Schema_Table_Name then
            --  Wait first column reading if table has to be created
            if Columns_Counter = 1 then
               Create_Table;
            end if;
            --  Process eventually a remaining column
            Create_Column;
            --  Last table command must have been read to eventually create the last index
            Create_Index;

            Current_Table_Name := I.Name;
            Current_Table_Comment := I.Comment;
            Log.Debug (Log_Header & "Load Table_Name: "
                       & Current_Table_Name & " - "
                       & Current_Table_Comment, Log_Filter);

            Current_Table_Not_Exists := not Is_Table_Exists (Database_Name, I.Name);
            Log.Debug (Log_Header & "Table_Name exists: "
                       & To_String (not Current_Table_Not_Exists), Log_Filter);

            Columns_Counter := 0;

         elsif I.Command = Schema_Table_Constraint then

            Current_Table_Constraint := I.Attribute;
            Log.Debug (Log_Header & "Load Table_Constraint: "
                       & Current_Table_Constraint, Log_Filter);

         elsif I.Command = Schema_Column_Name then
            --  Wait first column reading if table has to be created
            if Columns_Counter = 1 then
               Create_Table;
            else
               Create_Column;
            end if;

            Current_Column_Name := I.Name;
            Current_Column_Type := I.Attribute;
            Current_Column_Comment := I.Comment;
            Current_Column_Version := (if Is_Empty (I.Version) then Database_Version else I.Version);
            Log.Debug (Log_Header & "Load Column_Name: "
                       & Current_Column_Name & " - "
                       & Current_Column_Type & " - "
                       & Current_Column_Comment, Log_Filter);

            Columns_Counter := Columns_Counter + 1;

         elsif I.Command = Schema_Column_Constraint then

            Current_Column_Constraint := I.Attribute;
            Log.Debug (Log_Header & "Load Column_Constraint: "
                       & Current_Column_Constraint, Log_Filter);

            Constraint_Counter := Constraint_Counter + 1;

         elsif I.Command = Schema_Index_Name then
            --  Previous table command could be a column creation
            if Columns_Counter >= 2 then
               Create_Column;
            end if;
            --  Previous table command could be an index creation
            Create_Index;

            Current_Index_Name := I.Name;
            Current_Index_Key := I.Attribute;
            Log.Debug (Log_Header & "Load Index_Name: "
                       & Current_Index_Name & " - "
                       & Current_Index_Key, Log_Filter);

            Index_Counter := Index_Counter + 1;

         elsif I.Command = Schema_Index_Constraint then

            Current_Index_Constraint := I.Attribute;
            Log.Debug (Log_Header & "Load Index_Constraint: "
                       & Current_Index_Constraint, Log_Filter);
         end if;

      end loop;

      --  Deal with remaining work
      Create_Table;
      if Columns_Counter >= 2 then
         Create_Column;
      end if;
      Create_Index;

      --  Only main database has Sys_Users table
      if Is_Table_Exists (Database_Name, Table_Sys_Users) then
         --  If Sys_Users table has just been created, it still has no record,
         --  but Sys_Users must have, at least, one admin user registered
         if Count_Rows (Database_Name, "Sys_Users") = 0 then
            Log.Info ("Initialize " & Get_Main_Database & " newly created");
            --  Initialize Sys_Users with a default user with administrator rights
            declare
               Query : constant UString := "Login~admin" & CD &
                                          "First_Name~Number" & CD &
                                          "Last_Name~Six" & CD &
                                          "Password~" & From_Latin_1 (GNAT.SHA512.Digest ("password")) & CD &
                                          "Grants~" & GRANTS_ROLE_ADMINISTRATOR & SD & GRANTS_RIGHTS_FULL & CD &
                                          "Notes~Default administrator" & CD &
                                          "DTS_Creation~" & Prg.Date_Time_Stamp;
            begin
               Log.Debug (Log_Header & "Add default user credentials with: admin/password", Log_Filter);
               Log.Debug (Log_Header & "Query: " & Query, Log_Filter);
               Insert (Database_Name, "Sys_Users", Query);
            end;
         end if;
      end if;

      --  Update database schema version
      Set_Config ("Schema_Version", Database_Version);
   end Schema_Update;

   ----------------------------------------------------------------------------
   function Search (Database_Name : UString;
                    Table_Name : UString;
                    Condition : UString)
                    return Boolean is
      --  Intermediate variable to avoid GNAT error: actual for
      --  aliased formal Database_Handle must be aliased object
      Database_Handle : aliased Database_Handle_Type := Get_Database_Handle (Database_Name);
      Result : Boolean;
   begin
      Result := Search (Database_Handle, Table_Name, Condition);
      return Result;
   end Search;

   ----------------------------------------------------------------------------
   function Search (Database_Handle : aliased Database_Handle_Type;
                    Table_Name : UString;
                    Condition : UString)
                    return Boolean is

      Database_Brand : Database_Brand_Type := Get_Database_Brand (Database_Handle);
      Database_Query : UString := "SELECT * FROM " & Table_Name & " WHERE " & Condition & " LIMIT 1";

      Result : Boolean := False;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      if Is_Table_Exists (Database_Handle, Table_Name) then
         Log.Debug (Log_Header & "Database_Query: " & Database_Query, Log_Filter);
         --  Log.Debug (Log_Header & "Number_Of_Columns: " & To_String (Number_Of_Columns (Record_Set)), DEBUG_SQL);
         --  Log.Debug (Log_Header & "First_Row: " & From_Latin_1 ((if Record_Set.First_Row then "True" else "False")), DEBUG_SQL);
         case Database_Brand is
         when SQLite =>
            --  SQLite for LibreFrame is compiled with SQLITE_ENABLE_ICU on, so the databases are UTF-8 encoded.
            --   This means that when SQLite compares two strings, it considers 'A' and 'a' to be differents.
            declare
               Record_Set : Record_Set_SQLite_Type := Query (Database_Handle, Database_Query);
            begin
               Next (Record_Set); -- First record not significant
               Result := Next (Record_Set);
               Close_Record_Set (Record_Set);
            end;
         when MySQL =>
            --  By default, MySQL uses a case-insensitive collation for string comparisons.
            --  This means that when MySQL compares two strings, it considers 'A' and 'a' to be the same.
            declare
               Record_Set : Record_Set_MySQL_Type := Query (Database_Handle, Database_Query);
            begin
               Result := Next (Record_Set);
               Close_Record_Set (Record_Set);
            end;
         when PostgreSQL =>
            declare
            begin
               null;
            end;
         when Firebird =>
            declare
            begin
               null;
            end;
         when others => null;
         end case;
         Log.Debug (Log_Header & "Result: " & From_Latin_1 (Result'Image), Log_Filter);
      else
         Log_Error_Table_Not_Found (Log_Header, Table_Name);
      end if;
      return Result;
   end Search;

   ----------------------------------------------------------------------------
   procedure Set_Config (Parameter : UString;
                         Value : UString) is

      Database_Name : UString := Get_Main_Database;
      Where : UString := "Parameter" & " = '" & Parameter & "'";
      Query : UString := "Parameter" & ND & Parameter & CD & "Value" & ND & Value;

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      Log.Debug (Log_Header & "Database_Query: " & Query & " " & Where, Log_Filter);
      --  No Mutex as they already exists inside Update and Insert
      if Is_Table_Exists (Database_Name, Table_Sys_Config) then
         if Is_Column_Exists (Database_Name, Table_Sys_Config, "Parameter") and
           Is_Column_Exists (Database_Name, Table_Sys_Config, "Value") then
            if Search (Database_Name, Table_Sys_Config, Where) then
               Log.Debug (Log_Header & "Parameter: Found, branch to Update", Log_Filter);
               Update (Database_Name, Table_Sys_Config, Query, Where);
            else
               Log.Debug (Log_Header & "Parameter: Not found, branch to Insert", Log_Filter);
               Insert (Database_Name, Table_Sys_Config, Query);
            end if;
         else
            Log.Error (Log_Header & "Database: don't have column(s): Parameter and/or Value");
         end if;
      else
         Log_Error_Table_Not_Found (Log_Header, Table_Sys_Config);
      end if;
      Log.Debug (Log_Header & "Quit", Log_Filter);
   exception
      when E : others =>
         Log.Debug (Log_Header & "Quit", Log_Filter);
         Log.Error (Log_Header & "Error: " & From_Latin_1 (Ada.Exceptions.Exception_Information(E)));
   end Set_Config;

   ----------------------------------------------------------------------------
   procedure Set_Database_Main (Database_Name : UString) is
   begin
      Main_Database := Database_Name;
   end Set_Database_Main;

   ----------------------------------------------------------------------------
   procedure Update (Database_Name : UString;
                     Table_Name : UString;
                     Columns_Values : UString;
                     Condition : UString) is

      Database_Handle : aliased Database_Handle_Type := Get_Database_Handle (Database_Name);
   begin
      if Database_Handle /= null then
         Update (Database_Handle, Table_Name, Columns_Values, Condition);
      end if;
   end Update;

   procedure Update (Database_Handle : aliased Database_Handle_Type;
                     Table_Name : UString;
                     Columns_Values : UString;
                     Condition : UString) is

      Description_List : Column_List_Type;
      Description : Column_Record_Type;
      Current_Column, Current_Value, Update_Columns_Values, Current_Type, Sql_Query : UString := "";
      Sql_Condition : UString := Condition;
      Counter_Columns : constant Natural := Field_Count (Columns_Values, CD);

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      Sql_Mutex.Lock;
      if Is_Table_Exists (Database_Handle, Table_Name) then
         Description_List := Get_All_Columns_Properties (Database_Handle, Table_Name);
         Log.Debug (Log_Header & "Counter_Columns: " & To_String (Natural (Description_List.Last_Index)), Log_Filter);

         --  Check each field in parameter against the current table's column
         for Index in 1 .. Counter_Columns loop
            --  Iterate through each column
            for I in Description_List.First_Index .. Description_List.Last_Index loop
               Description := Description_List.Element (I);
               Current_Column := Trim_Both (Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 1, ND));
               Log.Debug (I, Log_Filter);
               Log.Debug (Description_List.Last_Index, Log_Filter);
               Log.Debug (Log_Header & "Current_Column (PRG name): " & To_Upper (Current_Column), Log_Filter);
               Log.Debug (Log_Header & "Current_Column (DB name): " & To_Upper (Description.Column_Name), Log_Filter);
               Log.Debug (Log_Header & "Get_Column_Length: " & To_String (Get_Column_Length (Description)), Log_Filter);

               --  If field name and column name match
               if To_Upper (Current_Column) = To_Upper (Description.Column_Name) then
                  --  Fill Name and Value, according to field type
                  Current_Value := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 2, ND);
                  Current_Type := To_Upper (Slice (Description.Data_Type, 1, 3));
                  Update_Columns_Values := Update_Columns_Values & Current_Column & " = ";
                  Log.Debug (Log_Header & "Current_Value: " & Current_Value, Log_Filter);
                  Log.Debug (Log_Header & "Current_Type: " & Current_Type, Log_Filter);
                  Log.Debug (Log_Header & "Update_Columns_Values: " & Update_Columns_Values, Log_Filter);
                  Log.Debug (Log_Header & "Column Type: " & Current_Type, Log_Filter);

                  --  Apply, depending of type :
                  --  BLOB, TEXT, VARCHAR: Single quotes outside string and escaping characters inside string
                  --  BIGINT, DECIMAL, DOUBLE, FLOAT, INTEGER: Insert 0 if empty
                  if Current_Type = "BIG" then --  BIGINT
                     Update_Columns_Values := Update_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "BLO" then --  BLOB
                     Update_Columns_Values := Update_Columns_Values & "'" & Escape_String (Current_Value) & "',";
                  elsif Current_Type = "DEC" then --  DECIMAL
                     Update_Columns_Values := Update_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "DOU" then --  DOUBLE
                     Update_Columns_Values := Update_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "FLO" then --  FLOAT
                     Update_Columns_Values := Update_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "INT" then --  MySQL INT but SQLite INTEGER
                     Update_Columns_Values := Update_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "TEX" then --  TEXT
                     Update_Columns_Values := Update_Columns_Values & "'" & Escape_String (Current_Value) & "',";
                  elsif Current_Type = "VAR" then --  VARCHAR
                     Current_Value := Escape_String (Current_Value);
                     --  Truncate string adjusting it to maximum column length
                     if Current_Value.Length > Get_Column_Length (Description) then
                        Current_Value := Slice (Current_Value, 1, Get_Column_Length (Description));
                        Log.Error (Log_Header & "String too long to fit in VARCHAR(" &
                                   To_String_Unsigned (Get_Column_Length (Description)) &
                                   "). Truncate it: " & Slice (Current_Value, 1, Get_Column_Length (Description)));
                     end if;
                     Update_Columns_Values := Update_Columns_Values & "'" & Current_Value & "',";
                  else
                      Log.Error (Log_Header & "Column: " & Current_Column & " does not handle Type: " & Current_Type);
                  end if;
                  exit; --  No need to iterate further after match
               else
                  if I = Description_List.Last_Index then
                      Log.Error (Log_Header & "Column: " & Current_Column & " does not exists in Table: " & Table_Name);
                  end if;
               end if;
            end loop;
         end loop;
         --  If at least one Column/Value pair has been processed
         if (Index (Update_Columns_Values, ",") > 0) then
            -- Trailing comma deletion
            Update_Columns_Values := Slice (Update_Columns_Values, 1, Length (Update_Columns_Values) - 1);
            --  Log.Debug (Log_Header & "Insert_Columns_Values: " & Update_Columns_Values, DEBUG_SQL_LEVEL_3);
            if not Is_Empty (Sql_Condition) then
               if Index (To_Upper (Sql_Condition), "WHERE") = 0 then
                  Sql_Condition := "WHERE " & Sql_Condition;
               end if;
            end if;
            Sql_Query := "UPDATE " & Table_Name & " SET " & Update_Columns_Values & " " & Sql_Condition & ";";
            Log.Debug (Log_Header & "Update: " & Sql_Query, Log_Filter);
            Query (Database_Handle, Sql_Query);
         end if;
      else
         Log_Error_Table_Not_Found (Log_Header, Table_Name);
      end if;

      Sql_Mutex.Unlock;
   exception
      when E : others =>
         Log.Error (Log_Header & "Error: " & From_Latin_1 (Ada.Exceptions.Exception_Information(E)));
         Sql_Mutex.Unlock;
   end Update;

   ----------------------------------------------------------------------------
   --  Private
   ----------------------------------------------------------------------------

   -------------------------------------------------------------------------------
   function Close_Connect (Database_Record : in out Database_Record_Type)
                           return Boolean is

      Result : Boolean := False;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin

      if Database_Record.Handle /= null then
         Result := True;
         case Database_Record.Brand is
         when SQLite =>
            declare
               --  https://sqlite.org/c3ref/close.html
               procedure SQLite_Close (Handle : Database_Handle_Type := Database_Record.Handle)
                                       with Import, Convention => C, External_Name => "sqlite3_close";
            begin
               SQLite_Close;
            end;
         when MySQL =>
            declare
               --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_close
               procedure MySQL_Close (Handle : Database_Handle_Type := Database_Record.Handle)
                                      with Import, Convention => C, External_Name => "mysql_close";
            begin
               MySQL_Close;
            end;
         when PostgreSQL =>
            declare
            begin
               null;
            end;
         when Firebird =>
            declare
            begin
               null;
            end;
         when others => null;
         end case;
      else
         Log.Error (Log_Header & "Database handle not found: " & Database_Record.Name);
      end if;
      Log.Debug (Log_Header & "Result: " & From_Latin_1 (Result'Image), Log_Filter);
      return Result;
   end Close_Connect;

   ----------------------------------------------------------------------------
   procedure Close_Delete (Database_Name : UString; To_Delete : Boolean)  is

      Database_Record : Database_List_Type;
      Element_To_Delete_Index : Natural := 0;

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      if Sql.Get_Database_Properties (Database_Name, Database_Record) then
         if Close_Connect (Database_Record) then
            for C in Databases.Iterate loop
               if Databases(C).Name = Database_Name then
                  Element_To_Delete_Index := Databases_List.To_Index (C);
                  exit;
               end if;
            end loop;

            for C in Databases.Iterate loop
               Log.Debug (Log_Filter & "Before: " & Databases(C).Name, Log_Filter);
            end loop;

            if To_Delete and Element_To_Delete_Index /= 0 then
               Databases.Delete (Element_To_Delete_Index);
            end if;

            for C in Databases.Iterate loop
               Log.Debug (Log_Filter & "After: " & Databases(C).Name, Log_Filter);
            end loop;
         else
            Log.Error (Log_Header & "Database not closed: " & Database_Name);
         end if;
      end if;
   end Close_Delete;

   -------------------------------------------------------------------------------
   function Error_Message_SQLite (Database_Handle : aliased Database_Handle_Type)
                                  return UString is

      subtype charbuf is Interfaces.C.char_array (1 .. Interfaces.C.size_t'Last);
      type charbuf_access is access all charbuf;

      --  https://sqlite.org/c3ref/errcode.html
      function SQLite_Error_Message (Handle : Database_Handle_Type := Database_Handle)
                                     return charbuf_access
                                     with Import, Convention => C, External_Name => "sqlite3_errmsg";
   begin
      return From_ASCII (Interfaces.C.To_Ada (SQLite_Error_Message.all));
   end Error_Message_SQLite;

   function Error_Message_MySQL (Database_Handle : Database_Handle_Type)
                                 return UString is

      subtype charbuf is Interfaces.C.char_array (1 .. Interfaces.C.size_t'Last);
      type charbuf_access is access all charbuf;

      --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_error
      function MySQL_Error  (Handle : Database_Handle_Type := Database_Handle)
                             return charbuf_access
                             with Import, Convention => C, External_Name => "mysql_error";
   begin
      return From_UTF_8 (Interfaces.C.To_Ada (MySQL_Error.all));
   end Error_Message_MySQL;

   function Error_Message_PostgreSQL (Database_Handle : Database_Handle_Type)
                                      return UString is

      subtype charbuf is Interfaces.C.char_array (1 .. Interfaces.C.size_t'Last);
      type charbuf_access is access all charbuf;

      --  https://www.postgresql.org/docs/17/libpq-status.html
      function PQ_Error_Message (Handle : Database_Handle_Type := Database_Handle)
                                 --  return ICS.chars_ptr
                                 return charbuf_access
                                 with Import, Convention => C, External_Name => "PQerrorMessage";
   begin
      --  return From_UTF_8 (Interfaces.C.To_Ada (PQ_Error_Message));
      return From_UTF_8 (Interfaces.C.To_Ada (PQ_Error_Message.all));
   end Error_Message_PostgreSQL;

   ----------------------------------------------------------------------------
   function Is_Main_Database_Set return Boolean is
   begin
      return not Is_Empty (Main_Database);
   end Is_Main_Database_Set;

   ----------------------------------------------------------------------------
   function Is_Null (Record_Set : Record_Set_SQLite_Type;
                     Column_Name : UString)
                     return Boolean is
      Result : Boolean := True;
   begin
      for I in 1 .. Record_Set.Column_Count loop
         if Column_Name = Get_Column_Name (Record_Set, I) then
             Result := Is_Null (Record_Set, I);
         end if;
      end loop;
      return Result;
   end Is_Null;

   function Is_Null (Record_Set : Record_Set_SQLite_Type;
                     Column_Number : Natural)
                     return Boolean is

      --  https://sqlite.org/c3ref/column_blob.html
      function Sqlite_Column_Type (Handle : Database_Handle_Type := Record_Set.Handle;
                                   Number : Natural := Column_Number - 1)
                                   return Integer
                                   with Import, Convention => C, External_Name => "sqlite3_column_type";
   begin
      return (Sqlite_Column_Type = SQLITE_NULL);
   end Is_Null;

   function Is_Null (Record_Set : Record_Set_MySQL_Type;
                     Column_Name : UString)
                     return Boolean is
      Result : Boolean := True;
   begin
      for I in 1 .. Record_Set.Column_Count loop
         if Column_Name = Get_Column_Name (Record_Set, I) then
             Result := Is_Null (Record_Set, I);
         end if;
      end loop;
      return Result;
   end Is_Null;

   function Is_Null (Record_Set : Record_Set_MySQL_Type;
                     Column_Number : Natural)
                     return Boolean is
   begin
      return (Record_Set.Last_Row (Column_Number) = null);
   end Is_Null;

----------------------------------------------------------------------------
   procedure Log_Error_Database_Not_Found (Log_Header : UString;
                                           Handle_Or_Name : UString;
                                           Database_Handle_Or_Name : UString) is
   begin
      Log.Error (Log_Header & "Database " & Handle_Or_Name & " not found: " & Database_Handle_Or_Name);
   end Log_Error_Database_Not_Found;

   ----------------------------------------------------------------------------
   procedure Log_Error_Table_Not_Found (Log_Header : UString;
                                        Table_Name : UString) is
   begin
      Log.Error (Log_Header & "Table not found: " & Table_Name);
   end Log_Error_Table_Not_Found;

   -------------------------------------------------------------------------------
   --  function Number_Of_Rows (Record_Set : Record_Set_SQLite_Type)
   --                           return Natural is
   --  Not implemented for SQLite and not useful for LibreFrame as SQLite C API offers workaround.

   function Number_Of_Rows (Record_Set : Record_Set_MySQL_Type)
                            return Natural is
   begin
      return Record_Set.Row_Count;
   end Number_Of_Rows;

   -------------------------------------------------------------------------------
   function Open_Connect (Database_Brand : Database_Brand_Type;
                          Database_Name : in UString;
                          Database_Handle : aliased in out Database_Handle_Type;
                          Database_Host : UString := "";
                          Database_Port : Integer := 0;
                          Database_User : UString := "";
                          Database_Password : UString := "")
                          return Boolean is

      Result : Integer := 1;
      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      Log.Debug (Log_Header & "Brand   : " & From_Latin_1 (Database_Brand'Image), Log_Filter);
      Log.Debug (Log_Header & "Name    : " & Database_Name, Log_Filter);
      Log.Debug (Log_Header & "Host    : " & Database_Host, Log_Filter);
      Log.Debug (Log_Header & "Port    : " & To_String (Database_Port), Log_Filter);
      Log.Debug (Log_Header & "User    : " & Database_User, Log_Filter);
      Log.Debug (Log_Header & "Password: " & Database_Password, Log_Filter);

      case Database_Brand is
      when SQLite =>
         declare
            --  https://sqlite.org/c3ref/open.html
            function Sqlite_Open (File_Name : Standard.String := To_UTF_8 (Database_Name) & Nul;
                                  Handle : access Database_Handle_Type := Database_Handle'Access;
                                  Flags : Integer := SQLITE_OPEN_READWRITE +
                                                     SQLITE_OPEN_CREATE +
                                                     SQLITE_OPEN_FULLMUTEX;
                                  Vfs : Integer := 0)
                                  return Integer
                                  with Import, Convention => C, External_Name => "sqlite3_open_v2";
         begin
            Result := Sqlite_Open;
            if Result /= 0 then
               Log.Error (Log_Header & "Can't connect to SQLite database: " & Database_Name &
                          (if Is_Empty (Error_Message_SQLite (Database_Handle))
                           then From_ASCII (" ")
                           else From_ASCII (" > ") & Error_Message_SQLite (Database_Handle)));
            end if;
         end;
      when MySQL =>
         declare
            --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_init
            function MySQL_Init (Handle : Database_Handle_Type := null)
                                 return Database_Handle_Type
                                 with Import, Convention => C, External_Name => "mysql_init";

            --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_real_connect
            function MySQL_Real_Connect (Handle : Database_Handle_Type;
                                         Host : Standard.String := To_UTF_8 (Database_Host) & Nul;
                                         User : Standard.String := To_UTF_8 (Database_User) & Nul;
                                         Password : Standard.String := To_UTF_8 (Database_Password) & Nul;
                                         Name : Standard.String := To_UTF_8 (Database_Name) & Nul;
                                         Port : Integer := 0;
                                         Unix_Socket : Standard.String := "";
                                         Client_Flag : Integer := 0)
                                         return Database_Handle_Type
                                         with Import, Convention => C, External_Name => "mysql_real_connect";

            --  https://mariadb.com/docs/connectors/mariadb-connector-c/api-functions/mysql_select_db
            function MySQL_Select_DB (Handle : Database_Handle_Type := Database_Handle;
                                      Name : Standard.String := To_UTF_8 (Database_Name) & Nul)
                                      return Integer
                                      with Import, Convention => C, External_Name => "mysql_select_db";

            Init : Database_Handle_Type;
         begin
            Init := MySQL_Init;
            if Init /= null then
               Database_Handle := MySQL_Real_Connect (Init);
               if Database_Handle /= null then
                  Result := MySQL_Select_DB;
                  if Result /= 0 then
                     Log.Error (Log_Header & "Error connecting MySQL database: " & Database_Name &
                                (if Is_Empty (Error_Message_MySQL (Database_Handle))
                                   then From_ASCII (" ")
                                   else From_ASCII (" > ") & Error_Message_MySQL (Database_Handle)));
                  end if;
               else
                  Log.Error (Log_Header & "Can't connect to MySQL database: " & Database_Name &
                             (if Is_Empty (Error_Message_MySQL (Database_Handle))
                                then From_ASCII (" ")
                                else From_ASCII (" > ") & Error_Message_MySQL (Database_Handle)));
               end if;
            else
               Log.Error (Log_Header & "Unable to initialize connection to MySQL client library: " & Database_Name &
                          (if Is_Empty (Error_Message_MySQL (Database_Handle))
                           then From_ASCII (" ")
                           else From_ASCII (" > ") & Error_Message_MySQL (Database_Handle)));
            end if;
         end;
      when PostgreSQL =>
         declare
            URI_PostgreSQL : UString := "postgresql://" & Database_User & ":" &
                                                          Database_Password & "@" &
                                                          Database_Host & ":" &
                                                          To_String (Database_Port) & "/" &
                                                          Database_Name;

            --  https://www.postgresql.org/docs/17/libpq-connect.html
            function PQ_Connect_Db (Options :  Standard.String := To_UTF_8 (URI_PostgreSQL) & Nul)
                                    return Database_Handle_Type
                                    with Import, Convention => C, External_Name => "PQconnectdb";

            --  https://www.postgresql.org/docs/17/libpq-status.html
            function PQ_Status (Handle : Database_Handle_Type)
                                return PostgreSQL_Status_Type
                                with Import, Convention => C, External_Name => "PQstatus";

            Status : PostgreSQL_Status_Type;

         begin
            Database_Handle := PQ_Connect_Db ;
            if Database_Handle /= null then
               Log.Info ("Connexion réussie");
               Status := PQ_Status (Database_Handle);
               Log.Info (From_Latin_1 (Status'Image));
            ---else
               Log.Info (Log_Header & "Error connecting PostgreSQL database: " & Database_Name &
                                (if Is_Empty (Error_Message_PostgreSQL (Database_Handle))
                                   then From_ASCII (" ")
                                   else From_ASCII (" > ") & Error_Message_PostgreSQL (Database_Handle)));
            end if;
         end;
      when Firebird =>
         declare
         begin
            null;
         end;
      when others => null;
      end case;
      Log.Debug (Log_Header & "Result: " & From_Latin_1 (Result'Image), Log_Filter);
      return (Result = 0);
   end Open_Connect;

   ----------------------------------------------------------------------------
   procedure Open_Load (Database_Brand : Database_Brand_Type;
                        Database_Name : UString;
                        Database_Status : in out Database_Status_Type;
                        Database_URI : UString;
                        Database_Host: UString;
                        Database_Port : Natural;
                        Database_User : UString;
                        Database_Password : UString;
                        Database_File : UString;
                        Database_Version : UString;
                        Database_Handle : aliased Database_Handle_Type;
                        Database_Rank : Database_Rank_Type := Main) is

      Get_Schema_Version : UString;
      DB_Schema_Version : Natural;
      DB_Index : Databases_List.Extended_Index;

      Database_Version_Level : UString := Database_Version;
      Schema_Version : Natural := (To_Integer (Field_By_Index (Database_Version, 1, ".")) * 10) +
                                   To_Integer (Field_By_Index (Database_Version, 2, "."));

      Pragmas_Registered : Boolean := True;

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;

      -------------------------------------------------------------------------
      procedure Record_Database is
      begin
         --  Database_Line'(record) mandatory with GCC 11,
         --  See http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/ai12-0400-1.txt?rev=1.3
         Databases.Append (Database_Record_Type'(Index => Databases.Last_Index + 1,
                                                 Name => Database_Name,
                                                 Brand => Database_Brand,
                                                 Status => Database_Status,
                                                 URI => Database_URI,
                                                 Host => Database_Host,
                                                 Port => Database_Port,
                                                 User => Database_User,
                                                 Password => Database_Password,
                                                 File => Database_File,
                                                 --  Store Database_Version for later writing in Schema_Update,
                                                 --  which must be done at the very end of the update
                                                 --  process to ensure update completion
                                                 Version => Database_Version_Level,
                                                 Handle => Database_Handle
                                                ));
      end Record_Database;

      -------------------------------------------------------------------------
      procedure Display_Database is
      begin
         Log.Debug (40 * "-", Log_Filter);
         -- Index
         DB_Index := Databases.Last_Index;
         Log.Debug ("Index:    " & Trim_Left (From_Latin_1 (Databases_List.Extended_Index'Image (DB_Index))), Log_Filter);
         Log.Debug ("URI:      " & Databases(DB_Index).URI);
         Log.Debug (40 * "-");
         -- Last_Element
         Log.Debug ("Index:    " & Trim_Left (To_String (Databases.Last_Element.Index)), Log_Filter);
         Log.Debug ("Name:     " & Databases.Last_Element.Name, Log_Filter);
         Log.Debug ("Brand:    " & From_Latin_1 (Databases.Last_Element.Brand'Image), Log_Filter);
         Log.Debug ("Status:   " & From_Latin_1 (Databases.Last_Element.Status'Image), Log_Filter);
         Log.Debug ("URI:      " & Databases.Last_Element.URI, Log_Filter);
         Log.Debug ("Name:     " & Databases.Last_Element.Name, Log_Filter);
         Log.Debug ("Host:     " & Databases.Last_Element.Host, Log_Filter);
         Log.Debug ("Port:     " & Trim_Left (To_String (Databases.Last_Element.Port)), Log_Filter);
         Log.Debug ("User:     " & Databases.Last_Element.User);
         Log.Debug ("Password: " & Databases.Last_Element.Password, Log_Filter);
         Log.Debug ("File:     " & Databases.Last_Element.File, Log_Filter);
         Log.Debug ("Version:  " & Databases.Last_Element.Version, Log_Filter);
         Log.Debug ("Handle:   " & From_Latin_1 (Databases.Last_Element.Handle'Image), Log_Filter);
         Log.Debug (40 * "-");
      end Display_Database;

   begin

      --  Clear database schema definitions
      Schema.Clear;

      --  Preload database definition
      Schema_Load (Schema_Database_Name, Database_Name); --  Database_Name is a enum. command of Schema_Command_Type

      case Database_Brand is
      when SQLite =>
         Schema_Load (Schema_Database_Pragma,"journal_mode","WAL");
         Schema_Load (Schema_Database_Pragma,"foreign_keys","ON"); --  As of SQLite v3.6.19, the default setting for FK is OFF.
         --  Apply pragmas
         for I of Schema loop
            if I.Command = Schema_Database_Name then --  Database_Name is a enum. command of Schema_Command_Type
               null; --  Do nothing
            elsif I.Command = Schema_Database_Pragma then
               if Query (Database_Brand, Database_Handle, "PRAGMA " & I.Name & "=" & I.Attribute) then
                  Log.Debug (Log_Header & "Load Database_Pragma: " & I.Name & "=" & I.Attribute, Log_Filter);
               else
                  Log.Error (Log_Header & "Database_Pragma not set: " & I.Name & "=" & I.Attribute);
                  Pragmas_Registered := False;
               end if;
            else
               exit;
            end if;
         end loop;
      when MySQL =>
         null;
      when PostgreSQL =>
         null;
      when Firebird =>
         null;
      when others => null;
      end case;

      if Pragmas_Registered then
         Log.Debug (Log_Header & "Load Database_Pragma: Registered", Log_Filter);

         --  Set the database holding the Sys_* tables
         if Database_Rank = Main then
            Set_Main_Database (Database_Name);
            Log.Debug (Log_Header & "Database_Rank is Main for: " & Database_Name, Log_Filter);
         end if;

         if Is_Main_Database_Set then

            if Is_Table_Exists (Database_Brand, Database_Handle, Table_Sys_Config) then
               --  Load if exists current database schema version else Database_Version = 0
               Get_Schema_Version := Get_Config (Database_Brand, Database_Name, Database_Handle, "Schema_Version");
            else
               Get_Schema_Version := "0.0";
               Schema_Version := (if Schema_Version = 0 then 1 else Schema_Version);
            end if;
            Log.Debug (Log_Header & "Schema_Version: " & Get_Schema_Version, Log_Filter);

            DB_Schema_Version := (To_Integer (Field_By_Index (Get_Schema_Version, 1, ".")) * 10) +
              To_Integer (Field_By_Index (Get_Schema_Version, 2, "."));

            --  False if DB_Schema_Version is >= Schema_Version => no need updating
            if (DB_Schema_Version < Schema_Version) then

               Log.Debug (Log_Header & "Status: Open_Need_Update", Log_Filter);
               Database_Status := Open_Need_Update;
               Record_Database;

               --  System tables creation only for main database
               if Database_Rank = Main then
                  Log.Debug (Log_Header & "Database_Rank: Main", Log_Filter);

                  -- Preload Sys_Schema table definition
                  Schema_Load (Schema_Table_Name, Table_Sys_Schema, Comment => "System schema table");
                  Schema_Load (Sql.Schema_Column_Name, "Id", "INTEGER", "Primary key");
                  Schema_Load (Schema_Table_Constraint, "Id", "PRIMARY KEY");
                  Schema_Load (Sql.Schema_Column_Constraint, "Id", "AUTO_INCREMENT");

                  Schema_Load (Schema_Column_Name, "Table_Name", "VARCHAR(40)");
                  Schema_Load (Schema_Column_Name, "Column_Name", "VARCHAR(40)");
                  Schema_Load (Schema_Column_Name, "Column_Type", "VARCHAR(20)");
                  Schema_Load (Schema_Column_Name, "Column_Constraint", "VARCHAR(20)");
                  Schema_Load (Schema_Column_Name, "Comment", "TEXT");
                  Schema_Load (Schema_Column_Name, "Version", "VARCHAR(10)");
                  Schema_Load (Schema_Index_Name, "Idx_Schema_Table_Name", "Table_Name, Column_Name");

                  -- Preload Sys_Config table definition
                  Schema_Load (Schema_Table_Name, Table_Sys_Config, Comment => "System config table");
                  Schema_Load (Sql.Schema_Column_Name, "Id", "INTEGER", "Primary key");
                  Schema_Load (Schema_Table_Constraint, "Id", "PRIMARY KEY");
                  Schema_Load (Sql.Schema_Column_Constraint, "Id", "AUTO_INCREMENT");

                  Schema_Load (Schema_Column_Name, "Parameter", "VARCHAR(40)");
                  Schema_Load (Schema_Column_Name, "Value", "TEXT", "Parameter value");
                  Schema_Load (Schema_Index_Name, "Idx_Config_Parameter","Parameter");

                  --  Preload Sys_User table definition
                  Schema_Load (Sql.Schema_Table_Name, Table_Sys_Users, Comment => "System user table");
                  Schema_Load (Sql.Schema_Column_Name, "Id", "INTEGER", "Primary key - User number");
                  Schema_Load (Schema_Table_Constraint, "Id", "PRIMARY KEY");
                  Schema_Load (Sql.Schema_Column_Constraint, "Id", "AUTO_INCREMENT");

                  Schema_Load (Sql.Schema_Column_Name, "DTS_Creation", "VARCHAR(15)", "User creation date time stamp");
                  Schema_Load (Sql.Schema_Column_Name, "DTS_Update", "VARCHAR(15)", "User update date time stamp");

                  Schema_Load (Sql.Schema_Column_Name, "Login", "VARCHAR(40)", "User login");
                  Schema_Load (Sql.Schema_Column_Name, "First_Name", "VARCHAR(40)", "User surname");
                  Schema_Load (Sql.Schema_Column_Name, "Last_Name", "VARCHAR(40)", "User name");
                  Schema_Load (Sql.Schema_Column_Name, "Phone", "VARCHAR(20)", "User phone");
                  Schema_Load (Sql.Schema_Column_Name, "Email", "VARCHAR(40)", "User email");
                  Schema_Load (Sql.Schema_Column_Name, "Password", "VARCHAR(128)", "User hashed password");

                  Schema_Load (Sql.Schema_Column_Name, "Password_Errors", "INTEGER", "Password errors counter");
                  Schema_Load (Sql.Schema_Column_Constraint, "Password_Errors", "DEFAULT 0");
                  Schema_Load (Sql.Schema_Column_Name, "Password_Validity", "INTEGER", "Password validity in seconds");
                  Schema_Load (Sql.Schema_Column_Constraint, "Password_Validity", "DEFAULT 0");

                  Schema_Load (Sql.Schema_Column_Name, "Grants", "VARCHAR(20)", "See LibreFrame.ads definitions");
                  Schema_Load (Sql.Schema_Column_Name, "Properties", "VARCHAR(40)", "Property_1:Value,Property_2:value...Property_N");
                  Schema_Load (Sql.Schema_Column_Name, "Language", "VARCHAR(5)", "Language from country code ISO 3166-1 alpha-2");
                  Schema_Load (Sql.Schema_Column_Name, "Time_Zone", "VARCHAR(10)", "Time zone TZ Database compliant see Wikipedia page");
                  Schema_Load (Sql.Schema_Column_Name, "Theme", "VARCHAR(20)", "Theme name");
                  Schema_Load (Sql.Schema_Column_Name, "Notes", "TEXT", "Notes");

                  Schema_Load (Sql.Schema_Column_Name, "Connection_Total", "INTEGER", "Duration of cumulated connections");
                  Schema_Load (Sql.Schema_Column_Constraint, "Connection_Total", "DEFAULT 0");
                  Schema_Load (Sql.Schema_Column_Name, "Connection_Counter", "INTEGER", "Total number of connections");
                  Schema_Load (Sql.Schema_Column_Constraint, "Connection_Counter", "DEFAULT 0");
                  Schema_Load (Sql.Schema_Column_Name, "Connection_Info", "VARCHAR(15)", "Current or last connection info (Datetime, IP, etc.");

                  Schema_Load (Sql.Schema_Index_Name, "Idx_User_Login", "Login");

               end if;
            else
               Log.Debug (Log_Header & "Status: Open_Success", Log_Filter);
               Database_Status := Open_Success;
               Record_Database;
            end if;

            if Log.Is_Debug and
               Log.Is_Debug_Filter (Log_Filter) then
               Display_Database;
            end if;

         else
            Database_Status := Open_Failed;
            Log.Error (Log_Header & "Main database not set");
         end if;

      else
         Database_Status := Open_Failed;
         Log.Error (Log_Header & "At least one Database_Pragma not set");
      end if;
   end Open_Load;

   ----------------------------------------------------------------------------
   procedure Ping_Send is

      Log_Filter : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
      Log_Header : constant UString := Log_Filter & LOG_PROMPT;
   begin
      Log.Debug (Log_Header & "Ping on databases triggered", Log_Filter);
      for C in Databases.Iterate loop
         case Databases(C).Brand is
         when SQLite =>
            null;  --  Not applicable
         when MySQL =>
            Sql_Mutex.Lock;
            Query (Databases(C).Handle, "SELECT 1");
            Sql_Mutex.Unlock;
            Log.Debug (Log_Header & "Ping on " & From_Latin_1 (Databases(C).Brand'Image)
                                  & " database: " & Databases(C).Name, Log_Filter);
         when PostgreSQL =>
                 null;
         when Firebird =>
                 null;
         when others => null;
         end case;
      end loop;

   exception
      when E : others =>
         Log.Error (Log_Header & "Error: " & From_Latin_1 (Ada.Exceptions.Exception_Information(E)));
         Sql_Mutex.Unlock;
   end Ping_Send;

   ----------------------------------------------------------------------------
   procedure Set_Main_Database (Database_Name : UString) is

   begin
      Main_Database := Database_Name;
   end Set_Main_Database;

------------------------------------------------------------------------------
end LibreFrame.Sql;
------------------------------------------------------------------------------
