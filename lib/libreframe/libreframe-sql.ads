-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe-sql.ads
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
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;

with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings;

with UXStrings.Hash;

with LibreFrame.Prg;
with LibreFrame.Uxs; use LibreFrame.Uxs;

package LibreFrame.Sql is

   package IC renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

  ---------------------------------------------------------------------------------
   type Database_Handle_Type is access all Integer;
   type Database_Brand_Type is (None,
                                MySQL,
                                SQLite,
                                PostgreSQL,
                                Firebird,
                                Unknown);
   type Database_Rank_Type is (Main,
                               Secondary);

   type Database_Status_Type is (None,
                                 Open_Failed,
                                 Open_Failed_Name_In_Use,
                                 Open_Failed_Parameter_Invalid,
                                 Open_Success,
                                 Open_Need_Update);

   type Schema_Command_Type is (Schema_Null,
                                Schema_Database_Name,
                                Schema_Database_Pragma,
                                Schema_Table_Name,
                                Schema_Table_Constraint,
                                Schema_Column_Name,
                                Schema_Column_Constraint,
                                Schema_Index_Name,
                                Schema_Index_Constraint);
   --package Schema_Command_List is new Ada.Text_IO.Enumeration_IO (Enum => Schema_Command_Type);

   ----------------------------------------------------------------------------
   type Database_Record_Type is record
      Index : Positive;
      Name : UString;
      Brand : Database_Brand_Type := None;
      Status : Database_Status_Type := None;
      URI : UString;
      Host : UString;
      Port : Natural;
      User : UString;
      Password : UString;
      File : UString;
      Version : UString;
      Handle : aliased Database_Handle_Type := null;
   end record;
   package Databases_List is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Database_Record_Type);
   subtype Database_List_Type is Database_Record_Type; -- Mandatory by the compiler
   --  Databases container

   type Schema_Record_Type is record
      Command : Schema_Command_Type;
      Name : UString;
      Attribute : UString;
      Comment : UString;
      Version : UString;
   end record;
   package Schemas_List is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Schema_Record_Type);
   --  Databases schema container

   package Data_Arrays is new Ada.Containers.Indefinite_Vectors (Positive, UString);
   subtype Data_Array_Type is Data_Arrays.Vector;
   --  Table result container

   type Record_Set_SQLite_Type is record
      Handle : aliased Database_Handle_Type := null;
      Column_Count : Natural := 0;
      Last_Result : Integer := 0;
      First_Row : Boolean := False;
   end record;
   package Records_List_SQLite is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Record_Set_SQLite_Type);
   subtype Record_List_SQLite_Type is Record_Set_SQLite_Type;
   --  Query result SQLite container

   subtype Column_Data is Interfaces.C.char_array (0 .. Interfaces.C.size_t'Last);
   type Column_Access is access all Column_Data;
   type Row_Data is array (1 .. Natural'Last) of aliased Column_Access;
   type Row_Access is access all Row_Data;
   --  Used to access MySQL row data

   subtype Column_Length is Interfaces.C.unsigned_long;
   type List_of_Lengths is array (1 .. Natural'Last) of aliased Column_Length;
   type List_of_Lengths_Access is access all List_of_Lengths;
   --  Used to access MySQL result Lengths

   type MySQL_Unsigned_Long_Long_Type is new Natural; for MySQL_Unsigned_Long_Long_Type'Size use 64;
   --  Return type for MySQL row related fields

   type Record_Set_MySQL_Type is record
      Handle : aliased Database_Handle_Type := null;
      Last_Row : Row_Access := null;
      Row_Count : Natural := 0;
      Column_Count : Natural := 0;
      Lengths : List_of_Lengths_Access := null;
   end record;
   package Records_List_MySQL is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Record_Set_MySQL_Type);
   subtype Record_List_MySQL_Type is Record_Set_MySQL_Type;
   --  Query result SQLite container

   type Column_Record_Type is record
      Database_Name : UString;
      Table_Name : UString;
      Column_Name : UString;
      Data_Type : UString;
      Can_Be_Null : Boolean;
      Default_Value : UString;
   end record;
   package Column_Records is new Ada.Containers.Indefinite_Vectors (Natural, Column_Record_Type);
   subtype Column_List_Type is Column_Records.Vector;
   --  Column result container (used by Get_Column_* functions)

   package Data_Maps is new Ada.Containers.Indefinite_Hashed_Maps (UString, UString, UXStrings.Hash, Equivalent_Keys => "=");
   subtype Data_Map_Type is Data_Maps.Map;
   -- Columns values result container (used by Get_All_Columns_Data routines)

   ---------------------------------------------------------------------------------
   --  SQLite definitions

   SQLITE_OPEN_READWRITE : constant := 16#2#;
   SQLITE_OPEN_CREATE    : constant := 16#4#;
   SQLITE_OPEN_FULLMUTEX : constant := 16#1_0000#;

   SQLITE_OK   : constant := 0;
   SQLITE_NULL : constant := 5;
   SQLITE_ROW  : constant := 100;
   SQLITE_DONE : constant := 101;

   ---------------------------------------------------------------------------------
   --  MySQL definitions

   type MySQL_Column is record
      Name          : Column_Access;
      Org_Name      : Column_Access;
      Table         : Column_Access;
      Org_Table     : Column_Access;
      DB            : Column_Access;
      Catalog       : Column_Access;
      Default       : Column_Access;
      Create_Length : Interfaces.C.unsigned_long;
      Max_Length    : Interfaces.C.unsigned_long;
      Name_L        : Interfaces.C.unsigned;
      Org_Name_L    : Interfaces.C.unsigned;
      Table_L       : Interfaces.C.unsigned;
      DB_L          : Interfaces.C.unsigned;
      Catalog_L     : Interfaces.C.unsigned;
      Default_L     : Interfaces.C.unsigned;
   end record with Convention => C;
   type MySQL_Column_Type is access all MySQL_Column;

   ---------------------------------------------------------------------------------
   --  PostgreSQL definitions

   type PostgreSQL_Connection_Type is (Connection_Ok,
                                       Connection_Bad,
                                       Connection_Started,
                                       Connection_Made,
                                       Awaiting_Response,
                                       Authentication_Ok,
                                       Set_Environment) with Convention => C;
   for PostgreSQL_Connection_Type'Size use IC.int'Size;

   type PostgreSQL_Status_Type is (Empty_Query,
                                    Command_Ok,
                                    Tuples_Ok,
                                    Copy_Out,
                                    Copy_In,
                                    Bad_Response,
                                    Non_Fatal_Error,
                                    Fatal_Error) with Convention => C;
   for PostgreSQL_Status_Type'Size use IC.int'Size;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   procedure Clear_Table (Database_Name : UString;
                          Table_Name : UString ;
                          Optimize : Boolean := False);
   --  Clear Table_Name and vacuum it if Optimize is True

   procedure Close_Database (Database_Name : UString);
   --  Close a database.

   procedure Close_All_Databases;
   --  Close all databases.

   procedure Close_Record_Set (Record_Set : in out Record_Set_SQLite_Type);
   procedure Close_Record_Set (Record_Set : in out Record_Set_MySQL_Type);
   --  Close current record set and free resources.

   function Count_Rows (Database_Name : UString;
                        Table_Name : UString;
                        Method : UString := "COUNT";
                        Option : UString := "*")
                        return Integer;
   --  Returns counted rows in Table_Name with Method COUNT or MAX and Options ALL or DISTINCT.

   procedure Delete (Database_Name : UString;
                     Table_Name : UString;
                     Condition : UString := "");
   --  Delete one or  more rows in Table_Name specifying a Condition.

   function Escape_String (String_To_Process : UString)
                           return UString;
   --  Escapes incompatible or dangerous characters in a query, impacting SQL query and/or SQL dump.
   --  \0      ASCII NUL character
   --  \b      Backspace character
   --  \t      Tab character
   --  \n      Newline (linefeed) character
   --  \r      Carriage return character
   --  \"      Double quote (") character
   --  \Z      ASCII 26 Control-Z character
   --  \'      Single quote (') character
   --  \%      Percentage character
   --  \\      Backslash character

   function Get_Affected_Rows (Database_Handle : aliased Database_Handle_Type)
                               return Natural;
   --  Returns the number of rows affected by a Query without a return argument.

   function Get_All_Columns_Data (Record_Set : Record_Set_SQLite_Type)
                                  return Data_Map_Type;
   function Get_All_Columns_Data (Record_Set : Record_Set_MySQL_Type)
                                  return Data_Map_Type;
   --  Returns map of all values for current row. Null values are set to an empty UString.

   function Get_All_Columns_Properties (Database_Handle : aliased Database_Handle_Type;
                                        Table_Name : UString)
                                        return Column_List_Type;
   --  Returns list of all column properties.

   function Get_Column_Data (Record_Set : Record_Set_SQLite_Type;
                             Column_Number : Natural)
                             return UString;
   function Get_Column_Data (Record_Set : Record_Set_SQLite_Type;
                             Column_Name : UString)
                             return UString;
   function Get_Column_Data (Record_Set : Record_Set_MySQL_Type;
                             Column_Number : Natural)
                             return UString;
   function Get_Column_Data (Record_Set : Record_Set_MySQL_Type;
                             Column_Name : UString)
                             return UString;
   --  Returns value of field. Null value from Column_Name returns an empty string.

   function Get_Column_Length (Column : Column_Record_Type)
                               return Natural;
   --  Returns the field size portion of a data type, for example:
   --  - If Column.Data_Type = varchar(80), returns 80;
   --  - If Column.Data_Type = decimal(10,2), returns 13 (10 + 1 +2);
   --  - If Column.Data_Type does not have a size portion, returns 0.

   function Get_Column_Name (Record_Set : Record_Set_SQLite_Type;
                             Column_Number : Natural)
                             return UString;
   function Get_Column_Name (Record_Set : Record_Set_MySQL_Type;
                             Column_Number : Natural)
                             return UString;
   --  Returns column's name.

   function Get_Config (Parameter : UString)
                        return UString;
   function Get_Config (Database_Brand : Database_Brand_Type;
                        Database_Name : UString;
                        Database_Handle : aliased Database_Handle_Type;
                        Parameter : UString)
                        return UString;
   --  Get configuration Value from Parameter stored in Sys_Config table.
   --  The Sys_Config table must be already created. Returns an empty string
   --  if the Sys_Config table or parameter does not exist.
   --  When using the first specification, the main database is automatically
   --  chosen.

   function Get_Database_Brand (Database_Name : UString)
                                return Database_Brand_Type;
   function Get_Database_Brand (Database_Handle : aliased Database_Handle_Type)
                                return Database_Brand_Type;
   --  Returns the database brand of Database_Name.

   function Get_Database_Handle (Database_Name : UString)
                                 return Database_Handle_Type;
   --  Get database handle from Database_Name.

   function Get_Main_Database return UString;
   --  Get the main application database. The main database owns Sys_Config,
   --  Sys_Schema and Sys_Users system tables.

   function Get_Database_Name (Database_Handle : aliased Database_Handle_Type)
                               return UString;
   --  Get database name from Database_Handle.

   function Get_Database_Properties (Database_Name : UString;
                                     Database_Record : in out Database_Record_Type)
                                     return Boolean;
   function Get_Database_Properties (Database_Handle : aliased Database_Handle_Type;
                                     Database_Record : in out Database_Record_Type)
                                     return Boolean;
   function Get_Database_Properties (Database_Index : Positive;
                            Database_Record : in out Database_Record_Type)
                            return Boolean;
   --  Returns database properties record.

   function Get_Version (Database_Name : UString) return UString;
   --  Returns database version. Examples (output samples):
   --  - MySQL: v10.3.39-MariaDB-0+deb10u1
   --  - SQLite: v3.43.0

   procedure Insert (Database_Name : UString;
                     Table_Name : UString;
                     Columns_Values : UString);
   procedure Insert (Database_Handle : aliased Database_Handle_Type;
                     Table_Name : UString;
                     Columns_Values : UString);
   --  Create a row in Table_Name with Columns_Values.
   --  The special character ^ (or constant CD as Column delimiter) is used to
   --  separate column/value pairs and the special character ~ (or constant ND
   --  as Name/value delimiter) is used to distinguish the name of a column
   --  from its value. A non existent Table or Column don't raise exception
   --  but an error is logged.
   --  Text fields that are too long to be saved in a VARCHAR are truncated at
   --  the maximum length of the VARCHAR. Truncations are recorded in the log.

   function Is_Column_Exists (Database_Name : UString;
                              Table_Name : UString;
                              Column_Name : UString)
                              return Boolean;
   --  Return true if Column_Name exists in Table_Name.
   --  Return False if Column_Name or Table_Name does not exist.

   function Is_Index_Exists (Database_Name : UString;
                             Table_Name : UString;
                             Index_Name : UString)
                             return Boolean;
   function Is_Index_Exists (Database_Handle : aliased Database_Handle_Type;
                             Table_Name : UString;
                             Index_Name : UString)
                             return Boolean;
   --  Return True if Index_Name exists for Table_Name.
   --  Return False if Index_Name or Table_Name does not exist.
   --  Names are case insensitive for MySQL and case sensitive for SQLite.

   function Is_Table_Exists (Database_Name : UString;
                             Table_Name : UString)
                             return Boolean;
   function Is_Table_Exists (Database_Handle : aliased Database_Handle_Type;
                             Table_Name : UString)
                             return Boolean;
   function Is_Table_Exists (Database_Brand : Database_Brand_Type;
                             Database_Handle : aliased Database_Handle_Type;
                             Table_Name : UString)
                             return Boolean;
   --  Return true if Table_Name exists.

   function List_Columns_Of_Table (Database_Name : UString;
                                  Table_Name : UString)
                                  return Data_Array_Type;
   --  Return an array of field names for Table_Name.

   function List_Of_Tables (Database_Name : UString)
                            return Data_Array_Type;
   function List_Of_Tables (Database_Handle : aliased Database_Handle_Type)
                            return Data_Array_Type;
   function List_Of_Tables (Database_Brand : Database_Brand_Type;
                            Database_Handle : aliased Database_Handle_Type)
                            return Data_Array_Type;
   --  Return an array of table names.

   procedure Next (Record_Set : in out Record_Set_SQLite_Type);
   function Next (Record_Set : in out Record_Set_SQLite_Type)
                  return Boolean;
   procedure Next (Record_Set : in out Record_Set_MySQL_Type);
   function Next (Record_Set : in out Record_Set_MySQL_Type)
                  return Boolean;
   --  Go to next row and return true if end of Record set not reached.

   function Number_Of_Columns (Record_Set : Record_Set_SQLite_Type)
                              return Natural;
   function Number_Of_Columns (Record_Set : Record_Set_MySQL_Type)
                              return Natural;
   --  Returns number of columns in record set.

   function Open_Database (Database_Brand : Database_Brand_Type;
                           Database_URI : UString := "";
                           Database_Version : UString := "";
                           Database_Rank : Database_Rank_Type := Main)
                           return Database_Status_Type;
   --  Open a database.
   --  URI conforms to RFC 3986 URI. See examples below:
   --  SQLite: file:data.db or file:data.db?mode=ro&cache=private
   --          see https://www.sqlite.org/c3ref/open.html for more information.
   --  MySQL:  db:db_name?host=192.168.0.243&port=3306&user=user_name&password=user_password
   --  Database_Version is major.minor DB schema version and is used to manage schema upgrades
   --  Database_Rank set to Main defines the database that will contain the Sys_* tables.
   --  Returns Database_Status see above

   task Ping is
      entry Start;
   end Ping;
   --  Ping.Start launch task to periodically reset timeout, avoiding the infamous MySQL error
   --  "server has gone away". This does not apply to SQLite databases.

   procedure Query (Database_Name : UString;
                    Database_Query : UString);
   procedure Query (Database_Handle : aliased Database_Handle_Type;
                    Database_Query : UString);
   --  Executes a SQL Query

   function Query (Database_Name : UString;
                   Database_Query : UString)
                   return Natural;
   --  Executes a SQL Query and returns the number of affected rows
   function Query (Database_Handle : aliased Database_Handle_Type;
                   Database_Query : UString)
                   return Natural;
   --  Executes a SQL Query and returns the number of affected rows

   function Query (Database_Handle : aliased Database_Handle_Type;
                   Database_Query : UString) return Boolean;
   function Query (Database_Brand : Database_Brand_Type;
                   Database_Handle : aliased Database_Handle_Type;
                   Database_Query : UString)
                   return Boolean;
   --  Executes a SQL Query and returns True if success.

   function Query (Database_Name : UString;
                   Database_Query : UString)
                   return Record_Set_SQLite_Type;
   function Query (Database_Handle : aliased Database_Handle_Type;
                   Database_Query : UString)
                   return Record_Set_SQLite_Type;
   function Query (Database_Name : UString;
                   Database_Query : UString)
                   return Record_Set_MySQL_Type;
   function Query (Database_Handle : aliased Database_Handle_Type;
                   Database_Query : UString)
                   return Record_Set_MySQL_Type;
   --  SQL query and returns a record set.

   function Read (Database_Name : UString;
                  Table_Name : UString;
                  Columns : UString;
                  Condition : UString := "") return UString;
   --  Returns an extraction from Table_Name with comma delimited Columns and
   --  standard SQL Condition (like WHERE, ORDER BY, LIMIT).
   --  The extraction is formatted with standard LibreFrame CD constant as Column
   --  delimiter and RD constant as Row delimiter.
   --  Returns an empty string if condition is not met.

   procedure Schema_Load (Command : Schema_Command_Type := Schema_Null;
                          Name : UString := "";
                          Attribute : UString := "";
                          Comment : UString := "";
                          Version : UString := "");
   --  Load a schema line. Commands will be executed with Schema_Update in code source order

   procedure Schema_Update (Database_Name : UString);
   --  Create, read, update and delete operations on database schema after loading schema by Schema_Load

   function Search (Database_Name : UString;
                    Table_Name : UString;
                    Condition : UString)
                    return Boolean;
   function Search (Database_Handle : aliased Database_Handle_Type;
                    Table_Name : UString;
                    Condition : UString)
                    return Boolean;
   --  Return True if Condition verified.

   procedure Set_Config (Parameter : UString;
                         Value : UString);
   --  Store configuration Parameter and Value to Sys_Config table.
   --  The new Value will replaced the eventually existing one.
   --  The Sys_Config table will be created if needed.

   procedure Update (Database_Name : UString;
                     Table_Name : UString;
                     Columns_Values : UString;
                     Condition : UString);
   procedure Update (Database_Handle : aliased Database_Handle_Type;
                     Table_Name : UString;
                     Columns_Values : UString;
                     Condition : UString);
   --  Update one or more row in Table_Name with Columns_Values specifying a
   --  Where_Condition.
   --  The special character ^ (or constant CD as Column delimiter) is used to
   --  separate column/value pairs and the special character ~ (or constant ND
   --  as Name/value delimiter) is used to distinguish the name of a column
   --  from its value.
   --  A non existent Table or Column don't raise exception but an error is
   --  logged.
   --  Text fields that are too long to be saved in a VARCHAR are truncated at
   --  the maximum length of the VARCHAR. Truncations are recorded in the log.
   --  The SQL clause WHERE is automatically added if Condition is not empty
   --  and the SQL clause WHERE is absent.

------------------------------------------------------------------------------

   Get_Properties_Error : exception;

------------------------------------------------------------------------------
private

   Package_Name : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
   --  Package's name

   Main_Database : UString := "";

   Table_Sys_Config : UString := "Sys_Config";
   Table_Sys_Schema : UString := "Sys_Schema";
   Table_Sys_Users : UString := "Sys_Users";

   Databases : Databases_List.Vector;
   Schema : Schemas_List.Vector;

   function Close_Connect (Database_Record : in out Database_Record_Type)
                           return Boolean;
   --  Close connection from server

   procedure Close_Delete (Database_Name : UString; To_Delete : Boolean);
   --  Close connection from server and delete database from Databases container

   function Error_Message_SQLite (Database_Handle : aliased Database_Handle_Type)
                                  return UString;
   function Error_Message_MySQL (Database_Handle : Database_Handle_Type)
                                 return UString;
   --  Returns the last error message that has occurred on this connection

   function Is_Main_Database_Set return Boolean;
   --  Return True if Main_Database is set.

   function Is_Null (Record_Set : Record_Set_SQLite_Type;
                     Column_Number : Natural)
                     return Boolean;
   function Is_Null (Record_Set : Record_Set_SQLite_Type;
                     Column_Name : UString)
                     return Boolean;
   function Is_Null (Record_Set : Record_Set_MySQL_Type;
                     Column_Number : Natural)
                     return Boolean;
   function Is_Null (Record_Set : Record_Set_MySQL_Type;
                     Column_Name : UString)
                     return Boolean;
   --  return True if value of field is null
   --  For SQLite Is_Null is only reliable before Get_Column_Data is taken

   procedure Log_Error_Database_Not_Found (Log_Header : UString;
                                           Handle_Or_Name : UString;
                                           Database_Handle_Or_Name : UString);
   --  Log error message factorization

   procedure Log_Error_Table_Not_Found (Log_Header : UString;
                                        Table_Name : UString);
   --  Log error message factorization

   --  function Number_Of_Rows (Record_Set : Record_Set_SQLite_Type)
   --                           return Natural;
   --  Not implemented for SQLite and not useful for LibreFrame as SQLite C API offers workaround.
   function Number_Of_Rows (Record_Set : Record_Set_MySQL_Type)
                            return Natural;
   --  Return number of rows in Record_Set

   function Open_Connect (Database_Brand : Database_Brand_Type;
                          Database_Name : in UString;
                          Database_Handle : aliased in out Database_Handle_Type;
                          Database_Host : UString := "";
                          Database_Port : Integer := 0;
                          Database_User : UString := "";
                          Database_Password : UString := "")
                          return Boolean;
   --  Open connection to database

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
                        Database_Rank : Database_Rank_Type := Main);

   procedure Ping_Send;
   --  Ping opened databases to reset timeout, to avoid the infamous MySQL error "server has gone away".
   --  This does not apply to SQLite databases.

   procedure Set_Main_Database (Database_Name : UString);
   --  Set the main application database. The main database owns Sys_Config,
   --  Sys_Schema and Sys_Users system tables.

------------------------------------------------------------------------------
end LibreFrame.Sql;
------------------------------------------------------------------------------
