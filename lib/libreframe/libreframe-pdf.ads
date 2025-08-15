-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe-pdf.ads
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
--  Lounès Souakri - ls - dev@soweb.io (main author)
--  Stéphane Rivière - sr - dev@soweb.io
--  Xavier Petit - xp - dev@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Numerics;

with PDF_Out; use PDF_Out;

with LibreFrame.Uxs;

package LibreFrame.Pdf is

   subtype Pie_Value is Float range 0.0..Float'Last;

   type Reference is (Go_To, Mail_To, URI);
   type Pie_Slice is record
      Value : Pie_Value;
      Color : PDF_Out.Color_Type;
   end record;
   type Pie_Content is array (Positive range <>) of Pie_Slice;
   type Header is record
      Path_To_Logo : UString := "";
      First_Line : UString := "";
      Second_Line : UString := "";
      Logo_Redirect : UString := "";
   end record;
   type Footer is record
      Website : UString := "";
      Contact : UString := "";
      Item : UString := "";
      Paginate : Integer := 0;
   end record;

   use Ada.Numerics, Real_Elementary_Functions;

   function Get_Scale (Object : in out PDF_Out.PDF_Out_File) return Real;
   -- Return : The amount of char that it is possible to display in one line depending of the font size

   function Get_Size (Object : in out PDF_Out.PDF_Out_File; Str : UString) return Real;
   -- Return : The size of the string (depending of the font size)

   function Get_X_Align (Object : in out PDF_Out.PDF_Out_File; X : Real ; Str : UString) return Real;
   -- Return : the X position where the text has to be placed to be centered on the X given

   procedure Put_X_Align (Object : in out PDF_Out.PDF_Out_File; X, Y : Real; Str : UString);
   -- Return : Place the text on Y and centered on the X position

   function Get_X_Right (Object : in out PDF_Out.PDF_Out_File; Right : Real ; Str : UString) return Real;
   -- Return : the X position where the text has to be placed to left a blank space on right

   procedure Put_X_Right (Object : in out PDF_Out.PDF_Out_File; Right, Y : Real; Str : UString);
   -- Return : Place the text and left a blank space on the right

   function Get_X_Center (Object : in out PDF_Out.PDF_Out_File; Str : UString) return Real;
   -- Return : The X value needed to center horizontally the text on the page

   procedure Put_X_Center (Object : in out PDF_Out.PDF_Out_File; Y : Real; Str : UString);
   -- Action : It place and center horizontally the text on the page

   procedure Put_Link (Object : in out PDF_Out.PDF_Out_File;
                           X, Y : Real ;
                           Url : UString ;
                           Pre : Reference ;
                           Str : UString := "";
                           Color : Color_Type := (0.0,0.0,0.4));
   -- Action : It display a clickable link (mail,url,pgae index) with a customisable color at the X Y coordinates.

   procedure Put_Pie_Chart (Object : in out PDF_Out.PDF_Out_File; Center : Point; Radius : Real; Data : Pie_Content);
   -- Action : Draw a pie chart from values

   procedure Set_Header (Logo_Path, Line_One, Line_Two : UString; Logo_Url : UString := "");
   -- Action : Setup the header

   procedure Put_Header (Object : in out PDF_Out.PDF_Out_File; Title : UString);
   -- Action : Display the previously setup header

   procedure Set_Footer (Website, Contact, Item : UString := ""; Paginate : Integer := 0);
   -- Action : Setup the footer

   procedure Put_Footer (Object : in out PDF_Out.PDF_Out_File);
   -- Action : Display the previously setup footer

------------------------------------------------------------------------------
private

   Package_Name : constant UString := From_Latin_1 (GSI.Enclosing_Entity);
   --  Package's name

   procedure Set_Font (Object : in out PDF_Out.PDF_Out_File);
   -- Use "courier" font

   Full_X : Real := one_cm * 21.0;
   Object_Header : Header;
   Object_Footer : Footer;

-------------------------------------------------------------------------------
end LibreFrame.Pdf;
-------------------------------------------------------------------------------
