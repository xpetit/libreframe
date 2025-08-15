-------------------------------------------------------------------------------
--                  _
--  |  . |_   _ _  |_  _ _   _ _   _
--  |_ | |_) | (/_ |  | (_| | | | (/_
--
--  @file      libreframe-uxs.adb
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  LibreFrame framework - UTF-8 Text files package
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

with Ada.Integer_Text_IO;

with LibreFrame.Fls;
with LibreFrame.Msg;
with LibreFrame.Prg;
with LibreFrame.Sys;
with LibreFrame.Tio;

package body LibreFrame.Uxf is

   function Image is new UXStrings.Conversions.Scalar_Image (Encoding_Scheme);
   function Value is new UXStrings.Conversions.Scalar_Value (Encoding_Scheme);

   ----------------------------------------------------------------------------
   --  API - Terminal
   ----------------------------------------------------------------------------


   ----------------------------------------------------------------------------
   --  API - Text File
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Append (Handle : in out File; Name : UString) is
   begin
      UTI.Open (Handle, UTI.Append_File, Name);
   end Append;

   ----------------------------------------------------------------------------
   procedure Create (Handle : in out File; Name : UString) is
   begin
      UTI.Create (Handle, UTI.Out_File, Name, UTF_8, UTI.LF_Ending);
   end Create;

   ----------------------------------------------------------------------------
   function Get_Line (Handle : in out File) return UString is
   begin
      return UTI.Get_Line (Handle);
   end Get_Line;

   procedure Get_Line (Handle : in out File; V : in out UString) is
   begin
      V := UTI.Get_Line (Handle);
   exception
      when Constraint_Error =>
         Msg.Error ("LibreFrame.Uxf.Get_Line > Exception catched CONSTRAINT_ERROR");
         V := "";
   end Get_Line;

   ----------------------------------------------------------------------------

   procedure Open_Read (Handle : in out File; Name : UString; Scheme : Encoding_Scheme := UTF_8) is
   begin
      UTI.Open (Handle, UTI.In_File, Name, Scheme, UTI.LF_Ending);
   end Open_Read;

------------------------------------------------------------------------------
end LibreFrame.Uxf;
------------------------------------------------------------------------------
