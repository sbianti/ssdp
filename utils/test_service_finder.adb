--------------------------------------------------------------------------------
--  Test_Service_Finder 						      --
--  									      --
--  Copyright © 2015 Sébastien Bianti					      --
--  									      --
--  This program is free software; you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License version 2 as	      --
--  published by the Free Software Foundation.				      --
--  									      --
--  This program is distributed in the hope that it will be useful,	      --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of	      --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	      --
--  GNU General Public License for more details.			      --
--  									      --
--  You should have received a copy of the GNU General Public License	      --
--  along with this program; if not, write to the Free Software	              --
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA --
--------------------------------------------------------------------------------

with Ada.Text_IO;
with SSDP.Service_Finder;

with Get_Options; --https://github.com/sbianti/GetOptions

procedure Test_Service_Finder is
   use SSDP, Ada.Text_IO;

   procedure Default_Scheduling is
      use SSDP.Service_Finder;

      Discover_Header: SSDP.Message_Header_Array :=
	(To_US("Toto: inutile:pardon"), To_US("Zak: important:rien"));

      Null_Header: SSDP.Message_Header_Array(1..0);

      UUID: constant String := "uuid:0dbcf247-96ca-4d58-b3de-a22cd083125b";

      Device: Finder_Device_Type := Initialize_Device("sncf:TGV", UUID);
      Str: String(1..10);
      Lg: Natural;
   begin
      Start_Listening;

      delay 0.5;
      M_Search(Device, Discover_Header);
      Put_Line("First discover sent");

      delay 1.0;
      M_Search(Device, NULL_Header);
      Put_Line("Second discover sent");

      Get_Line(Str, Lg);

      Stop_Listening;
   end Default_Scheduling;

   type Test_Options is (Batch);

   package Get_Test_Options is new Get_Options(Test_Options);
   use Get_Test_Options;

   EOL: constant Character := Character'Val(10);

   Help_Section: constant Unbounded_String := To_US("Example:");

   Example_Value: constant Unbounded_String :=
     To_US("""discover,3,0.5,2.5 sleep,10.0 discover,3,1.0,3.0""");

   Description: constant Unbounded_String :=
     To_US("Three DISCOVER sent with a random delay between 0""5 and 2""5" &
	     EOL & "followed by a delay of 10""" & EOL &
	     "followed by three DISCOVER spaced by a random duration between " &
	     " 1"" and 3""");

   Help_Header: constant String :=
     "   Test program for service finder API" & EOL & EOL &
     "   usage: " & Command_Name & " [--batch «batch_line»]" & EOL & EOL &
     "     batch_line ≡ command [command ]*" & EOL &
     "     command ≡ command_name[,occurence_number[,random_time_range]" &
     "|[,fix_delay]]" & EOL &
     "       command_name ∈ {discover, sleep}" & EOL &
     "       occurence_number ≡ INTEGER_VALUE" & EOL &
     "       fix_delay ≡ DECIMAL_VALUE" & EOL &
     "       random_time_range ≡ lower_bound,upper_bound" & EOL &
     "       lower_bound and upper_bound ∈ DECIMAL_VALUES";

   Result: Option_Result_Array;

   Setting: Option_Setting_Array := (Batch =>
				       (Short_Name => No_Short_Name,
					Needs_Value => Yes,
					Short_Description => Description,
					Value_Form => Example_Value)
				    );
begin
   if Argument_Count = 0 then
      Default_Scheduling;
   else
      Result := Parse(Setting, Help_Header, "", Help_Sections =>
			(Batch => Help_Section));
   end if;

exception
   when End_Of_Program_With_Help_Menu => Service_Finder.Stop_Listening;
end Test_Service_Finder;
