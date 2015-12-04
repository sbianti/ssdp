--------------------------------------------------------------------------------
--  This file is part of SSDP package   				      --
--  									      --
--  Copyright © 2015 Sébastien Bianti					      --
--  									      --
--  This program is free software; you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License version 3 as	      --
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
with Ada.Exceptions;
with SSDP.Service_Finder;
with SSDP.Command_Scheduling;

with Test_Utils;

with Get_Options; --https://github.com/sbianti/GetOptions

procedure Test_Service_Finder is
   use SSDP, Ada.Exceptions, Test_Utils;

   Device: Service_Finder.Finder_Device_Type;

   Discover_Header: SSDP.Message_Header_Array :=
     (Service_Finder.To_US("Toto: inutile:pardon"),
      Service_Finder.To_US("Zak: important:rien"));

   Null_Header: SSDP.Message_Header_Array(1..0);

   UUID: constant String := "uuid:0dbcf247-96ca-4d58-b3de-a22cd083125b";

   Default_Service_Type: aliased constant String := "sncf:TGV";

   Service_Type_Value: access constant String;

   Str: String(1..10);

   Lg: Natural;

   procedure Default_Scheduling is
      use SSDP.Service_Finder, Ada.Text_IO;
   begin
      Device := Initialize_Device(Service_Type_Value.all, UUID);

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

   type Test_Options is (Batch, Service_Type);

   package Get_Test_Options is new Get_Options(Test_Options);
   use Get_Test_Options;

   EOL: constant Character := Character'Val(10);

   Help_Section: constant Unbounded_String := To_US("Example:");

   Example_Value: constant array(Test_Options) of Unbounded_String :=
     (To_US("""discover,3,0.5,2.5 sleep,10.0 discover,3,1.0,3.0"""),
      To_US(Default_Service_Type));

   Description_Value: constant array(Test_Options) of Unbounded_String :=
     (To_US("Three DISCOVER sent with a random delay between 0”5 and 2”5" &
	      EOL & "followed by a delay of 10”" & EOL &
	      "followed by three DISCOVER spaced by a random duration " &
	      "between 1” and 3”"),
      To_US("The device service type to search for " &
	      "(previous value is the default)"));

   Help_Header: constant String :=
     "   Test program for service finder API" & EOL & EOL &
     "   usage: " & Command_Name &
     " [--batch «batch_line»][--service_type=oven:micro_wave]" & EOL & EOL &
     "     batch_line ≡ command [command ]*" & EOL &
     "     command ≡ command_name[,occurence_number[,random_time_range]" &
     "|[,fix_delay]]" & EOL &
     "       command_name ∈ {discover, sleep}" & EOL &
     "       occurence_number ≡ INTEGER_VALUE" & EOL &
     "       fix_delay ≡ DECIMAL_VALUE" & EOL &
     "       random_time_range ≡ lower_bound,upper_bound" & EOL &
     "       lower_bound and upper_bound ∈ DECIMAL_VALUES";

   Result: Option_Result_Array;

   Setting: Option_Setting_Array :=
     (Batch =>
	(Short_Name => No_Short_Name,
	 Needs_Value => Yes,
	 Short_Description => Description_Value(Batch),
	 Value_Form => Example_Value(Batch)),

      Service_Type =>
	(Short_Name => 't',
	 Needs_Value => Yes,
	 Short_Description => Description_Value(Service_Type),
	 Value_Form => Example_Value(Service_Type))
     );

   package Scheduling is new SSDP.Command_Scheduling(Finder_Command_Name_Type);
   use Scheduling;

   Schedule: Schedule_Type;

begin
   Result := Parse(Setting, Help_Header, "", Help_Sections =>
		     (Batch => Help_Section,
		      others => Null_Unbounded_String));

   if Result(Service_Type).Is_Set then
      Service_Type_Value := new String'(Get_Value(Result(Service_Type), 1));
   else
      Service_Type_Value := Default_Service_Type'Access;
   end if;

   if not Result(Batch).Is_Set then
      Default_Scheduling;
   else
      Device := Service_Finder.Initialize_Device(Service_Type_Value.all, UUID);

      Service_Finder.Start_Listening;

      Schedule := Parse(Get_Value(Result(Batch), 1));

      Batch(Device, Discover_Header, Schedule);

      -- Waiting for user interaction:
      Ada.Text_IO.Get_Line(Str, Lg);
      Service_Finder.Stop_Listening;
   end if;

exception
   when End_Of_Program_With_Help_Menu =>
      Service_Finder.Stop_Listening;

   when E: Scheduling.Parsing_Error =>
      Pl_Error(Exception_Message(E));
      Service_Finder.Stop_Listening;
end Test_Service_Finder;
