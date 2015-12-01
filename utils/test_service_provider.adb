--------------------------------------------------------------------------------
--  Test_Service_Provider						      --
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
with SSDP.Service_Provider;

with Get_Options; --https://github.com/sbianti/GetOptions

procedure Test_Service_Provider is
   use Ada.Text_IO, SSDP;

   procedure Default_Scheduling is
      use SSDP.Service_Provider;

      Notify_Header: SSDP.Message_Header_Array :=
	(To_US("Affaires: dentifrice"), To_US("DIY: courses:rien"));

      UUID: constant String := "822ccbbf-3aa6-44c2-80ef-0307f9673521";

      Null_Header: SSDP.Message_Header_Array(1..0);

      Device: Service_Provider_Device_Type :=
	Initialize_Device(Service_Type => "chauffage:central",
			  Universal_Serial_Number => UUID,
			  Location => "",
			  AL => "<http://halsensortester.mp.intel.com>",
			  Cache_Control => "max-age = 600",
			  Expires => "");

      Str: String(1..10);
      Lg: Natural;
   begin
      Start_Listening;

      delay 0.2;
      Notify_Alive(Device, Notify_Header);
      Put_Line("Notify sent");

      delay 2.5;
      Notify_Alive(Device, Null_Header);
      Put_Line("Second notify sent");

      Get_Line(Str, Lg);
      Put_Line("Bye bye !");
      Notify_Bye_Bye(Device);

      Stop_Listening;
   end Default_Scheduling;

   type Test_Options is (Batch);

   package Get_Test_Options is new Get_Options(Test_Options);
   use Get_Test_Options;

   EOL: constant Character := Character'Val(10);

   Help_Section: constant Unbounded_String := To_US("Example:");

   Example_Value: constant Unbounded_String :=
     To_US("""alive,5,1.5,4.0 sleep,12.0 alive,3,1.0,3.0 byebye,2""");

   Description: constant Unbounded_String :=
     To_US("Five ALIVE sent with a random delay between 1""5 and 4""" & EOL &
	     "followed by a delay of 12""" & EOL &
	     "followed by three ALIVE spaced by a random duration between " &
	     " 1"" and 3""" & EOL &
	     "followed by two BYEBYE messages sent without delay");

   Help_Header: constant String :=
     "   Test program for service provider API" & EOL & EOL &
     "   usage: " & Command_Name & " [--batch «batch_line»]" & EOL & EOL &
     "     batch_line ≡ command [command ]*" & EOL &
     "     command ≡ command_name[,occurence_number[,random_time_range]" &
     "|[,fix_delay]]" & EOL &
     "       command_name ∈ {alive, sleep, byebye}" & EOL &
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
   when End_Of_Program_With_Help_Menu => Service_Provider.Stop_Listening;
end Test_Service_Provider;
