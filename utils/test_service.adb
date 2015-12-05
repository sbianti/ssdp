--------------------------------------------------------------------------------
--  This file is part of SSDP package			        	      --
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
with SSDP.Services;
with SSDP.Command_Scheduling;

with Test_Utils;

with Get_Options; --https://github.com/sbianti/GetOptions

procedure Test_Service is
   use SSDP, Ada.Exceptions, Test_Utils;

   subtype SSDP_Service is Services.SSDP_Service;

   Device: SSDP_Service;

   Notify_Header: SSDP.Message_Header_Array :=
     (Services.To_US("Affaires: dentifrice"),
      Services.To_US("DIY: courses:rien"));

   UUID: constant String := "822ccbbf-3aa6-44c2-80ef-0307f9673521";

   Default_Service_Type: aliased constant String := "chauffage:central";

   Service_Type_Value: access constant String;

   Null_Header: SSDP.Message_Header_Array(1..0);

   Str: String(1..10);

   Lg: Natural;

   function Default_Initialization return SSDP_Service is
      use SSDP.Services;
   begin
      return Initialize_Device(Service_Type => Service_Type_Value.all,
			       Universal_Serial_Number => UUID,
			       Location => "",
			       AL => "<http://halsensortester.mp.intel.com>",
			       Cache_Control => "max-age = 600",
			       Expires => "");
   end Default_Initialization;

   procedure Default_Scheduling is
      use SSDP.Services, Ada.Text_IO;
   begin

      Device := Default_Initialization;

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

   type Test_Options is (Batch, Service_Type, Bye_Bye_On_Exit);

   package Get_Test_Options is new Get_Options(Test_Options);
   use Get_Test_Options;

   EOL: constant Character := Character'Val(10);

   Help_Section: constant Unbounded_String := To_US("Example:");

   Example_Value: constant array(Test_Options) of Unbounded_String :=
     (To_US("""alive,5,1.5,4.0 sleep,12.0 alive,3,1.0,3.0 byebye,2"""),
      To_US(Default_Service_Type),
      To_US("2")
     );

   Description_Value: constant array(Test_Options) of Unbounded_String :=
     (To_US("Five ALIVE sent with a random delay between 1""5 and 4""" & EOL &
	      "followed by a delay of 12""" & EOL &
	      "followed by three ALIVE spaced by a random duration between " &
	      " 1"" and 3""" & EOL &
	      "followed by two BYEBYE messages sent without delay"),
      To_US("The device service type to search for " &
	      "(previous value is the default)"),
      To_US("One, or many bye-byes are sent when exiting normaly"));

   Help_Header: constant String :=
     "   Test program for SSDP services API" & EOL & EOL &
     "   usage: " & Command_Name &
     " [--batch «batch_line»][--service_type=oven:micro_wave]" &
     "[--bye_bye_on_exit]" & EOL & EOL &
     "     batch_line ≡ command [command ]*" & EOL &
     "     command ≡ command_name[,occurence_number[,random_time_range]" &
     "|[,fix_delay]]" & EOL &
     "       command_name ∈ {alive, sleep, byebye}" & EOL &
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
	 Value_Form => Example_Value(Service_Type)),

      Bye_Bye_On_Exit =>
	(Short_Name => 'b',
	 Needs_Value => Optional,
	 Short_Description => Description_Value(Bye_Bye_On_Exit),
	 Value_Form => Example_Value(Bye_Bye_On_Exit))
     );

   package Scheduling is new Command_Scheduling(Service_Command_Name_Type);
   use Scheduling;

   Schedule: Schedule_Type;
begin
   Result := Parse(Setting, Help_Header, "", Help_Sections =>
		     (Batch => Help_Section, others => Null_Unbounded_String));

   if Result(Service_Type).Is_Set then
      Service_Type_Value := new String'(Get_Value(Result(Service_Type), 1));
   else
      Service_Type_Value := Default_Service_Type'Access;
   end if;

   if not Result(Batch).Is_Set then
      Default_Scheduling;
   else
      Device := Default_Initialization;

      Services.Start_Listening;

      Schedule := Parse(Get_Value(Result(Batch), 1));

      Batch(Device, Notify_Header, Schedule);

      -- Waiting for user interaction:
      Ada.Text_IO.Get_Line(Str, Lg);

      if Result(Bye_Bye_On_Exit).Is_Set then
	 declare
	    package Natural_IO is new Ada.Text_IO.Integer_IO(Positive);

	    Bye_Bye_Value: String := Get_Value(Result(Bye_Bye_On_Exit), 1);
	    Bye_Bye_Number: Positive;
	    Last: Natural;
	 begin

	    if Bye_Bye_Value = "" then
	       Bye_Bye_Number := 1;
	    else
	       Natural_IO.Get(Bye_Bye_Value, Bye_Bye_Number, Last);

	       if Bye_Bye_Number > 30 then
		  Pl_Warning("Bye-Bye number limited to 30 to avoid flooding");
		  Bye_Bye_Number := 30;
	       end if;
	    end if;

	    Pl_Debug("Sending" & Bye_Bye_Number'Img & " final ByeBye");
	    for I in 1..Bye_Bye_Number loop
	       SSDP.Services.Notify_Bye_Bye(Device);
	    end loop;

	 exception
	    when E: Ada.Text_IO.Data_Error =>
	       Pl_Error(Exception_Name(E) & ": Bad bye_bye value '" &
			  Bye_Bye_Value & "' should be a positive number." &
			  " Sending one ByeBye");
	       SSDP.Services.Notify_Bye_Bye(Device);
	    when E: others =>
	       Pl_Debug(Exception_Information(E));
	 end;
      end if;

      Services.Stop_Listening;
   end if;

exception
   when End_Of_Program_With_Help_Menu => Services.Stop_Listening;

   when E: Scheduling.Parsing_Error =>
      Pl_Error(Exception_Message(E));
      Services.Stop_Listening;
end Test_Service;
