--------------------------------------------------------------------------------
--  This file is part SSDP package            				      --
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
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Numerics.Float_Random;

with SSDP.Utils, SSDP.Service_Finder, SSDP.Service_Provider;

package body SSDP.Command_Scheduling is
   use Ada.Strings.Fixed, Ada.Characters;

   function Parse(Raw_Commands_String: in String) return Schedule_Type is

      LF: constant Character := Latin_1.LF;

      Commands_String: String := Trim(Raw_Commands_String, Ada.Strings.Both);

      Schedule: Schedule_Type;

      Command_Name: Restricted_Command_Name_Type;
      Command_Access: Command_Type_Access;

      Next, Current: Natural;
      Last: constant Natural := Commands_String'Last;

      function Command_Debug return String is
      begin
	 return Commands_String & LF & (Current - 1) * ' ' & "↑" & LF &
	   "see --help option" & LF;
      end Command_Debug;
      pragma Inline_Always(Command_Debug);

      package Command_Name_IO is
	 new Ada.Text_IO.Enumeration_IO(Restricted_Command_Name_Type);
      package Duration_IO is new Ada.Text_IO.Fixed_IO(Duration);
      package Integer_IO is new Ada.Text_IO.Integer_IO(Integer);

      procedure Get_Values(Command: in out Command_Type) is

	 function Coma_Missing_After_Sleep(Bad_Char: in Character)
					  return String is
	    Message: constant String :=  "',<value>' missing after sleep:";
	 begin
	    if Bad_Char = Latin_1.Nul then
	       return Message & LF & Command_Debug;
	    else
	       return Message & " found «" & Bad_Char & "»" & LF &
		 Command_Debug;
	    end if;
	 end Coma_Missing_After_Sleep;

	 function Value_Missing_After_Coma(Bad_Val: in String) return String is
	    Message: constant String := "value missing after character ','";
	 begin
	    if Bad_Val = "" then
	       return Message & LF & Command_Debug;
	    else
	       return Message & " found «" & Bad_Val & "»" & LF &
		 Command_Debug;
	    end if;
	 end Value_Missing_After_Coma;

	 function Bad_Character_Debug return String is
	 begin
	    return "character after " & Command_Name'Img &
	      " command name should be ',' or ' ' here: " &
	      ''' & Commands_String(Current) & ''' & LF & Command_Debug;
	 end Bad_Character_Debug;

	 pragma Inline_Always(Coma_Missing_After_Sleep,
			      Value_Missing_After_Coma,
			      Bad_Character_Debug);
      begin

	 case Command_Name is
	    when Discover | Alive | Bye_Bye =>

	       -- command_name“…”
	       --              ×
	       if Current > Last then
		  raise Parsing_Complete;
	       elsif Commands_String(Current) = ' ' then
		  return;
	       elsif Commands_String(Current) /= ',' then raise Parsing_Error
		 with Bad_Character_Debug;
	       end if;

	       Current := Current + 1;
	       -- command_name,“…”
	       --               ×
	       if Current > Last then raise Parsing_Error
		 with Value_Missing_After_Coma("");
	       elsif Commands_String(Current) = ' ' then raise Parsing_Error
		 with Value_Missing_After_Coma("space");
	       end if;

	       -- command_name,“…”
	       --               ×
	       begin
		  Integer_IO.Get(Commands_String(Current..Last),
				 Command.Number, Next);
	       exception
		  when others => raise Parsing_Error
		     with Value_Missing_After_Coma(Commands_String(Current..Last));
	       end;

	       --  SSDP.Utils.Pl_Debug("value: " & Command.Number'Img);
	       --  SSDP.Utils.Pl_Debug("reste: " & Commands_String(Next..Last));

	       if Next = Last then
		  -- command_name,<int>
		  --                   ×
		  raise Parsing_Complete;
	       end if;

	       Current := Next + 1;

	       -- command_name,<int>“…”
	       --                    ×
	       if Commands_String(Current) = ' ' then
		  -- no duration values so messages will be sent in a burst
		  return;
	       elsif Commands_String(Current) /= ',' then raise Parsing_Error
		 with Bad_Character_Debug;
	       end if;

	       -- command_name,<int>,“…”
	       --                   ×
	       if Current = Last then raise Parsing_Error
		 with Value_Missing_After_Coma("END_OF_STRING");
	       end if;

	       Current := Current + 1;
	       -- command_name,<int>,“…”
	       --                     ×
	       if Commands_String(Current) = ' ' then raise Parsing_Error
		 with Value_Missing_After_Coma("space");
	       end if;

	       begin
		  Duration_IO.Get(Commands_String(Current..Last),
				  Command.Min_Random_Value, Next);
	       exception
		  when others => raise Parsing_Error
		     with Value_Missing_After_Coma(Commands_String(Current..Last));
	       end;

	       if Next = Last then
		  -- command_name,<int>,<duration>
		  --                            ×
		  raise Parsing_Complete;
	       end if;

	       Current := Next + 1;
	       -- command_name,<int>,<duration>“…”
	       --                               ×
	       if Commands_String(Current) = ' ' then
		  -- no max value so messages will be sent with a fix delay
		  return;
	       elsif Commands_String(Current) /= ',' then raise Parsing_Error
		 with Bad_Character_Debug;
	       end if;

	       if Current = Last then
		  -- command_name,<int>,<duration>,
		  --                              ×
		  raise Parsing_Error
		    with Value_Missing_After_Coma("END_OF_STRING");
	       end if;

	       Current := Current + 1;
	       -- command_name,<int>,<duration>,“…”
	       --                                ×
	       if Commands_String(Current) = ' ' then raise Parsing_Error
		 with Value_Missing_After_Coma("space");
	       end if;

	       begin
		  Duration_IO.Get(Commands_String(Current..Last),
				  Command.Max_Random_Value, Next);
	       exception
		  when others => raise Parsing_Error
		     with Value_Missing_After_Coma(Commands_String(Current..Last));
	       end;

	       Current:= Next + 1;

	       if Current > Last then
		  raise Parsing_Complete;
	       elsif Commands_String(Current) /= ' ' then raise Parsing_Error
		 with "commands should be separated with spaces:" & LF
		 & Command_Debug;
	       end if;

	    when Sleep =>
	       if Current > Last then raise Parsing_Error
		 with Coma_Missing_After_Sleep(Latin_1.Nul);
	       elsif Commands_String(Current) /= ',' then
		  raise Parsing_Error
		    with Coma_Missing_After_Sleep(Commands_String(Current));
	       end if;

	       Current := Current + 1;

	       begin
		  Duration_IO.Get(Commands_String(Current..Last),
				  Command.Value, Next);
	       exception
		  when others => raise Parsing_Error
		     with Value_Missing_After_Coma("");
	       end;

	       current := next + 1;

	       if Current > Last then
		  raise Parsing_Complete;
	       elsif Commands_String(Current) /= ' ' then raise Parsing_Error
		 with "commands should be separated with spaces:" & LF
		 & Command_Debug;
	       end if;
	 end case;
      end Get_Values;

   begin
      Current := Commands_String'First;

      loop
	 begin
	    Command_Name_IO.Get(Commands_String(Current..Last),
				Command_Name, Next);
	 exception
	    when others =>
	       while Commands_String(Current) = ' ' loop
		  Current := Current + 1;
	       end loop;

	       raise Parsing_Error
		 with "Bad command name:" & LF & Command_Debug;
	 end;

	 Current := Next + 1;

	 Command_Access := new Command_Type(Command_Name);

	 Schedule.Append(Command_Access);

	 Get_Values(Command_Access.all);

      end loop;

   exception
      when Parsing_Complete => return Schedule;
      when Constraint_Error =>
	 SSDP.Utils.Pl_Error("Parsing failed");
	 raise Parsing_Error;
   end Parse;

   procedure Batch(Device: in out Device_Type'Class;
		   Header: in Message_Header_Array;
		   Schedule: in Schedule_Type) is
      use SSDP.Utils;

      procedure Random_Delay(Min, Max: in Duration) is
	 use Ada.Numerics.Float_Random;

	 Delay_Width: Duration := Max - Min;
	 Gen: Generator;
	 Amount_Of_Time: Duration range Min..Max;
      begin
	 Reset(Gen);

	 Amount_Of_Time := Duration(Random(Gen)) * Delay_Width + Min;

	 Pl_Debug("Delay of" & Amount_Of_Time'Img);
	 delay Amount_Of_Time;
      end Random_Delay;

      use SSDP.Service_Finder, SSDP.Service_Provider;
   begin

      for I in 1..Index_Type(Schedule.Length) loop

	 case Schedule.Element(I).Command_Name is

	    when Alive | Discover | Bye_Bye =>
	       for N in 1..Schedule.Element(I).Number loop
		  case Schedule.Element(I).Command_Name is
		     when Alive =>
			Pl_Debug("Alive" & N'Img & " /" &
				   Schedule.Element(I).Number'Img);
			Notify_Alive(SSDP_Service(Device), Header);

		     when Discover =>
			Pl_Debug("Discover" & N'Img & " /" &
				   Schedule.Element(I).Number'Img);
			M_Search(SSDP_Client(Device), Header);

		     when Bye_Bye =>
			Pl_Debug("Bye bye" & N'Img & " /" &
				   Schedule.Element(I).Number'Img);
			Notify_Bye_Bye(SSDP_Service(Device));

		     when others => null;
		  end case;

		  exit when Schedule.Element(I).Number = N;

		  if Schedule.Element(I).Max_Random_Value = 0.0 and
		    Schedule.Element(I).Min_Random_Value /= 0.0 then
		     Pl_Debug("     delay of" &
				Schedule.Element(I).Min_Random_Value'Img);
		     delay Schedule.Element(I).Min_Random_Value;
		  elsif Schedule.Element(I).Max_Random_Value /= 0.0 then
		     Random_Delay(Schedule.Element(I).Min_Random_Value,
				  Schedule.Element(I).Max_Random_Value);
		  end if;

	       end loop;

	    when Sleep =>
	       Pl_Debug("SLEEP" & Schedule.Element(I).Value'Img & '"');
	       delay Schedule.Element(I).Value;
	 end case;

      end loop;

   exception
      when others => Pl_Debug("Scheduling error");
	 raise;
   end Batch;

end SSDP.Command_Scheduling;
