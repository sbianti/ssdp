--------------------------------------------------------------------------------
--  This file is part of SSDP package          				      --
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

with Ada.Containers.Vectors;

generic
   type Restricted_Command_Name_Type is new Command_Name_Type;
package SSDP.Command_Scheduling is

   subtype Index_Type is Positive range 1..100;

   type Command_Type (Command_Name: Restricted_Command_Name_Type) is record
      case Command_Name is
	 when Discover | Alive | Bye_Bye =>
	    Number: Positive := 1;
	    Min_Random_Value,
	    Max_Random_Value: Duration := 0.0;
	 when Sleep =>
	    Value: Duration;
      end case;
   end record;

   type Command_Type_Access is access Command_Type;

   package Command_Vector is new Ada.Containers.Vectors(Index_Type,
							Command_Type_Access);

   subtype Schedule_Type is Command_Vector.Vector;

   function Parse(Raw_Commands_String: in String) return Schedule_Type;

   procedure Batch(Device: in out Device_Type'Class;
		   Header: in Message_Header_Array;
		   Schedule: in Schedule_Type);

   Parsing_Complete, Parsing_Error: exception;

end SSDP.Command_Scheduling;
